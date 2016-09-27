(*
 * Copyright (C) 2015 Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Tar archives as read-only key=value stores for Mirage *)

open Lwt.Infix

module StringMap = Map.Make(String)

module Make_KV_RO (BLOCK : V1_LWT.BLOCK) = struct

  type t = {
    b: BLOCK.t;
    map: (Tar.Header.t * int64) StringMap.t;
    info: BLOCK.info;
  }

  type id = BLOCK.id
  type 'a io = 'a Lwt.t

  type error = Unknown_key of string | Failure of string
  type page_aligned_buffer = Cstruct.t

  (* Compare filenames without a leading / or ./ *)
  let trim_slash x =
    let startswith prefix x =
      let prefix' = String.length prefix in
      let x' = String.length x in
      x' >= prefix' && (String.sub x 0 prefix' = prefix) in
    if startswith "./" x
    then String.sub x 2 (String.length x - 2)
    else if startswith "/" x
    then String.sub x 1 (String.length x - 1)
    else x

  module Reader = struct
    type in_channel = {
      b: BLOCK.t;
      mutable offset: int64;
      info: BLOCK.info;
    }
    type 'a t = 'a Lwt.t
    let really_read in_channel buffer =
      assert(Cstruct.len buffer <= 512);
      (* Tar assumes 512 byte sectors, but BLOCK might have 4096 byte sectors for example *)
      let sector' = Int64.(div in_channel.offset (of_int in_channel.info.BLOCK.sector_size)) in
      let page = Io_page.(to_cstruct @@ get 1) in
      BLOCK.read in_channel.b sector' [ page ]
      >>= function
      | `Error (`Unknown x) -> Lwt.fail (Failure (Printf.sprintf "Failed to read sector %Ld from block devi
      ce: %s" sector' x))
      | `Error _ -> Lwt.fail (Failure (Printf.sprintf "Failed to read sector %Ld from block device" sector'))
      | `Ok () ->
        (* If the BLOCK sector size is big, then we need to select the 512 bytes we want *)
        let offset = Int64.(to_int (sub in_channel.offset (mul sector' (of_int in_channel.info.BLOCK.sector_size)))) in
        in_channel.offset <- Int64.(add in_channel.offset (of_int (Cstruct.len buffer)));
        Cstruct.blit page offset buffer 0 (Cstruct.len buffer);
        Lwt.return_unit
    let skip in_channel n =
      in_channel.offset <- Int64.(add in_channel.offset (of_int n));
      Lwt.return_unit
    let get_current_tar_sector in_channel = Int64.div in_channel.offset 512L

  end
  module HR = Tar.HeaderReader(Lwt)(Reader)

  let connect b =
    BLOCK.get_info b
    >>= fun info ->
    let in_channel = { Reader.b; offset = 0L; info } in
    let rec loop map =
      HR.read in_channel
      >>= function
      | Result.Error `Eof -> Lwt.return map
      | Result.Ok tar ->
        let filename = trim_slash tar.Tar.Header.file_name in
        let data_tar_offset = Int64.div in_channel.Reader.offset 512L in
        let map = StringMap.add filename (tar, data_tar_offset) map in
        Reader.skip in_channel (Int64.to_int tar.Tar.Header.file_size) >>= fun () ->
        Reader.skip in_channel (Tar.Header.compute_zero_padding_length tar) >>= fun () ->
        loop map in
    loop StringMap.empty
    >>= fun map ->
    Lwt.return (`Ok { b; map; info })

  let disconnect _ = Lwt.return ()

  let mem t key =
    Lwt.return (`Ok (StringMap.mem key t.map))

  let read t key offset length =
    let key = trim_slash key in
    if not(StringMap.mem key t.map)
    then Lwt.return (`Error (Unknown_key key))
    else begin
      let hdr, start_sector = StringMap.find key t.map in

      BLOCK.get_info t.b >>= fun info ->
      let open Int64 in
      let sector_size = of_int info.BLOCK.sector_size in

      (* Compute the unaligned data we need to read *)
      let start_bytes = add (mul start_sector 512L) (of_int offset) in
      let length_bytes = min (of_int length) (sub hdr.Tar.Header.file_size (of_int offset)) in
      let end_bytes = add start_bytes length_bytes in
      (* Compute the starting sector and ending sector (rounding down then up) *)
      let start_sector, start_padding = div start_bytes sector_size, rem start_bytes sector_size in
      let end_sector = div (pred (add end_bytes sector_size)) sector_size in
      let n_sectors = succ (sub end_sector start_sector) in

      let n_bytes = to_int (mul sector_size n_sectors) in
      let n_pages = (n_bytes + 4095) / 4096 in
      let block = Io_page.get n_pages in
      let pages = List.map Io_page.to_cstruct (Io_page.to_pages block) in
      BLOCK.read t.b start_sector pages
      >>= function
      | `Error _ -> Lwt.fail (Failure (Printf.sprintf "Failed to read %s" key))
      | `Ok () -> Lwt.return (`Ok [ Cstruct.sub (Io_page.to_cstruct block) (to_int start_padding) (to_int length_bytes) ])
    end

  let size t key =
    let key = trim_slash key in
    if not(StringMap.mem key t.map)
    then Lwt.return (`Error (Unknown_key key))
    else begin
      let hdr, start_sector = StringMap.find key t.map in
      Lwt.return (`Ok hdr.Tar.Header.file_size)
    end
end
