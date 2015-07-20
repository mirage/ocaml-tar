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

module Archive = Tar.Archive(Lwt)
module StringMap = Map.Make(String)

module Make_KV_RO (BLOCK : V1_LWT.BLOCK) = struct

  type t = {
    b: BLOCK.t;
    map: (Tar.Header.t * int64) StringMap.t;
    info: BLOCK.info;
  }

  type id = BLOCK.id
  type 'a io = 'a Lwt.t

  type error = Unknown_key of string
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

  let connect b =
    BLOCK.get_info b
    >>= fun info ->
    let buffer = Io_page.(to_cstruct @@ get 1) in
    let read sector =
      (* Tar assumes 512 byte sectors, but BLOCK might have 4096 byte sectors for example *)
      let sector' = Int64.(div (mul sector 512L) (of_int info.BLOCK.sector_size)) in
      BLOCK.read b sector' [ buffer ]
      >>= function
      | `Error (`Unknown x) -> Lwt.fail (Failure (Printf.sprintf "Failed to read sector %Ld from block device: %s" sector x))
      | `Error _ -> Lwt.fail (Failure (Printf.sprintf "Failed to read sector %Ld from block device" sector))
      | `Ok () ->
        (* If the BLOCK sector size is big, then we need to select the 512 bytes we want *)
        let offset = Int64.(to_int (sub (mul sector 512L) (mul sector' (of_int info.BLOCK.sector_size)))) in
        Lwt.return (Cstruct.sub buffer offset 512) in

    Archive.fold (fun map tar data_offset ->
      let filename = trim_slash tar.Tar.Header.file_name in
      let map = StringMap.add filename (tar, data_offset) map in
      Lwt.return map
    ) StringMap.empty read
    >>= fun map ->
    Lwt.return (`Ok { b; map; info })

  let disconnect _ = Lwt.return ()

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
