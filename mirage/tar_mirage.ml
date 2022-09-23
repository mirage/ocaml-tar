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

module Make_KV_RO (BLOCK : Mirage_block.S) = struct

  type entry =
    | Value of Tar.Header.t * int64
    | Dict of Tar.Header.t * entry StringMap.t

  type t = {
    b: BLOCK.t;
    map: entry;
    info: Mirage_block.info;
  }

  type key = Mirage_kv.Key.t

  type error = [ Mirage_kv.error | `Block of BLOCK.error ]

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | `Block b -> BLOCK.pp_error ppf b

  let get_node t key =
    let rec find e = function
      | [] -> Ok e
      | hd::tl -> match e with
        | Value _ -> Error (`Dictionary_expected key)
        | Dict (_, m) -> match StringMap.find_opt hd m with
          | Some e -> find e tl
          | None -> Error (`Not_found key)
    in
    find t (Mirage_kv.Key.segments key)

  let exists t key =
    let r = match get_node t.map key with
      | Ok (Value _) -> Ok (Some `Value)
      | Ok (Dict _) -> Ok (Some `Dictionary)
      | Error (`Not_found _) -> Ok None
      | Error e -> Error e
    in
    Lwt.return r

  let size t key =
    let r = match get_node t.map key with
      | Ok (Value (e, _)) -> Ok (Int64.to_int e.file_size)
      | Ok (Dict (e, _)) -> Ok (Int64.to_int e.file_size)
      | Error e -> Error e
    in
    Lwt.return r

  module Reader = struct
    type in_channel = {
      b: BLOCK.t;
      mutable offset: int64;
      info: Mirage_block.info;
    }
    type 'a t = 'a Lwt.t
    let really_read in_channel buffer =
      let len = Cstruct.length buffer in
      assert(len <= 512);
      (* Tar assumes 512 byte sectors, but BLOCK might have 4096 byte sectors for example *)
      let sector_size = in_channel.info.Mirage_block.sector_size in
      let sector' = Int64.(div in_channel.offset (of_int sector_size)) in
      let sector_aligned_len =
        if len mod sector_size == 0 then len else
          len + (sector_size - len mod sector_size)
      in
      let tmp = Cstruct.create sector_aligned_len in
      BLOCK.read in_channel.b sector' [ tmp ]
      >>= function
      | Error e -> Lwt.fail (Failure (Format.asprintf "Failed to read sector %Ld from block device: %a" sector'
                             BLOCK.pp_error e))
      | Ok () ->
        (* If the BLOCK sector size is big, then we need to select the 512 bytes we want *)
        let offset = Int64.(to_int (sub in_channel.offset (mul sector' (of_int sector_size)))) in
        in_channel.offset <- Int64.(add in_channel.offset (of_int len));
        Cstruct.blit tmp offset buffer 0 len;
        Lwt.return_unit
    let skip in_channel n =
      in_channel.offset <- Int64.(add in_channel.offset (of_int n));
      Lwt.return_unit
    let _get_current_tar_sector in_channel = Int64.div in_channel.offset 512L

  end
  module HR = Tar.HeaderReader(Lwt)(Reader)

  let get_partial t key ~offset ~length =
    match get_node t.map key with
    | Error e -> Lwt.return (Error e)
    | Ok (Dict _) -> Lwt.return (Error (`Value_expected key))
    | Ok (Value (hdr, start_sector)) ->
      let open Int64 in
      let sector_size = of_int t.info.Mirage_block.sector_size in
      (* Compute the unaligned data we need to read *)
      let start_bytes =
        let sec = mul start_sector 512L in
        add sec (of_int offset)
      in
      let length_bytes =
        min (sub hdr.Tar.Header.file_size (of_int offset)) (of_int length)
      in
      if length_bytes < 0L then
        Lwt.return (Ok "")
      else
        let end_bytes = add start_bytes length_bytes in
        (* Compute the starting sector and ending sector (rounding down then up) *)
        let start_sector, start_padding =
          div start_bytes sector_size, rem start_bytes sector_size
        in
        let end_sector = div (pred (add end_bytes sector_size)) sector_size in
        let n_sectors = succ (sub end_sector start_sector) in
        let buf = Cstruct.create (to_int (mul n_sectors sector_size)) in
        let tmps =
          List.init (to_int n_sectors)
            (fun sec -> Cstruct.sub buf (sec * to_int sector_size) (to_int sector_size))
        in
        BLOCK.read t.b start_sector tmps >|= function
        | Error b -> Error (`Block b)
        | Ok () ->
          let buf =
            Cstruct.sub buf (to_int start_padding) (to_int length_bytes)
          in
          Ok (Cstruct.to_string buf)

  let get t key =
    get_partial t key ~offset:0 ~length:max_int

  let list t key =
    let r = match get_node t.map key with
      | Ok (Dict (_, m)) ->
        Ok (StringMap.fold (fun key value acc ->
            match value with
            | Dict _ -> (key, `Dictionary) :: acc
            | Value _ -> (key, `Value) :: acc)
            m [])
      | Ok (Value _) -> Error (`Dictionary_expected key)
      | Error e -> Error e
    in
    Lwt.return r

  let to_day_ps hdr =
    let ts =
      match Ptime.Span.of_float_s (Int64.to_float hdr.Tar.Header.mod_time) with
      | None -> Ptime.epoch
      | Some span -> match Ptime.add_span Ptime.epoch span with
        | None -> Ptime.epoch
        | Some ts -> ts
    in
    Ptime.(Span.to_d_ps (to_span ts))

  let last_modified t key =
    let r = match get_node t.map key with
      | Ok (Dict (hdr, _)) -> Ok (to_day_ps hdr)
      | Ok (Value (hdr, _)) -> Ok (to_day_ps hdr)
      | Error e -> Error e
    in
    Lwt.return r

  let digest t key =
    get t key >|= function
    | Error e -> Error e
    | Ok data -> Ok (Digest.string data)

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

  let is_dict filename =
    String.get filename (pred (String.length filename)) = '/'

  let insert map key value =
    let rec go m = function
      | [] -> assert false
      | [hd] -> StringMap.add hd value m
      | hd::tl ->
        let hdr, m' = match StringMap.find_opt hd m with
          | None -> Tar.Header.make hd 0L, StringMap.empty
          | Some (Value _) -> assert false
          | Some (Dict (hdr, m)) -> hdr, m
        in
        let m'' = go m' tl in
        StringMap.add hd (Dict (hdr, m'')) m
    in
    go map (Mirage_kv.Key.segments key)

  let connect b =
    BLOCK.get_info b >>= fun info ->
    let in_channel = { Reader.b; offset = 0L; info } in
    let rec loop map =
      HR.read in_channel >>= function
      | Error `Eof -> Lwt.return map
      | Ok tar ->
        let filename = trim_slash tar.Tar.Header.file_name in
        let map =
          if filename = "" then
            map
          else
            let data_tar_offset = Int64.div in_channel.Reader.offset 512L in
            let v_or_d = if is_dict filename then Dict (tar, StringMap.empty) else Value (tar, data_tar_offset) in
            insert map (Mirage_kv.Key.v filename) v_or_d
        in
        Reader.skip in_channel (Int64.to_int tar.Tar.Header.file_size) >>= fun () ->
        Reader.skip in_channel (Tar.Header.compute_zero_padding_length tar) >>= fun () ->
        loop map
    in
    let root = StringMap.empty in
    loop root >>= fun map ->
    let map = Dict (Tar.Header.make "/" 0L, map) in
    Lwt.return ({ b; map; info })

  let disconnect _ = Lwt.return_unit

end
