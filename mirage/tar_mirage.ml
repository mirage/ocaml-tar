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
    mutable map: entry;
    (** offset in bytes *)
    mutable end_of_archive: int64;
    info: Mirage_block.info;
    write_lock : Lwt_mutex.t;
  }

  type key = Mirage_kv.Key.t

  type error = [ Mirage_kv.error | `Block of BLOCK.error ]

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | `Block b -> BLOCK.pp_error ppf b

  let read t sector_start buffers =
    Lwt_result.map_error (fun e -> `Block e)
      (BLOCK.read t.b sector_start buffers)

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
      | Ok (Value (e, _)) -> Ok (Optint.Int63.of_int64 e.file_size)
      | Ok (Dict (e, _)) -> Ok (Optint.Int63.of_int64 e.file_size)
      | Error e -> Error e
    in
    Lwt.return r

  module Reader = struct
    type in_channel = {
      b: BLOCK.t;
      (** offset in bytes *)
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
      | Error e -> failwith (Format.asprintf "Failed to read sector %Ld from block device: %a" sector'
                               BLOCK.pp_error e)
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

  (* [read_partial_sector t sector_start ~offset ~length dst]
     reads a single sector and blits [length] bytes from [offset] into [dst]
     with the same offset. *)
  let read_partial_sector t sector_start ~offset ~length dst =
    let length = Int64.to_int length and offset = Int64.to_int offset in
    assert (Cstruct.length dst >= t.info.sector_size);
    assert (offset + length <= t.info.sector_size);
    if length = 0 then Lwt_result.return () else
    let ( >>>= ) = Lwt_result.bind in
    let src = Cstruct.create t.info.sector_size in
    read t sector_start [ src ] >>>= fun () ->
    Cstruct.blit src offset dst offset length;
    Lwt_result.return ()

  let get_partial t key ~offset ~length =
    match get_node t.map key with
    | Error e -> Lwt.return (Error e)
    | Ok (Dict _) -> Lwt.return (Error (`Value_expected key))
    | Ok (Value (hdr, start_sector)) ->
      let open Int64 in
      let offset = Optint.Int63.to_int64 offset in
      let sector_size = of_int t.info.Mirage_block.sector_size in
      (* Compute the unaligned data we need to read *)
      let start_bytes =
        let sec = mul start_sector 512L in
        add sec offset
      in
      let length_bytes =
        min (sub hdr.Tar.Header.file_size offset)
          (of_int length)
      in
      if length_bytes < 0L then
        Lwt.return (Ok "")
      else
        let end_bytes = add start_bytes length_bytes in
        (* Compute the starting sector and ending sector (rounding down then up) *)
        let start_sector, start_padding =
          div start_bytes sector_size, rem start_bytes sector_size
        in
        let end_sector = div end_bytes sector_size in
        let n_sectors = succ (sub end_sector start_sector) in
        let buf = Cstruct.create (to_int (mul n_sectors sector_size)) in
        (* XXX: this is to work around limitations in some block implementations *)
        let tmps =
          List.init (to_int n_sectors)
            (fun sec -> Cstruct.sub buf (sec * to_int sector_size) (to_int sector_size))
        in
        read t start_sector tmps >|= function
        | Error _ as e -> e
        | Ok () ->
          let buf =
            Cstruct.sub buf (to_int start_padding) (to_int length_bytes)
          in
          Ok (Cstruct.to_string buf)

  let get t key =
    get_partial t key ~offset:Optint.Int63.zero ~length:max_int

  let list t key =
    let r = match get_node t.map key with
      | Ok (Dict (_, m)) ->
        Ok (StringMap.fold (fun sub value acc ->
            let key = Mirage_kv.Key.add key sub in
            match value with
            | Dict _ -> (key, `Dictionary) :: acc
            | Value _ -> (key, `Value) :: acc)
            m [])
      | Ok (Value _) -> Error (`Dictionary_expected key)
      | Error e -> Error e
    in
    Lwt.return r

  let to_ptime hdr =
    Option.value ~default:Ptime.epoch
      (Ptime.of_float_s (Int64.to_float hdr.Tar.Header.mod_time))

  let last_modified t key =
    let r = match get_node t.map key with
      | Ok (Dict (hdr, _)) -> Ok (to_ptime hdr)
      | Ok (Value (hdr, _)) -> Ok (to_ptime hdr)
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
    let ssize = info.Mirage_block.sector_size in
    if ssize mod 512 <> 0 || ssize < 512 then
      invalid_arg "Sector size needs to be >= 512 and a multiple of 512";
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
    (* This is after the two [zero_block]s *)
    let end_of_archive = in_channel.Reader.offset in
    let map = Dict (Tar.Header.make "/" 0L, map) in
    let write_lock = Lwt_mutex.create () in
    Lwt.return ({ b; map; info; end_of_archive; write_lock })

  let disconnect _ = Lwt.return_unit

end


module Make_KV_RW (CLOCK : Mirage_clock.PCLOCK) (BLOCK : Mirage_block.S) = struct

  include Make_KV_RO(BLOCK)

  type write_error = [ `Block of BLOCK.error | `Block_write of BLOCK.write_error | Mirage_kv.write_error | `Entry_already_exists | `Path_segment_is_a_value | `Append_only ]

  let pp_write_error ppf = function
   | `Block e -> Fmt.pf ppf "read error while writing: %a" BLOCK.pp_error e
   | `Block_write e -> BLOCK.pp_write_error ppf e
   | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e
   | `Entry_already_exists -> Fmt.string ppf "entry already exists"
   | `Path_segment_is_a_value -> Fmt.string ppf "path segment is a value"
   | `Append_only -> Fmt.string ppf "append only"
   | `Write_header msg -> Fmt.pf ppf "writing tar header failed: %s" msg

  let write t sector_start buffers =
    Lwt_result.map_error (fun e -> `Block_write e)
      (BLOCK.write t.b sector_start buffers)

  let free t =
    Int64.(sub (mul (of_int t.info.sector_size) t.info.size_sectors)
             t.end_of_archive)

  let is_safe_to_set t key =
    let rec find e path =
      match e, path with
      | (Value _ | Dict _), [] -> Error `Entry_already_exists
      | Value _, _hd :: _tl -> Error `Path_segment_is_a_value
      | Dict (_, m), hd :: tl ->
        match StringMap.find_opt hd m with
        | Some e -> find e tl
        | None ->
          (* if either (part of) the path or the file doesn't exist we're good *)
          Ok ()
    in
    find t.map (Mirage_kv.Key.segments key)

  let header_of_key ?last_modified key len =
    let mod_time =
      match last_modified with
      | Some mod_time -> Int64.of_float (Ptime.to_float_s mod_time)
      | None ->
        let ptime = Ptime.v (CLOCK.now_d_ps ()) in
        Int64.of_float (Ptime.to_float_s ptime)
    in
    Tar.Header.make ~mod_time (Mirage_kv.Key.to_string key) (Int64.of_int len)

  (* [space_needed header] is the number of bytes necessary for the data part including padding *)
  let space_needed header =
    let data_size = header.Tar.Header.file_size in
    let padding_size = Tar.Header.compute_zero_padding_length header in
    Int64.(add (of_int padding_size) data_size)

  let update_insert map key hdr offset =
    match map with
    | Value _ ->
      (* if the root is a value we have done something very wrong. This should
         be catched by [is_safe_to_set]. *)
      assert false
    | Dict (root, map) ->
      (* [insert] may raise if [key] is [empty]. However, [is_safe_to_set]
         should catch that since [empty] always exists as a dict (root). *)
      let map = insert map key (Value (hdr, offset)) in
      Dict (root, map)

  module Writer = struct
    type out_channel = {
      b: BLOCK.t;
      (** offset in bytes *)
      mutable offset: int64;
      info: Mirage_block.info;
    }
    type 'a t = 'a Lwt.t
    exception Read of BLOCK.error
    exception Write of BLOCK.write_error
    let really_write out_channel data =
      assert (Cstruct.length data <= 512);
      let data = Cstruct.(append data (create (512 - length data))) in
      let sector_size = out_channel.info.sector_size in
      let sector = Int64.(div out_channel.offset (of_int sector_size)) in
      let block = Cstruct.create sector_size in
      BLOCK.read out_channel.b sector [ block ] >>= function
      | Error e -> raise (Read e)
      | Ok () ->
        let start_offset = Int64.to_int out_channel.offset mod sector_size in
        Cstruct.blit data 0 block start_offset (Cstruct.length data);
        BLOCK.write out_channel.b sector [ block ] >>= function
        | Error e -> raise (Write e)
        | Ok () ->
          Lwt.return_unit
  end
  module HW = Tar.HeaderWriter(Lwt)(Writer)

  let set t key data =
    Lwt_mutex.with_lock t.write_lock (fun () ->
        let data = Cstruct.of_string data in
        let ( >>>= ) = Lwt_result.bind in
        let r =
          let ( let* ) = Result.bind in
          let* () = is_safe_to_set t key in
          let hdr = header_of_key key (Cstruct.length data) in
          let space_needed = space_needed hdr in
          let* () = if free t >= space_needed then Ok () else Error `No_space in
          Ok (hdr, space_needed)
        in
        Lwt.return r >>>= fun (hdr, space_needed) ->
        let open Int64 in
        let sector_size = of_int t.info.Mirage_block.sector_size in

        let data_start_bytes = sub t.end_of_archive (of_int Tar.Header.length) in
        let header_start_bytes = sub data_start_bytes (of_int Tar.Header.length) in
        let sentinel = mul 2L (of_int Tar.Header.length) in
        let end_bytes = add data_start_bytes (add space_needed sentinel) in
        (* Compute the starting sector and ending sector *)
        let data_start_sector, data_start_sector_offset =
          div data_start_bytes sector_size,
          rem data_start_bytes sector_size
        in
        let end_sector = div (add end_bytes sector_size) sector_size in
        let last_sector_offset = rem end_bytes sector_size in
        let pad = Tar.Header.compute_zero_padding_length hdr in

        let data = Cstruct.concat [
            Cstruct.create (to_int data_start_sector_offset);
            data;
            Cstruct.create (pad + to_int sentinel);
            (if last_sector_offset = 0L then
               Cstruct.empty
             else
               Cstruct.create (to_int (sub sector_size last_sector_offset)));
          ]
        in
        (* [data] is always at least one sector as the sentinel is always present *)
        let first_sector, remaining_sectors = Cstruct.split data t.info.sector_size in
        let last_sector =
          (* sub on whole [data] as the first sector and last sector might be the same *)
          Cstruct.sub data
            (Stdlib.max 0 (Cstruct.length data - t.info.sector_size))
            t.info.sector_size
        in
        (* blit in slack at the end *)
        read_partial_sector t (max data_start_sector (pred end_sector))
          ~offset:last_sector_offset last_sector
          ~length:(sub sector_size last_sector_offset) >>>= fun () ->
        (* to write robustly as we can:
           - we write sectors 2..n,
           - then the header,
           - then we blit the first (data) sector as it may contain the header,
           - finally we write the first (data) sector which contains the first tar data block.
        *)
        let remaining_sectors =
          (* XXX: this is to work around limitations in some block implementations *)
          List.init (Cstruct.length remaining_sectors / to_int sector_size)
            (fun sector ->
               Cstruct.sub remaining_sectors
                 (sector * to_int sector_size)
                 (to_int sector_size))
        in
        write t (succ data_start_sector) remaining_sectors >>>= fun () ->
        (* finally write header and first block *)
        let hw = Writer.{ b = t.b ; offset = header_start_bytes ; info = t.info } in
        (* it is important we write at level [Ustar] at most as we assume the
           header(s) taking up exactly 512 bytes. With [GNU] level extra blocks
           may be used for long names. *)
        Lwt.catch
          (fun () -> HW.write ~level:Tar.Header.Ustar hdr hw >|= fun () -> Ok ())
          (function
            | Writer.Read e -> Lwt.return (Error (`Block e))
            | Writer.Write e -> Lwt.return (Error (`Block_write e))
            | exn -> raise exn) >>>= fun () ->
        (* read in slack at beginning which could include the header *)
        read_partial_sector t data_start_sector first_sector
          ~offset:0L ~length:data_start_sector_offset >>>= fun () ->
        write t data_start_sector [ first_sector ] >>>= fun () ->
        let tar_offset = Int64.div (sub t.end_of_archive 512L) 512L in
        t.end_of_archive <- end_bytes;
        t.map <- update_insert t.map key hdr tar_offset;
        Lwt.return (Ok ()))

  let remove _ _ =
    Lwt.return (Error `Append_only)

  let rename _ ~source:_ ~dest:_ =
    Lwt.return (Error `Append_only)

  let set_partial t key ~offset data =
    let ( >>>= ) = Lwt_result.bind in
    if Optint.Int63.(compare offset zero < 0) then
      invalid_arg "Tar_mirage.set_partial: negative offset";
    Lwt.return begin match get_node t.map key with
      | Ok Value (hdr, offset) -> Ok (hdr, offset)
      | Ok Dict _ -> Error `Path_segment_is_a_value
      | Error _ as e -> e
    end >>>= fun (hdr, data_offset) ->
    (* FIXME: check more thoroughly offset *)
    let open Int64 in
    let offset = Optint.Int63.to_int64 offset in
    let end_bytes = add offset (of_int (String.length data)) in
    begin if hdr.file_size < end_bytes then
        Lwt_result.fail `Append_only
      else
        Lwt_result.return ()
    end >>>= fun () ->
    let start_bytes = add data_offset hdr.file_size
    and end_bytes = add data_offset end_bytes in
    let sector_size = of_int t.info.sector_size in
    let start_sector_offset = rem start_bytes sector_size in
    let end_sector = div (add end_bytes sector_size) sector_size in
    let last_sector_offset = rem end_bytes sector_size in

    let data' =
      let len =
        add start_sector_offset
          (add (of_int (String.length data))
             (if last_sector_offset = 0L then
                0L
              else
                sub sector_size last_sector_offset))
      in
      Cstruct.create (to_int len)
    in
    read_partial_sector t (div start_bytes sector_size) data'
      ~offset:0L ~length:start_sector_offset >>>= fun () ->
    let last_sector =
      Cstruct.sub data' (Cstruct.length data' - t.info.sector_size)
        t.info.sector_size
    in
    read_partial_sector t (pred end_sector) last_sector
      ~offset:last_sector_offset
      ~length:(sub sector_size last_sector_offset) >>>= fun () ->
    let data' =
      List.init (Cstruct.length data' / t.info.sector_size)
        (fun sector ->
           Cstruct.sub data' (sector * t.info.sector_size) t.info.sector_size)
    in
    write t (div start_bytes sector_size) data'

  let allocate t key ?last_modified size =
    Lwt_mutex.with_lock t.write_lock (fun () ->
        let ( >>>= ) = Lwt_result.bind in
        let r =
          let ( let* ) = Result.bind in
          (* XXX: map `Entry_already_exists to `Append_only ?! *)
          let* () = is_safe_to_set t key in
          let hdr = header_of_key ?last_modified key (Optint.Int63.to_int size) in
          let space_needed = space_needed hdr in
          let* () = if free t >= space_needed then Ok () else Error `No_space in
          Ok (hdr, space_needed)
        in
        Lwt.return r >>>= fun (hdr, space_needed) ->
        let open Int64 in
        let sector_size = of_int t.info.Mirage_block.sector_size in
        let header_start_bytes =
          sub t.end_of_archive (of_int (2 * Tar.Header.length))
        in
        let to_zero_start_bytes = t.end_of_archive in
        let end_bytes = add t.end_of_archive
            (add space_needed (of_int Tar.Header.length)) in
        (* Compute the starting sector and ending sector *)
        let to_zero_start_sector, to_zero_start_sector_offset =
          div to_zero_start_bytes sector_size,
          rem to_zero_start_bytes sector_size
        in
        let last_sector_offset = rem end_bytes sector_size in
        let end_sector = div (add end_bytes sector_size) sector_size in
        let num_sectors = to_int (min (sub end_sector to_zero_start_sector) 0L) in

        let zero_sector = Cstruct.create t.info.Mirage_block.sector_size in
        let data = Array.init num_sectors (fun _ -> zero_sector) in
        let copy_cstruct c =
          let c' = Cstruct.create (Cstruct.length c) in
          Cstruct.blit c 0 c' 0 (Cstruct.length c);
          c'
        in
        (* read first and last sector, if necessary *)
        begin if num_sectors = 0 then
            Lwt_result.return ()
          else begin
            begin if to_zero_start_sector_offset <> 0L then
                let () = data.(0) <- copy_cstruct data.(0) in
                read_partial_sector t to_zero_start_sector data.(0)
                  ~offset:0L
                  ~length:to_zero_start_sector_offset
              else
                Lwt_result.return ()
            end >>>= fun () ->
            begin if last_sector_offset <> 0L then
                let last = copy_cstruct data.(num_sectors - 1) in
                let () = data.(num_sectors - 1 ) <- last in
                read_partial_sector t (pred end_sector) last
                  ~offset:last_sector_offset
                  ~length:(sub sector_size last_sector_offset)
              else
                Lwt_result.return ()
            end
          end
        end >>>= fun () ->
        write t to_zero_start_sector (Array.to_list data) >>>= fun () ->
        let hw = Writer.{ b = t.b ; offset = header_start_bytes ; info = t.info } in
        Lwt.catch
          (fun () -> HW.write ~level:Tar.Header.Ustar hdr hw >|= fun () -> Ok ())
          (function
            | Writer.Read e -> Lwt.return (Error (`Block e))
            | Writer.Write e -> Lwt.return (Error (`Block_write e))
            | exn -> raise exn) >>>= fun () ->
        let tar_offset = div (sub t.end_of_archive 512L) 512L in
        t.end_of_archive <- end_bytes;
        t.map <- update_insert t.map key hdr tar_offset;
        Lwt.return (Ok ()))
end
