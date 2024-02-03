(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
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

let ( let* ) = Result.bind

let rec safe ~off f a =
  try Ok (f a) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe ~off f a
  | Unix.Unix_error (e, f, s) -> Error (`Unix (off, e, f, s))

let safe_close fd =
  try Unix.close fd with _ -> ()

let read_complete ~off fd buf len =
  let rec loop offset =
    if offset < len then
      let* n = safe ~off (Unix.read fd buf offset) (len - offset) in
      if n = 0 then
        Error (`Unexpected_end_of_file off)
      else
        loop (offset + n)
    else
      Ok ()
  in
  loop 0

let seek ~off fd n =
  safe ~off (Unix.lseek fd n) Unix.SEEK_CUR

type decode_error = [
  | `Fatal of int * [ `Checksum_mismatch | `Corrupt_pax_header | `Unmarshal of string ]
  | `Unix of int * Unix.error * string * string
  | `Unexpected_end_of_file of int
  | `Msg of int * string
]

let pp_decode_error ppf = function
  | `Fatal (off, err) ->
    Format.fprintf ppf "Offset %u, %a" off Tar.pp_error err
  | `Unix (off, err, fname, arg) ->
    Format.fprintf ppf "Offset %u, Unix error %s (function %s, arg %s)" off
      (Unix.error_message err) fname arg
  | `Unexpected_end_of_file off ->
    Format.fprintf ppf "Offset %u unexpected end of file" off
  | `Msg (off, msg) ->
    Format.fprintf ppf "Offset %u error %s" off msg

let fold f filename init =
  let* fd = safe ~off:0 Unix.(openfile filename [ O_RDONLY ]) 0 in
  let rec go ~off t fd ?global ?data acc =
    let* data = match data with
      | None ->
        let buf = Bytes.make Tar.Header.length '\000' in
        let* () = read_complete ~off fd buf Tar.Header.length in
        Ok (Bytes.unsafe_to_string buf)
      | Some data -> Ok data
    in
    match Tar.decode t data with
    | Ok (t, Some `Header hdr, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let* acc' =
        Result.map_error
          (fun (`Msg s) -> `Msg (off, s))
          (f fd ?global hdr acc)
      in
      let* off = seek ~off fd (Tar.Header.compute_zero_padding_length hdr) in
      go ~off t fd ?global acc'
    | Ok (t, Some `Skip n, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let* off = seek ~off fd n in
      go ~off t fd ?global acc
    | Ok (t, Some `Read n, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let buf = Bytes.make n '\000' in
      let* () = read_complete ~off fd buf n in
      let data = Bytes.unsafe_to_string buf in
      go ~off:(off + n) t fd ?global ~data acc
    | Ok (t, None, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      go ~off t fd ?global acc
    | Error `Eof -> Ok acc
    | Error `Fatal e -> Error (`Fatal (off, e))
  in
  Fun.protect
    ~finally:(fun () -> safe_close fd)
    (fun () -> go ~off:0 (Tar.decode_state ()) fd init)

let map_to_msg = function
  | `Unix (_off, e, f, s) ->
    `Msg (Format.sprintf "error %s in function %s %s"
            (Unix.error_message e) f s)

let copy ~src_fd ~dst_fd len =
  let blen = 65536 in
  let buffer = Bytes.make blen '\000' in
  let rec read_write ~src_fd ~dst_fd len =
    if len = 0 then
      Ok ()
    else
      let l = min blen len in
      let* () =
        Result.map_error
          (function
            | `Unix _ as e -> map_to_msg e
            | `Unexpected_end_of_file _off ->
              `Msg ("Unexpected end of file"))
          (read_complete ~off:0 src_fd buffer l)
      in
      let* _written =
        Result.map_error map_to_msg
          (safe ~off:0 (Unix.write dst_fd buffer 0) l)
      in
      read_write ~src_fd ~dst_fd (len - l)
  in
  read_write ~src_fd ~dst_fd len

let extract ?(filter = fun _ -> true) ~src dst =
  let f fd ?global:_ hdr () =
    if filter hdr then
      match hdr.Tar.Header.link_indicator with
      | Tar.Header.Link.Normal ->
        let* dst =
          Result.map_error map_to_msg
            (safe ~off:0 Unix.(openfile (Filename.concat dst hdr.Tar.Header.file_name)
                                 [ O_WRONLY ; O_CREAT ]) hdr.Tar.Header.file_mode)
        in
        Fun.protect ~finally:(fun () -> safe_close dst)
          (fun () -> copy ~src_fd:fd ~dst_fd:dst (Int64.to_int hdr.Tar.Header.file_size))
      (* TODO set owner / mode / mtime etc. *)
      | _ -> Error (`Msg "not yet handled")
    else
      let* _off =
        Result.map_error (fun (`Unix (_off, e, f, s)) ->
            `Msg (Format.sprintf "error %s in function %s %s"
                    (Unix.error_message e) f s))
          (seek ~off:0 fd (Int64.to_int hdr.Tar.Header.file_size))
      in
      Ok ()
  in
  fold f src ()


(** Return the header needed for a particular file on disk *)
let header_of_file ?level file =
  let level = Tar.Header.compatibility level in
  let* stat = safe ~off:0 Unix.LargeFile.lstat file in
  let file_mode = stat.Unix.LargeFile.st_perm in
  let user_id = stat.Unix.LargeFile.st_uid in
  let group_id = stat.Unix.LargeFile.st_gid in
  let mod_time = Int64.of_float stat.Unix.LargeFile.st_mtime in
  (* TODO evaluate stat.st_kind *)
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let uname = if level = V7 then "" else (Unix.getpwuid stat.Unix.LargeFile.st_uid).Unix.pw_name in
  let devmajor = if level = Ustar then stat.Unix.LargeFile.st_dev else 0 in
  let gname = if level = V7 then "" else (Unix.getgrgid stat.Unix.LargeFile.st_gid).Unix.gr_name in
  let devminor = if level = Ustar then stat.Unix.LargeFile.st_rdev else 0 in
  Ok (Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
        ~uname ~gname ~devmajor ~devminor file stat.Unix.LargeFile.st_size)

let append_file ?level ?header filename fd =
  let* header = match header with
    | None -> header_of_file ?level filename
    | Some x -> Ok x
  in
  let* header_strings = Tar.encode_header ?level header in
  let* _off =
    List.fold_left (fun acc d ->
        let* _off = acc in
        Result.map_error map_to_msg
          (safe ~off:0 (Unix.write_substring fd d 0) (String.length d)))
      (Ok 0) header_strings
  in
  let* src =
    Result.map_error (fun (`Unix (_off, e, f, s)) ->
        `Msg (Format.sprintf "error %s in function %s %s"
                (Unix.error_message e) f s))
      (safe ~off:0 Unix.(openfile filename [ O_RDONLY ]) 0)
  in
  (* TOCTOU [also, header may not be valid for file] *)
  Fun.protect ~finally:(fun () -> safe_close src)
    (fun () -> copy ~src_fd:src ~dst_fd:fd
        (Int64.to_int header.Tar.Header.file_size))

let write_global_extended_header ?level header fd =
  let* header_strings = Tar.encode_global_extended_header ?level header in
  let* _off =
    List.fold_left (fun acc d ->
        let* _off = acc in
        Result.map_error map_to_msg
          (safe ~off:0 (Unix.write_substring fd d 0) (String.length d)))
      (Ok 0) header_strings
  in
  Ok ()

let write_end fd =
  let* _written =
    Result.map_error map_to_msg
      (safe ~off:0
         (Unix.write_substring fd (Tar.Header.zero_block ^ Tar.Header.zero_block) 0)
         (Tar.Header.length + Tar.Header.length))
  in
  Ok ()

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  let* dst_fd =
    Result.map_error map_to_msg
      (safe ~off:0 Unix.(openfile dst [ O_WRONLY ; O_CREAT ])
         0o644)
  in
  Fun.protect ~finally:(fun () -> safe_close dst_fd)
    (fun () ->
       let* () = match global with
         | None -> Ok ()
         | Some hdr ->
           write_global_extended_header ?level hdr dst_fd
       in
       let rec copy_files directory =
         let* dir = safe ~off:0 Unix.opendir directory in
         Fun.protect ~finally:(fun () -> try Unix.closedir dir with _ -> ())
           (fun () ->
              let rec next () =
                try
                  let* name = safe ~off:0 Unix.readdir dir in
                  let filename = Filename.concat directory name in
                  let* header = header_of_file ?level filename in
                  if filter header then
                    match header.Tar.Header.link_indicator with
                    | Normal ->
                      let* () = append_file ?level ~header filename dst_fd in
                      next ()
                    | Directory ->
                      (* TODO first finish curdir (and close the dir fd), then go deeper *)
                      let* () = copy_files filename in
                      next ()
                    | _ -> Ok () (* NYI *)
                  else Ok ()
                with End_of_file -> Ok ()
              in
              next ())
       in
       let* () = copy_files src in
       write_end dst_fd)
