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

let rec safe f a =
  try Ok (f a) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe f a
  | Unix.Unix_error (e, f, s) -> Error (`Unix (e, f, s))

let safe_close fd =
  try Unix.close fd with _ -> ()

let read_complete fd buf len =
  let rec loop offset =
    if offset < len then
      let* n = safe (Unix.read fd buf offset) (len - offset) in
      if n = 0 then
        Error `Unexpected_end_of_file
      else
        loop (offset + n)
    else
      Ok ()
  in
  loop 0

let seek fd n =
  safe (Unix.lseek fd n) Unix.SEEK_CUR

type decode_error = [
  | `Fatal of [ `Checksum_mismatch | `Corrupt_pax_header | `Unmarshal of string ]
  | `Unix of Unix.error * string * string
  | `Unexpected_end_of_file
  | `Msg of string
]

let pp_decode_error ppf = function
  | `Fatal err -> Tar.pp_error ppf err
  | `Unix (err, fname, arg) ->
    Format.fprintf ppf "Unix error %s (function %s, arg %s)"
      (Unix.error_message err) fname arg
  | `Unexpected_end_of_file ->
    Format.fprintf ppf "Unexpected end of file"
  | `Msg msg ->
    Format.fprintf ppf "Error %s" msg

let fold f filename init =
  let* fd = safe Unix.(openfile filename [ O_RDONLY ]) 0 in
  let rec go t fd ?global ?data acc =
    let* data = match data with
      | None ->
        let buf = Bytes.make Tar.Header.length '\000' in
        let* () = read_complete fd buf Tar.Header.length in
        Ok (Bytes.unsafe_to_string buf)
      | Some data -> Ok data
    in
    match Tar.decode t data with
    | Ok (t, Some `Header hdr, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let* acc' = f fd ?global hdr acc in
      let* _off = seek fd (Tar.Header.compute_zero_padding_length hdr) in
      go t fd ?global acc'
    | Ok (t, Some `Skip n, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let* _off = seek fd n in
      go t fd ?global acc
    | Ok (t, Some `Read n, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      let buf = Bytes.make n '\000' in
      let* () = read_complete fd buf n in
      let data = Bytes.unsafe_to_string buf in
      go t fd ?global ~data acc
    | Ok (t, None, g) ->
      let global = Option.fold ~none:global ~some:(fun g -> Some g) g in
      go t fd ?global acc
    | Error `Eof -> Ok acc
    | Error `Fatal _ as e -> e
  in
  Fun.protect
    ~finally:(fun () -> safe_close fd)
    (fun () -> go (Tar.decode_state ()) fd init)

let unix_err_to_msg = function
  | `Unix (e, f, s) ->
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
            | `Unix _ as e -> unix_err_to_msg e
            | `Unexpected_end_of_file ->
              `Msg "Unexpected end of file")
          (read_complete src_fd buffer l)
      in
      let* _written =
        Result.map_error unix_err_to_msg
          (safe (Unix.write dst_fd buffer 0) l)
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
          Result.map_error unix_err_to_msg
            (safe Unix.(openfile (Filename.concat dst hdr.Tar.Header.file_name)
                          [ O_WRONLY ; O_CREAT ]) hdr.Tar.Header.file_mode)
        in
        Fun.protect ~finally:(fun () -> safe_close dst)
          (fun () -> copy ~src_fd:fd ~dst_fd:dst (Int64.to_int hdr.Tar.Header.file_size))
        (* TODO set owner / mode / mtime etc. *)
      | _ ->
        (* TODO handle directories, links, etc. *)
        let* _off =
          Result.map_error unix_err_to_msg
            (seek fd (Int64.to_int hdr.Tar.Header.file_size))
        in
        Ok ()
    else
      let* _off =
        Result.map_error unix_err_to_msg
          (seek fd (Int64.to_int hdr.Tar.Header.file_size))
      in
      Ok ()
  in
  fold f src ()

(** Return the header needed for a particular file on disk *)
let header_of_file ?level file =
  let level = Tar.Header.compatibility level in
  let* stat = safe Unix.LargeFile.lstat file in
  let file_mode = stat.Unix.LargeFile.st_perm in
  let user_id = stat.Unix.LargeFile.st_uid in
  let group_id = stat.Unix.LargeFile.st_gid in
  let mod_time = Int64.of_float stat.Unix.LargeFile.st_mtime in
  (* TODO evaluate stat.st_kind *)
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let* uname =
    if level = V7 then
      Ok ""
    else
      try
        let* passwd_entry = safe Unix.getpwuid stat.Unix.LargeFile.st_uid in
        Ok passwd_entry.Unix.pw_name
      with Not_found -> Error (`Msg ("No user entry found for UID"))
  in
  let devmajor = if level = Ustar then stat.Unix.LargeFile.st_dev else 0 in
  let* gname =
    if level = V7 then
      Ok ""
    else
      try
        let* passwd_entry = safe Unix.getgrgid stat.Unix.LargeFile.st_gid in
        Ok passwd_entry.Unix.gr_name
      with Not_found -> Error (`Msg "No group entry found for GID")
  in
  let devminor = if level = Ustar then stat.Unix.LargeFile.st_rdev else 0 in
  Ok (Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
        ~uname ~gname ~devmajor ~devminor file stat.Unix.LargeFile.st_size)

let append_file ?level ?header filename fd =
  let* header = match header with
    | None -> header_of_file ?level filename
    | Some x -> Ok x
  in
  let* header_strings = Tar.encode_header ?level header in
  let* _written =
    List.fold_left (fun acc d ->
        let* _written = acc in
        Result.map_error unix_err_to_msg
          (safe (Unix.write_substring fd d 0) (String.length d)))
      (Ok 0) header_strings
  in
  let* src =
    Result.map_error unix_err_to_msg
      (safe Unix.(openfile filename [ O_RDONLY ]) 0)
  in
  (* TOCTOU [also, header may not be valid for file] *)
  Fun.protect ~finally:(fun () -> safe_close src)
    (fun () -> copy ~src_fd:src ~dst_fd:fd
        (Int64.to_int header.Tar.Header.file_size))

let write_global_extended_header ?level header fd =
  let* header_strings = Tar.encode_global_extended_header ?level header in
  let* _written =
    List.fold_left (fun acc d ->
        let* _written = acc in
        Result.map_error unix_err_to_msg
          (safe (Unix.write_substring fd d 0) (String.length d)))
      (Ok 0) header_strings
  in
  Ok ()

let write_end fd =
  let* _written =
    Result.map_error unix_err_to_msg
      (safe
         (Unix.write_substring fd (Tar.Header.zero_block ^ Tar.Header.zero_block) 0)
         (Tar.Header.length + Tar.Header.length))
  in
  Ok ()

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  let* dst_fd =
    Result.map_error unix_err_to_msg
      (safe Unix.(openfile dst [ O_WRONLY ; O_CREAT ]) 0o644)
  in
  Fun.protect ~finally:(fun () -> safe_close dst_fd)
    (fun () ->
       let* () = match global with
         | None -> Ok ()
         | Some hdr -> write_global_extended_header ?level hdr dst_fd
       in
       let rec copy_files directory =
         let* dir = safe Unix.opendir directory in
         Fun.protect ~finally:(fun () -> try Unix.closedir dir with _ -> ())
           (fun () ->
              let rec next () =
                try
                  let* name = safe Unix.readdir dir in
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
