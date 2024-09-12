(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

type decode_error = [
  | `Fatal of Tar.error
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

let safe f a =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> f a >|= fun r -> Ok r)
    (function
      | Unix.Unix_error (e, f, a) -> Lwt.return (Error (`Unix (e, f, a)))
      | e -> Lwt.reraise e)

let read_complete fd buf len =
  let open Lwt_result.Infix in
  let rec loop offset =
    if offset < len then
      safe (Lwt_unix.read fd buf offset) (len - offset) >>= fun read ->
      if read = 0 then
        Lwt.return (Error `Unexpected_end_of_file)
      else
        loop (offset + read)
    else
      Lwt.return (Ok ())
  in
  loop 0

let seek fd n =
  safe (Lwt_unix.lseek fd n) Unix.SEEK_CUR
  |> Lwt_result.map ignore

let safe_close fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

module High : sig
  type t
  type 'a s = 'a Lwt.t

    external inj : 'a s -> ('a, t) Tar.io = "%identity"
    external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a Lwt.t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type t = High.t

let value v = Tar.High (High.inj v)

let run t fd =
  let open Lwt_result.Infix in
  let rec run : type a. (a, [> decode_error ] as 'err, t) Tar.t -> (a, 'err) result Lwt.t = function
    | Tar.Write str ->
      safe (Lwt_unix.write_string fd str 0) (String.length str) >>= fun _write ->
      Lwt_result.return ()
    | Tar.Read len ->
      let b = Bytes.make len '\000' in
      safe (Lwt_unix.read fd b 0) len >>= fun read ->
      if read = 0 then
        Lwt_result.fail `Unexpected_end_of_file
      else if len = read then
        Lwt_result.return (Bytes.unsafe_to_string b)
      else
        Lwt_result.return (Bytes.sub_string b 0 read)
    | Tar.Really_read len ->
      let buf = Bytes.make len '\000' in
      read_complete fd buf Tar.Header.length >|= fun () ->
      Bytes.unsafe_to_string buf
    | Tar.Seek len -> seek fd len
    | Tar.Return value -> Lwt.return value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) ->
      run x >>= fun value -> run (f value) in
  run t

let with_in filename f =
  let open Lwt_result.Infix in
  safe Lwt_unix.(openfile filename [ O_RDONLY ]) 0 >>= fun fd ->
  Lwt.finalize (fun () -> f fd) (fun () -> safe_close fd)

let fold f filename init =
  with_in filename (fun fd -> run (Tar.fold f init) fd)

let fold_gz f filename init =
  with_in filename (fun fd -> run (Tar_gz.in_gzipped (Tar.fold f init)) fd)

let unix_err_to_msg = function
  | `Unix (e, f, s) ->
    `Msg (Format.sprintf "error %s in function %s %s"
            (Unix.error_message e) f s)

let copy ~dst_fd len =
  let blen = 65536 in
  let rec read_write ~dst_fd len =
    if len = 0 then value (Lwt.return (Ok ()))
    else
      let ( let* ) = Tar.( let* ) in
      let slen = min blen len in
      let* str = Tar.really_read slen in
      let* _written = Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write_string dst_fd str 0) slen) |> value in
      read_write ~dst_fd (len - slen)
  in
  read_write ~dst_fd len

let extract ~filter dst =
  let safe_close fd =
    let open Lwt.Infix in
    safe_close fd >|= Result.ok in
  let f ?global:_ hdr () =
    let ( let* ) = Tar.( let* ) in
    match filter hdr, hdr.Tar.Header.link_indicator with
    | true, Tar.Header.Link.Normal ->
      let* dst = Lwt_result.map_error
        unix_err_to_msg
        (safe Lwt_unix.(openfile (Filename.concat dst hdr.Tar.Header.file_name) [ O_WRONLY; O_CREAT ]) hdr.Tar.Header.file_mode)
        |> value in
      begin try
        let* () = copy ~dst_fd:dst (Int64.to_int hdr.Tar.Header.file_size) in
        let* () = value (safe_close dst) in
        Tar.return (Ok ())
      with exn ->
        let* () = value (safe_close dst) in
        Tar.return (Error (`Exn exn))
      end
    | _ ->
      let* () = Tar.seek (Int64.to_int hdr.Tar.Header.file_size) in
      Tar.return (Ok ())
  in
  Tar.fold f ()

let extract ?(filter = fun _ -> true) ~src dst =
  with_in src (fun fd -> run (extract ~filter dst) fd)
and extract_gz ?(filter = fun _ -> true) ~src dst =
  with_in src (fun fd -> run (Tar_gz.in_gzipped (extract ~filter dst)) fd)

(** Return the header needed for a particular file on disk *)
let header_of_file ?level file =
  let open Lwt_result.Infix in
  let level = Tar.Header.compatibility level in
  safe Lwt_unix.LargeFile.stat file >>= fun stat ->
  let file_mode = stat.Lwt_unix.LargeFile.st_perm in
  let user_id = stat.Lwt_unix.LargeFile.st_uid in
  let group_id = stat.Lwt_unix.LargeFile.st_gid in
  let file_size = stat.Lwt_unix.LargeFile.st_size in
  let mod_time = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  (if level = V7 then
     Lwt.return (Ok "")
   else
     Lwt.catch
       (fun () -> safe Lwt_unix.getpwuid stat.Lwt_unix.LargeFile.st_uid)
       (function
         | Not_found ->
           Lwt.return (Error (`Msg ("No user entry found for UID")))
         | e -> Lwt.reraise e) >|= fun pwent ->
     pwent.Lwt_unix.pw_name) >>= fun uname ->
  (if level = V7 then
     Lwt.return (Ok "")
   else
     Lwt.catch
       (fun () -> safe Lwt_unix.getgrgid stat.Lwt_unix.LargeFile.st_gid)
       (function
         | Not_found ->
           Lwt.return (Error (`Msg ("No group entry found for GID")))
         | e -> Lwt.reraise e) >|= fun grent ->
     grent.Lwt_unix.gr_name) >>= fun gname ->
  let devmajor = if level = Ustar then stat.Lwt_unix.LargeFile.st_dev else 0 in
  let devminor = if level = Ustar then stat.Lwt_unix.LargeFile.st_rdev else 0 in
  let hdr = Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
      ~uname ~gname ~devmajor ~devminor file file_size
  in
  Lwt.return (Ok hdr)

let write_strings fd datas =
  let open Lwt_result.Infix in
  Lwt_list.fold_left_s (fun acc d ->
      Lwt_result.lift acc >>= fun _written ->
      Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write_string fd d 0) (String.length d)))
    (Ok 0) datas >|= fun _written ->
  ()

let write_header ?level header fd =
  let open Lwt_result.Infix in
  Lwt_result.lift (Tar.encode_header ?level header) >>= fun header_strings ->
  write_strings fd header_strings

let copy ~src_fd ~dst_fd len =
  let open Lwt_result.Infix in
  let blen = 65536 in
  let buffer = Bytes.make blen '\000' in
  let rec read_write ~src_fd ~dst_fd len =
    if len = 0 then
      Lwt.return (Ok ())
    else
      let l = min blen len in
      Lwt_result.map_error
        (function
          | `Unix _ as e -> unix_err_to_msg e
          | `Unexpected_end_of_file ->
            `Msg "Unexpected end of file")
        (read_complete src_fd buffer l) >>= fun () ->
      Lwt_result.map_error unix_err_to_msg
        (safe (Lwt_unix.write dst_fd buffer 0) l) >>= fun _written ->
      read_write ~src_fd ~dst_fd (len - l)
  in
  read_write ~src_fd ~dst_fd len

let append_file ?level ?header filename fd =
  let open Lwt_result.Infix in
  (match header with
   | None -> header_of_file ?level filename
   | Some x -> Lwt.return (Ok x)) >>= fun header ->
  write_header ?level header fd >>= fun () ->
  Lwt_result.map_error unix_err_to_msg
    (safe Lwt_unix.(openfile filename [ O_RDONLY ]) 0) >>= fun src ->
  (* TOCTOU [also, header may not be valid for file] *)
  Lwt.finalize
    (fun () -> copy ~src_fd:src ~dst_fd:fd
        (Int64.to_int header.Tar.Header.file_size))
    (fun () -> safe_close src)

let write_global_extended_header ?level header fd =
  let open Lwt_result.Infix in
  Lwt_result.lift (Tar.encode_global_extended_header ?level header) >>= fun header_strings ->
  write_strings fd header_strings

let write_end fd =
  write_strings fd [ Tar.Header.zero_block ; Tar.Header.zero_block ]

let header_of_stat level stat link_indicator file =
  let file_mode = stat.Lwt_unix.LargeFile.st_perm in
  let user_id = stat.Lwt_unix.LargeFile.st_uid in
  let group_id = stat.Lwt_unix.LargeFile.st_gid in
  let file_size =
    match link_indicator with
    | Tar.Header.Link.Normal -> stat.Lwt_unix.LargeFile.st_size
    | _ ->
      (* XXX: assumes Tar.Header.Link.Directory *)
      0L
  in
  let mod_time = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
  let link_name = "" in
  (* TODO: the following uses potentially block getpwuid() and getgrid() *)
  let uname =
    if level = Tar.Header.V7 then
      ""
    else try
        (Unix.getpwuid stat.Lwt_unix.LargeFile.st_uid).Unix.pw_name
      with Not_found -> ""
  in
  let gname =
    if level = Tar.Header.V7 then
      ""
    else try
        (Unix.getgrgid stat.Lwt_unix.LargeFile.st_gid).Unix.gr_name
      with Not_found -> ""
  in
  let devmajor = if level = Tar.Header.Ustar then stat.Lwt_unix.LargeFile.st_dev else 0 in
  let devminor = if level = Tar.Header.Ustar then stat.Lwt_unix.LargeFile.st_rdev else 0 in
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
    ~uname ~gname ~devmajor ~devminor file file_size

let create ~level ~global ~filter ~src =
  let ( let* ) = Tar.( let* ) in
  let entries =
    let contents_of_path path =
      let fd = ref `None in
      let buf = Bytes.create 0x100 in
      let rec dispenser () = match !fd with
        | `Closed -> Tar.return (Ok None)
        | `None ->
          let fd' = Unix.openfile path Unix.[ O_RDONLY; O_CLOEXEC ] 0o644 in
          fd := `Active fd';
          dispenser ()
        | `Active fd' ->
          match Unix.read fd' buf 0 (Bytes.length buf) with
          | 0 | exception End_of_file ->
            Unix.close fd'; fd := `Closed; Tar.return (Ok None)
          | len ->
            let str = Bytes.sub_string buf 0 len in
            Tar.return (Ok (Some str)) in
      dispenser
    in
    let level = Tar.Header.compatibility level in
    let pending = ref [`Filename src] in
    let rec entries () =
      match !pending with
      | [] -> Tar.return (Ok None)
      | `Filename hd :: remaining ->
        pending := remaining;
        entry hd
      | `Dir_handle (parent, dir_handle) :: remaining ->
        let* next =
          value
            (safe (fun () ->
                 (Lwt.catch (fun () ->
                      Lwt_unix.readdir dir_handle
                      |> Lwt.map (fun child ->
                        Some (Filename.concat parent child)))
                   (function Not_found -> Lwt.return_none | e -> Lwt.reraise e)))
                ())
        in
        match next with
        | None ->
          let* () = value (safe Lwt_unix.closedir dir_handle) in
          pending := remaining;
          entries ()
        | Some f ->
          entry f
    and entry f =
      let* stat = value (safe Lwt_unix.LargeFile.stat f) in
      match stat.Lwt_unix.LargeFile.st_kind with
      | Unix.S_REG ->
        let hdr = header_of_stat level stat Tar.Header.Link.Normal f in
        if filter hdr then
          Tar.return (Ok (Some (Some level, hdr, contents_of_path f)))
        else
          entries ()
      | Unix.S_DIR ->
        let hdr = header_of_stat level stat Tar.Header.Link.Directory f in
        if filter hdr then
          let* dir_handle = value (safe Lwt_unix.opendir f) in
          pending :=  `Dir_handle (f, dir_handle) :: !pending;
          Tar.return (Ok (Some (Some level, hdr, (fun () -> Tar.return (Ok None)))))
        else
          entries ()
      | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
        (* silently(?!) skip these special files *)
        entries ()
    in
    entries
  in
  Tar.out ?level ?global_hdr:global entries

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  let open Lwt_result.Infix in
  safe Lwt_unix.(openfile dst [ O_CREAT ; O_WRONLY ; O_TRUNC ]) 0o644 >>= fun fd ->
  Lwt.finalize (fun () -> run (create ~level ~global ~filter ~src) fd)
    (fun () -> safe_close fd)
and create_gz ?level ?global ?(filter = fun _ -> true) ?(gz_level = 9) ~gz_mtime:mtime ~src dst =
  let open Lwt_result.Infix in
  safe Lwt_unix.(openfile dst [ O_CREAT ; O_WRONLY ; O_TRUNC ]) 0o644 >>= fun fd ->
  let os = match Sys.os_type with
    | "Win32" -> Gz.NTFS (* XXX(dinosaure): true? *)
    | "Unix" | "Cygwin" | _ -> Gz.Unix in
  let tar_t =
    Tar_gz.out_gzipped ~level:gz_level ~mtime os
      (create ~level ~global ~filter ~src)
  in
  Lwt.finalize (fun () -> run tar_t fd) (fun () -> safe_close fd)
