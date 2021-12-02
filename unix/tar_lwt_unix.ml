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

open Lwt.Infix

module Reader = struct
  type in_channel = Lwt_unix.file_descr
  type 'a t = 'a Lwt.t
  let really_read fd = Lwt_cstruct.(complete (read fd))
  let skip (ifd: Lwt_unix.file_descr) (n: int) =
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n: int) =
      if n <= 0 then Lwt.return_unit
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        really_read ifd block >>= fun () ->
        loop (n - amount) in
    loop n
end
let really_read = Reader.really_read
module Writer = struct
  type out_channel = Lwt_unix.file_descr
  type 'a t = 'a Lwt.t
  let really_write fd = Lwt_cstruct.(complete (write fd))
end
let really_write = Writer.really_write

let copy_n ifd ofd n =
  let block_size = 32768 in
  let buffer = Cstruct.create block_size in
  let rec loop remaining =
    if remaining = 0L then Lwt.return_unit else begin
      let this = Int64.(to_int (min (of_int block_size) remaining)) in
      let block = Cstruct.sub buffer 0 this in
      really_read ifd block >>= fun () ->
      really_write ofd block >>= fun () ->
      loop (Int64.(sub remaining (of_int this)))
    end in
  loop n

module HR = Tar.HeaderReader(Lwt)(Reader)
module HW = Tar.HeaderWriter(Lwt)(Writer)

let get_next_header ?level ic =
  HR.read ?level ic
  >>= function
  | Error `Eof -> Lwt.return None
  | Ok hdr -> Lwt.return (Some hdr)

(** Return the header needed for a particular file on disk *)
let header_of_file ?level (file: string) : Tar.Header.t Lwt.t =
  let level = match level with None -> Tar.Header.V7 | Some level -> level in
  Lwt_unix.LargeFile.stat file >>= fun stat ->
  Lwt_unix.getpwuid stat.Lwt_unix.LargeFile.st_uid >>= fun pwent ->
  Lwt_unix.getgrgid stat.Lwt_unix.LargeFile.st_gid >>= fun grent ->
  let file_mode   = stat.Lwt_unix.LargeFile.st_perm in
  let user_id     = stat.Lwt_unix.LargeFile.st_uid in
  let group_id    = stat.Lwt_unix.LargeFile.st_gid in
  let file_size   = stat.Lwt_unix.LargeFile.st_size in
  let mod_time    = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name   = "" in
  let uname       = if level = V7 then "" else pwent.Lwt_unix.pw_name in
  let gname       = if level = V7 then "" else grent.Lwt_unix.gr_name in
  let devmajor    = if level = Ustar then stat.Lwt_unix.LargeFile.st_dev else 0 in
  let devminor    = if level = Ustar then stat.Lwt_unix.LargeFile.st_rdev else 0 in
  Lwt.return (Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
    ~uname ~gname ~devmajor ~devminor file file_size)

let write_block ?level (header: Tar.Header.t) (body: Lwt_unix.file_descr -> unit Lwt.t) (fd : Lwt_unix.file_descr) =
  HW.write ?level header fd
  >>= fun () ->
  body fd >>= fun () ->
  really_write fd (Tar.Header.zero_padding header)

let write_end (fd: Lwt_unix.file_descr) =
  really_write fd Tar.Header.zero_block >>= fun () ->
  really_write fd Tar.Header.zero_block

(** Utility functions for operating over whole tar archives *)
module Archive = struct

  let with_file name flags perms f =
    Lwt_unix.openfile name flags perms >>= fun fd ->
    Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  let with_next_file (fd: Lwt_unix.file_descr) (f: Lwt_unix.file_descr -> Tar.Header.t -> 'a Lwt.t) =
    get_next_header fd >>= function
    | Some hdr ->
      f fd hdr >>= fun result ->
      Reader.skip fd (Tar.Header.compute_zero_padding_length hdr) >>= fun () ->
      Lwt.return (Some result)
    | None ->
      Lwt.return None

  (** List the contents of a tar *)
  let list ?level fd =
    let rec loop acc = get_next_header ?level fd >>= function
      | None -> Lwt.return (List.rev acc)
      | Some hdr ->
        Reader.skip fd (Int64.to_int hdr.Tar.Header.file_size) >>= fun () ->
        Reader.skip fd (Tar.Header.compute_zero_padding_length hdr) >>= fun () ->
        loop (hdr :: acc) in
    loop []

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd =
    let rec loop () = get_next_header ifd >>= function
      | None -> Lwt.return_unit
      | Some hdr ->
        let filename = dest hdr.Tar.Header.file_name in
        with_file filename [Unix.O_WRONLY; O_CLOEXEC] 0 @@ fun ofd ->
        copy_n ifd ofd hdr.Tar.Header.file_size >>= fun () ->
        Reader.skip ifd (Tar.Header.compute_zero_padding_length hdr) >>= fun () ->
        loop () in
    loop ()

  let transform ?level f ifd ofd =
    let rec loop () = get_next_header ifd >>= function
      | None -> Lwt.return_unit
      | Some header' ->
        let header = f header' in
        let body = fun _ -> copy_n ifd ofd header.Tar.Header.file_size in
        write_block ?level header body ofd >>= fun () ->
        Reader.skip ifd (Tar.Header.compute_zero_padding_length header') >>= fun () ->
        loop () in
    loop () >>= fun () ->
    write_end ofd

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create files ofd =
    let file filename =
      Lwt_unix.stat filename >>= fun stat ->
      if stat.Unix.st_kind <> Unix.S_REG then
        (* Skipping, not a regular file. *)
        Lwt.return_unit
      else begin
        header_of_file filename >>= fun hdr ->

        write_block hdr (fun ofd ->
            with_file filename [O_RDONLY; O_CLOEXEC] 0 @@ fun ifd ->
            copy_n ifd ofd hdr.Tar.Header.file_size
          ) ofd
      end in
    Lwt_list.iter_s file files >>= fun () ->
    (* Add two empty blocks *)
    write_end ofd

end
