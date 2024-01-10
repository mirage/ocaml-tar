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

module Io = struct
  type in_channel = Lwt_unix.file_descr
  type 'a io = 'a Lwt.t
  let really_read fd = Lwt_cstruct.(complete (read fd))
  let skip (ifd: Lwt_unix.file_descr) (n: int) =
    (* Here it would make sense to use [Lwt_unix.lseek] if we can detect if
       [ifd] is seekable *)
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

  type out_channel = Lwt_unix.file_descr
  let really_write fd = Lwt_cstruct.(complete (write fd))
end

include Io
module HeaderReader = Tar.HeaderReader(Lwt)(Io)
module HeaderWriter = Tar.HeaderWriter(Lwt)(Io)

(** Return the header needed for a particular file on disk *)
let header_of_file ?level (file: string) : Tar.Header.t Lwt.t =
  let level = match level with None -> !Tar.Header.compatibility_level | Some level -> level in
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
