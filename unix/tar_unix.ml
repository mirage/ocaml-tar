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

module Direct = struct
  type 'a t = 'a
  let return x = x
  let ( >>= ) m f = f m
end

module Driver = struct
  type 'a io = 'a Direct.t
  type in_channel = Unix.file_descr
  type out_channel = Unix.file_descr

  let rec with_restart op fd buf off len =
    try op fd buf off len with
      Unix.Unix_error (Unix.EINTR,_,_) ->
      with_restart op fd buf off len

  let really_read fd buf =
    (* Change code once this is merged:
       https://github.com/mirage/ocaml-cstruct/pull/302 *)
    let b = Bytes.create (min 4096 (Cstruct.length buf)) in
    let rec loop offset =
      let len = min (Bytes.length b) (Cstruct.length buf - offset) in
      if len > 0 then
        let n = with_restart Unix.read fd b 0 len in
        if n = 0 then raise End_of_file;
        Cstruct.blit_from_bytes b 0 buf offset n;
        loop (offset + n)
    in
    loop 0

  let skip fd n =
    (* Here it would make sense to use [Lwt_unix.lseek] if we can detect if
       [ifd] is seekable *)
    let b = Bytes.create (min 4096 n) in
    let rem = ref n in
    while !rem > 0 do
      let len = min (Bytes.length b) !rem in
      rem := !rem - with_restart Unix.read fd b 0 len
    done

  let really_write fd buf =
    (* FIXME: This is not very good :(
       also: https://github.com/mirage/ocaml-cstruct/pull/302 *)
    let b = Cstruct.to_bytes buf in
    let offset = ref 0 in
    while !offset < Bytes.length b do
      offset := !offset + with_restart Unix.write fd b 0 (Bytes.length b)
    done
end

module HeaderReader = Tar.HeaderReader(Direct)(Driver)
module HeaderWriter = Tar.HeaderWriter(Direct)(Driver)

include Driver

  (** Return the header needed for a particular file on disk *)
let header_of_file ?level (file: string) : Tar.Header.t =
  let level = match level with None -> !Tar.Header.compatibility_level | Some level -> level in
  let stat = Unix.LargeFile.lstat file in
  let file_mode = stat.Unix.LargeFile.st_perm in
  let user_id = stat.Unix.LargeFile.st_uid in
  let group_id = stat.Unix.LargeFile.st_gid in
  let mod_time = Int64.of_float stat.Unix.LargeFile.st_mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let uname = if level = V7 then "" else (Unix.getpwuid stat.Unix.LargeFile.st_uid).Unix.pw_name in
  let devmajor = if level = Ustar then stat.Unix.LargeFile.st_dev else 0 in
  let gname = if level = V7 then "" else (Unix.getgrgid stat.Unix.LargeFile.st_gid).Unix.gr_name in
  let devminor = if level = Ustar then stat.Unix.LargeFile.st_rdev else 0 in
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
    ~uname ~gname ~devmajor ~devminor file stat.Unix.LargeFile.st_size
