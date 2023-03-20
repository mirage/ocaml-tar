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

[@@@warning "-3"] (* FIXME Tar.HeaderWriter needs to be used here *)

module Driver = struct
  type in_channel = Unix.file_descr
  type out_channel = Unix.file_descr


  let rec with_restart op fd buf off len =
    try op fd buf off len with
      Unix.Unix_error (Unix.EINTR,_,_) ->
      with_restart op fd buf off len

  let rec really_input fd buf off = function
    | 0 -> ()
    | len ->
      let m = Unix.read fd buf off len in
      if m = 0 then raise End_of_file;
      really_input fd buf (off+m) (len-m)


  let rec really_output fd buf off = function
    | 0 -> ()
    | len ->
      let m = Unix.write fd buf off len in
      really_output fd buf (off+m) (len-m)

  let output = with_restart really_output
  let input = with_restart Unix.read
  let really_input = with_restart really_input
  let close_out = Unix.close
end

module T = Tar.Make(Driver)

let really_write = T.really_write

let really_read = T.really_read

let get_next_header = T.get_next_header

  (** Return the header needed for a particular file on disk *)
let header_of_file ?level (file: string) : Tar.Header.t =
  let level = match level with None -> Tar.Header.V7 | Some level -> level in
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

let write_block = T.write_block
let write_end = T.write_end

(** Utility functions for operating over whole tar archives *)
module Archive = struct
  include T.Archive

  let with_file name flags perms f =
    let fd = Unix.openfile name flags perms in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd =
    let dest hdr =
      let filename = dest hdr.Tar.Header.file_name in
      Unix.openfile filename [O_WRONLY; O_CLOEXEC] 0
    in
    extract_gen dest ifd

  let transform ?level f ifd ofd =
    let rec loop global () =
      match get_next_header ?global ifd with
      | exception Tar.Header.End_of_stream -> ()
      | (header', global) ->
        let header = f header' in
        let body = fun _ -> copy_n ifd ofd header.Tar.Header.file_size in
        write_block ?level header body ofd;
        skip ifd (Tar.Header.compute_zero_padding_length header');
        loop global () in
    loop None ();
    write_end ofd

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create files ofd =
    let files =
      let f filename =
        let stat = Unix.stat filename in
        if stat.Unix.st_kind <> Unix.S_REG
        then
          (* Skipping, not a regular file. *)
          None
        else
          let hdr = header_of_file filename in
          Some (hdr, (fun ofd ->
              with_file filename [O_RDONLY; O_CLOEXEC] 0 @@ fun ifd ->
              copy_n ifd ofd hdr.Tar.Header.file_size))
      in
      List.filter_map f files
    in
    create_gen (Stream.of_list files) ofd

  (** Multicast 'n' bytes from input fd 'ifd' to output fds 'ofds'. NB if one deadlocks
      they all stop.*)
  let multicast_n ?(buffer_size=1024*1024) (ifd: Unix.file_descr) (ofds: Unix.file_descr list) (n: int64) =
    let buffer = Bytes.make buffer_size '\000' in
    let rec loop (n: int64) =
      if n <= 0L then ()
      else
        let amount = Int64.to_int (min n (Int64.of_int(Bytes.length buffer))) in
        let read = Unix.read ifd buffer 0 amount in
        if read = 0 then raise End_of_file;
        List.iter (fun ofd -> ignore(Unix.write ofd buffer 0 read)) ofds;
        loop (Int64.sub n (Int64.of_int read)) in
    loop n

end
