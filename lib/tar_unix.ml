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

module Driver = struct
  type in_channel = Unix.file_descr
  type out_channel = Unix.file_descr

  let rec really_input fd string off n =
    if n=0 then () else
      let m = Unix.read fd string off n in
      if m = 0 then raise End_of_file;
      really_input fd string (off+m) (n-m)

  let output fd str off n =
    let written = Unix.write fd str off n in
    if str <> "" && String.length str > written then failwith "Truncated write"

  let input = Unix.read
  let close_out = Unix.close
end

module List = struct
  include List

  let filter_map f l =
    let rec g a = function
      [] -> List.rev a
    | x::l ->
      match f x with
        None -> g a l
      | Some x -> g (x::a) l
    in
      g [] l
end

module T = Tar.Make(Driver)

let really_write = T.really_write

let really_read = T.really_read

module Header = struct
  include T.Header

  (** Return the header needed for a particular file on disk *)
  let of_file ?(level = V7) (file: string) : t =
    let stat = Unix.LargeFile.lstat file in
    { file_name   = file;
      file_mode   = stat.Unix.LargeFile.st_perm;
      user_id     = stat.Unix.LargeFile.st_uid;
      group_id    = stat.Unix.LargeFile.st_gid;
      file_size   = stat.Unix.LargeFile.st_size;
      mod_time    = Int64.of_float stat.Unix.LargeFile.st_mtime;
      link_indicator = Link.Normal;
      link_name   = "";
      uname       = if level = V7 then "" else (Unix.getpwuid stat.Unix.LargeFile.st_uid).Unix.pw_name;
      gname       = if level = V7 then "" else (Unix.getgrgid stat.Unix.LargeFile.st_gid).Unix.gr_name;
      devmajor    = if level = Ustar then stat.Unix.LargeFile.st_dev else 0;
      devminor    = if level = Ustar then stat.Unix.LargeFile.st_rdev else 0; }
end

let write_block = T.write_block
let write_end = T.write_end

(** Utility functions for operating over whole tar archives *)
module Archive = struct
  include T.Archive

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd =
    let dest hdr =
      let filename = dest hdr.Header.file_name in
      print_endline filename;
      Unix.openfile filename [Unix.O_WRONLY] 0644
    in
    extract_gen dest ifd

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create files ofd =
    let files =
      let f filename =
        let stat = Unix.stat filename in
        if stat.Unix.st_kind <> Unix.S_REG
        then begin
               Printf.eprintf "Skipping %s: not a regular file\n" filename;
               None
             end
        else
          let hdr = Header.of_file filename in
          Some (hdr, (fun ofd ->
            let ifd = Unix.openfile filename [Unix.O_RDONLY] 0644 in
            copy_n ifd ofd hdr.Header.file_size))
      in
        List.filter_map f files
    in
    create_gen (Stream.of_list files) ofd

  (** Multicast 'n' bytes from input fd 'ifd' to output fds 'ofds'. NB if one deadlocks
      they all stop.*)
  let multicast_n ?(buffer_size=1024*1024) (ifd: Unix.file_descr) (ofds: Unix.file_descr list) (n: int64) = 
    let buffer = String.make buffer_size '\000' in
    let rec loop (n: int64) = 
      if n <= 0L then ()
      else 
       let amount = Int64.to_int (min n (Int64.of_int(String.length buffer))) in
       let read = Unix.read ifd buffer 0 amount in
       if read = 0 then raise End_of_file;
       List.iter (fun ofd -> ignore(Unix.write ofd buffer 0 read)) ofds;
       loop (Int64.sub n (Int64.of_int read)) in
    loop n

  let multicast_n_string buffer ofds n =
    List.iter (fun ofd -> ignore(Unix.write ofd buffer 0 n)) ofds

end
