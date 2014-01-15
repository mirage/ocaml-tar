(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then raise End_of_file;
    really_read fd string (off+m) (n-m)

(* XXX: there's no function to read directly into a bigarray *)
let really_read ifd buffer =
  let s = String.create (Cstruct.len buffer) in
  really_read ifd s 0 (Cstruct.len buffer);
  Cstruct.blit_from_string s 0 buffer 0 (Cstruct.len buffer)

let really_write fd str off n =
  let written = Unix.write fd str off n in
  if str <> "" && String.length str > written then failwith "Truncated write"

(* XXX: there's no function to write directly from a bigarray *)
let really_write fd buffer =
  let s = Cstruct.to_string buffer in
  really_write fd s 0 (String.length s)

let copy_n ifd ofd n =
  let buffer = String.create 16384 in
  let rec loop remaining =
    if remaining = 0L then () else begin
      let this = Int64.(to_int (min (of_int (String.length buffer)) remaining)) in
      let n = Unix.read ifd buffer 0 this in
      if n = 0 then raise End_of_file;
      let m = Unix.write ofd buffer 0 n in
      if m < n then raise End_of_file;
      loop (Int64.(sub remaining (of_int n)))
    end in
  loop n

let finally fct clean_f =
  let result =
    try fct ();
    with exn ->
      clean_f ();
      raise exn in
  clean_f ();
  result

module Header = struct
  include Tar.Header

  (** Returns the next header block or throws End_of_stream if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block. Unix.End_of_file is thrown if the stream
      unexpectedly fails *)
  let get_next_header (ifd: Unix.file_descr) : t = 
    let next () =
      let buffer = Cstruct.create length in
      really_read ifd buffer;
      unmarshal buffer 
    in
    match next () with
    | Some x -> x
    | None -> 
	begin match next () with
	| Some x -> x
	| None -> raise End_of_stream
	end
	  
  (** Return the header needed for a particular file on disk *)
  let of_file (file: string) : t =
    let stat = Unix.LargeFile.lstat file in
    { file_name   = file;
      file_mode   = stat.Unix.LargeFile.st_perm;
      user_id     = stat.Unix.LargeFile.st_uid;
      group_id    = stat.Unix.LargeFile.st_gid;
      file_size   = stat.Unix.LargeFile.st_size;
      mod_time    = Int64.of_float stat.Unix.LargeFile.st_mtime;
      link_indicator = Link.Normal;
      link_name   = "" }
end

let write_block (header: Tar.Header.t) (body: Unix.file_descr -> unit) (fd : Unix.file_descr) = 
  let buffer = Cstruct.create Tar.Header.length in
  for i = 0 to Tar.Header.length - 1 do
      Cstruct.set_uint8 buffer i 0
  done;
  Tar.Header.marshal buffer header;
  really_write fd buffer;
  body fd;
  really_write fd (Tar.Header.zero_padding header)

let write_end (fd: Unix.file_descr) =
  really_write fd Tar.Header.zero_block;
  really_write fd Tar.Header.zero_block

(** Utility functions for operating over whole tar archives *)
module Archive = struct

  (** Skip 'n' bytes from input channel 'ifd' *)
  let skip (ifd: Unix.file_descr) (n: int) = 
    let buffer = String.make 4096 '\000' in
    let rec loop (n: int) = 
      if n <= 0 then ()
      else 
	let amount = min n (String.length buffer) in
	let m = Unix.read ifd buffer 0 amount in
	if m = 0 then raise End_of_file;
	loop (n - m) in
    loop n

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  let with_next_file (fd: Unix.file_descr) (f: Unix.file_descr -> Tar.Header.t -> 'a) = 
    let hdr = Header.get_next_header fd in
    (* NB if the function 'f' fails we're boned *)
    finally (fun () -> f fd hdr) 
      (fun () -> skip fd (Tar.Header.compute_zero_padding_length hdr))

  (** List the contents of a tar *)
  let list fd = 
    let list = ref [] in
    try
      while true do
	let hdr = Header.get_next_header fd in
	list := hdr :: !list;
	skip fd (Int64.to_int hdr.Tar.Header.file_size);
	skip fd (Tar.Header.compute_zero_padding_length hdr)
      done;
      List.rev !list;
    with 
    | End_of_file -> failwith "Unexpected end of file while reading stream"
    | Tar.Header.End_of_stream -> List.rev !list

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd = 
    try
      while true do
	let hdr = Header.get_next_header ifd in
	let filename = dest hdr.Tar.Header.file_name in
	print_endline filename;
	let ofd = Unix.openfile filename [Unix.O_WRONLY] 0644 in
	copy_n ifd ofd hdr.Tar.Header.file_size;
	skip ifd (Tar.Header.compute_zero_padding_length hdr)
      done
    with 
    | End_of_file -> failwith "Unexpected end of file while reading stream"
    | Tar.Header.End_of_stream -> ()

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create files ofd = 
    let file filename = 
      let stat = Unix.stat filename in
      if stat.Unix.st_kind <> Unix.S_REG 
      then Printf.eprintf "Skipping %s: not a regular file\n" filename
      else 
	let hdr = Header.of_file filename in
	write_block hdr (fun ofd ->
	  let ifd = Unix.openfile filename [Unix.O_RDONLY] 0644 in
	  copy_n ifd ofd hdr.Tar.Header.file_size) ofd;
    in
    List.iter file files;
    (* Add two empty blocks *)
    write_end ofd

  let copy_n = copy_n

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

  let skip (ifd: Unix.file_descr) (n: int) = 
    let buffer = String.make 4096 '\000' in
    let rec loop (n: int) = 
      if n <= 0 then ()
      else 
       let amount = min n (String.length buffer) in
       let m = Unix.read ifd buffer 0 amount in
       if m = 0 then raise End_of_file;
       loop (n - m) in
    loop n

end
