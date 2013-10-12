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

(** Process and create tar file headers *)
module Header = struct
  (** Map of field name -> (start offset, length) taken from wikipedia:
      http://en.wikipedia.org/w/index.php?title=Tar_%28file_format%29&oldid=83554041 *)

  cstruct hdr {
    uint8_t file_name[100];
    uint8_t file_mode[8];
    uint8_t user_id[8];
    uint8_t group_id[8];
    uint8_t file_size[12];
    uint8_t mod_time[12];
    uint8_t chksum[8];
    uint8_t link_indicator;
    uint8_t link_name[100]
  } as little_endian (* doesn't matter, all are strings *)

  let sizeof_hdr_file_name = 100
  let sizeof_hdr_file_mode = 8
  let sizeof_hdr_user_id   = 8
  let sizeof_hdr_group_id  = 8
  let sizeof_hdr_file_size = 12
  let sizeof_hdr_mod_time  = 12
  let sizeof_hdr_chksum    = 8
  let sizeof_hdr_link_name = 100

  module Link = struct
    type t =
      | Normal
      | Hard
      | Symbolic

    let to_int = function
      | Normal   -> 0
      | Hard     -> 49 (* '1' *)
      | Symbolic -> 50 (* '2' *)

    let of_int = function
      | 49 (* '1' *) -> Hard
      | 50 (* '2' *) -> Symbolic
      | _ -> Normal (* if value is malformed, treat as a normal file *)

    let to_string = function
      | Normal -> "Normal"
      | Hard -> "Hard"
      | Symbolic -> "Symbolic"
  end 

  (** Represents a standard (non-USTAR) archive (note checksum not stored) *)
  type t = { file_name: string;
	     file_mode: int;
	     user_id: int;
	     group_id: int;
	     file_size: int64;
	     mod_time: int64;
	     link_indicator: Link.t;
	     link_name: string;
	   }

  (** Helper function to make a simple header *)
  let make ?(file_mode=0) ?(user_id=0) ?(group_id=0) ?(mod_time=0L) ?(link_indicator=Link.Normal) ?(link_name="") file_name file_size = 
    { file_name = file_name;
      file_mode = file_mode;
      user_id = user_id;
      group_id = group_id;
      file_size = file_size;
      mod_time = mod_time;
      link_indicator;
      link_name }

  (** Length of a header block *)
  let length = 512

  (** A blank header block (two of these in series mark the end of the tar) *)
  let zero_block =
    let buf = Cstruct.create length in
    for i = 0 to Cstruct.len buf - 1 do
      Cstruct.set_uint8 buf i 0
    done;
    buf

  (** [allzeroes buf] is true if [buf] contains only zero bytes *)
  let allzeroes buf =
    let rec loop i =
      (i >= Cstruct.len buf) || (Cstruct.get_uint8 buf i = 0 && (loop (i + 1))) in
    loop 0

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the LHS *)
  let pad_left (x: string) (n: int) (c: char) = 
    if String.length x >= n then x
    else let buffer = String.make n c in
         String.blit x 0 buffer (n - (String.length x)) (String.length x);
         buffer

  (** Return a string containing 'x' padded out to 'n' bytes by adding 'c' to the RHS *)
  let pad_right (x: string) (n: int) (c: char) = 
    if String.length x >= n then x
    else let buffer = String.make n c in
         String.blit x 0 buffer 0 (String.length x);
         buffer

  (** Pretty-print the header record *)
  let to_detailed_string (x: t) = 
    let table = [ "file_name",      x.file_name;
		  "file_mode",      string_of_int x.file_mode;
		  "user_id",        string_of_int x.user_id;
		  "group_id",       string_of_int x.group_id;
		  "file_size",      Int64.to_string x.file_size;
		  "mod_time",       Int64.to_string x.mod_time;
		  "link_indicator", Link.to_string x.link_indicator;
		  "link_name",      x.link_name ] in
    "{\n" ^ (String.concat "\n\t" (List.map (fun (k, v) -> k ^ ": " ^ v) table)) ^ "}"

  (** For debugging: pretty-print a string as hex *)
  let to_hex (x: string) : string =
    let result = String.make (String.length x * 3) ' ' in
    for i = 0 to String.length x - 1 do
      let byte = Printf.sprintf "%02x" (int_of_char x.[i]) in
      String.blit byte 0 result (i * 3) 2
    done;
    result

  (** Marshal an integer field of size 'n' *)
  let marshal_int (x: int) (n: int) = 
    let octal = Printf.sprintf "%o" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an int64 field of size 'n' *)
  let marshal_int64 (x: int64) (n: int) = 
    let octal = Printf.sprintf "%Lo" x in
    let result = pad_left octal (n-1) '0' in
    result ^ "\000" (* space or NULL allowed *)

  (** Marshal an string field of size 'n' *)
  let marshal_string (x: string) (n: int) = pad_right x n '\000'

  let trim regexp x = match Re_str.split regexp x with
    | [] -> ""
    | x :: _ -> x
  let trim_numerical = trim (Re_str.regexp "[\000 ]+")
  let trim_string = trim (Re_str.regexp "[\000]+")

  (** Unmarshal an integer field (stored as 0-padded octal) *)
  let unmarshal_int (x: string) : int = 
    let tmp = "0o0" ^ (trim_numerical x) in
    try
      int_of_string tmp
    with Failure "int_of_string" as e -> 
      Printf.eprintf "Failed to parse integer [%s] == %s\n" tmp (to_hex tmp);
      raise e

  (** Unmarshal an int64 field (stored as 0-padded octal) *)
  let unmarshal_int64 (x: string) : int64 = 
    let tmp = "0o0" ^ (trim_numerical x) in
    Int64.of_string tmp

  (** Unmarshal a string *)
  let unmarshal_string (x: string) : string = trim_string x

  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch

  (** From an already-marshalled block, compute what the checksum should be *)
  let checksum (x: Cstruct.t) : int64 = 
    (* Sum of all the byte values of the header with the checksum field taken
       as 8 ' ' (spaces) *)
    let result = ref 0 in
    for i = 0 to Cstruct.len x - 1 do
      result := !result + (Cstruct.get_uint8 x i)
    done;
    (* since we included the checksum, subtract it and add the spaces *)
    let chksum = get_hdr_chksum x in
    for i = 0 to Cstruct.len chksum - 1 do
      result := !result - (Cstruct.get_uint8 chksum i) + (int_of_char ' ')
    done;
    Int64.of_int !result

  (** Unmarshal a header block, returning None if it's all zeroes *)
  let unmarshal (c: Cstruct.t) : t option = 
    if allzeroes c then None
    else 
      let chksum = unmarshal_int64 (copy_hdr_chksum c) in
      if checksum c <> chksum then raise Checksum_mismatch
      else Some { file_name = unmarshal_string (copy_hdr_file_name c);
		  file_mode = unmarshal_int    (copy_hdr_file_mode c);
		  user_id   = unmarshal_int    (copy_hdr_user_id c);
		  group_id  = unmarshal_int    (copy_hdr_group_id c);
		  file_size = unmarshal_int64  (copy_hdr_file_size c);
		  mod_time  = unmarshal_int64  (copy_hdr_mod_time c);
		  link_indicator = Link.of_int (get_hdr_link_indicator c);
		  link_name = unmarshal_string (copy_hdr_link_name c);
		}

  (** Marshal a header block, computing and inserting the checksum *)
  let marshal c (x: t) = 
    set_hdr_file_name (marshal_string x.file_name sizeof_hdr_file_name) 0 c;
    set_hdr_file_mode (marshal_int x.file_mode sizeof_hdr_file_mode) 0 c;
    set_hdr_user_id   (marshal_int x.user_id sizeof_hdr_user_id) 0 c;
    set_hdr_group_id  (marshal_int x.group_id sizeof_hdr_group_id) 0 c;
    set_hdr_file_size (marshal_int64 x.file_size sizeof_hdr_file_size) 0 c;
    set_hdr_mod_time  (marshal_int64 x.mod_time sizeof_hdr_mod_time) 0 c;
    set_hdr_link_indicator c (Link.to_int x.link_indicator);
    set_hdr_link_name (marshal_string x.link_name sizeof_hdr_link_name) 0 c;
    (* Finally, compute the checksum *)
    let chksum = checksum c in
    set_hdr_chksum    (marshal_int64 chksum sizeof_hdr_chksum) 0 c

  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  let compute_zero_padding_length (x: t) : int = 
    (* round up to next whole number of block lengths *)
    let length = Int64.of_int length in
    let lenm1 = Int64.sub length Int64.one in
    let next_block_length = (Int64.mul length (Int64.div (Int64.add x.file_size lenm1) length)) in
    Int64.to_int (Int64.sub next_block_length x.file_size)

  (** Return the required zero-padding as a string *)
  let zero_padding (x: t) = 
    let zero_padding_len = compute_zero_padding_length x in
    Cstruct.sub zero_block 0 zero_padding_len

  let to_sectors (x: t) =
    let bytes = Int64.(add x.file_size (of_int (compute_zero_padding_length x))) in
    Int64.div bytes 512L
end

let is_zero x =
  try
    for i = 0 to Cstruct.len x - 1 do
      if Cstruct.get_uint8 x i <> 0 then raise Not_found
    done;
    true
  with Not_found -> false

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module Archive = functor(ASYNC: ASYNC) -> struct
  open ASYNC

  let fold f initial read =
    let rec aux zeroes_so_far from acc =
      if zeroes_so_far = 2
      then return acc
      else
        read from >>= fun hdr ->
        if is_zero hdr
        then aux (zeroes_so_far + 1) (Int64.succ from) acc
        else
          match Header.unmarshal hdr with
          | None -> return acc
          | Some tar ->
            f acc tar (Int64.succ from) >>= fun acc ->
            aux 0 (Int64.(add (add from (Header.to_sectors tar)) 1L)) acc in
    aux 0 0L initial

end
