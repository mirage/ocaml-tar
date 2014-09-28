(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Tar utilities *)

module Header : sig
  (** Process and create tar file headers *)

  module Link : sig
    (** Determines the type of the file *)
    type t =
      | Normal
      | Hard (** a hard link *)
      | Symbolic (** a symbolic link *)
    val to_string: t -> string
  end

  (** Represents a standard (non-USTAR) archive (note checksum not stored) *)
  type t = {
    file_name : string;
    file_mode: int;
    user_id: int;
    group_id: int;
    file_size: int64;
    mod_time: int64;
    link_indicator: Link.t;
    link_name: string;
  }

  (** Helper function to make a simple header *)
  val make : ?file_mode:int -> ?user_id:int -> ?group_id:int -> ?mod_time:int64 -> ?link_indicator:Link.t -> ?link_name:string -> string -> int64 -> t
    
  (** Length of a header block *)
  val length : int  
    
  (** A blank header block (two of these in series mark the end of the tar) *)
  val zero_block : Cstruct.t
    
  (** Pretty-print the header record *)
  val to_detailed_string : t -> string

  (** For debugging: pretty-print a string as hex *)
  val to_hex : string -> string
    
  (** Thrown when unmarshalling a header if the checksums don't match *)
  exception Checksum_mismatch
    
  (** Thrown if we detect the end of the tar (at least two zero blocks in sequence) *)
  exception End_of_stream
    
  (** Unmarshal a header block, returning None if it's all zeroes *)
  val unmarshal : Cstruct.t -> t option
    
  (** Marshal a header block, computing and inserting the checksum *)
  val marshal : Cstruct.t -> t -> unit
    
  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks *)
  val compute_zero_padding_length : t -> int

  (** Return the required zero-padding as a string *)
  val zero_padding : t -> Cstruct.t

  (** [to_sectors t] is the number of sectors occupied by the data *)
  val to_sectors: t -> int64
end

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module Archive : functor(ASYNC: ASYNC) -> sig
  val fold: ('a -> Header.t -> int64 -> 'a ASYNC.t) -> 'a -> (int64 -> Cstruct.t ASYNC.t) -> 'a ASYNC.t
  (** [fold f initial read] folds [f acc hdr data_offset] over all the
      files within the archive *)
end

module type IO = sig
  type in_channel
  type out_channel

  val really_input : in_channel -> string -> int -> int -> unit
  val input : in_channel -> string -> int -> int -> int
  val output : out_channel -> string -> int -> int -> unit
  val close_out : out_channel -> unit
end

module Make (IO : IO) : sig
  val really_read: IO.in_channel -> Cstruct.t -> unit
  (** [really_read fd buf] fills [buf] with data from [fd] or raises
      End_of_file *)

  val really_write: IO.out_channel -> Cstruct.t -> unit
  (** [really_write fd buf] writes the full contents of [buf] to [fd]
      or raises End_of_file *)

  module Header : sig
    include module type of Header

    (** Returns the next header block or throws End_of_stream if two consecutive
        zero-filled blocks are discovered. Assumes stream is positioned at the
        possible start of a header block. End_of_file is thrown if the stream
        unexpectedly fails *)
    val get_next_header : IO.in_channel -> t
  end

  val write_block: Header.t -> (IO.out_channel -> unit) -> IO.out_channel -> unit
  (** [write_block hdr write_body fd] is DEPRECATED.
      It writes [hdr], then calls [write_body fd] to write the body,
      then zero-pads so the stream is positioned for the next block. *)

  val write_end: IO.out_channel -> unit
  (** [write_end fd] is DEPRECATED.
      It writes a stream terminator to [fd]  *)

  module Archive : sig
    (** Utility functions for operating over whole tar archives *)

    (** Read the next header, apply the function 'f' to the fd and the header. The function
        should leave the fd positioned immediately after the datablock. Finally the function
        skips past the zero padding to the next header *)
    val with_next_file : IO.in_channel -> (IO.in_channel -> Header.t -> 'a) -> 'a

    (** List the contents of a tar *)
    val list : IO.in_channel -> Header.t list

    (** [extract_gen dest] extract the contents of a tar.
        Apply 'dest' on each header to get a handle to the file to write to *)
    val extract_gen : (Header.t -> IO.out_channel) -> IO.in_channel -> unit

    (** Create a tar on file descriptor fd from the stream of headers.  *)
    val create_gen : (Header.t * (IO.out_channel -> unit)) Stream.t -> IO.out_channel -> unit

    (** This function is DEPRECATED.
        [copy_n ifd odf n] copies exactly [n] bytes from [ifd] to [ofd] *)
    val copy_n : IO.in_channel -> IO.out_channel -> int64 -> unit

    (** This function is DEPRECATED.
        [skip fd n] reads and throws away [n] bytes from [fd] *)
    val skip : IO.in_channel -> int -> unit
  end
end
