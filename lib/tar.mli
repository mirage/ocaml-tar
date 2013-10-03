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

  (** Make a single line summary which looks like the output of tar -tv *)
  val to_summary_string : t -> string
    
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
end
