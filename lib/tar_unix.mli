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

(** Unix I/O for tar-formatted data *)

val really_read: Unix.file_descr -> Cstruct.t -> unit
(** [really_read fd buf] fills [buf] with data from [fd] or raises
    End_of_file *)

val really_write: Unix.file_descr -> Cstruct.t -> unit
(** [really_write fd buf] writes the full contents of [buf] to [fd]
    or raises End_of_file *)

module Header : sig
  include module type of Tar.Header

  (** Returns the next header block or throws End_of_stream if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block. End_of_file is thrown if the stream
      unexpectedly fails *)
  val get_next_header : Unix.file_descr -> t
    
  (** Return the header needed for a particular file on disk *)
  val of_file : string -> t
end

val write_block: Header.t -> (Unix.file_descr -> unit) -> Unix.file_descr -> unit
(** [write_block hdr write_body fd] is DEPRECATED.
    It writes [hdr], then calls [write_body fd] to write the body,
    then zero-pads so the stream is positioned for the next block. *)

val write_end: Unix.file_descr -> unit
(** [write_end fd] is DEPRECATED.
    It writes a stream terminator to [fd]  *)

module Archive : sig
  (** Utility functions for operating over whole tar archives *)

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  val with_next_file : Unix.file_descr -> (Unix.file_descr -> Header.t -> 'a) -> 'a

  (** List the contents of a tar *)
  val list : Unix.file_descr -> Header.t list

  (** [extract dest] extract the contents of a tar.
      Apply 'dest' on each source filename to know the destination filename *)
  val extract : (string -> string) -> Unix.file_descr -> unit
    
  (** Create a tar on file descriptor fd from the filename list 'files' *)
  val create : string list -> Unix.file_descr -> unit

  (** This function is DEPRECATED.
      [copy_n ifd odf n] copies exactly [n] bytes from [ifd] to [ofd] *)
  val copy_n : Unix.file_descr -> Unix.file_descr -> int64 -> unit

  (** This function is DEPRECATED.
      [multicast_n ?buffer_size ifd ofds n] copies exactly [n] bytes from [ifd] to all [ofds] *)
  val multicast_n : ?buffer_size:int -> Unix.file_descr -> Unix.file_descr list -> int64 -> unit

  (** This function is DEPRECATED.
      [multicast_n_string buffer ofds n] copies [n] bytes from [buffer] to all [ofds] *)
  val multicast_n_string : string -> Unix.file_descr list -> int -> unit

  (** This function is DEPRECATED.
      [skip fd n] reads and throws away [n] bytes from [fd] *)
  val skip : Unix.file_descr -> int -> unit
end
