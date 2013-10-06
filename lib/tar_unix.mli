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
      possible start of a header block. Unix.End_of_file is thrown if the stream
      unexpectedly fails *)
  val get_next_header : Unix.file_descr -> t
    
  (** Return the header needed for a particular file on disk *)
  val of_file : string -> t
end

module Archive : sig
  (** Utility functions for operating over whole tar archives *)

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  val with_next_file : Unix.file_descr -> (Unix.file_descr -> Header.t -> 'a) -> 'a

  (** List the contents of a tar to stdout *)
  val list : Unix.file_descr -> Header.t list

  (** [extract dest] extract the contents of a tar.
      Apply 'dest' on each source filename to know the destination filename *)
  val extract : (string -> string) -> Unix.file_descr -> unit
    
  (** Create a tar on file descriptor fd from the filename list 'files' *)
  val create : string list -> Unix.file_descr -> unit
end
