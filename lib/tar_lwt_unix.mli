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

(** Lwt_unix I/O for tar-formatted data *)

val really_read: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t
(** [really_read fd buf] fills [buf] with data from [fd] or fails
    with End_of_file *)

val really_write: Lwt_unix.file_descr -> Cstruct.t -> unit Lwt.t
(** [really_write fd buf] writes the full contents of [buf] to
    [fd] or fails with End_of_file *)

module Header : sig
  include module type of Tar.Header

  (** Returns the next header block or None if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block. Unix.End_of_file is thrown if the stream
      unexpectedly fails *)
  val get_next_header : Lwt_unix.file_descr -> t option Lwt.t
    
  (** Return the header needed for a particular file on disk *)
  val of_file : string -> t Lwt.t
end

module Archive : sig
  (** Utility functions for operating over whole tar archives *)

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  val with_next_file : Lwt_unix.file_descr -> (Lwt_unix.file_descr -> Header.t -> 'a Lwt.t) -> 'a option Lwt.t

  (** List the contents of a tar to stdout *)
  val list : Lwt_unix.file_descr -> Header.t list Lwt.t

  (** [extract dest] extract the contents of a tar.
      Apply 'dest' on each source filename to know the destination filename *)
  val extract : (string -> string) -> Lwt_unix.file_descr -> unit Lwt.t
    
  (** Create a tar on file descriptor fd from the filename list 'files' *)
  val create : string list -> Lwt_unix.file_descr -> unit Lwt.t
end
