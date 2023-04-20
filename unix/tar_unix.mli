(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Unix I/O for tar-formatted data. *)

val really_read: Unix.file_descr -> Cstruct.t -> unit
(** [really_read fd buf] fills [buf] with data from [fd] or raises
   {!Stdlib.End_of_file}. *)

val really_write: Unix.file_descr -> Cstruct.t -> unit
(** [really_write fd buf] writes the full contents of [buf] to [fd]
    or {!Stdlib.End_of_file}. *)

(** Returns the next header block or throws End_of_stream if two consecutive
    zero-filled blocks are discovered. Assumes stream is positioned at the
    possible start of a header block.
    @raise Stdlib.End_of_file if the stream unexpectedly fails. *)
val get_next_header : ?level:Tar.Header.compatibility -> global:Tar.Header.Extended.t option ->
  Unix.file_descr -> Tar.Header.t * Tar.Header.Extended.t option

(** Return the header needed for a particular file on disk. *)
val header_of_file : ?level:Tar.Header.compatibility -> string -> Tar.Header.t

val write_block: ?level:Tar.Header.compatibility -> ?global:Tar.Header.Extended.t ->
  Tar.Header.t -> (Unix.file_descr -> unit) -> Unix.file_descr -> unit
  [@@ocaml.deprecated "Deprecated in favor of Tar.HeaderWriter"]
  (** Write [hdr], then call [write_body fd] to write the body,
      then zero-pads so the stream is positioned for the next block. *)

val write_end: Unix.file_descr -> unit
  [@@ocaml.deprecated "Deprecated in favor of Tar.HeaderWriter"]
  (** Write a stream terminator to [fd]. *)

module Archive : sig
  (** Utility functions for operating over whole tar archives. *)

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header. *)
  val with_next_file : Unix.file_descr -> global:Tar.Header.Extended.t option -> (Unix.file_descr -> Tar.Header.Extended.t option -> Tar.Header.t -> 'a) -> 'a

  (** List the contents of a tar. *)
  val list : ?level:Tar.Header.compatibility -> Unix.file_descr -> Tar.Header.t list

  (** [extract dest] extract the contents of a tar.
     Apply [dest] on each source filename to change the destination
     filename. It only supports extracting regular files from the
     top-level of the archive. *)
  val extract : (string -> string) -> Unix.file_descr -> unit

  (** [transform f in_fd out_fd] applies [f] to the header of each
      file in the tar inputted in [in_fd], and writes the resulting
      headers to [out_fd] preserving the content and structure of the
      archive. *)
  val transform : ?level:Tar.Header.compatibility -> (Tar.Header.t -> Tar.Header.t) -> Unix.file_descr -> Unix.file_descr -> unit

  (** Create a tar on file descriptor fd from the filename list
     'files'. It only supports regular files. *)
  val create : string list -> Unix.file_descr -> unit

  (** [copy_n ifd odf n] copies exactly [n] bytes from [ifd] to [ofd]. *)
  val copy_n : Unix.file_descr -> Unix.file_descr -> int64 -> unit
  [@@ocaml.deprecated "Deprecated: use your own helper function"]

  (** [multicast_n ?buffer_size ifd ofds n] copies exactly [n] bytes from [ifd] to all [ofds]. *)
  val multicast_n : ?buffer_size:int -> Unix.file_descr -> Unix.file_descr list -> int64 -> unit
    [@@ocaml.deprecated "Deprecated: use your own helper function"]

  (** [skip fd n] reads and throws away [n] bytes from [fd]. *)
  val skip : Unix.file_descr -> int -> unit
    [@@ocaml.deprecated "Deprecated: use your own helper function"]

end
