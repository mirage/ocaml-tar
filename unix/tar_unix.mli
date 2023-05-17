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

val skip : Unix.file_descr -> int -> unit

(** Return the header needed for a particular file on disk. *)
val header_of_file : ?level:Tar.Header.compatibility -> string -> Tar.Header.t

module HeaderReader : Tar.HEADERREADER with type in_channel = Unix.file_descr and type 'a io = 'a
module HeaderWriter : Tar.HEADERWRITER with type out_channel = Unix.file_descr and type 'a io = 'a
