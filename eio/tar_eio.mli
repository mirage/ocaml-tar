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

(** I/O for tar-formatted data *)

val really_read: Eio.Flow.source -> Cstruct.t -> unit
(** [really_read fd buf] fills [buf] with data from [fd] or fails
    with {!Stdlib.End_of_file}. *)

val really_write: Eio.Flow.sink -> Cstruct.t -> unit
(** [really_write fd buf] writes the full contents of [buf] to
    [fd] or fails with {!Stdlib.End_of_file}. *)

val skip : Eio.Flow.source -> int -> unit
(** [skip fd n] reads [n] bytes from [fd] and discards them. If possible, you
    should use [Lwt_unix.lseek fd n Lwt_unix.SEEK_CUR] instead. *)

(** Return the header needed for a particular file on disk. [getpwuid] and [getgrgid] are optional
    functions that should take the uid and gid respectively and return the passwd and group entry
    names for each. These will be added to the header. *)
val header_of_file :
    ?level:Tar.Header.compatibility ->
    ?getpwuid:(int64 -> string) ->
    ?getgrgid:(int64 -> string) ->
    Eio.Fs.dir Eio.Path.t ->
    Tar.Header.t

module HeaderReader : Tar.HEADERREADER with type in_channel = Eio.Flow.source and type 'a io = 'a
module HeaderWriter : Tar.HEADERWRITER with type our_channel = Eio.Flow.sink and type 'a io = 'a
