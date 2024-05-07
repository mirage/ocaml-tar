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

type t

val value : ('a, 'err) result -> ('a, 'err, t) Tar.t

val run : ('a, [> `Unexpected_end_of_file] as 'b, t) Tar.t -> Eio.Flow.source -> ('a, 'b) result

val fold :
  (?global:Tar.Header.Extended.t ->
   Tar.Header.t ->
   'a ->
   ('a, [> `Fatal of Tar.error | `Unexpected_end_of_file ] as 'b, t) Tar.t) ->
  Eio.Fs.dir Eio.Path.t ->
  'a ->
  ('a, 'b) result

(** Return the header needed for a particular file on disk. [getpwuid] and [getgrgid] are optional
    functions that should take the uid and gid respectively and return the passwd and group entry
    names for each. These will be added to the header. *)
val header_of_file :
    ?level:Tar.Header.compatibility ->
    ?getpwuid:(int64 -> string) ->
    ?getgrgid:(int64 -> string) ->
    Eio.Fs.dir Eio.Path.t ->
    Tar.Header.t

val extract : ?filter:(Tar.Header.t -> bool) ->
  src:Eio.Fs.dir Eio.Path.t ->
  Eio.Fs.dir Eio.Path.t ->
  (unit, _) result

val create : ?level:Tar.Header.compatibility ->
  ?global:Tar.Header.Extended.t ->
  ?filter:(Tar.Header.t -> bool) ->
  src:Eio.Fs.dir Eio.Path.t ->
  Eio.Fs.dir Eio.Path.t ->
  (unit, _) result

val append_file : ?level:Tar.Header.compatibility ->
  ?header:Tar.Header.t ->
  Eio.Fs.dir Eio.Path.t ->
  Eio.Flow.sink ->
  (unit, _) result

val write_header : ?level:Tar.Header.compatibility ->
  Tar.Header.t -> Eio.Flow.sink ->
  (unit, _) result

val write_global_extended_header : ?level:Tar.Header.compatibility ->
  Tar.Header.Extended.t -> Eio.Flow.sink ->
  (unit, _) result

val write_end : Eio.Flow.sink -> (unit, _) result
