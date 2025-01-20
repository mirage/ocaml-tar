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

(** {1 Eio Tar} 

    This library provides low-level and high-level abstractions for reading
    and writing Tar files using Eio.
*)

type t

type src =
  | Flow : _ Eio.Flow.source -> src
  | File : _ Eio.File.ro -> src  (** Sources for tar files *)

type decode_error =
  [ `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ]
(** Possible decoding errors *)

(** {2 High-level Interface} *)

val run :
  ('a, ([> `Unexpected_end_of_file ] as 'b), t) Tar.t -> src -> ('a, 'b) result
(** [run tar src] will run the given [tar] using {! Eio} on [src]. *)

val extract :
  ?filter:(Tar.Header.t -> bool) ->
  src ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (unit, [> decode_error ]) result
(** [extract src dst] extracts the tar file from [src] into [dst]. For example:

    {[
      Eio.Path.with_open_in src @@ fun src ->
      Tar_eio.extract src dst
    ]}

    will extract the file at [src] into the directory at [dst]. Note that this function
    only creates {b files}, {b directories} and {b symlinks} with the correct mode (it does not, for
    example, set the ownership of the files according to the tar file).

    @param filter Can be used to exclude certain entries based on their header. *)

val create :
  ?level:Tar.Header.compatibility ->
  ?global:Tar.Header.Extended.t ->
  ?filter:(Tar.Header.t -> bool) ->
  src:Eio.Fs.dir_ty Eio.Path.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result
(** [create ~src dst] is the opposite of {! extract}. The path [src] is tarred
    into [dst].

    @param filter Can be used to exclude certain entries based on their header.
*)

val fold :
  (?global:Tar.Header.Extended.t ->
  Tar.Header.t ->
  'acc ->
  ('acc, ([> `Fatal of Tar.error | `Unexpected_end_of_file ] as 'b), t) Tar.t) ->
  src ->
  'acc ->
  ('acc, 'b) result
(** [fold f src init] folds over the tarred [src]. *)

(** {2 Low-level Interface} *)

val value : ('a, 'err) result -> ('a, 'err, t) Tar.t
(** Converts a normal result into {! Tar}s IO type *)

val append_file :
  ?level:Tar.Header.compatibility ->
  ?header:Tar.Header.t ->
  Eio.Fs.dir_ty Eio.Path.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result
(** [append_file dst sink] *)

val header_of_file :
  ?level:Tar.Header.compatibility ->
  ?getpwuid:(int64 -> string) ->
  ?getgrgid:(int64 -> string) ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Tar.Header.t
(** Return the header needed for a particular file on disk. [getpwuid] and [getgrgid] are optional
    functions that should take the uid and gid respectively and return the passwd and group entry
    names for each. These will be added to the header. *)

val write_header :
  ?level:Tar.Header.compatibility ->
  Tar.Header.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result

val write_global_extended_header :
  ?level:Tar.Header.compatibility ->
  Tar.Header.Extended.t ->
  _ Eio.Flow.sink ->
  (unit, [> decode_error ]) result

val write_end : _ Eio.Flow.sink -> unit
