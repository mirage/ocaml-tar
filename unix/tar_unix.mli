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

(** [fold f filename acc] folds over the tar archive. The function [f] is called
    for each [hdr : Tar.Header.t]. It should forward the position in the file
    descriptor by [hdr.Tar.Header.file_size]. *)
val fold :
  ((Unix.file_descr * Tar.Header.t * Tar.Header.Extended.t option,
    [
      | `Fatal of [ `Checksum_mismatch | `Corrupt_pax_header | `Unmarshal of string ]
      | `Unix of Unix.error * string * string
      | `End_of_file
    ]) result -> 'a -> 'a) ->
  string -> 'a -> 'a

(** [extract ~src ~dst] extracts the tar archive [src] into the directory [dst].
    If [dst] does not exist, it is created. *)
val extract : src:string -> dst:string ->
  (unit, [ `Fatal of [ `Checksum_mismatch | `Corrupt_pax_header | `Unmarshal of string ]
         | `Unix of Unix.error ]) result

(** [create ~level ~src ~dst] creates a tar archive at [dst]. It uses [src], a
    filename or directory name, as input. *)
val create : ?level:Tar.Header.compatibility -> src:string -> dst:string ->
  (unit, [ `Msg of string | `Unix of Unix.error ]) result

(** [header_of_file ~level filename] returns the tar header of [filename]. *)
val header_of_file : ?level:Tar.Header.compatibility -> string -> Tar.Header.t

(** [append_file ~level ~header filename fd] appends the contents of [filename]
    to the tar archive [fd]. If [header] is not provided, {header_of_file} is
    used for constructing a header. *)
val append_file : ?level:Tar.Header.compatibility -> ?header:Tar.Header.t ->
  string -> Unix.file_descr ->
  (unit, [ `Msg of string | `Unix of Unix.error ]) result

(** [write_extended_header ~level hdr fd] writes the extended header [hdr] to
    [fd]. *)
val write_extended_header : ?level:Tar.Header.compatibility ->
  Tar.Header.Extended.t -> Unix.file_descr ->
  (unit, [ `Msg of string | `Unix of Unix.error ]) result

(** [write_end fd] writes the tar end marker to [fd]. *)
val write_end : Unix.file_descr -> (unit, Unix.error) result
