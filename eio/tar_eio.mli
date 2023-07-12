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

(** Returns the next header block or None if two consecutive
    zero-filled blocks are discovered. Assumes stream is positioned at the
    possible start of a header block.
    @raise End_of_file if the stream unexpectedly fails. *)
val get_next_header : ?level:Tar.Header.compatibility -> Eio.Flow.source ->
                      Tar.Header.t option

(** Return the header needed for a particular file on disk. [getpwuid] and [getgrgid] are optional
    functions that should take the uid and gid respectively and return the passwd and group entry
    names for each. These will be added to the header. *)
val header_of_file :
    ?level:Tar.Header.compatibility ->
    ?getpwuid:(int64 -> string) ->
    ?getgrgid:(int64 -> string) ->
    Eio.Fs.dir Eio.Path.t ->
    Tar.Header.t

module Archive : sig
  (** Utility functions for operating over whole tar archives *)

  (** Read the next header, apply the function 'f' to the source and the header. The function
      should leave the source positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header. *)
  val with_next_file : Eio.Flow.source ->
                       (Eio.Flow.source -> Tar.Header.t -> 'a) -> 'a option

  (** List the contents of a tar to stdout. *)
  val list : ?level:Tar.Header.compatibility -> #Eio.Flow.source -> Tar.Header.t list

  (** [extract dest] extract the contents of a tar.
     Apply [dest] on each source filename to change the destination
     filename. It only supports extracting regular files from the
     top-level of the archive. *)
  val extract : (string -> Eio.Fs.dir Eio.Path.t) -> Eio.Flow.source -> unit

  (** [transform f src sink] applies [f] to the header of each
     file in the tar inputted in [src], and writes the resulting
     headers to [sink] preserving the content and structure of the
     archive. *)
  val transform : ?level:Tar.Header.compatibility -> (Tar.Header.t -> Tar.Header.t) -> #Eio.Flow.source -> #Eio.Flow.sink -> unit

  (** Create a tar in the sink from a list of file paths. It only supports regular files.

      See {! header_of_file} for the meaning of [getpwuid] and [getgrgid]. *)
  val create :
    ?getpwuid:(int64 -> string) ->
    ?getgrgid:(int64 -> string) ->
    Eio.Fs.dir Eio.Path.t list ->
    #Eio.Flow.sink ->
    unit
end
