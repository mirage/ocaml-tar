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

(** Tar utilities

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** The type of errors that may occur. *)
type error = [`Checksum_mismatch | `Corrupt_pax_header | `Zero_block | `Unmarshal of string]

(** [pp_error ppf e] pretty prints the error [e] on the formatter [ppf]. *)
val pp_error : Format.formatter -> [< error] -> unit

module Header : sig
  (** Process and create tar file headers. *)

  (** tar format assumptions. Default is {!V7} (for compatibility with versions of ocaml-tar before this type was introduced).
      @see <https://www.gnu.org/software/tar/manual/html_section/Formats.html> *)
  type compatibility =
    | OldGNU (** GNU tar < 1.12 *)
    | GNU (** GNU tar 1.12 - 1.13.25 *)
    | V7 (** Origin 7th Release format *)
    | Ustar (** POSIX.1-1988 *)
    | Posix (** POSIX.1-2001 *)

  (** Default compatibility if [?level] is omitted. Defaults to {!V7}. *)
  val compatibility_level : compatibility ref

  module Link : sig
    (** Determines the type of the file. *)
    type t =
      | Normal
      | Hard (** a hard link *)
      | Symbolic (** a symbolic link *)
      | Character (** a character device node *)
      | Block (** a block device node *)
      | Directory (** a directory (also indicated by trailing [/] in [file_name]) *)
      | FIFO (** a FIFO node *)
      | GlobalExtendedHeader (** a PaxExtension global header *)
      | PerFileExtendedHeader (** a PaxExtension per-file header *)
      | LongLink (** a GNU LongLink i.e. a very long link name *)
      | LongName (** a GNU LongName i.e. a very long filename *)
    val to_string: t -> string
  end

  module Extended: sig
    type t = {
      access_time: int64 option; (** second granularity since the Epoch *)
      charset: string option;
      comment: string option;
      group_id: int option;
      gname: string option;
      header_charset: string option;
      link_path: string option;
      mod_time: int64 option; (** second granularity since the Epoch *)
      path: string option;
      file_size: int64 option;
      user_id: int option;
      uname: string option;
    }
    (** Represents a "Pax" extended header. *)

    (** [make ()] creates an extended header. *)
    val make : ?access_time:int64 -> ?charset:string -> ?comment:string -> ?group_id:int -> ?gname:string -> ?header_charset:string -> ?link_path:string -> ?mod_time:int64 -> ?path:string -> ?file_size:int64 -> ?user_id:int -> ?uname:string -> unit -> t

    (** Pretty-print the extended header record. *)
    val to_detailed_string : t -> string

    (** Unmarshal a pax Extended Header block. This header block may
        be preceded by [global] blocks which will override some
        fields. *)
    val unmarshal : global:t option -> Cstruct.t -> (t, [> error ]) result
  end

  (** Represents a standard archive (note checksum not stored). *)
  type t = {
    file_name : string;
    file_mode: int;
    user_id: int;
    group_id: int;
    file_size: int64;
    mod_time: int64;
    link_indicator: Link.t;
    link_name: string;
    uname: string;
    gname: string;
    devmajor: int;
    devminor: int;
    extended: Extended.t option;
  }

  (** Helper function to make a simple header. *)

  (** [make file_name file_size] creates a simple header.
      [file_mode] defaults to [0o400], [user_id], [group_id] default to [0],
      [mod_time] defaults to [0L] (epoch), [link_indicator] defaults to [Normal],
      [link_name], [uname] and [gname] default to [""], and [devmajor] and
      [devminor] default to [0]. *)
  val make : ?file_mode:int -> ?user_id:int -> ?group_id:int -> ?mod_time:int64 -> ?link_indicator:Link.t -> ?link_name:string -> ?uname:string -> ?gname:string -> ?devmajor:int -> ?devminor:int -> string -> int64 -> t

  (** Length of a header block. *)
  val length : int

  (** A blank header block (two of these in series mark the end of the tar). *)
  val zero_block : Cstruct.t

  (** Pretty-print the header record. *)
  val to_detailed_string : t -> string

  (** Unmarshal a header block, returning [None] if it's all zeroes.
      This header block may be preceded by an [?extended] block which
      will override some fields. *)
  val unmarshal : ?extended:Extended.t -> Cstruct.t -> (t, [`Zero_block | `Checksum_mismatch | `Unmarshal of string]) result

  (** Marshal a header block, computing and inserting the checksum. *)
  val marshal : ?level:compatibility -> Cstruct.t -> t -> (unit, [> `Msg of string ]) result

  (** Compute the amount of zero-padding required to round up the file size
      to a whole number of blocks. *)
  val compute_zero_padding_length : t -> int

  (** Return the required zero-padding as a string. *)
  val zero_padding : t -> Cstruct.t

  (** [to_sectors t] is the number of sectors occupied by the data. *)
  val to_sectors: t -> int64
end

module type ASYNC = sig
  type 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
end

module type READER = sig
  type in_channel
  type 'a io
  val really_read: in_channel -> Cstruct.t -> unit io
  val skip: in_channel -> int -> unit io
end

module type WRITER = sig
  type out_channel
  type 'a io
  val really_write: out_channel -> Cstruct.t -> unit io
end

module type HEADERREADER = sig
  type in_channel
  type 'a io

  (** Returns the next header block or error [`Eof] if two consecutive
      zero-filled blocks are discovered. Assumes stream is positioned at the
      possible start of a header block.
      @param global Holds the current global pax extended header, if
        any. Needs to be given to the next call to [read]. *)
  val read : global:Header.Extended.t option -> in_channel ->
    (Header.t * Header.Extended.t option, [ `Eof | `Fatal of [ `Checksum_mismatch | `Corrupt_pax_header | `Unmarshal of string ] ]) result io
end

module type HEADERWRITER = sig
  type out_channel
  type 'a io
  val write : ?level:Header.compatibility -> Header.t -> out_channel -> (unit, [> `Msg of string ]) result io
  val write_global_extended_header : Header.Extended.t -> out_channel -> (unit, [> `Msg of string ]) result io
end

module HeaderReader(Async: ASYNC)(Reader: READER with type 'a io = 'a Async.t) :
  HEADERREADER with type in_channel = Reader.in_channel and type 'a io = 'a Async.t

module HeaderWriter(Async: ASYNC)(Writer: WRITER with type 'a io = 'a Async.t) :
  HEADERWRITER with type out_channel = Writer.out_channel and type 'a io = 'a Async.t
