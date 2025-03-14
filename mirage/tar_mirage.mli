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

(** Tar for Mirage *)

module Make_KV_RO (BLOCK : Mirage_block.S) : sig
  (** Construct a read-only key-value store from an existing block device
      containing tar-format data. *)

  include Mirage_kv.RO

  val connect: BLOCK.t -> t Lwt.t
  (** [connect block]

      @raise Invalid_argument if [block] has a sector size that is not a
                              positive multiple of 512. *)
end

module Make_KV_RW (BLOCK : Mirage_block.S) : sig
  (** Construct a read-write key-value store from an existing block device
      containing tar-format data. Note that it is append-only meaning removing
      or renaming files is currently unsupported and will return an error. *)

  include Mirage_kv.RW

  val connect: BLOCK.t -> t Lwt.t
  (** [connect block]

      @raise Invalid_argument if [block] has a sector size that is not a
                              positive multiple of 512. *)

  val free : t -> int64
  (** [free t] is the number of unused bytes. *)
end
