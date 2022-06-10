(*
 * Copyright (C) 2022 Romain Calascibetta <romain.calascibetta@gmail.com>
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

module Make
  (Async : Tar.ASYNC)
  (Writer : Tar.WRITER with type 'a t = 'a Async.t) : sig
  type out_channel

  val of_out_channel : ?bits:int -> ?q:int -> level:int ->
    mtime:int32 -> Gz.os -> Writer.out_channel -> out_channel

  val write_block : ?level:Tar.Header.compatibility -> Tar.Header.t ->
    out_channel -> (unit -> string option Async.t) -> unit Async.t
  (** [write_block hdr oc stream] writes [hdr], then {i deflate} the given
      [stream], then zero-pads so the stream is positionned for the next
      block.

      A simple usage to write a file:
      {[
        let stream_of_fd fd =
          let buf = Bytes.create 0x1000 in
          fun () -> match Unix.read fd buf 0 (Bytes.length buf) with
          | 0 -> None
          | len -> Some (Bytes.sub_string buf 0 len)
          | exception End_of_file -> None

        let add_file oc filename =
          let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
          let hdr = Tar.Header.make ... in
          write_block hdr oc (stream_of_fd fd) ;
          Unix.close fd
      ]} *)

  val write_end : out_channel -> unit Async.t
  (** [write_end oc] writes a stream terminator to [oc]. *)
end
