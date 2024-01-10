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

module type READER = sig
  include Tar.READER

  val read : in_channel -> Cstruct.t -> int io
end

module Make
  (Async : Tar.ASYNC)
  (Writer : Tar.WRITER with type 'a io = 'a Async.t)
  (Reader : READER with type 'a io = 'a Async.t)
= struct
  open Async

  module Gz_writer = struct
    type out_channel =
      { mutable gz : Gz.Def.encoder
      ; ic_buffer : Cstruct.t
      ; oc_buffer : Cstruct.t
      ; out_channel : Writer.out_channel }

    type 'a io = 'a Async.t

    let really_write ({ gz; oc_buffer; out_channel; _ } as state) cs =
      let rec until_await gz =
        match Gz.Def.encode gz with
        | `Await gz -> state.gz <- gz ; Async.return ()
        | `Flush gz ->
          let max = Cstruct.length oc_buffer - Gz.Def.dst_rem gz in
          Writer.really_write out_channel (Cstruct.sub oc_buffer 0 max) >>= fun () ->
          let { Cstruct.buffer; off= cs_off; len= cs_len; } = oc_buffer in
          until_await (Gz.Def.dst gz buffer cs_off cs_len)
        | `End _gz -> assert false in
      if Cstruct.length cs = 0
      then Async.return ()
      else ( let { Cstruct.buffer; off; len; } = cs in
             let gz = Gz.Def.src gz buffer off len in
             until_await gz )
  end

  module Gz_reader = struct
    type in_channel =
      { mutable gz : Gz.Inf.decoder
      ; ic_buffer : Cstruct.t
      ; oc_buffer : Cstruct.t
      ; in_channel : Reader.in_channel
      ; mutable pos : int }

    type 'a io = 'a Async.t

    let really_read
      : in_channel -> Cstruct.t -> unit io
      = fun ({ ic_buffer; oc_buffer; in_channel; _ } as state) res ->
        let rec until_full_or_end gz res =
          match Gz.Inf.decode gz with
          | `Flush gz ->
            let max = Cstruct.length oc_buffer - Gz.Inf.dst_rem gz in
            let len = min (Cstruct.length res) max in
            Cstruct.blit oc_buffer 0 res 0 len ;
            if len < max
            then ( state.pos <- len
                 ; state.gz <- gz
                 ; Async.return () )
            else until_full_or_end (Gz.Inf.flush gz) (Cstruct.shift res len)
          | `End gz ->
            let max = Cstruct.length oc_buffer - Gz.Inf.dst_rem gz in
            let len = min (Cstruct.length res) max in
            Cstruct.blit oc_buffer 0 res 0 len ;
            if Cstruct.length res > len
            then raise End_of_file
            else ( state.pos <- len
                 ; state.gz <- gz
                 ; Async.return () )
          | `Await gz ->
            Reader.read in_channel ic_buffer >>= fun len ->
            let { Cstruct.buffer; off; len= _; } = ic_buffer in
            let gz = Gz.Inf.src gz buffer off len in
            until_full_or_end gz res
          | `Malformed err -> failwith ("gzip: " ^ err) in
        let max = (Cstruct.length oc_buffer - Gz.Inf.dst_rem state.gz) - state.pos in
        let len = min (Cstruct.length res) max in
        Cstruct.blit oc_buffer state.pos res 0 len ;

        if len < max
        then ( state.pos <- state.pos + len
             ; Async.return () )
        else ( let res = Cstruct.shift res len in
               until_full_or_end (Gz.Inf.flush state.gz) res )

    let skip
      : in_channel -> int -> unit io
      = fun state len ->
        let oc_buffer = Cstruct.create len in
        really_read state oc_buffer
  end

  module HeaderWriter = Tar.HeaderWriter (Async) (Gz_writer)
  module HeaderReader = Tar.HeaderReader (Async) (Gz_reader)

  type in_channel = Gz_reader.in_channel

  let of_in_channel ~internal:oc_buffer in_channel =
    let { Cstruct.buffer; off; len; } = oc_buffer in
    let o = Bigarray.Array1.sub buffer off len in
    { Gz_reader.gz= Gz.Inf.decoder `Manual ~o
    ; oc_buffer
    ; ic_buffer= Cstruct.create 0x1000
    ; in_channel
    ; pos= 0 }

  let really_read = Gz_reader.really_read
  let skip = Gz_reader.skip

  type out_channel = Gz_writer.out_channel

  let of_out_channel ?bits:(w_bits= 15) ?q:(q_len= 0x1000) ~level ~mtime os out_channel =
    let ic_buffer = Cstruct.create (4 * 4 * 1024) in
    let oc_buffer = Cstruct.create 4096 in
    let gz =
      let w = De.Lz77.make_window ~bits:w_bits in
      let q = De.Queue.create q_len in
      Gz.Def.encoder `Manual `Manual ~mtime os ~q ~w ~level in
    let { Cstruct.buffer; off; len; } = oc_buffer in
    let gz = Gz.Def.dst gz buffer off len in
    { Gz_writer.gz; ic_buffer; oc_buffer; out_channel; }

  let write_block ?level hdr ({ Gz_writer.ic_buffer= buf; oc_buffer; out_channel; _ } as state) block =
    HeaderWriter.write ?level hdr state >>= function
    | Error _ as e -> return e
    | Ok () ->
      (* XXX(dinosaure): we can refactor this codec with [Gz_writer.really_write]
         but this loop saves and uses [ic_buffer]/[buf] to avoid extra
         allocations on the case between [string] and [Cstruct.t]. *)
      let rec deflate (str, off, len) gz = match Gz.Def.encode gz with
        | `Await gz ->
          if len = 0
          then block () >>= function
            | None -> state.gz <- gz ; Async.return ()
            | Some str -> deflate (str, 0, String.length str) gz
          else ( let len' = min len (Cstruct.length buf) in
                 Cstruct.blit_from_string str off buf 0 len' ;
                 let { Cstruct.buffer; off= cs_off; len= _; } = buf in
                 deflate (str, off + len', len - len')
                   (Gz.Def.src gz buffer cs_off len') )
        | `Flush gz ->
          let max = Cstruct.length oc_buffer - Gz.Def.dst_rem gz in
          Writer.really_write out_channel (Cstruct.sub oc_buffer 0 max) >>= fun () ->
          let { Cstruct.buffer; off= cs_off; len= cs_len; } = oc_buffer in
          deflate (str, off, len) (Gz.Def.dst gz buffer cs_off cs_len)
        | `End _gz -> assert false in
      deflate ("", 0, 0) state.gz >>= fun () ->
      Gz_writer.really_write state (Tar.Header.zero_padding hdr) >>= fun () ->
      return (Ok ())

 let write_end ({ Gz_writer.oc_buffer; out_channel; _ } as state) =
   Gz_writer.really_write state Tar.Header.zero_block >>= fun () ->
   Gz_writer.really_write state Tar.Header.zero_block >>= fun () ->
   let rec until_end gz = match Gz.Def.encode gz with
     | `Await _gz -> assert false
     | `Flush gz | `End gz as flush_or_end ->
       let max = Cstruct.length oc_buffer - Gz.Def.dst_rem gz in
       Writer.really_write out_channel (Cstruct.sub oc_buffer 0 max) >>= fun () ->
       match flush_or_end with
       | `Flush gz ->
         let { Cstruct.buffer; off= cs_off; len= cs_len; } = oc_buffer in
         until_end (Gz.Def.dst gz buffer cs_off cs_len)
       | `End _gz -> Async.return () in
   until_end (Gz.Def.src state.gz De.bigstring_empty 0 0)
end
