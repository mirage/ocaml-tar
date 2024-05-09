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

external ba_get_int32_ne : De.bigstring -> int -> int32 = "%caml_bigstring_get32"
external ba_set_int32_ne : De.bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"

(*
let bigstring_to_string ?(off= 0) ?len ba =
  let len = match len with
    | Some len -> len
    | None -> De.bigstring_length ba - off in
  let res = Bytes.create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = ba_get_int32_ne ba i in
    Bytes.set_int32_ne res i v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = Bigarray.Array1.get ba i in
    Bytes.set res i v
  done;
  Bytes.unsafe_to_string res
*)

let bigstring_blit_string src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    (* TODO: use String.get_int32_ne when ocaml-tar requires OCaml >= 4.13 *)
    let v = Bytes.get_int32_ne (Bytes.unsafe_of_string src) (src_off + i) in
    ba_set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = String.get src (src_off + i) in
    Bigarray.Array1.set dst (dst_off + i) v
  done

let bigstring_blit_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = ba_get_int32_ne src (src_off + i) in
    Bytes.set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = Bigarray.Array1.get src (src_off + i) in
    Bytes.set dst (dst_off + i) v
  done

type decoder =
  { mutable gz : Gz.Inf.decoder
  ; ic_buffer : De.bigstring
  ; oc_buffer : De.bigstring
  ; tp_length : int
  ; mutable pos : int }

let really_read_through_gz
  : decoder -> bytes -> (unit, 'err, _) Tar.t
  = fun ({ ic_buffer; oc_buffer; tp_length; _ } as state) res ->
    let open Tar in
    let rec until_full_or_end gz (res, res_off, res_len) =
      match Gz.Inf.decode gz with
      | `Flush gz ->
        let max = De.bigstring_length oc_buffer - Gz.Inf.dst_rem gz in
        let len = min res_len max in
        bigstring_blit_bytes oc_buffer ~src_off:0 res ~dst_off:res_off ~len;
        if len < max
        then ( state.pos <- len
             ; state.gz <- gz
             ; return (Ok ()) )
        else until_full_or_end (Gz.Inf.flush gz) (res, res_off + len, res_len - len)
      | `End gz ->
        let max = De.bigstring_length oc_buffer - Gz.Inf.dst_rem gz in
        let len = min res_len max in
        bigstring_blit_bytes oc_buffer ~src_off:0 res ~dst_off:res_off ~len;
        if res_len > len
        then return (Error `Eof)
        else ( state.pos <- len
             ; state.gz <- gz
             ; return (Ok ()) )
      | `Await gz ->
        let* tp_buffer = Tar.read tp_length in
        let len = String.length tp_buffer in
        bigstring_blit_string tp_buffer ~src_off:0 ic_buffer ~dst_off:0 ~len;
        let gz = Gz.Inf.src gz ic_buffer 0 len in
        until_full_or_end gz (res, res_off, res_len)
      | `Malformed err -> return (Error (`Gz err)) in
    let max = (De.bigstring_length oc_buffer - Gz.Inf.dst_rem state.gz) - state.pos in
    let len = min (Bytes.length res) max in
    bigstring_blit_bytes oc_buffer ~src_off:state.pos res ~dst_off:0 ~len;
    if len < max
    then ( state.pos <- state.pos + len
         ; return (Ok ()) )
    else until_full_or_end (Gz.Inf.flush state.gz) (res, len, Bytes.length res - len)

let really_read_through_gz decoder len =
  let open Tar in
  let res = Bytes.create len in
  let* () = really_read_through_gz decoder res in
  Tar.return (Ok (Bytes.unsafe_to_string res))

type error = [ `Fatal of Tar.error | `Eof | `Gz of string ]

let seek_through_gz : decoder -> int -> (unit, [> error ], _) Tar.t = fun state len ->
  let open Tar in
  let* _buf = really_read_through_gz state len in
  Tar.return (Ok ())

let gzipped t =
  let rec go : type a. decoder -> (a, [> error ] as 'err, 't) Tar.t -> (a, 'err, 't) Tar.t = fun decoder -> function
    | Tar.Really_read len ->
      really_read_through_gz decoder len
    | Tar.Read _len -> assert false (* XXX(dinosaure): actually does not emit [Tar.Read]. *)
    | Tar.Seek len -> seek_through_gz decoder len
    | Tar.Return _ as ret -> ret
    | Tar.Bind (x, f) ->
      Tar.Bind (go decoder x, (fun x -> go decoder (f x)))
    | Tar.High _ as high -> high in
  let decoder =
    let oc_buffer = De.bigstring_create 0x1000 in
    { gz= Gz.Inf.decoder `Manual ~o:oc_buffer
    ; oc_buffer
    ; ic_buffer= De.bigstring_create 0x1000
    ; tp_length= 0x1000
    ; pos= 0 } in
  go decoder t

(*
module Make
  (Async : Tar.ASYNC)
  (Writer : Tar.WRITER with type 'a io = 'a Async.t)
  (Reader : READER with type 'a io = 'a Async.t)
= struct
  open Async

  module Gz_writer = struct
    type out_channel =
      { mutable gz : Gz.Def.encoder
      ; ic_buffer : De.bigstring
      ; oc_buffer : De.bigstring
      ; out_channel : Writer.out_channel }

    type 'a io = 'a Async.t

    let really_write ({ gz; ic_buffer; oc_buffer; out_channel; _ } as state) str =
      let rec until_await gz =
        match Gz.Def.encode gz with
        | `Await gz -> Async.return gz
        | `Flush gz ->
          let len = De.bigstring_length oc_buffer - Gz.Def.dst_rem gz in
          let str = bigstring_to_string oc_buffer ~off:0 ~len in
          Writer.really_write out_channel str >>= fun () ->
          until_await (Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer))
        | `End _gz -> assert false
      and go gz (str, str_off, str_len) =
        if str_len = 0
        then ( state.gz <- gz ; Async.return () )
        else ( let len = min str_len (De.bigstring_length ic_buffer) in
               bigstring_blit_string str ~src_off:0 ic_buffer ~dst_off:0 ~len;
               let gz = Gz.Def.src gz ic_buffer 0 len in
               until_await gz >>= fun gz ->
               go gz (str, str_off + len, str_len - len) ) in
      go gz (str, 0, String.length str)
  end

  type out_channel = Gz_writer.out_channel

  let of_out_channel ?bits:(w_bits= 15) ?q:(q_len= 0x1000) ~level ~mtime os out_channel =
    let ic_buffer = De.bigstring_create (4 * 4 * 1024) in
    let oc_buffer = De.bigstring_create 4096 in
    let gz =
      let w = De.Lz77.make_window ~bits:w_bits in
      let q = De.Queue.create q_len in
      Gz.Def.encoder `Manual `Manual ~mtime os ~q ~w ~level in
    let gz = Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer) in
    { Gz_writer.gz; ic_buffer; oc_buffer; out_channel; }

  let write_block ?level hdr ({ Gz_writer.ic_buffer= buf; oc_buffer; out_channel; _ } as state) block =
    HeaderWriter.write ?level hdr state >>= function
    | Error _ as e -> return e
    | Ok () ->
      (* XXX(dinosaure): we can refactor this codec with [Gz_writer.really_write]
         but this loop saves and uses [ic_buffer]/[buf] to avoid extra
         allocations on the case between [string] and [bigstring]. *)
      let rec deflate (str, off, len) gz = match Gz.Def.encode gz with
        | `Await gz ->
          if len = 0
          then block () >>= function
            | None -> state.gz <- gz ; Async.return ()
            | Some str -> deflate (str, 0, String.length str) gz
          else ( let len' = min len (De.bigstring_length buf) in
                 bigstring_blit_string str ~src_off:off buf ~dst_off:0 ~len:len';
                 deflate (str, off + len', len - len')
                   (Gz.Def.src gz buf 0 len') )
        | `Flush gz ->
          let len = De.bigstring_length oc_buffer - Gz.Def.dst_rem gz in
          let out = bigstring_to_string oc_buffer ~len in
          Writer.really_write out_channel out >>= fun () ->
          deflate (str, off, len) (Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer))
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
       let max = De.bigstring_length oc_buffer - Gz.Def.dst_rem gz in
       Writer.really_write out_channel (bigstring_to_string oc_buffer ~len:max) >>= fun () ->
       match flush_or_end with
       | `Flush gz ->
         until_end (Gz.Def.dst gz oc_buffer 0 (De.bigstring_length oc_buffer))
       | `End _gz -> Async.return () in
   until_end (Gz.Def.src state.gz De.bigstring_empty 0 0)
end
*)
