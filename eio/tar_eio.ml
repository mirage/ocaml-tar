(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
 * Copyright (C)      2012 Thomas Gazagnaire <thomas@ocamlpro.com>
 * Copyright (C)      2023 Patrick Ferris <patrick@sirref.org>
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

open Eio

module Monad = struct
  type 'a t = 'a
  let (>>=) a f = f a
  let return = Fun.id
end

module Io = struct
  type in_channel = Flow.source
  type 'a io = 'a
  let really_read f b =
    let len = Bytes.length b in
    let cs = Cstruct.create len in
    Flow.read_exact f cs;
    Cstruct.blit_to_bytes cs 0 b 0 len
  let skip f (n: int) =
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n: int) =
      if n <= 0 then ()
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        Flow.read_exact f block;
        loop (n - amount) in
    loop n

  type out_channel = Flow.sink
  let really_write f str = Flow.write f [ Cstruct.of_string str ]
end

let really_read = Flow.read_exact
let skip = Io.skip
let really_write f b = Flow.write f [ b ]

module HeaderReader = Tar.HeaderReader(Monad)(Io)
module HeaderWriter = Tar.HeaderWriter(Monad)(Io)

(* Eio needs a non-file-opening stat. *)
let stat path =
  Eio.Path.with_open_in path @@ fun f ->
  Eio.File.stat f

(** Return the header needed for a particular file on disk *)
let header_of_file ?level ?getpwuid ?getgrgid filepath : Tar.Header.t =
  let level = match level with None -> !Tar.Header.compatibility_level | Some level -> level in
  let stat = stat filepath in
  let pwent = Option.map (fun f -> f stat.uid) getpwuid in
  let grent = Option.map (fun f -> f stat.gid) getgrgid in
  let uname       = if level = V7 then Some "" else pwent in
  let gname       = if level = V7 then Some "" else grent in
  let file_mode   = stat.perm in
  let user_id     = stat.uid |> Int64.to_int in
  let group_id    = stat.gid |> Int64.to_int in
  let file_size   = stat.size |> Optint.Int63.to_int64 in
  let mod_time    = Int64.of_float stat.mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name   = "" in
  let devmajor    = if level = Ustar then stat.dev |> Int64.to_int else 0 in
  let devminor    = if level = Ustar then stat.rdev |> Int64.to_int else 0 in
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
                  ?uname ?gname ~devmajor ~devminor (snd filepath) file_size
