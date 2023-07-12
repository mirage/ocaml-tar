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
  let return_unit = ()
end

module Reader = struct
  type in_channel = Flow.source
  type 'a t = 'a Monad.t
  let really_read f b = Flow.read_exact f b |> Monad.return
  let skip f (n: int) =
    let open Monad in
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n: int) =
      if n <= 0 then Monad.return ()
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        really_read f block >>= fun () ->
        loop (n - amount) in
    loop n
end
let really_read = Reader.really_read

module Writer = struct
  type out_channel = Flow.sink
  type 'a t = 'a Monad.t
  let really_write f b = Flow.write f [ b ] |> Monad.return
end
let really_write = Writer.really_write

let copy_n ifd ofd n =
  let open Monad in
  let block_size = 32768 in
  let buffer = Cstruct.create block_size in
  let rec loop remaining =
    if remaining = 0L then Monad.return_unit else begin
      let this = Int64.(to_int (min (of_int block_size) remaining)) in
      let block = Cstruct.sub buffer 0 this in
      really_read ifd block >>= fun () ->
      really_write ofd block >>= fun () ->
      loop (Int64.(sub remaining (of_int this)))
    end in
  loop n

module HR = Tar.HeaderReader(Monad)(Reader)
module HW = Tar.HeaderWriter(Monad)(Writer)

let get_next_header ?level ic =
  match HR.read ?level (ic :> Flow.source) with
  | Error `Eof -> Monad.return None
  | Ok hdrs -> Monad.return (Some hdrs)

(* Eio needs a non-file-opening stat. *)
let stat path =
  Eio.Path.with_open_in path @@ fun f ->
  Eio.File.stat f

(** Return the header needed for a particular file on disk *)
let header_of_file ?level ?getpwuid ?getgrgid filepath : Tar.Header.t =
  let level = match level with None -> Tar.Header.V7 | Some level -> level in
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
  Monad.return (Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
                  ?uname ?gname ~devmajor ~devminor (snd filepath) file_size)

let write_block ?level (header: Tar.Header.t) (body: #Flow.sink -> unit) sink =
  HW.write ?level header (sink :> Flow.sink);
  body sink;
  really_write sink (Tar.Header.zero_padding header)

let write_end sink =
  really_write sink Tar.Header.zero_block;
  really_write sink Tar.Header.zero_block

(** Utility functions for operating over whole tar archives *)
module Archive = struct

  (** Read the next header, apply the function 'f' to the fd and the header. The function
      should leave the fd positioned immediately after the datablock. Finally the function
      skips past the zero padding to the next header *)
  let with_next_file src (f: Eio.Flow.source -> Tar.Header.t -> 'a) =
    match get_next_header src with
    | Some hdr ->
      let result = f src hdr in
      Reader.skip src (Tar.Header.compute_zero_padding_length hdr);
      Some result
    | None ->
      None

  (** List the contents of a tar *)
  let list ?level fd =
    let rec loop acc =
      match get_next_header ?level (fd :> Flow.source) with
      | None -> Monad.return (List.rev acc)
      | Some hdr ->
        Reader.skip fd (Int64.to_int hdr.Tar.Header.file_size);
        Reader.skip fd (Tar.Header.compute_zero_padding_length hdr);
        loop (hdr :: acc) in
    loop []

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd =
    let rec loop () =
      match get_next_header ifd with
      | None -> Monad.return_unit
      | Some hdr ->
        let filename = dest hdr.Tar.Header.file_name in
        Eio.Path.(with_open_out ~create:(`Exclusive 0) filename) @@ fun ofd ->
        copy_n ifd ofd hdr.Tar.Header.file_size;
        Reader.skip ifd (Tar.Header.compute_zero_padding_length hdr);
        loop ()
    in
    loop ()

  let transform ?level f (ifd : #Flow.source) (ofd : #Flow.sink) =
    let rec loop () =
      match get_next_header ifd with
      | None -> Monad.return_unit
      | Some header' ->
        let header = f header' in
        let body = fun _ -> copy_n ifd ofd header.Tar.Header.file_size in
        write_block ?level header body ofd;
        Reader.skip ifd (Tar.Header.compute_zero_padding_length header');
        loop ()
    in
    loop ();
    write_end ofd

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create ?getpwuid ?getgrgid files ofd =
    let file filename =
      let stat = stat filename in
      if stat.kind <> `Regular_file then
        (* Skipping, not a regular file. *)
        Monad.return_unit
      else begin
        let hdr = header_of_file ?getpwuid ?getgrgid filename in
        write_block hdr (fun ofd ->
            Eio.Path.with_open_in filename @@ fun ifd ->
            copy_n ifd ofd hdr.Tar.Header.file_size
          ) ofd
      end in
    List.iter file files;
    (* Add two empty blocks *)
    write_end ofd

end
