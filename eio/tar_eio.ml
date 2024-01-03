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

module Reader = struct
  type in_channel = Flow.source_ty Resource.t
  type 'a t = 'a
  let really_read f b = Flow.read_exact f b
  let skip f (n: int) =
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n: int) =
      if n <= 0 then ()
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        really_read f block;
        loop (n - amount) in
    loop n
end
let really_read = Reader.really_read

module Writer = struct
  type out_channel = Flow.sink_ty Resource.t
  type 'a t = 'a
  let really_write f b = Flow.write f [ b ]
end
let really_write = Writer.really_write

let copy_n ifd ofd n =
  let block_size = 32768 in
  let buffer = Cstruct.create block_size in
  let rec loop remaining =
    if remaining = 0L then () else begin
      let this = Int64.(to_int (min (of_int block_size) remaining)) in
      let block = Cstruct.sub buffer 0 this in
      really_read ifd block;
      really_write ofd block;
      loop (Int64.(sub remaining (of_int this)))
    end in
  loop n

module HR = Tar.HeaderReader(Monad)(Reader)
module HW = Tar.HeaderWriter(Monad)(Writer)

let get_next_header ?level ~global ic =
  match HR.read ?level ~global (ic :> Eio.Flow.source_ty Eio.Flow.source) with
  | Error `Eof -> None
  | Ok hdrs -> Some hdrs

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
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator ~link_name
                  ?uname ?gname ~devmajor ~devminor (snd filepath) file_size

let write_block ?level ?global (header: Tar.Header.t) body sink =
  HW.write ?level ?global header (sink :> Eio.Flow.sink_ty Eio.Flow.sink);
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
  let with_next_file src ~(global: Tar.Header.Extended.t option)
      (f: _ -> Tar.Header.Extended.t option -> Tar.Header.t -> 'a) =
    match get_next_header ~global src with
    | Some (hdr, global) ->
      let result = f src global hdr in
      Reader.skip src (Tar.Header.compute_zero_padding_length hdr);
      Some result
    | None ->
      None

  (** List the contents of a tar *)
  let list ?level fd =
    let rec loop global acc =
      match get_next_header ?level ~global fd with
      | None -> List.rev acc
      | Some (hdr, global) ->
        Reader.skip fd (Int64.to_int hdr.Tar.Header.file_size);
        Reader.skip fd (Tar.Header.compute_zero_padding_length hdr);
        loop global (hdr :: acc) in
    loop None []

  (** Extract the contents of a tar to directory 'dest' *)
  let extract dest ifd =
    let rec loop global () =
      match get_next_header ~global ifd with
      | None -> ()
      | Some (hdr, global) ->
        let filename = dest hdr.Tar.Header.file_name in
        Eio.Path.(with_open_out ~create:(`Exclusive 0) filename) @@ fun ofd ->
        copy_n ifd ofd hdr.Tar.Header.file_size;
        Reader.skip ifd (Tar.Header.compute_zero_padding_length hdr);
        loop global ()
    in
    loop None ()

  let transform ?level f ifd ofd =
    let rec loop global () =
      match get_next_header ~global ifd with
      | None -> ()
      | Some (header', global') ->
        let header = f header' in
        let body = fun _ -> copy_n ifd ofd header.Tar.Header.file_size in
        write_block ?level ?global:(if global <> global' then global' else None) header body ofd;
        Reader.skip ifd (Tar.Header.compute_zero_padding_length header');
        loop global' ()
    in
    loop None ();
    write_end ofd

  (** Create a tar on file descriptor fd from the filename list
      'files' *)
  let create ?getpwuid ?getgrgid files ofd =
    let file filename =
      let stat = stat filename in
      if stat.kind <> `Regular_file then
        (* Skipping, not a regular file. *)
        ()
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
