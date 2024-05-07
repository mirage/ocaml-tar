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

module High : sig
  type t
  type 'a s = 'a

  external inj : 'a s -> ('a, t) Tar.io = "%identity"
  external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a

  external inj : 'a -> 'b = "%identity"
  external prj : 'b -> 'a = "%identity"
end

type t = High.t

let value v = Tar.High (High.inj v)

let run t f =
  let rec run : type a. (a, 'err, t) Tar.t -> (a, 'err) result = function
    | Tar.Read len ->
      let b = Cstruct.create len in
      (match Flow.single_read f b with
       | len ->
         Ok (Cstruct.to_string ~len b)
       | exception End_of_file ->
         (* XXX: should we catch other exceptions?! *)
         Error `Unexpected_end_of_file)
    | Tar.Really_read len ->
      let b = Cstruct.create len in
      (try
         Flow.read_exact f b;
         Ok (Cstruct.to_string b)
       with End_of_file -> Error `Unexpected_end_of_file)
    | Tar.Seek n ->
      let buffer_size = 32768 in
      let buffer = Cstruct.create buffer_size in
      let rec loop (n: int) =
        if n <= 0 then Ok (-1) (* XXX: I dunno... *)
        else
          let amount = min n buffer_size in
          let block = Cstruct.sub buffer 0 amount in
          Flow.read_exact f block;
          loop (n - amount) in
      loop n
    | Tar.Return value -> value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) ->
      match run x with
      | Ok value -> run (f value)
      | Error _ as err -> err in
  run t

let fold f filename init =
  (* XXX(reynir): ??? *)
  Eio.Path.with_open_in filename
    (run (Tar.fold f init))

(* Eio needs a non-file-opening stat. *)
let stat path =
  Eio.Path.with_open_in path @@ fun f ->
  Eio.File.stat f

(** Return the header needed for a particular file on disk *)
let header_of_file ?level ?getpwuid ?getgrgid filepath : Tar.Header.t =
  let level = Tar.Header.compatibility level in
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
let extract ?filter:(_ = fun _ -> true) ~src:_ _dst =
  (* TODO *)
  failwith "TODO"

let create ?level:_ ?global:_ ?filter:(_ = fun _ -> true) ~src:_ _dst =
  (* TODO *)
  failwith "TODO"

let append_file ?level:_ ?header:_ _filename _dst =
  (* TODO *)
  failwith "TODO"

let write_header ?level:_ _hdr _fl =
  (* TODO *)
  failwith "TODO"

let write_global_extended_header ?level:_ _global _fl =
  (* TODO *)
  failwith "TODO"

let write_end fl =
  let zero_block = Cstruct.of_string Tar.Header.zero_block in
  (* TODO: catch exceptions?! *)
  Eio.Flow.write fl [ zero_block; zero_block ];
  Ok ()
