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

type decode_error =
  [ `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ]

let ( / ) = Eio.Path.( / )
let ( let* ) = Result.bind
let ( let+ ) v f = Result.map f v

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
    | Tar.Write _ -> assert false
    | Tar.Read len -> (
        let b = Cstruct.create len in
        match Eio.Flow.single_read f b with
        | len -> Ok (Cstruct.to_string ~len b)
        | exception End_of_file ->
            (* XXX: should we catch other exceptions?! *)
            Error `Unexpected_end_of_file)
    | Tar.Really_read len -> (
        let b = Cstruct.create len in
        try
          Eio.Flow.read_exact f b;
          Ok (Cstruct.to_string b)
        with End_of_file -> Error `Unexpected_end_of_file)
    | Tar.Seek n ->
        let _set : Optint.Int63.t =
          Eio.File.seek f (Optint.Int63.of_int n) `Set
        in
        Ok ()
    | Tar.Return value -> value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) -> (
        match run x with Ok value -> run (f value) | Error _ as err -> err)
  in
  run t

let fold f source init = run (Tar.fold f init) source
let stat path = Eio.Path.stat ~follow:true path

(** Return the header needed for a particular file on disk *)
let header_of_file ?level ?getpwuid ?getgrgid filepath : Tar.Header.t =
  let level = Tar.Header.compatibility level in
  let stat = stat filepath in
  let pwent = Option.map (fun f -> f stat.uid) getpwuid in
  let grent = Option.map (fun f -> f stat.gid) getgrgid in
  let uname = if level = V7 then Some "" else pwent in
  let gname = if level = V7 then Some "" else grent in
  let file_mode = stat.perm in
  let user_id = stat.uid |> Int64.to_int in
  let group_id = stat.gid |> Int64.to_int in
  let file_size = stat.size |> Optint.Int63.to_int64 in
  let mod_time = Int64.of_float stat.mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let devmajor = if level = Ustar then stat.dev |> Int64.to_int else 0 in
  let devminor = if level = Ustar then stat.rdev |> Int64.to_int else 0 in
  Tar.Header.make ~file_mode ~user_id ~group_id ~mod_time ~link_indicator
    ~link_name ?uname ?gname ~devmajor ~devminor (snd filepath) file_size

let copy dst len =
  let blen = 65536 in
  let rec read_write dst len =
    if len = 0 then value (Ok ())
    else
      let ( let* ) = Tar.( let* ) in
      let slen = min blen len in
      let* str = Tar.really_read slen in
      let* _written = Result.ok (Eio.Flow.copy_string str dst) |> value in
      read_write dst (len - slen)
  in
  read_write dst len

let extract ?(filter = fun _ -> true) src dst =
  let f ?global:_ hdr () =
    let ( let* ) = Tar.( let* ) in
    match (filter hdr, hdr.Tar.Header.link_indicator) with
    | true, Tar.Header.Link.Normal ->
        Eio.Path.with_open_out ~create:(`If_missing hdr.Tar.Header.file_mode)
          (dst / hdr.file_name)
        @@ fun dst -> copy dst (Int64.to_int hdr.Tar.Header.file_size)
    | _ ->
        let* () = Tar.seek (Int64.to_int hdr.Tar.Header.file_size) in
        Tar.return (Ok ())
  in
  fold f src ()

let write_strings fd datas =
  List.iter (fun d -> Eio.Flow.copy_string d fd) datas

let write_header ?level hdr fl =
  let+ bytes = Tar.encode_header ?level hdr in
  write_strings fl bytes

let copy src sink len =
  let blen = 65536 in
  let buf = Cstruct.create blen in
  let rec read_and_write len =
    if len = 0 then Ok ()
    else
      match Eio.Flow.single_read src buf with
      | n ->
          Eio.Flow.write sink [ Cstruct.sub buf 0 n ];
          read_and_write (len - n)
      | exception End_of_file -> Error (`Msg "Unexpected end of file")
  in
  read_and_write len

let append_file ?level ?header filename dst =
  let header =
    match header with None -> header_of_file ?level filename | Some x -> x
  in
  let* () = write_header ?level header dst in
  Eio.Path.with_open_in filename @@ fun src ->
  (* TOCTOU [also, header may not be valid for file] *)
  copy src dst (Int64.to_int header.Tar.Header.file_size)

let write_global_extended_header ?level header sink =
  Result.map (write_strings sink)
    (Tar.encode_global_extended_header ?level header)

let write_end fl =
  write_strings fl [ Tar.Header.zero_block; Tar.Header.zero_block ]

let create ?level ?global ?(filter = fun _ -> true) ~src dst =
  match global with
  | None -> Ok ()
  | Some hdr ->
      let* () = write_global_extended_header ?level hdr dst in
      let rec copy_files directory =
        let rec next = function
          | [] -> Ok ()
          | name :: names -> (
              try
                let filename = directory / name in
                let header = header_of_file ?level filename in
                if filter header then
                  match header.Tar.Header.link_indicator with
                  | Normal ->
                      let* () = append_file ?level ~header filename dst in
                      next names
                  | Directory ->
                      (* TODO first finish curdir (and close the dir fd), then go deeper *)
                      let* () = copy_files filename in
                      next names
                  | _ -> Ok () (* NYI *)
                else Ok ()
              with End_of_file -> Ok ())
        in
        next (Eio.Path.read_dir directory)
      in
      let+ () = copy_files src in
      write_end dst
