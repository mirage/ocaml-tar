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

module Tar_gz = Tar_gz.Make
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (struct type out_channel = Stdlib.out_channel
          type 'a t = 'a
          let really_write oc cs =
            let str = Cstruct.to_string cs in
            output_string oc str end)

let ( / ) = Filename.concat

let stream_of_fd fd =
  let buf = Bytes.create 0x1000 in
  fun () -> match Unix.read fd buf 0 (Bytes.length buf) with
  | 0 -> None
  | len ->
    let str = Bytes.sub_string buf 0 len in
    Some str
  | exception End_of_file -> None

let always x = fun _ -> x

let create_tarball directory oc =
  let files = Sys.readdir directory in
  let os = match Sys.os_type with
    | "Win32" -> Gz.NTFS (* XXX(dinosaure): true? *)
    | "Unix" | "Cygwin" | _ -> Gz.Unix in
  let mtime = Unix.gettimeofday () in
  let out_channel = Tar_gz.of_out_channel ~level:4 ~mtime:(Int32.of_float mtime) os oc in
  let hdr = Tar.Header.make ~file_mode:0o755
    ~mod_time:(Int64.of_float mtime) (directory  ^ "/") 0L in
  Tar_gz.write_block ~level:Tar.Header.Ustar hdr out_channel (always None) ;
  Array.iter begin fun filename -> 
  let fd        = Unix.openfile (directory / filename) Unix.[ O_RDONLY; O_CLOEXEC ] 0o644 in
  let stat      = Unix.LargeFile.lstat (directory / filename) in
  let stream    = stream_of_fd fd in
  let file_mode = if stat.Unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644 in
  let mod_time  = Int64.of_float stat.Unix.LargeFile.st_mtime in
  let user_id   = stat.Unix.LargeFile.st_uid in
  let group_id  = stat.Unix.LargeFile.st_gid in
  let hdr = Tar.Header.make
    ~file_mode ~mod_time ~user_id ~group_id
    (directory / filename) stat.Unix.LargeFile.st_size in
  Tar_gz.write_block ~level:Tar.Header.Ustar hdr out_channel stream ;
  Unix.close fd ;
  end files ;
  Tar_gz.write_end out_channel

let run directory oc =
  let oc, oc_close, _gz = match oc with
    | None -> stdout, ignore, false
    | Some filename ->
      let oc = open_out filename in
      oc, (fun () -> close_out oc), Filename.extension filename = ".gz" in
  create_tarball directory oc ; oc_close ()

let () = match Sys.argv with
  | [| _; directory |] when Sys.is_directory directory ->
    run directory None
  | [| _; directory; output |] when Sys.is_directory directory ->
    run directory (Some output)
  | _ -> Format.eprintf "%s <directory> [<filename.tar.gz>]" Sys.argv.(0)
