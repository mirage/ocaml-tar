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

let () = Printexc.record_backtrace true

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

(*
let create_tarball directory oc =
  let files = Sys.readdir directory in
  let os = match Sys.os_type with
    | "Win32" -> Gz.NTFS (* XXX(dinosaure): true? *)
    | "Unix" | "Cygwin" | _ -> Gz.Unix in
  let mtime = Unix.gettimeofday () in
  let out_channel = Tar_gz.of_out_channel ~level:4 ~mtime:(Int32.of_float mtime) os oc in
  let hdr = Tar.Header.make ~file_mode:0o755
      ~mod_time:(Int64.of_float mtime) (Filename.concat directory "") 0L in
  (match Tar_gz.write_block ~level:Tar.Header.Ustar hdr out_channel (always None) with
   | Ok () -> ()
   | Error `Msg msg -> Format.eprintf "Error %s writing block\n%!" msg);
  Array.iter begin fun filename ->
  let fd        = Unix.openfile (directory / filename) Unix.[ O_RDONLY; O_CLOEXEC ] 0o644 in
  let stat      = Unix.LargeFile.lstat (directory / filename) in
  match stat.st_kind with
  | Unix.S_REG ->
    let stream    = stream_of_fd fd in
    let file_mode = if stat.Unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644 in
    let mod_time  = Int64.of_float stat.Unix.LargeFile.st_mtime in
    let user_id   = stat.Unix.LargeFile.st_uid in
    let group_id  = stat.Unix.LargeFile.st_gid in
    let hdr = Tar.Header.make
        ~file_mode ~mod_time ~user_id ~group_id
        (directory / filename) stat.Unix.LargeFile.st_size in
    (match Tar_gz.write_block ~level:Tar.Header.Ustar hdr out_channel stream with
     | Ok () -> ()
     | Error `Msg msg -> Format.eprintf "Error %s writing block\n%!" msg);
    Unix.close fd ;
  | _ ->
    Format.eprintf "Skipping non-regular file %s\n" (Filename.concat directory filename)
  end files ;
  Tar_gz.write_end out_channel

let make directory oc =
  let oc, oc_close, _gz = match oc with
    | None -> stdout, ignore, false
    | Some filename ->
      let oc = open_out filename in
      oc, (fun () -> close_out oc), Filename.extension filename = ".gz" in
  create_tarball directory oc ; oc_close ()
   *)

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) ppf = function
  | 0L -> Format.fprintf ppf "0 byte"
  | n ->
      let n = Int64.to_float n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Format.fprintf ppf "%.*f %s" decimals r sizes.(int_of_float i)

let list filename =
  let go ?global:_ hdr () =
    Format.printf "%s (%s, %a)\n%!"
      hdr.Tar.Header.file_name
      (Tar.Header.Link.to_string hdr.link_indicator)
      (bytes_to_size ~decimals:2) hdr.Tar.Header.file_size ;
    (*
    (* Alternatively:
         let padding = Tar.Header.compute_zero_padding_length hdr in
         let data = Int64.to_int hdr.Tar.Header.file_size in
         let to_skip = data + padding in *)
    Tar_gz.skip ic to_skip ;
    go global ()
    | Error `Eof -> ()
    | Error `Fatal e ->
      Format.eprintf "Error listing archive: %a\n%!" Tar.pp_error e;
      exit 2
       *)
    let open Tar in
    let to_skip = Header.(Int64.to_int (to_sectors hdr) * length) in
    let* _ = seek to_skip in
    return (Ok ())
  in
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
  match Tar_unix.run (Tar_gz.gzipped (Tar.fold go ())) fd with
  | Ok () -> ()
  | Error (`Unix _) ->
    Format.eprintf "Some UNIX error occurred.\n%!"
  | Error (`Msg e) ->
    Format.eprintf "Some error: %s.\n%!" e
  | Error `Unexpected_end_of_file ->
    Format.eprintf "Unexpected end of file.\n%!"
  | Error `Eof | Error `Gz _ ->
    Format.eprintf "Some fatal error occurred.\n%!"
  | Error (`Fatal _) ->
    Format.eprintf "Some fatal error occurred.\n%!"

let () = match Sys.argv with
  | [| _; "list"; filename; |] when Sys.file_exists filename ->
    list filename
      (*
  | [| _; directory |] when Sys.is_directory directory ->
    make directory None
  | [| _; directory; output |] when Sys.is_directory directory ->
    make directory (Some output)
         *)
  | _ ->
    let cmd = Filename.basename Sys.argv.(0) in
    Format.eprintf "%s <directory> [<filename.tar.gz>]\n%s list <filename.tar.gz>\n" cmd cmd
