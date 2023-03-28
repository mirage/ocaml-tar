(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

[@@@warning "-3-27"] (* FIXME: deprecation from the tar library *)

open Lwt.Infix

let convert_path os path =
  let ch = Unix.open_process_in (Printf.sprintf "cygpath -%c -- %s" (match os with `Mixed -> 'm' | `Unix -> 'u' | `Windows -> 'w') path) in
  let line = input_line ch in
  close_in ch;
  line

module Unix = struct
  include Unix

  let openfile path =
    if Sys.win32 then openfile (convert_path `Windows path) else openfile path
  let stat path =
    if Sys.win32 then stat (convert_path `Windows path) else stat path
  let truncate path =
    if Sys.win32 then truncate (convert_path `Windows path) else truncate path
end

let cstruct = Alcotest.testable
                (fun f x -> Fmt.pf f "%a" Cstruct.hexdump_pp x)
                Cstruct.equal
let pp_header f x = Fmt.pf f "%s" (Tar.Header.to_detailed_string x)
let header =
  Alcotest.testable (fun f x -> Fmt.pf f "%a" (Fmt.option pp_header) x) ( = )

let header () =
  (* check header marshalling and unmarshalling *)
  let h = Tar.Header.make ~file_mode:5 ~user_id:1001 ~group_id:1002 ~mod_time:55L ~link_name:"" "hello" 1234L in
  let txt = "hello\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0000000005\0000001751\0000001752\00000000002322\00000000000067\0000005534\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" in
  let c = Cstruct.create (String.length txt) in
  Cstruct.blit_from_string txt 0 c 0 (String.length txt);
  let c' = Cstruct.create Tar.Header.length in
  for i = 0 to Tar.Header.length - 1 do Cstruct.set_uint8 c' i 0 done;
  Tar.Header.marshal c' h;
  Alcotest.(check cstruct) "marshalled headers" c c';
  Alcotest.(check header) "unmarshalled headers" (Some h) (Tar.Header.unmarshal c');
  Alcotest.(check int) "zero padding length" 302 (Tar.Header.compute_zero_padding_length h)

let set_difference a b = List.filter (fun a -> not(List.mem a b)) a

let with_file ~prefix ~suffix f =
  let filename = Filename.temp_file prefix suffix in
  Fun.protect (fun () -> f filename) ~finally:(fun () -> Sys.remove filename)

let with_tmpdir f =
  let filename = Filename.(temp_file (__FILE__ |> basename |> remove_extension) "") in
  Sys.remove filename;
  Unix.mkdir filename 0o700;
  Fun.protect (fun () -> f filename) ~finally:(fun () -> Unix.rmdir filename)

let with_tar ?(level:Tar.Header.compatibility option) ?files ?(sector_size = 512) () f =
  let format = match level with
    | None -> ""
    | Some format -> "--format=" ^ match format with
      | Tar.Header.OldGNU -> "oldgnu" | GNU -> "gnu" | V7 -> "v7" | Ustar -> "ustar" | Posix -> "posix"
  in
  let files = match files with
    | None -> List.map (fun x -> "lib/" ^ x) (Array.to_list (Sys.readdir "lib"))
    | Some files -> files in
  with_file ~prefix:"tar-test" ~suffix:".tar" @@ fun tar_filename ->
  let tar_filename = if Sys.win32 then convert_path `Unix tar_filename else tar_filename in
  let tar_block_size = sector_size / 512 in
  let cmdline = Printf.sprintf "tar -cf %s -b %d %s %s" tar_filename tar_block_size format (String.concat " " files) in
  begin match Unix.system cmdline with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n -> failwith (Printf.sprintf "%s: exited with %d" cmdline n)
    | _ -> failwith (Printf.sprintf "%s: unknown error" cmdline)
  end;
  f tar_filename files

let can_read_tar () =
  with_tar () @@ fun tar_filename files ->
  let fd = Unix.openfile tar_filename [ O_RDONLY; O_CLOEXEC ] 0 in
  let files' = List.map (fun t -> t.Tar.Header.file_name) (Tar_unix.Archive.list fd) in
  Unix.close fd;
  let missing = set_difference files files' in
  let missing' = set_difference files' files in
  Alcotest.(check (list string)) "missing" [] missing;
  Alcotest.(check (list string)) "missing'" [] missing'

let can_write_pax () =
  let open Tar_unix in
  with_file ~prefix:"tar-test" ~suffix:".tar" @@ fun filename ->
  (* This userid is too large for a regular ustar header *)
  let user_id = 0x07777777 + 1 in
  (* Write a file which would need a pax header *)
  let fd = Unix.openfile filename [ O_CREAT; O_WRONLY; O_CLOEXEC ] 0o0644 in
  Fun.protect
    (fun () ->
      let hdr = Tar.Header.make ~user_id "test" 0L in
      write_block hdr (fun _ -> ()) fd;
      write_end fd;
    ) ~finally:(fun () -> Unix.close fd);
  (* Read it back and verify the header was read *)
  let fd = Unix.openfile filename [ O_RDONLY; O_CLOEXEC ] 0 in
  Fun.protect
    (fun () ->
      match Archive.list fd with
      | [ one ] -> Alcotest.(check int) "user_id" user_id one.Tar.Header.user_id
      | xs -> Alcotest.failf "Headers = %a" (Fmt.list pp_header) xs
    ) ~finally:(fun () -> Unix.close fd)


let can_list_longlink_tar () =
  let open Tar_unix in
  let fd = Unix.openfile "lib_test/long.tar" [ O_RDONLY; O_CLOEXEC ] 0o0 in
  Fun.protect
    (fun () ->
      let all = Archive.list fd in
      let filenames = List.map (fun h -> h.Tar.Header.file_name) all in
      (* List.iteri (fun i x -> Printf.fprintf stderr "%d: %s\n%!" i x) filenames; *)
      let expected = [
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/";
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/BCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/";
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/BCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/CDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.txt";
      ] in
      Alcotest.(check (list string)) "respects filenames" expected filenames
    ) ~finally:(fun () -> Unix.close fd)

let starts_with ~prefix s =
  let len_s = String.length s
  and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.unsafe_get s i <> String.unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

let can_transform_tar () =
  let level = Tar.Header.Ustar in
  with_tar ~level () @@ fun tar_in _file_list ->
  let fd_in = Unix.openfile tar_in [O_RDONLY] 0 in
  let tar_out = Filename.temp_file "tar-transformed" ".tar" in
  let fd_out = Unix.openfile tar_out [ O_WRONLY; O_CREAT; O_CLOEXEC ] 0o644 in
  with_tmpdir @@ fun temp_dir ->
  Tar_unix.Archive.transform ~level (fun hdr ->
      {hdr with Tar.Header.file_name = Filename.concat temp_dir hdr.file_name})
    fd_in fd_out;
  Unix.close fd_in;
  Unix.close fd_out;
  let fd_in = Unix.openfile tar_out [ O_RDONLY; O_CLOEXEC ] 0 in
  Tar_unix.Archive.with_next_file fd_in (fun _fd_in hdr ->
      Alcotest.(check bool) "Filename was transformed" true (starts_with ~prefix:temp_dir hdr.file_name));
  Unix.close fd_in

module Block4096 = struct
  include Block

  let sector_size = 4096

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect ~prefered_sector_size:(Some 4096) name
end

module type BLOCK = sig
  include Mirage_block.S
  val connect: string -> t Lwt.t
  val sector_size : int
end

module B = struct
  include Block

  let sector_size = 512

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect ~prefered_sector_size:(Some 512) name
end

module Test(B: BLOCK) = struct
  let add_data_to_tar ?(level:Tar.Header.compatibility option) ?files switch () f =
    with_tar ?level ?files ~sector_size:B.sector_size () @@ fun tar_filename files ->
    let size = Unix.(stat tar_filename).st_size in
    let size = B.sector_size * ((pred size + 4096 + B.sector_size) / B.sector_size) in
    Unix.truncate tar_filename size;
    B.connect tar_filename >>= fun b ->
    let module KV_RW = Tar_mirage.Make_KV_RW(Pclock)(B) in
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") "foobar" >>= fun x ->
    Result.iter_error (fun e ->
        failwith (Fmt.to_to_string KV_RW.pp_write_error e))
      x;
    let files = "barf" :: files in
    f tar_filename files

  let add_more_data_to_tar ?(level:Tar.Header.compatibility option) ?files switch () f =
    with_tar ?level ?files ~sector_size:B.sector_size () @@ fun tar_filename files ->
    let size = Unix.(stat tar_filename).st_size in
    (* Add 4 KB rounding up to block size *)
    let size = B.sector_size * ((pred size + 4096 + B.sector_size) / B.sector_size) in
    Unix.truncate tar_filename size;
    B.connect tar_filename >>= fun b ->
    let module KV_RW = Tar_mirage.Make_KV_RW(Pclock)(B) in
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") "foobar" >>= fun x ->
    Result.iter_error (fun e ->
        failwith (Fmt.to_to_string KV_RW.pp_write_error e))
      x;
    KV_RW.set t (Mirage_kv.Key.v "barf2") "foobar2" >>= fun x ->
    Result.iter_error (fun e ->
        failwith (Fmt.to_to_string KV_RW.pp_write_error e))
      x;
    let files = "barf" :: "barf2" :: files in
    f tar_filename files

  let write_with_full_archive ?(level:Tar.Header.compatibility option) ?files switch () =
    with_tar ?level ?files () @@ fun tar_filename files ->
    B.connect tar_filename >>= fun b ->
    let module KV_RW = Tar_mirage.Make_KV_RW(Pclock)(B) in
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") "foobar" >>= function
    | Error `No_space -> Lwt.return ()
    | _ -> failwith "expected `No_space"

  let check_tar tar_filename files =
    B.connect tar_filename >>= fun b ->
    let module KV_RO = Tar_mirage.Make_KV_RO(B) in
    KV_RO.connect b >>= fun k ->
    files |> Lwt_list.iter_s @@ fun file ->
      let size =
        if file = "barf" then 6L
        else if file = "barf2" then 7L
        else Unix.LargeFile.((stat file).st_size)
      in
      let read_file key ofs len =
        if key = "barf" then String.sub "foobar" ofs len
        else if key = "barf2" then String.sub "foobar2" ofs len
        else
          let fd = Unix.openfile key [ O_RDONLY; O_CLOEXEC ] 0 in
          Fun.protect
            (fun () ->
               let (_: int) = Unix.lseek fd ofs Unix.SEEK_SET in
               let buf = Bytes.make len '\000' in
               let len' = Unix.read fd buf 0 len in
               Alcotest.(check int) "same length" len len';
               Bytes.to_string buf
            ) ~finally:(fun () -> Unix.close fd) in
      let read_tar key =
        KV_RO.get k key >>= function
        | Error e -> Fmt.failwith "KV_RO.read (%a) %a" Mirage_kv.Key.pp key KV_RO.pp_error e
        | Ok buf -> Lwt.return buf in
      (* Read whole file *)
      let value = read_file file 0 (Int64.to_int size) in
      read_tar (Mirage_kv.Key.v file) >>= fun value' ->
      Alcotest.(check string) "same content" value value';
      if Int64.compare size 2L = 1 then begin
        let value = read_file file 1 ((Int64.to_int size) - 2) in
        read_tar (Mirage_kv.Key.v file) >>= fun value' ->
        let value'' = String.sub value' 1 ((Int64.to_int size) - 2) in
        Alcotest.(check string) "same content" value value'';
        Lwt.return_unit
      end else Lwt.return_unit

  let can_read_through_BLOCK ~files switch () =
    with_tar ~files ~sector_size:B.sector_size () check_tar

  let write_test switch () =
     add_data_to_tar switch () check_tar

  let write_more_test switch () =
     add_more_data_to_tar switch () check_tar

  let check_not_padded switch () =
    Unix.openfile "empty" [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o644 |> Unix.close;
    can_read_through_BLOCK ~files:["empty"] switch ()
end

module Sector512 = Test(B)
module Sector4096 = Test(Block4096)

let () =
  let ( >:: ) desc f = Alcotest.test_case desc `Quick f in
  let suite = "parse_test", [
      "header" >:: header;
      "can_read_tar" >:: can_read_tar;
      "can write pax headers" >:: can_write_pax;
      "can read @Longlink" >:: can_list_longlink_tar;
      "can transform tars" >:: can_transform_tar;
    ]
  in
  let ( >:: ) desc f = Alcotest_lwt.test_case desc `Quick f in
  let lwt_suite = "parse_test-lwt", [
      "can_read_through_BLOCK/512" >:: Sector512.can_read_through_BLOCK ~files:[];
      "not 4KiB padded" >:: Sector512.check_not_padded;
      "can_read_through_BLOCK/4096" >:: Sector4096.can_read_through_BLOCK ~files:[];
      "add_data_to_tar BLOCK/512" >:: Sector512.write_test;
      "add_more_data_to_tar BLOCK/512" >:: Sector512.write_more_test;
      "write_with_full_archive BLOCK/512" >:: Sector512.write_with_full_archive;
      "add_data_to_tar BLOCK/4096" >:: Sector4096.write_test;
      "add_more_data_to_tar BLOCK/4096" >:: Sector4096.write_more_test;
    ]
  in
  (* pwd = _build/default/lib_test *)
  Unix.chdir "../../..";
  Alcotest.run "parse-test" [suite];
  Lwt_main.run @@ Alcotest_lwt.run "parse-test-lwt" [lwt_suite]
