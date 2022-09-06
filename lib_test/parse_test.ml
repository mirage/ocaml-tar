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

open OUnit2
open Lwt.Infix

let convert_path os path =
  let ch = Unix.open_process_in (Printf.sprintf "cygpath -%c -- %s" (match os with `Mixed -> 'm' | `Unix -> 'u' | `Windows -> 'w') path) in
  let line = input_line ch in
  close_in ch;
  line

let win32_openfile path flags perms =
  Unix.openfile (convert_path `Windows path) flags perms

module Unix = struct
  include Unix

  let openfile = if Sys.win32 then win32_openfile else openfile
end

exception Cstruct_differ

let cstruct_equal a b =
  let check_contents a b =
    try
      for i = 0 to Cstruct.length a - 1 do
        let a' = Cstruct.get_char a i in
        let b' = Cstruct.get_char b i in
        if a' <> b' then raise Cstruct_differ
      done;
      true
    with _ -> false in
  (Cstruct.length a = (Cstruct.length b)) && (check_contents a b)

let header _test_ctxt =
  (* check header marshalling and unmarshalling *)
  let h = Tar.Header.make ~file_mode:5 ~user_id:1001 ~group_id:1002 ~mod_time:55L ~link_name:"" "hello" 1234L in
  let txt = "hello\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0000000005\0000001751\0000001752\00000000002322\00000000000067\0000005534\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" in
  let c = Cstruct.create (String.length txt) in
  Cstruct.blit_from_string txt 0 c 0 (String.length txt);
  let c' = Cstruct.create Tar.Header.length in
  for i = 0 to Tar.Header.length - 1 do Cstruct.set_uint8 c' i 0 done;
  Tar.Header.marshal c' h;
  assert_equal ~printer:(fun x -> String.escaped (Cstruct.to_string x)) ~cmp:cstruct_equal c c';
  let printer = function
    | None -> "None"
    | Some x -> "Some " ^ (Tar.Header.to_detailed_string x) in
  assert_equal ~printer (Some h) (Tar.Header.unmarshal c');
  assert_equal ~printer:string_of_int 302 (Tar.Header.compute_zero_padding_length h)

let set_difference a b = List.filter (fun a -> not(List.mem a b)) a

let with_tar ?(level:Tar.Header.compatibility option) ?files test_ctxt f =
  let format = match level with
    | None -> ""
    | Some format -> "--format=" ^ match format with
      | Tar.Header.OldGNU -> "oldgnu" | GNU -> "gnu" | V7 -> "v7" | Ustar -> "ustar" | Posix -> "posix"
  in
  let files = match files with
    | None -> List.map (fun x -> "lib/" ^ x) (Array.to_list (Sys.readdir "lib"))
    | Some files -> files in
  let tar_filename, ch = bracket_tmpfile ~prefix:"tar-test" ~suffix:".tar" test_ctxt in
  close_out ch;
  let tar_filename = if Sys.win32 then convert_path `Unix tar_filename else tar_filename in
  let cmdline = Printf.sprintf "tar -cf %s %s %s" tar_filename format (String.concat " " files) in
  begin match Unix.system cmdline with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n -> failwith (Printf.sprintf "%s: exited with %d" cmdline n)
    | _ -> failwith (Printf.sprintf "%s: unknown error" cmdline)
  end;
  f tar_filename files

let can_read_tar test_ctxt =
  with_tar test_ctxt
    (fun tar_filename files ->
       let fd = Unix.openfile tar_filename [ Unix.O_RDONLY ] 0 in
       let files' = List.map (fun t -> t.Tar.Header.file_name) (Tar_unix.Archive.list fd) in
       Unix.close fd;
       let missing = set_difference files files' in
       let missing' = set_difference files' files in
       assert_equal ~printer:(String.concat "; ") [] missing;
       assert_equal ~printer:(String.concat "; ") [] missing'
    )

let can_write_pax test_ctxt =
  let open Tar_unix in
  let filename, ch = bracket_tmpfile ~prefix:"tar-test" ~suffix:".tar" test_ctxt in
  close_out ch;
  (* This userid is too large for a regular ustar header *)
  let user_id = 0x07777777 + 1 in
  (* Write a file which would need a pax header *)
  let fd = Unix.openfile filename [ Unix.O_CREAT; Unix.O_WRONLY ] 0o0644 in
  Fun.protect
    (fun () ->
      let hdr = Tar.Header.make ~user_id "test" 0L in
      write_block hdr (fun _ -> ()) fd;
      write_end fd;
    ) ~finally:(fun () -> Unix.close fd);
  (* Read it back and verify the header was read *)
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
  Fun.protect
    (fun () ->
      match Archive.list fd with
      | [ one ] -> assert (one.Tar.Header.user_id = user_id)
      | xs ->
        Printf.fprintf stderr "Headers = [ %s ]\n%!" (String.concat "; " (List.map Tar.Header.to_detailed_string xs));
        assert false
    ) ~finally:(fun () -> Unix.close fd)


let can_list_longlink_tar _test_ctxt =
  let open Tar_unix in
  let fd = Unix.openfile "lib_test/long.tar" [ Unix.O_RDONLY ] 0o0 in
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
      assert_equal ~printer:(String.concat ", ") expected filenames
    ) ~finally:(fun () -> Unix.close fd)

let starts_with ~prefix s =
  let len_s = String.length s
  and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.unsafe_get s i <> String.unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

let can_transform_tar test_ctxt =
  let level = Tar.Header.Ustar in
  with_tar ~level test_ctxt (fun tar_in _file_list ->
      let fd_in = Unix.openfile tar_in [O_RDONLY] 0 in
      let tar_out = Filename.temp_file "tar-transformed" ".tar" in
      let fd_out = Unix.openfile tar_out [O_WRONLY; O_CREAT] 0o644 in
      let temp_dir = bracket_tmpdir test_ctxt in
      Tar_unix.Archive.transform ~level (fun hdr ->
          {hdr with Tar.Header.file_name = Filename.concat temp_dir hdr.file_name})
        fd_in fd_out;
      Unix.close fd_in;
      Unix.close fd_out;
      let fd_in = Unix.openfile tar_out [O_RDONLY] 0 in
      Tar_unix.Archive.with_next_file fd_in (fun _fd_in hdr ->
          assert_bool "Filename was transformed" (starts_with ~prefix:temp_dir hdr.file_name)))

module Block4096 = struct
  include Block

  let get_info b =
    Block.get_info b
    >>= fun info ->
    let size_sectors = Int64.(div (add info.size_sectors 7L) 8L) in
    Lwt.return { info with Mirage_block.sector_size = 4096; size_sectors }

  let read b ofs bufs =
    Block.get_info b
    >>= fun info ->
    let len = List.fold_left (+) 0 (List.map Cstruct.length bufs) in
    let requested_end = Int64.(add (mul ofs 4096L) (of_int len)) in
    let end_of_file = Int64.(mul info.size_sectors (of_int info.sector_size)) in
    let need_to_trim = max 0L (Int64.sub requested_end end_of_file) |> Int64.to_int in
    let need_to_keep = len - need_to_trim in
    let rec trimmed len = function
      | [] -> []
      | b :: bs ->
        let b' = Cstruct.length b in
        for _ = 0 to b' do Cstruct.set_uint8 b 0 0 done;
        let to_drop = max 0 (len + b' - need_to_keep) in
        let to_keep = max 0 (b' - to_drop) in
        Cstruct.sub b 0 to_keep :: (trimmed (len + b') bs) in
    let trimmed =  (trimmed 0 bufs) in
    Block.read b (Int64.mul ofs 8L) trimmed

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect name
end

module type BLOCK = sig
  include Mirage_block.S
  val connect: string -> t Lwt.t
end

module B = struct
  include Block

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect name
end

module Test(B: BLOCK) = struct
  let add_data_to_tar ?(level:Tar.Header.compatibility option) ?files test_ctxt f =
    let f tar_filename files =
      (match Unix.system ("truncate -s +1K " ^ tar_filename) with
       | Unix.WEXITED 0 -> ()
       | Unix.WEXITED n -> failwith (Printf.sprintf "truncate exited with %d" n)
       | _ -> failwith "truncate: exited with error");
      B.connect tar_filename >>= fun b ->
      let module KV_RW = Tar_mirage.Make_KV_RW(B) in
      KV_RW.connect b >>= fun t ->
      KV_RW.set t (Mirage_kv.Key.v "barf") "foobar" >>= fun _ ->
      let files = "barf" :: files in
      f tar_filename files
    in
    with_tar ?level ?files test_ctxt f

  let write_with_full_archive ?(level:Tar.Header.compatibility option) ?files test_ctxt =
    let f tar_filename files =
      B.connect tar_filename >>= fun b ->
      let module KV_RW = Tar_mirage.Make_KV_RW(B) in
      KV_RW.connect b >>= fun t ->
      KV_RW.set t (Mirage_kv.Key.v "barf") "foobar" >>= function
      | Error `No_space -> Lwt.return ()
      | _ -> failwith "expected `No_space"
    in
    with_tar ?level ?files test_ctxt f

  let check_tar tar_filename files =
    B.connect tar_filename >>= fun b ->
    let module KV_RO = Tar_mirage.Make_KV_RO(B) in
    KV_RO.connect b >>= fun k ->
    Lwt_list.iter_s
      (fun file ->
         let size = 
           if file = "barf" then 6L else Unix.LargeFile.((stat file).st_size)
	 in
         let read_file key ofs len =
	   if key = "barf" then String.sub "foobar" ofs len else
           let fd = Unix.openfile key [ Unix.O_RDONLY ] 0 in
           Fun.protect
             (fun () ->
                let (_: int) = Unix.lseek fd ofs Unix.SEEK_SET in
                let buf = Bytes.make len '\000' in
                let len' = Unix.read fd buf 0 len in
                assert_equal ~printer:string_of_int len len';
                Bytes.to_string buf
             ) ~finally:(fun () -> Unix.close fd) in
         let read_tar key =
           KV_RO.get k key >>= function
           | Error e -> Fmt.failwith "KV_RO.read: %a" KV_RO.pp_error e
           | Ok buf -> Lwt.return buf in
         (* Read whole file *)
         let value = read_file file 0 (Int64.to_int size) in
         read_tar (Mirage_kv.Key.v file) >>= fun value' ->
         assert_equal ~printer:(fun x -> x) value value';
         if Int64.compare size 2L = 1 then begin
           let value = read_file file 1 ((Int64.to_int size) - 2) in
           read_tar (Mirage_kv.Key.v file) >>= fun value' ->
           let value'' = String.sub value' 1 ((Int64.to_int size) - 2) in
           assert_equal ~printer:(fun x -> x) value value'';
           Lwt.return_unit
         end else Lwt.return_unit
      ) files

  let can_read_through_BLOCK ?files test_ctxt =
    with_tar ?files test_ctxt check_tar

  let write_test test_ctxt =
     add_data_to_tar test_ctxt check_tar

  let check_not_padded test_ctxt =
    Unix.openfile "empty" [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 |> Unix.close;
    can_read_through_BLOCK ~files:["empty"] test_ctxt
end

module Sector512 = Test(B)
module Sector4096 = Test(Block4096)

let () =
  let suite = "tar" >:::
              [
                "header" >:: header;
                "can_read_tar" >:: can_read_tar;
                "can_read_through_BLOCK/512" >:: OUnitLwt.lwt_wrapper Sector512.can_read_through_BLOCK;
                "not 4KiB padded" >:: OUnitLwt.lwt_wrapper Sector512.check_not_padded;
                "can_read_through_BLOCK/4096" >:: OUnitLwt.lwt_wrapper Sector4096.can_read_through_BLOCK;
                "can write pax headers" >:: can_write_pax;
                "can read @Longlink" >:: can_list_longlink_tar;
                "can transform tars" >:: can_transform_tar;
                "add_data_to_tar BLOCK/512" >:: OUnitLwt.lwt_wrapper Sector512.write_test;
		"write_with_full_archive BLOCK/512" >:: OUnitLwt.lwt_wrapper Sector512.write_with_full_archive;
                (*"add_data_to_tar BLOCK/4096" >:: OUnitLwt.lwt_wrapper Sector4096.write_test;*)
              ] in
  (* pwd = _build/default/lib_test *)
  Unix.chdir "../../..";
  run_test_tt_main suite
