open OUnit2
open Lwt.Infix

module B(Size : sig val block_size : int end) = struct
  include Block
  include Size

  let convert_path os path =
    let ch = Unix.open_process_in (Printf.sprintf "cygpath -%c -- %s" (match os with `Mixed -> 'm' | `Unix -> 'u' | `Windows -> 'w') path) in
    let line = input_line ch in
    close_in ch;
    line

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect ~prefered_sector_size:(Some block_size) name
end

module Block512 = B(struct let block_size = 512 end)

module Block4096 = B(struct let block_size = 4096 end)

module type BLOCK = sig
  include module type of Block
  val connect: string -> t Lwt.t
  val block_size : int
end

module Test(B : BLOCK) = struct
  module KV_RW = Tar_mirage.Make_KV_RW(Pclock)(B)

  let kv_rw_write_error =
    Lwt.wrap1
      (Result.iter_error (Fmt.kstr failwith "%a" KV_RW.pp_write_error))

  let str n =
    Bytes.unsafe_to_string (Bytes.create n)

  let connect_block test_ctxt =
    let filename, ch = bracket_tmpfile ~prefix:"tar-write-test" ~suffix:".tar" test_ctxt in
    close_out ch;
    B.connect filename

  let resize b size =
    B.resize b size >|= fun x ->
    Result.iter_error (fun e ->
        Fmt.kstr failwith "%a" B.pp_write_error e)
      x

  let write_empty_file test_ctxt =
    connect_block test_ctxt >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") "" >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_block_size_file test_ctxt =
    connect_block test_ctxt >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str B.block_size) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_block_size test_ctxt =
    connect_block test_ctxt >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (B.block_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_block_size ctx =
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (2 * B.block_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_files ctx =
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "first") (str (B.block_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t (Mirage_kv.Key.v "second") (str (2 * B.block_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit
end

module Test512 = Test(Block512)
module Test4096 = Test(Block4096)

let () =
  let suite =
    "tar-write" >:::
    [
      "write empty b512" >:: OUnitLwt.lwt_wrapper Test512.write_empty_file;
      "write empty b4096" >:: OUnitLwt.lwt_wrapper Test4096.write_empty_file;
      "write block size 512" >:: OUnitLwt.lwt_wrapper Test512.write_block_size_file;
      "write block size 4096" >:: OUnitLwt.lwt_wrapper Test4096.write_block_size_file;
      "write block size 512" >:: OUnitLwt.lwt_wrapper Test512.write_block_size;
      "write block size 4096" >:: OUnitLwt.lwt_wrapper Test4096.write_block_size;
      "write two blocks 512" >:: OUnitLwt.lwt_wrapper Test512.write_two_block_size;
      "write two blocks 4096" >:: OUnitLwt.lwt_wrapper Test4096.write_two_block_size;
      "write two files 512" >:: OUnitLwt.lwt_wrapper Test512.write_two_files;
      "write two files 4096" >:: OUnitLwt.lwt_wrapper Test4096.write_two_files;
    ]
  in
  run_test_tt_main suite
