open OUnit2
open Lwt.Infix

module B(Size : sig val sector_size : int end) = struct
  include Block
  include Size

  let convert_path os path =
    let ch = Unix.open_process_in (Printf.sprintf "cygpath -%c -- %s" (match os with `Mixed -> 'm' | `Unix -> 'u' | `Windows -> 'w') path) in
    let line = input_line ch in
    close_in ch;
    line

  let connect name =
    let name = if Sys.win32 then convert_path `Windows name else name in
    connect ~prefered_sector_size:(Some sector_size) name
end

module Block512 = B(struct let sector_size = 512 end)

module Block4096 = B(struct let sector_size = 4096 end)

module type BLOCK = sig
  include module type of Block
  val connect: string -> t Lwt.t
  val sector_size : int
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

  let write_sector_size_file test_ctxt =
    connect_block test_ctxt >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str B.sector_size) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_sector_size test_ctxt =
    connect_block test_ctxt >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_sector_size ctx =
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "barf") (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_files ctx =
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t (Mirage_kv.Key.v "first") (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t (Mirage_kv.Key.v "second") (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    Lwt.return_unit

  let write_two_files_remove_first ctx =
    let first = Mirage_kv.Key.v "first" and second = Mirage_kv.Key.v "second" in
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t second (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t first >>= function
    | Error _ (* XXX: `Append_only *) ->
      Lwt.return_unit
    | Ok () -> OUnit2.assert_failure "Expected Error `Append_only"

  let write_two_files_remove_second ctx =
    let first = Mirage_kv.Key.v "first" and second = Mirage_kv.Key.v "second" in
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str (B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.set t second (str (2 * B.sector_size - 512)) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t second >>=
    kv_rw_write_error

  let remove_odd_file ctx =
    let first = Mirage_kv.Key.v "first" in
    connect_block ctx >>= fun b ->
    resize b 10240L >>= fun () ->
    KV_RW.connect b >>= fun t ->
    KV_RW.set t first (str 1) >>=
    kv_rw_write_error >>= fun () ->
    KV_RW.remove t first >>=
    kv_rw_write_error

  let allocate_doesn't_affect_beyond_end_of_archive data_size ctx =
    let first = Mirage_kv.Key.v "first" in
    connect_block ctx >>= fun b ->
    let align_sector n = (n + pred B.sector_size) / B.sector_size in
    let align_block n = (n + 511) / 512 in
    let size = B.sector_size * align_sector (512 + 512 * (align_block data_size) + 1024 + 512) in
    resize b (Int64.of_int size) >>= fun () ->
    (* write an empty archive with trailing \xFFs *)
    let config = B.to_config b in
    let oc = open_out config.path in
    output_string oc (String.init 1024 (Fun.const '\000'));
    output_string oc (String.init (size - 1024) (Fun.const '\xFF'));
    close_out oc;
    KV_RW.connect b >>= fun t ->
    KV_RW.allocate t first (Optint.Int63.of_int data_size) >>=
    kv_rw_write_error >|= fun () ->
    let ic = open_in config.path in
    In_channel.seek ic 512L; (* skip header *)
    (* check file content *)
    for _ = 1 to data_size do
      assert_equal ~cmp:Char.equal ~printer:Char.escaped
        '\x00' (input_char ic)
        ~msg:"corrupt data";
    done;
    for _ = 1 to 512 * align_block data_size - data_size do
      assert_equal ~cmp:Char.equal ~printer:Char.escaped
        '\x00' (input_char ic)
        ~msg:"corrupt padding";
    done;
    (* check sentinel *)
    for _ = 1 to 1024 do
      assert_equal ~cmp:Char.equal ~printer:Char.escaped
        '\x00' (input_char ic)
        ~msg:"corrupt sentinel"
    done;
    (* check tail is untouched *)
    for _ = 1 to size - 512 - 512 * align_block data_size - 1024 do
      assert_equal ~cmp:Char.equal ~printer:Char.escaped
        '\xff' (input_char ic)
        ~msg:"corrupt tail"
    done;
    assert (In_channel.pos ic = Int64.of_int size)



  let tests =
    let ( >:: ) desc f =
      Printf.sprintf "%s b%d" desc B.sector_size >:: OUnitLwt.lwt_wrapper f
    in
    [
      "write empty" >:: write_empty_file;
      "write block size" >:: write_sector_size_file;
      "write block size" >:: write_sector_size;
      "write two blocks" >:: write_two_sector_size;
      "write two files" >:: write_two_files;
      "write two files remove first" >:: write_two_files_remove_first;
      "write two files remove second" >:: write_two_files_remove_second;
      "remove odd sized file" >:: remove_odd_file;
      "allocate doesn't affect tail after archive 0" >:: allocate_doesn't_affect_beyond_end_of_archive 0;
      "allocate doesn't affect tail after archive 1" >:: allocate_doesn't_affect_beyond_end_of_archive 1;
      "allocate doesn't affect tail after archive 1 sector" >:: allocate_doesn't_affect_beyond_end_of_archive (B.sector_size - 512);
    ]
end

module Test512 = Test(Block512)
module Test4096 = Test(Block4096)

let () =
  let suite =
    "tar-write" >:::
    (Test512.tests @ Test4096.tests)
  in
  run_test_tt_main suite
