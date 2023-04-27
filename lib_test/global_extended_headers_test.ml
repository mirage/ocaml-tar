let level = Tar.Header.Ustar

module Writer = struct
  type out_channel = Stdlib.out_channel
  type 'a t = 'a
  let really_write oc cs =
    let str = Cstruct.to_string cs in
    output_string oc str
end

module HW = Tar.HeaderWriter
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (Writer)

module Reader = struct
  type in_channel = Stdlib.in_channel
  type 'a t = 'a
  let really_read ic cs =
    let len = Cstruct.length cs in
    let buf = Bytes.create len in
    really_input ic buf 0 len ;
    Cstruct.blit_from_bytes buf 0 cs 0 len
  let skip ic len = really_read ic (Cstruct.create len)
  let read ic cs =
    let max = Cstruct.length cs in
    let buf = Bytes.create max in
    let len = input ic buf 0 max in
    Cstruct.blit_from_bytes buf 0 cs 0 len ; len
end

module HR = Tar.HeaderReader
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (Reader)

let make_extended user_id =
  Tar.Header.Extended.make ~user_id ()

let make_file =
  let gen = ref 0 in
  fun () ->
  let name = "file" ^ string_of_int !gen in
  incr gen;
  let hdr = Tar.Header.make name 0L in
  hdr, fun cout ->
       Tar.Header.zero_padding hdr
       |> Cstruct.to_string
       |> output_string cout

(* Tests that global and per-file extended headers correctly override
   each other. *)
let use_global_extended_headers _test_ctxt =
  (* Write an archive using global and per-file pax extended headers *)
  begin try Sys.remove "test.tar" with _ -> () end;
  let cout = open_out_bin "test.tar" in
  let g0 = make_extended 1000 in
  let hdr, f = make_file () in
  HW.write ~level ~global:g0 hdr cout;
  f cout;
  let hdr, f = make_file () in
  let hdr = { hdr with Tar.Header.extended = Some (make_extended 2000) } in
  HW.write ~level hdr cout;
  f cout;
  let hdr, f = make_file () in
  HW.write ~level hdr cout;
  f cout;
  let g1 = make_extended 3000 in
  let hdr, f = make_file () in
  HW.write ~level ~global:g1 hdr cout;
  f cout;
  Writer.really_write cout Tar.Header.zero_block;
  Writer.really_write cout Tar.Header.zero_block;
  close_out cout;
  (* Read the same archive, testing that headers have been squashed. *)
  let cin = open_in_bin "test.tar" in
  let global = ref None in
  let header =
    let pp ppf hdr = Fmt.pf ppf "%s" (Tar.Header.Extended.to_detailed_string hdr) in
    Alcotest.testable (fun ppf hdr -> Fmt.pf ppf "%a" Fmt.(option pp) hdr) ( = )
  in
  ( match HR.read ~level ~global:!global cin with
    | Ok (hdr, global') ->
       Alcotest.check header "expected global header" (Some g0) global';
       global := global';
       Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
       let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
       Reader.skip cin to_skip;
    | Error `Eof -> failwith "Couldn't read header" );
  ( match HR.read ~level ~global:!global cin with
    | Ok (hdr, global') ->
       Alcotest.check header "expected global header" (Some g0) global';
       global := global';
       Alcotest.(check int) "expected user" 2000 hdr.Tar.Header.user_id;
       let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
       Reader.skip cin to_skip;
    | Error `Eof -> failwith "Couldn't read header" );
  ( match HR.read ~level ~global:!global cin with
    | Ok (hdr, global') ->
       Alcotest.check header "expected global header" (Some g0) global';
       global := global';
       Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
       let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
       Reader.skip cin to_skip;
    | Error `Eof -> failwith "Couldn't read header" );
  ( match HR.read ~level ~global:!global cin with
    | Ok (hdr, global') ->
       Alcotest.check header "expected global header" (Some g1) global';
       global := global';
       Alcotest.(check int) "expected user" 3000 hdr.Tar.Header.user_id;
       let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
       Reader.skip cin to_skip;
    | Error `Eof -> failwith "Couldn't read header" );
  ( match HR.read ~level ~global:!global cin with
    | Ok _ -> failwith "Should have found EOF"
    | Error `Eof -> () );
  ()

let () =
  let suite = "tar - pax global extended headers", [
      Alcotest.test_case "can use pax global extended headers" `Quick use_global_extended_headers;
    ]
  in
  Alcotest.run "global extended headers" [suite]
