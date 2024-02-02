let level = Tar.Header.Ustar

module Writer = struct
  type out_channel = Stdlib.out_channel
  type 'a io = 'a
  let really_write oc str =
    output_string oc str
end

module HW = Tar.HeaderWriter
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (Writer)

module Reader = struct
  type in_channel = Stdlib.in_channel
  type 'a io = 'a
  let really_read ic buf =
    really_input ic buf 0 (Bytes.length buf)
  let skip ic len =
    let cur = pos_in ic in
    seek_in ic (cur + len)
  let read ic buf =
    let max = Bytes.length buf in
    input ic buf 0 max
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
       |> output_string cout

(* Tests that global and per-file extended headers correctly override
   each other. *)
let use_global_extended_headers _test_ctxt =
  (* Write an archive using global and per-file pax extended headers *)
  begin try Sys.remove "test.tar" with _ -> () end;
  let cout = open_out_bin "test.tar" in
  let g0 = make_extended 1000 in
  let hdr, f = make_file () in
  match HW.write_global_extended_header g0 cout with
  | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
  | Ok () ->
    match HW.write ~level hdr cout with
    | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
    | Ok () ->
      f cout;
      let hdr, f = make_file () in
      let hdr = { hdr with Tar.Header.extended = Some (make_extended 2000) } in
      match HW.write ~level hdr cout with
      | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
      | Ok () ->
        f cout;
        let hdr, f = make_file () in
        match HW.write ~level hdr cout with
        | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
        | Ok () ->
          f cout;
          let g1 = make_extended 3000 in
          let hdr, f = make_file () in
          match HW.write_global_extended_header g1 cout with
          | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
          | Ok () ->
            match HW.write ~level hdr cout with
            | Error `Msg msg -> Alcotest.failf "failed to write header %s" msg
            | Ok () ->
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
              ( match HR.read ~global:!global cin with
                | Ok (hdr, global') ->
                  Alcotest.check header "expected global header" (Some g0) global';
                  global := global';
                  Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
                  let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
                  Reader.skip cin to_skip;
                | Error `Eof -> failwith "Couldn't read header, end of file"
                | Error (`Fatal err) -> Fmt.failwith "Couldn't read header: %a" Tar.pp_error err );
              ( match HR.read ~global:!global cin with
                | Ok (hdr, global') ->
                  Alcotest.check header "expected global header" (Some g0) global';
                  global := global';
                  Alcotest.(check int) "expected user" 2000 hdr.Tar.Header.user_id;
                  let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
                  Reader.skip cin to_skip;
                | Error _ -> failwith "Couldn't read header" );
              ( match HR.read ~global:!global cin with
                | Ok (hdr, global') ->
                  Alcotest.check header "expected global header" (Some g0) global';
                  global := global';
                  Alcotest.(check int) "expected user" 1000 hdr.Tar.Header.user_id;
                  let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
                  Reader.skip cin to_skip;
                | Error _ -> failwith "Couldn't read header" );
              ( match HR.read ~global:!global cin with
                | Ok (hdr, global') ->
                  Alcotest.check header "expected global header" (Some g1) global';
                  global := global';
                  Alcotest.(check int) "expected user" 3000 hdr.Tar.Header.user_id;
                  let to_skip = Tar.Header.(Int64.to_int (to_sectors hdr) * length) in
                  Reader.skip cin to_skip;
                | Error _ -> failwith "Couldn't read header" );
              ( match HR.read ~global:!global cin with
                | Error `Eof -> ()
                | _ -> failwith "Should have found EOF");
              ()

let () =
  let suite = "tar - pax global extended headers", [
      Alcotest.test_case "can use pax global extended headers" `Quick use_global_extended_headers;
    ]
  in
  Alcotest.run "global extended headers" [suite]
