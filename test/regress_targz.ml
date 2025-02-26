let test_filter : Tar.Header.t -> bool =
  let search_for = Str.regexp ".*/openapi/spec3.json$" in
  fun { file_name; _ } ->
    match Str.search_forward search_for file_name 0 with
    | exception Not_found -> false
    | _ -> true

module TestExtract = struct
  let do_test filename =
    let open Lwt.Infix in
    Lwt_unix.mkdir "extracted" 0o750 >>= fun () ->
    Tar_lwt_unix.extract ~filter:test_filter ~src:filename "extracted/"
    >>= function
    | Ok v -> Lwt.return v
    | Error (`Gz msg) ->
        Format.kasprintf failwith "Gzip decompression failed. %s" msg
    | Error `Eof -> failwith "End of file reached."
    | Error (`Exn e) -> raise e
    | Error ((`Fatal _ | `Unix _ | `Unexpected_end_of_file | `Msg _) as e) ->
        Format.kasprintf failwith "Could not find entry. %a"
          Tar_lwt_unix.pp_decode_error e
end

module TestUntar = struct
  let max_int_64 = Int64.of_int Int.max_int

  let max_string_64 =
    (* https://ocaml.org/manual/4.14/values.html *)
    if Sys.int_size > 32 then
      let open Int64 in
      (* 2^57 - 9 = 144115188075855863L *)
      sub (shift_left (of_int 1) 57) 9L
    else (* 2^24 − 5 *)
      Int64.of_int 16777211

  let safe f a =
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> f a >|= fun r -> Ok r)
      (function
        | Unix.Unix_error (e, f, a) -> Lwt.return (Error (`Unix (e, f, a)))
        | e -> Lwt.reraise e)

  let safe_close fd =
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let fold ~gunzip f filename init =
    let open Lwt_result.Infix in
    let decompress = if gunzip then Tar_gz.in_gzipped else Fun.id in
    safe Lwt_unix.(openfile filename [ O_RDONLY ]) 0 >>= fun fd ->
    Lwt.finalize
      (fun () -> Tar_lwt_unix.run (decompress (Tar.fold f init)) fd)
      (fun () -> safe_close fd)

  let find_entry_aux :
      ?global:Tar.Header.Extended.t ->
      Tar.Header.t ->
      'a ->
      ( 'a,
        ([> `Eof
         | `Fatal of Tar.error
         | `Gz of string
         | `Msg of string
         | `Unexpected_end_of_file
         | `Unix of Unix.error * string * string ]
         as
         'b),
        Tar_lwt_unix.t )
      Tar.t =
   fun ?global:_ hdr acc ->
   let ( let* ) = Tar.( let* ) in
   let rec skip_until = function
     | n when n <= max_int_64 -> Tar.seek (Int64.to_int n)
     | n ->
       let* () = Tar.seek Int.max_int in
       skip_until (Int64.sub n max_int_64)
   in
    match acc with
    | Some found ->
      let* () = skip_until hdr.Tar.Header.file_size in
      Tar.return (Ok (Some found))
    | None ->
        if test_filter hdr then
          (* Found entry *)
          let* entry_len =
            match hdr.Tar.Header.file_size with
            | n when n <= max_string_64 -> Tar.return (Ok (Int64.to_int n))
            | _ ->
                Tar.return
                  (Error
                     (`Msg
                       (Format.asprintf
                          "The tar entry %s exceeded the architecture-specific \
                           maximum %d bytes that fit in an in-memory string."
                          hdr.file_name Int.max_int)))
          in
          let* entry_content = Tar.really_read entry_len in
          Tar.return (Ok (Some entry_content))
        else
          (* Did not find entry *)
          let* _ = skip_until hdr.Tar.Header.file_size in
          Tar.return (Ok None)

  let find_entry_opt ~gunzip filename =
    let open Lwt.Infix in
    fold ~gunzip find_entry_aux filename None >>= function
    | Ok v -> Lwt.return v
    | Error (`Gz msg) ->
        Format.kasprintf failwith "Gzip decompression failed. %s" msg
    | Error `Eof -> failwith "End of file reached."
    | Error ((`Fatal _ | `Unix _ | `Unexpected_end_of_file | `Msg _) as e) ->
        Format.kasprintf failwith "Could not find entry. %a"
          Tar_lwt_unix.pp_decode_error e

  let do_test ~gunzip filename =
    let open Lwt.Infix in
    find_entry_opt ~gunzip filename >>= function
    | Some entry ->
        Printf.printf "Found /openapi/spec3.json entry. Size = %d\n"
          (String.length entry);
        Lwt.return ()
    | None -> failwith "No /openapi/spec3.json found"
end

let () =
  let filename = Sys.argv.(1) in
  let test =
    match Sys.argv.(2) with
    | "untar" -> TestUntar.do_test ~gunzip:false filename
    | "untargz" -> TestUntar.do_test ~gunzip:true filename
    | "extract" -> TestExtract.do_test filename
    | _ -> failwith "Must be untar/untargz/extract"
  in
  Lwt_main.run test
