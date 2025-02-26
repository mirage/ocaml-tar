TEST SUITE
----------

Test downloading https://github.com/stripe/openapi/archive/refs/tags/v1044.tar.gz
and extracting openapi/spec3.json in one of three ways:
A. "Extract"-ing it in OCaml
B. Decompressing in Unix, and then untarring it in OCaml
C. Decompressing and untarring it in OCaml

Download v1044.tar.gz
  $ curl -s -L -o v1044.tar.gz https://github.com/stripe/openapi/archive/refs/tags/v1044.tar.gz
  $ wc -c v1044.tar.gz
  4307906 v1044.tar.gz

Uncompress it
_     (fun () -> Tar_lwt_unix.run (decompress (Tar.fold f init)) fd)
_  in regress_targz.ml:fold works when `decompress = Fun.id`
  $ gunzip -c v1044.tar.gz > v1044.tar
  $ wc -c v1044.tar
  42301440 v1044.tar

A. "Extract"-ing it in OCaml.
That is, TestExtract.do_test
  $ OCAMLRUNPARAM=b ./regress_targz.exe v1044.tar extract
  Fatal error: exception Invalid_argument("Lwt_unix.read")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Tar_lwt_unix.safe.(fun) in file "unix/tar_lwt_unix.ml", line 38, characters 15-18
  Called from Lwt.Sequential_composition.catch in file "src/core/lwt.ml", line 2016, characters 10-14
  Re-raised at Tar_lwt_unix.safe.(fun) in file "unix/tar_lwt_unix.ml", line 41, characters 13-26
  Called from Tar_lwt_unix.read_complete.loop in file "unix/tar_lwt_unix.ml", line 47, characters 6-55
  Called from Tar_lwt_unix.run.run in file "unix/tar_lwt_unix.ml", line 99, characters 6-44
  Called from Tar_lwt_unix.run.run in file "unix/tar_lwt_unix.ml", line 105, characters 6-11
  Called from Lwt.Sequential_composition.bind.create_result_promise_and_callback_if_deferred.callback in file "src/core/lwt.ml", line 1844, characters 16-19
  Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3123, characters 20-29
  Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 27, characters 10-20
  Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 106, characters 8-13
  Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 112, characters 4-13
  Called from Dune__exe__Regress_targz in file "test/regress_targz.ml", line 131, characters 2-19
  [2]

B. Decompressing in Unix, and then untarring it in OCaml.
That is, TestUntar.do_test ~gunzip:false where ...
_     (fun () -> Tar_lwt_unix.run (decompress (Tar.fold f init)) fd)
in regress_targz.ml:fold works when `decompress = Fun.id`
  $ OCAMLRUNPARAM=b ./regress_targz.exe v1044.tar untar
  Fatal error: exception Invalid_argument("Lwt_unix.read")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Tar_lwt_unix.safe.(fun) in file "unix/tar_lwt_unix.ml", line 38, characters 15-18
  Called from Lwt.Sequential_composition.catch in file "src/core/lwt.ml", line 2016, characters 10-14
  Re-raised at Tar_lwt_unix.safe.(fun) in file "unix/tar_lwt_unix.ml", line 41, characters 13-26
  Called from Tar_lwt_unix.read_complete.loop in file "unix/tar_lwt_unix.ml", line 47, characters 6-55
  Called from Tar_lwt_unix.run.run in file "unix/tar_lwt_unix.ml", line 99, characters 6-44
  Called from Tar_lwt_unix.run.run in file "unix/tar_lwt_unix.ml", line 105, characters 6-11
  Called from Lwt.Sequential_composition.bind.create_result_promise_and_callback_if_deferred.callback in file "src/core/lwt.ml", line 1844, characters 16-19
  Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3123, characters 20-29
  Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 27, characters 10-20
  Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 106, characters 8-13
  Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 112, characters 4-13
  Called from Dune__exe__Regress_targz in file "test/regress_targz.ml", line 131, characters 2-19
  [2]

C. Decompressing and untarring it in OCaml.
That is, TestUntar.do_test ~gunzip:true where ...
_     (fun () -> Tar_lwt_unix.run (decompress (Tar.fold f init)) fd)
in regress_targz.ml:fold works when `decompress = Tar_gz.in_gzipped`
  $ OCAMLRUNPARAM=b ./regress_targz.exe v1044.tar.gz untargz
  Fatal error: exception Failure("Could not find entry. unmarshal Int64.of_string: failed to parse int64 \"0o0erated c\"")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lwt.Sequential_composition.bind.create_result_promise_and_callback_if_deferred.callback in file "src/core/lwt.ml", line 1844, characters 16-19
  Re-raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3123, characters 20-29
  Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 27, characters 10-20
  Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 106, characters 8-13
  Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 112, characters 4-13
  Called from Dune__exe__Regress_targz in file "test/regress_targz.ml", line 131, characters 2-19
  [2]
