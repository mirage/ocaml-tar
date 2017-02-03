#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let mirage = Conf.with_pkg ~default:false "mirage"
let lwt = Conf.with_pkg ~default:false "lwt"

let () =
  let opams =
    [ Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["ppx_tools" ; "ounit" ; "oUnit"]) ]
  in
  Pkg.describe ~opams "tar-format" @@ fun c ->
  let mirage = Conf.value c mirage
  and lwt = Conf.value c lwt
  in
  Ok [
    Pkg.mllib "lib/tar.mllib";
    Pkg.mllib "lib/tar_unix.mllib";
    Pkg.mllib ~cond:lwt "lib/tar_lwt_unix.mllib";
    Pkg.mllib ~cond:mirage "lib/tar_mirage.mllib";
    Pkg.test "lib_test/parse_test"
  ]
