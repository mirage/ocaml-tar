(lang dune 2.9)
(name tar)

(generate_opam_files true)

(source (github mirage/ocaml-tar))
(license ISC)
(authors
 "Dave Scott"
 "Thomas Gazagnaire"
 "David Allsopp"
)
(maintainers "dave@recoil.org")
(documentation "https://mirage.github.io/ocaml-tar/")

(package
 (name tar)
 (synopsis "Decode and encode tar format files in pure OCaml")
 (description
"\| tar is a simple library to read and write tar files with an emphasis on
"\| streaming.
"\|
"\| This is pure OCaml code, no C bindings.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 4.03.0))
  (ppx_cstruct (and :build (>= 3.6.0)))
  (cstruct (>= 6.0.0))
  re
 )
)

(package
 (name tar-unix)
 (synopsis "Decode and encode tar format files from Unix")
 (description
"\| tar is a simple library to read and write tar files with an emphasis on
"\| streaming.  This library provides a Unix or Windows compatible interface.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 4.03.0))
  (cstruct (>= 6.0.0))
  cstruct-lwt
  lwt
  re
  (tar (= :version))
 )
)

(package
 (name tar-mirage)
 (synopsis "Read and write tar format files via MirageOS interfaces")
 (description
"\| tar is a simple library to read and write tar files with an emphasis on
"\| streaming.  This library is functorised over external OS dependencies
"\| to facilitate embedding within MirageOS.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 4.05.0))
  (cstruct (>= 1.9.0))
  io-page
  lwt
  mirage-block
  (mirage-kv (>= "3.0.0"))
  ptime
  (re (>= 1.7.2))
  (tar (= :version))
  (io-page-unix :with-test)
  (mirage-block-unix (and :with-test (>= 2.5.0)))
  (ounit2 :with-test)
  (tar-unix (and :with-test (= :version)))
 )
)
