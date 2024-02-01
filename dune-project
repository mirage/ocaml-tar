(lang dune 2.9)
(name tar)
(cram enable)

(formatting disabled)
(generate_opam_files true)

(source (github mirage/ocaml-tar))
(license ISC)
(authors
 "Dave Scott"
 "Thomas Gazagnaire"
 "David Allsopp"
 "Antonin Décimo"
 "Reynir Björnsson"
 "Hannes Mehnert"
)
(maintainers
  "Reynir Björnsson <reynir@reynir.dk>"
  "dave@recoil.org")
(documentation "https://mirage.github.io/ocaml-tar/")

(package
 (name tar)
 (synopsis "Decode and encode tar format files in pure OCaml")
 (description
"\| tar is a library to read and write tar files with an emphasis on
"\| streaming.
"\|
"\| This is pure OCaml code, no C bindings.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 4.13.0))
  (decompress (>= 1.5.1))
 )
)

(package
 (name tar-unix)
 (synopsis "Decode and encode tar format files from Unix")
 (description
"\| tar is a library to read and write tar files with an emphasis on
"\| streaming.  This library provides a Unix or Windows compatible interface.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 4.13.0))
  lwt
  (tar (= :version))
 )
)

(package
 (name tar-mirage)
 (synopsis "Read and write tar format files via MirageOS interfaces")
 (description
"\| tar is a library to read and write tar files with an emphasis on
"\| streaming.  This library is functorised over external OS dependencies
"\| to facilitate embedding within MirageOS.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (conflicts (result (< 1.5)))
 (depends
  (ocaml (>= 4.13.0))
  (cstruct (>= 6.0.0))
  (lwt (>= 5.6.0))
  (mirage-block (>= 2.0.0))
  (mirage-clock (>= 4.0.0))
  (mirage-kv (>= 6.0.0))
  optint
  ptime
  (tar (= :version))
  (mirage-block-unix (and :with-test (>= 2.13.0)))
  (mirage-clock-unix (and :with-test (>= 4.0.0)))
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (tar-unix (and :with-test (= :version)))
 )
)

(package
 (name tar-eio)
 (synopsis "Decode and encode tar format files using Eio")
 (description
"\| tar is a library to read and write tar files with an emphasis on
"\| streaming.  This library uses Eio to provide a portable tar library.
 )
 (tags ("org:xapi-project" "org:mirage"))
 (depends
  (ocaml (>= 5.00.0))
  (eio (and (>= 0.10.0) (< 0.12)))
  (tar (= :version))
 )
)
