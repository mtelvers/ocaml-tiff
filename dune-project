(lang dune 3.0)
(name tiff)

(generate_opam_files true)

(source (github mtelvers/ocaml-tiff))
(license MIT)
(authors "Mark Elvers <mark.elvers@tunbury.org>")
(maintainers "Mark Elvers <mark.elvers@tunbury.org>")

(package
 (name tiff)
 (synopsis "Pure OCaml TIFF/GeoTIFF library")
 (description "A native OCaml library for reading and writing TIFF and GeoTIFF files")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (alcotest :with-test)))
