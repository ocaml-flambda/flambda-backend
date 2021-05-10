(lang dune 2.3)
(name memtrace)
(version 0.2.1.2)

(generate_opam_files true)

(source (github janestreet/memtrace))
(license MIT)
(authors "Jane Street Group, LLC <opensource@janestreet.com>")
(maintainers "opensource@janestreet.com")

(package
 (name memtrace)
 (synopsis "Streaming client for Memprof")
 (description "Generates compact traces of a program's memory use.")
 (depends
  (ocaml (>= 4.11.0))))
