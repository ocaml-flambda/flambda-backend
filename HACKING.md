# Hacking on Flambda backend project for OCaml

This page is intended to keep track of useful information for people who
want to modify Flambda backend.

## Bootstrapping

- Bootstrapping: use the same process as the upstream compiler. In
  `ocaml` subdirectory, follow the instructions in
  [ocaml/BOOTSTRAP.adoc](ocaml/BOOSTRAP.adoc).

- Changes in `ocaml/asmcomp` and `ocaml/middle_end` directories should
  also be applied to the corresponding directories `backend` and
  `middle_end`.

