[@@@ocaml.warning "+a-9-40-41-42"]

type t =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of t * int
