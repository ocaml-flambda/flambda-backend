type t =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of t * int
