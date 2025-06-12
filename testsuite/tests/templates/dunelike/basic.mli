(* Parameters: P *)

(* A library with a single parameter, no submodules, and no dependencies on
   other libraries. *)

type t

val create : P.t -> t
val to_string : t -> string
