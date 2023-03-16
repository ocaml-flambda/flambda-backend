(** Alpha-equivalence checker **)

type eq = bool

module Env : sig
  type t
  val create : unit -> t
end

val eq_string : eq -> string

val equiv_primitives :
  Env.t -> Flambda2_core.primitive -> Flambda2_core.primitive -> eq
val core_eq : Flambda2_core.core_exp -> Flambda2_core.core_exp -> eq
