type 'a pattern_var

module Env : sig
  type t
end

type cmm_pattern =
  | Any
  | Var of Cmm.expression pattern_var
  | Const_int_fixed of int
  | Const_int_var of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint_var of Nativeint.t pattern_var
  | Add of cmm_pattern * cmm_pattern
  | When of cmm_pattern * (Env.t -> bool)

type 'a clause =
  cmm_pattern * (Env.t -> 'a)

val run : Cmm.expression -> Cmm.expression clause list -> Cmm.expression

module Syntax : sig
  val (#.) : Env.t -> 'a pattern_var -> 'a
end

module Default_variables : sig
  val c : Cmm.expression pattern_var
  val c1 : Cmm.expression pattern_var
  val c2 : Cmm.expression pattern_var
  val i : int pattern_var
  val i1 : int pattern_var
  val i2 : int pattern_var
  val n : Nativeint.t pattern_var
  val n1 : Nativeint.t pattern_var
  val n2 : Nativeint.t pattern_var
end
