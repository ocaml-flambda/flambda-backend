type _ pattern_kind =
| Expr : Cmm.expression pattern_kind
| Int : int pattern_kind
| Natint : Nativeint.t pattern_kind

type 'a pattern_var

val create_var : 'a pattern_kind -> string -> 'a pattern_var

module Default_variables : sig
  val c : Cmm.expression pattern_var
  val c1 : Cmm.expression pattern_var
  val c2 : Cmm.expression pattern_var
  val n : int pattern_var
  val n1 : int pattern_var
  val n2 : int pattern_var
end

module Env : sig
  type t
end

type binop =
  | Add
  | Sub
  | Lsl
  | Or

type cmm_pattern =
  | Any of Cmm.expression pattern_var
  | As of Cmm.expression pattern_var * cmm_pattern
  | Const_int_fixed of int
  | Const_int of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint of Nativeint.t pattern_var
  | Binop of binop * cmm_pattern * cmm_pattern
  | When of cmm_pattern * (Env.t -> bool)

type 'a clause

val run : Cmm.expression -> Cmm.expression clause list -> Cmm.expression

module Syntax : sig
  val (=>) : cmm_pattern -> (Env.t -> 'a) -> 'a clause
  val (#.) : Env.t -> 'a pattern_var -> 'a
end
