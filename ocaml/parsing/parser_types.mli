(* Tracks types that the parser uses that get exposed when compiling via Menhir. This
   prevents the compiled parser.mli from having undeclared types, since it doesn't
   copy the implementation section from parser.mly, causing types declared in
   parser.mly and then exposed by Menhir in function signatures in parser.mli
   will be unbound. *)

open Asttypes
open Parsetree
open Docstrings

module Constant : sig
  type t = private
    | Value of constant
    | Unboxed of Jane_syntax.Layouts.constant

  type loc := Lexing.position * Lexing.position

  val value : Parsetree.constant -> t
  val unboxed : Jane_syntax.Layouts.constant -> t
  val to_expression : loc:loc -> t -> expression
  val to_pattern : loc:loc -> t -> pattern
end

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_constraint: value_constraint option;
    lb_is_pun: bool;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }

module N_ary = Jane_syntax.N_ary_functions
module Mode = Jane_syntax.Mode_expr
