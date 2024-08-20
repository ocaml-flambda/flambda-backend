open Asttypes
open Parsetree
open Ast_helper
open Docstrings

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}
let mkexp ~loc ?attrs d = Exp.mk ~loc:(make_loc loc) ?attrs d
let mkpat ~loc ?attrs d = Pat.mk ~loc:(make_loc loc) ?attrs d


module Constant : sig
  type t = private
    | Value of constant
    | Unboxed of Jane_syntax.Layouts.constant

  type loc := Lexing.position * Lexing.position

  val value : Parsetree.constant -> t
  val unboxed : Jane_syntax.Layouts.constant -> t
  val to_expression : loc:loc -> t -> expression
  val to_pattern : loc:loc -> t -> pattern
end = struct
  type t =
    | Value of constant
    | Unboxed of Jane_syntax.Layouts.constant

  let value x = Value x

  let unboxed x = Unboxed x

  let to_expression ~loc : t -> expression = function
    | Value const_value ->
        mkexp ~loc (Pexp_constant const_value)
    | Unboxed const_unboxed ->
      Jane_syntax.Layouts.expr_of ~loc:(make_loc loc)
        (Lexp_constant const_unboxed)

  let to_pattern ~loc : t -> pattern = function
    | Value const_value ->
        mkpat ~loc (Ppat_constant const_value)
    | Unboxed const_unboxed ->
      Jane_syntax.Layouts.pat_of
        ~loc:(make_loc loc) (Lpat_constant const_unboxed)
end


type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_constraint: value_constraint option;
    lb_is_pun: bool;
    lb_modes: modes;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }
