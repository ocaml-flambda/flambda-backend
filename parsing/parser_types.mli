(* These are types that are used by the parser. They need to be placed in a separate
   module (ie., not parser.mly) because adding the --inspection flag, the parser's
   interface refers to the result type of each parsing rule, so the types need to be
   available outside of just parser.ml. *)

open Asttypes
open Parsetree
open Docstrings

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
    lbs_mutable: mutable_flag;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }
