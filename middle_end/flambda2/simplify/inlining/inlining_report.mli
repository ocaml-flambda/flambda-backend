(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020--2020 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Report inlining decisions *)

type at_call_site =
  | Known_function of
      { code_id : Code_id.exported;  (** code id of the callee *)
        decision : Call_site_inlining_decision.t
      }  (** Function call where the function's type is known *)
  | Unknown_function  (** Function call where the function's type is unknown. *)

(** There are three decisions made for each function declaration: on after
    conversion in CPS and closure, one before simplifying the body, and one
    after (this is useful for e.g. recursive functions). *)
type fundecl_pass =
  | After_closure_conversion
  | Before_simplify of { dbg_including_inlining_stack : Debuginfo.t }
  | After_simplify
(**)

type at_function_declaration =
  { pass : fundecl_pass;
    code_id : Code_id.exported;  (** code id of the function being declared *)
    decision : Function_decl_inlining_decision_type.t
  }

(** This defines the various kinds of decisions related to inlining that will be
    reported, together with some additional information to better identify to
    what the decision refers to. *)

type decision =
  | At_call_site of at_call_site
  | At_function_declaration of at_function_declaration
(**)

(** Record a decision. *)
val record_decision : dbg:Debuginfo.t -> decision -> unit

(** Output the report for all recorded decisions up to that point, and
    clean/forget all decisions.

    Note that this function should be called once for each round of
    simplification. *)
val output_then_forget_decisions : output_prefix:string -> unit
