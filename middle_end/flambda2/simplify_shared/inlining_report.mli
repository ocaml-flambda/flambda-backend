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

module Pass : sig
  type t =
    | After_closure_conversion
    | Before_simplify
    | After_simplify

  val print : Format.formatter -> t -> unit
end

module Context : sig
  (* Represents the context under which an inlining decision was taken. *)
  type t =
    { args : Inlining_arguments.t;
      cost_metrics : Cost_metrics.t option;
      depth : int option;
      unrolling_depth : int option option;
      are_rebuilding_terms : Are_rebuilding_terms.t;
      pass : Pass.t
    }

  val print : Format.formatter -> t -> unit
end

module Decision_with_context : sig
  type decision =
    | Call of Call_site_inlining_decision_type.t
    | Fundecl of Function_decl_inlining_decision_type.t

  type t =
    { context : Context.t;
      decision : decision
    }

  val print_decision : Format.formatter -> decision -> unit

  val print : include_header:bool -> Format.formatter -> t -> unit
end

module Uid : sig
  type t =
    { compilation_unit : Compilation_unit.t;
      t : string
    }
end

module Inlining_tree : sig
  module Key : sig
    type scope =
      | Module
      | Class
      | Unknown

    val compare_scope : scope -> scope -> int

    type element =
      | Fundecl of string
      | Scope of scope * string
      | Call of Inlining_history.Absolute.t

    val compare_element : element -> element -> int

    type t = Debuginfo.t * element

    val compare : t -> t -> int
  end

  module Map : Map.S with type key = Key.t

  type decision_or_reference =
    | Decision of Decision_with_context.t
    | Reference of Inlining_history.Absolute.t
    | Unavailable

  type item =
    | Call of
        { decision : decision_or_reference;
          tree : t
        }
    | Fundecl of
        { decisions : decisions;
          body : t
        }
    | Scope of t

  and t = item Map.t

  and decisions = Decision_with_context.t list
end

val record_decision_at_call_site_for_known_function :
  tracker:Inlining_history.Tracker.t ->
  unrolling_depth:int option ->
  apply:Apply_expr.t ->
  pass:Pass.t ->
  callee:Inlining_history.Absolute.t ->
  are_rebuilding_terms:Are_rebuilding_terms.t ->
  Call_site_inlining_decision_type.t ->
  unit

val record_decision_at_call_site_for_unknown_function :
  tracker:Inlining_history.Tracker.t ->
  apply:Apply_expr.t ->
  pass:Pass.t ->
  unit ->
  unit

val record_decision_at_function_definition :
  absolute_history:Inlining_history.Absolute.t ->
  code_metadata:Code_metadata.t ->
  pass:Pass.t ->
  are_rebuilding_terms:Are_rebuilding_terms.t ->
  Function_decl_inlining_decision_type.t ->
  unit

(** Output the report for all recorded decisions up to that point, and
    clean/forget all decisions.

    Note that this function should be called once for each round of
    simplification. *)
val output_then_forget_decisions : output_prefix:string -> Inlining_tree.t
