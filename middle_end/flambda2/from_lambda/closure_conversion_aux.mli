(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Environments and auxiliary structures used during closure conversion. *)

module IR : sig
  type simple =
    | Var of Ident.t
    | Const of Lambda.structured_constant

  type exn_continuation =
    { exn_handler : Continuation.t;
      extra_args : (simple * Lambda.value_kind) list
    }

  type trap_action =
    | Push of { exn_handler : Continuation.t }
    | Pop of { exn_handler : Continuation.t }

  type user_visible =
    | User_visible
    | Not_user_visible

  type named =
    | Simple of simple
    | Get_tag of Ident.t (* Intermediary primitive for block switch *)
    | Prim of
        { prim : Lambda.primitive;
          args : simple list;
          loc : Lambda.scoped_location;
          exn_continuation : exn_continuation option
        }

  type apply_kind =
    | Function
    | Method of
        { kind : Lambda.meth_kind;
          obj : simple
        }

  type apply =
    { kind : apply_kind;
      func : Ident.t;
      args : simple list;
      continuation : Continuation.t;
      exn_continuation : exn_continuation;
      loc : Lambda.scoped_location;
      tailcall : Lambda.tailcall_attribute;
      inlined : Lambda.inlined_attribute;
      specialised : Lambda.specialise_attribute;
      probe : Lambda.probe
    }

  type switch =
    { numconsts : int;
      consts : (int * Continuation.t * trap_action option * simple list) list;
      failaction : (Continuation.t * trap_action option * simple list) option
    }

  val print_named : Format.formatter -> named -> unit
end

(** Used to remember which [Variable.t] values correspond to which [Ident.t]
    values during closure conversion, and similarly for static exception
    identifiers. *)
module Env : sig
  type value_approximation =
    | Value_unknown
    | Closure_approximation of Code_id.t * Code.t option
    | Block_approximation of value_approximation array

  type t

  val create : symbol_for_global:(Ident.t -> Symbol.t) -> big_endian:bool -> t

  val clear_local_bindings : t -> t

  val add_var : t -> Ident.t -> Variable.t -> t

  val add_vars : t -> Ident.t list -> Variable.t list -> t

  val add_var_map : t -> Variable.t Ident.Map.t -> t

  val add_var_like : t -> Ident.t -> IR.user_visible -> t * Variable.t

  val add_vars_like :
    t -> (Ident.t * IR.user_visible) list -> t * Variable.t list

  val find_name : t -> Ident.t -> Name.t

  val find_name_exn : t -> Ident.t -> Name.t

  val find_var : t -> Ident.t -> Variable.t

  val find_var_exn : t -> Ident.t -> Variable.t

  val find_vars : t -> Ident.t list -> Variable.t list

  val add_global : t -> int -> Symbol.t -> t

  val find_global : t -> int -> Symbol.t

  val add_simple_to_substitute : t -> Ident.t -> Simple.t -> t

  val add_simple_to_substitute_map : t -> Simple.t Ident.Map.t -> t

  val find_simple_to_substitute_exn : t -> Ident.t -> Simple.t

  val add_value_approximation : t -> Name.t -> value_approximation -> t

  val add_closure_approximation : t -> Name.t -> Code_id.t * Code.t option -> t

  val add_block_approximation : t -> Name.t -> value_approximation array -> t

  val add_approximation_alias : t -> Name.t -> Name.t -> t

  val find_value_approximation : t -> Simple.t -> value_approximation

  val current_depth : t -> Variable.t option

  val with_depth : t -> Variable.t -> t

  val current_unit_id : t -> Ident.t

  val symbol_for_global : t -> Ident.t -> Symbol.t

  val big_endian : t -> bool
end

(** Used to pipe some data through closure conversion *)
module Acc : sig
  type t

  val create : symbol_for_global:(Ident.t -> Symbol.t) -> t

  val declared_symbols : t -> (Symbol.t * Static_const.t) list

  val shareable_constants : t -> Symbol.t Static_const.Map.t

  val code : t -> Code.t Code_id.Map.t

  val free_names : t -> Name_occurrences.t

  val seen_a_function : t -> bool

  val with_seen_a_function : t -> bool -> t

  val add_declared_symbol : symbol:Symbol.t -> constant:Static_const.t -> t -> t

  val add_shareable_constant :
    symbol:Symbol.t -> constant:Static_const.t -> t -> t

  val add_code : code_id:Code_id.t -> code:Code.t -> t -> t

  val add_free_names : Name_occurrences.t -> t -> t

  val remove_var_from_free_names : Variable.t -> t -> t

  val remove_continuation_from_free_names : Continuation.t -> t -> t

  val continuation_known_arguments :
    cont:Continuation.t -> t -> Env.value_approximation list option

  val with_free_names : Name_occurrences.t -> t -> t

  (* This is intended to evaluate a distinct free_names from the one in acc, one
     must be careful to update acc afterward when necessary *)
  val eval_branch_free_names :
    t -> f:(t -> t * 'a) -> Name_occurrences.t * t * 'a

  val cost_metrics : t -> Cost_metrics.t

  val increment_metrics : Cost_metrics.t -> t -> t

  val with_cost_metrics : Cost_metrics.t -> t -> t

  (* Executes [f] in an acc with an empty cost metrics and returns the cost
     metrics for the term generated by f separately from the one in the acc. As
     for [eval_branch_free_names], the returned free_names differ from the one
     in acc *)
  val measure_cost_metrics :
    t -> f:(t -> t * 'a) -> Cost_metrics.t * Name_occurrences.t * t * 'a

  val symbol_for_global : t -> Ident.t -> Symbol.t
end

(** Used to represent information about a set of function declarations during
    closure conversion. (The only case in which such a set may contain more than
    one declaration is when processing "let rec".) *)
module Function_decls : sig
  module Function_decl : sig
    type t

    val create :
      let_rec_ident:Ident.t option ->
      closure_id:Closure_id.t ->
      kind:Lambda.function_kind ->
      params:(Ident.t * Lambda.value_kind) list ->
      return:Lambda.value_kind ->
      return_continuation:Continuation.t ->
      exn_continuation:IR.exn_continuation ->
      body:(Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t) ->
      attr:Lambda.function_attribute ->
      loc:Lambda.scoped_location ->
      free_idents_of_body:Ident.Set.t ->
      stub:bool ->
      Recursive.t ->
      t

    val let_rec_ident : t -> Ident.t

    val closure_id : t -> Closure_id.t

    val kind : t -> Lambda.function_kind

    val params : t -> (Ident.t * Lambda.value_kind) list

    val return : t -> Lambda.value_kind

    val return_continuation : t -> Continuation.t

    val exn_continuation : t -> IR.exn_continuation

    val body : t -> Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t

    val inline : t -> Lambda.inline_attribute

    val specialise : t -> Lambda.specialise_attribute

    val is_a_functor : t -> bool

    val stub : t -> bool

    val loc : t -> Lambda.scoped_location

    val recursive : t -> Recursive.t

    (* Like [all_free_idents], but for just one function. *)
    val free_idents : t -> Ident.Set.t
  end

  type t

  val create : Function_decl.t list -> t

  val to_list : t -> Function_decl.t list

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> Ident.Set.t
end

open! Flambda.Import

module Expr_with_acc : sig
  type t = Expr.t

  val create_apply_cont : Acc.t -> Apply_cont.t -> Acc.t * t

  val create_apply : Acc.t -> Apply.t -> Acc.t * t

  val create_let : Acc.t * Let.t -> Acc.t * t

  val create_switch : Acc.t -> Switch.t -> Acc.t * t

  val create_invalid :
    Acc.t -> ?semantics:Invalid_term_semantics.t -> unit -> Acc.t * t
end

module Apply_cont_with_acc : sig
  val create :
    Acc.t ->
    ?trap_action:Trap_action.t ->
    ?args_approx:Env.value_approximation list ->
    Continuation.t ->
    args:Simple.t list ->
    dbg:Debuginfo.t ->
    Acc.t * Apply_cont.t

  val goto : Acc.t -> Continuation.t -> Acc.t * Apply_cont.t
end

module Let_with_acc : sig
  val create :
    Acc.t -> Bound_pattern.t -> Named.t -> body:Expr_with_acc.t -> Acc.t * Let.t
end

module Let_cont_with_acc : sig
  val build_recursive :
    Acc.t ->
    handlers:
      ((Acc.t -> Acc.t * Expr_with_acc.t) * Bound_parameter.t list * bool)
      Continuation.Map.t ->
    body:(Acc.t -> Acc.t * Expr_with_acc.t) ->
    Acc.t * Expr_with_acc.t

  val build_non_recursive :
    Acc.t ->
    Continuation.t ->
    handler_params:Bound_parameter.t list ->
    handler:(Acc.t -> Acc.t * Expr_with_acc.t) ->
    body:(Acc.t -> Acc.t * Expr_with_acc.t) ->
    is_exn_handler:bool ->
    Acc.t * Expr_with_acc.t
end
