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

(** Environments and auxiliary structures used during closure conversion. *)

module IR : sig
  type simple =
    | Var of Ident.t
    | Const of Lambda.structured_constant

  type exn_continuation =
    { exn_handler : Continuation.t;
      extra_args : (simple * Flambda_kind.With_subkind.t) list
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
    | Begin_region of { is_try_region : bool }
    | End_region of
        { is_try_region : bool;
          region : Ident.t
        }
        (** [Begin_region] and [End_region] are needed because these primitives
            don't exist in Lambda *)
    | Prim of
        { prim : Lambda.primitive;
          args : simple list list;
          loc : Lambda.scoped_location;
          exn_continuation : exn_continuation option;
          region : Ident.t
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
      region_close : Lambda.region_close;
      inlined : Lambda.inlined_attribute;
      probe : Lambda.probe;
      mode : Lambda.alloc_mode;
      region : Ident.t;
      original_position : Lambda.position_and_tail_attribute;
      args_arity : [`Complex] Flambda_arity.t;
      return_arity : [`Unarized] Flambda_arity.t
    }

  type switch =
    { numconsts : int;
      (* CR mshinwell: use record types *)
      consts :
        (int * Continuation.t * Debuginfo.t * trap_action option * simple list)
        list;
      failaction :
        (Continuation.t * Debuginfo.t * trap_action option * simple list) option
    }

  val print_simple : Format.formatter -> simple -> unit

  val print_named : Format.formatter -> named -> unit
end

module Inlining : sig
  type inlinable_result =
    | Not_inlinable
    | Inlinable of Code.t

  val threshold : unit -> int

  val definition_inlining_decision :
    Inline_attribute.t ->
    Cost_metrics.t ->
    Function_decl_inlining_decision_type.t
end

(** Used to remember which [Variable.t] values correspond to which [Ident.t]
    values during closure conversion, and similarly for static exception
    identifiers. *)
module Env : sig
  type value_approximation = Code_or_metadata.t Value_approximation.t

  type t

  (** Create an environment marked as being at toplevel. *)
  val create : big_endian:bool -> t

  val set_not_at_toplevel : t -> t

  val at_toplevel : t -> bool

  val clear_local_bindings : t -> t

  val add_var : t -> Ident.t -> Variable.t -> Flambda_kind.With_subkind.t -> t

  val add_vars :
    t -> Ident.t list -> (Variable.t * Flambda_kind.With_subkind.t) list -> t

  val add_var_map :
    t -> (Variable.t * Flambda_kind.With_subkind.t) Ident.Map.t -> t

  val add_var_like :
    t ->
    Ident.t ->
    IR.user_visible ->
    Flambda_kind.With_subkind.t ->
    t * Variable.t

  val add_vars_like :
    t ->
    (Ident.t * IR.user_visible * Flambda_kind.With_subkind.t) list ->
    t * Variable.t list

  val find_name : t -> Ident.t -> Name.t

  val find_name_exn : t -> Ident.t -> Name.t

  val find_var : t -> Ident.t -> Variable.t * Flambda_kind.With_subkind.t

  val find_var_exn : t -> Ident.t -> Variable.t * Flambda_kind.With_subkind.t

  val find_vars :
    t -> Ident.t list -> (Variable.t * Flambda_kind.With_subkind.t) list

  val add_global : t -> int -> Symbol.t -> t

  val find_global : t -> int -> Symbol.t

  val add_simple_to_substitute :
    t -> Ident.t -> Simple.t -> Flambda_kind.With_subkind.t -> t

  val add_simple_to_substitute_map :
    t -> (Simple.t * Flambda_kind.With_subkind.t) Ident.Map.t -> t

  val find_simple_to_substitute_exn :
    t -> Ident.t -> Simple.t * Flambda_kind.With_subkind.t

  val add_var_approximation : t -> Variable.t -> value_approximation -> t

  val add_block_approximation :
    t ->
    Variable.t ->
    Tag.t ->
    value_approximation array ->
    Alloc_mode.For_types.t ->
    t

  val find_var_approximation : t -> Variable.t -> value_approximation

  val current_depth : t -> Variable.t option

  val with_depth : t -> Variable.t -> t

  val current_unit : t -> Compilation_unit.t

  val big_endian : t -> bool

  val set_path_to_root : t -> Debuginfo.Scoped_location.t -> t

  val path_to_root : t -> Debuginfo.Scoped_location.t

  (* The inlining tracker is used to ensure that absolute histories are shared
     between functions defined under the same scope. *)
  val use_inlining_history_tracker : t -> Inlining_history.Tracker.t -> t

  val inlining_history_tracker : t -> Inlining_history.Tracker.t

  (* Relative paths are built directly from scoped locations.

     This is fine because when we convert a function call we know that it was
     never inlined beforehand and thus should inherit a path corresponding to
     its true location in the source file. *)
  val relative_history_from_scoped :
    loc:Debuginfo.Scoped_location.t -> t -> Inlining_history.Relative.t
end

(** Used to pipe some data through closure conversion *)
module Acc : sig
  type closure_info = private
    { code_id : Code_id.t;
      return_continuation : Continuation.t;
      exn_continuation : Exn_continuation.t;
      my_closure : Variable.t;
      is_purely_tailrec : bool;
      slot_offsets_at_definition : Slot_offsets.t
    }

  type t

  val create : cmx_loader:Flambda_cmx.loader -> t

  val manufacture_symbol_short_name : t -> t * Linkage_name.t

  val declared_symbols : t -> (Symbol.t * Static_const.t) list

  val lifted_sets_of_closures :
    t -> (Symbol.t Function_slot.Lmap.t * Flambda.Set_of_closures.t) list

  val shareable_constants : t -> Symbol.t Static_const.Map.t

  val code : t -> Code.t Code_id.Lmap.t

  val code_map : t -> Code.t Code_id.Map.t

  val free_names : t -> Name_occurrences.t

  val seen_a_function : t -> bool

  val with_seen_a_function : t -> bool -> t

  val add_declared_symbol : symbol:Symbol.t -> constant:Static_const.t -> t -> t

  val add_lifted_set_of_closures :
    symbols:Symbol.t Function_slot.Lmap.t ->
    set_of_closures:Flambda.Set_of_closures.t ->
    t ->
    t

  val add_shareable_constant :
    symbol:Symbol.t -> constant:Static_const.t -> t -> t

  val add_code :
    code_id:Code_id.t -> code:Code.t -> ?slot_offsets:Slot_offsets.t -> t -> t

  val add_free_names : Name_occurrences.t -> t -> t

  val remove_var_from_free_names : Variable.t -> t -> t

  val remove_continuation_from_free_names : Continuation.t -> t -> t

  val mark_continuation_as_untrackable : Continuation.t -> t -> t

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

  val slot_offsets : t -> Slot_offsets.t

  val code_slot_offsets : t -> Slot_offsets.t Code_id.Map.t

  val add_offsets_from_code : t -> Code_id.t -> t

  val add_set_of_closures_offsets :
    is_phantom:bool -> t -> Set_of_closures.t -> t

  val top_closure_info : t -> closure_info option

  val push_closure_info :
    t ->
    return_continuation:Continuation.t ->
    exn_continuation:Exn_continuation.t ->
    my_closure:Variable.t ->
    is_purely_tailrec:bool ->
    code_id:Code_id.t ->
    t

  val pop_closure_info : t -> closure_info * t

  val add_symbol_approximation : t -> Symbol.t -> Env.value_approximation -> t

  val find_symbol_approximation : t -> Symbol.t -> Env.value_approximation

  val symbol_approximations : t -> Env.value_approximation Symbol.Map.t
end

(** Used to represent information about a set of function declarations during
    closure conversion. (The only case in which such a set may contain more than
    one declaration is when processing "let rec".) *)
module Function_decls : sig
  module Function_decl : sig
    type unboxing_kind =
      | Fields_of_block_with_tag_zero of Flambda_kind.With_subkind.t list
      | Unboxed_number of Flambda_kind.Boxable_number.t
      | Unboxed_float_record of int

    type calling_convention =
      | Normal_calling_convention
      | Unboxed_calling_convention of
          unboxing_kind option list * unboxing_kind option * Function_slot.t

    type t

    type param =
      { name : Ident.t;
        kind : Flambda_kind.With_subkind.t;
        attributes : Lambda.parameter_attribute;
        mode : Lambda.alloc_mode
      }

    val create :
      let_rec_ident:Ident.t option ->
      function_slot:Function_slot.t ->
      kind:Lambda.function_kind ->
      params:param list ->
      params_arity:[`Complex] Flambda_arity.t ->
      removed_params:Ident.Set.t ->
      return:[`Unarized] Flambda_arity.t ->
      calling_convention:calling_convention ->
      return_continuation:Continuation.t ->
      exn_continuation:IR.exn_continuation ->
      my_region:Ident.t ->
      body:(Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t) ->
      attr:Lambda.function_attribute ->
      loc:Lambda.scoped_location ->
      free_idents_of_body:Ident.Set.t ->
      Recursive.t ->
      closure_alloc_mode:Lambda.alloc_mode ->
      first_complex_local_param:int ->
      result_mode:Lambda.alloc_mode ->
      contains_no_escaping_local_allocs:bool ->
      t

    val let_rec_ident : t -> Ident.t

    val function_slot : t -> Function_slot.t

    val kind : t -> Lambda.function_kind

    val params : t -> param list

    val params_arity : t -> [`Complex] Flambda_arity.t

    val return : t -> [`Unarized] Flambda_arity.t

    val calling_convention : t -> calling_convention

    val return_continuation : t -> Continuation.t

    val exn_continuation : t -> IR.exn_continuation

    val my_region : t -> Ident.t

    val body : t -> Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t

    val inline : t -> Lambda.inline_attribute

    val specialise : t -> Lambda.specialise_attribute

    val poll_attribute : t -> Lambda.poll_attribute

    val loop : t -> Lambda.loop_attribute

    val is_a_functor : t -> bool

    val is_opaque : t -> bool

    val zero_alloc_attribute : t -> Lambda.zero_alloc_attribute

    val stub : t -> bool

    val loc : t -> Lambda.scoped_location

    val recursive : t -> Recursive.t

    val closure_alloc_mode : t -> Lambda.alloc_mode

    val first_complex_local_param : t -> int

    val result_mode : t -> Lambda.alloc_mode

    val contains_no_escaping_local_allocs : t -> bool

    (* Like [all_free_idents], but for just one function. *)
    val free_idents : t -> Ident.Set.t
  end

  type t

  val create : Function_decl.t list -> Lambda.alloc_mode -> t

  val alloc_mode : t -> Lambda.alloc_mode

  val to_list : t -> Function_decl.t list

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> Ident.Set.t
end

open! Flambda.Import

module Expr_with_acc : sig
  type t = Acc.t * Expr.t

  val create_apply_cont : Acc.t -> Apply_cont.t -> t

  val create_apply : Acc.t -> Apply.t -> t

  val create_switch : Acc.t -> Switch.t -> t

  val create_invalid : Acc.t -> Flambda.Invalid.t -> t
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
    Acc.t -> Bound_pattern.t -> Named.t -> body:Expr.t -> Expr_with_acc.t
end

module Let_cont_with_acc : sig
  val build_recursive :
    Acc.t ->
    invariant_params:Bound_parameters.t ->
    handlers:
      ((Acc.t -> Expr_with_acc.t) * Bound_parameters.t * bool * bool)
      Continuation.Map.t ->
    body:(Acc.t -> Expr_with_acc.t) ->
    Expr_with_acc.t

  val build_non_recursive :
    Acc.t ->
    Continuation.t ->
    handler_params:Bound_parameters.t ->
    handler:(Acc.t -> Expr_with_acc.t) ->
    body:(Acc.t -> Expr_with_acc.t) ->
    is_exn_handler:bool ->
    is_cold:bool ->
    Expr_with_acc.t
end
