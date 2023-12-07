(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR chambart/gbury: we might want to also track function_slots in addition to
   value_slots. *)

(* CR-someday chambart/gbury: get rid of Name_occurences everywhere, this is not
   small while we need only the names

   mshinwell: in practice I'm not sure this will make any difference *)

(* Continuation arguments at call sites of continuations *)
(* ***************************************************** *)

module Cont_arg : sig
  type t =
    | Function_result
    | Simple of Simple.t
    | New_let_binding of Variable.t * Name_occurrences.t

  val print : Format.formatter -> t -> unit
end

(* Primitives that we must track for mutable unboxing *)
(* ************************************************** *)

module Mutable_prim : sig
  type t =
    | Is_int of Variable.t
    | Get_tag of Variable.t
    | Block_load of
        { bak : Flambda_primitive.Block_access_kind.t;
          mut : Mutability.t;
          block : Variable.t;
          field : int
        }
    | Block_set of
        { bak : Flambda_primitive.Block_access_kind.t;
          block : Variable.t;
          field : int;
          value : Simple.t
        }
    | Make_block of
        { kind : Flambda_primitive.Block_kind.t;
          mut : Mutability.t;
          alloc_mode : Alloc_mode.For_allocations.t;
          fields : Simple.t list
        }

  val print : Format.formatter -> t -> unit
end

(* Bindings to primitive that we track for the mutable unboxing *)
(* ************************************************************ *)

module Mutable_let_prim : sig
  type t =
    { bound_var : Variable.t;
      prim : Mutable_prim.t;
      original_prim : Flambda_primitive.t;
      named_rewrite_id : Named_rewrite_id.t
    }

  val print : Format.formatter -> t -> unit

  module List : sig
    type nonrec t = t list

    val print_rev : Format.formatter -> t -> unit
  end
end

(* Accumulated flow information for a single continuation handler *)
(* ************************************************************** *)

module Continuation_info : sig
  type t =
    { continuation : Continuation.t;
      recursive : bool;
      is_exn_handler : bool;
      params : Bound_parameters.t;
      parent_continuation : Continuation.t option;
      used_in_handler : Name_occurrences.t;
      bindings : Name_occurrences.t Name.Map.t;
      direct_aliases : Simple.t Variable.Map.t;
      mutable_let_prims_rev : Mutable_let_prim.List.t;
      defined : Variable.Set.t;
      code_ids : Name_occurrences.t Code_id.Map.t;
      value_slots : Name_occurrences.t Name.Map.t Value_slot.Map.t;
      apply_cont_args :
        Cont_arg.t Numeric_types.Int.Map.t Apply_cont_rewrite_id.Map.t
        Continuation.Map.t
    }

  val print : Format.formatter -> t -> unit
end

(* Flow accumulator *)
(* **************** *)

module Acc : sig
  type t =
    { stack : Continuation_info.t list;
      map : Continuation_info.t Continuation.Map.t;
      extra : Continuation_extra_params_and_args.t Continuation.Map.t;
      dummy_toplevel_cont : Continuation.t
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis: reachable code ids *)
(* *********************************************** *)

module Reachable_code_ids : sig
  type t =
    { live_code_ids : Code_id.Set.t;  (** The set of code ids live/reachable. *)
      ancestors_of_live_code_ids : Code_id.Set.t
          (** The set of code ids that are ancestors of at least one live code
              id. *)
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis: data flow analysis *)
(* *********************************************** *)

module Data_flow_result : sig
  type t =
    { required_names : Name.Set.t;
          (** The set of all variables that are in fact used to compute the
              returned value of the function being analyzed. *)
      reachable_code_ids : Reachable_code_ids.t Or_unknown.t
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis: aliased parameters of continuations *)
(* **************************************************************** *)

module Continuation_param_aliases : sig
  type recursive_continuation_wrapper =
    | No_wrapper
    | Wrapper_needed

  type t =
    { removed_aliased_params_and_extra_params : Variable.Set.t;
      lets_to_introduce : Variable.t Variable.Map.t;
      extra_args_for_aliases : Variable.Set.t;
      recursive_continuation_wrapper : recursive_continuation_wrapper
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis: alias analysis result *)
(* ************************************************** *)

module Alias_result : sig
  type t =
    { aliases_kind : Flambda_kind.t Variable.Map.t;
      continuation_parameters : Continuation_param_aliases.t Continuation.Map.t
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis: mutable unboxing *)
(* ********************************************* *)

module Mutable_unboxing_result : sig
  type t =
    { did_unbox_a_mutable_block : bool;
      additionnal_epa : Continuation_extra_params_and_args.t Continuation.Map.t;
      let_rewrites : Named_rewrite.t Named_rewrite_id.Map.t
    }

  val print : Format.formatter -> t -> unit
end

(* Result of the flow analysis *)
(* *************************** *)

module Flow_result : sig
  type t =
    { data_flow_result : Data_flow_result.t;
      aliases_result : Alias_result.t;
      mutable_unboxing_result : Mutable_unboxing_result.t
    }

  val print : Format.formatter -> t -> unit
end
