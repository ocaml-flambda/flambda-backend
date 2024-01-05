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

module Cont_arg = struct
  type t =
    | Function_result
    | Simple of Simple.t
    | New_let_binding of Variable.t * Name_occurrences.t

  let print ppf = function
    | Function_result -> Format.fprintf ppf "Function_result"
    | Simple s -> Simple.print ppf s
    | New_let_binding (v, _) ->
      Format.fprintf ppf "New_let_binding %a" Variable.print v
end

(* Primitives that we must track for mutable unboxing *)
(* ************************************************** *)

module Mutable_prim = struct
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

  let print ppf = function
    | Is_int v -> Format.fprintf ppf "Is_int (%a)" Variable.print v
    | Get_tag v -> Format.fprintf ppf "Get_tag (%a)" Variable.print v
    | Block_load { block; field; _ } ->
      Format.fprintf ppf "Block_load (%a, %i)" Variable.print block field
    | Block_set { block; field; value; _ } ->
      Format.fprintf ppf "Block_set (%a, %i, %a)" Variable.print block field
        Simple.print value
    | Make_block { fields; _ } ->
      Format.fprintf ppf "Make_block [%a]" Simple.List.print fields
end

(* Bindings to primitive that we track for the mutable unboxing *)
(* ************************************************************ *)

module Mutable_let_prim = struct
  type t =
    { bound_var : Variable.t;
      prim : Mutable_prim.t;
      original_prim : Flambda_primitive.t;
      named_rewrite_id : Named_rewrite_id.t
    }

  let print ppf { bound_var; prim; original_prim = _; named_rewrite_id = _ } =
    Format.fprintf ppf "%a = %a" Variable.print bound_var Mutable_prim.print
      prim

  module List = struct
    type nonrec t = t list

    let print_rev ppf l =
      Format.fprintf ppf "[%a]"
        (Format.pp_print_list print ~pp_sep:Format.pp_print_space)
        (List.rev l)
  end
end

(* Accumulated flow information for a single continuation handler *)
(* ************************************************************** *)

module Continuation_info = struct
  (* Some notes:

     - {direct_aliases} is used to have a more precise escaping analysis for
     mutable unboxing, since from_lambda occasionally generates aliases of the
     form [let r' = r], which without a precise alias tracking, would be
     considered as escaping.

     - the {bindings} field records dependencies between names, usually created
     by primitive applications; in the case of effectful primitives (that can't
     be removed), we do not record dependencies, but instead recrod all args of
     the effectful prim as unconditionally used (in {used_in_handler}).
     Similarly, for primitives tracked in {mutable_let_prims_rev}, we do not
     record the dependencies in this field, since we already have the more
     precise information. *)
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

  let [@ocamlformat "disable"] print ppf
      { continuation;
        recursive;
        is_exn_handler;
        params;
        parent_continuation;
        used_in_handler;
        bindings;
        direct_aliases;
        mutable_let_prims_rev;
        defined;
        code_ids;
        value_slots;
        apply_cont_args
      } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(continuation %a)@]@ \
        %s\
        %s\
        @[<hov 1>(params %a)@]@ \
        @[<hov 1>(parent_continuation %a)@]@ \
        @[<hov 1>(used_in_handler %a)@]@ \
        @[<hov 1>(bindings %a)@]@ \
        @[<hov 1>(direct_aliases %a)@]@ \
        @[<hov 1>(ref_prims %a)@]@ \
        @[<hov 1>(defined %a)@]@ \
        @[<hov 1>(code_ids %a)@]@ \
        @[<hov 1>(value_slots %a)@]@ \
        @[<hov 1>(apply_cont_args %a)@]\
      )@]"
      Continuation.print continuation
      (if recursive then "(recursive) " else "")
      (if is_exn_handler then "(exn_handler) " else "")
      Bound_parameters.print params
      (Format.pp_print_option ~none:(fun ppf () -> Format.fprintf ppf "root")
         Continuation.print) parent_continuation
      Name_occurrences.print used_in_handler
      (Name.Map.print Name_occurrences.print) bindings
      (Variable.Map.print Simple.print) direct_aliases
      Mutable_let_prim.List.print_rev mutable_let_prims_rev
      Variable.Set.print
      defined
      (Code_id.Map.print Name_occurrences.print)
      code_ids
      (Value_slot.Map.print (Name.Map.print Name_occurrences.print))
      value_slots
      (Continuation.Map.print (Apply_cont_rewrite_id.Map.print
        (Numeric_types.Int.Map.print Cont_arg.print)))
      apply_cont_args
end

(* Flow accumulator *)
(* **************** *)

module Acc = struct
  type t =
    { stack : Continuation_info.t list;
      map : Continuation_info.t Continuation.Map.t;
      extra : Continuation_extra_params_and_args.t Continuation.Map.t;
      dummy_toplevel_cont : Continuation.t
    }

  let print_stack ppf stack =
    Format.fprintf ppf "@[<v 1>(%a)@]"
      (Format.pp_print_list Continuation_info.print
         ~pp_sep:Format.pp_print_space)
      stack

  let print_map ppf map = Continuation.Map.print Continuation_info.print ppf map

  let print_extra ppf extra =
    Continuation.Map.print Continuation_extra_params_and_args.print ppf extra

  let [@ocamlformat "disable"] print ppf { stack; map; extra; dummy_toplevel_cont = _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(stack %a)@]@ \
       @[<hov 1>(map %a)@]@ \
       @[<hov 1>(extra %a)@]\
       )@]"
      print_stack stack
      print_map map
      print_extra extra
end

(* Result of the flow analysis: reachable code ids *)
(* *********************************************** *)

module Reachable_code_ids = struct
  type t =
    { live_code_ids : Code_id.Set.t;
      ancestors_of_live_code_ids : Code_id.Set.t
    }

  let [@ocamlformat "disable"] print ppf { live_code_ids; ancestors_of_live_code_ids; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(live_code_ids@ %a)@]@ \
        @[<hov 1>(ancestors_of_live_code_ids@ %a)@]\
      )@]"
      Code_id.Set.print live_code_ids
      Code_id.Set.print ancestors_of_live_code_ids
end

(* Result of the flow analysis: data flow analysis *)
(* *********************************************** *)

module Data_flow_result = struct
  type t =
    { required_names : Name.Set.t;
      reachable_code_ids : Reachable_code_ids.t Or_unknown.t
    }

  let[@ocamlformat "disable"] print ppf
      { required_names; reachable_code_ids; } =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(required_names@ %a)@]@ \
         @[<hov 1>(reachable_code_ids@ %a)@]@ \
       )@]"
    Name.Set.print required_names
    (Or_unknown.print Reachable_code_ids.print) reachable_code_ids
end

(* Result of the flow analysis: aliased parameters of continuations *)
(* **************************************************************** *)

module Continuation_param_aliases = struct
  type recursive_continuation_wrapper =
    | No_wrapper
    | Wrapper_needed

  type t =
    { removed_aliased_params_and_extra_params : Variable.Set.t;
      lets_to_introduce : Variable.t Variable.Map.t;
      extra_args_for_aliases : Variable.Set.t;
      recursive_continuation_wrapper : recursive_continuation_wrapper
    }

  let [@ocamlformat "disable"] print ppf
      { removed_aliased_params_and_extra_params; lets_to_introduce;
        extra_args_for_aliases; recursive_continuation_wrapper } =
    let pp_wrapper ppf = function
      | No_wrapper -> ()
      | Wrapper_needed ->
        Format.fprintf ppf "@ @[<hov 1>(recursive_continuation_wrapper needed)@]"
    in
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(removed_aliased_params_and_extra_params %a)@]@ \
        @[<hov 1>(lets_to_introduce %a)@]\
        @[<hov 1>(extra_args_for_aliases %a)@]\
        %a\
       )@]"
      Variable.Set.print removed_aliased_params_and_extra_params
      (Variable.Map.print Variable.print) lets_to_introduce
      Variable.Set.print extra_args_for_aliases
      pp_wrapper recursive_continuation_wrapper
end

(* Result of the flow analysis: alias analysis result *)
(* ************************************************** *)

module Alias_result = struct
  type t =
    { aliases_kind : Flambda_kind.t Variable.Map.t;
      continuation_parameters : Continuation_param_aliases.t Continuation.Map.t
    }

  let [@ocamlformat "disable"] print ppf
      { aliases_kind; continuation_parameters } =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(aliases_kind@ %a)@]@ \
         @[<hov 1>(continuation_parameters@ %a)@]\
       )@]"
      (Variable.Map.print Flambda_kind.print) aliases_kind
      (Continuation.Map.print Continuation_param_aliases.print) continuation_parameters
end

(* Result of the flow analysis: mutable unboxing *)
(* ********************************************* *)

module Mutable_unboxing_result = struct
  type t =
    { did_unbox_a_mutable_block : bool;
      additionnal_epa : Continuation_extra_params_and_args.t Continuation.Map.t;
      let_rewrites : Named_rewrite.t Named_rewrite_id.Map.t
    }

  let [@ocamlformat "disable"] print ppf { did_unbox_a_mutable_block; additionnal_epa; let_rewrites } =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(did_unbox_a_mutable_block@ %b)@]@ \
         @[<hov 1>(additionnal_epa@ %a)@]@ \
         @[<hov 1>(let_rewrites@ %a)@]\
       )@]"
      did_unbox_a_mutable_block
      (Continuation.Map.print Continuation_extra_params_and_args.print) additionnal_epa
      (Named_rewrite_id.Map.print Named_rewrite.print) let_rewrites
end

(* Result of the flow analysis *)
(* *************************** *)

module Flow_result = struct
  type t =
    { data_flow_result : Data_flow_result.t;
      aliases_result : Alias_result.t;
      mutable_unboxing_result : Mutable_unboxing_result.t
    }

  let [@ocamlformat "disable"] print ppf
      { data_flow_result; aliases_result; mutable_unboxing_result; } =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(data_flow_result@ %a)@]@ \
         @[<hov 1>(aliases_result@ %a)@]@ \
         @[<hov 1>(mutable_unboxing_result@ %a)@]\
       )@]"
    Data_flow_result.print data_flow_result
    Alias_result.print aliases_result
    Mutable_unboxing_result.print mutable_unboxing_result
end
