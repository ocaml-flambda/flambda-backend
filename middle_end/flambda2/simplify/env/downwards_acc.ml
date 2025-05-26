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

module CUE = Continuation_uses_env
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env

type t =
  { denv : DE.t;
    continuation_uses_env : CUE.t;
    shareable_constants : Symbol.t Static_const.Map.t;
    used_value_slots : Name_occurrences.t;
    lifted_constants : LCS.t;
    flow_acc : Flow.Acc.t;
    demoted_exn_handlers : Continuation.Set.t;
    code_ids_to_remember : Code_id.Set.t;
    code_ids_to_never_delete : Code_id.Set.t;
    code_ids_never_simplified : Code_id.Set.t;
    slot_offsets : Slot_offsets.t Code_id.Map.t;
    debuginfo_rewrites : Debuginfo.t Simple.Map.t;
    are_lifting_conts : Are_lifting_conts.t;
    lifted_continuations : (DE.t * Original_handlers.t) list;
    (* head of the list is the innermost continuation being lifted *)
    continuation_lifting_budget : int;
    continuation_specialization_budget : int;
    continuations_to_specialize : Continuation.Set.t;
    (* CR gbury: we could try and encode the set of continuations to specialize
       into the map below as the keys of the map *)
    specialization_map : Continuation.t Continuation_callsite_map.t
  }

let print_lifted_cont ppf (denv, original_handlers) =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(denv %a)@]@ @[<hov 1>(original_handlers %a)@])@]"
    DE.print denv Original_handlers.print original_handlers

let [@ocamlformat "disable"] print ppf
      { denv; continuation_uses_env; shareable_constants; used_value_slots;
        lifted_constants; flow_acc; demoted_exn_handlers; code_ids_to_remember;
        code_ids_to_never_delete; code_ids_never_simplified; slot_offsets; debuginfo_rewrites;
        are_lifting_conts; lifted_continuations; continuation_lifting_budget;
        continuation_specialization_budget; continuations_to_specialize; specialization_map; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(denv@ %a)@]@ \
      @[<hov 1>(continuation_uses_env@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]@ \
      @[<hov 1>(used_value_slots@ %a)@]@ \
      @[<hov 1>(lifted_constant_state@ %a)@]@ \
      @[<hov 1>(flow_acc@ %a)@]@ \
      @[<hov 1>(demoted_exn_handlers@ %a)@]@ \
      @[<hov 1>(code_ids_to_remember@ %a)@]@ \
      @[<hov 1>(code_ids_to_never_delete@ %a)@]@ \
      @[<hov 1>(code_ids_never_simplified@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a)@ \
      @[<hov 1>(debuginfo_rewrites@ %a)@]@ \
      @[<hov 1>(are_lifting_conts@ %a)@]@ \
      @[<hov 1>(lifted_continuations@ %a)@]@ \
      @[<hov 1>(continuation_lifting_budget %d)@]@ \
      @[<hov 1>(continuation_specialization_budget %d)@]@ \
      @[<hov 1>(continuations_to_specialize %a)@]@ \
      @[<hov 1>(specialization_map %a)@]\
      )@]"
    DE.print denv
    CUE.print continuation_uses_env
    (Static_const.Map.print Symbol.print) shareable_constants
    Name_occurrences.print used_value_slots
    LCS.print lifted_constants
    Flow.Acc.print flow_acc
    Continuation.Set.print demoted_exn_handlers
    Code_id.Set.print code_ids_to_remember
    Code_id.Set.print code_ids_to_never_delete
    Code_id.Set.print code_ids_never_simplified
    (Code_id.Map.print Slot_offsets.print) slot_offsets
    (Simple.Map.print Debuginfo.print_compact) debuginfo_rewrites
    Are_lifting_conts.print are_lifting_conts
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       print_lifted_cont) lifted_continuations
    continuation_lifting_budget
    continuation_specialization_budget
    Continuation.Set.print continuations_to_specialize
    (Continuation.Map.print (Apply_cont_rewrite_id.Map.print Continuation.print)) specialization_map

let create denv slot_offsets continuation_uses_env =
  { denv;
    continuation_uses_env;
    slot_offsets;
    shareable_constants = Static_const.Map.empty;
    used_value_slots = Name_occurrences.empty;
    lifted_constants = LCS.empty;
    flow_acc = Flow.Acc.empty ();
    demoted_exn_handlers = Continuation.Set.empty;
    code_ids_to_remember = Code_id.Set.empty;
    code_ids_to_never_delete = Code_id.Set.empty;
    code_ids_never_simplified = Code_id.Set.empty;
    debuginfo_rewrites = Simple.Map.empty;
    are_lifting_conts = Are_lifting_conts.no_lifting;
    lifted_continuations = [];
    continuation_lifting_budget = Flambda_features.Expert.cont_lifting_budget ();
    continuation_specialization_budget =
      Flambda_features.Expert.cont_spec_budget ();
    continuations_to_specialize = Continuation.Set.empty;
    specialization_map = Continuation.Map.empty
  }

let denv t = t.denv

let flow_acc t = t.flow_acc

let[@inline always] map_flow_acc t ~f = { t with flow_acc = f t.flow_acc }

let[@inline always] map_denv t ~f = { t with denv = f t.denv }

let[@inline always] with_denv t denv = { t with denv }

let with_continuation_uses_env t ~cont_uses_env =
  { t with continuation_uses_env = cont_uses_env }

let record_continuation_use t cont use_kind ~env_at_use ~arg_types =
  let cont_uses_env, id =
    CUE.record_continuation_use t.continuation_uses_env cont use_kind
      ~env_at_use ~arg_types
  in
  with_continuation_uses_env t ~cont_uses_env, id

let delete_continuation_uses t cont =
  let cont_uses_env =
    CUE.delete_continuation_uses t.continuation_uses_env cont
  in
  with_continuation_uses_env t ~cont_uses_env

let num_continuation_uses t cont =
  CUE.num_continuation_uses t.continuation_uses_env cont

let continuation_uses_env t = t.continuation_uses_env

let code_age_relation t = TE.code_age_relation (DE.typing_env (denv t))

let with_code_age_relation t ~code_age_relation =
  let typing_env =
    TE.with_code_age_relation (DE.typing_env (denv t)) code_age_relation
  in
  with_denv t (DE.with_typing_env (denv t) typing_env)

let typing_env t = DE.typing_env (denv t)

let add_variable t var ty = with_denv t (DE.add_variable (denv t) var ty)

let get_typing_env_no_more_than_one_use t k =
  CUE.get_typing_env_no_more_than_one_use t.continuation_uses_env k

let add_to_lifted_constant_accumulator ?also_add_to_env t constants =
  let also_add_to_env =
    match also_add_to_env with None -> false | Some () -> true
  in
  let lifted_constants = LCS.union t.lifted_constants constants in
  let denv =
    if also_add_to_env then LCS.add_to_denv t.denv constants else t.denv
  in
  { t with lifted_constants; denv }

let get_lifted_constants t = t.lifted_constants

let clear_lifted_constants t = { t with lifted_constants = LCS.empty }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let get_and_clear_lifted_constants t =
  let constants = t.lifted_constants in
  let t = clear_lifted_constants t in
  t, constants

let set_lifted_constants t consts = { t with lifted_constants = consts }

let find_shareable_constant t static_const =
  Static_const.Map.find_opt static_const t.shareable_constants

let consider_constant_for_sharing t symbol static_const =
  if not (Static_const.can_share static_const)
  then t
  else
    { t with
      shareable_constants =
        Static_const.Map.add static_const symbol t.shareable_constants
    }

let with_shareable_constants t ~shareable_constants =
  { t with shareable_constants }

let shareable_constants t = t.shareable_constants

let add_use_of_value_slot t value_slot =
  { t with
    used_value_slots =
      Name_occurrences.add_value_slot_in_projection t.used_value_slots
        value_slot Name_mode.normal
  }

let used_value_slots t = t.used_value_slots

let all_continuations_used t =
  CUE.all_continuations_used t.continuation_uses_env

let with_used_value_slots t ~used_value_slots = { t with used_value_slots }

let add_code_ids_to_remember t code_ids =
  if DE.at_unit_toplevel t.denv
  then t
  else
    { t with
      code_ids_to_remember = Code_id.Set.union code_ids t.code_ids_to_remember
    }

let code_ids_to_remember t = t.code_ids_to_remember

let with_code_ids_to_remember t ~code_ids_to_remember =
  { t with code_ids_to_remember }

let add_code_ids_to_never_delete t code_ids =
  { t with
    code_ids_to_never_delete =
      Code_id.Set.union code_ids t.code_ids_to_never_delete
  }

let code_ids_to_never_delete t = t.code_ids_to_never_delete

let with_code_ids_to_never_delete t ~code_ids_to_never_delete =
  { t with code_ids_to_never_delete }

let add_code_ids_never_simplified t ~old_code_ids =
  { t with
    code_ids_never_simplified =
      Code_id.Set.union old_code_ids t.code_ids_never_simplified
  }

let code_ids_never_simplified t = t.code_ids_never_simplified

let with_code_ids_never_simplified t ~code_ids_never_simplified =
  { t with code_ids_never_simplified }

let are_rebuilding_terms t = DE.are_rebuilding_terms t.denv

let demote_exn_handler t cont =
  { t with
    demoted_exn_handlers = Continuation.Set.add cont t.demoted_exn_handlers
  }

let demoted_exn_handlers t = t.demoted_exn_handlers

let slot_offsets t = t.slot_offsets

let with_slot_offsets t ~slot_offsets = { t with slot_offsets }

let find_debuginfo_rewrite t ~bound_to =
  Simple.Map.find_opt bound_to t.debuginfo_rewrites

let merge_debuginfo_rewrite t ~bound_to dbg =
  let dbg =
    match find_debuginfo_rewrite t ~bound_to with
    | None -> dbg
    | Some earlier_dbg -> Debuginfo.merge ~into:earlier_dbg dbg
  in
  { t with
    debuginfo_rewrites =
      Simple.Map.add (* or replace *) bound_to dbg t.debuginfo_rewrites
  }

let are_lifting_conts t = t.are_lifting_conts

let with_are_lifting_conts t are_lifting_conts = { t with are_lifting_conts }

let get_and_clear_lifted_continuations t =
  { t with lifted_continuations = [] }, t.lifted_continuations

let add_lifted_continuation denv original_handlers t =
  { t with
    lifted_continuations = (denv, original_handlers) :: t.lifted_continuations
  }

(* Invariant: budget < 0 means no limit on cont lifting *)
let get_continuation_lifting_budget t =
  let budget = t.continuation_lifting_budget in
  if budget < 0 then max_int else budget

let with_continuation_lifting_budget t budget =
  { t with continuation_lifting_budget = budget }

let reset_continuation_lifting_budget t =
  with_continuation_lifting_budget t
    (Flambda_features.Expert.cont_lifting_budget ())

let decrease_continuation_lifting_budget t cost =
  if t.continuation_lifting_budget < 0
  then t
  else
    with_continuation_lifting_budget t
      (max 0 (t.continuation_lifting_budget - cost))

(* CR gbury: remove this code and use a proper heuristic for specialization *)
let get_continuation_specialization_budget t =
  let budget = t.continuation_specialization_budget in
  if budget < 0 then max_int else budget

let with_continuation_specialization_budget t budget =
  { t with continuation_specialization_budget = budget }

let reset_continuation_specialization_budget t =
  with_continuation_specialization_budget t
    (Flambda_features.Expert.cont_spec_budget ())

let decrease_continuation_specialization_budget t cost =
  if t.continuation_specialization_budget < 0
  then t
  else
    let budget = max 0 (t.continuation_specialization_budget - cost) in
    with_continuation_specialization_budget t budget

let prepare_for_speculative_inlining dacc =
  let dacc =
    map_denv ~f:DE.set_do_not_rebuild_terms_and_disable_inlining dacc
  in
  with_are_lifting_conts dacc Are_lifting_conts.no_lifting

let continuations_to_specialize t = t.continuations_to_specialize

let add_continuation_to_specialize t cont =
  { t with
    continuations_to_specialize =
      Continuation.Set.add cont t.continuations_to_specialize
  }

let add_specialization t id ~old ~specialized =
  let specialization_map =
    Continuation_callsite_map.add old id specialized t.specialization_map
  in
  { t with specialization_map }

let specialization_map t = t.specialization_map
