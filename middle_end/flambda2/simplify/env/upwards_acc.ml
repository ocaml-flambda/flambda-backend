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

module ART = Are_rebuilding_terms
module DA = Downwards_acc
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env
module UE = Upwards_env

type t =
  { uenv : UE.t;
    creation_dacc : DA.t;
    code_age_relation : Code_age_relation.t;
    lifted_constants : LCS.t;
    all_code : Exported_code.t;
    name_occurrences : Name_occurrences.t;
    used_value_slots : Name_occurrences.t;
    shareable_constants : Symbol.t Static_const.Map.t;
    cost_metrics : Cost_metrics.t;
    are_rebuilding_terms : ART.t;
    generate_phantom_lets : bool;
    demoted_exn_handlers : Continuation.Set.t;
    slot_offsets : Slot_offsets.t Or_unknown.t;
    flow_result : Flow_types.Flow_result.t;
    resimplify : bool;
    code_ids_kept_for_zero_alloc : Code_id.Set.t
  }

let [@ocamlformat "disable"] print ppf
      { uenv; creation_dacc = _; code_age_relation; lifted_constants;
        name_occurrences; used_value_slots; all_code = _;
        shareable_constants; cost_metrics; are_rebuilding_terms;
        generate_phantom_lets;
        demoted_exn_handlers; slot_offsets; flow_result; resimplify;
        code_ids_kept_for_zero_alloc;
      } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(uenv@ %a)@]@ \
      @[<hov 1>(code_age_relation@ %a)@]@ \
      @[<hov 1>(lifted_constants@ %a)@]@ \
      @[<hov 1>(name_occurrences@ %a)@]@ \
      @[<hov 1>(used_value_slots@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(are_rebuilding_terms@ %a)@]@ \
      @[<hov 1>(generate_phantom_lets@ %b)@]@ \
      @[<hov 1>(demoted_exn_handlers@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a@)@]@ \
      @[<hov 1>(code_ids_kept_for_zero_alloc@ %a)@]@ \
      @[<hov 1>(flow_result@ %a)@]\
      %a\
      )@]"
    UE.print uenv
    Code_age_relation.print code_age_relation
    LCS.print lifted_constants
    Name_occurrences.print name_occurrences
    Name_occurrences.print used_value_slots
    (Static_const.Map.print Symbol.print) shareable_constants
    Cost_metrics.print cost_metrics
    ART.print are_rebuilding_terms
    generate_phantom_lets
    Continuation.Set.print demoted_exn_handlers
    (Or_unknown.print Slot_offsets.print) slot_offsets
    Flow_types.Flow_result.print flow_result
    (if resimplify then
       (fun ppf () -> Format.fprintf ppf "@ @[<hov 1>(should_resimplify)@]")
     else
       (fun _ppf () -> ())) ()
    Code_id.Set.print code_ids_kept_for_zero_alloc

let create ~flow_result ~compute_slot_offsets ~code_ids_kept_for_zero_alloc uenv
    dacc =
  let are_rebuilding_terms = DE.are_rebuilding_terms (DA.denv dacc) in
  let generate_phantom_lets = DE.generate_phantom_lets (DA.denv dacc) in
  let slot_offsets : _ Or_unknown.t =
    if compute_slot_offsets then Known Slot_offsets.empty else Unknown
  in
  { uenv;
    creation_dacc = dacc;
    code_age_relation = TE.code_age_relation (DA.typing_env dacc);
    lifted_constants = LCS.empty;
    all_code = Exported_code.empty;
    name_occurrences = Name_occurrences.empty;
    (* [used_value_slots] must be kept separate from the normal free names
       tracking in [name_occurrences], since it is always accumulated, and never
       saved and restored (like free name information is when dealing with a
       [Let_cont]). *)
    (* CR gbury: since [used_value_slots] (and [mshinwell:] various other
       things), are actually never modified in the uacc, and initialised using
       the dacc, why not access them through the dacc ? that would reduce the
       number of words allocated for each uacc (at the cost of an extra
       lookup) *)
    used_value_slots = DA.used_value_slots dacc;
    shareable_constants = DA.shareable_constants dacc;
    cost_metrics = Cost_metrics.zero;
    are_rebuilding_terms;
    generate_phantom_lets;
    demoted_exn_handlers = DA.demoted_exn_handlers dacc;
    slot_offsets;
    flow_result;
    resimplify = false;
    code_ids_kept_for_zero_alloc
  }

let creation_dacc t = t.creation_dacc

let uenv t = t.uenv

let code_age_relation t = t.code_age_relation

let lifted_constants t = t.lifted_constants

let get_and_clear_lifted_constants t =
  { t with lifted_constants = LCS.empty }, t.lifted_constants

let add_lifted_constant t const =
  { t with lifted_constants = LCS.add t.lifted_constants const }

let cost_metrics t = t.cost_metrics

let are_rebuilding_terms t = t.are_rebuilding_terms

let with_lifted_constants t lifted_constants = { t with lifted_constants }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let map_uenv t ~f = { t with uenv = f t.uenv }

let with_uenv t uenv = { t with uenv }

let remember_code_for_cmx t code =
  if ART.do_not_rebuild_terms t.are_rebuilding_terms
  then t
  else
    let keep_code code_id =
      Code_id.Set.mem code_id (DA.code_ids_to_remember t.creation_dacc)
    in
    let all_code = Exported_code.add_code ~keep_code code t.all_code in
    { t with all_code }

let all_code t = t.all_code

let name_occurrences t = t.name_occurrences

let with_name_occurrences t ~name_occurrences =
  if name_occurrences == t.name_occurrences
  then t
  else { t with name_occurrences }

let clear_name_occurrences t =
  with_name_occurrences t ~name_occurrences:Name_occurrences.empty

let add_free_names t free_names =
  let name_occurrences = Name_occurrences.union t.name_occurrences free_names in
  { t with name_occurrences }

let used_value_slots t = t.used_value_slots

let shareable_constants t = t.shareable_constants

let remove_all_occurrences_of_free_names t to_remove =
  let name_occurrences =
    Name_occurrences.diff t.name_occurrences ~without:to_remove
  in
  { t with name_occurrences }

let clear_cost_metrics t = { t with cost_metrics = Cost_metrics.zero }

let with_cost_metrics cost_metrics t = { t with cost_metrics }

let notify_added ~code_size t =
  { t with cost_metrics = Cost_metrics.notify_added ~code_size t.cost_metrics }

let notify_removed ~operation t =
  { t with
    cost_metrics = Cost_metrics.notify_removed ~operation t.cost_metrics
  }

let add_cost_metrics cost_metrics t =
  { t with cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics }

let add_cost_metrics_and_with_name_occurrences t cost_metrics name_occurrences =
  { t with
    cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics;
    name_occurrences
  }

let generate_phantom_lets t = t.generate_phantom_lets

let is_demoted_exn_handler t cont =
  Continuation.Set.mem cont t.demoted_exn_handlers

let slot_offsets t = t.slot_offsets

let with_slot_offsets t slot_offsets = { t with slot_offsets }

let required_names t = t.flow_result.data_flow_result.required_names

let reachable_code_ids t = t.flow_result.data_flow_result.reachable_code_ids

let continuation_param_aliases t = t.flow_result.aliases_result

let mutable_unboxing_result t = t.flow_result.mutable_unboxing_result

let set_resimplify t = { t with resimplify = true }

let resimplify t = t.resimplify

let code_ids_kept_for_zero_alloc t = t.code_ids_kept_for_zero_alloc
