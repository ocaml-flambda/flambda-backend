(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    params : Bound_parameters.t;
    my_closure : Variable.t;
    my_region : Variable.t;
    my_ghost_region : Variable.t;
    my_depth : Variable.t
  }

let[@ocamlformat "disable"] print ppf
    { return_continuation; exn_continuation; params; my_closure; my_region;
      my_ghost_region; my_depth } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(return_continuation@ %a)@]@ \
      @[<hov 1>(exn_continuation@ %a)@]@ \
      @[<hov 1>(params@ %a)@]@ \
      @[<hov 1>(my_closure@ %a)@]@ \
      @[<hov 1>(my_region@ %a)@]@ \
      @[<hov 1>(my_ghost_region@ %a)@]@ \
      @[<hov 1>(my_depth@ %a)@])@]"
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Bound_parameters.print params
    Variable.print my_closure
    Variable.print my_region
    Variable.print my_ghost_region
    Variable.print my_depth

let create ~return_continuation ~exn_continuation ~params ~my_closure ~my_region
    ~my_ghost_region ~my_depth =
  Bound_parameters.check_no_duplicates params;
  (if Flambda_features.check_invariants ()
  then
    let params_set = Bound_parameters.var_set params in
    let my_set =
      Variable.Set.of_list [my_closure; my_region; my_ghost_region; my_depth]
    in
    if Variable.Set.cardinal my_set <> 4
       || not (Variable.Set.is_empty (Variable.Set.inter my_set params_set))
    then
      Misc.fatal_errorf
        "[my_closure], [my_region], [my_ghost_region] and [my_depth] must be \
         disjoint from themselves and the other parameters");
  { return_continuation;
    exn_continuation;
    params;
    my_closure;
    my_region;
    my_ghost_region;
    my_depth
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let params t = t.params

let my_closure t = t.my_closure

let my_region t = t.my_region

let my_ghost_region t = t.my_ghost_region

let my_depth t = t.my_depth

let free_names
    { return_continuation;
      exn_continuation;
      params;
      my_closure;
      my_region;
      my_ghost_region;
      my_depth
    } =
  (* See [bound_continuations.ml] for why [add_traps] is [true]. *)
  let free_names =
    Name_occurrences.add_continuation Name_occurrences.empty return_continuation
      ~has_traps:true
  in
  let free_names =
    Name_occurrences.add_continuation free_names exn_continuation
      ~has_traps:true
  in
  let free_names =
    Name_occurrences.union free_names (Bound_parameters.free_names params)
  in
  let free_names =
    Name_occurrences.add_variable free_names my_closure Name_mode.normal
  in
  let free_names =
    Name_occurrences.add_variable free_names my_region Name_mode.normal
  in
  let free_names =
    Name_occurrences.add_variable free_names my_ghost_region Name_mode.normal
  in
  Name_occurrences.add_variable free_names my_depth Name_mode.normal

let apply_renaming
    { return_continuation;
      exn_continuation;
      params;
      my_closure;
      my_region;
      my_ghost_region;
      my_depth
    } renaming =
  let return_continuation =
    Renaming.apply_continuation renaming return_continuation
  in
  let exn_continuation =
    Renaming.apply_continuation renaming exn_continuation
  in
  let params = Bound_parameters.apply_renaming params renaming in
  let my_closure = Renaming.apply_variable renaming my_closure in
  let my_region = Renaming.apply_variable renaming my_region in
  let my_ghost_region = Renaming.apply_variable renaming my_ghost_region in
  let my_depth = Renaming.apply_variable renaming my_depth in
  (* CR mshinwell: this should have a phys-equal check *)
  { return_continuation;
    exn_continuation;
    params;
    my_closure;
    my_region;
    my_ghost_region;
    my_depth
  }

let ids_for_export
    { return_continuation;
      exn_continuation;
      params;
      my_closure;
      my_region;
      my_ghost_region;
      my_depth
    } =
  let ids =
    Ids_for_export.add_continuation Ids_for_export.empty return_continuation
  in
  let ids = Ids_for_export.add_continuation ids exn_continuation in
  let ids = Ids_for_export.union ids (Bound_parameters.ids_for_export params) in
  let ids = Ids_for_export.add_variable ids my_closure in
  let ids = Ids_for_export.add_variable ids my_region in
  let ids = Ids_for_export.add_variable ids my_ghost_region in
  Ids_for_export.add_variable ids my_depth

let rename
    { return_continuation;
      exn_continuation;
      params;
      my_closure;
      my_region;
      my_ghost_region;
      my_depth
    } =
  { return_continuation = Continuation.rename return_continuation;
    exn_continuation = Continuation.rename exn_continuation;
    params = Bound_parameters.rename params;
    my_closure = Variable.rename my_closure;
    my_region = Variable.rename my_region;
    my_ghost_region = Variable.rename my_ghost_region;
    my_depth = Variable.rename my_depth
  }

let renaming
    { return_continuation = return_continuation1;
      exn_continuation = exn_continuation1;
      params = params1;
      my_closure = my_closure1;
      my_region = my_region1;
      my_ghost_region = my_ghost_region1;
      my_depth = my_depth1
    }
    ~guaranteed_fresh:
      { return_continuation = return_continuation2;
        exn_continuation = exn_continuation2;
        params = params2;
        my_closure = my_closure2;
        my_region = my_region2;
        my_ghost_region = my_ghost_region2;
        my_depth = my_depth2
      } =
  let renaming =
    Renaming.add_fresh_continuation Renaming.empty return_continuation1
      ~guaranteed_fresh:return_continuation2
  in
  let renaming =
    Renaming.add_fresh_continuation renaming exn_continuation1
      ~guaranteed_fresh:exn_continuation2
  in
  let renaming =
    Renaming.compose
      ~second:(Bound_parameters.renaming params1 ~guaranteed_fresh:params2)
      ~first:renaming
  in
  let renaming =
    Renaming.add_fresh_variable renaming my_closure1
      ~guaranteed_fresh:my_closure2
  in
  let renaming =
    Renaming.add_fresh_variable renaming my_region1 ~guaranteed_fresh:my_region2
  in
  let renaming =
    Renaming.add_fresh_variable renaming my_ghost_region1
      ~guaranteed_fresh:my_ghost_region2
  in
  Renaming.add_fresh_variable renaming my_depth1 ~guaranteed_fresh:my_depth2
