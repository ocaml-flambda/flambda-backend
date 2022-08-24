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

module T = Flow_types
module EPA = Continuation_extra_params_and_args

let ref_to_var_debug = Sys.getenv_opt "RTV" <> None

type extra_ref_params = Bound_parameter.t list

type extra_ref_args =
  Simple.t Numeric_types.Int.Map.t Apply_cont_rewrite_id.Map.t
    Continuation.Map.t

type t =
  { (* non_escaping_references : Variable.Set.t *)
    non_escaping_makeblock : Flambda_kind.With_subkind.t list Variable.Map.t;
    continuations_with_live_ref : Variable.Set.t Continuation.Map.t;
    extra_ref_params_and_args :
      (extra_ref_params * extra_ref_args) Continuation.Map.t;
    rewrites : Named_rewrite.t Named_rewrite_id.Map.t
  }

let free_names_of_apply_cont_args
    (apply_cont_args :
       T.Cont_arg.t Numeric_types.Int.Map.t Apply_cont_rewrite_id.Map.t) :
  Variable.Set.t =
  Apply_cont_rewrite_id.Map.fold
    (fun _ args free ->
       Numeric_types.Int.Map.fold
         (fun _ (arg : T.Cont_arg.t) free ->
            match arg with
            | Function_result -> free
            | New_let_binding _ ->
              free (* Doesn't really matter, already escaping *)
            | Simple s -> (
                match Simple.must_be_var s with
                | None -> free
                | Some (var, _coer) -> Variable.Set.add var free))
         args free)
    apply_cont_args Variable.Set.empty

let escaping ~(dom : Dominator_graph.alias_map) ~(dom_graph : Dominator_graph.t)
    ~(source_info : T.Acc.t) ~return_continuation ~exn_continuation =
  ignore source_info;
  let escaping_by_alias =
    Variable.Map.fold
      (fun flow_to flow_from escaping ->
         let alias_to =
           match Variable.Map.find flow_to dom with
           | exception Not_found -> flow_to
           | v -> v
         in
         let new_escaping =
           Variable.Set.fold
             (fun flow_from escaping ->
                let alias_from =
                  match Variable.Map.find flow_from dom with
                  | exception Not_found -> flow_from
                  | v -> v
                in
                let is_escaping = not (Variable.equal alias_to alias_from) in
                if is_escaping
                then Variable.Set.add alias_from escaping
                else escaping)
             flow_from Variable.Set.empty
         in
         (* Format.printf "From %a to %a@." Variable.Set.print flow_from
          *   Variable.print flow_to; *)
         (* if not (Variable.Set.is_empty new_escaping)
          * then
          *   Format.printf "Escaping by alias %a@." Variable.Set.print
          *     new_escaping; *)
         Variable.Set.union escaping new_escaping)
      dom_graph.graph Variable.Set.empty
  in
  let ref_prim_escaping (prim : T.Mutable_prim.t) =
    match prim with
    | Make_block (_, _, _, args) -> Simple.List.free_names args
    | Block_set (_, _, _, _, value) -> Simple.free_names value
    | Block_load _ -> Name_occurrences.empty
  in
  let escaping_by_use (elt : T.Continuation_info.t) =
    let add_name_occurrences occurrences init =
      Name_occurrences.fold_variables occurrences ~init
        ~f:(fun escaping var ->
            let escaping =
              match Variable.Map.find var dom with
              | exception Not_found -> escaping
              | var -> Variable.Set.add var escaping
            in
            Variable.Set.add var escaping)
    in
    let escaping =
      add_name_occurrences elt.used_in_handler Variable.Set.empty
    in
    let escaping =
      (* TODO: filter out deps when _name is not in required_names *)
      Name.Map.fold
        (fun _name deps escaping -> add_name_occurrences deps escaping)
        elt.bindings escaping
    in
    let escaping =
      Value_slot.Map.fold
        (fun _value_slot map escaping ->
           Name.Map.fold
             (fun _closure_name values_in_env escaping ->
                add_name_occurrences values_in_env escaping)
             map escaping)
        elt.value_slots escaping
    in
    let escaping =
      List.fold_left
        (fun escaping T.Mutable_let_prim.{ prim; _ } ->
           add_name_occurrences (ref_prim_escaping prim) escaping)
        escaping elt.mutable_let_prims_rev
    in
    escaping
  in
  let escaping_by_use =
    Continuation.Map.fold
      (fun _cont elt escaping ->
         Variable.Set.union (escaping_by_use elt) escaping)
      source_info.map Variable.Set.empty
  in
  (* if not (Variable.Set.is_empty escaping_by_use)
   * then Format.printf "Escaping by use %a@." Variable.Set.print escaping_by_use; *)
  let escaping_by_return =
    Continuation.Map.fold
      (fun _cont (elt : T.Continuation_info.t) escaping ->
         let add_escaping cont escaping =
           match Continuation.Map.find cont elt.apply_cont_args with
           | exception Not_found -> escaping
           | apply_cont_args ->
             Variable.Set.fold
               (fun var escaping ->
                  let escaping =
                    match Variable.Map.find var dom with
                    | exception Not_found -> escaping
                    | var -> Variable.Set.add var escaping
                  in
                  Variable.Set.add var escaping)
               (free_names_of_apply_cont_args apply_cont_args)
               escaping
         in
         let escaping = add_escaping return_continuation escaping in
         add_escaping exn_continuation escaping)
      source_info.map Variable.Set.empty
  in
  (* if not (Variable.Set.is_empty escaping_by_return)
   * then
   *   Format.printf "Escaping by return %a@." Variable.Set.print
   *     escaping_by_return; *)
  Variable.Set.union escaping_by_alias
    (Variable.Set.union escaping_by_return escaping_by_use)

let non_escaping_makeblocks ~escaping ~source_info =
  Continuation.Map.fold
    (fun _cont (elt : T.Continuation_info.t) map ->
       List.fold_left
         (fun map T.Mutable_let_prim.{ bound_var = var; prim; _ } ->
            match prim with
            | Make_block (kind, Mutable, _, args) ->
              (* TODO: remove the mutable constraint, there is no reason to
                 restrict to it. This is only there to test on the mutable
                 cases *)
              if Variable.Set.mem var escaping
              then map
              else
                let kinds =
                  match kind with
                  | Naked_floats ->
                    List.map
                      (fun _ -> Flambda_kind.With_subkind.naked_float)
                      args
                  | Values (_, kinds) -> kinds
                in
                Variable.Map.add var kinds map
            | Make_block (_, (Immutable | Immutable_unique), _, _)
            | Block_load _ | Block_set _ ->
              map)
         map elt.mutable_let_prims_rev)
    source_info.T.Acc.map Variable.Map.empty

let prims_using_ref ~non_escaping_refs ~dom prim =
  match (prim : T.Mutable_prim.t) with
  | Make_block _ -> Variable.Set.empty
  | Block_set (_, _, block, _, _) | Block_load { block; _ } ->
    let block =
      match Variable.Map.find block dom with
      | exception Not_found -> block
      | block -> block
    in
    if Variable.Map.mem block non_escaping_refs
    then Variable.Set.singleton block
    else Variable.Set.empty

let continuations_using_refs ~non_escaping_refs ~dom ~(source_info : T.Acc.t) =
  Continuation.Map.mapi
    (fun _cont (elt : T.Continuation_info.t) ->
       let used =
         List.fold_left
           (fun used_ref T.Mutable_let_prim.{ prim; _ } ->
              Variable.Set.union used_ref
                (prims_using_ref ~non_escaping_refs ~dom prim))
           Variable.Set.empty elt.mutable_let_prims_rev
       in
       if (not (Variable.Set.is_empty used)) && ref_to_var_debug
       then
         Format.printf "Cont using ref %a %a@." Continuation.print _cont
           Variable.Set.print used;
       used)
    source_info.map

let continuations_defining_refs ~non_escaping_refs ~(source_info : T.Acc.t) =
  Continuation.Map.mapi
    (fun _cont (elt : T.Continuation_info.t) ->
       let defined =
         List.fold_left
           (fun defined_refs T.Mutable_let_prim.{ bound_var = var; _ } ->
              if Variable.Map.mem var non_escaping_refs
              then Variable.Set.add var defined_refs
              else defined_refs)
           Variable.Set.empty elt.mutable_let_prims_rev
       in
       if (not (Variable.Set.is_empty defined)) && ref_to_var_debug
       then
         Format.printf "Cont defining ref %a %a@." Continuation.print _cont
           Variable.Set.print defined;
       defined)
    source_info.map

let continuations_with_live_ref ~non_escaping_refs ~dom ~source_info ~callers
  =
  let continuations_defining_refs =
    continuations_defining_refs ~non_escaping_refs ~source_info
  in
  let continuations_using_refs =
    continuations_using_refs ~non_escaping_refs ~dom ~source_info
  in
  (* TODO factorise fixpoint code *)
  let q_is_empty, pop, push =
    let q = Queue.create () in
    let q_s = ref Continuation.Set.empty in
    ( (fun () -> Queue.is_empty q),
      (fun () ->
         let k = Queue.pop q in
         q_s := Continuation.Set.remove k !q_s;
         k),
      fun k ->
        if not (Continuation.Set.mem k !q_s)
        then (
          Queue.add k q;
          q_s := Continuation.Set.add k !q_s) )
  in
  let continuations_using_refs_but_not_defining_them =
    Continuation.Map.merge (fun _cont defined used ->
        match defined, used with
        | None, None -> assert false (* *)
        | None, Some _used ->
          Misc.fatal_errorf "In Data_flow: incomplete map of continuation defining refs"
        | Some _defined, None ->
          Misc.fatal_errorf "In Data_flow: incomplete map of continuation using refs"
        | Some defined, Some used ->
          Some (Variable.Set.diff used defined)
      ) continuations_defining_refs continuations_using_refs
  in
  let () =
    Continuation.Map.iter
      (fun cont used -> if not (Variable.Set.is_empty used) then push cont)
      continuations_using_refs_but_not_defining_them
  in
  let res = ref continuations_using_refs_but_not_defining_them in
  while not (q_is_empty ()) do
    let k = pop () in
    (* let elt = Continuation.Map.find k source_info.map in *)
    let used_refs = Continuation.Map.find k !res in
    let callers =
      match Continuation.Map.find k callers with
      | exception Not_found ->
        Misc.fatal_errorf "Callers not found for: %a" Continuation.print k
      | callers -> callers
    in
    Continuation.Set.iter
      (fun caller ->
         let defined_refs =
           Continuation.Map.find caller continuations_defining_refs
         in
         let old_using_refs = Continuation.Map.find caller !res in
         let new_using_refs =
           Variable.Set.diff
             (Variable.Set.union old_using_refs used_refs)
             defined_refs
         in
         if not (Variable.Set.equal old_using_refs new_using_refs)
         then (
           res := Continuation.Map.add caller new_using_refs !res;
           push caller))
      callers
  done;
  !res

let list_to_int_map l =
  let _, map =
    List.fold_left
      (fun (i, fields) elt ->
         let fields = Numeric_types.Int.Map.add i elt fields in
         i + 1, fields)
      (0, Numeric_types.Int.Map.empty)
      l
  in
  map

let int_map_to_list m = List.map snd (Numeric_types.Int.Map.bindings m)

module Fold_prims = struct
  type env =
    { bindings : Simple.t Numeric_types.Int.Map.t Variable.Map.t;
      (* non escaping references bindings *)
      rewrites : Named_rewrite.t Named_rewrite_id.Map.t
    }

  let apply_prim ~dom ~non_escaping_refs env rewrite_id var (prim : T.Mutable_prim.t)
    =
    match prim with
    | Block_load { block; field; _} -> (
        let block =
          match Variable.Map.find block dom with
          | exception Not_found -> block
          | block -> block
        in
        match Variable.Map.find block env.bindings with
        | exception Not_found -> env
        | fields ->
          let bound_to = Numeric_types.Int.Map.find field fields in
          let rewrite =
            Named_rewrite.prim_rewrite @@
            Named_rewrite.Prim_rewrite.replace_by_binding ~var ~bound_to
          in
          { env with
            rewrites = Named_rewrite_id.Map.add rewrite_id rewrite env.rewrites
          })
    | Block_set (_, _, block, field, value) -> (
        let block =
          match Variable.Map.find block dom with
          | exception Not_found -> block
          | block -> block
        in
        match Variable.Map.find block env.bindings with
        | exception Not_found -> env
        | fields ->
          if ref_to_var_debug
          then Format.printf "Remove Block set %a@." Variable.print var;
          let rewrite =
            Named_rewrite.prim_rewrite @@
            Named_rewrite.Prim_rewrite.remove_prim
          in
          let fields = Numeric_types.Int.Map.add field value fields in
          { bindings = Variable.Map.add block fields env.bindings;
            rewrites = Named_rewrite_id.Map.add rewrite_id rewrite env.rewrites
          })
    | Make_block (_, _, _, values) ->
      if not (Variable.Map.mem var non_escaping_refs)
      then env
      else
        let () =
          if ref_to_var_debug
          then Format.printf "Remove Makeblock %a@." Variable.print var
        in
        let rewrite =
          Named_rewrite.prim_rewrite @@
          Named_rewrite.Prim_rewrite.remove_prim
        in
        let fields = list_to_int_map values in
        { bindings = Variable.Map.add var fields env.bindings;
          rewrites = Named_rewrite_id.Map.add rewrite_id rewrite env.rewrites
        }

  let init_env
      ~(non_escaping_refs : Flambda_kind.With_subkind.t list Variable.Map.t)
      ~(refs_needed : Variable.Set.t) ~rewrites =
    let env, params =
      Variable.Set.fold
        (fun ref_needed (env, params) ->
           let arity = Variable.Map.find ref_needed non_escaping_refs in
           let ref_params =
             List.mapi
               (fun i kind ->
                  let name = Variable.unique_name ref_needed in
                  let var = Variable.create (Printf.sprintf "%s_%i" name i) in
                  Bound_parameter.create var kind)
               arity
           in
           let env =
             let fields =
               list_to_int_map
                 (List.map
                    (fun bp -> Simple.var (Bound_parameter.var bp))
                    ref_params)
             in
             { env with
               bindings = Variable.Map.add ref_needed fields env.bindings
             }
           in
           env, List.rev_append ref_params params)
        refs_needed
        ({ bindings = Variable.Map.empty; rewrites }, [])
    in
    env, List.rev params

  let append_int_map i1 i2 =
    if Numeric_types.Int.Map.is_empty i1
    then i2
    else
      let max, _ = Numeric_types.Int.Map.max_binding i1 in
      let shifted_i2 =
        Numeric_types.Int.Map.map_keys (fun i -> i + max + 1) i2
      in
      Numeric_types.Int.Map.disjoint_union i1 shifted_i2

  let do_stuff
      ~(non_escaping_refs : Flambda_kind.With_subkind.t list Variable.Map.t)
      ~continuations_with_live_ref ~dom ~(source_info : T.Acc.t) =
    let rewrites = ref Named_rewrite_id.Map.empty in
    let extra_params_and_args =
      Continuation.Map.mapi
        (fun cont (refs_needed : Variable.Set.t) ->
           let elt = Continuation.Map.find cont source_info.map in
           let env, extra_ref_params =
             init_env ~non_escaping_refs ~refs_needed ~rewrites:!rewrites
           in
           let env =
             List.fold_left
               (fun env T.Mutable_let_prim.{ named_rewrite_id; bound_var; prim; } ->
                  apply_prim ~dom ~non_escaping_refs env named_rewrite_id bound_var prim)
               env
               (List.rev elt.mutable_let_prims_rev)
           in
           rewrites := env.rewrites;
           let refs_params_to_add cont rewrites =
             Apply_cont_rewrite_id.Map.map
               (fun _args ->
                  match
                    Continuation.Map.find cont continuations_with_live_ref
                  with
                  | exception Not_found -> Numeric_types.Int.Map.empty
                  | refs_needed ->
                    let extra_args =
                      Variable.Set.fold
                        (fun ref_needed extra_args ->
                           let args =
                             Variable.Map.find ref_needed env.bindings
                           in
                           append_int_map extra_args args)
                        refs_needed Numeric_types.Int.Map.empty
                    in
                    extra_args)
               rewrites
           in
           let new_apply_cont_args =
             Continuation.Map.mapi refs_params_to_add elt.apply_cont_args
           in
           extra_ref_params, new_apply_cont_args)
        continuations_with_live_ref
    in
    extra_params_and_args, !rewrites
end

(* TODO: For all continuations with live ref, add parameters for every field
   of every live ref.

   for all continuations, fold over the ref_prims (after list rev) to
   propagate the names of the fields at each block_set and record aliases for
   block_load.

   for evey apply cont to those continuations, add the corresponding arguments
   using the last names of the fields. *)

let create ~(dom : Dominator_graph.alias_map) ~(dom_graph : Dominator_graph.t)
    ~(source_info : T.Acc.t)
    ~(callers : Continuation.Set.t Continuation.Map.t) ~return_continuation
    ~exn_continuation : t =
  let escaping =
    escaping ~dom ~dom_graph ~source_info ~return_continuation
      ~exn_continuation
  in
  (* Format.printf "@[<hov 2>Escaping vars@ %a@]@." Variable.Set.print
     escaping; *)
  let non_escaping_refs = non_escaping_makeblocks ~escaping ~source_info in
  if (not (Variable.Map.is_empty non_escaping_refs)) && ref_to_var_debug
  then
    Format.printf "Non escaping makeblocks %a@."
      (Variable.Map.print (fun ppf kinds ->
           Format.fprintf ppf "[%a]"
             (Format.pp_print_list ~pp_sep:Format.pp_print_space
                Flambda_kind.With_subkind.print)
             kinds))
      non_escaping_refs;
  let continuations_with_live_ref =
    continuations_with_live_ref ~non_escaping_refs ~dom ~source_info ~callers
  in
  (* Format.printf "@[<hov 2>Cont using refs:@ %a@]@."
   *   (Continuation.Map.print Variable.Set.print)
   *   continuations_with_live_ref; *)
  let toplevel_used =
    Continuation.Map.find source_info.dummy_toplevel_cont
      continuations_with_live_ref
  in
  if not (Variable.Set.is_empty toplevel_used)
  then
    Misc.fatal_errorf
      "Toplevel continuation cannot have needed extra argument for ref: %a@."
      Variable.Set.print toplevel_used;
  let extra_ref_params_and_args, rewrites =
    Fold_prims.do_stuff ~dom ~source_info ~continuations_with_live_ref
      ~non_escaping_refs
  in
  { extra_ref_params_and_args;
    non_escaping_makeblock = non_escaping_refs;
    continuations_with_live_ref;
    rewrites
  }

let pp_node { non_escaping_makeblock = _; continuations_with_live_ref; _ } ppf
    cont =
  match Continuation.Map.find cont continuations_with_live_ref with
  | exception Not_found -> ()
  | live_refs -> Format.fprintf ppf " %a" Variable.Set.print live_refs

let add_to_extra_params_and_args result =
  let epa = Continuation.Map.empty in
  let extra_ref_args :
    EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t list Continuation.Map.t =
    Continuation.Map.fold
      (fun _caller_cont (_extra_params, extra_args) all_extra_args ->
         Continuation.Map.fold
           (fun callee_cont
             (caller_extra_args :
                Simple.t Numeric_types.Int.Map.t Apply_cont_rewrite_id.Map.t)
             (all_extra_args :
                EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t list
                  Continuation.Map.t) :
             EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t list
             Continuation.Map.t ->
             Continuation.Map.update callee_cont
               (fun previous_extra_args ->
                  let previous_extra_args :
                    EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t list =
                    match previous_extra_args with
                    | None -> (
                        match
                          Continuation.Map.find callee_cont
                            result.extra_ref_params_and_args
                        with
                        | exception Not_found -> []
                        | extra_params, _ ->
                          List.map
                            (fun _ -> Apply_cont_rewrite_id.Map.empty)
                            extra_params)
                    | Some extra_args -> extra_args
                  in
                  let extra_args =
                    Apply_cont_rewrite_id.Map.fold
                      (fun rewrite_id (args : Simple.t Numeric_types.Int.Map.t)
                        (previous_extra_args :
                           EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t list) ->
                        let args = int_map_to_list args in
                        List.map2
                          (fun arg previous_extra_args ->
                             Apply_cont_rewrite_id.Map.add rewrite_id
                               (EPA.Extra_arg.Already_in_scope arg)
                               previous_extra_args)
                          args previous_extra_args)
                      caller_extra_args previous_extra_args
                  in
                  Some extra_args)
               all_extra_args)
           extra_args all_extra_args)
      result.extra_ref_params_and_args Continuation.Map.empty
  in
  let epa =
    Continuation.Map.fold
      (fun cont extra_args epa ->
         let extra_params =
           match
             Continuation.Map.find cont result.extra_ref_params_and_args
           with
           | exception Not_found -> []
           | extra_params, _ -> extra_params
         in
         Continuation.Map.update cont
           (fun epa_for_cont ->
              let epa_for_cont =
                match epa_for_cont with None -> EPA.empty | Some epa -> epa
              in
              let epa_for_cont =
                List.fold_left2
                  (fun epa_for_cont extra_param extra_args ->
                     EPA.add epa_for_cont ~extra_param ~extra_args)
                  epa_for_cont extra_params extra_args
              in
              Some epa_for_cont)
           epa)
      extra_ref_args epa
  in
  epa

let make_result result =
  let additionnal_epa = add_to_extra_params_and_args result in
  let let_rewrites = result.rewrites in
  T.Mutable_unboxing_result.{ additionnal_epa; let_rewrites }


