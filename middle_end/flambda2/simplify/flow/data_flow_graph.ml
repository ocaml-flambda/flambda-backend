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

type t =
  { code_age_relation : Code_age_relation.t;
    name_to_name : Name.Set.t Name.Map.t;
    name_to_code_id : Code_id.Set.t Name.Map.t;
    code_id_to_name : Name.Set.t Code_id.Map.t;
    code_id_to_code_id : Code_id.Set.t Code_id.Map.t;
    unconditionally_used : Name.Set.t;
    code_id_unconditionally_used : Code_id.Set.t;
    is_toplevel : bool
  }

module Reachable = struct
  module Edge (Src_map : Container_types.Map) (Dst_set : Container_types.Set) =
  struct
    type src = Src_map.key

    type dst = Dst_set.elt

    let push ~(src : src) (enqueued : Dst_set.t) (queue : dst Queue.t)
        (graph : Dst_set.t Src_map.t) : Dst_set.t =
      let neighbours =
        match Src_map.find src graph with
        | exception Not_found -> Dst_set.empty
        | set -> set
      in
      let new_neighbours = Dst_set.diff neighbours enqueued in
      Dst_set.iter (fun dst -> Queue.push dst queue) new_neighbours;
      Dst_set.union enqueued new_neighbours
  end
  [@@inline]
  (* TODO check that this applied here *)

  module Name_Name_Edge = Edge (Name.Map) (Name.Set)
  module Name_Code_id_Edge = Edge (Name.Map) (Code_id.Set)
  module Code_id_Name_Edge = Edge (Code_id.Map) (Name.Set)
  module Code_id_Code_id_Edge = Edge (Code_id.Map) (Code_id.Set)

  (* breadth-first reachability analysis. *)
  let rec reachable_names t code_id_queue code_id_enqueued older_enqueued
      name_queue name_enqueued =
    match Queue.take name_queue with
    | exception Queue.Empty ->
      if t.is_toplevel
      then
        if Queue.is_empty code_id_queue
        then
          T.Data_flow_result.
            { required_names = name_enqueued;
              reachable_code_ids =
                Known
                  T.Reachable_code_ids.
                    { live_code_ids = code_id_enqueued;
                      ancestors_of_live_code_ids = older_enqueued
                    }
            }
        else
          reachable_code_ids t code_id_queue code_id_enqueued (Queue.create ())
            older_enqueued name_queue name_enqueued
      else
        T.Data_flow_result.
          { required_names = name_enqueued; reachable_code_ids = Unknown }
    | src ->
      let name_enqueued =
        Name_Name_Edge.push ~src name_enqueued name_queue t.name_to_name
      in
      let code_id_enqueued =
        Name_Code_id_Edge.push ~src code_id_enqueued code_id_queue
          t.name_to_code_id
      in
      reachable_names t code_id_queue code_id_enqueued older_enqueued name_queue
        name_enqueued

  and reachable_code_ids t code_id_queue code_id_enqueued older_queue
      older_enqueued name_queue name_enqueued =
    match Queue.take code_id_queue with
    | exception Queue.Empty ->
      if Queue.is_empty older_queue
      then
        reachable_names t code_id_queue code_id_enqueued older_enqueued
          name_queue name_enqueued
      else
        reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
          older_enqueued name_queue name_enqueued
    | src ->
      let name_enqueued =
        Code_id_Name_Edge.push ~src name_enqueued name_queue t.code_id_to_name
      in
      let code_id_enqueued =
        Code_id_Code_id_Edge.push ~src code_id_enqueued code_id_queue
          t.code_id_to_code_id
      in
      let older_enqueued =
        if Code_id.Set.mem src older_enqueued
        then older_enqueued
        else (
          Queue.push src older_queue;
          Code_id.Set.add src older_enqueued)
      in
      reachable_code_ids t code_id_queue code_id_enqueued older_queue
        older_enqueued name_queue name_enqueued

  and reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
      older_enqueued name_queue name_enqueued =
    match Queue.take older_queue with
    | exception Queue.Empty ->
      reachable_code_ids t code_id_queue code_id_enqueued older_queue
        older_enqueued name_queue name_enqueued
    | src -> (
      match Code_age_relation.get_older_version_of t.code_age_relation src with
      | None ->
        reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
          older_enqueued name_queue name_enqueued
      | Some dst ->
        if Code_id.Set.mem dst older_enqueued
        then (
          if Code_id.Set.mem dst code_id_enqueued
          then
            reachable_older_code_ids t code_id_queue code_id_enqueued
              older_queue older_enqueued name_queue name_enqueued
          else
            let code_id_enqueued = Code_id.Set.add dst code_id_enqueued in
            Queue.push dst code_id_queue;
            reachable_older_code_ids t code_id_queue code_id_enqueued
              older_queue older_enqueued name_queue name_enqueued)
        else
          let older_enqueued = Code_id.Set.add dst older_enqueued in
          reachable_older_code_ids t code_id_queue code_id_enqueued older_queue
            older_enqueued name_queue name_enqueued)
end

let empty code_age_relation is_toplevel =
  { code_age_relation;
    is_toplevel;
    name_to_name = Name.Map.empty;
    name_to_code_id = Name.Map.empty;
    code_id_to_name = Code_id.Map.empty;
    code_id_to_code_id = Code_id.Map.empty;
    unconditionally_used = Name.Set.empty;
    code_id_unconditionally_used = Code_id.Set.empty
  }

let print ppf
    { is_toplevel;
      name_to_name;
      name_to_code_id;
      code_id_to_name;
      code_id_to_code_id;
      code_age_relation;
      unconditionally_used;
      code_id_unconditionally_used
    } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(is_toplevel %b)@]@ @[<hov 1>(code_age_relation@ \
     %a)@]@ @[<hov 1>(name_to_name@ %a)@]@ @[<hov 1>(name_to_code_id@ %a)@]@ \
     @[<hov 1>(code_id_to_name@ %a)@]@ @[<hov 1>(code_id_to_code_id@ %a)@]@ \
     @[<hov 1>(unconditionally_used@ %a)@]@ @[<hov \
     1>(code_id_unconditionally_used@ %a)@])@]"
    is_toplevel Code_age_relation.print code_age_relation
    (Name.Map.print Name.Set.print)
    name_to_name
    (Name.Map.print Code_id.Set.print)
    name_to_code_id
    (Code_id.Map.print Name.Set.print)
    code_id_to_name
    (Code_id.Map.print Code_id.Set.print)
    code_id_to_code_id Name.Set.print unconditionally_used Code_id.Set.print
    code_id_unconditionally_used

(* *)
let fold_name_occurrences name_occurrences ~init ~names ~code_ids =
  Name_occurrences.fold_names name_occurrences ~f:names
    ~init:(code_ids init (Name_occurrences.code_ids name_occurrences))

(* Some auxiliary functions *)
let add_code_id_dep ~src ~(dst : Code_id.Set.t) ({ name_to_code_id; _ } as t) =
  let name_to_code_id =
    Name.Map.update src
      (function
        | None -> if Code_id.Set.is_empty dst then None else Some dst
        | Some old ->
          Misc.fatal_errorf "Same name bound multiple times: %a -> %a, %a"
            Name.print src Code_id.Set.print old Code_id.Set.print dst)
      name_to_code_id
  in
  { t with name_to_code_id }

let add_dependency ~src ~dst ({ name_to_name; _ } as t) =
  let name_to_name =
    Name.Map.update src
      (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set))
      name_to_name
  in
  { t with name_to_name }

let add_code_id_dependency ~src ~dst ({ code_id_to_name; _ } as t) =
  let code_id_to_name =
    Code_id.Map.update src
      (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set))
      code_id_to_name
  in
  { t with code_id_to_name }

let add_code_id_to_code_id ~src ~dst ({ code_id_to_code_id; _ } as t) =
  let code_id_to_code_id =
    Code_id.Map.update src
      (function
        | None -> if Code_id.Set.is_empty dst then None else Some dst
        | Some old ->
          Misc.fatal_errorf "Same code_id bound multiple times: %a -> %a, %a"
            Code_id.print src Code_id.Set.print old Code_id.Set.print dst)
      code_id_to_code_id
  in
  { t with code_id_to_code_id }

let add_name_occurrences name_occurrences
    ({ unconditionally_used; code_id_unconditionally_used; _ } as t) =
  let unconditionally_used =
    Name_occurrences.fold_names name_occurrences
      ~f:(fun set name -> Name.Set.add name set)
      ~init:unconditionally_used
  in
  let code_id_unconditionally_used =
    Code_id.Set.union
      (Name_occurrences.code_ids name_occurrences)
      code_id_unconditionally_used
  in
  { t with unconditionally_used; code_id_unconditionally_used }

let add_continuation_info map ~return_continuation ~exn_continuation
    ~used_value_slots _
    T.Continuation_info.
      { apply_cont_args;
        (* CR pchambart: properly follow dependencies in exception extra args.
           They are currently marked as always used, so it is correct, but not
           optimal *)
        used_in_handler;
        bindings;
        direct_aliases;
        mutable_let_prims_rev;
        defined = _;
        code_ids;
        value_slots;
        continuation = _;
        recursive = _;
        is_exn_handler = _;
        parent_continuation = _;
        params = _
      } t =
  (* Add the vars used in the handler *)
  let t = add_name_occurrences used_in_handler t in
  (* Add the dependencies created by closures vars in envs *)
  let is_value_slot_used =
    match (used_value_slots : _ Or_unknown.t) with
    | Unknown -> fun _ -> true
    | Known used_value_slots ->
      Name_occurrences.value_slot_is_used_or_imported used_value_slots
  in
  let t =
    Value_slot.Map.fold
      (fun value_slot map t ->
        if not (is_value_slot_used value_slot)
        then t
        else
          Name.Map.fold
            (fun closure_name values_in_env t ->
              Name_occurrences.fold_names
                ~f:(fun t value_in_env ->
                  add_dependency ~src:closure_name ~dst:value_in_env t)
                values_in_env ~init:t)
            map t)
      value_slots t
  in
  (* Build the graph of dependencies between names *)
  let t =
    Name.Map.fold
      (fun src name_occurrences graph ->
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_dep ~src ~dst t))
      bindings t
  in
  let t =
    Variable.Map.fold
      (fun src simple graph ->
        let src = Name.var src in
        let name_occurrences = Simple.free_names simple in
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_dep ~src ~dst t))
      direct_aliases t
  in
  let t =
    List.fold_left
      (fun t
           T.Mutable_let_prim.
             { bound_var; prim; original_prim; named_rewrite_id = _ } ->
        let src = Name.var bound_var in
        match prim with
        | Is_int _ | Get_tag _ | Make_block _ | Block_load _ ->
          Name_occurrences.fold_names
            ~f:(fun t dst -> add_dependency ~src ~dst t)
            (Flambda_primitive.free_names original_prim)
            ~init:t
        | Block_set _ ->
          add_name_occurrences (Flambda_primitive.free_names original_prim) t)
      t mutable_let_prims_rev
  in
  let t =
    Code_id.Map.fold
      (fun src name_occurrences graph ->
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_code_id_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_to_code_id ~src ~dst t))
      code_ids t
  in
  (* Build the graph of dependencies between continuation parameters and
     arguments. *)
  Continuation.Map.fold
    (fun k rewrite_ids t ->
      if Continuation.equal return_continuation k
         || Continuation.equal exn_continuation k
      then
        Apply_cont_rewrite_id.Map.fold
          (fun _rewrite_id args t ->
            Numeric_types.Int.Map.fold
              (fun _ (cont_arg : T.Cont_arg.t) t ->
                match cont_arg with
                | Simple simple ->
                  add_name_occurrences (Simple.free_names simple) t
                | New_let_binding (var, prim_free_names) ->
                  add_name_occurrences
                    (Name_occurrences.union prim_free_names
                       (Name_occurrences.singleton_variable var Name_mode.normal))
                    t
                | Function_result -> t)
              args t)
          rewrite_ids t
      else
        let params =
          match Continuation.Map.find k map with
          | elt ->
            Array.of_list (Bound_parameters.vars elt.T.Continuation_info.params)
          | exception Not_found ->
            Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
              Continuation.print k
        in
        Apply_cont_rewrite_id.Map.fold
          (fun rewrite_id args t ->
            let correct_number_of_arguments =
              match Numeric_types.Int.Map.max_binding args with
              | exception Not_found -> Array.length params = 0
              | max_arg, _ -> max_arg = Array.length params - 1
            in
            if not correct_number_of_arguments
            then
              Misc.fatal_errorf
                "Mismatched number of argument and params for %a at rewrite_id \
                 %a"
                Continuation.print k Apply_cont_rewrite_id.print rewrite_id;
            Numeric_types.Int.Map.fold
              (fun i (cont_arg : T.Cont_arg.t) t ->
                (* Note on the direction of the edge:

                   We later do a reachability analysis to compute the transitive
                   closure of the used variables.

                   Therefore an edge from src to dst means: if src is used, then
                   dst is also used.

                   Applied here, this means : if the param of a continuation is
                   used, then any argument provided for that param is also used.
                   The other way wouldn't make much sense. *)
                let src = Name.var params.(i) in
                match cont_arg with
                | Simple simple ->
                  Name_occurrences.fold_names (Simple.free_names simple) ~init:t
                    ~f:(fun t dst -> add_dependency ~src ~dst t)
                | New_let_binding (var, prim_free_names) ->
                  let t = add_dependency ~src ~dst:(Name.var var) t in
                  Name_occurrences.fold_names prim_free_names ~init:t
                    ~f:(fun t dst -> add_dependency ~src:(Name.var var) ~dst t)
                | Function_result -> t)
              args t)
          rewrite_ids t)
    apply_cont_args t

let create ~return_continuation ~exn_continuation ~code_age_relation
    ~used_value_slots map =
  (* Build the dependencies using the regular params and args of continuations,
     and the let-bindings in continuations handlers. *)
  let is_toplevel =
    match (used_value_slots : _ Or_unknown.t) with
    | Known _ -> true
    | Unknown -> false
  in
  let t =
    Continuation.Map.fold
      (add_continuation_info map ~return_continuation ~exn_continuation
         ~used_value_slots)
      map
      (empty code_age_relation is_toplevel)
  in
  t

let required_names
    ({ code_age_relation = _;
       name_to_name = _;
       name_to_code_id = _;
       code_id_to_name = _;
       code_id_to_code_id = _;
       unconditionally_used;
       code_id_unconditionally_used;
       is_toplevel
     } as t) =
  let name_queue = Queue.create () in
  Name.Set.iter (fun v -> Queue.push v name_queue) unconditionally_used;
  let code_id_queue = Queue.create () in
  if is_toplevel
  then
    Code_id.Set.iter
      (fun v -> Queue.push v code_id_queue)
      code_id_unconditionally_used;
  Reachable.reachable_names t code_id_queue code_id_unconditionally_used
    Code_id.Set.empty name_queue unconditionally_used
