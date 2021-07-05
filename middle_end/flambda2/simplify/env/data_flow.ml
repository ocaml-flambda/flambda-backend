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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Helper module *)
(* ************* *)

module Reachable_code_ids = struct

  type t = {
    live_code_ids : Code_id.Set.t;
    ancestors_of_live_code_id : Code_id.Set.t;
  }

  let print ppf { live_code_ids; ancestors_of_live_code_id; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(live_code_ids@ %a)@]@ \
        @[<hov 1>(ancestors_of_live_code_id@ %a)@]\
      )@]"
      Code_id.Set.print live_code_ids
      Code_id.Set.print ancestors_of_live_code_id

end

(* Typedefs *)
(* ******** *)

(* CR chambart/gbury: get rid of Name_occurences everywhere,
                      this is not small while we need only the names *)
type elt = {
  continuation : Continuation.t;
  params : Variable.t list;
  used_in_handler : Name_occurrences.t;
  apply_result_conts : Continuation.Set.t;
  bindings : Name_occurrences.t Name.Map.t;
  code_ids : Name_occurrences.t Code_id.Map.t;
  closure_envs : Name_occurrences.t Name.Map.t Var_within_closure.Map.t;
  apply_cont_args :
    Name_occurrences.t Numeric_types.Int.Map.t Continuation.Map.t;
}

type t = {
  stack : elt list;
  map : elt Continuation.Map.t;
  extra : Continuation_extra_params_and_args.t Continuation.Map.t;
}

type result = {
  required_names : Name.Set.t;
  reachable_code_ids : Reachable_code_ids.t;
}

(* Print *)
(* ***** *)

let print_elt ppf
      { continuation; params; used_in_handler; apply_result_conts;
        bindings; code_ids; closure_envs; apply_cont_args; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(continuation %a)@]@ \
      @[<hov 1>(params %a)@]@ \
      @[<hov 1>(used_in_handler %a)@]@ \
      @[<hov 1>(apply_result_conts %a)@]@ \
      @[<hov 1>(bindings %a)@]@ \
      @[<hov 1>(code_ids %a)@]@ \
      @[<hov 1>(closure_envs %a)@]@ \
      @[<hov 1>(apply_cont_args %a)@]\
      )@]"
    Continuation.print continuation
    Variable.print_list params
    Name_occurrences.print used_in_handler
    Continuation.Set.print apply_result_conts
    (Name.Map.print Name_occurrences.print) bindings
    (Code_id.Map.print Name_occurrences.print) code_ids
    (Var_within_closure.Map.print (Name.Map.print Name_occurrences.print)) closure_envs
    (Continuation.Map.print (Numeric_types.Int.Map.print Name_occurrences.print))
    apply_cont_args

let print_stack ppf stack =
  Format.fprintf ppf "@[<v 1>(%a)@]"
    (Format.pp_print_list print_elt ~pp_sep:Format.pp_print_space)
    stack

let print_map ppf map =
  Continuation.Map.print print_elt ppf map

let print_extra ppf extra =
  Continuation.Map.print Continuation_extra_params_and_args.print ppf extra

let print ppf { stack; map; extra } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(stack %a)@]@ \
      @[<hov 1>(map %a)@]@ \
      @[<hov 1>(extra %a)@]\
      )@]"
    print_stack stack
    print_map map
    print_extra extra

let _print_result ppf { required_names; reachable_code_ids; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(required_names@ %a)@]@ \
        @[<hov 1>(reachable_code_ids@ %a)@]\
        )@]"
      Name.Set.print required_names
      Reachable_code_ids.print reachable_code_ids


(* Creation *)
(* ******** *)

let empty = {
  stack = [];
  map = Continuation.Map.empty;
  extra = Continuation.Map.empty;
}

(* Updates *)
(* ******* *)

let add_extra_params_and_args cont extra t =
  let extra =
    Continuation.Map.update cont (function
      | Some _ ->
        Misc.fatal_errorf "Continuation extended a second time"
      | None -> Some extra
    ) t.extra
  in
  { t with extra; }

let enter_continuation continuation params t =
  let elt = {
    continuation; params;
    bindings = Name.Map.empty;
    code_ids = Code_id.Map.empty;
    closure_envs = Var_within_closure.Map.empty;
    used_in_handler = Name_occurrences.empty;
    apply_cont_args = Continuation.Map.empty;
    apply_result_conts = Continuation.Set.empty;
  }
  in
  { t with stack = elt :: t.stack; }

let init_toplevel continuation params _t =
  enter_continuation continuation params empty

let exit_continuation cont t =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | ({ continuation; _ } as elt) :: stack ->
    assert (Continuation.equal cont continuation);
    let map = Continuation.Map.add cont elt t.map in
    { t with stack; map; }

let update_top_of_stack ~t ~f =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | elt :: stack -> { t with stack = f elt :: stack; }

let record_var_binding var name_occurrences ~generate_phantom_lets t =
  update_top_of_stack ~t ~f:(fun elt ->
    let bindings =
      Name.Map.update (Name.var var) (function
        | None -> Some name_occurrences
        | Some _ ->
            Misc.fatal_errorf
              "The following variable has been bound twice: %a"
              Variable.print var
      ) elt.bindings
    in
    let used_in_handler =
      if Variable.user_visible var && generate_phantom_lets then
        Name_occurrences.add_variable elt.used_in_handler var Name_mode.phantom
      else
        elt.used_in_handler
    in
    { elt with bindings; used_in_handler; }
  )

let record_symbol_binding symbol name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let bindings =
      Name.Map.update (Name.symbol symbol) (function
        | None -> Some name_occurrences
        | Some _ ->
            Misc.fatal_errorf
              "The following symbol has been bound twice: %a"
              Symbol.print symbol
      ) elt.bindings
    in
    { elt with bindings; }
  )

let record_code_id_binding code_id name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let code_ids =
      Code_id.Map.update code_id (function
        | None -> Some name_occurrences
        | Some _ ->
            Misc.fatal_errorf
              "The following code_id has been bound twice: %a"
              Code_id.print code_id
      ) elt.code_ids
    in
    { elt with code_ids; }
  )

let record_closure_element_binding src closure_var dst t =
  update_top_of_stack ~t ~f:(fun elt ->
    let closure_envs =
      Var_within_closure.Map.update closure_var (function
        | None -> Some (Name.Map.singleton src dst)
        | Some map ->
          Some
            (Name.Map.update src (function
               | None -> Some dst
               | Some dst' -> Some (Name_occurrences.union dst dst'))
               map)
      ) elt.closure_envs
    in
    { elt with closure_envs }
  )

let add_used_in_current_handler name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let used_in_handler =
      Name_occurrences.union elt.used_in_handler name_occurrences
    in
    { elt with used_in_handler; }
  )

let add_apply_result_cont k t =
  update_top_of_stack ~t ~f:(fun elt ->
    let apply_result_conts = Continuation.Set.add k elt.apply_result_conts in
    { elt with apply_result_conts; }
  )

let add_apply_cont_args cont arg_name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
    let apply_cont_args =
      Continuation.Map.update cont (fun map_opt ->
        let map = Option.value ~default:Numeric_types.Int.Map.empty map_opt in
        let map, _ = List.fold_left (fun (map, i) name_occurrences ->
          let map =
            Numeric_types.Int.Map.update i (fun old_opt ->
              let old = Option.value ~default:Name_occurrences.empty old_opt in
              Some (Name_occurrences.union old name_occurrences)
            ) map
          in
          map, i + 1
          ) (map, 0) arg_name_occurrences
        in
        Some map
      ) elt.apply_cont_args
    in
    { elt with apply_cont_args; }
  )

(* Dependency graph *)
(* **************** *)

module Dependency_graph = struct

  type t = {
    code_age_relation : Code_age_relation.t;
    name_to_name : Name.Set.t Name.Map.t;
    name_to_code_id : Code_id.Set.t Name.Map.t;
    code_id_to_name : Name.Set.t Code_id.Map.t;
    code_id_to_code_id : Code_id.Set.t Code_id.Map.t;
    unconditionally_used : Name.Set.t;
    code_id_unconditionally_used : Code_id.Set.t;
  }

  module Reachable = struct
    module Edge (Src_map : Map.S) (Dst_set : Set.S) = struct
      type src = Src_map.key
      type dst = Dst_set.elt
      let push ~(src:src)
            (enqueued : Dst_set.t)
            (queue : dst Queue.t)
            (graph : Dst_set.t Src_map.t) : Dst_set.t =
        let neighbours =
          match Src_map.find src graph with
          | exception Not_found -> Dst_set.empty
          | set -> set
        in
        let new_neighbours = Dst_set.diff neighbours enqueued in
        Dst_set.iter (fun dst -> Queue.push dst queue) new_neighbours;
        Dst_set.union enqueued new_neighbours
    end [@@inline] (* TODO check that this applied here *)

    module Name_Name_Edge = Edge(Name.Map)(Name.Set)
    module Name_Code_id_Edge = Edge(Name.Map)(Code_id.Set)
    module Code_id_Name_Edge = Edge(Code_id.Map)(Name.Set)
    module Code_id_Code_id_Edge = Edge(Code_id.Map)(Code_id.Set)

    (* breadth-first reachability analysis. *)
    let rec reachable_names t
              code_id_queue code_id_enqueued
              older_enqueued
              name_queue name_enqueued =
      match Queue.take name_queue with
      | exception Queue.Empty ->
        if Queue.is_empty code_id_queue then
          { required_names = name_enqueued;
            reachable_code_ids = {
              live_code_ids = code_id_enqueued;
              ancestors_of_live_code_id = older_enqueued;
            }; }
        else
          reachable_code_ids t
            code_id_queue code_id_enqueued
            (Queue.create ()) older_enqueued
            name_queue name_enqueued
      | src ->
        let name_enqueued = Name_Name_Edge.push ~src name_enqueued name_queue t.name_to_name in
        let code_id_enqueued = Name_Code_id_Edge.push ~src code_id_enqueued code_id_queue t.name_to_code_id in
        reachable_names t
          code_id_queue code_id_enqueued
          older_enqueued
          name_queue name_enqueued

    and reachable_code_ids t
              code_id_queue code_id_enqueued
              older_queue older_enqueued
              name_queue name_enqueued =
      match Queue.take code_id_queue with
      | exception Queue.Empty ->
        if Queue.is_empty older_queue then
          reachable_names t
            code_id_queue code_id_enqueued
            older_enqueued
            name_queue name_enqueued
        else
          reachable_older_code_ids t
            code_id_queue code_id_enqueued
            older_queue older_enqueued
            name_queue name_enqueued
      | src ->
        let name_enqueued =
          Code_id_Name_Edge.push ~src name_enqueued name_queue t.code_id_to_name
        in
        let code_id_enqueued =
          Code_id_Code_id_Edge.push ~src code_id_enqueued code_id_queue t.code_id_to_code_id
        in
        let older_enqueued =
          if Code_id.Set.mem src older_enqueued then older_enqueued
          else begin
            Queue.push src older_queue;
            Code_id.Set.add src older_enqueued
          end
        in
        reachable_code_ids t
          code_id_queue code_id_enqueued
          older_queue older_enqueued
          name_queue name_enqueued

      and reachable_older_code_ids t
            code_id_queue code_id_enqueued
            older_queue older_enqueued
            name_queue name_enqueued =
        match Queue.take older_queue with
        | exception Queue.Empty ->
          reachable_code_ids t
            code_id_queue code_id_enqueued
            older_queue older_enqueued
            name_queue name_enqueued
        | src ->
          begin match Code_age_relation.get_older_version_of t.code_age_relation src with
          | None ->
            reachable_older_code_ids t
              code_id_queue code_id_enqueued
              older_queue older_enqueued
              name_queue name_enqueued
          | Some dst ->
            if Code_id.Set.mem dst older_enqueued then begin
              if Code_id.Set.mem dst code_id_enqueued then
                reachable_older_code_ids t
                  code_id_queue code_id_enqueued
                  older_queue older_enqueued
                  name_queue name_enqueued
              else begin
                let code_id_enqueued = Code_id.Set.add dst code_id_enqueued in
                Queue.push dst code_id_queue;
                reachable_older_code_ids t
                  code_id_queue code_id_enqueued
                  older_queue older_enqueued
                  name_queue name_enqueued
              end
            end else
              let older_enqueued = Code_id.Set.add dst older_enqueued in
              reachable_older_code_ids t
                code_id_queue code_id_enqueued
                older_queue older_enqueued
                name_queue name_enqueued
          end

  end

  let empty code_age_relation = {
    code_age_relation;
    name_to_name = Name.Map.empty;
    name_to_code_id = Name.Map.empty;
    code_id_to_name = Code_id.Map.empty;
    code_id_to_code_id = Code_id.Map.empty;
    unconditionally_used = Name.Set.empty;
    code_id_unconditionally_used = Code_id.Set.empty;
  }

  let _print ppf { name_to_name; name_to_code_id; code_id_to_name; code_id_to_code_id;
                   code_age_relation; unconditionally_used; code_id_unconditionally_used} =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(code_age_relation@ %a)@]@ \
        @[<hov 1>(name_to_name@ %a)@]@ \
        @[<hov 1>(name_to_code_id@ %a)@]@ \
        @[<hov 1>(code_id_to_name@ %a)@]@ \
        @[<hov 1>(code_id_to_code_id@ %a)@]@ \
        @[<hov 1>(unconditionally_used@ %a)@]@ \
        @[<hov 1>(code_id_unconditionally_used@ %a)@]\
        )@]"
      Code_age_relation.print code_age_relation
      (Name.Map.print Name.Set.print) name_to_name
      (Name.Map.print Code_id.Set.print) name_to_code_id
      (Code_id.Map.print Name.Set.print) code_id_to_name
      (Code_id.Map.print Code_id.Set.print) code_id_to_code_id
      Name.Set.print unconditionally_used
      Code_id.Set.print code_id_unconditionally_used

  (* *)
  let fold_name_occurrences name_occurrences ~init ~names ~code_ids =
    Name_occurrences.fold_names name_occurrences ~f:names
      ~init:(code_ids init (Name_occurrences.code_ids name_occurrences))

  (* Some auxiliary functions *)
  let add_code_id_dep ~src ~(dst : Code_id.Set.t) ({ name_to_code_id; _ } as t) =
    let name_to_code_id = Name.Map.update src (function
      | None ->
        if Code_id.Set.is_empty dst then
          None
        else
          Some dst
      | Some old ->
        Misc.fatal_errorf "Same name bound multiple times: %a -> %a, %a"
          Name.print src Code_id.Set.print old Code_id.Set.print dst
    ) name_to_code_id
    in
    { t with name_to_code_id; }

  let add_dependency ~src ~dst ({ name_to_name; _ } as t) =
    let name_to_name =
      Name.Map.update src (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set)
      ) name_to_name
    in
    { t with name_to_name; }

  let add_name_used ({ unconditionally_used; _ } as t) v =
    let unconditionally_used =  Name.Set.add v unconditionally_used in
    { t with unconditionally_used; }

  let add_code_id_dependency ~src ~dst ({ code_id_to_name; _ } as t) =
    let code_id_to_name =
      Code_id.Map.update src (function
        | None -> Some (Name.Set.singleton dst)
        | Some set -> Some (Name.Set.add dst set))
        code_id_to_name
    in
    { t with code_id_to_name; }

  let add_code_id_to_code_id ~src ~dst ({ code_id_to_code_id; _ } as t) =
    let code_id_to_code_id =
      Code_id.Map.update src (function
        | None ->
          if Code_id.Set.is_empty dst then
            None
          else
            Some dst
        | Some old ->
          Misc.fatal_errorf "Same code_id bound multiple times: %a -> %a, %a"
            Code_id.print src Code_id.Set.print old Code_id.Set.print dst)
        code_id_to_code_id
    in
    { t with code_id_to_code_id; }

  let add_var_used t v = add_name_used t (Name.var v)

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

  let add_continuation_info map
        ~return_continuation ~exn_continuation ~used_closure_vars
        _ { apply_cont_args; apply_result_conts; used_in_handler;
            bindings; code_ids; closure_envs; continuation = _; params = _; } t =
    (* Add the vars used in the handler *)
    let t = add_name_occurrences used_in_handler t in
    (* Add the dependencies created by closures vars in envs *)
    let is_closure_var_used =
      match (used_closure_vars : _ Or_unknown.t) with
      | Unknown -> (fun _ -> true)
      | Known used_closure_vars -> Name_occurrences.mem_closure_var used_closure_vars
    in
    let t =
      Var_within_closure.Map.fold (fun closure_var map t ->
        if not (is_closure_var_used closure_var) then t
        else begin
          Name.Map.fold (fun closure_name values_in_env t ->
            Name_occurrences.fold_names ~f:(fun t value_in_env ->
              add_dependency ~src:closure_name ~dst:value_in_env t)
              values_in_env ~init:t
          ) map t
        end) closure_envs t
    in
    (* Add the vars of continuation used as function call return as used *)
    let t =
      Continuation.Set.fold (fun k t ->
        match Continuation.Map.find k map with
        | elt -> List.fold_left add_var_used t elt.params
        | exception Not_found ->
          if Continuation.equal return_continuation k ||
             Continuation.equal exn_continuation k
          then t
          else
            Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
              Continuation.print k
      ) apply_result_conts t
    in
    (* Build the graph of dependencies between names *)
    let t =
      Name.Map.fold (fun src name_occurrences graph ->
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_dep ~src ~dst t)
      ) bindings t
    in
    let t =
      Code_id.Map.fold (fun src name_occurrences graph ->
        fold_name_occurrences name_occurrences ~init:graph
          ~names:(fun t dst -> add_code_id_dependency ~src ~dst t)
          ~code_ids:(fun t dst -> add_code_id_to_code_id ~src ~dst t)
      ) code_ids t
    in
    (* Build the graph of dependencies between continuation
       parameters and arguments. *)
    Continuation.Map.fold (fun k args t ->
      if Continuation.equal return_continuation k ||
         Continuation.equal exn_continuation k then begin
        Numeric_types.Int.Map.fold (fun _ name_occurrences t ->
          add_name_occurrences name_occurrences t
        ) args t
      end else begin
        let params =
          match Continuation.Map.find k map with
          | elt -> Array.of_list elt.params
          | exception Not_found ->
            Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
              Continuation.print k
        in
        Numeric_types.Int.Map.fold (fun i name_occurrence t ->
          (* Note on the direction of the edge:
             We later do a reachability analysis to compute the
             transitive closure of the used variables.
             Therefore an edge from src to dst means: if src is used, then
             dst is also used.
             Applied here, this means : if the param of a continuation is used,
             then any argument provided for that param is also used.
             The other way wouldn't make much sense. *)
          let src = Name.var params.(i) in
          Name_occurrences.fold_names name_occurrence ~init:t
            ~f:(fun t dst -> add_dependency ~src ~dst t)
        ) args t
      end
    ) apply_cont_args t

  let create
        ~return_continuation ~exn_continuation
        ~code_age_relation ~used_closure_vars
        map extra =
    (* Build the dependencies using the regular params and args of
       continuations, and the let-bindings in continuations handlers. *)
    let t =
      Continuation.Map.fold
        (add_continuation_info map
           ~return_continuation
           ~exn_continuation
           ~used_closure_vars)
        map (empty code_age_relation)
    in
    (* Take into account the extra params and args. *)
    let t =
      Continuation.Map.fold (
        fun _ (extra_params_and_args : Continuation_extra_params_and_args.t) t
        ->
          Apply_cont_rewrite_id.Map.fold (fun _ extra_args t ->
            List.fold_left2 (fun t extra_param extra_arg ->
              let src = Name.var (Kinded_parameter.var extra_param) in
              match
                (extra_arg : Continuation_extra_params_and_args.Extra_arg.t)
              with
              | Already_in_scope simple ->
                Name_occurrences.fold_names (Simple.free_names simple)
                  ~init:t
                  ~f:(fun t dst -> add_dependency ~src ~dst t)
              | New_let_binding (src', prim) ->
                let src' = Name.var src' in
                Name_occurrences.fold_names
                  (Flambda_primitive.free_names prim)
                  ~f:(fun t dst -> add_dependency ~src:src' ~dst t)
                  ~init:(add_dependency ~src ~dst:src' t)
              | New_let_binding_with_named_args (_src', _prim_gen) ->
                (* In this case, the free_vars present in the result of
                   _prim_gen are fresh (and a subset of the simples given to
                   _prim_gen) and generated when going up while creating a
                   wrapper continuation for the return of a function
                   application.
                   In that case, the fresh parameters created for the wrapper
                   cannot introduce dependencies to other variables or
                   parameters of continuations.
                   Therefore, in this case, the data_flow analysis is
                   incomplete, and we instead rely on the free_names analysis
                   to eliminate the extra_let binding if it is unneeded. *)
                t
            ) t extra_params_and_args.extra_params extra_args
          ) extra_params_and_args.extra_args t
      ) extra t
    in
    t

  let required_names
        ({ code_age_relation = _;
           name_to_name = _; name_to_code_id = _;
           code_id_to_name = _; code_id_to_code_id = _;
           unconditionally_used; code_id_unconditionally_used; } as t) =
    let name_queue = Queue.create () in
    Name.Set.iter (fun v -> Queue.push v name_queue) unconditionally_used;
    let code_id_queue = Queue.create () in
    Code_id.Set.iter (fun v -> Queue.push v code_id_queue) code_id_unconditionally_used;
    Reachable.reachable_names t
      code_id_queue code_id_unconditionally_used
      Code_id.Set.empty
      name_queue unconditionally_used

end

(* Analysis *)
(* ******** *)

let analyze
      ~return_continuation ~exn_continuation
      ~code_age_relation ~used_closure_vars
      { stack; map; extra; } =
  Profile.record_call ~accumulate:true "data_flow" (fun () ->
    assert (stack = []);
    let deps =
      Dependency_graph.create map extra
        ~return_continuation ~exn_continuation ~code_age_relation ~used_closure_vars
    in
    (* Format.eprintf "/// graph@\n%a@\n@." Dependency_graph._print deps; *)
    let result = Dependency_graph.required_names deps in
    (* Format.eprintf "/// result@\n%a@\n@." _print_result result; *)
    result
  )

