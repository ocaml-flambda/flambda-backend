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

module EPA = Continuation_extra_params_and_args

(* Helper module *)
(* ************* *)

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

(* Typedefs *)
(* ******** *)

(* CR chambart/gbury: we might want to also track function_slots in addition to
   value_slots. *)

(* CR-someday chambart/gbury: get rid of Name_occurences everywhere, this is not
   small while we need only the names

   mshinwell: in practice I'm not sure this will make any difference *)
type elt =
  { continuation : Continuation.t;
    recursive : bool;
    params : Bound_parameters.t;
    parent_continuation : Continuation.t option;
    used_in_handler : Name_occurrences.t;
    apply_result_conts : Continuation.Set.t;
    apply_exn_conts : Continuation.Set.t;
    bindings : Name_occurrences.t Name.Map.t;
    defined : Variable.Set.t;
    code_ids : Name_occurrences.t Code_id.Map.t;
    value_slots : Name_occurrences.t Name.Map.t Value_slot.Map.t;
    apply_cont_args : Simple.Set.t Numeric_types.Int.Map.t Continuation.Map.t
  }

type t =
  { stack : elt list;
    map : elt Continuation.Map.t;
    extra : EPA.t Continuation.Map.t;
    dummy_toplevel_cont : Continuation.t
  }

(* type alias useful for later *)
type source_info = t

type continuation_param_aliases =
  { aliases : Variable.t Variable.Map.t;
    aliases_kind : Flambda_kind.t Variable.Map.t;
    extra_args_for_aliases : Variable.Set.t Continuation.Map.t
  }

type dead_variable_result =
  { required_names : Name.Set.t;
    reachable_code_ids : Reachable_code_ids.t
  }

type result =
  { dead_variable_result : dead_variable_result;
    continuation_param_aliases : continuation_param_aliases
  }

(* Print *)
(* ***** *)

let [@ocamlformat "disable"] print_elt ppf
    { continuation;
      recursive;
      params;
      parent_continuation;
      used_in_handler;
      apply_result_conts;
      apply_exn_conts;
      bindings;
      defined;
      code_ids;
      value_slots;
      apply_cont_args
    } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation %a)@]@ \
      %s\
      @[<hov 1>(params %a)@]@ \
      @[<hov 1>(parent_continuation %a)@]@ \
      @[<hov 1>(used_in_handler %a)@]@ \
      @[<hov 1>(apply_result_conts %a)@]@ \
      @[<hov 1>(apply_exn_conts %a)@]@ \
      @[<hov 1>(bindings %a)@]@ \
      @[<hov 1>(defined %a)@]@ \
      @[<hov 1>(code_ids %a)@]@ \
      @[<hov 1>(value_slots %a)@]@ \
      @[<hov 1>(apply_cont_args %a)@]\
    )@]"
    Continuation.print continuation
    (if recursive then "(recursive)" else "")
    Bound_parameters.print params
    (Format.pp_print_option ~none:(fun ppf () -> Format.fprintf ppf "root")
       Continuation.print) parent_continuation
    Name_occurrences.print used_in_handler
    Continuation.Set.print apply_result_conts
    Continuation.Set.print apply_exn_conts
    (Name.Map.print Name_occurrences.print)
    bindings
    Variable.Set.print
    defined
    (Code_id.Map.print Name_occurrences.print)
    code_ids
    (Value_slot.Map.print (Name.Map.print Name_occurrences.print))
    value_slots
    (Continuation.Map.print (Numeric_types.Int.Map.print Simple.Set.print))
    apply_cont_args

let print_stack ppf stack =
  Format.fprintf ppf "@[<v 1>(%a)@]"
    (Format.pp_print_list print_elt ~pp_sep:Format.pp_print_space)
    stack

let print_map ppf map = Continuation.Map.print print_elt ppf map

let print_extra ppf extra = Continuation.Map.print EPA.print ppf extra

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

let [@ocamlformat "disable"] print_continuation_param_aliases ppf
    { aliases; aliases_kind; extra_args_for_aliases } =
  Format.fprintf ppf
    "@[<hov 1>(\
       @[<hov 1>(aliases@ %a)@]@ \
       @[<hov 1>(aliases_kind@ %a)@]@ \
       @[<hov 1>(extra_args_for_aliases@ %a)@]\
     )@]"
    (Variable.Map.print Variable.print) aliases
    (Variable.Map.print Flambda_kind.print) aliases_kind
    (Continuation.Map.print Variable.Set.print) extra_args_for_aliases

let [@ocamlformat "disable"] _print_result ppf
    { dead_variable_result = { required_names; reachable_code_ids };
      continuation_param_aliases = { aliases; aliases_kind; extra_args_for_aliases } } =
  Format.fprintf ppf
    "@[<hov 1>(\
       @[<hov 1>(required_names@ %a)@]@ \
       @[<hov 1>(reachable_code_ids@ %a)@]@ \
       @[<hov 1>(aliases@ %a)@]@ \
       @[<hov 1>(aliases_kind@ %a)@]@ \
       @[<hov 1>(extra_args_for_aliases@ %a)@]\
     )@]"
    Name.Set.print required_names
    Reachable_code_ids.print reachable_code_ids
    (Variable.Map.print Variable.print) aliases
    (Variable.Map.print Flambda_kind.print) aliases_kind
    (Continuation.Map.print Variable.Set.print) extra_args_for_aliases

(* Creation *)
(* ******** *)

let wrong_dummy_toplevel_cont_name = "wrong toplevel cont"

let empty () =
  let wrong_dummy_toplevel_cont =
    Continuation.create ~name:wrong_dummy_toplevel_cont_name ()
  in
  { stack = [];
    map = Continuation.Map.empty;
    extra = Continuation.Map.empty;
    dummy_toplevel_cont = wrong_dummy_toplevel_cont
  }

(* Updates *)
(* ******* *)

let add_extra_params_and_args cont extra t =
  let extra =
    Continuation.Map.update cont
      (function
        | Some _ -> Misc.fatal_errorf "Continuation extended a second time"
        | None -> Some extra)
      t.extra
  in
  { t with extra }

let enter_continuation continuation ~recursive params t =
  let parent_continuation =
    match t.stack with [] -> None | parent :: _ -> Some parent.continuation
  in
  let elt =
    { continuation;
      recursive;
      params;
      parent_continuation;
      bindings = Name.Map.empty;
      defined = Variable.Set.empty;
      code_ids = Code_id.Map.empty;
      value_slots = Value_slot.Map.empty;
      used_in_handler = Name_occurrences.empty;
      apply_cont_args = Continuation.Map.empty;
      apply_result_conts = Continuation.Set.empty;
      apply_exn_conts = Continuation.Set.empty
    }
  in
  { t with stack = elt :: t.stack }

let init_toplevel ~dummy_toplevel_cont params _t =
  enter_continuation dummy_toplevel_cont ~recursive:false params
    { (empty ()) with dummy_toplevel_cont }

let exit_continuation cont t =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | ({ continuation; _ } as elt) :: stack ->
    assert (Continuation.equal cont continuation);
    let map = Continuation.Map.add cont elt t.map in
    { t with stack; map }

let update_top_of_stack ~t ~f =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | elt :: stack -> { t with stack = f elt :: stack }

let record_defined_var var t =
  update_top_of_stack ~t ~f:(fun elt ->
      let defined = Variable.Set.add var elt.defined in
      { elt with defined })

let record_var_binding var name_occurrences ~generate_phantom_lets t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.var var)
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf
                "The following variable has been bound twice: %a" Variable.print
                var)
          elt.bindings
      in
      let used_in_handler =
        if Variable.user_visible var && generate_phantom_lets
        then
          Name_occurrences.add_variable elt.used_in_handler var
            Name_mode.phantom
        else elt.used_in_handler
      in
      let defined = Variable.Set.add var elt.defined in
      { elt with bindings; used_in_handler; defined })

let record_symbol_projection var name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.var var)
          (function
            | None -> Some name_occurrences
            | Some prior_occurences as original ->
              if Name_occurrences.equal prior_occurences name_occurrences
              then original
              else
                Misc.fatal_errorf
                  "@[<v>The following projection has been bound to different \
                   symbols:%a@ previously bound to:@ %a@ and now to@ %a@]"
                  Variable.print var Name_occurrences.print prior_occurences
                  Name_occurrences.print name_occurrences)
          elt.bindings
      in
      { elt with bindings })

let record_symbol_binding symbol name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.symbol symbol)
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf "The following symbol has been bound twice: %a"
                Symbol.print symbol)
          elt.bindings
      in
      { elt with bindings })

let record_code_id_binding code_id name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let code_ids =
        Code_id.Map.update code_id
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf "The following code_id has been bound twice: %a"
                Code_id.print code_id)
          elt.code_ids
      in
      { elt with code_ids })

let record_value_slot src value_slot dst t =
  update_top_of_stack ~t ~f:(fun elt ->
      let value_slots =
        Value_slot.Map.update value_slot
          (function
            | None -> Some (Name.Map.singleton src dst)
            | Some map ->
              Some
                (Name.Map.update src
                   (function
                     | None -> Some dst
                     | Some dst' -> Some (Name_occurrences.union dst dst'))
                   map))
          elt.value_slots
      in
      { elt with value_slots })

let add_used_in_current_handler name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let used_in_handler =
        Name_occurrences.union elt.used_in_handler name_occurrences
      in
      { elt with used_in_handler })

let add_apply_conts ~result_cont ~exn_cont t =
  update_top_of_stack ~t ~f:(fun elt ->
      let apply_result_conts =
        match result_cont with
        | None -> elt.apply_result_conts
        | Some result_cont ->
          Continuation.Set.add result_cont elt.apply_result_conts
      in
      let apply_exn_conts =
        Continuation.Set.add exn_cont elt.apply_result_conts
      in
      { elt with apply_result_conts; apply_exn_conts })

let add_apply_cont_args cont arg_name_simples t =
  update_top_of_stack ~t ~f:(fun elt ->
      let apply_cont_args =
        Continuation.Map.update cont
          (fun map_opt ->
            let map =
              Option.value ~default:Numeric_types.Int.Map.empty map_opt
            in
            let map, _ =
              List.fold_left
                (fun (map, i) arg_simple ->
                  let map =
                    Numeric_types.Int.Map.update i
                      (function
                        | None -> Some (Simple.Set.singleton arg_simple)
                        | Some set -> Some (Simple.Set.add arg_simple set))
                      map
                  in
                  map, i + 1)
                (map, 0) arg_name_simples
            in
            Some map)
          elt.apply_cont_args
      in
      { elt with apply_cont_args })

(* Dependency graph *)
(* **************** *)

module Dependency_graph = struct
  type t =
    { code_age_relation : Code_age_relation.t;
      name_to_name : Name.Set.t Name.Map.t;
      name_to_code_id : Code_id.Set.t Name.Map.t;
      code_id_to_name : Name.Set.t Code_id.Map.t;
      code_id_to_code_id : Code_id.Set.t Code_id.Map.t;
      unconditionally_used : Name.Set.t;
      code_id_unconditionally_used : Code_id.Set.t
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
        if Queue.is_empty code_id_queue
        then
          { required_names = name_enqueued;
            reachable_code_ids =
              { live_code_ids = code_id_enqueued;
                ancestors_of_live_code_ids = older_enqueued
              }
          }
        else
          reachable_code_ids t code_id_queue code_id_enqueued (Queue.create ())
            older_enqueued name_queue name_enqueued
      | src ->
        let name_enqueued =
          Name_Name_Edge.push ~src name_enqueued name_queue t.name_to_name
        in
        let code_id_enqueued =
          Name_Code_id_Edge.push ~src code_id_enqueued code_id_queue
            t.name_to_code_id
        in
        reachable_names t code_id_queue code_id_enqueued older_enqueued
          name_queue name_enqueued

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
        match
          Code_age_relation.get_older_version_of t.code_age_relation src
        with
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
            reachable_older_code_ids t code_id_queue code_id_enqueued
              older_queue older_enqueued name_queue name_enqueued)
  end

  let empty code_age_relation =
    { code_age_relation;
      name_to_name = Name.Map.empty;
      name_to_code_id = Name.Map.empty;
      code_id_to_name = Code_id.Map.empty;
      code_id_to_code_id = Code_id.Map.empty;
      unconditionally_used = Name.Set.empty;
      code_id_unconditionally_used = Code_id.Set.empty
    }

  let _print ppf
      { name_to_name;
        name_to_code_id;
        code_id_to_name;
        code_id_to_code_id;
        code_age_relation;
        unconditionally_used;
        code_id_unconditionally_used
      } =
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(code_age_relation@ %a)@]@ @[<hov 1>(name_to_name@ \
       %a)@]@ @[<hov 1>(name_to_code_id@ %a)@]@ @[<hov 1>(code_id_to_name@ \
       %a)@]@ @[<hov 1>(code_id_to_code_id@ %a)@]@ @[<hov \
       1>(unconditionally_used@ %a)@]@ @[<hov 1>(code_id_unconditionally_used@ \
       %a)@])@]"
      Code_age_relation.print code_age_relation
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
  let add_code_id_dep ~src ~(dst : Code_id.Set.t) ({ name_to_code_id; _ } as t)
      =
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

  let add_name_used ({ unconditionally_used; _ } as t) v =
    let unconditionally_used = Name.Set.add v unconditionally_used in
    { t with unconditionally_used }

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

  let add_var_used t v = add_name_used t (Name.var v)

  let free_names_of_simple_set s =
    Simple.Set.fold
      (fun simple name_occurrences ->
        Name_occurrences.union name_occurrences (Simple.free_names simple))
      s Name_occurrences.empty

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
      { apply_cont_args;
        apply_result_conts;
        apply_exn_conts = _;
        (* CR pchambart: properly follow dependencies in exception extra args.
           They are currently marked as always used, so it is correct, but not
           optimal *)
        used_in_handler;
        bindings;
        defined = _;
        code_ids;
        value_slots;
        continuation = _;
        recursive = _;
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
    (* Add the vars of continuation used as function call return as used *)
    let t =
      Continuation.Set.fold
        (fun k t ->
          match Continuation.Map.find k map with
          | elt ->
            List.fold_left add_var_used t (Bound_parameters.vars elt.params)
          | exception Not_found ->
            if Continuation.equal return_continuation k
               || Continuation.equal exn_continuation k
            then t
            else
              Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
                Continuation.print k)
        apply_result_conts t
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
      (fun k args t ->
        if Continuation.equal return_continuation k
           || Continuation.equal exn_continuation k
        then
          Numeric_types.Int.Map.fold
            (fun _ simple_set t ->
              let name_occurrences = free_names_of_simple_set simple_set in
              add_name_occurrences name_occurrences t)
            args t
        else
          let params =
            match Continuation.Map.find k map with
            | elt -> Array.of_list (Bound_parameters.vars elt.params)
            | exception Not_found ->
              Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
                Continuation.print k
          in
          Numeric_types.Int.Map.fold
            (fun i simple_set t ->
              (* Note on the direction of the edge:

                 We later do a reachability analysis to compute the transitive
                 closure of the used variables.

                 Therefore an edge from src to dst means: if src is used, then
                 dst is also used.

                 Applied here, this means : if the param of a continuation is
                 used, then any argument provided for that param is also used.
                 The other way wouldn't make much sense. *)
              let src = Name.var params.(i) in
              let name_occurrences = free_names_of_simple_set simple_set in
              Name_occurrences.fold_names name_occurrences ~init:t
                ~f:(fun t dst -> add_dependency ~src ~dst t))
            args t)
      apply_cont_args t

  let create ~return_continuation ~exn_continuation ~code_age_relation
      ~used_value_slots map extra =
    (* Build the dependencies using the regular params and args of
       continuations, and the let-bindings in continuations handlers. *)
    let t =
      Continuation.Map.fold
        (add_continuation_info map ~return_continuation ~exn_continuation
           ~used_value_slots)
        map (empty code_age_relation)
    in
    (* Take into account the extra params and args. *)
    let t =
      Continuation.Map.fold
        (fun _ (extra_params_and_args : EPA.t) t ->
          Apply_cont_rewrite_id.Map.fold
            (fun _ extra_args t ->
              List.fold_left2
                (fun t extra_param extra_arg ->
                  let src = Name.var (Bound_parameter.var extra_param) in
                  match (extra_arg : EPA.Extra_arg.t) with
                  | Already_in_scope simple ->
                    Name_occurrences.fold_names (Simple.free_names simple)
                      ~init:t ~f:(fun t dst -> add_dependency ~src ~dst t)
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

                       In that case, the fresh parameters created for the
                       wrapper cannot introduce dependencies to other variables
                       or parameters of continuations.

                       Therefore, in this case, the data_flow analysis is
                       incomplete, and we instead rely on the free_names
                       analysis to eliminate the extra_let binding if it is
                       unneeded. *)
                    t)
                t
                (Bound_parameters.to_list
                   (EPA.extra_params extra_params_and_args))
                extra_args)
            (EPA.extra_args extra_params_and_args)
            t)
        extra t
    in
    t

  let required_names
      ({ code_age_relation = _;
         name_to_name = _;
         name_to_code_id = _;
         code_id_to_name = _;
         code_id_to_code_id = _;
         unconditionally_used;
         code_id_unconditionally_used
       } as t) =
    let name_queue = Queue.create () in
    Name.Set.iter (fun v -> Queue.push v name_queue) unconditionally_used;
    let code_id_queue = Queue.create () in
    Code_id.Set.iter
      (fun v -> Queue.push v code_id_queue)
      code_id_unconditionally_used;
    Reachable.reachable_names t code_id_queue code_id_unconditionally_used
      Code_id.Set.empty name_queue unconditionally_used
end

(* Dominator graph *)
(* *************** *)

module Dominator_graph = struct
  module G = Strongly_connected_components.Make (Variable)

  type t =
    { required_names : Name.Set.t;
      params_kind : Flambda_kind.With_subkind.t Variable.Map.t;
      graph : G.directed_graph;
      dominator_roots : Variable.Set.t
          (* variables that are dominated only by themselves, usually because a
             constant or a symbol can flow to that variable, and thus that
             variable cannot be dominated by another variable. *)
    }

  let empty ~required_names =
    let graph = Variable.Map.empty in
    let dominator_roots = Variable.Set.empty in
    let params_kind = Variable.Map.empty in
    { required_names; params_kind; graph; dominator_roots }

  let add_node t var =
    if not (Name.Set.mem (Name.var var) t.required_names)
    then t
    else
      let graph =
        Variable.Map.update var
          (function None -> Some Variable.Set.empty | Some _ as res -> res)
          t.graph
      in
      { t with graph }

  let add_root var t =
    if not (Name.Set.mem (Name.var var) t.required_names)
    then t
    else { t with dominator_roots = Variable.Set.add var t.dominator_roots }

  let add_edge ~src ~dst t =
    if not (Name.Set.mem (Name.var src) t.required_names)
    then t
    else
      Simple.pattern_match' dst
        ~const:(fun _ -> add_root src t)
        ~symbol:(fun _ ~coercion:_ -> add_root src t)
        ~var:(fun dst ~coercion:_ ->
          let graph =
            Variable.Map.update src
              (function
                | None -> Some (Variable.Set.singleton dst)
                | Some s -> Some (Variable.Set.add dst s))
              t.graph
          in
          { t with graph })

  let add_continuation_info map _k elt t ~return_continuation ~exn_continuation
      =
    let t =
      List.fold_left
        (fun t bp ->
          let var = Bound_parameter.var bp in
          let t = add_node t var in
          let params_kind =
            Variable.Map.add var (Bound_parameter.kind bp) t.params_kind
          in
          { t with params_kind })
        t
        (Bound_parameters.to_list elt.params)
    in
    Continuation.Map.fold
      (fun k args t ->
        if Continuation.equal return_continuation k
           || Continuation.equal exn_continuation k
        then t
        else
          let params =
            match Continuation.Map.find k map with
            | elt -> Array.of_list (Bound_parameters.vars elt.params)
            | exception Not_found ->
              Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
                Continuation.print k
          in
          Numeric_types.Int.Map.fold
            (fun i simple_set t ->
              (* Note on the direction of the edge:

                 We later do a dominator analysis on this graph. To do so, we
                 consider that an edge from ~src to ~dst means: ~dst is used as
                 argument (of an apply_cont), that maps to ~src (as param of a
                 continuation). *)
              let src = params.(i) in
              Simple.Set.fold (fun dst t -> add_edge ~src ~dst t) simple_set t)
            args t)
      elt.apply_cont_args t

  let create ~required_names ~return_continuation ~exn_continuation map extra =
    let t = empty ~required_names in
    let t =
      Continuation.Map.fold
        (add_continuation_info ~return_continuation ~exn_continuation map)
        map t
    in
    let t =
      Continuation.Map.fold
        (fun _ (extra_params_and_args : EPA.t) t ->
          let t =
            List.fold_left
              (fun t bp ->
                let params_kind =
                  Variable.Map.add (Bound_parameter.var bp)
                    (Bound_parameter.kind bp) t.params_kind
                in
                { t with params_kind })
              t
              (Bound_parameters.to_list
                 (EPA.extra_params extra_params_and_args))
          in
          Apply_cont_rewrite_id.Map.fold
            (fun _ extra_args t ->
              List.fold_left2
                (fun t extra_param extra_arg ->
                  let src = Bound_parameter.var extra_param in
                  match
                    (extra_arg : Continuation_extra_params_and_args.Extra_arg.t)
                  with
                  | Already_in_scope simple -> add_edge ~src ~dst:simple t
                  | New_let_binding (tmp_var, _)
                  | New_let_binding_with_named_args (tmp_var, _) ->
                    (* In these cases, we mainly want to record that the
                       `tmp_var` is a root value / self-dominator, i.e. ~src
                       will not be dominated by another variable (and in
                       particular it will not be dominated by a continaution
                       parameter). *)
                    add_edge ~src ~dst:(Simple.var tmp_var) t)
                t
                (Bound_parameters.to_list
                   (EPA.extra_params extra_params_and_args))
                extra_args)
            (EPA.extra_args extra_params_and_args)
            t)
        extra t
    in
    let all_variables =
      Variable.Map.fold
        (fun v dsts acc -> Variable.Set.add v (Variable.Set.union dsts acc))
        t.graph t.dominator_roots
    in
    (* ensure that all variable are mapped: this is a requirement for the SCC
       computation *)
    let t =
      Variable.Set.fold
        (fun var t ->
          let graph =
            Variable.Map.update var
              (function
                | Some _ as res -> res | None -> Some Variable.Set.empty)
              t.graph
          in
          { t with graph })
        all_variables t
    in
    (* Format.eprintf "GRAPH:@\n%a@." (Variable.Map.print Variable.Set.print)
       t.graph; *)
    t

  let find_dom var doms =
    (* there are tow cases where the variable is not in the "doms" maps:

       - is not mapped in the graph, which means that it is a let-bound
       variable, in which case it can only be dominated by itself.

       - we are in th efirst iteration of a loop fixpoint, in which case we also
       want to initialize the dominator to the variable itself. *)
    try Variable.Map.find var doms with Not_found -> var

  let update_doms_for_one_var { dominator_roots; graph; _ } doms var =
    let dom =
      if Variable.Set.mem var dominator_roots
      then var
      else
        match Variable.Map.find var graph with
        | exception Not_found -> var
        | predecessors ->
          let s =
            Variable.Set.map
              (fun predecessor -> find_dom predecessor doms)
              predecessors
          in
          if Variable.Set.cardinal s = 1 then Variable.Set.choose s else var
    in
    Variable.Map.add var dom doms

  let initialize_doms_for_fixpoint { graph; _ } doms vars =
    (* Note: since all vars are in a cycle, all_predecessors will include all
       vars *)
    let all_predecessors =
      List.fold_left
        (fun acc var ->
          let predecessors =
            try Variable.Map.find var graph with Not_found -> assert false
          in
          Variable.Set.union predecessors acc)
        Variable.Set.empty vars
    in
    let init_doms =
      Variable.Set.map (fun var -> find_dom var doms) all_predecessors
    in
    let outside_cycle =
      Variable.Map.of_set
        (fun var -> Variable.Set.singleton (find_dom var doms))
        (Variable.Set.diff all_predecessors (Variable.Set.of_list vars))
    in
    List.fold_left
      (fun doms var -> Variable.Map.add var init_doms doms)
      outside_cycle vars

  let rec dom_fixpoint ({ graph; dominator_roots; _ } as t) acc vars =
    let acc' =
      List.fold_left
        (fun acc var ->
          if Variable.Set.mem var dominator_roots
          then Variable.Map.add var (Variable.Set.singleton var) acc
          else
            let init_doms = Variable.Map.find var acc in
            let predecessors =
              try Variable.Map.find var graph with Not_found -> assert false
            in
            let new_doms =
              Variable.Set.fold
                (fun predecessor new_doms ->
                  Variable.Set.inter new_doms
                    (Variable.Map.find predecessor acc))
                predecessors init_doms
            in
            let new_doms = Variable.Set.add var new_doms in
            Variable.Map.add var new_doms acc)
        acc vars
    in
    if Variable.Map.equal Variable.Set.equal acc acc'
    then acc
    else dom_fixpoint t acc' vars

  let extract_doms doms fixpoint_result vars =
    let var_set = Variable.Set.of_list vars in
    List.fold_left
      (fun doms var ->
        let fixpoint_doms = Variable.Map.find var fixpoint_result in
        let var_doms = Variable.Set.diff fixpoint_doms var_set in
        let cardinal = Variable.Set.cardinal var_doms in
        assert (cardinal <= 1);
        let dom = if cardinal = 1 then Variable.Set.choose var_doms else var in
        Variable.Map.add var dom doms)
      doms vars

  let dominator_analysis ({ graph; _ } as t) =
    let components = G.connected_components_sorted_from_roots_to_leaf graph in
    let dominators =
      Array.fold_right
        (fun component doms ->
          match component with
          | G.No_loop var -> update_doms_for_one_var t doms var
          | G.Has_loop vars ->
            let loop_doms = initialize_doms_for_fixpoint t doms vars in
            let loop_result = dom_fixpoint t loop_doms vars in
            let doms = extract_doms doms loop_result vars in
            doms)
        components Variable.Map.empty
    in
    dominators

  let aliases_kind { params_kind; required_names; _ } aliases =
    Variable.Map.fold
      (fun param kind acc ->
        if not (Name.Set.mem (Name.var param) required_names)
        then acc
        else
          let alias = Variable.Map.find param aliases in
          (* CR: Not sure this is absolutely necessary, but it's simpler. The
             alternative would be to do a join of all kinds with subkinds for
             all the members of the alias class. *)
          let kind = Flambda_kind.With_subkind.kind kind in
          (match Variable.Map.find alias acc with
          | exception Not_found -> ()
          | alias_kind ->
            if not (Flambda_kind.equal kind alias_kind)
            then Misc.fatal_errorf "Incoherent kinds for aliases !");
          Variable.Map.add alias kind acc)
      params_kind Variable.Map.empty

  module Dot = struct
    let node_id ~ctx ppf (variable : Variable.t) =
      Format.fprintf ppf "node_%d_%d" ctx (variable :> int)

    let node ~ctx ~root ppf var =
      if root
      then
        Format.fprintf ppf "%a [shape=record label=\"%a\"];@\n" (node_id ~ctx)
          var Variable.print var
      else
        Format.fprintf ppf "%a [label=\"%a\"];@\n" (node_id ~ctx) var
          Variable.print var

    let nodes ~ctx ~roots ppf var_map =
      Variable.Map.iter
        (fun var _ ->
          let root = Variable.Set.mem var roots in
          node ~ctx ~root ppf var)
        var_map

    let edge ~ctx ~color ppf src dst =
      Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
        (node_id ~ctx) dst color

    let edges ~ctx ~color ppf edge_map =
      Variable.Map.iter
        (fun src dst_set ->
          Variable.Set.iter (fun dst -> edge ~ctx ~color ppf src dst) dst_set)
        edge_map

    let edges' ~ctx ~color ppf edge_map =
      Variable.Map.iter (fun src dst -> edge ~ctx ~color ppf src dst) edge_map

    let print ~ctx ~print_name ~doms ppf t =
      Flambda_colours.without_colours ~f:(fun () ->
          Format.fprintf ppf
            "subgraph cluster_%d { label=\"%s\"@\n%a@\n%a@\n%a@\n}@." ctx
            print_name
            (nodes ~ctx ~roots:t.dominator_roots)
            t.graph
            (edges ~ctx ~color:"black")
            t.graph (edges' ~ctx ~color:"red") doms)
  end
end

module Control_flow_graph = struct
  type t =
    { dummy_toplevel_cont : Continuation.t;
      callers : Continuation.Set.t Continuation.Map.t;
      parents : Continuation.t Continuation.Map.t;
      children : Continuation.Set.t Continuation.Map.t
    }

  let add ~caller ~callee map =
    Continuation.Map.update callee
      (function
        | None -> Some (Continuation.Set.singleton caller)
        | Some callers -> Some (Continuation.Set.add caller callers))
      map

  let create ~dummy_toplevel_cont { map; _ } =
    let parents =
      Continuation.Map.filter_map
        (fun _ (elt : elt) -> elt.parent_continuation)
        map
    in
    let children =
      Continuation.Map.fold
        (fun k parent acc ->
          Continuation.Map.update parent
            (function
              | None -> Some (Continuation.Set.singleton k)
              | Some set -> Some (Continuation.Set.add k set))
            acc)
        parents Continuation.Map.empty
    in
    let callers =
      Continuation.Map.fold
        (fun caller (elt : elt) acc ->
          let acc =
            Continuation.Map.merge
              (fun _callee acc args ->
                match acc, args with
                | None, None -> assert false
                | Some set, None -> Some set
                | None, Some _ -> Some (Continuation.Set.singleton caller)
                | Some set, Some _ -> Some (Continuation.Set.add caller set))
              acc elt.apply_cont_args
          in
          let acc =
            Continuation.Set.fold
              (fun callee acc -> add ~caller ~callee acc)
              elt.apply_result_conts acc
          in
          Continuation.Set.fold
            (fun callee acc -> add ~caller ~callee acc)
            elt.apply_exn_conts acc)
        map
        (Continuation.Map.singleton dummy_toplevel_cont Continuation.Set.empty)
    in
    { dummy_toplevel_cont; callers; parents; children }

  (* This does not need to be tail-rec as other parts of flambda2 are already
     not tail-rec in the number of nested continuations. *)
  let map_fold_on_children { children; dummy_toplevel_cont; _ } f acc =
    let rec aux k acc =
      let acc, to_add = f k acc in
      let map = Continuation.Map.singleton k to_add in
      match Continuation.Map.find k children with
      | exception Not_found -> map
      | s ->
        Continuation.Set.fold
          (fun child map -> Continuation.Map.disjoint_union map (aux child acc))
          s map
    in
    aux dummy_toplevel_cont acc

  let compute_available_variables ~(source_info : source_info) t =
    map_fold_on_children t
      (fun k acc ->
        let elt = Continuation.Map.find k source_info.map in
        let extra_vars =
          match Continuation.Map.find k source_info.extra with
          | exception Not_found -> Variable.Set.empty
          | epa ->
            let extra_params =
              Continuation_extra_params_and_args.extra_params epa
            in
            Bound_parameters.var_set extra_params
        in
        let acc =
          Variable.Set.union
            (Variable.Set.union acc (Bound_parameters.var_set elt.params))
            extra_vars
        in
        acc, acc)
      Variable.Set.empty

  let compute_transitive_parents t =
    map_fold_on_children t
      (fun k acc ->
        let acc = Continuation.Set.add k acc in
        acc, acc)
      Continuation.Set.empty

  let compute_added_extra_args added_extra_args t =
    map_fold_on_children t
      (fun k available ->
        ( Variable.Set.union (Continuation.Map.find k added_extra_args) available,
          available ))
      Variable.Set.empty

  let compute_continuation_extra_args_for_aliases ~required_names
      ~(source_info : source_info) doms t =
    let available_variables = compute_available_variables ~source_info t in
    let transitive_parents = compute_transitive_parents t in
    let remove_vars_in_scope_of k var_set =
      let elt : elt = Continuation.Map.find k source_info.map in
      let res =
        Variable.Set.diff var_set (Continuation.Map.find k available_variables)
      in
      Variable.Set.diff res elt.defined
    in
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
    let init =
      Continuation.Map.mapi
        (fun k elt ->
          let s =
            List.fold_left
              (fun acc param ->
                let dom =
                  match Variable.Map.find param doms with
                  | exception Not_found ->
                    if Name.Set.mem (Name.var param) required_names
                    then
                      Misc.fatal_errorf "Dom not found for: %a@." Variable.print
                        param
                    else param
                  | dom -> dom
                in
                if Variable.equal param dom
                then acc
                else Variable.Set.add dom acc)
              Variable.Set.empty
              (Bound_parameters.vars elt.params)
          in
          let s = remove_vars_in_scope_of k s in
          if not (Variable.Set.is_empty s) then push k;
          s)
        source_info.map
    in
    let res = ref init in
    while not (q_is_empty ()) do
      let k = pop () in
      let elt = Continuation.Map.find k source_info.map in
      let aliases_needed = Continuation.Map.find k !res in
      let callers =
        match Continuation.Map.find k t.callers with
        | exception Not_found ->
          Misc.fatal_errorf "Callers not found for: %a" Continuation.print k
        | callers -> callers
      in
      let callers =
        if not elt.recursive
        then callers
        else
          Continuation.Set.filter
            (fun caller ->
              not
                (Continuation.Set.mem k
                   (Continuation.Map.find caller transitive_parents)))
            callers
      in
      Continuation.Set.iter
        (fun caller ->
          let old_aliases_needed = Continuation.Map.find caller !res in
          let new_aliases_needed =
            Variable.Set.union old_aliases_needed
              (remove_vars_in_scope_of caller aliases_needed)
          in
          if not (Variable.Set.equal old_aliases_needed new_aliases_needed)
          then (
            res := Continuation.Map.add caller new_aliases_needed !res;
            push caller))
        callers
    done;
    let extra_args_for_toplevel_cont =
      Continuation.Map.find t.dummy_toplevel_cont !res
    in
    if not (Variable.Set.is_empty extra_args_for_toplevel_cont)
    then
      Format.eprintf
        "ERROR:@\nToplevel continuation cannot have needed extra argument: %a@."
        Variable.Set.print extra_args_for_toplevel_cont;
    let added_extra_args = !res in
    let available_added_extra_args =
      compute_added_extra_args added_extra_args t
    in
    Continuation.Map.mapi
      (fun k aliases_needed ->
        let available = Continuation.Map.find k available_added_extra_args in
        Variable.Set.diff aliases_needed available)
      added_extra_args

  module Dot = struct
    let node_id ~ctx ppf (cont : Continuation.t) =
      Format.fprintf ppf "node_%d_%d" ctx (cont :> int)

    let node ?(extra_args = Variable.Set.empty) ?(info = "") ~df ~ctx () ppf
        cont =
      let params, shape =
        match Continuation.Map.find cont df.map with
        | exception Not_found -> "[ none ]", ""
        | elt ->
          let params =
            Format.asprintf "[%a]"
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                 Variable.print)
              (Bound_parameters.vars elt.params)
          in
          let shape = if elt.recursive then "shape=record" else "" in
          params, shape
      in
      Format.fprintf ppf "%a [label=\"%a %s %s\" %s %s];@\n" (node_id ~ctx) cont
        Continuation.print cont params
        (String.map
           (function '{' -> '[' | '}' -> ']' | c -> c)
           (Format.asprintf "%a" Variable.Set.print extra_args))
        shape info

    let nodes ~df ~ctx ~return_continuation ~exn_continuation
        ~extra_args_for_aliases ppf cont_map =
      Continuation.Set.iter
        (fun cont ->
          let extra_args =
            Continuation.Map.find_opt cont extra_args_for_aliases
          in
          let info =
            if Continuation.equal return_continuation cont
            then "color=blue"
            else if Continuation.equal exn_continuation cont
            then "color=red"
            else ""
          in
          node ?extra_args ~df ~ctx ~info () ppf cont)
        cont_map

    let edge ~ctx ~color ppf src dst =
      Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
        (node_id ~ctx) dst color

    let edges ~ctx ~color ppf edge_map =
      Continuation.Map.iter
        (fun src dst_set ->
          Continuation.Set.iter
            (fun dst -> edge ~ctx ~color ppf src dst)
            dst_set)
        edge_map

    let edges' ~ctx ~color ppf edge_map =
      Continuation.Map.iter
        (fun src dst -> edge ~ctx ~color ppf src dst)
        edge_map

    let print ~ctx ~df ~print_name ppf ~return_continuation ~exn_continuation
        ~extra_args_for_aliases (t : t) =
      let dummy_toplevel_cont = t.dummy_toplevel_cont in
      let all_conts =
        Continuation.Map.fold
          (fun cont callers acc ->
            Continuation.Set.add cont (Continuation.Set.union callers acc))
          t.callers
          (Continuation.Set.of_list
             [dummy_toplevel_cont; return_continuation; exn_continuation])
      in
      let all_conts =
        Continuation.Map.fold
          (fun cont _parent acc -> Continuation.Set.add cont acc)
          t.parents all_conts
      in
      Flambda_colours.without_colours ~f:(fun () ->
          Format.fprintf ppf
            "subgraph cluster_%d { label=\"%s\";@\n%a%a@\n%a@\n%a@\n}@." ctx
            print_name (node ~df ~ctx ()) dummy_toplevel_cont
            (nodes ~df ~return_continuation ~exn_continuation ~ctx
               ~extra_args_for_aliases)
            all_conts
            (edges' ~ctx ~color:"green")
            t.parents
            (edges ~ctx ~color:"black")
            t.callers)
  end
end

(* Analysis *)
(* ******** *)

let r = ref ~-1

let dominator_graph_ppf =
  lazy
    (match Sys.getenv_opt "DOM_GRAPH" with
    | None -> None
    | Some filename ->
      let ch = open_out filename in
      let ppf = Format.formatter_of_out_channel ch in
      Format.fprintf ppf "digraph g {@\n";
      at_exit (fun () ->
          Format.fprintf ppf "@\n}@.";
          close_out ch);
      Some ppf)

let control_flow_graph_ppf =
  lazy
    (match Sys.getenv_opt "FLOW_GRAPH" with
    | None -> None
    | Some filename ->
      let ch = open_out filename in
      let ppf = Format.formatter_of_out_channel ch in
      Format.fprintf ppf "digraph g {@\n";
      at_exit (fun () ->
          Format.fprintf ppf "@\n}@.";
          close_out ch);
      Some ppf)

let analyze ?print_name ~return_continuation ~exn_continuation
    ~code_age_relation ~used_value_slots
    ({ stack; map; extra; dummy_toplevel_cont } as t) : result =
  Profile.record_call ~accumulate:true "data_flow" (fun () ->
      assert (stack = []);
      assert (
        not
          (Continuation.name dummy_toplevel_cont
          = wrong_dummy_toplevel_cont_name));
      Format.eprintf "SOURCE:@\n%a@\n@." print t;

      (* Dead variable analysis *)
      let deps =
        Dependency_graph.create map extra ~return_continuation ~exn_continuation
          ~code_age_relation ~used_value_slots
      in
      Format.eprintf "/// graph@\n%a@\n@." Dependency_graph._print deps;
      let dead_variable_result = Dependency_graph.required_names deps in

      (* Aliases analysis *)
      let dom_graph =
        Dominator_graph.create map extra ~return_continuation ~exn_continuation
          ~required_names:dead_variable_result.required_names
      in
      let aliases = Dominator_graph.dominator_analysis dom_graph in
      let aliases_kind = Dominator_graph.aliases_kind dom_graph aliases in
      (match print_name with
      | None -> ()
      | Some print_name ->
        Option.iter
          (fun ppf ->
            incr r;
            Dominator_graph.Dot.print ~print_name ~ctx:!r ~doms:aliases ppf
              dom_graph)
          (Lazy.force dominator_graph_ppf));
      let control = Control_flow_graph.create ~dummy_toplevel_cont t in
      let extra_args_for_aliases =
        Control_flow_graph.compute_continuation_extra_args_for_aliases
          ~source_info:t aliases control
          ~required_names:dead_variable_result.required_names
      in
      (match print_name with
      | None -> ()
      | Some print_name ->
        Option.iter
          (fun ppf ->
            incr r;
            Control_flow_graph.Dot.print ~df:t ~print_name ~ctx:!r ppf
              ~return_continuation ~exn_continuation ~extra_args_for_aliases
              control)
          (Lazy.force control_flow_graph_ppf));

      (* Return *)
      let result =
        { dead_variable_result;
          continuation_param_aliases =
            { aliases; aliases_kind; extra_args_for_aliases }
        }
      in
      Format.eprintf "/// result@\n%a@\n@." _print_result result;
      result)
