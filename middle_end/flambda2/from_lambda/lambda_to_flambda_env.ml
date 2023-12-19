(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2023 OCamlPro SAS                                    *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type region_closure_continuation =
  { continuation_closing_region : Continuation.t;
    continuation_after_closing_region : Continuation.t
  }

type region_stack_element = Ident.t

let same_region = Ident.same

type t =
  { current_unit : Compilation_unit.t;
    current_values_of_mutables_in_scope :
      (Ident.t * Flambda_kind.With_subkind.t) Ident.Map.t;
    mutables_needed_by_continuations : Ident.Set.t Continuation.Map.t;
    unboxed_product_components_in_scope :
      ([`Complex] Flambda_arity.Component_for_creation.t
      * (Ident.t * Flambda_kind.With_subkind.t) array)
      Ident.Map.t;
    unboxed_product_components_needed_by_continuations :
      Ident.Set.t Continuation.Map.t;
    try_stack : Continuation.t list;
    try_stack_at_handler : Continuation.t list Continuation.Map.t;
    static_exn_continuation : Continuation.t Numeric_types.Int.Map.t;
    recursive_static_catches : Numeric_types.Int.Set.t;
    my_region : Ident.t;
    (* CR-someday ncourant: replace this with [my_region: Ident.t option] *)
    region_stack : region_stack_element list;
    region_stack_in_cont_scope : region_stack_element list Continuation.Map.t;
    region_closure_continuations : region_closure_continuation Ident.Map.t;
    ident_stamp_upon_starting : int
  }

let create ~current_unit ~return_continuation ~exn_continuation ~my_region =
  let mutables_needed_by_continuations =
    Continuation.Map.of_list
      [return_continuation, Ident.Set.empty; exn_continuation, Ident.Set.empty]
  in
  let id = Ident.create_local "unused" in
  let ident_stamp_upon_starting = Ident.stamp id in
  let unboxed_product_components_needed_by_continuations =
    Continuation.Map.of_list
      [return_continuation, Ident.Set.empty; exn_continuation, Ident.Set.empty]
  in
  { current_unit;
    current_values_of_mutables_in_scope = Ident.Map.empty;
    mutables_needed_by_continuations;
    unboxed_product_components_in_scope = Ident.Map.empty;
    unboxed_product_components_needed_by_continuations;
    try_stack = [];
    try_stack_at_handler = Continuation.Map.empty;
    static_exn_continuation = Numeric_types.Int.Map.empty;
    recursive_static_catches = Numeric_types.Int.Set.empty;
    my_region;
    region_stack = [];
    region_stack_in_cont_scope =
      Continuation.Map.singleton return_continuation [];
    region_closure_continuations = Ident.Map.empty;
    ident_stamp_upon_starting
  }

let current_unit t = t.current_unit

let ident_stamp_upon_starting t = t.ident_stamp_upon_starting

let is_mutable t id = Ident.Map.mem id t.current_values_of_mutables_in_scope

let register_mutable_variable t id kind =
  if Ident.Map.mem id t.current_values_of_mutables_in_scope
  then Misc.fatal_errorf "Redefinition of mutable variable %a" Ident.print id;
  let new_id = Ident.rename id in
  let current_values_of_mutables_in_scope =
    Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
  in
  let t = { t with current_values_of_mutables_in_scope } in
  t, new_id

let update_mutable_variable t id =
  match Ident.Map.find id t.current_values_of_mutables_in_scope with
  | exception Not_found ->
    Misc.fatal_errorf "Mutable variable %a not in environment" Ident.print id
  | _old_id, kind ->
    let new_id = Ident.rename id in
    let current_values_of_mutables_in_scope =
      Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
    in
    let t = { t with current_values_of_mutables_in_scope } in
    t, new_id

let mutables_in_scope t = Ident.Map.keys t.current_values_of_mutables_in_scope

let register_unboxed_product t ~unboxed_product ~before_unarization ~fields =
  { t with
    unboxed_product_components_in_scope =
      Ident.Map.add unboxed_product
        (before_unarization, Array.of_list fields)
        t.unboxed_product_components_in_scope
  }

let unboxed_product_components_in_scope t =
  Ident.Map.keys t.unboxed_product_components_in_scope

type add_continuation_result =
  { body_env : t;
    handler_env : t;
    extra_params : (Ident.t * Flambda_kind.With_subkind.t) list
  }

let add_continuation t cont ~push_to_try_stack (recursive : Asttypes.rec_flag) =
  let region_stack_in_cont_scope =
    Continuation.Map.add cont t.region_stack t.region_stack_in_cont_scope
  in
  let body_env =
    let mutables_needed_by_continuations =
      Continuation.Map.add cont (mutables_in_scope t)
        t.mutables_needed_by_continuations
    in
    let unboxed_product_components_needed_by_continuations =
      Continuation.Map.add cont
        (unboxed_product_components_in_scope t)
        t.unboxed_product_components_needed_by_continuations
    in
    let try_stack =
      if push_to_try_stack then cont :: t.try_stack else t.try_stack
    in
    { t with
      mutables_needed_by_continuations;
      unboxed_product_components_needed_by_continuations;
      try_stack;
      region_stack_in_cont_scope
    }
  in
  let current_values_of_mutables_in_scope =
    Ident.Map.mapi
      (fun mut_var (_outer_value, kind) -> Ident.rename mut_var, kind)
      t.current_values_of_mutables_in_scope
  in
  let unboxed_product_components_in_scope =
    Ident.Map.map
      (fun (before_unarization, fields) ->
        let fields =
          Array.map (fun (field, layout) -> Ident.rename field, layout) fields
        in
        before_unarization, fields)
      t.unboxed_product_components_in_scope
  in
  let handler_env =
    let handler_env =
      match recursive with
      | Nonrecursive -> t
      | Recursive ->
        if push_to_try_stack
        then Misc.fatal_error "Try continuations should not be recursive";
        body_env
    in
    { handler_env with
      current_values_of_mutables_in_scope;
      unboxed_product_components_in_scope;
      region_stack_in_cont_scope
    }
  in
  let extra_params_for_unboxed_products =
    Ident.Map.data handler_env.unboxed_product_components_in_scope
    |> List.map snd |> List.map Array.to_list |> List.concat
  in
  let extra_params =
    Ident.Map.data handler_env.current_values_of_mutables_in_scope
    @ extra_params_for_unboxed_products
  in
  { body_env; handler_env; extra_params }

let add_static_exn_continuation t static_exn cont =
  let t =
    { t with
      try_stack_at_handler =
        Continuation.Map.add cont t.try_stack t.try_stack_at_handler;
      static_exn_continuation =
        Numeric_types.Int.Map.add static_exn cont t.static_exn_continuation;
      region_stack_in_cont_scope =
        Continuation.Map.add cont t.region_stack t.region_stack_in_cont_scope
    }
  in
  let recursive : Asttypes.rec_flag =
    if Numeric_types.Int.Set.mem static_exn t.recursive_static_catches
    then Recursive
    else Nonrecursive
  in
  add_continuation t cont ~push_to_try_stack:false recursive

let get_static_exn_continuation t static_exn =
  match Numeric_types.Int.Map.find static_exn t.static_exn_continuation with
  | exception Not_found ->
    Misc.fatal_errorf "Unbound static exception %d" static_exn
  | continuation -> continuation

let mark_as_recursive_static_catch t static_exn =
  if Numeric_types.Int.Set.mem static_exn t.recursive_static_catches
  then
    Misc.fatal_errorf
      "Static catch with continuation %d already marked as recursive -- is it \
       being redefined?"
      static_exn;
  { t with
    recursive_static_catches =
      Numeric_types.Int.Set.add static_exn t.recursive_static_catches
  }

let is_static_exn_recursive t static_exn =
  Numeric_types.Int.Set.mem static_exn t.recursive_static_catches

let get_try_stack t = t.try_stack

let get_try_stack_at_handler t continuation =
  match Continuation.Map.find continuation t.try_stack_at_handler with
  | exception Not_found ->
    Misc.fatal_errorf "No try stack recorded for handler %a" Continuation.print
      continuation
  | stack -> stack

let extra_args_for_continuation_with_kinds t cont =
  let for_mutables =
    match Continuation.Map.find cont t.mutables_needed_by_continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a" Continuation.print cont
    | mutables ->
      let mutables = Ident.Set.elements mutables in
      List.map
        (fun mut ->
          match Ident.Map.find mut t.current_values_of_mutables_in_scope with
          | exception Not_found ->
            Misc.fatal_errorf "No current value for %a" Ident.print mut
          | current_value, kind -> current_value, kind)
        mutables
  in
  let for_unboxed_products =
    match
      Continuation.Map.find cont
        t.unboxed_product_components_needed_by_continuations
    with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a" Continuation.print cont
    | unboxed_products_to_fields ->
      let unboxed_products = Ident.Set.elements unboxed_products_to_fields in
      List.concat_map
        (fun unboxed_product ->
          match
            Ident.Map.find unboxed_product t.unboxed_product_components_in_scope
          with
          | exception Not_found ->
            Misc.fatal_errorf "No field list registered for unboxed product %a"
              Ident.print unboxed_product
          | _, fields -> Array.to_list fields)
        unboxed_products
  in
  for_mutables @ for_unboxed_products

let extra_args_for_continuation t cont =
  List.map fst (extra_args_for_continuation_with_kinds t cont)

let get_mutable_variable_with_kind t id =
  match Ident.Map.find id t.current_values_of_mutables_in_scope with
  | exception Not_found ->
    Misc.fatal_errorf "Mutable variable %a not bound in env" Ident.print id
  | id, kind -> id, kind

let get_unboxed_product_fields t id =
  match Ident.Map.find id t.unboxed_product_components_in_scope with
  | exception Not_found -> None
  | before_unarization, fields ->
    Some (before_unarization, List.map fst (Array.to_list fields))

let entering_region t id ~continuation_closing_region
    ~continuation_after_closing_region =
  { t with
    region_stack = id :: t.region_stack;
    region_closure_continuations =
      Ident.Map.add id
        { continuation_closing_region; continuation_after_closing_region }
        t.region_closure_continuations
  }

let leaving_region t =
  match t.region_stack with
  | [] -> Misc.fatal_error "Cannot pop region, region stack is empty"
  | _ :: region_stack -> { t with region_stack }

let current_region t =
  if not (Flambda_features.stack_allocation_enabled ())
  then t.my_region
  else match t.region_stack with [] -> t.my_region | region :: _ -> region

let my_region t = t.my_region

let region_stack t = t.region_stack

let region_stack_in_cont_scope t continuation =
  match Continuation.Map.find continuation t.region_stack_in_cont_scope with
  | exception Not_found ->
    Misc.fatal_errorf "No region stack recorded for handler %a"
      Continuation.print continuation
  | stack -> stack

let pop_region = function [] -> None | region :: rest -> Some (region, rest)

let pop_regions_up_to_context t continuation =
  let initial_stack_context = region_stack_in_cont_scope t continuation in
  let rec pop to_pop region_stack =
    match initial_stack_context, region_stack with
    | [], [] -> to_pop
    | [], region :: regions -> pop (Some region) regions
    | _initial_stack_top :: _, [] ->
      Misc.fatal_errorf "Unable to restore region stack for %a"
        Continuation.print continuation
    | initial_stack_top :: _, region :: regions ->
      if Ident.same initial_stack_top region
      then to_pop
      else pop (Some region) regions
  in
  pop None t.region_stack

let region_closure_continuation t id =
  try Ident.Map.find id t.region_closure_continuations
  with Not_found ->
    Misc.fatal_errorf "No region closure continuation found for %a" Ident.print
      id
