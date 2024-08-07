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

module Region_stack_element : sig
  type t

  val create : region:Ident.t -> ghost_region:Ident.t -> t

  val region : t -> Ident.t

  val ghost_region : t -> Ident.t

  include Container_types.S with type t := t
end = struct
  module T0 = struct
    type t =
      { region : Ident.t;
        ghost_region : Ident.t
      }

    let print ppf t =
      Format.fprintf ppf "@[<hov 1>((region@ %a)@ (ghost_region@ %a))@]"
        Ident.print t.region Ident.print t.ghost_region

    let equal { region = region1; ghost_region = ghost_region1 }
        { region = region2; ghost_region = ghost_region2 } =
      Ident.same region1 region2 && Ident.same ghost_region1 ghost_region2

    let compare { region = region1; ghost_region = ghost_region1 }
        { region = region2; ghost_region = ghost_region2 } =
      let c = Ident.compare region1 region2 in
      if c <> 0 then c else Ident.compare ghost_region1 ghost_region2

    let hash { region; ghost_region } =
      Hashtbl.hash (Ident.hash region, Ident.hash ghost_region)
  end

  include T0
  include Container_types.Make (T0)

  let create ~region ~ghost_region = { region; ghost_region }

  let region t = t.region

  let ghost_region t = t.ghost_region
end

type t =
  { current_unit : Compilation_unit.t;
    current_values_of_mutables_in_scope :
      (Ident.t * Flambda_kind.With_subkind.t) Ident.Map.t;
    mutables_needed_by_continuations : Ident.Set.t Continuation.Map.t;
    unboxed_product_components_in_scope :
      ([`Complex] Flambda_arity.Component_for_creation.t * Ident.t list)
      Ident.Map.t;
    try_stack : Continuation.t list;
    try_stack_at_handler : Continuation.t list Continuation.Map.t;
    static_exn_continuation : Continuation.t Numeric_types.Int.Map.t;
    recursive_static_catches : Numeric_types.Int.Set.t;
    my_region : Region_stack_element.t;
    (* CR-someday ncourant/mshinwell: replace this with [my_region:
       Region_stack_element.t option] *)
    region_stack : Region_stack_element.t list;
    region_stack_in_cont_scope : Region_stack_element.t list Continuation.Map.t;
    region_closure_continuations :
      region_closure_continuation Region_stack_element.Map.t;
    ident_stamp_upon_starting : int
  }

let create ~current_unit ~return_continuation ~exn_continuation ~my_region =
  let mutables_needed_by_continuations =
    Continuation.Map.of_list
      [return_continuation, Ident.Set.empty; exn_continuation, Ident.Set.empty]
  in
  let id = Ident.create_local "unused" in
  let ident_stamp_upon_starting = Ident.stamp id in
  { current_unit;
    current_values_of_mutables_in_scope = Ident.Map.empty;
    mutables_needed_by_continuations;
    unboxed_product_components_in_scope = Ident.Map.empty;
    try_stack = [];
    try_stack_at_handler = Continuation.Map.empty;
    static_exn_continuation = Numeric_types.Int.Map.empty;
    recursive_static_catches = Numeric_types.Int.Set.empty;
    my_region;
    region_stack = [];
    region_stack_in_cont_scope =
      Continuation.Map.singleton return_continuation [];
    region_closure_continuations = Region_stack_element.Map.empty;
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
        (before_unarization, fields)
        t.unboxed_product_components_in_scope
  }

let register_unboxed_product_with_kinds t ~unboxed_product ~before_unarization
    ~fields =
  register_unboxed_product t ~unboxed_product ~before_unarization
    ~fields:(List.map fst fields)

type add_continuation_result =
  { body_env : t;
    handler_env : t;
    extra_params : (Ident.t * Flambda_kind.With_subkind.t) list
  }

let add_continuation t cont ~push_to_try_stack ~pop_region
    (recursive : Asttypes.rec_flag) =
  let region_stack =
    if pop_region
    then
      match t.region_stack with
      | [] -> Misc.fatal_error "Cannot pop region, region stack is empty"
      | _ :: region_stack -> region_stack
    else t.region_stack
  in
  let region_stack_in_cont_scope =
    Continuation.Map.add cont region_stack t.region_stack_in_cont_scope
  in
  let body_env =
    let mutables_needed_by_continuations =
      Continuation.Map.add cont (mutables_in_scope t)
        t.mutables_needed_by_continuations
    in
    let try_stack =
      if push_to_try_stack then cont :: t.try_stack else t.try_stack
    in
    { t with
      mutables_needed_by_continuations;
      try_stack;
      region_stack_in_cont_scope
    }
  in
  let current_values_of_mutables_in_scope =
    Ident.Map.mapi
      (fun mut_var (_outer_value, kind) -> Ident.rename mut_var, kind)
      t.current_values_of_mutables_in_scope
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
      region_stack_in_cont_scope;
      region_stack
    }
  in
  let extra_params =
    Ident.Map.data handler_env.current_values_of_mutables_in_scope
  in
  { body_env; handler_env; extra_params }

let add_static_exn_continuation t static_exn ~pop_region cont =
  let t =
    { t with
      try_stack_at_handler =
        Continuation.Map.add cont t.try_stack t.try_stack_at_handler;
      static_exn_continuation =
        Numeric_types.Int.Map.add static_exn cont t.static_exn_continuation
    }
  in
  let recursive : Asttypes.rec_flag =
    if Numeric_types.Int.Set.mem static_exn t.recursive_static_catches
    then Recursive
    else Nonrecursive
  in
  add_continuation t cont ~push_to_try_stack:false ~pop_region recursive

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
  | before_unarization, fields -> Some (before_unarization, fields)

let entering_region t region_stack_element ~continuation_closing_region
    ~continuation_after_closing_region =
  { t with
    region_stack = region_stack_element :: t.region_stack;
    region_closure_continuations =
      Region_stack_element.Map.add region_stack_element
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
  else
    match t.region_stack with
    | [] -> t.my_region
    | region_stack_elt :: _ -> region_stack_elt

let parent_region t =
  if not (Flambda_features.stack_allocation_enabled ())
  then t.my_region
  else
    match t.region_stack with
    | [] ->
      Misc.fatal_error "Cannot determine parent region, region stack is empty"
    | [_] -> t.my_region
    | _ :: region :: _ -> region

let my_region t = t.my_region

let region_stack t = t.region_stack

let region_stack_in_cont_scope t continuation =
  match Continuation.Map.find continuation t.region_stack_in_cont_scope with
  | exception Not_found ->
    Misc.fatal_errorf "No region stack recorded for handler %a"
      Continuation.print continuation
  | stack -> stack

let pop_region = function [] -> None | region :: rest -> Some (region, rest)

let pop_one_region t =
  if not (Flambda_features.stack_allocation_enabled ())
  then t, t.my_region
  else
    match t.region_stack with
    | [] -> Misc.fatal_error "No regions available to pop"
    | region :: region_stack -> { t with region_stack }, region

let pop_regions_up_to_context t continuation =
  let initial_stack_context = region_stack_in_cont_scope t continuation in
  let rec pop to_pop region_stack =
    match initial_stack_context, region_stack with
    | [], [] -> to_pop
    | [], region_stack_elt :: regions -> pop (Some region_stack_elt) regions
    | _initial_stack_top :: _, [] ->
      Misc.fatal_errorf "Unable to restore region stack for %a"
        Continuation.print continuation
    | initial_stack_top :: _, region_stack_elt :: regions ->
      if Region_stack_element.equal initial_stack_top region_stack_elt
      then to_pop
      else pop (Some region_stack_elt) regions
  in
  pop None t.region_stack

let region_closure_continuation t id =
  try Region_stack_element.Map.find id t.region_closure_continuations
  with Not_found ->
    Misc.fatal_errorf "No region closure continuation found for %a"
      Region_stack_element.print id
