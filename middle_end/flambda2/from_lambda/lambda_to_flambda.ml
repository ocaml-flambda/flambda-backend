(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Use CPS". -- A. Kennedy, "Compiling with Continuations Continued", ICFP
   2007. *)

module L = Lambda
module CC = Closure_conversion
module P = Flambda_primitive
module IR = Closure_conversion.IR
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Function_decl = Closure_conversion_aux.Function_decls.Function_decl

module Env : sig
  type t

  type region_stack_element = private
    | Regular of Ident.t
    | Try_with of Ident.t

  val same_region : region_stack_element -> region_stack_element -> bool

  val create :
    current_unit:Compilation_unit.t ->
    return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    my_region:Ident.t ->
    t

  val current_unit : t -> Compilation_unit.t

  val ident_stamp_upon_starting : t -> int

  val is_mutable : t -> Ident.t -> bool

  val register_mutable_variable : t -> Ident.t -> Lambda.layout -> t * Ident.t

  val update_mutable_variable : t -> Ident.t -> t * Ident.t

  type add_continuation_result = private
    { body_env : t;
      handler_env : t;
      extra_params : (Ident.t * Lambda.layout) list
    }

  val add_continuation :
    t ->
    Continuation.t ->
    push_to_try_stack:bool ->
    Asttypes.rec_flag ->
    add_continuation_result

  val add_static_exn_continuation :
    t -> int -> Continuation.t -> add_continuation_result

  val get_static_exn_continuation : t -> int -> Continuation.t

  val mark_as_recursive_static_catch : t -> int -> t

  val is_static_exn_recursive : t -> int -> bool

  val get_try_stack : t -> Continuation.t list

  val get_try_stack_at_handler : t -> Continuation.t -> Continuation.t list

  val extra_args_for_continuation : t -> Continuation.t -> Ident.t list

  val extra_args_for_continuation_with_kinds :
    t -> Continuation.t -> (Ident.t * Lambda.layout) list

  val get_mutable_variable : t -> Ident.t -> Ident.t

  val get_mutable_variable_with_kind : t -> Ident.t -> Ident.t * Lambda.layout

  (** About local allocation regions:

      In this pass, we have to transform [Lregion] expressions in Lambda to
      primitives that mark the opening and closing of stack regions. We need to
      ensure regions are always closed so as to not leak out of their scope.
      They must also never be closed twice.

      Several nested regions can be closed with one primitive as [End_region id]
      which will close [id] and every other region opened in its scope. As such,
      the transformation doesn't need to generate strict pairings of
      [Begin_region] and [End_region] in every case. We may jump out of the
      scope of several regions at once, in particular with exception raises from
      [Lstaticraise].

      Another case requiring attention is function calls in tail position for
      which we may need to add an [End_region] before the jump.

      This implementation works as follows.

      For normal control flow, following the block structure of Lambda
      expressions, we insert a new continuation (called the "region closure
      continuation") upon encountering [Begin_region]; then at every leaf we
      cause the control flow to jump via that continuation. The region closure
      continuation closes the relevant region before jumping to what would have
      been the "real" continuation of the leaf expressions in question. The
      insertion of the continuation avoids duplication of the [End_region]
      constructs. (We only need one [Begin_region] per region, but potentially
      as many [End_region]s as there are leaves in the subsequent term.)

      For exceptional control flow, the region closure continuation is not used;
      instead, a region is opened before the beginning of a Trywith, so that we
      can use this region to close every subsequent regions opened in its scope
      at the beginning of the handler.

      Likewise, when regions must be closed explicitly prior to tail calls to
      avoid leaking memory on the local allocation stack, the closure
      continuation is also not used in favour of explicit insertion of
      [End_region] operations.

      Region closure continuations are created alongside corresponding
      [Begin_region]s in the [Lregion] cases of [cps_non_tail] and [cps_tail].
      The decision as to calling a closure continuation or adding explicit
      [End_region]s is done in [restore_continuation_context] and
      [wrap_return_continuation]. Exceptional control flow cases are handled by
      the [compile_staticfail] and [Ltrywith] cases of the main transformation
      functions. *)

  val entering_region :
    t ->
    Ident.t ->
    continuation_closing_region:Continuation.t ->
    continuation_after_closing_region:Continuation.t ->
    t

  val leaving_region : t -> t

  val entering_try_region : t -> Ident.t -> t

  val leaving_try_region : t -> t

  val current_region : t -> Ident.t

  val my_region : t -> Ident.t

  (** The innermost (newest) region is first in the list. *)
  val region_stack : t -> region_stack_element list

  val region_stack_in_cont_scope :
    t -> Continuation.t -> region_stack_element list

  (** Hack for staticfail (which should eventually use
      [pop_regions_up_to_context]) *)
  val pop_region :
    region_stack_element list ->
    (region_stack_element * region_stack_element list) option

  val pop_regions_up_to_context : t -> Continuation.t -> Ident.t option

  type region_closure_continuation =
    { continuation_closing_region : Continuation.t;
      continuation_after_closing_region : Continuation.t
    }

  val region_closure_continuation : t -> Ident.t -> region_closure_continuation
end = struct
  type region_closure_continuation =
    { continuation_closing_region : Continuation.t;
      continuation_after_closing_region : Continuation.t
    }

  type region_stack_element =
    | Regular of Ident.t
    | Try_with of Ident.t

  let same_region region1 region2 =
    match region1, region2 with
    | Regular _, Try_with _ | Try_with _, Regular _ -> false
    | Regular id1, Regular id2 | Try_with id1, Try_with id2 ->
      Ident.same id1 id2

  type t =
    { current_unit : Compilation_unit.t;
      current_values_of_mutables_in_scope :
        (Ident.t * Lambda.layout) Ident.Map.t;
      mutables_needed_by_continuations : Ident.Set.t Continuation.Map.t;
      try_stack : Continuation.t list;
      try_stack_at_handler : Continuation.t list Continuation.Map.t;
      static_exn_continuation : Continuation.t Numeric_types.Int.Map.t;
      recursive_static_catches : Numeric_types.Int.Set.t;
      my_region : Ident.t;
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
    { current_unit;
      current_values_of_mutables_in_scope = Ident.Map.empty;
      mutables_needed_by_continuations;
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

  type add_continuation_result =
    { body_env : t;
      handler_env : t;
      extra_params : (Ident.t * Lambda.layout) list
    }

  let add_continuation t cont ~push_to_try_stack (recursive : Asttypes.rec_flag)
      =
    let region_stack_in_cont_scope =
      Continuation.Map.add cont t.region_stack t.region_stack_in_cont_scope
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
        region_stack_in_cont_scope
      }
    in
    let extra_params =
      Ident.Map.data handler_env.current_values_of_mutables_in_scope
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
        "Static catch with continuation %d already marked as recursive -- is \
         it being redefined?"
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
      Misc.fatal_errorf "No try stack recorded for handler %a"
        Continuation.print continuation
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

  let get_mutable_variable t id = fst (get_mutable_variable_with_kind t id)

  let entering_region t id ~continuation_closing_region
      ~continuation_after_closing_region =
    { t with
      region_stack = Regular id :: t.region_stack;
      region_closure_continuations =
        Ident.Map.add id
          { continuation_closing_region; continuation_after_closing_region }
          t.region_closure_continuations
    }

  let leaving_region t =
    match t.region_stack with
    | [] -> Misc.fatal_error "Cannot pop region, region stack is empty"
    | Regular _ :: region_stack -> { t with region_stack }
    | Try_with region :: _ ->
      Misc.fatal_errorf "Attempted to pop region but found try region %a"
        Ident.print region

  let entering_try_region t region =
    { t with region_stack = Try_with region :: t.region_stack }

  let leaving_try_region t =
    match t.region_stack with
    | [] -> Misc.fatal_error "Cannot pop try region, region stack is empty"
    | Try_with _ :: region_stack -> { t with region_stack }
    | Regular region :: _ ->
      Misc.fatal_errorf
        "Attempted to pop try region but found regular region %a" Ident.print
        region

  let current_region t =
    if not (Flambda_features.stack_allocation_enabled ())
    then t.my_region
    else
      match t.region_stack with
      | [] -> t.my_region
      | (Regular region | Try_with region) :: _ -> region

  let my_region t = t.my_region

  let region_stack t = t.region_stack

  let region_stack_in_cont_scope t continuation =
    match Continuation.Map.find continuation t.region_stack_in_cont_scope with
    | exception Not_found ->
      Misc.fatal_errorf "No region stack recorded for handler %a"
        Continuation.print continuation
    | stack -> stack

  let pop_region = function
    | [] -> None
    | ((Try_with _ | Regular _) as region) :: rest -> Some (region, rest)

  let pop_regions_up_to_context t continuation =
    let initial_stack_context = region_stack_in_cont_scope t continuation in
    let rec pop to_pop region_stack =
      match initial_stack_context, region_stack with
      | [], [] -> to_pop
      | ([] | Try_with _ :: _), Regular region :: regions ->
        pop (Some region) regions
      | ([] | Regular _ :: _), Try_with _ :: regions -> pop to_pop regions
      | _initial_stack_top :: _, [] ->
        Misc.fatal_errorf "Unable to restore region stack for %a"
          Continuation.print continuation
      | Regular initial_stack_top :: _, Regular region :: regions ->
        if Ident.same initial_stack_top region
        then to_pop
        else pop (Some region) regions
      | Try_with initial_stack_top :: _, Try_with region :: regions ->
        if Ident.same initial_stack_top region
        then to_pop
        else pop to_pop regions
    in
    pop None t.region_stack

  let region_closure_continuation t id =
    try Ident.Map.find id t.region_closure_continuations
    with Not_found ->
      Misc.fatal_errorf "No region closure continuation found for %a"
        Ident.print id
end

module CCenv = Closure_conversion_aux.Env

(* CR pchambart: Replace uses by CC.Acc.t *)
module Acc = Closure_conversion_aux.Acc

type primitive_transform_result =
  | Primitive of L.primitive * L.lambda list * L.scoped_location
  | Transformed of L.lambda

let print_compact_location ppf (loc : Location.t) =
  if loc.loc_start.pos_fname = "//toplevel//"
  then ()
  else
    let file, line, startchar = Location.get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "%a:%i" Location.print_filename file line;
    if startchar >= 0 then Format.fprintf ppf ",%i--%i" startchar endchar

let name_for_function (func : Lambda.lfunction) =
  (* Name anonymous functions by their source location, if known. *)
  match func.loc with
  | Loc_unknown -> "anon-fn"
  | Loc_known { loc; _ } ->
    Format.asprintf "anon-fn[%a]" print_compact_location loc

let extra_args_for_exn_continuation env exn_handler =
  let more_extra_args =
    Env.extra_args_for_continuation_with_kinds env exn_handler
  in
  List.map
    (fun (arg, kind) : (IR.simple * _) ->
      Var arg, Flambda_kind.With_subkind.from_lambda kind)
    more_extra_args

let _print_stack ppf stack =
  Format.fprintf ppf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
       (fun ppf (_id, cont) -> Format.fprintf ppf "%a" Continuation.print cont))
    stack

(* Uses of [Lstaticfail] that jump out of try-with handlers need special care:
   the correct number of pop trap operations must be inserted. A similar thing
   is also necessary for closing local allocation regions. *)
let compile_staticfail acc env ccenv ~(continuation : Continuation.t) ~args :
    Expr_with_acc.t =
  let try_stack_at_handler = Env.get_try_stack_at_handler env continuation in
  let try_stack_now = Env.get_try_stack env in
  let dbg =
    Debuginfo.none
    (* CR mshinwell: should probably be fixed in Lambda (on Lstaticraise) *)
  in
  if List.length try_stack_at_handler > List.length try_stack_now
  then
    Misc.fatal_errorf
      "Cannot jump to continuation %a: it would involve jumping into a \
       try-with body"
      Continuation.print continuation;
  assert (
    Continuation.Set.subset
      (Continuation.Set.of_list try_stack_at_handler)
      (Continuation.Set.of_list try_stack_now));
  let rec add_pop_traps acc ~try_stack_now =
    let add_pop cont ~try_stack_now after_pop =
      let mk_remaining_traps acc = add_pop_traps acc ~try_stack_now in
      let wrapper_cont = Continuation.create () in
      let trap_action : IR.trap_action = Pop { exn_handler = cont } in
      let handler = mk_remaining_traps acc after_pop in
      let body acc ccenv =
        CC.close_apply_cont acc ccenv ~dbg wrapper_cont (Some trap_action) []
      in
      fun acc env ->
        CC.close_let_cont acc env ~name:wrapper_cont ~is_exn_handler:false
          ~params:[] ~recursive:Nonrecursive ~body ~handler
    in
    let no_pop after_pop = after_pop in
    match try_stack_now, try_stack_at_handler with
    | [], [] -> no_pop
    | cont1 :: try_stack_now, cont2 :: _ ->
      if Continuation.equal cont1 cont2
      then no_pop
      else add_pop cont1 ~try_stack_now
    | cont :: try_stack_now, [] -> add_pop cont ~try_stack_now
    | [], _ :: _ -> assert false
    (* see above *)
  in
  let region_stack_at_handler =
    Env.region_stack_in_cont_scope env continuation
  in
  let region_stack_now = Env.region_stack env in
  if List.length region_stack_at_handler > List.length region_stack_now
  then
    Misc.fatal_errorf
      "Cannot jump to continuation %a: it would involve jumping into a local \
       allocation region"
      Continuation.print continuation;
  let rec add_end_regions acc ~region_stack_now =
    (* This can maybe only be exercised right now using "match with exception",
       since that causes jumps out of try-regions (but not normal regions). *)
    (* CR pchambart: This closes all the regions between region_stack_now and
       region_stack_at_handler, but closing only the last one should be
       sufficient. *)
    let add_end_region (region : Env.region_stack_element) ~region_stack_now
        after_everything =
      let add_remaining_end_regions acc =
        add_end_regions acc ~region_stack_now
      in
      let body = add_remaining_end_regions acc after_everything in
      fun acc ccenv ->
        match region with
        | Try_with _ -> body acc ccenv
        | Regular region_ident ->
          CC.close_let acc ccenv
            [ ( Ident.create_local "unit",
                Flambda_kind.With_subkind.tagged_immediate ) ]
            Not_user_visible (End_region region_ident) ~body
    in
    let no_end_region after_everything = after_everything in
    match
      Env.pop_region region_stack_now, Env.pop_region region_stack_at_handler
    with
    | None, None -> no_end_region
    | Some (region1, region_stack_now), Some (region2, _) ->
      if Env.same_region region1 region2
      then no_end_region
      else add_end_region region1 ~region_stack_now
    | Some (((Regular _ | Try_with _) as region), region_stack_now), None ->
      add_end_region region ~region_stack_now
    | None, Some _ -> assert false
    (* see above *)
  in
  add_pop_traps acc ~try_stack_now
    (fun acc ccenv ->
      add_end_regions acc ~region_stack_now
        (fun acc ccenv ->
          CC.close_apply_cont acc ccenv ~dbg continuation None args)
        acc ccenv)
    acc ccenv

let switch_for_if_then_else ~cond ~ifso ~ifnot ~kind =
  let switch : Lambda.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None
    }
  in
  L.Lswitch (cond, switch, Loc_unknown, kind)

let transform_primitive env (prim : L.primitive) args loc =
  match prim, args with
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    Transformed
      (L.Llet
         ( Strict,
           Lambda.layout_int,
           const_true,
           Lconst (Const_base (Const_int 1)),
           L.Llet
             ( Strict,
               Lambda.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond)
                 ~ifso:(L.Lvar const_true) ~ifnot:arg2 ~kind:Lambda.layout_int
             ) ))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    Transformed
      (L.Llet
         ( Strict,
           Lambda.layout_int,
           const_false,
           Lconst (Const_base (Const_int 0)),
           L.Llet
             ( Strict,
               Lambda.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond) ~ifso:arg2
                 ~ifnot:(L.Lvar const_false) ~kind:Lambda.layout_int ) ))
  | (Psequand | Psequor), _ ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | ( (Pbytes_to_string | Pbytes_of_string | Parray_of_iarray | Parray_to_iarray),
      [arg] ) ->
    Transformed arg
  | Pignore, [arg] ->
    let result = L.Lconst (Const_base (Const_int 0)) in
    Transformed (L.Lsequence (arg, result))
  | Pfield _, [L.Lprim (Pgetglobal cu, [], _)]
    when Compilation_unit.equal cu (Env.current_unit env) ->
    Misc.fatal_error
      "[Pfield (Pgetglobal ...)] for the current compilation unit is forbidden \
       upon entry to the middle end"
  | Psetfield (_, _, _), [L.Lprim (Pgetglobal _, [], _); _] ->
    Misc.fatal_error
      "[Psetfield (Pgetglobal ...)] is forbidden upon entry to the middle end"
  | Pfield (index, _), _ when index < 0 ->
    Misc.fatal_error "Pfield with negative field index"
  | Pfloatfield (i, _, _), _ when i < 0 ->
    Misc.fatal_error "Pfloatfield with negative field index"
  | Psetfield (index, _, _), _ when index < 0 ->
    Misc.fatal_error "Psetfield with negative field index"
  | Pmakeblock (tag, _, _, _), _ when tag < 0 || tag >= Obj.no_scan_tag ->
    Misc.fatal_errorf "Pmakeblock with wrong or non-scannable block tag %d" tag
  | Pmakefloatblock (_mut, _mode), args when List.length args < 1 ->
    Misc.fatal_errorf "Pmakefloatblock must have at least one argument"
  | Pfloatcomp CFnlt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFlt, args, loc)], loc)
  | Pfloatcomp CFngt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFgt, args, loc)], loc)
  | Pfloatcomp CFnle, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFle, args, loc)], loc)
  | Pfloatcomp CFnge, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFge, args, loc)], loc)
  | Pbigarrayref (_unsafe, num_dimensions, kind, layout), args -> (
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some _, Some _ -> Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3
      then
        let arity = 1 + num_dimensions in
        let name = "caml_ba_get_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple_on_values ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      else
        Misc.fatal_errorf
          "Lambda_to_flambda.transform_primitive: Pbigarrayref with unknown \
           layout and elements should only have dimensions between 1 and 3 \
           (see translprim).")
  | Pbigarrayset (_unsafe, num_dimensions, kind, layout), args -> (
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some _, Some _ -> Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3
      then
        let arity = 2 + num_dimensions in
        let name = "caml_ba_set_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple_on_values ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      else
        Misc.fatal_errorf
          "Lambda_to_flambda.transform_primitive: Pbigarrayset with unknown \
           layout and elements should only have dimensions between 1 and 3 \
           (see translprim).")
  | _, _ -> Primitive (prim, args, loc)
  [@@ocaml.warning "-fragile-match"]

let rec_catch_for_while_loop env cond body =
  let cont = L.next_raise_count () in
  let env = Env.mark_as_recursive_static_catch env cont in
  let cond_result = Ident.create_local "while_cond_result" in
  let lam : L.lambda =
    Lstaticcatch
      ( Lstaticraise (cont, []),
        (cont, []),
        Llet
          ( Strict,
            Lambda.layout_int,
            cond_result,
            cond,
            Lifthenelse
              ( Lvar cond_result,
                Lsequence (body, Lstaticraise (cont, [])),
                Lconst (Const_base (Const_int 0)),
                Lambda.layout_unit ) ),
        Lambda.layout_unit )
  in
  env, lam

let rec_catch_for_for_loop env ident start stop (dir : Asttypes.direction_flag)
    body =
  let cont = L.next_raise_count () in
  let env = Env.mark_as_recursive_static_catch env cont in
  let start_ident = Ident.create_local "for_start" in
  let stop_ident = Ident.create_local "for_stop" in
  let first_test : L.lambda =
    match dir with
    | Upto ->
      Lprim (Pintcomp Cle, [L.Lvar start_ident; L.Lvar stop_ident], Loc_unknown)
    | Downto ->
      Lprim (Pintcomp Cge, [L.Lvar start_ident; L.Lvar stop_ident], Loc_unknown)
  in
  let subsequent_test : L.lambda =
    Lprim (Pintcomp Cne, [L.Lvar ident; L.Lvar stop_ident], Loc_unknown)
  in
  let one : L.lambda = Lconst (Const_base (Const_int 1)) in
  let next_value_of_counter =
    match dir with
    | Upto -> L.Lprim (Paddint, [L.Lvar ident; one], Loc_unknown)
    | Downto -> L.Lprim (Psubint, [L.Lvar ident; one], Loc_unknown)
  in
  let lam : L.lambda =
    (* Care needs to be taken here not to cause overflow if, for an incrementing
       for-loop, the upper bound is [max_int]; likewise, for a decrementing
       for-loop, if the lower bound is [min_int]. *)
    Llet
      ( Strict,
        Lambda.layout_int,
        start_ident,
        start,
        Llet
          ( Strict,
            Lambda.layout_int,
            stop_ident,
            stop,
            Lifthenelse
              ( first_test,
                Lstaticcatch
                  ( Lstaticraise (cont, [L.Lvar start_ident]),
                    (cont, [ident, Lambda.layout_int]),
                    Lsequence
                      ( body,
                        Lifthenelse
                          ( subsequent_test,
                            Lstaticraise (cont, [next_value_of_counter]),
                            L.lambda_unit,
                            Lambda.layout_unit ) ),
                    Lambda.layout_unit ),
                L.lambda_unit,
                Lambda.layout_unit ) ) )
  in
  env, lam

let is_user_visible env id : IR.user_visible =
  if Ident.stamp id >= Env.ident_stamp_upon_starting env
  then Not_user_visible
  else
    let name = Ident.name id in
    if String.starts_with ~prefix:"*opt*" name
    then User_visible
    else
      let len = String.length name in
      if len > 0 && Char.equal name.[0] '*'
      then Not_user_visible
      else User_visible

let let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler
    ~params
    ~(body : Acc.t -> Env.t -> CCenv.t -> Continuation.t -> Expr_with_acc.t)
    ~(handler : Acc.t -> Env.t -> CCenv.t -> Expr_with_acc.t) : Expr_with_acc.t
    =
  let cont = Continuation.create () in
  let { Env.body_env; handler_env; extra_params } =
    Env.add_continuation env cont ~push_to_try_stack:is_exn_handler Nonrecursive
  in
  let params =
    List.map
      (fun (id, visible, kind) ->
        id, visible, Flambda_kind.With_subkind.from_lambda kind)
      params
  in
  let extra_params =
    List.map
      (fun (id, kind) ->
        id, is_user_visible env id, Flambda_kind.With_subkind.from_lambda kind)
      extra_params
  in
  let handler acc ccenv = handler acc handler_env ccenv in
  let body acc ccenv = body acc body_env ccenv cont in
  CC.close_let_cont acc ccenv ~name:cont ~is_exn_handler
    ~params:(params @ extra_params) ~recursive:Nonrecursive ~body ~handler

let restore_continuation_context acc env ccenv cont ~close_early body =
  match Env.pop_regions_up_to_context env cont with
  | None -> body acc ccenv cont
  | Some region ->
    (* If we need to close regions early then do it now; otherwise redirect the
       return continuation to the one closing such regions, if any exist. See
       comment in [cps] on the [Lregion] case. *)
    if close_early
    then
      CC.close_let acc ccenv
        [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
        Not_user_visible (End_region region)
        ~body:(fun acc ccenv -> body acc ccenv cont)
    else
      let ({ continuation_closing_region; continuation_after_closing_region }
            : Env.region_closure_continuation) =
        Env.region_closure_continuation env region
      in
      if not (Continuation.equal cont continuation_after_closing_region)
      then
        Misc.fatal_errorf
          "The continuation %a following the region closure should be the \
           current continuation %a"
          Continuation.print continuation_after_closing_region
          Continuation.print cont;
      body acc ccenv continuation_closing_region

let restore_continuation_context_for_switch_arm env cont =
  match Env.pop_regions_up_to_context env cont with
  | None -> cont
  | Some region ->
    let ({ continuation_closing_region; continuation_after_closing_region }
          : Env.region_closure_continuation) =
      Env.region_closure_continuation env region
    in
    if not (Continuation.equal cont continuation_after_closing_region)
    then
      Misc.fatal_errorf
        "The continuation %a following the region closure should be the \
         current continuation %a"
        Continuation.print continuation_after_closing_region Continuation.print
        cont;
    continuation_closing_region

let apply_cont_with_extra_args acc env ccenv ~dbg cont traps args =
  let extra_args =
    List.map
      (fun var : IR.simple -> Var var)
      (Env.extra_args_for_continuation env cont)
  in
  restore_continuation_context acc env ccenv cont ~close_early:false
    (fun acc ccenv cont ->
      CC.close_apply_cont acc ~dbg ccenv cont traps (args @ extra_args))

let wrap_return_continuation acc env ccenv (apply : IR.apply) =
  let extra_args = Env.extra_args_for_continuation env apply.continuation in
  let close_early, region =
    match apply.region_close with
    | Rc_normal | Rc_nontail -> false, apply.region
    | Rc_close_at_apply -> true, Env.my_region env
  in
  let body acc ccenv continuation =
    match extra_args with
    | [] -> CC.close_apply acc ccenv { apply with continuation; region }
    | _ :: _ ->
      let wrapper_cont = Continuation.create () in
      let return_value = Ident.create_local "return_val" in
      let args =
        List.map (fun var : IR.simple -> Var var) (return_value :: extra_args)
      in
      let dbg = Debuginfo.none in
      let handler acc ccenv =
        CC.close_apply_cont acc ccenv ~dbg continuation None args
      in
      let body acc ccenv =
        CC.close_apply acc ccenv
          { apply with continuation = wrapper_cont; region }
      in
      let return_arity =
        match Flambda_arity.to_list apply.return_arity with
        | [return_kind] -> return_kind
        | _ :: _ ->
          Misc.fatal_errorf
            "Multiple return values for application of %a not supported yet"
            Ident.print apply.func
        | [] ->
          Misc.fatal_errorf "Nullary return arity for application of %a"
            Ident.print apply.func
      in
      CC.close_let_cont acc ccenv ~name:wrapper_cont ~is_exn_handler:false
        ~params:[return_value, Not_user_visible, return_arity]
        ~recursive:Nonrecursive ~body ~handler
  in
  restore_continuation_context acc env ccenv apply.continuation ~close_early
    body

let primitive_can_raise (prim : Lambda.primitive) =
  match prim with
  | Pccall _ | Praise _ | Parrayrefs _ | Parraysets _ | Pmodint _ | Pdivint _
  | Pstringrefs | Pbytesrefs | Pbytessets
  | Pstring_load_16 false
  | Pstring_load_32 (false, _)
  | Pstring_load_64 (false, _)
  | Pbytes_load_16 false
  | Pbytes_load_32 (false, _)
  | Pbytes_load_64 (false, _)
  | Pbytes_set_16 false
  | Pbytes_set_32 false
  | Pbytes_set_64 false
  | Pbigstring_load_16 false
  | Pbigstring_load_32 (false, _)
  | Pbigstring_load_64 (false, _)
  | Pbigstring_set_16 false
  | Pbigstring_set_32 false
  | Pbigstring_set_64 false
  | Pdivbint { is_safe = Safe; _ }
  | Pmodbint { is_safe = Safe; _ }
  | Pbigarrayref (false, _, _, _)
  | Pbigarrayset (false, _, _, _)
  (* These bigarray primitives are translated into c-calls which may raise even
     if the unsafe flag is true *)
  | Pbigarrayref (_, _, Pbigarray_unknown, _)
  | Pbigarrayset (_, _, Pbigarray_unknown, _)
  | Pbigarrayref (_, _, _, Pbigarray_unknown_layout)
  | Pbigarrayset (_, _, _, Pbigarray_unknown_layout) ->
    true
  | Pbytes_to_string | Pbytes_of_string | Parray_of_iarray | Parray_to_iarray
  | Pignore | Pgetglobal _ | Psetglobal _ | Pgetpredef _ | Pmakeblock _
  | Pmakefloatblock _ | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Psequand | Psequor | Pnot | Pnegint | Paddint | Psubint | Pmulint | Pandint
  | Porint | Pxorint | Plslint | Plsrint | Pasrint | Pintcomp _ | Pcompare_ints
  | Pcompare_floats | Pcompare_bints _ | Poffsetint _ | Poffsetref _
  | Pintoffloat | Pfloatofint _ | Pnegfloat _ | Pabsfloat _ | Paddfloat _
  | Psubfloat _ | Pmulfloat _ | Pdivfloat _ | Pfloatcomp _ | Pstringlength
  | Pstringrefu | Pbyteslength | Pbytesrefu | Pbytessetu | Pmakearray _
  | Pduparray _ | Parraylength _ | Parrayrefu _ | Parraysetu _ | Pisint _
  | Pisout | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _ | Paddbint _
  | Psubbint _ | Pmulbint _
  | Pdivbint { is_safe = Unsafe; _ }
  | Pmodbint { is_safe = Unsafe; _ }
  | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
  | Pbintcomp _ | Pbigarraydim _
  | Pbigarrayref
      ( true,
        _,
        ( Pbigarray_float32 | Pbigarray_float64 | Pbigarray_sint8
        | Pbigarray_uint8 | Pbigarray_sint16 | Pbigarray_uint16
        | Pbigarray_int32 | Pbigarray_int64 | Pbigarray_caml_int
        | Pbigarray_native_int | Pbigarray_complex32 | Pbigarray_complex64 ),
        _ )
  | Pbigarrayset
      ( true,
        _,
        ( Pbigarray_float32 | Pbigarray_float64 | Pbigarray_sint8
        | Pbigarray_uint8 | Pbigarray_sint16 | Pbigarray_uint16
        | Pbigarray_int32 | Pbigarray_int64 | Pbigarray_caml_int
        | Pbigarray_native_int | Pbigarray_complex32 | Pbigarray_complex64 ),
        (Pbigarray_c_layout | Pbigarray_fortran_layout) )
  | Pstring_load_16 true
  | Pstring_load_32 (true, _)
  | Pstring_load_64 (true, _)
  | Pbytes_load_16 true
  | Pbytes_load_32 (true, _)
  | Pbytes_load_64 (true, _)
  | Pbytes_set_16 true
  | Pbytes_set_32 true
  | Pbytes_set_64 true
  | Pbigstring_load_16 true
  | Pbigstring_load_32 (true, _)
  | Pbigstring_load_64 (true, _)
  | Pbigstring_set_16 true
  | Pbigstring_set_32 true
  | Pbigstring_set_64 true
  | Pctconst _ | Pbswap16 | Pbbswap _ | Pint_as_pointer _ | Popaque _
  | Pprobe_is_enabled _ | Pobj_dup | Pobj_magic _ | Pbox_float _ | Punbox_float
  | Punbox_int _ | Pbox_int _ ->
    false

type cps_continuation =
  | Tail of Continuation.t
  | Non_tail of (Acc.t -> Env.t -> CCenv.t -> IR.simple -> Expr_with_acc.t)

let apply_cps_cont_simple k ?(dbg = Debuginfo.none) acc env ccenv simple =
  match k with
  | Tail k -> apply_cont_with_extra_args acc env ccenv ~dbg k None [simple]
  | Non_tail k -> k acc env ccenv simple

let apply_cps_cont k ?dbg acc env ccenv id =
  apply_cps_cont_simple k ?dbg acc env ccenv (IR.Var id)

let maybe_insert_let_cont result_var_name kind k acc env ccenv body =
  match k with
  | Tail k -> body acc env ccenv k
  | Non_tail k ->
    let result_var = Ident.create_local result_var_name in
    let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler:false
      ~params:[result_var, IR.Not_user_visible, kind]
      ~handler:(fun acc env ccenv -> k acc env ccenv (IR.Var result_var))
      ~body

let name_if_not_var acc ccenv name simple kind body =
  match simple with
  | IR.Var id -> body id acc ccenv
  | IR.Const _ ->
    let id = Ident.create_local name in
    CC.close_let acc ccenv
      [id, kind]
      Not_user_visible (IR.Simple simple) ~body:(body id)

let rec cps acc env ccenv (lam : L.lambda) (k : cps_continuation)
    (k_exn : Continuation.t) : Expr_with_acc.t =
  match lam with
  | Lvar id ->
    assert (not (Env.is_mutable env id));
    apply_cps_cont k acc env ccenv id
  | Lmutvar id ->
    let return_id = Env.get_mutable_variable env id in
    apply_cps_cont k acc env ccenv return_id
  | Lconst const -> apply_cps_cont_simple k acc env ccenv (IR.Const const)
  | Lapply
      { ap_func;
        ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall = _;
        ap_inlined;
        ap_specialised = _;
        ap_probe
      } ->
    (* Note that we don't need kind information about [ap_args] since we already
       have it on the corresponding [Simple]s in the environment. *)
    maybe_insert_let_cont "apply_result" ap_result_layout k acc env ccenv
      (fun acc env ccenv k ->
        cps_tail_apply acc env ccenv ap_func ap_args ap_region_close ap_mode
          ap_loc ap_inlined ap_probe ap_result_layout k k_exn)
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let dbg = Debuginfo.from_location func.loc in
    let func =
      cps_function env ~fid:id ~recursive:(Non_recursive : Recursive.t) func
    in
    let body acc ccenv = apply_cps_cont k ~dbg acc env ccenv id in
    CC.close_let_rec acc ccenv ~function_declarations:[func] ~body
      ~current_region:(Env.current_region env)
  | Lmutlet (value_kind, id, defining_expr, body) ->
    (* CR mshinwell: user-visibleness needs thinking about here *)
    let temp_id = Ident.create_local "let_mutable" in
    let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler:false
      ~params:[temp_id, IR.Not_user_visible, value_kind]
      ~body:(fun acc env ccenv after_defining_expr ->
        cps_tail acc env ccenv defining_expr after_defining_expr k_exn)
      ~handler:(fun acc env ccenv ->
        let env, new_id = Env.register_mutable_variable env id value_kind in
        let kind = Flambda_kind.With_subkind.from_lambda value_kind in
        let body acc ccenv = cps acc env ccenv body k k_exn in
        CC.close_let acc ccenv
          [new_id, kind]
          User_visible (Simple (Var temp_id)) ~body)
  | Llet ((Strict | Alias | StrictOpt), _, fun_id, Lfunction func, body) ->
    (* This case is here to get function names right. *)
    let bindings = cps_function_bindings env [fun_id, L.Lfunction func] in
    let body acc ccenv = cps acc env ccenv body k k_exn in
    let let_expr =
      List.fold_left
        (fun body func acc ccenv ->
          CC.close_let_rec acc ccenv ~function_declarations:[func] ~body
            ~current_region:(Env.current_region env))
        body bindings
    in
    let_expr acc ccenv
  | Llet ((Strict | Alias | StrictOpt), layout, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body acc ccenv = cps acc env ccenv body k k_exn in
    CC.close_let acc ccenv
      [id, Flambda_kind.With_subkind.from_lambda layout]
      (is_user_visible env id) (Simple (Const const)) ~body
  | Llet
      ( ((Strict | Alias | StrictOpt) as let_kind),
        layout,
        id,
        Lprim (prim, args, loc),
        body ) -> (
    match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : IR.exn_continuation option =
        if primitive_can_raise prim
        then
          Some
            { exn_handler = k_exn;
              extra_args = extra_args_for_exn_continuation env k_exn
            }
        else None
      in
      cps_non_tail_list acc env ccenv args
        (fun acc env ccenv args ->
          let args = List.map (fun arg -> [arg]) args in
          let body acc ccenv = cps acc env ccenv body k k_exn in
          let region = Env.current_region env in
          CC.close_let acc ccenv
            [id, Flambda_kind.With_subkind.from_lambda layout]
            (is_user_visible env id)
            (Prim { prim; args; loc; exn_continuation; region })
            ~body)
        k_exn
    | Transformed lam ->
      cps acc env ccenv (L.Llet (let_kind, layout, id, lam, body)) k k_exn)
  | Llet
      ( (Strict | Alias | StrictOpt),
        _,
        id,
        Lassign (being_assigned, new_value),
        body ) ->
    (* This case is also to avoid extraneous continuations in code that relies
       on the ref-conversion optimisation. *)
    if not (Env.is_mutable env being_assigned)
    then
      Misc.fatal_errorf "Lassign on non-mutable variable %a" Ident.print
        being_assigned;
    cps_non_tail_simple acc env ccenv new_value
      (fun acc env ccenv new_value ->
        let env, new_id = Env.update_mutable_variable env being_assigned in
        let body acc ccenv =
          let body acc ccenv = cps acc env ccenv body k k_exn in
          CC.close_let acc ccenv
            [id, Flambda_kind.With_subkind.tagged_immediate]
            Not_user_visible (Simple (Const L.const_unit)) ~body
        in
        let value_kind =
          snd (Env.get_mutable_variable_with_kind env being_assigned)
        in
        let value_kind = Flambda_kind.With_subkind.from_lambda value_kind in
        CC.close_let acc ccenv
          [new_id, value_kind]
          User_visible (Simple new_value) ~body)
      k_exn
  | Llet ((Strict | Alias | StrictOpt), layout, id, defining_expr, body) ->
    let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler:false
      ~params:[id, is_user_visible env id, layout]
      ~body:(fun acc env ccenv after_defining_expr ->
        cps_tail acc env ccenv defining_expr after_defining_expr k_exn)
      ~handler:(fun acc env ccenv -> cps acc env ccenv body k k_exn)
  (* CR pchambart: This version would avoid one let cont, but would miss the
     value kind. It should be used when CC.close_let can propagate the
     value_kind. *)
  (* let k acc env ccenv value =
   *   let body acc ccenv = cps acc env ccenv body k k_exn in
   *   CC.close_let acc ccenv id User_visible value_kind (Simple value) ~body
   * in
   * cps_non_tail_simple acc env ccenv defining_expr k k_exn *)
  | Lletrec (bindings, body) -> (
    let free_vars_kind id =
      let _, kind_with_subkind = CCenv.find_var ccenv id in
      Some
        (Flambda_kind.to_lambda
           (Flambda_kind.With_subkind.kind kind_with_subkind))
    in
    match Dissect_letrec.dissect_letrec ~bindings ~body ~free_vars_kind with
    | Unchanged ->
      let function_declarations = cps_function_bindings env bindings in
      let body acc ccenv = cps acc env ccenv body k k_exn in
      CC.close_let_rec acc ccenv ~function_declarations ~body
        ~current_region:(Env.current_region env)
    | Dissected lam -> cps acc env ccenv lam k k_exn)
  | Lprim (prim, args, loc) -> (
    match[@ocaml.warning "-fragile-match"] prim with
    | Praise raise_kind -> (
      match args with
      | [_] ->
        cps_non_tail_list acc env ccenv args
          (fun acc _env ccenv args ->
            let exn_continuation : IR.exn_continuation =
              { exn_handler = k_exn;
                extra_args = extra_args_for_exn_continuation env k_exn
              }
            in
            let dbg = Debuginfo.from_location loc in
            CC.close_raise acc ccenv ~raise_kind ~arg:(List.hd args) ~dbg
              exn_continuation)
          k_exn
      | [] | _ :: _ ->
        Misc.fatal_errorf "Wrong number of arguments for Lraise: %a"
          Printlambda.primitive prim)
    | _ ->
      let name = Printlambda.name_of_primitive prim in
      let id = Ident.create_local name in
      let result_layout = L.primitive_result_layout prim in
      (match result_layout with
      | Pvalue _ | Punboxed_float | Punboxed_int _ | Punboxed_vector _ -> ()
      | Ptop | Pbottom ->
        Misc.fatal_errorf "Invalid result layout %a for primitive %a"
          Printlambda.layout result_layout Printlambda.primitive prim);
      cps acc env ccenv
        (L.Llet (Strict, result_layout, id, lam, L.Lvar id))
        k k_exn)
  | Lswitch (scrutinee, switch, loc, kind) ->
    maybe_insert_let_cont "switch_result" kind k acc env ccenv
      (fun acc env ccenv k ->
        cps_switch acc env ccenv switch
          ~condition_dbg:(Debuginfo.from_location loc)
          ~scrutinee k k_exn)
  | Lstringswitch (scrutinee, cases, default, loc, kind) ->
    cps acc env ccenv
      (Matching.expand_stringswitch loc kind scrutinee cases default)
      k k_exn
  | Lstaticraise (static_exn, args) ->
    let continuation = Env.get_static_exn_continuation env static_exn in
    cps_non_tail_list acc env ccenv args
      (fun acc env ccenv args ->
        let extra_args =
          List.map
            (fun var : IR.simple -> Var var)
            (Env.extra_args_for_continuation env continuation)
        in
        compile_staticfail acc env ccenv ~continuation ~args:(args @ extra_args))
      k_exn
  | Lstaticcatch (body, (static_exn, args), handler, layout) ->
    maybe_insert_let_cont "staticcatch_result" layout k acc env ccenv
      (fun acc env ccenv k ->
        let continuation = Continuation.create () in
        let { Env.body_env; handler_env; extra_params } =
          Env.add_static_exn_continuation env static_exn continuation
        in
        let recursive : Asttypes.rec_flag =
          if Env.is_static_exn_recursive env static_exn
          then Recursive
          else Nonrecursive
        in
        let params =
          List.map
            (fun (arg, kind) ->
              ( arg,
                is_user_visible env arg,
                Flambda_kind.With_subkind.from_lambda kind ))
            (args @ extra_params)
        in
        let handler acc ccenv =
          let ccenv = CCenv.set_not_at_toplevel ccenv in
          cps_tail acc handler_env ccenv handler k k_exn
        in
        let body acc ccenv = cps_tail acc body_env ccenv body k k_exn in
        CC.close_let_cont acc ccenv ~name:continuation ~is_exn_handler:false
          ~params ~recursive ~body ~handler)
  | Lsend (meth_kind, meth, obj, args, pos, mode, loc, layout) ->
    cps_non_tail_simple acc env ccenv obj
      (fun acc env ccenv obj ->
        cps_non_tail_var "meth" acc env ccenv meth
          Flambda_kind.With_subkind.any_value
          (fun acc env ccenv meth ->
            cps_non_tail_list acc env ccenv args
              (fun acc env ccenv args ->
                maybe_insert_let_cont "send_result" layout k acc env ccenv
                  (fun acc env ccenv k ->
                    let exn_continuation : IR.exn_continuation =
                      { exn_handler = k_exn;
                        extra_args = extra_args_for_exn_continuation env k_exn
                      }
                    in
                    let apply : IR.apply =
                      { kind = Method { kind = meth_kind; obj };
                        func = meth;
                        continuation = k;
                        exn_continuation;
                        args;
                        loc;
                        region_close = pos;
                        inlined = Default_inlined;
                        probe = None;
                        mode;
                        region = Env.current_region env;
                        return_arity =
                          Flambda_arity.create
                            [Flambda_kind.With_subkind.from_lambda layout]
                      }
                    in
                    wrap_return_continuation acc env ccenv apply))
              k_exn)
          k_exn)
      k_exn
  | Ltrywith (body, id, handler, kind) ->
    let dbg = Debuginfo.none (* CR mshinwell: fix [Lambda] *) in
    let body_result = Ident.create_local "body_result" in
    let region = Ident.create_local "try_region" in
    (* As for all other constructs, the OCaml type checker and the Lambda
       generation pass ensures that there will be an enclosing region around the
       whole [Ltrywith] (possibly not immediately enclosing, but maybe further
       out). The only reason we need a [Begin_region] here is to be able to
       unwind the local allocation stack if the exception handler is invoked.
       There is no corresponding [End_region] on the non-exceptional path
       because there might be a local allocation in the "try" block that needs
       to be returned. In effect, such allocations are treated as if they were
       in the parent region, although they will be annotated with the region
       identifier of the "try region". To handle this correctly we annotate the
       [Begin_region] with its parent region. This use of the parent region will
       ensure that the parent does not get deleted unless the try region is
       unused. *)
    (* Under a try-with block, any exception might introduce a branch to the
       handler. So while for static catches we could simplify the body in the
       same toplevel context, here we need to assume that all of the body could
       be behind a branch. *)
    let ccenv = CCenv.set_not_at_toplevel ccenv in
    CC.close_let acc ccenv
      [region, Flambda_kind.With_subkind.region]
      Not_user_visible
      (Begin_region { try_region_parent = Some (Env.current_region env) })
      ~body:(fun acc ccenv ->
        maybe_insert_let_cont "try_with_result" kind k acc env ccenv
          (fun acc env ccenv k ->
            let env = Env.entering_try_region env region in
            let_cont_nonrecursive_with_extra_params acc env ccenv
              ~is_exn_handler:true
              ~params:[id, is_user_visible env id, Lambda.layout_block]
              ~body:(fun acc env ccenv handler_continuation ->
                let_cont_nonrecursive_with_extra_params acc env ccenv
                  ~is_exn_handler:false
                  ~params:[body_result, Not_user_visible, kind]
                  ~body:(fun acc env ccenv poptrap_continuation ->
                    let_cont_nonrecursive_with_extra_params acc env ccenv
                      ~is_exn_handler:false ~params:[]
                      ~body:(fun acc env ccenv body_continuation ->
                        apply_cont_with_extra_args acc env ccenv ~dbg
                          body_continuation
                          (Some (IR.Push { exn_handler = handler_continuation }))
                          [])
                      ~handler:(fun acc env ccenv ->
                        cps_tail acc env ccenv body poptrap_continuation
                          handler_continuation))
                  ~handler:(fun acc env ccenv ->
                    let env = Env.leaving_try_region env in
                    apply_cont_with_extra_args acc env ccenv ~dbg k
                      (Some (IR.Pop { exn_handler = handler_continuation }))
                      [IR.Var body_result]))
              ~handler:(fun acc env ccenv ->
                CC.close_let acc ccenv
                  [ ( Ident.create_local "unit",
                      Flambda_kind.With_subkind.tagged_immediate ) ]
                  Not_user_visible (End_region region)
                  ~body:(fun acc ccenv ->
                    let env = Env.leaving_try_region env in
                    cps_tail acc env ccenv handler k k_exn))))
  | Lifthenelse (cond, ifso, ifnot, kind) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot ~kind in
    cps acc env ccenv lam k k_exn
  | Lsequence (lam1, lam2) ->
    let k acc env ccenv _value = cps acc env ccenv lam2 k k_exn in
    cps_non_tail_simple acc env ccenv lam1 k k_exn
  | Lwhile { wh_cond = cond; wh_body = body } ->
    (* CR-someday mshinwell: make use of wh_cond_region / wh_body_region? *)
    let env, loop = rec_catch_for_while_loop env cond body in
    cps acc env ccenv loop k k_exn
  | Lfor
      { for_id = ident;
        for_from = start;
        for_to = stop;
        for_dir = dir;
        for_body = body
      } ->
    let env, loop = rec_catch_for_for_loop env ident start stop dir body in
    cps acc env ccenv loop k k_exn
  | Lassign (being_assigned, new_value) ->
    if not (Env.is_mutable env being_assigned)
    then
      Misc.fatal_errorf "Lassign on non-mutable variable %a" Ident.print
        being_assigned;
    cps_non_tail_simple acc env ccenv new_value
      (fun acc env ccenv new_value ->
        let env, new_id = Env.update_mutable_variable env being_assigned in
        let body acc ccenv =
          apply_cps_cont_simple k acc env ccenv (Const L.const_unit)
        in
        let _, value_kind =
          Env.get_mutable_variable_with_kind env being_assigned
        in
        let value_kind = Flambda_kind.With_subkind.from_lambda value_kind in
        CC.close_let acc ccenv
          [new_id, value_kind]
          User_visible (Simple new_value) ~body)
      k_exn
  | Levent (body, _event) -> cps acc env ccenv body k k_exn
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if an
       identifier is. Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression, or
       by completely removing it (replacing by unit). *)
    Misc.fatal_error
      "[Lifused] should have been removed by [Simplif.simplify_lets]"
  | Lregion (body, _) when not (Flambda_features.stack_allocation_enabled ()) ->
    cps acc env ccenv body k k_exn
  | Lexclave body ->
    let region = Env.current_region env in
    CC.close_let acc ccenv
      [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
      Not_user_visible (End_region region)
      ~body:(fun acc ccenv ->
        let env = Env.leaving_region env in
        cps acc env ccenv body k k_exn)
  | Lregion (body, layout) ->
    (* Here we need to build the region closure continuation (see long comment
       above). Since we're not in tail position, we also need to have a new
       continuation for the code after the body. *)
    let region = Ident.create_local "region" in
    let dbg = Debuginfo.none in
    CC.close_let acc ccenv
      [region, Flambda_kind.With_subkind.region]
      Not_user_visible
      (Begin_region { try_region_parent = None })
      ~body:(fun acc ccenv ->
        maybe_insert_let_cont "body_return" layout k acc env ccenv
          (fun acc env ccenv k ->
            let wrap_return = Ident.create_local "region_return" in
            let_cont_nonrecursive_with_extra_params acc env ccenv
              ~is_exn_handler:false
              ~params:[wrap_return, Not_user_visible, layout]
              ~body:(fun acc env ccenv continuation_closing_region ->
                (* We register this region to be closed by the newly-created
                   region closure continuation. When we reach a point in [body]
                   where we would normally jump to [return_continuation] (i.e.
                   leaving the body), we will instead jump to
                   [region_closure_continuation] to ensure the region is closed
                   at the right time. Exception raises and tailcall cases will
                   generate their own [End_region]s and use
                   [return_continuation] directly. (See long comment above.)

                   In the case where we jump out of the scope of several regions
                   at once, we will jump directly to the region closure
                   continuation for the outermost open region. For this to be
                   correct we rely on the fact that the code structure here,
                   which follows the block structure of the Lambda code, ensures
                   this is equivalent to going through the sequence of nested
                   [region_closure_continuation]s we generate.

                   In the event the region closure continuation isn't used (e.g.
                   the only exit is a tailcall), the [Let_cont] will be
                   discarded by [Closure_conversion]. *)
                let env =
                  Env.entering_region env region ~continuation_closing_region
                    ~continuation_after_closing_region:k
                in
                cps_tail acc env ccenv body k k_exn)
              ~handler:(fun acc env ccenv ->
                CC.close_let acc ccenv
                  [ ( Ident.create_local "unit",
                      Flambda_kind.With_subkind.tagged_immediate ) ]
                  Not_user_visible (End_region region)
                  ~body:(fun acc ccenv ->
                    (* Both body and handler will continue at
                       [return_continuation] by default.
                       [restore_region_context] will intercept the
                       [Lstaticraise] jump to this handler if needed. *)
                    apply_cont_with_extra_args acc env ccenv ~dbg k None
                      [IR.Var wrap_return]))))

and cps_non_tail_simple acc env ccenv lam k k_exn =
  cps acc env ccenv lam (Non_tail k) k_exn

and cps_non_tail_var name acc env ccenv lam kind k k_exn =
  cps_non_tail_simple acc env ccenv lam
    (fun acc env ccenv simple ->
      name_if_not_var acc ccenv name simple kind (fun var acc ccenv ->
          k acc env ccenv var))
    k_exn

and cps_tail_apply acc env ccenv ap_func ap_args ap_region_close ap_mode ap_loc
    ap_inlined ap_probe ap_return (k : Continuation.t) (k_exn : Continuation.t)
    : Expr_with_acc.t =
  cps_non_tail_list acc env ccenv ap_args
    (fun acc env ccenv args ->
      cps_non_tail_var "func" acc env ccenv ap_func
        Flambda_kind.With_subkind.any_value
        (fun acc env ccenv func ->
          let exn_continuation : IR.exn_continuation =
            { exn_handler = k_exn;
              extra_args = extra_args_for_exn_continuation env k_exn
            }
          in
          let apply : IR.apply =
            { kind = Function;
              func;
              continuation = k;
              exn_continuation;
              args;
              loc = ap_loc;
              region_close = ap_region_close;
              inlined = ap_inlined;
              probe = ap_probe;
              mode = ap_mode;
              region = Env.current_region env;
              return_arity =
                Flambda_arity.create
                  [Flambda_kind.With_subkind.from_lambda ap_return]
            }
          in
          wrap_return_continuation acc env ccenv apply)
        k_exn)
    k_exn

and cps_tail acc env ccenv (lam : L.lambda) (k : Continuation.t)
    (k_exn : Continuation.t) : Expr_with_acc.t =
  cps acc env ccenv lam (Tail k) k_exn

and cps_non_tail_list acc env ccenv lams k k_exn =
  let lams = List.rev lams in
  (* Always evaluate right-to-left. *)
  cps_non_tail_list_core acc env ccenv lams
    (fun acc env ccenv ids -> k acc env ccenv (List.rev ids))
    k_exn

and cps_non_tail_list_core acc env ccenv (lams : L.lambda list)
    (k : Acc.t -> Env.t -> CCenv.t -> IR.simple list -> Expr_with_acc.t)
    (k_exn : Continuation.t) =
  match lams with
  | [] -> k acc env ccenv []
  | lam :: lams ->
    cps_non_tail_simple acc env ccenv lam
      (fun acc env ccenv simple ->
        cps_non_tail_list_core acc env ccenv lams
          (fun acc env ccenv simples -> k acc env ccenv (simple :: simples))
          k_exn)
      k_exn

and cps_function_bindings env (bindings : (Ident.t * L.lambda) list) =
  let bindings_with_wrappers =
    List.map
      (fun [@ocaml.warning "-fragile-match"] (fun_id, binding) ->
        match binding with
        | L.Lfunction
            { kind; params; body = fbody; attr; loc; mode; region; return; _ }
          -> (
          match
            Simplif.split_default_wrapper ~id:fun_id ~kind ~params ~body:fbody
              ~return ~attr ~loc ~mode ~region
          with
          | [(id, L.Lfunction lfun)] -> [id, lfun]
          | [(id1, L.Lfunction lfun1); (id2, L.Lfunction lfun2)] ->
            [id1, lfun1; id2, lfun2]
          | [(_, _)] | [(_, _); (_, _)] ->
            Misc.fatal_errorf
              "Expected `Lfunction` terms from [split_default_wrapper] when \
               translating:@ %a"
              Printlambda.lambda binding
          | _ ->
            Misc.fatal_errorf
              "Unexpected return value from [split_default_wrapper] when \
               translating:@ %a"
              Printlambda.lambda binding)
        | _ ->
          Misc.fatal_errorf
            "Only [Lfunction] expressions are permitted in function bindings \
             upon entry to CPS conversion: %a"
            Printlambda.lambda binding)
      bindings
  in
  let free_idents, directed_graph =
    let fun_ids = Ident.Set.of_list (List.map fst bindings) in
    List.fold_left
      (fun (free_ids, graph) (fun_id, ({ body; _ } : L.lfunction)) ->
        let free_ids_of_body = Lambda.free_variables body in
        let free_ids = Ident.Map.add fun_id free_ids_of_body free_ids in
        let free_fun_ids = Ident.Set.inter fun_ids free_ids_of_body in
        let graph = Ident.Map.add fun_id free_fun_ids graph in
        free_ids, graph)
      (Ident.Map.empty, Ident.Map.empty)
      (List.flatten bindings_with_wrappers)
  in
  let recursive_functions =
    let module SCC = Strongly_connected_components.Make (Ident) in
    let connected_components =
      SCC.connected_components_sorted_from_roots_to_leaf directed_graph
    in
    Array.fold_left
      (fun rec_ids component ->
        match component with
        | SCC.No_loop _ -> rec_ids
        | SCC.Has_loop elts -> List.fold_right Ident.Set.add elts rec_ids)
      Ident.Set.empty connected_components
  in
  let recursive fun_id : Recursive.t =
    if Ident.Set.mem fun_id recursive_functions
    then Recursive
    else Non_recursive
  in
  let bindings_with_wrappers = List.flatten bindings_with_wrappers in
  List.map
    (fun (fun_id, def) ->
      cps_function env ~fid:fun_id ~recursive:(recursive fun_id)
        ~precomputed_free_idents:(Ident.Map.find fun_id free_idents)
        def)
    bindings_with_wrappers

and cps_function env ~fid ~(recursive : Recursive.t) ?precomputed_free_idents
    ({ kind; params; return; body; attr; loc; mode; region } : L.lfunction) :
    Function_decl.t =
  let first_complex_local_param =
    List.length params
    - match kind with Curried { nlocal } -> nlocal | Tupled -> 0
  in
  let body_cont = Continuation.create ~sort:Return () in
  let body_exn_cont = Continuation.create () in
  let free_idents_of_body =
    match precomputed_free_idents with
    | Some ids -> ids
    | None -> Lambda.free_variables body
  in
  let my_region = Ident.create_local "my_region" in
  let new_env =
    Env.create ~current_unit:(Env.current_unit env)
      ~return_continuation:body_cont ~exn_continuation:body_exn_cont ~my_region
  in
  let exn_continuation : IR.exn_continuation =
    { exn_handler = body_exn_cont; extra_args = [] }
  in
  let function_slot =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name:(Ident.name fid) Flambda_kind.With_subkind.any_value
  in
  let body acc ccenv =
    let ccenv = CCenv.set_path_to_root ccenv loc in
    let ccenv = CCenv.set_not_at_toplevel ccenv in
    cps_tail acc new_env ccenv body body_cont body_exn_cont
  in
  let params =
    List.map
      (fun (p : Lambda.lparam) : Function_decl.param ->
        { name = p.name;
          kind = Flambda_kind.With_subkind.from_lambda p.layout;
          attributes = p.attributes;
          mode = p.mode
        })
      params
  in
  let return =
    Flambda_arity.create [Flambda_kind.With_subkind.from_lambda return]
  in
  Function_decl.create ~let_rec_ident:(Some fid) ~function_slot ~kind ~params
    ~return ~return_continuation:body_cont ~exn_continuation ~my_region ~body
    ~attr ~loc ~free_idents_of_body recursive ~closure_alloc_mode:mode
    ~first_complex_local_param ~contains_no_escaping_local_allocs:region

and cps_switch acc env ccenv (switch : L.lambda_switch) ~condition_dbg
    ~scrutinee (k : Continuation.t) (k_exn : Continuation.t) : Expr_with_acc.t =
  let block_nums, sw_blocks = List.split switch.sw_blocks in
  List.iter
    (fun sw_tag ->
      match Tag.Scannable.create sw_tag with
      | Some tag ->
        let tag' = Tag.Scannable.to_tag tag in
        if Tag.is_structured_block_but_not_data_constructor tag'
        then
          Misc.fatal_errorf
            "Bad tag %a in [Lswitch] (tag is that of a scannable block, but \
             not one treated like a variant; [Lswitch] can only be used for \
             variant matching)"
            Tag.print tag'
      | None ->
        Misc.fatal_errorf
          "Bad tag %d in [Lswitch] (not the tag of a GC-scannable block)" sw_tag)
    block_nums;
  if switch.sw_numblocks > Obj.last_non_constant_constructor_tag + 1
  then
    Misc.fatal_errorf
      "Too many blocks (%d) in [Lswitch], would overlap into tag space for \
       blocks that are not treated like variants; [Lswitch] can only be used \
       for variant matching"
      switch.sw_numblocks;
  let convert_arms_rev env cases wrappers =
    List.fold_left
      (fun (consts_rev, wrappers) (arm, (action : L.lambda)) ->
        match action with
        | Lvar var ->
          assert (not (Env.is_mutable env var));
          let extra_args =
            List.map
              (fun arg : IR.simple -> Var arg)
              (Env.extra_args_for_continuation env k)
          in
          let k = restore_continuation_context_for_switch_arm env k in
          let consts_rev =
            (arm, k, None, IR.Var var :: extra_args) :: consts_rev
          in
          consts_rev, wrappers
        | Lconst cst ->
          let extra_args =
            List.map
              (fun arg : IR.simple -> Var arg)
              (Env.extra_args_for_continuation env k)
          in
          let k = restore_continuation_context_for_switch_arm env k in
          let consts_rev =
            (arm, k, None, IR.Const cst :: extra_args) :: consts_rev
          in
          consts_rev, wrappers
        | Lmutvar _ | Lapply _ | Lfunction _ | Llet _ | Lmutlet _ | Lletrec _
        | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
        | Lstaticcatch _ | Ltrywith _ | Lifthenelse _ | Lsequence _ | Lwhile _
        | Lfor _ | Lassign _ | Lsend _ | Levent _ | Lifused _ | Lregion _
        | Lexclave _ ->
          (* The continuations created here (and for failactions) are local. The
             bodies of the let_conts will not modify mutable variables. Hence,
             it is safe to exclude them from passing along the extra arguments
             for mutable values. *)
          let cont = Continuation.create () in
          let action acc ccenv = cps_tail acc env ccenv action k k_exn in
          let consts_rev = (arm, cont, None, []) :: consts_rev in
          let wrappers = (cont, action) :: wrappers in
          consts_rev, wrappers)
      ([], wrappers) cases
  in
  cps_non_tail_var "scrutinee" acc env ccenv scrutinee
    Flambda_kind.With_subkind.any_value
    (fun acc env ccenv scrutinee ->
      let ccenv = CCenv.set_not_at_toplevel ccenv in
      let consts_rev, wrappers = convert_arms_rev env switch.sw_consts [] in
      let blocks_rev, wrappers =
        convert_arms_rev env (List.combine block_nums sw_blocks) wrappers
      in
      let consts = List.rev consts_rev in
      let blocks = List.rev blocks_rev in
      let failaction, wrappers =
        match switch.sw_failaction with
        | None -> None, wrappers
        | Some action ->
          let cont = Continuation.create () in
          let action acc ccenv = cps_tail acc env ccenv action k k_exn in
          let wrappers = (cont, action) :: wrappers in
          Some (cont, None, []), wrappers
      in
      let const_switch : IR.switch =
        { numconsts = switch.sw_numconsts; consts; failaction }
      in
      let block_switch : IR.switch =
        { numconsts = switch.sw_numblocks; consts = blocks; failaction }
      in
      let build_switch scrutinee wrappers =
        let const_switch acc ccenv =
          CC.close_switch acc ccenv ~condition_dbg scrutinee const_switch
        in
        let scrutinee_tag = Ident.create_local "scrutinee_tag" in
        let block_switch acc ccenv =
          let body acc ccenv =
            CC.close_switch acc ccenv ~condition_dbg scrutinee_tag block_switch
          in
          CC.close_let acc ccenv
            [scrutinee_tag, Flambda_kind.With_subkind.naked_immediate]
            Not_user_visible (Get_tag scrutinee) ~body
        in
        if switch.sw_numblocks = 0
        then const_switch, wrappers
        else if switch.sw_numconsts = 0
        then block_switch, wrappers
        else
          let const_cont = Continuation.create () in
          let block_cont = Continuation.create () in
          let isint_switch : IR.switch =
            { numconsts = 2;
              consts = [0, block_cont, None, []; 1, const_cont, None, []];
              failaction = None
            }
          in
          let is_scrutinee_int = Ident.create_local "is_scrutinee_int" in
          let isint_switch acc ccenv =
            let body acc ccenv =
              CC.close_switch acc ccenv ~condition_dbg is_scrutinee_int
                isint_switch
            in
            let region = Env.current_region env in
            CC.close_let acc ccenv
              [is_scrutinee_int, Flambda_kind.With_subkind.naked_immediate]
              Not_user_visible
              (Prim
                 { prim = Pisint { variant_only = true };
                   args = [[Var scrutinee]];
                   loc = Loc_unknown;
                   exn_continuation = None;
                   region
                 })
              ~body
          in
          ( isint_switch,
            (const_cont, const_switch) :: (block_cont, block_switch) :: wrappers
          )
      in
      let switch, wrappers = build_switch scrutinee wrappers in
      let switch_expr =
        List.fold_left
          (fun body (cont, action) acc ccenv ->
            CC.close_let_cont acc ccenv ~name:cont ~is_exn_handler:false
              ~params:[] ~recursive:Nonrecursive ~body ~handler:action)
          switch wrappers
      in
      switch_expr acc ccenv)
    k_exn

(* CR pchambart: define a record `target_config` to hold things like
   `big_endian` *)
let lambda_to_flambda ~mode ~big_endian ~cmx_loader ~compilation_unit
    ~module_block_size_in_words (lam : Lambda.lambda) =
  let return_continuation = Continuation.create ~sort:Define_root_symbol () in
  let exn_continuation = Continuation.create () in
  let toplevel_my_region = Ident.create_local "toplevel_my_region" in
  let env =
    Env.create ~current_unit:compilation_unit ~return_continuation
      ~exn_continuation ~my_region:toplevel_my_region
  in
  let program acc ccenv =
    cps_tail acc env ccenv lam return_continuation exn_continuation
  in
  CC.close_program ~mode ~big_endian ~cmx_loader ~compilation_unit
    ~module_block_size_in_words ~program ~prog_return_cont:return_continuation
    ~exn_continuation ~toplevel_my_region
