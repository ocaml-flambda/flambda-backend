(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module Env = To_cmm_env
module Ece = Effects_and_coeffects
module KS = Flambda_kind.With_subkind
module R = To_cmm_result

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

type translate_expr =
  To_cmm_env.t ->
  To_cmm_result.t ->
  Expr.t ->
  Cmm.expression * To_cmm_env.free_vars * To_cmm_result.t

(* Filling of closure blocks *)

type closure_code_pointers =
  | Full_application_only
  | Full_and_partial_application

let get_func_decl_params_arity t code_id =
  let info = Env.get_code_metadata t code_id in
  (* Avoid generation of excessive amounts of caml_curry functions that only
     distinguish between values and tagged integers; see comments in
     cmm_helpers.ml. *)
  let params_ty =
    List.map
      (fun ks ->
        List.map
          (fun k ->
            C.extended_machtype_of_kind k
            |> C.Extended_machtype.change_tagged_int_to_val)
          ks
        |> Array.concat)
      (Flambda_arity.unarize_per_parameter (Code_metadata.params_arity info))
  in
  let result_machtype =
    C.extended_machtype_of_return_arity (Code_metadata.result_arity info)
    |> C.Extended_machtype.change_tagged_int_to_val
  in
  let kind : Lambda.function_kind =
    if Code_metadata.is_tupled info
    then Lambda.Tupled
    else
      let nlocal =
        Flambda_arity.num_params (Code_metadata.params_arity info)
        - Code_metadata.first_complex_local_param info
      in
      Lambda.Curried { nlocal }
  in
  let closure_code_pointers =
    match kind, params_ty with
    | Curried _, ([] | [_]) -> Full_application_only
    | (Curried _ | Tupled), _ -> Full_and_partial_application
  in
  let arity = kind, params_ty, result_machtype in
  arity, closure_code_pointers, Code_metadata.dbg info

type for_static_sets =
  { closure_symbols : Symbol.t Function_slot.Map.t;
    (* Data symbols for each closure *)
    function_slot_offset_for_updates : int;
    (* The offset of the function slot from which we will index in order to make
       initialising updates to value slots. (We choose a particular slot to use
       for this purpose to avoid any notion of "set of closures symbol" whose
       definition would overlap with that of other symbols.) *)
    closure_symbol_for_updates : Cmm.symbol
        (* The closure symbol of the function slot corresponding to
           [function_slot_offset_for_updates]. *)
  }

module Make_layout_filler (P : sig
  type cmm_term

  val int : dbg:Debuginfo.t -> nativeint -> cmm_term

  val simple :
    dbg:Debuginfo.t ->
    To_cmm_env.t ->
    To_cmm_result.t ->
    Simple.t ->
    [`Data of cmm_term list | `Var of Variable.t]
    * To_cmm_env.free_vars
    * To_cmm_env.t
    * To_cmm_result.t
    * Ece.t

  val infix_header : dbg:Debuginfo.t -> function_slot_offset:int -> cmm_term

  val term_of_symbol : dbg:Debuginfo.t -> Cmm.symbol -> cmm_term

  val define_symbol : Cmm.symbol -> cmm_term list
end) : sig
  val fill_layout :
    for_static_sets option ->
    Code_id.t Function_slot.Map.t ->
    Debuginfo.t ->
    startenv:int ->
    Simple.t Value_slot.Map.t ->
    Env.t ->
    To_cmm_result.t ->
    Ece.t ->
    prev_updates:To_cmm_env.expr_with_info option ->
    (int * Slot_offsets.Layout.slot) list ->
    P.cmm_term list
    * To_cmm_env.free_vars
    * int
    * Env.t
    * To_cmm_result.t
    * Ece.t
    * To_cmm_env.expr_with_info option
end = struct
  (* The [offset]s here are measured in units of words. *)
  let fill_slot for_static_sets decls dbg ~startenv value_slots env res acc
      ~slot_offset updates slot =
    match (slot : Slot_offsets.Layout.slot) with
    | Infix_header ->
      let field = P.infix_header ~function_slot_offset:(slot_offset + 1) ~dbg in
      ( field :: acc,
        Backend_var.Set.empty,
        slot_offset + 1,
        env,
        res,
        Ece.pure,
        updates )
    | Value_slot { value_slot; is_scanned; size = _ } ->
      let simple = Value_slot.Map.find value_slot value_slots in
      let kind = Value_slot.kind value_slot in
      if (not
            (Flambda_kind.equal
               (Flambda_kind.With_subkind.kind kind)
               Flambda_kind.value))
         && is_scanned
      then
        Misc.fatal_errorf
          "Value slot %a not of kind Value (%a) but is visible by GC"
          Simple.print simple Debuginfo.print_compact dbg;
      let contents, free_vars, env, res, eff = P.simple ~dbg env res simple in
      let env, res, fields, updates =
        match contents with
        | `Data fields -> env, res, fields, updates
        | `Var v -> (
          (* We should only get here in the static allocation case. *)
          match for_static_sets with
          | None -> assert false
          | Some
              { function_slot_offset_for_updates;
                closure_symbol_for_updates;
                _
              } ->
            let update_kind =
              let module UK = C.Update_kind in
              match KS.kind kind with
              | Value ->
                if KS.Subkind.equal (KS.subkind kind) Tagged_immediate
                then UK.tagged_immediates
                else UK.pointers
              | Naked_number Naked_immediate
              | Naked_number Naked_int64
              | Naked_number Naked_nativeint ->
                UK.naked_int64s
              | Naked_number Naked_float -> UK.naked_floats
              | Naked_number Naked_vec128 -> UK.naked_vec128_fields
              (* The "fields" update kinds are used because we are writing into
                 a 64-bit slot, and wish to initialize the whole. *)
              | Naked_number Naked_int32 -> UK.naked_int32_fields
              | Naked_number Naked_float32 -> UK.naked_float32_fields
              | Region | Rec_info ->
                Misc.fatal_errorf "Unexpected value slot kind for %a: %a"
                  Value_slot.print value_slot KS.print kind
            in
            let env, res, updates =
              C.make_update env res dbg update_kind
                ~symbol:(C.symbol ~dbg closure_symbol_for_updates)
                v
                ~index:(slot_offset - function_slot_offset_for_updates)
                ~prev_updates:updates
            in
            env, res, [P.int ~dbg 1n], updates)
      in
      ( List.rev_append fields acc,
        free_vars,
        slot_offset + 1,
        env,
        res,
        eff,
        updates )
    | Function_slot { size; function_slot; last_function_slot } -> (
      let code_id = Function_slot.Map.find function_slot decls in
      let code_symbol =
        R.symbol_of_code_id res code_id ~currently_in_inlined_body:false
      in
      let (kind, params_ty, result_ty), closure_code_pointers, dbg =
        get_func_decl_params_arity env code_id
      in
      let closure_info =
        C.closure_info' ~arity:(kind, params_ty)
          ~startenv:(startenv - slot_offset) ~is_last:last_function_slot
      in
      let acc =
        match for_static_sets with
        | None -> acc
        | Some { closure_symbols; _ } ->
          let function_symbol =
            Function_slot.Map.find function_slot closure_symbols
          in
          List.rev_append (P.define_symbol (R.symbol res function_symbol)) acc
      in
      (* We build here the **reverse** list of fields for the function slot *)
      match closure_code_pointers with
      | Full_application_only ->
        if size <> 2
        then
          Misc.fatal_errorf
            "fill_slot: Function slot %a is of size %d, but it is used to \
             store code ID %a which is classified as Full_application_only (so \
             the expected size is 2)"
            Function_slot.print function_slot size Code_id.print code_id;
        let acc =
          P.int ~dbg closure_info :: P.term_of_symbol ~dbg code_symbol :: acc
        in
        ( acc,
          Backend_var.Set.empty,
          slot_offset + size,
          env,
          res,
          Ece.pure,
          updates )
      | Full_and_partial_application ->
        if size <> 3
        then
          Misc.fatal_errorf
            "fill_slot: Function slot %a is of size %d, but it is used to \
             store code ID %a which is classified as \
             Full_and_partial_application (so the expected size is 3)"
            Function_slot.print function_slot size Code_id.print code_id;
        let acc =
          P.term_of_symbol ~dbg code_symbol
          :: P.int ~dbg closure_info
          :: P.term_of_symbol ~dbg
               (C.curry_function_sym kind params_ty result_ty)
          :: acc
        in
        ( acc,
          Backend_var.Set.empty,
          slot_offset + size,
          env,
          res,
          Ece.pure,
          updates ))

  let rec fill_layout0 for_static_sets decls dbg ~startenv value_slots env res
      effs acc updates ~free_vars ~starting_offset slots =
    match slots with
    | [] -> List.rev acc, free_vars, starting_offset, env, res, effs, updates
    | (slot_offset, slot) :: slots ->
      let acc =
        if starting_offset > slot_offset
        then
          Misc.fatal_errorf "Starting offset %d is past slot offset %d"
            starting_offset slot_offset
        else if starting_offset = slot_offset
        then acc
        else
          (* The space between slot offsets has to be padded with precisely the
             value tagged 0, as it is scanned by the GC during compaction. This
             value can't be confused with either infix headers or inverted
             pointers, as noted in the comment in compact.c *)
          List.init (slot_offset - starting_offset) (fun _ -> P.int ~dbg 1n)
          @ acc
      in
      let acc, slot_free_vars, next_offset, env, res, eff, updates =
        fill_slot for_static_sets decls dbg ~startenv value_slots env res acc
          ~slot_offset updates slot
      in
      let free_vars = Backend_var.Set.union free_vars slot_free_vars in
      let effs = Ece.join eff effs in
      fill_layout0 for_static_sets decls dbg ~startenv value_slots env res effs
        acc updates ~free_vars ~starting_offset:next_offset slots

  let fill_layout for_static_sets decls dbg ~startenv value_slots env res effs
      ~prev_updates slots =
    fill_layout0 for_static_sets decls dbg ~startenv value_slots env res effs []
      prev_updates ~free_vars:Backend_var.Set.empty ~starting_offset:0 slots
end

(* Filling-up of dynamically-allocated sets of closures. *)
module Dynamic = Make_layout_filler (struct
  type cmm_term = Cmm.expression

  let int ~dbg i = C.nativeint ~dbg i

  (* The reason why we can inline simples here is the same as in
     `To_cmm_shared.simple_list`: the first simple translated (and thus in which
     an inlining/substitution can occur), is the last simple that will be
     evaluated, according to the right-to-left evaluation order. This is ensured
     by the fact that we build each field of the set of closures in
     left-to-right order, so that the first translated field is actually
     evaluated last. *)
  let simple ~dbg env res simple =
    let To_cmm_env.{ env; res; expr = { cmm; free_vars; effs } } =
      C.simple ~dbg env res simple
    in
    `Data [cmm], free_vars, env, res, effs

  let infix_header ~dbg ~function_slot_offset =
    C.alloc_infix_header function_slot_offset dbg

  let term_of_symbol ~dbg sym = C.symbol ~dbg sym

  let define_symbol _ = assert false
end)

(* Filling-up of statically-allocated sets of closures. *)
module Static = Make_layout_filler (struct
  type cmm_term = Cmm.data_item

  let int ~dbg:_ i = C.cint i

  let simple ~dbg:_ env res simple =
    let contents = C.simple_static res simple in
    contents, Backend_var.Set.empty, env, res, Ece.pure

  let infix_header ~dbg:_ ~function_slot_offset =
    C.cint (C.infix_header function_slot_offset)

  let term_of_symbol ~dbg:_ sym = C.symbol_address sym

  let define_symbol sym = C.define_symbol sym
end)

(* Translation of "check" attributes on functions. *)

let transl_check_attrib : Zero_alloc_attribute.t -> Cmm.codegen_option list =
  function
  | Default_zero_alloc -> []
  | Assume { strict; never_returns_normally; never_raises; loc } ->
    [Assume_zero_alloc { strict; never_returns_normally; never_raises; loc }]
  | Check { strict; loc } -> [Check_zero_alloc { strict; loc }]

(* Translation of the bodies of functions. *)

let params_and_body0 env res code_id ~fun_dbg ~zero_alloc_attribute
    ~return_continuation ~exn_continuation params ~body ~my_closure
    ~(is_my_closure_used : _ Or_unknown.t) ~my_region ~translate_expr =
  let params =
    let is_my_closure_used =
      match is_my_closure_used with
      | Unknown -> true
      | Known is_my_closure_used -> is_my_closure_used
    in
    if not is_my_closure_used
    then params
    else
      let my_closure_param =
        Bound_parameter.create my_closure Flambda_kind.With_subkind.any_value
      in
      Bound_parameters.append params
        (Bound_parameters.create [my_closure_param])
  in
  (* Init the env and create a jump id for the return continuation in case a
     trap action is attached to one of its calls *)
  let env =
    Env.enter_function_body env ~return_continuation ~exn_continuation
  in
  (* [my_region] can be referenced in [Begin_try_region] primitives so must be
     in the environment; however it should never end up in actual generated
     code, so we don't need any binder for it (this is why we can ignore
     [_bound_var]). If it does end up in generated code, Selection will complain
     and refuse to compile the code. *)
  let env, my_region_var = Env.create_bound_parameter env my_region in
  (* Translate the arg list and body *)
  let env, fun_params = C.function_bound_parameters env params in
  let fun_body, fun_body_free_vars, res = translate_expr env res body in
  let fun_free_vars =
    C.remove_vars_with_machtype
      (C.remove_var_with_provenance fun_body_free_vars my_region_var)
      fun_params
  in
  if not (Backend_var.Set.is_empty fun_free_vars)
  then
    Misc.fatal_errorf
      "Unbound free_vars in function body when translating to cmm: %a@\n\
       function body: %a" Backend_var.Set.print fun_free_vars
      Printcmm.expression fun_body;
  let fun_body =
    if !Clflags.afl_instrument
    then Afl_instrument.instrument_function fun_body fun_dbg
    else fun_body
  in
  let fun_flags =
    transl_check_attrib zero_alloc_attribute
    @
    if Flambda_features.optimize_for_speed () then [] else [Cmm.Reduce_code_size]
  in
  let fun_sym =
    R.symbol_of_code_id res code_id ~currently_in_inlined_body:false
  in
  let fun_poll =
    Env.get_code_metadata env code_id
    |> Code_metadata.poll_attribute |> Poll_attribute.to_lambda
  in
  C.fundecl fun_sym fun_params fun_body fun_flags fun_dbg fun_poll, res

let params_and_body env res code_id p ~fun_dbg ~zero_alloc_attribute
    ~translate_expr =
  Function_params_and_body.pattern_match p
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used
         ~my_region
         ~my_depth:_
         ~free_names_of_body:_
       ->
      try
        params_and_body0 env res code_id ~fun_dbg ~zero_alloc_attribute
          ~return_continuation ~exn_continuation params ~body ~my_closure
          ~is_my_closure_used ~my_region ~translate_expr
      with Misc.Fatal_error as e ->
        let bt = Printexc.get_raw_backtrace () in
        Format.eprintf
          "\n\
           %tContext is:%t translating function %a to Cmm with return cont %a, \
           exn cont %a and body:@ %a\n"
          Flambda_colours.error Flambda_colours.pop Code_id.print code_id
          Continuation.print return_continuation Continuation.print
          exn_continuation Expr.print body;
        Printexc.raise_with_backtrace e bt)

(* Translation of sets of closures. *)

let layout_for_set_of_closures env set =
  Slot_offsets.Layout.make (Env.exported_offsets env)
    (Set_of_closures.function_decls set |> Function_declarations.funs_in_order)
    (Set_of_closures.value_slots set)

let debuginfo_for_set_of_closures env set =
  let code_ids_in_set =
    Set_of_closures.function_decls set
    |> Function_declarations.funs |> Function_slot.Map.data
  in
  let dbg =
    List.map
      (fun code_id -> Env.get_code_metadata env code_id |> Code_metadata.dbg)
      code_ids_in_set
    |> List.sort Debuginfo.compare
  in
  (* Choose the debuginfo with the earliest source location. *)
  match dbg with [] -> Debuginfo.none | dbg :: _ -> dbg

let let_static_set_of_closures0 env res closure_symbols
    (layout : Slot_offsets.Layout.t) set ~prev_updates =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let value_slots = Set_of_closures.value_slots set in
  let dbg = debuginfo_for_set_of_closures env set in
  (* Pick one of the closure symbols to do any updates relative to (see
     above). *)
  let function_slot_offset_for_updates, closure_symbol_for_updates =
    match
      List.find_map
        (fun (offset, (layout_slot : Slot_offsets.Layout.slot)) ->
          match layout_slot with
          | Function_slot { function_slot; _ } -> Some (offset, function_slot)
          | Infix_header | Value_slot _ -> None)
        layout.slots
    with
    | Some (function_slot_offset, function_slot) -> (
      match Function_slot.Map.find function_slot closure_symbols with
      | closure_symbol -> function_slot_offset, R.symbol res closure_symbol
      | exception Not_found ->
        Misc.fatal_errorf "No closure symbol for function slot %a"
          Function_slot.print function_slot)
    | None -> assert false
    (* have checked elsewhere that function slots exist *)
  in
  let for_static_sets =
    { closure_symbols;
      function_slot_offset_for_updates;
      closure_symbol_for_updates
    }
  in
  let l, free_vars, length, env, res, _effs, updates =
    Static.fill_layout (Some for_static_sets) decls dbg
      ~startenv:layout.startenv value_slots env res Ece.pure ~prev_updates
      layout.slots
  in
  if not (Backend_var.Set.is_empty free_vars)
  then
    Misc.fatal_errorf
      "Non-empty set of free_vars for a static set of closures (*not* \
       including updates):@ %a"
      Backend_var.Set.print free_vars;
  let block =
    match l with
    | _ :: _ ->
      let header = C.cint (C.black_closure_header length) in
      header :: l
    | [] ->
      Misc.fatal_error "Cannot statically allocate an empty set of closures"
  in
  env, res, block, updates

let let_static_set_of_closures env res closure_symbols set ~prev_updates =
  let layout = layout_for_set_of_closures env set in
  let_static_set_of_closures0 env res closure_symbols layout set ~prev_updates

(* Sets of closures with no value slots can be statically allocated. This
   usually happens earlier (in Simplify, or Closure_conversion for classic mode)
   but the extra information that To_cmm has about unused closure variables
   enables certain extra cases to be caught. For example the following closure
   [g] is not lifted by Simplify, but can be in To_cmm:

 * let f () =
 *   let x = Sys.opaque_identity 0 in
 *   let y = true in
 *   let g () = if y then 1 else x in
 *   g

 *)
let lift_set_of_closures env res ~body ~bound_vars layout set ~translate_expr
    ~num_normal_occurrences_of_bound_vars =
  (* Generate symbols for the set of closures, and each of the closures *)
  let comp_unit = Compilation_unit.get_current_exn () in
  let dbg = debuginfo_for_set_of_closures env set in
  let cids =
    Function_declarations.funs_in_order (Set_of_closures.function_decls set)
    |> Function_slot.Lmap.keys
  in
  let closure_symbols =
    List.map2
      (fun cid v ->
        let v = Bound_var.var v in
        (* Rename v to have different names for the symbol and variable *)
        let name = Variable.unique_name (Variable.rename v) in
        cid, Symbol.create comp_unit (Linkage_name.of_string name))
      cids bound_vars
    |> Function_slot.Map.of_list
  in
  (* Statically allocate the set of closures *)
  let env, res, static_data, updates =
    let_static_set_of_closures0 env res closure_symbols layout set
      ~prev_updates:None
  in
  (* There should be no updates as there are no value slots *)
  if Option.is_some updates
  then
    Misc.fatal_errorf "non-empty [updates] when lifting set of closures: %a"
      Set_of_closures.print set;
  (* Update the result with the new static data *)
  let res = R.archive_data (R.set_data res static_data) in
  (* Bind the variables to the symbols for function slots. *)
  let env, res =
    List.fold_left2
      (fun (env, res) cid v ->
        let v = Bound_var.var v in
        let sym =
          C.symbol ~dbg
            (R.symbol res (Function_slot.Map.find cid closure_symbols))
        in
        Env.bind_variable env res v ~defining_expr:sym
          ~free_vars_of_defining_expr:Backend_var.Set.empty
          ~num_normal_occurrences_of_bound_vars
          ~effects_and_coeffects_of_defining_expr:Ece.pure_can_be_duplicated)
      (env, res) cids bound_vars
  in
  translate_expr env res body

let let_dynamic_set_of_closures0 env res ~body ~bound_vars set
    (layout : Slot_offsets.Layout.t) ~num_normal_occurrences_of_bound_vars
    ~(closure_alloc_mode : Alloc_mode.For_allocations.t) ~translate_expr =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs_in_order fun_decls in
  let value_slots = Set_of_closures.value_slots set in
  let dbg = debuginfo_for_set_of_closures env set in
  let effs : Ece.t =
    ( Only_generative_effects Immutable,
      (match closure_alloc_mode with
      | Heap -> No_coeffects
      | Local _ -> Has_coeffects),
      Strict )
  in
  let decl_map =
    decls |> Function_slot.Lmap.bindings |> Function_slot.Map.of_list
  in
  let l, free_vars, _offset, env, res, effs, updates =
    Dynamic.fill_layout None decl_map dbg ~startenv:layout.startenv value_slots
      env res effs ~prev_updates:None layout.slots
  in
  assert (Option.is_none updates);
  let csoc =
    assert (List.compare_length_with l 0 > 0);
    let tag = Tag.(to_int closure_tag) in
    C.make_alloc
      ~mode:(Alloc_mode.For_allocations.to_lambda closure_alloc_mode)
      dbg tag l
  in
  let soc_var = Variable.create "*set_of_closures*" in
  let defining_expr = Env.simple csoc free_vars in
  let env, res =
    Env.bind_variable_to_primitive env res soc_var ~inline:Env.Do_not_inline
      ~defining_expr ~effects_and_coeffects_of_defining_expr:effs
  in
  (* Get from the env the cmm variable that was created and bound to the
     compiled set of closures. *)
  let To_cmm_env.
        { env;
          res;
          expr = { cmm = soc_cmm_var; free_vars = s_free_vars; effs = peff }
        } =
    Env.inline_variable env res soc_var
  in
  assert (
    match To_cmm_effects.classify_by_effects_and_coeffects peff with
    | Pure -> true
    | Generative_immutable | Effect | Coeffect_only -> false);
  (* Helper function to get the cmm expr for a closure offset *)
  let get_closure_by_offset env function_slot =
    match
      Exported_offsets.function_slot_offset (Env.exported_offsets env)
        function_slot
    with
    | Some (Live_function_slot { offset; _ }) ->
      Some
        (C.infix_field_address ~dbg:Debuginfo.none soc_cmm_var offset, Ece.pure)
    | Some Dead_function_slot -> None
    | None ->
      Misc.fatal_errorf "Missing offset for function slot %a"
        Function_slot.print function_slot
  in
  (* Add env bindings for all of the function slots. *)
  let env, res =
    List.fold_left2
      (fun (env, res) cid v ->
        match get_closure_by_offset env cid with
        | None -> env, res
        | Some (defining_expr, effects_and_coeffects_of_defining_expr) ->
          let v = Bound_var.var v in
          Env.bind_variable env res v ~defining_expr
            ~free_vars_of_defining_expr:s_free_vars
            ~num_normal_occurrences_of_bound_vars
            ~effects_and_coeffects_of_defining_expr)
      (env, res)
      (Function_slot.Lmap.keys decls)
      bound_vars
  in
  translate_expr env res body

let let_dynamic_set_of_closures env res ~body ~bound_vars
    ~num_normal_occurrences_of_bound_vars set ~translate_expr =
  let layout = layout_for_set_of_closures env set in
  if layout.empty_env
  then
    lift_set_of_closures env res ~body ~bound_vars layout set ~translate_expr
      ~num_normal_occurrences_of_bound_vars
  else
    let_dynamic_set_of_closures0 env res ~body ~bound_vars
      ~num_normal_occurrences_of_bound_vars set layout
      ~closure_alloc_mode:(Set_of_closures.alloc_mode set)
      ~translate_expr
