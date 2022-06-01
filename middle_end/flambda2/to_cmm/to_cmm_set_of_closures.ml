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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module Env = To_cmm_env
module Ece = Effects_and_coeffects
module R = To_cmm_result

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

type translate_expr =
  To_cmm_env.t -> To_cmm_result.t -> Expr.t -> Cmm.expression * To_cmm_result.t

(* Filling of closure blocks *)

type closure_code_pointers =
  | Full_application_only
  | Full_and_partial_application

let get_func_decl_params_arity t code_id =
  let info = Env.get_code_metadata t code_id in
  let num_params =
    Flambda_arity.With_subkinds.cardinal (Code_metadata.params_arity info)
  in
  let kind : Lambda.function_kind =
    if Code_metadata.is_tupled info
    then Lambda.Tupled
    else
      Lambda.Curried { nlocal = Code_metadata.num_trailing_local_params info }
  in
  let closure_code_pointers =
    match kind, num_params with
    | Curried _, (0 | 1) -> Full_application_only
    | (Curried _ | Tupled), _ -> Full_and_partial_application
  in
  let arity = kind, num_params in
  arity, closure_code_pointers, Code_metadata.dbg info

type pass =
  | Offsets
  | Data_items

module Make_layout_filler (P : sig
  type cmm_term

  val int : dbg:Debuginfo.t -> nativeint -> cmm_term

  val simple :
    dbg:Debuginfo.t ->
    To_cmm_env.t ->
    To_cmm_result.t ->
    Simple.t ->
    [`Data of cmm_term list | `Var of Variable.t] * To_cmm_env.t * Ece.t

  val infix_header : dbg:Debuginfo.t -> function_slot_offset:int -> cmm_term

  val symbol_from_linkage_name : dbg:Debuginfo.t -> Linkage_name.t -> cmm_term

  val define_global_symbol :
    To_cmm_result.t ->
    Symbol.t ->
    size_in_words_excluding_header:int ->
    To_cmm_result.t

  val increment_symbol_offset :
    To_cmm_result.t -> size_in_words:int -> To_cmm_result.t
end) : sig
  val fill_layout :
    pass ->
    To_cmm_result.t ->
    Symbol.t Function_slot.Map.t option ->
    Code_id.t Function_slot.Map.t ->
    Debuginfo.t ->
    startenv:int ->
    Simple.t Value_slot.Map.t ->
    Env.t ->
    Ece.t ->
    prev_updates:Cmm.expression option ->
    (int * Slot_offsets.layout_slot) list ->
    P.cmm_term list
    * int
    * Env.t
    * To_cmm_result.t
    * Ece.t
    * Cmm.expression option
end = struct
  (* The [offset]s here are measured in units of words. *)
  let fill_slot (pass : pass) r symbs decls dbg ~startenv value_slots env acc
      ~slot_offset updates ~(all_slots : (int * Slot_offsets.layout_slot) list)
      slot =
    match (slot : Slot_offsets.layout_slot) with
    | Infix_header ->
      let field = P.infix_header ~function_slot_offset:(slot_offset + 1) ~dbg in
      let r =
        match pass with
        | Offsets -> P.increment_symbol_offset r ~size_in_words:1
        | Data_items -> r
      in
      field :: acc, slot_offset + 1, env, r, Ece.pure, updates
    | Value_slot v ->
      let simple =
        match pass with
        | Offsets ->
          (* [Simple]s cannot be translated during the offsets pass, as they
             might involve symbols. *)
          Simple.const_zero
        | Data_items -> Value_slot.Map.find v value_slots
      in
      let contents, env, eff = P.simple ~dbg env r simple in
      let env, fields, updates =
        match contents with
        | `Data fields ->
          assert (List.length fields = 1);
          env, fields, updates
        | `Var v -> (
          (* We should only get here in the static allocation case. *)
          match symbs with
          | None -> assert false
          | Some symbs ->
            assert (match pass with Data_items -> true | Offsets -> false);
            (* Pick one of the closure symbols to do the updates relative to. *)
            let function_slot_offset, closure_symbol =
              match
                List.find_map
                  (fun (offset, (layout_slot : Slot_offsets.layout_slot)) ->
                    match layout_slot with
                    | Function_slot function_slot -> Some (offset, function_slot)
                    | Infix_header | Value_slot _ -> None)
                  all_slots
              with
              | Some (function_slot_offset, function_slot) -> (
                match Function_slot.Map.find function_slot symbs with
                | closure_symbol -> function_slot_offset, closure_symbol
                | exception Not_found ->
                  Misc.fatal_errorf "No closure symbol for function slot %a"
                    Function_slot.print function_slot)
              | None -> assert false
              (* have checked elsewhere that function slots exist *)
            in
            let env, updates =
              C.make_update env dbg Word_val
                ~symbol:(R.expr_symbol_address r closure_symbol dbg)
                v
                ~index:(slot_offset - function_slot_offset)
                ~prev_updates:updates
            in
            env, [P.int ~dbg 1n], updates)
      in
      let r =
        match pass with
        | Offsets -> P.increment_symbol_offset r ~size_in_words:1
        | Data_items -> r
      in
      List.rev_append fields acc, slot_offset + 1, env, r, eff, updates
    | Function_slot c -> (
      let code_id = Function_slot.Map.find c decls in
      let code_linkage_name = Code_id.linkage_name code_id in
      let arity, closure_code_pointers, dbg =
        get_func_decl_params_arity env code_id
      in
      let closure_info =
        C.closure_info ~arity ~startenv:(startenv - slot_offset)
      in
      let r =
        match symbs with
        | None -> r
        | Some symbs -> (
          match pass with
          | Data_items -> r
          | Offsets ->
            let closure_symbol = Function_slot.Map.find c symbs in
            P.define_global_symbol r closure_symbol
              ~size_in_words_excluding_header:
                ((match closure_code_pointers with
                 | Full_application_only -> 2
                 | Full_and_partial_application -> 3)
                - 1 (* infix headers accounted for elsewhere *)))
      in
      (* We build here the **reverse** list of fields for the function slot *)
      match closure_code_pointers with
      | Full_application_only ->
        let acc =
          P.int ~dbg closure_info
          :: P.symbol_from_linkage_name ~dbg code_linkage_name
          :: acc
        in
        acc, slot_offset + 2, env, r, Ece.pure, updates
      | Full_and_partial_application ->
        let acc =
          P.symbol_from_linkage_name ~dbg code_linkage_name
          :: P.int ~dbg closure_info
          :: P.symbol_from_linkage_name ~dbg
               (Linkage_name.create (C.curry_function_sym arity))
          :: acc
        in
        acc, slot_offset + 3, env, r, Ece.pure, updates)

  let rec fill_layout0 (pass : pass) r symbs decls dbg ~startenv value_slots env
      effs acc updates ~starting_offset ~all_slots slots =
    match slots with
    | [] ->
      (* Skip over the header of the subsequent block *)
      let r =
        match pass with
        | Offsets -> P.increment_symbol_offset r ~size_in_words:1
        | Data_items -> r
      in
      List.rev acc, starting_offset, env, r, effs, updates
    | (slot_offset, slot) :: slots ->
      let r, acc =
        if starting_offset > slot_offset
        then
          Misc.fatal_errorf "Starting offset %d is past slot offset %d"
            starting_offset slot_offset
        else if starting_offset = slot_offset
        then r, acc
        else
          let gap_in_words = slot_offset - starting_offset in
          let r =
            match pass with
            | Offsets -> P.increment_symbol_offset r ~size_in_words:gap_in_words
            | Data_items -> r
          in
          let acc = List.init gap_in_words (fun _ -> P.int ~dbg 1n) @ acc in
          r, acc
      in
      let acc, next_offset, env, r, eff, updates =
        fill_slot pass r symbs decls dbg ~startenv value_slots env acc
          ~slot_offset updates ~all_slots slot
      in
      let effs = Ece.join eff effs in
      fill_layout0 pass r symbs decls dbg ~startenv value_slots env effs acc
        updates ~starting_offset:next_offset ~all_slots slots

  let fill_layout pass r symbs decls dbg ~startenv value_slots env effs
      ~prev_updates slots =
    fill_layout0 pass r symbs decls dbg ~startenv value_slots env effs []
      prev_updates ~starting_offset:0 ~all_slots:slots slots
end

(* Filling-up of dynamically-allocated sets of closures. *)
module Dynamic = Make_layout_filler (struct
  type cmm_term = Cmm.expression

  let int ~dbg i = C.nativeint ~dbg i

  let simple ~dbg env r simple =
    let term, env, eff = C.simple ~dbg env r simple in
    `Data [term], env, eff

  let infix_header ~dbg ~function_slot_offset =
    C.alloc_infix_header function_slot_offset dbg

  let symbol_from_linkage_name ~dbg linkage_name =
    C.symbol_from_linkage_name ~dbg linkage_name

  let define_global_symbol _ _ ~size_in_words_excluding_header:_ = assert false

  let increment_symbol_offset r ~size_in_words:_ = r
end)

(* Filling-up of statically-allocated sets of closures. *)
module Static = Make_layout_filler (struct
  type cmm_term = Cmm.data_item

  let int ~dbg:_ i = C.cint i

  let simple ~dbg:_ env r simple =
    let contents = C.simple_static r simple in
    contents, env, Ece.pure

  let infix_header ~dbg:_ ~function_slot_offset =
    C.cint (C.infix_header function_slot_offset)

  let symbol_from_linkage_name ~dbg:_ linkage_name =
    C.symbol_address (Linkage_name.to_string linkage_name)

  let define_global_symbol r sym ~size_in_words_excluding_header =
    R.record_symbol_offset r sym ~size_in_words_excluding_header

  let increment_symbol_offset r ~size_in_words =
    R.increment_symbol_offset r ~size_in_words
end)

(* Translation of the bodies of functions. *)

let params_and_body0 env res code_id ~fun_dbg ~return_continuation
    ~exn_continuation params ~body ~my_closure
    ~(is_my_closure_used : _ Or_unknown.t) ~translate_expr =
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
  (* Translate the arg list and body *)
  let env, fun_args = C.bound_parameters env params in
  let fun_body, res = translate_expr env res body in
  let fun_flags =
    if Flambda_features.optimize_for_speed () then [] else [Cmm.Reduce_code_size]
  in
  let linkage_name = Linkage_name.to_string (Code_id.linkage_name code_id) in
  C.fundecl linkage_name fun_args fun_body fun_flags fun_dbg, res

let params_and_body env res code_id p ~fun_dbg ~translate_expr =
  Function_params_and_body.pattern_match p
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used
         ~my_depth:_
         ~free_names_of_body:_
       ->
      try
        params_and_body0 env res code_id ~fun_dbg ~return_continuation
          ~exn_continuation params ~body ~my_closure ~is_my_closure_used
          ~translate_expr
      with Misc.Fatal_error as e ->
        Format.eprintf
          "\n\
           %sContext is:%s translating function %a to Cmm with return cont %a, \
           exn cont %a and body:@ %a\n"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Code_id.print code_id Continuation.print return_continuation
          Continuation.print exn_continuation Expr.print body;
        raise e)

(* Translation of sets of closures. *)

let layout_for_set_of_closures env set =
  Slot_offsets.layout (Env.exported_offsets env)
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

let let_static_set_of_closures0 (pass : pass) env r symbs
    (layout : Slot_offsets.layout) set ~prev_updates =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let value_slots = Set_of_closures.value_slots set in
  let dbg = debuginfo_for_set_of_closures env set in
  let l, length, env, r, _effs, updates =
    Static.fill_layout pass r (Some symbs) decls dbg ~startenv:layout.startenv
      value_slots env Ece.pure ~prev_updates layout.slots
  in
  let block =
    match l with
    | _ :: _ ->
      let header = C.cint (C.black_closure_header length) in
      header :: l
    | [] ->
      Misc.fatal_error "Cannot statically allocate an empty set of closures"
  in
  env, r, block, updates

let let_static_set_of_closures pass env r symbs set ~prev_updates =
  let layout = layout_for_set_of_closures env set in
  let_static_set_of_closures0 pass env r symbs layout set ~prev_updates

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
let lift_set_of_closures env res ~body ~bound_vars layout set ~translate_expr =
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
        cid, Symbol.create comp_unit (Linkage_name.create name))
      cids bound_vars
    |> Function_slot.Map.of_list
  in
  (* Statically allocate the set of closures *)
  let env, res, static_data, updates =
    let_static_set_of_closures0 Offsets env res closure_symbols layout set
      ~prev_updates:None
  in
  (* There should be no updates as there are no value slots *)
  if Option.is_some updates
  then
    Misc.fatal_errorf "non-empty [updates] when lifting set of closures: %a"
      Set_of_closures.print set;
  (* Update the result with the new static data *)
  let res = R.archive_offset_data (R.set_data res static_data) in
  (* Bind the variables to the symbols for function slots. *)
  (* CR-someday gbury: inline the variables (requires extending To_cmm_env to
     inline pure variables more than once). *)
  let env =
    List.fold_left2
      (fun acc cid v ->
        let v = Bound_var.var v in
        let sym =
          R.expr_symbol_address res
            (Function_slot.Map.find cid closure_symbols)
            dbg
        in
        Env.bind_variable acc v ~effects_and_coeffects_of_defining_expr:Ece.pure
          ~num_normal_occurrences_of_bound_vars:Unknown ~defining_expr:sym)
      env cids bound_vars
  in
  translate_expr env res body

let let_dynamic_set_of_closures0 env res ~body ~bound_vars set
    (layout : Slot_offsets.layout) ~num_normal_occurrences_of_bound_vars
    ~(closure_alloc_mode : Alloc_mode.t) ~translate_expr =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs_in_order fun_decls in
  let value_slots = Set_of_closures.value_slots set in
  let dbg = debuginfo_for_set_of_closures env set in
  let effs : Ece.t =
    ( Only_generative_effects Immutable,
      match closure_alloc_mode with
      | Heap -> No_coeffects
      | Local -> Has_coeffects )
  in
  let decl_map =
    decls |> Function_slot.Lmap.bindings |> Function_slot.Map.of_list
  in
  let l, _offset, env, res, effs, updates =
    Dynamic.fill_layout Data_items res None decl_map dbg
      ~startenv:layout.startenv value_slots env effs ~prev_updates:None
      layout.slots
  in
  assert (Option.is_none updates);
  let csoc =
    assert (List.compare_length_with l 0 > 0);
    let tag = Tag.(to_int closure_tag) in
    C.make_alloc ~mode:(Alloc_mode.to_lambda closure_alloc_mode) dbg tag l
  in
  let soc_var = Variable.create "*set_of_closures*" in
  let env =
    Env.bind_variable env soc_var ~effects_and_coeffects_of_defining_expr:effs
      ~num_normal_occurrences_of_bound_vars:Unknown ~defining_expr:csoc
  in
  (* Get from the env the cmm variable that was created and bound to the
     compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (
    match To_cmm_effects.classify_by_effects_and_coeffects peff with
    | Pure -> true
    | Effect | Coeffect_only -> false);
  (* Add env bindings for all of the value slots. *)
  let get_closure_by_offset env set_cmm function_slot =
    match
      Exported_offsets.function_slot_offset (Env.exported_offsets env)
        function_slot
    with
    | Some (Live_function_slot { offset; _ }) ->
      Some (C.infix_field_address ~dbg:Debuginfo.none set_cmm offset, Ece.pure)
    | Some Dead_function_slot -> None
    | None ->
      Misc.fatal_errorf "Missing offset for function slot %a"
        Function_slot.print function_slot
  in
  (* Add env bindings for all of the function slots. *)
  let env =
    List.fold_left2
      (fun acc cid v ->
        match get_closure_by_offset env soc_cmm_var cid with
        | None -> acc
        | Some (defining_expr, effects_and_coeffects_of_defining_expr) ->
          let v = Bound_var.var v in
          Env.bind_variable acc v
            ~num_normal_occurrences_of_bound_vars:
              (Known num_normal_occurrences_of_bound_vars)
            ~effects_and_coeffects_of_defining_expr ~defining_expr)
      env
      (Function_slot.Lmap.keys decls)
      bound_vars
  in
  translate_expr env res body

let let_dynamic_set_of_closures env res ~body ~bound_vars
    ~num_normal_occurrences_of_bound_vars set ~translate_expr =
  let layout = layout_for_set_of_closures env set in
  if layout.empty_env
  then lift_set_of_closures env res ~body ~bound_vars layout set ~translate_expr
  else
    let_dynamic_set_of_closures0 env res ~body ~bound_vars
      ~num_normal_occurrences_of_bound_vars set layout
      ~closure_alloc_mode:(Set_of_closures.alloc_mode set)
      ~translate_expr
