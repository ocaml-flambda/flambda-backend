(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

let get_whole_closure_symbol =
  let whole_closure_symb_count = ref 0 in
  fun ~set_of_closures_symbol_ref ->
    match !set_of_closures_symbol_ref with
    | Some set_of_closures_symbol -> set_of_closures_symbol
    | None ->
      incr whole_closure_symb_count;
      let comp_unit = Compilation_unit.get_current_exn () in
      let linkage_name =
        Linkage_name.create
          (Printf.sprintf ".clos_%d" !whole_closure_symb_count)
      in
      let set_of_closures_symbol = Symbol.create comp_unit linkage_name in
      set_of_closures_symbol_ref := Some set_of_closures_symbol;
      set_of_closures_symbol

module Make_layout_filler (P : sig
  type cmm_term

  val int : ?dbg:Debuginfo.t -> nativeint -> cmm_term

  val simple :
    To_cmm_env.t ->
    Simple.t ->
    [`Data of cmm_term list | `Var of Variable.t] * To_cmm_env.t * Ece.t

  val infix_header : first_value_slot_offset:int -> Debuginfo.t -> cmm_term

  val symbol_from_linkage_name : ?dbg:Debuginfo.t -> Linkage_name.t -> cmm_term

  val define_global_symbol : string -> cmm_term list
end) : sig
  val fill_layout :
    set_of_closures_symbol_ref:Symbol.t option ref ->
    Symbol.t Function_slot.Map.t option ->
    Code_id.t Function_slot.Map.t ->
    startenv:int ->
    Simple.t Value_slot.Map.t ->
    Env.t ->
    Ece.t ->
    P.cmm_term list ->
    Cmm.expression option ->
    starting_offset:int ->
    (int * Slot_offsets.layout_slot) list ->
    P.cmm_term list * int * Env.t * Ece.t * Cmm.expression option
end = struct
  (* The [offset]s here are measured in units of words. *)
  let fill_slot ~set_of_closures_symbol_ref symbs decls ~startenv elts env acc
      ~slot_offset updates slot =
    match (slot : Slot_offsets.layout_slot) with
    | Infix_header ->
      let field =
        P.infix_header ~first_value_slot_offset:(slot_offset + 1) Debuginfo.none
      in
      field :: acc, slot_offset + 1, env, Ece.pure, updates
    | Value_slot v ->
      let simple = Value_slot.Map.find v elts in
      let contents, env, eff = P.simple env simple in
      let env, fields, updates =
        match contents with
        | `Data fields -> env, fields, updates
        | `Var v ->
          (* We should only get here in the static allocation case. *)
          assert (Option.is_some symbs);
          let set_of_closures_symbol =
            get_whole_closure_symbol ~set_of_closures_symbol_ref
          in
          let env, updates =
            C.make_update env Word_val
              ~symbol:(C.symbol set_of_closures_symbol)
              v ~index:slot_offset ~prev_updates:updates
          in
          env, [P.int 1n], updates
      in
      List.rev fields @ acc, slot_offset + 1, env, eff, updates
    | Function_slot c -> (
      let code_id = Function_slot.Map.find c decls in
      (* CR-someday mshinwell: We should probably use the code's [dbg], but it
         would be tricky to get hold of, and this is very unlikely to make any
         difference in practice. mshinwell: this can now be got from
         Code_metadata. *)
      let dbg = Debuginfo.none in
      let code_linkage_name = Code_id.linkage_name code_id in
      let arity, closure_code_pointers =
        Env.get_func_decl_params_arity env code_id
      in
      let closure_info =
        C.closure_info ~arity ~startenv:(startenv - slot_offset)
      in
      let acc =
        match symbs with
        | None -> acc
        | Some symbs ->
          let function_symbol = Function_slot.Map.find c symbs in
          List.rev
            (P.define_global_symbol
               (Symbol.linkage_name_as_string function_symbol))
          @ acc
      in
      (* We build here the **reverse** list of fields for the closure *)
      match closure_code_pointers with
      | Full_application_only ->
        let acc =
          P.int ~dbg closure_info
          :: P.symbol_from_linkage_name ~dbg code_linkage_name
          :: acc
        in
        acc, slot_offset + 2, env, Ece.pure, updates
      | Full_and_partial_application ->
        let acc =
          P.symbol_from_linkage_name ~dbg code_linkage_name
          :: P.int ~dbg closure_info
          :: P.symbol_from_linkage_name ~dbg
               (Linkage_name.create (C.curry_function_sym arity))
          :: acc
        in
        acc, slot_offset + 3, env, Ece.pure, updates)

  let rec fill_layout ~set_of_closures_symbol_ref symbs decls ~startenv elts env
      effs acc updates ~starting_offset slots =
    match slots with
    | [] -> List.rev acc, starting_offset, env, effs, updates
    | (slot_offset, slot) :: slots ->
      let acc =
        if starting_offset > slot_offset
        then
          Misc.fatal_errorf "Starting offset %d is past slot offset %d"
            starting_offset slot_offset;
        List.init (slot_offset - starting_offset) (fun _ -> P.int 1n)
      in
      let acc, next_offset, env, eff, updates =
        fill_slot ~set_of_closures_symbol_ref symbs decls ~startenv elts env acc
          ~slot_offset updates slot
      in
      let effs = Ece.join eff effs in
      fill_layout ~set_of_closures_symbol_ref symbs decls ~startenv elts env
        effs acc updates ~starting_offset:next_offset slots
end

(* Filling-up of dynamically-allocated sets of closures. *)
module Dynamic = Make_layout_filler (struct
  type cmm_term = Cmm.expression

  let int = C.nativeint

  let simple env simple =
    let term, env, eff = C.simple env simple in
    `Data [term], env, eff

  let infix_header ~first_value_slot_offset dbg =
    C.alloc_infix_header first_value_slot_offset dbg

  let symbol_from_linkage_name = C.symbol_from_linkage_name

  let define_global_symbol _ = []
end)

(* Filling-up of statically-allocated sets of closures. *)
module Static = Make_layout_filler (struct
  type cmm_term = Cmm.data_item

  let int ?dbg i =
    ignore dbg;
    C.cint i

  let simple env simple =
    let env, contents = C.simple_static env simple in
    contents, env, Ece.pure

  let infix_header ~first_value_slot_offset _dbg =
    C.cint (C.infix_header first_value_slot_offset)

  let symbol_from_linkage_name ?dbg linkage_name =
    ignore dbg;
    C.symbol_address (Linkage_name.to_string linkage_name)

  let define_global_symbol sym = List.rev (C.define_symbol ~global:true sym)
end)

(* Helpers for translating functions *)

let function_params params my_closure ~(is_my_closure_used : _ Or_unknown.t) =
  let is_my_closure_used =
    match is_my_closure_used with
    | Unknown -> true
    | Known is_my_closure_used -> is_my_closure_used
  in
  if is_my_closure_used
  then
    let my_closure_param =
      Bound_parameter.create my_closure Flambda_kind.With_subkind.any_value
    in
    Bound_parameters.append params (Bound_parameters.create [my_closure_param])
  else params

let function_flags () =
  if Flambda_features.optimize_for_speed () then [] else [Cmm.Reduce_code_size]

(* Translate a function declaration. *)

let params_and_body env res code_id p ~fun_dbg ~translate_expr =
  Function_params_and_body.pattern_match p
    ~f:(fun
         ~return_continuation:k
         ~exn_continuation:k_exn
         params
         ~body
         ~my_closure
         ~is_my_closure_used
         ~my_depth:_
         ~free_names_of_body:_
       ->
      try
        let params = function_params params my_closure ~is_my_closure_used in
        (* Init the env and create a jump id for the ret closure in case a trap
           action is attached to one of tis call *)
        let env = Env.enter_function_def env k k_exn in
        (* translate the arg list and body *)
        let env, fun_args = C.bound_parameters env params in
        let fun_body, res = translate_expr env res body in
        let fun_flags = function_flags () in
        let linkage_name =
          Linkage_name.to_string (Code_id.linkage_name code_id)
        in
        C.fundecl linkage_name fun_args fun_body fun_flags fun_dbg, res
      with Misc.Fatal_error as e ->
        Format.eprintf
          "\n\
           %sContext is:%s translating function %a to Cmm with return cont %a, \
           exn cont %a and body:@ %a\n"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Code_id.print code_id Continuation.print k Continuation.print k_exn
          Expr.print body;
        raise e)

let let_static_set_of_closures env symbs set (layout : Slot_offsets.layout)
    ~prev_updates =
  let set_of_closures_symbol_ref = ref None in
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.value_slots set in
  let l, length, env, _effs, updates =
    Static.fill_layout ~set_of_closures_symbol_ref (Some symbs) decls
      ~startenv:layout.startenv elts env Ece.pure [] prev_updates
      ~starting_offset:0 layout.slots
  in
  let block =
    match l with
    (* Closures can be deleted by flambda but still appear in let_symbols, hence
       we may end up with empty sets of closures. *)
    | [] -> []
    (* Regular case. *)
    | _ ->
      let header = C.cint (C.black_closure_header length) in
      let sdef =
        match !set_of_closures_symbol_ref with
        | None -> []
        | Some s ->
          C.define_symbol ~global:false (Symbol.linkage_name_as_string s)
      in
      (header :: sdef) @ l
  in
  env, block, updates

(* Sets of closures with no environment can be turned into statically allocated
   symbols, rather than have to allocate them each time *)
let lift_set_of_closures env res ~body ~bound_vars s layout ~translate_expr =
  (* Generate symbols for the set of closures, and each of the closures *)
  let comp_unit = Compilation_unit.get_current_exn () in
  let cids =
    Function_declarations.funs_in_order (Set_of_closures.function_decls s)
    |> Function_slot.Lmap.keys
  in
  let closure_symbols =
    List.map2
      (fun cid v ->
        let v = Bound_var.var v in
        (* rename v just to have a different name for the symbol and the
           variable *)
        let name = Variable.unique_name (Variable.rename v) in
        cid, Symbol.create comp_unit (Linkage_name.create name))
      cids bound_vars
    |> Function_slot.Map.of_list
  in
  (* Statically allocate the set of closures *)
  let env, static_data, updates =
    let_static_set_of_closures env closure_symbols s layout ~prev_updates:None
  in
  (* As there is no env vars for the set of closures, there must be no
     updates *)
  begin
    match updates with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf "non-empty updates when lifting set of closures"
  end;
  (* update the result with the new static data *)
  let res = R.archive_data (R.set_data res static_data) in
  (* Bind the variables to the symbols for function slots. *)
  (* CR gbury: inline the variables (requires extending To_cmm_env to inline
     pure variables more than once). *)
  let env =
    List.fold_left2
      (fun acc cid v ->
        let v = Bound_var.var v in
        let sym = C.symbol (Function_slot.Map.find cid closure_symbols) in
        Env.bind_variable acc v Ece.pure false sym)
      env cids bound_vars
  in
  (* go on in the body *)
  translate_expr env res body

let make_closure_block ?(dbg = Debuginfo.none) alloc_mode l =
  assert (List.compare_length_with l 0 > 0);
  let tag = Tag.(to_int closure_tag) in
  C.make_alloc ~mode:(Alloc_mode.to_lambda alloc_mode) dbg tag l

(* Sets of closures with a non-empty environment are allocated *)
let let_dynamic_set_of_closures env res ~body ~bound_vars s
    (layout : Slot_offsets.layout) ~num_normal_occurrences_of_bound_vars
    ~(closure_alloc_mode : Alloc_mode.t) ~translate_expr ~let_expr_bind =
  (* unwrap the set of closures a bit *)
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs_in_order fun_decls in
  let elts = Set_of_closures.value_slots s in
  (* Allocating the closure has at least generative effects. It is also deemed
     to have coeffects if it is a local closure allocation. *)
  let effs =
    ( Effects.Only_generative_effects Immutable,
      match closure_alloc_mode with
      | Heap -> Coeffects.No_coeffects
      | Local -> Coeffects.Has_coeffects )
  in
  let decl_map =
    decls |> Function_slot.Lmap.bindings |> Function_slot.Map.of_list
  in
  let l, _offset, env, effs, updates =
    Dynamic.fill_layout ~set_of_closures_symbol_ref:(ref None) None decl_map
      ~startenv:layout.startenv elts env effs [] None ~starting_offset:0
      layout.slots
  in
  assert (Option.is_none updates);
  let csoc = make_closure_block closure_alloc_mode l in
  (* Create a variable to hold the set of closure *)
  let soc_var = Variable.create "*set_of_closures*" in
  let env = Env.bind_variable env soc_var effs false csoc in
  (* Get from the env the cmm variable that was created and bound to the
     compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (Env.classify peff = Env.Pure);
  (* Add env bindings for all the value slots. *)
  let get_closure_by_offset env set_cmm cid =
    match Env.function_slot_offset env cid with
    | Live_function_slot { offset; _ } ->
      Some (C.infix_field_address ~dbg:Debuginfo.none set_cmm offset, Ece.pure)
    | Dead_function_slot -> None
  in
  let env =
    List.fold_left2
      (fun acc cid v ->
        match get_closure_by_offset env soc_cmm_var cid with
        | None -> acc
        | Some (e, effs) ->
          let v = Bound_var.var v in
          let_expr_bind ?extra:None acc v ~num_normal_occurrences_of_bound_vars
            e effs)
      env
      (Function_slot.Lmap.keys decls)
      bound_vars
  in
  (* The set of closures, as well as the individual closure variables are
     correctly set in the env, go on translating the body. *)
  translate_expr env res body

let let_dynamic_set_of_closures env res ~body ~bound_vars
    ~num_normal_occurrences_of_bound_vars s ~translate_expr ~let_expr_bind =
  let layout = Env.layout env s in
  if layout.empty_env
  then lift_set_of_closures env res ~body ~bound_vars s layout ~translate_expr
  else
    let_dynamic_set_of_closures env res ~body ~bound_vars
      ~num_normal_occurrences_of_bound_vars s layout
      ~closure_alloc_mode:(Set_of_closures.alloc_mode s)
      ~translate_expr ~let_expr_bind
