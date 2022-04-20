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
  include To_cmm_helper
end

type translate_expr =
  To_cmm_env.t -> To_cmm_result.t -> Expr.t -> Cmm.expression * To_cmm_result.t

(* Filling of dynamically-allocated closure blocks *)

let get_closure_by_offset env set_cmm cid =
  match Env.function_slot_offset env cid with
  | Live_function_slot { offset; _ } ->
    Some (C.infix_field_address ~dbg:Debuginfo.none set_cmm offset, Ece.pure)
  | Dead_function_slot -> None

let fill_slot decls startenv elts env acc offset slot =
  match (slot : Slot_offsets.layout_slot) with
  | Infix_header ->
    let field = C.alloc_infix_header (offset + 1) Debuginfo.none in
    field :: acc, offset + 1, env, Ece.pure
  | Value_slot v ->
    let field, env, eff = C.simple env (Value_slot.Map.find v elts) in
    field :: acc, offset + 1, env, eff
  | Function_slot (c : Function_slot.t) -> (
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
    let closure_info = C.closure_info ~arity ~startenv:(startenv - offset) in
    (* We build here the **reverse** list of fields for the closure *)
    match closure_code_pointers with
    | Full_application_only ->
      let acc =
        C.nativeint ~dbg closure_info
        :: C.symbol_from_linkage_name ~dbg code_linkage_name
        :: acc
      in
      acc, offset + 2, env, Ece.pure
    | Full_and_partial_application ->
      let acc =
        C.symbol_from_linkage_name ~dbg code_linkage_name
        :: C.nativeint ~dbg closure_info
        :: C.symbol_from_linkage_name ~dbg
             (Linkage_name.create (C.curry_function_sym arity))
        :: acc
      in
      acc, offset + 3, env, Ece.pure)

(* Filling of statically-allocated closure blocks *)

let get_whole_closure_symbol =
  let whole_closure_symb_count = ref 0 in
  fun r ->
    match !r with
    | Some s -> s
    | None ->
      incr whole_closure_symb_count;
      let comp_unit = Compilation_unit.get_current_exn () in
      let linkage_name =
        Linkage_name.create
        @@ Printf.sprintf ".clos_%d" !whole_closure_symb_count
      in
      let s = Symbol.create comp_unit linkage_name in
      r := Some s;
      s

let fill_static_slot s symbs decls startenv elts env acc offset updates slot =
  match (slot : Slot_offsets.layout_slot) with
  | Infix_header ->
    let field = C.cint (C.infix_header (offset + 1)) in
    env, field :: acc, offset + 1, updates
  | Value_slot v ->
    let env, contents = C.simple_static env (Value_slot.Map.find v elts) in
    let env, fields, updates =
      match contents with
      | `Data fields -> env, fields, updates
      | `Var v ->
        let s = get_whole_closure_symbol s in
        let env, updates =
          C.make_update env Cmm.Word_val ~symbol:(C.symbol s) v ~index:offset
            ~prev_updates:updates
        in
        env, [C.cint 1n], updates
    in
    env, List.rev fields @ acc, offset + 1, updates
  | Function_slot c -> (
    let code_id = Function_slot.Map.find c decls in
    let symb = Function_slot.Map.find c symbs in
    let code_name = Linkage_name.to_string (Code_id.linkage_name code_id) in
    let acc =
      List.rev
        (C.define_symbol ~global:true (Symbol.linkage_name_as_string symb))
      @ acc
    in
    let arity, closure_code_pointers =
      Env.get_func_decl_params_arity env code_id
    in
    let closure_info = C.closure_info ~arity ~startenv:(startenv - offset) in
    (* We build here the **reverse** list of fields for the closure *)
    match closure_code_pointers with
    | Full_application_only ->
      let acc = C.cint closure_info :: C.symbol_address code_name :: acc in
      env, acc, offset + 2, updates
    | Full_and_partial_application ->
      let acc =
        C.symbol_address code_name :: C.cint closure_info
        :: C.symbol_address (C.curry_function_sym arity)
        :: acc
      in
      env, acc, offset + 3, updates)

let rec fill_static_up_to j acc i =
  if i = j then acc else fill_static_up_to j (C.cint 1n :: acc) (i + 1)

let rec fill_up_to j acc i =
  if i > j then Misc.fatal_errorf "Problem while filling up a closure in to_cmm";
  if i = j then acc else fill_up_to j (C.int 1 :: acc) (i + 1)

let rec fill_layout decls startenv elts env effs acc i = function
  | [] -> List.rev acc, env, effs
  | (j, slot) :: r ->
    let acc = fill_up_to j acc i in
    let acc, offset, env, eff = fill_slot decls startenv elts env acc j slot in
    let effs = Ece.join eff effs in
    fill_layout decls startenv elts env effs acc offset r

let rec fill_static_layout s symbs decls startenv elts env acc updates i =
  function
  | [] -> env, List.rev acc, updates, i
  | (j, slot) :: r ->
    let acc = fill_static_up_to j acc i in
    let env, acc, offset, updates =
      fill_static_slot s symbs decls startenv elts env acc j updates slot
    in
    fill_static_layout s symbs decls startenv elts env acc updates offset r

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
  let clos_symb = ref None in
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.value_slots set in
  let env, l, updates, length =
    fill_static_layout clos_symb symbs decls layout.startenv elts env []
      prev_updates 0 layout.slots
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
        match !clos_symb with
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
  let l, env, effs =
    fill_layout decl_map layout.startenv elts env effs [] 0 layout.slots
  in
  let csoc = make_closure_block closure_alloc_mode l in
  (* Create a variable to hold the set of closure *)
  let soc_var = Variable.create "*set_of_closures*" in
  let env = Env.bind_variable env soc_var effs false csoc in
  (* Get from the env the cmm variable that was created and bound to the
     compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (Env.classify peff = Env.Pure);
  (* Add env bindings for all the value slots. *)
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
  (* The set of closures, as well as the individual closures variables are
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
