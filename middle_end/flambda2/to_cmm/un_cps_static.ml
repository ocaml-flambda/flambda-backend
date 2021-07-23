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

[@@@ocaml.warning "-27"] (* FIXME remove this once closures changes done *)
[@@@ocaml.warning "-32"] (* FIXME remove this once closures changes done *)

open! Flambda.Import

module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

module Env = Un_cps_env
module SC = Flambda.Static_const
module R = Un_cps_result

(* CR mshinwell: Share these next functions with Un_cps.  Unfortunately
   there's a name clash with at least one of them ("symbol") with functions
   already in Un_cps_helper. *)
let symbol s =
  Linkage_name.to_string (Symbol.linkage_name s)

let tag_targetint t = Targetint_32_64.(add (shift_left t 1) one)

let targetint_of_imm i = Targetint_31_63.Imm.to_targetint i.Targetint_31_63.value

let nativeint_of_targetint t =
  match Targetint_32_64.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

let todo () = failwith "Not yet implemented"
(* ----- End of functions to share ----- *)

let name_static env name =
  Name.pattern_match name
    ~var:(fun v -> env, `Var v)
    ~symbol:(fun s ->
      Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s),
      `Data [C.symbol_address (symbol s)])

let const_static _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i ->
      [C.cint (nativeint_of_targetint (targetint_of_imm i))]
  | Tagged_immediate i ->
      [C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))]
  | Naked_float f ->
      [C.cfloat (Numeric_types.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i ->
      [C.cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
      if C.arch32 then todo() (* split int64 on 32-bit archs *)
      else [C.cint (Int64.to_nativeint i)]
  | Naked_nativeint t ->
      [C.cint (nativeint_of_targetint t)]

let simple_static env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static env n)
    ~const:(fun c -> env, `Data (const_static env c))

let static_value env v =
  match (v : SC.Field_of_block.t) with
  | Symbol s ->
      Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s),
      C.symbol_address (symbol s)
  | Dynamically_computed _ -> env, C.cint 1n
  | Tagged_immediate i ->
      env, C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))

let or_variable f default v cont =
  match (v : _ Or_variable.t) with
  | Const c -> f c cont
  | Var _ -> f default cont

let make_update env kind symb var i prev_update =
  let e = Env.get_variable env var in
  let address = C.field_address symb i Debuginfo.none in
  let update = C.store kind Lambda.Root_initialization address e in
  match prev_update with
  | None -> Some update
  | Some prev_update -> Some (C.sequence prev_update update)

let rec static_block_updates symb env acc i = function
  | [] -> acc
  | sv :: r ->
      begin match (sv : SC.Field_of_block.t) with
      | Symbol _
      | Tagged_immediate _ ->
          static_block_updates symb env acc (i + 1) r
      | Dynamically_computed var ->
          let acc = make_update env Cmm.Word_val symb var i acc in
          static_block_updates symb env acc (i + 1) r
      end

let rec static_float_array_updates symb env acc i = function
  | [] -> acc
  | sv :: r ->
      begin match (sv : _ Or_variable.t) with
      | Const _ ->
          static_float_array_updates symb env acc (i + 1) r
      | Var var ->
          let acc = make_update env Cmm.Double symb var i acc in
          static_float_array_updates symb env acc (i + 1) r
      end

let static_boxed_number kind env s default emit transl v r updates =
  let name = symbol s in
  let aux x cont = emit (name, Cmmgen_state.Global) (transl x) cont in
  let updates =
    match (v : _ Or_variable.t) with
    | Const _ -> None
    | Var v ->
        make_update env kind (C.symbol name) v 0 updates
  in
  R.update_data r (or_variable aux default v), updates

let get_whole_closure_symbol =
  let whole_closure_symb_count = ref 0 in
  (fun r ->
     match !r with
     | Some s -> s
     | None ->
         incr whole_closure_symb_count;
         let comp_unit = Compilation_unit.get_current_exn () in
         let linkage_name =
           Linkage_name.create @@
           Printf.sprintf ".clos_%d" !whole_closure_symb_count
         in
         let s = Symbol.create comp_unit linkage_name in
         r := Some s;
         s
  )

let rec static_set_of_closures env symbs set prev_update =
  let clos_symb = ref None in
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts =
    Un_cps_closure.filter_closure_vars set
      ~used_closure_vars:(Env.used_closure_vars env)
  in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let env, l, updates, length =
    fill_static_layout
      clos_symb symbs decls layout.startenv
      elts env [] prev_update 0 layout.slots
  in
  let block =
    match l with
    (* Closures can be deleted by flambda but still appear in let_symbols,
       hence we may end up with empty sets of closures. *)
    | [] -> []
    (* Regular case. *)
    | _ ->
      let header = C.cint (C.black_closure_header length) in
      let sdef = match !clos_symb with
        | None -> []
        | Some s -> C.define_symbol ~global:false (symbol s)
      in
      header :: sdef @ l
  in
  env, block, updates

and fill_static_layout s symbs decls startenv elts env acc updates i = function
  | [] -> env, List.rev acc, updates, i
  | (j, slot) :: r ->
      let acc = fill_static_up_to j acc i in
      let env, acc, offset, updates =
        fill_static_slot s symbs decls startenv elts env acc j updates slot
      in
      fill_static_layout s symbs decls startenv elts env acc updates offset r

and fill_static_slot s symbs decls startenv elts env acc offset updates slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.cint (C.infix_header (offset + 1)) in
      env, field :: acc, offset + 1, updates
  | Env_var v ->
      let env, contents =
        simple_static env (Var_within_closure.Map.find v elts)
      in
      let fields, updates =
        match contents with
        | `Data fields -> fields, updates
        | `Var v ->
            let s = get_whole_closure_symbol s in
            let updates =
              make_update env Cmm.Word_val (C.symbol (symbol s)) v offset updates
            in
            [C.cint 1n], updates
      in
      env, List.rev fields @ acc, offset + 1, updates
  | Closure c ->
      let code_id = Closure_id.Map.find c decls in
      let symb = Closure_id.Map.find c symbs in
      let external_name = symbol symb in
      let code_symbol = Code_id.code_symbol code_id in
      let code_name = Linkage_name.to_string (Symbol.linkage_name code_symbol) in
      let acc = List.rev (C.define_symbol ~global:true external_name) @ acc in
      let arity = Env.get_func_decl_params_arity env code_id in
      let closure_info = C.closure_info ~arity ~startenv:(startenv - offset) in
      (* We build here the **reverse** list of fields for the closure *)
      if arity = 1 || arity = 0 then begin
        let acc =
          C.cint closure_info ::
          C.symbol_address code_name ::
          acc
        in
        env, acc, offset + 2, updates
      end else begin
        let acc =
          C.symbol_address code_name ::
          C.cint closure_info ::
          C.symbol_address (C.curry_function_sym arity) ::
          acc
        in
        env, acc, offset + 3, updates
      end

and fill_static_up_to j acc i =
  if i = j then acc
  else fill_static_up_to j (C.cint 1n :: acc) (i + 1)

let update_env_for_code env (code : Code.t) =
  (* Check scope of the code ID *)
  let code_id = Code.code_id code in
  let env =
    match Code.newer_version_of code with
    | None -> env
    | Some code_id ->
      Env.check_scope ~allow_deleted:true env
        (Code_id_or_symbol.Code_id code_id)
  in
  match Code.params_and_body code with
  | Deleted ->
    Env.mark_code_id_as_deleted env code_id
  | Present _ ->
    (* Function info should already have been computed *)
    env

let add_function env r ~params_and_body code_id p =
  let fun_symbol = Code_id.code_symbol code_id in
  let fun_name =
    Linkage_name.to_string (Symbol.linkage_name fun_symbol)
  in
  let fundecl, r = params_and_body env r fun_name p in
  R.add_function r fundecl

let add_functions env ~params_and_body r (code : Code.t) =
  match Code.params_and_body code with
  | Deleted -> r
  | Present p -> add_function env r ~params_and_body (Code.code_id code) p

let preallocate_set_of_closures (r, updates, env) ~closure_symbols
      set_of_closures =
  let env, data, updates =
    let closure_symbols =
      closure_symbols
      |> Closure_id.Lmap.bindings
      |> Closure_id.Map.of_list
    in
    static_set_of_closures env closure_symbols set_of_closures updates
  in
  let r = R.set_data r data in
  r, updates, env

let static_const0 env r ~updates ~params_and_body
      (bound_symbols : Bound_symbols.Pattern.t)
      (static_const : Static_const.t) =
  match bound_symbols, static_const with
  | Block_like s, Block (tag, _mut, fields) ->
      let name = symbol s in
      let tag = Tag.Scannable.to_int tag in
      let block_name = name, Cmmgen_state.Global in
      let header = C.black_block_header tag (List.length fields) in
      let env, static_fields =
        List.fold_right
          (fun v (env, static_fields) ->
             let env, static_field = static_value env v in
             env, static_field :: static_fields)
          fields (env, [])
      in
      let block = C.emit_block block_name header static_fields in
      let updates = static_block_updates (C.symbol name) env updates 0 fields in
      env, R.set_data r block, updates
  | Code code_id, Code code ->
      if not (Code_id.equal code_id (Code.code_id code)) then begin
        Misc.fatal_errorf "Code ID mismatch:@ %a@ =@ %a"
          Bound_symbols.Pattern.print bound_symbols
          Static_const.print static_const
      end;
      (* Nothing needs doing here as we've already added the code to the
         environment. *)
      env, r, updates
  | Set_of_closures closure_symbols, Set_of_closures set_of_closures ->
      let r, updates, env =
        preallocate_set_of_closures (r, updates, env) ~closure_symbols
          set_of_closures
      in
      env, r, updates
  | Block_like s, Boxed_float v ->
      let default = Numeric_types.Float_by_bit_pattern.zero in
      let transl = Numeric_types.Float_by_bit_pattern.to_float in
      let r, updates =
        static_boxed_number
          Cmm.Double env s default C.emit_float_constant transl v r updates
      in
      env, r, updates
  | Block_like s, Boxed_int32 v ->
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s 0l C.emit_int32_constant Fun.id v r updates
      in
      env, r, updates
  | Block_like s, Boxed_int64 v ->
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s 0L C.emit_int64_constant Fun.id v r updates
      in
      env, r, updates
  | Block_like s, Boxed_nativeint v ->
      let default = Targetint_32_64.zero in
      let transl = nativeint_of_targetint in
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s default C.emit_nativeint_constant transl v r
            updates
      in
      env, r, updates
  | Block_like s,
    (Immutable_float_block fields | Immutable_float_array fields) ->
      let name = symbol s in
      let aux =
        Or_variable.value_map ~default:0.
          ~f:Numeric_types.Float_by_bit_pattern.to_float
      in
      let static_fields = List.map aux fields in
      let float_array =
        C.emit_float_array_constant (name, Cmmgen_state.Global) static_fields
      in
      let e =
        static_float_array_updates (C.symbol name) env updates 0 fields
      in
      env, R.update_data r float_array, e
  | Block_like s, Mutable_string { initial_value = str; }
  | Block_like s, Immutable_string str ->
      let name = symbol s in
      let data = C.emit_string_constant (name, Cmmgen_state.Global) str in
      env, R.update_data r data, updates
  | Block_like _, (Code _ | Set_of_closures _) ->
      Misc.fatal_errorf "[Code] and [Set_of_closures] cannot be bound by \
          [Block_like] bindings:@ %a"
        SC.print static_const
  | (Code _ | Set_of_closures _),
    (Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _
      | Immutable_float_array _ | Mutable_string _ | Immutable_string _) ->
      Misc.fatal_errorf "Block-like constants cannot be bound by \
          [Code] or [Set_of_closures] bindings:@ %a"
        SC.print static_const
  | Code _, Set_of_closures _ ->
    Misc.fatal_errorf "Sets of closures cannot be bound by [Code] bindings:\
        @ %a"
      SC.print static_const
  | Set_of_closures _, Code _ ->
    Misc.fatal_errorf "Pieces of code cannot be bound by [Set_of_closures] \
        bindings:@ %a"
      SC.print static_const

let static_const env r ~updates ~params_and_body bound_symbols static_const =
  let env, r, updates =
    static_const0 env r ~updates ~params_and_body bound_symbols static_const
  in
  env, R.archive_data r, updates

let static_consts0 env r ~params_and_body bound_symbols static_consts =
  (* We cannot both build the environment and compile any functions in
     one traversal, as the bodies may contain direct calls to the code IDs
     being defined. *)
  let static_consts' = Static_const.Group.to_list static_consts in
  let bound_symbols' = Bound_symbols.to_list bound_symbols in
  if not (List.compare_lengths bound_symbols' static_consts' = 0) then begin
    Misc.fatal_errorf "Mismatch between [Bound_symbols] and \
        [Static_const]s:@ %a@ =@ %a"
      Bound_symbols.print bound_symbols
      Static_const.Group.print static_consts
  end;
  let env =
    ListLabels.fold_left static_consts' ~init:env ~f:(fun env static_const ->
      match Static_const.to_code static_const with
      | None -> env
      | Some code -> update_env_for_code env code)
  in
  let r =
    ListLabels.fold_left static_consts' ~init:r ~f:(fun r static_const ->
      match Static_const.to_code static_const with
      | None -> r
      | Some code -> add_functions env ~params_and_body r code)
  in
  ListLabels.fold_left2 bound_symbols' static_consts'
    ~init:(env, r, None)
    ~f:(fun (env, r, updates) bound_symbol_pat const ->
      static_const env r ~updates ~params_and_body
        bound_symbol_pat const)

let static_consts env r ~params_and_body bound_symbols static_consts =
  try
    (* Gc roots: statically allocated blocks themselves do not need to be
       scanned, however if statically allocated blocks contain dynamically
       allocated contents, then that block has to be registered as Gc roots for
       the Gc to correctly patch it if/when it moves some of the dynamically
       allocated blocks. As a safe over-approximation, we thus register as
       gc_roots all symbols who have an associated computation (and thus are not
       fully_static). *)
    let roots =
      if Static_const.Group.is_fully_static static_consts then []
      else Bound_symbols.gc_roots bound_symbols
    in
    let r = R.add_gc_roots r roots in
    static_consts0 env r ~params_and_body bound_symbols static_consts
  with Misc.Fatal_error as e ->
    (* Create a new "let symbol" with a dummy body to better print the bound
        symbols and static consts. *)
    let dummy_body = Expr.create_invalid () in
    let tmp_let_symbol =
      Let.create (Bindable_let_bound.symbols bound_symbols Syntactic)
        (Named.create_static_consts static_consts)
        ~body:dummy_body
        ~free_names_of_body:(Known Name_occurrences.empty)
      |> Expr.create_let
    in
    Format.eprintf
      "\n@[<v 0>%sContext is:%s translating `let symbol' to Cmm:@ %a@."
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Expr.print tmp_let_symbol;
    raise e
