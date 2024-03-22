(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module V = Backend_var
module VP = Backend_var.With_provenance
module Int = Misc.Stdlib.Int

type 'a for_one_or_more_units = {
  fun_offset_table : int Closure_id.Map.t;
  fv_offset_table : int Var_within_closure.Map.t;
  constant_closures : Closure_id.Set.t;
  closures: Closure_id.Set.t;
}

type t = {
  current_unit :
    Set_of_closures_id.t for_one_or_more_units;
  imported_units :
    Simple_value_approx.function_declarations for_one_or_more_units;
  ppf_dump : Format.formatter;
  mutable constants_for_instrumentation :
    Clambda.ustructured_constant Symbol.Map.t;
}

let get_fun_offset t closure_id =
  let fun_offset_table =
    if Closure_id.in_compilation_unit closure_id
         (Compilation_unit.get_current_exn ())
    then
      t.current_unit.fun_offset_table
    else
      t.imported_units.fun_offset_table
  in
  try Closure_id.Map.find closure_id fun_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for closure %a"
      Closure_id.print closure_id

let get_fv_offset t var_within_closure =
  let fv_offset_table =
    if Var_within_closure.in_compilation_unit var_within_closure
         (Compilation_unit.get_current_exn ())
    then t.current_unit.fv_offset_table
    else t.imported_units.fv_offset_table
  in
  try Var_within_closure.Map.find var_within_closure fv_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for variable %a"
      Var_within_closure.print var_within_closure

let is_function_constant t closure_id =
  if Closure_id.Set.mem closure_id t.current_unit.closures then
    Closure_id.Set.mem closure_id t.current_unit.constant_closures
  else if Closure_id.Set.mem closure_id t.imported_units.closures then
    Closure_id.Set.mem closure_id t.imported_units.constant_closures
  else
    Misc.fatal_errorf "Flambda_to_clambda: missing closure %a"
      Closure_id.print closure_id

(* Instrumentation of closure and field accesses to try to catch compiler
   bugs. *)

let check_closure t ulam named : Clambda.ulambda =
  if not !Clflags.clambda_checks then ulam
  else
    let desc =
      Lambda.simple_prim_on_values ~name:"caml_check_value_is_closure"
        ~arity:2 ~alloc:false
    in
    let str = Format.asprintf "%a" Flambda.print_named named in
    let sym = Symbol.for_new_const_in_current_unit () in
    t.constants_for_instrumentation <-
      Symbol.Map.add sym (Clambda.Uconst_string str)
        t.constants_for_instrumentation;
    let sym = Symbol.linkage_name sym |> Linkage_name.to_string in
    Uprim (Pccall desc,
           [ulam; Clambda.Uconst (Uconst_ref (sym, None))],
           Debuginfo.none)

let clambda_arity (func : Flambda.function_declaration) : Clambda.arity =
  let nlocal =
    func.params
    |> List.filter (fun p -> Lambda.is_local_mode (Parameter.alloc_mode p))
    |> List.length
  in
  {
    function_kind = Curried {nlocal} ;
    params_layout = List.map Parameter.kind func.params ;
    return_layout = func.return_layout ;
  }

let check_field t ulam pos named_opt : Clambda.ulambda =
  if not !Clflags.clambda_checks then ulam
  else
    let desc =
      Lambda.simple_prim_on_values ~name:"caml_check_field_access"
        ~arity:3 ~alloc:false
    in
    let str =
      match named_opt with
      | None -> "<none>"
      | Some named -> Format.asprintf "%a" Flambda.print_named named
    in
    let sym = Symbol.for_new_const_in_current_unit () in
    t.constants_for_instrumentation <-
      Symbol.Map.add sym (Clambda.Uconst_string str)
        t.constants_for_instrumentation;
    let sym = Symbol.linkage_name sym in
    Uprim (Pccall desc, [ulam; Clambda.Uconst (Uconst_int pos);
        Clambda.Uconst (Uconst_ref (sym |> Linkage_name.to_string, None))],
      Debuginfo.none)

module Env : sig
  type t

  val empty : t

  val add_subst : t -> Variable.t -> Clambda.ulambda -> Lambda.layout -> t
  val find_subst_exn : t -> Variable.t -> Clambda.ulambda * Lambda.layout

  val add_fresh_ident : t -> Variable.t -> Lambda.layout -> V.t * t
  val ident_for_var_exn : t -> Variable.t -> V.t * Lambda.layout

  val add_fresh_mutable_ident : t -> Mutable_variable.t -> Lambda.layout -> V.t * t
  val ident_for_mutable_var_exn : t -> Mutable_variable.t -> V.t * Lambda.layout

  val add_allocated_const : t -> Symbol.t -> Allocated_const.t -> t
  val allocated_const_for_symbol : t -> Symbol.t -> Allocated_const.t option

  val keep_only_symbols : t -> t
end = struct
  type t =
    { subst : (Clambda.ulambda * Lambda.layout) Variable.Map.t;
      var : (V.t * Lambda.layout) Variable.Map.t;
      mutable_var : (V.t * Lambda.layout) Mutable_variable.Map.t;
      allocated_constant_for_symbol : Allocated_const.t Symbol.Map.t;
    }

  let empty =
    { subst = Variable.Map.empty;
      var = Variable.Map.empty;
      mutable_var = Mutable_variable.Map.empty;
      allocated_constant_for_symbol = Symbol.Map.empty;
    }

  let add_subst t id subst layout =
    { t with subst = Variable.Map.add id (subst, layout) t.subst }

  let find_subst_exn t id = Variable.Map.find id t.subst

  let ident_for_var_exn t id = Variable.Map.find id t.var

  let add_fresh_ident t var layout =
    let id = V.create_local (Variable.name var) in
    id, { t with var = Variable.Map.add var (id, layout) t.var }

  let ident_for_mutable_var_exn t mut_var =
    Mutable_variable.Map.find mut_var t.mutable_var

  let add_fresh_mutable_ident t mut_var layout =
    let id = V.create_local (Mutable_variable.name mut_var) in
    let mutable_var =
      Mutable_variable.Map.add mut_var (id, layout) t.mutable_var
    in
    id, { t with mutable_var; }

  let add_allocated_const t sym cons =
    { t with
      allocated_constant_for_symbol =
        Symbol.Map.add sym cons t.allocated_constant_for_symbol;
    }

  let allocated_const_for_symbol t sym =
    try
      Some (Symbol.Map.find sym t.allocated_constant_for_symbol)
    with Not_found -> None

  let keep_only_symbols t =
    { empty with
      allocated_constant_for_symbol = t.allocated_constant_for_symbol;
    }
end

let subst_var env var : Clambda.ulambda * Lambda.layout =
  try Env.find_subst_exn env var
  with Not_found ->
    try
      let v, layout = Env.ident_for_var_exn env var in
      Uvar v, layout
    with Not_found ->
      Misc.fatal_errorf "Flambda_to_clambda: unbound variable %a@."
        Variable.print var

let subst_vars env vars = List.map (subst_var env) vars

let build_uoffset ulam offset : Clambda.ulambda =
  if offset = 0 then ulam
  else Uoffset (ulam, offset)

let to_clambda_allocated_constant (const : Allocated_const.t)
      : Clambda.ustructured_constant =
  match const with
  | Float f -> Uconst_float f
  | Int32 i -> Uconst_int32 i
  | Int64 i -> Uconst_int64 i
  | Nativeint i -> Uconst_nativeint i
  | Immutable_string s | String s -> Uconst_string s
  | Immutable_float_array a | Float_array a -> Uconst_float_array a

let to_uconst_symbol env symbol : Clambda.ustructured_constant option =
  match Env.allocated_const_for_symbol env symbol with
  | Some ((Float _ | Int32 _ | Int64 _ | Nativeint _) as const) ->
    Some (to_clambda_allocated_constant const)
  | None  (* CR-soon mshinwell: Try to make this an error. *)
  | Some _ -> None

let to_clambda_symbol' env sym : Clambda.uconstant =
  let lbl = Symbol.linkage_name sym |> Linkage_name.to_string in
  Uconst_ref (lbl, to_uconst_symbol env sym)

let to_clambda_symbol env sym : Clambda.ulambda =
  Uconst (to_clambda_symbol' env sym)

let to_clambda_const env (const : Flambda.constant_defining_value_block_field)
      : Clambda.uconstant =
  match const with
  | Symbol symbol -> to_clambda_symbol' env symbol
  | Const (Int i) -> Uconst_int i
  | Const (Char c) -> Uconst_int (Char.code c)

let rec to_clambda t env (flam : Flambda.t) : Clambda.ulambda * Lambda.layout =
  match flam with
  | Var var -> subst_var env var
  | Let { var; defining_expr; body; _ } ->
    let defining_expr, defining_expr_layout = to_clambda_named t env var defining_expr in
    let id, env_body = Env.add_fresh_ident env var defining_expr_layout in
    let body, body_layout = to_clambda t env_body body in
    Ulet (Immutable, defining_expr_layout, VP.create id, defining_expr, body),
    body_layout
  | Let_mutable { var = mut_var; initial_value = var; body; contents_kind } ->
    let id, env_body = Env.add_fresh_mutable_ident env mut_var contents_kind in
    let def, def_layout = subst_var env var in
    assert(Lambda.compatible_layout def_layout contents_kind);
    let body, body_layout = to_clambda t env_body body in
    Ulet (Mutable, contents_kind, VP.create id, def, body), body_layout
  | Apply { func; args; kind = Direct direct_func; probe; dbg; reg_close; mode; result_layout } ->
    (* The closure _parameter_ of the function is added by cmmgen.
       At the call site, for a direct call, the closure argument must be
       explicitly added (by [to_clambda_direct_apply]); there is no special
       handling of such in the direct call primitive.
       For an indirect call, we do not need to do anything here; Cmmgen will
       do the equivalent of the previous paragraph when it generates a direct
       call to [caml_apply]. *)
    to_clambda_direct_apply t func args direct_func probe dbg reg_close mode result_layout env,
    result_layout
  | Apply { func; args; kind = Indirect; probe = None; dbg; reg_close; mode; result_layout } ->
    let callee, callee_layout = subst_var env func in
    assert(Lambda.compatible_layout callee_layout Lambda.layout_function);
    let args, args_layout = List.split (subst_vars env args) in
    Ugeneric_apply (check_closure t callee (Flambda.Expr (Var func)),
      args, args_layout, result_layout, (reg_close, mode), dbg),
    result_layout
  | Apply { probe = Some {name}; _ } ->
    Misc.fatal_errorf "Cannot apply indirect handler for probe %s" name ()
  | Switch (arg, sw) ->
    let aux () : Clambda.ulambda * Lambda.layout =
      let const_index, const_actions =
        to_clambda_switch t env sw.consts sw.numconsts sw.failaction sw.kind
      in
      let block_index, block_actions =
        to_clambda_switch t env sw.blocks sw.numblocks sw.failaction sw.kind
      in
      let arg, arg_layout = subst_var env arg in
      assert(Lambda.compatible_layout arg_layout Lambda.layout_any_value);
      Uswitch (arg,
        { us_index_consts = const_index;
          us_actions_consts = const_actions;
          us_index_blocks = block_index;
          us_actions_blocks = block_actions;
        },
        Debuginfo.none, sw.kind),  (* debug info will be added by GPR#855 *)
      sw.kind
    in
    (* Check that the [failaction] may be duplicated.  If this is not the
       case, share it through a static raise / static catch. *)
    (* CR-someday pchambart for pchambart: This is overly simplified.
       We should verify that this does not generates too bad code.
       If it the case, handle some let cases.
    *)
    begin match sw.failaction with
    | None -> aux ()
    | Some (Static_raise _) -> aux ()
    | Some failaction ->
      let exn = Static_exception.create () in
      let sw =
        { sw with
          failaction = Some (Flambda.Static_raise (exn, []));
        }
      in
      let expr : Flambda.t =
        Static_catch (exn, [], Switch (arg, sw), failaction, sw.kind)
      in
      to_clambda t env expr
    end
  | String_switch (arg, sw, def, kind) ->
    let arg, arg_layout = subst_var env arg in
    assert(Lambda.compatible_layout arg_layout Lambda.layout_string);
    let sw =
      List.map (fun (s, e) ->
          let e, layout = to_clambda t env e in
          assert(Lambda.compatible_layout layout kind);
          s, e
        ) sw
    in
    let def =
      Option.map (fun e ->
          let e, layout = to_clambda t env e in
          assert(Lambda.compatible_layout layout kind);
          e
        ) def
    in
    Ustringswitch (arg, sw, def, kind), kind
  | Static_raise (static_exn, args) ->
    (* CR pchambart: there probably should be an assertion that the
       layouts matches the static_catch ones *)
    let args =
      List.map (fun arg ->
          let arg, _layout = subst_var env arg in
          arg
        ) args
    in
    Ustaticfail (Static_exception.to_int static_exn, args),
    Lambda.layout_bottom
  | Static_catch (static_exn, vars, body, handler, kind) ->
    let env_handler, ids =
      List.fold_right (fun (var, layout) (env, ids) ->
          let id, env = Env.add_fresh_ident env var layout in
          env, (VP.create id, layout) :: ids)
        vars (env, [])
    in
    let body, body_layout = to_clambda t env body in
    let handler, handler_layout = to_clambda t env_handler handler in
    assert(Lambda.compatible_layout body_layout kind);
    assert(Lambda.compatible_layout handler_layout kind);
    Ucatch (Static_exception.to_int static_exn, ids,
      body, handler, kind),
    kind
  | Try_with (body, var, handler, kind) ->
    let id, env_handler = Env.add_fresh_ident env var Lambda.layout_exception in
    let body, body_layout = to_clambda t env body in
    let handler, handler_layout = to_clambda t env_handler handler in
    assert(Lambda.compatible_layout body_layout kind);
    assert(Lambda.compatible_layout handler_layout kind);
    Utrywith (body, VP.create id, handler, kind),
    kind
  | If_then_else (arg, ifso, ifnot, kind) ->
    let arg, arg_layout = subst_var env arg in
    let ifso, ifso_layout = to_clambda t env ifso in
    let ifnot, ifnot_layout = to_clambda t env ifnot in
    assert(Lambda.compatible_layout arg_layout Lambda.layout_any_value);
    assert(Lambda.compatible_layout ifso_layout kind);
    assert(Lambda.compatible_layout ifnot_layout kind);
    Uifthenelse (arg, ifso, ifnot, kind),
    kind
  | While (cond, body) ->
    let cond, cond_layout = to_clambda t env cond in
    let body, body_layout = to_clambda t env body in
    assert(Lambda.compatible_layout cond_layout Lambda.layout_any_value);
    assert(Lambda.compatible_layout body_layout Lambda.layout_unit);
    Uwhile (cond, body),
    Lambda.layout_unit
  | For { bound_var; from_value; to_value; direction; body } ->
    let id, env_body = Env.add_fresh_ident env bound_var Lambda.layout_int in
    let from_value, from_value_layout = subst_var env from_value in
    let to_value, to_value_layout = subst_var env to_value in
    let body, body_layout = to_clambda t env_body body in
    assert(Lambda.compatible_layout from_value_layout Lambda.layout_int);
    assert(Lambda.compatible_layout to_value_layout Lambda.layout_int);
    assert(Lambda.compatible_layout body_layout Lambda.layout_unit);
    Ufor (VP.create id, from_value, to_value, direction, body),
    Lambda.layout_unit
  | Assign { being_assigned; new_value } ->
    let id, id_layout =
      try Env.ident_for_mutable_var_exn env being_assigned
      with Not_found ->
        Misc.fatal_errorf "Unbound mutable variable %a in [Assign]: %a"
          Mutable_variable.print being_assigned
          Flambda.print flam
    in
    let new_value, new_value_layout = subst_var env new_value in
    assert(Lambda.compatible_layout id_layout new_value_layout);
    Uassign (id, new_value),
    Lambda.layout_unit
  | Send { kind; meth; obj; args; dbg; reg_close; mode; result_layout } ->
    let args, args_layout = List.split (subst_vars env args) in
    let meth, _meth_layout = subst_var env meth in
    let obj, _obj_layout = subst_var env obj in
    Usend (kind, meth, obj,
      args, args_layout, result_layout, (reg_close,mode), dbg),
    result_layout
  | Region body ->
      let body, body_layout = to_clambda t env body in
      let is_trivial =
        match body with
        | Uvar _ | Uconst _ -> true
        | _ -> false
      in
      if is_trivial then body, body_layout
      else Uregion body, body_layout
  | Exclave body ->
      let body, body_layout = to_clambda t env body in
      let is_trivial =
        match body with
        | Uvar _ | Uconst _ -> true
        | _ -> false
      in
      if is_trivial then body, body_layout
      else Uexclave body, body_layout
  | Proved_unreachable -> Uunreachable, Lambda.layout_bottom

and to_clambda_named t env var (named : Flambda.named) : Clambda.ulambda * Lambda.layout =
  match named with
  | Symbol sym -> to_clambda_symbol env sym, Lambda.layout_any_value
  | Const (Int n) -> Uconst (Uconst_int n), Lambda.layout_int
  | Const (Char c) -> Uconst (Uconst_int (Char.code c)), Lambda.layout_int
  | Allocated_const _ ->
    Misc.fatal_errorf "[Allocated_const] should have been lifted to a \
        [Let_symbol] construction before [Flambda_to_clambda]: %a = %a"
      Variable.print var
      Flambda.print_named named
  | Read_mutable mut_var ->
    begin try
      let mut_var, layout = Env.ident_for_mutable_var_exn env mut_var in
      Uvar mut_var, layout
    with Not_found ->
      Misc.fatal_errorf "Unbound mutable variable %a in [Read_mutable]: %a"
        Mutable_variable.print mut_var
        Flambda.print_named named
    end
  | Read_symbol_field (symbol, field) ->
    Uprim (Pfield (field, Pvalue Pgenval, Pointer, Mutable),
           [to_clambda_symbol env symbol], Debuginfo.none),
    Lambda.layout_any_value
  | Set_of_closures set_of_closures ->
    to_clambda_set_of_closures t env set_of_closures,
    Lambda.layout_any_value
  | Project_closure { set_of_closures; closure_id } ->
    (* Note that we must use [build_uoffset] to ensure that we do not generate
       a [Uoffset] construction in the event that the offset is zero, otherwise
       we might break pattern matches in Cmmgen (in particular for the
       compilation of "let rec"). *)
    let set_of_closures_expr, _layout_set_of_closures =
      subst_var env set_of_closures
    in
    check_closure t (
      build_uoffset
        (check_closure t set_of_closures_expr
           (Flambda.Expr (Var set_of_closures)))
        (get_fun_offset t closure_id))
      named,
    Lambda.layout_function
  | Move_within_set_of_closures { closure; start_from; move_to } ->
    let closure_expr, _layout_closure = subst_var env closure in
    check_closure t (build_uoffset
      (check_closure t closure_expr
         (Flambda.Expr (Var closure)))
      ((get_fun_offset t move_to) - (get_fun_offset t start_from)))
      named,
    Lambda.layout_function
  | Project_var { closure; var; closure_id; kind } ->
    let ulam, _closure_layout = subst_var env closure in
    let fun_offset = get_fun_offset t closure_id in
    let var_offset = get_fv_offset t var in
    let pos = var_offset - fun_offset in
    Uprim (Pfield (pos, kind, Pointer, Mutable),
      [check_field t (check_closure t ulam (Expr (Var closure)))
         pos (Some named)],
      Debuginfo.none),
    kind
  | Prim (Pfield (index, layout, ptr, mut), [block], dbg) ->
    begin match layout with
      | Pvalue _ -> ()
      | _ ->
        Misc.fatal_errorf "Pfield can only be of layout value %a"
          Flambda.print_named named
    end;
    let block, _block_layout = subst_var env block in
    Uprim (Pfield (index, layout, ptr, mut),
      [check_field t block index None], dbg),
    Lambda.layout_value_field
  | Prim (Psetfield (index, maybe_ptr, init), [block; new_value], dbg) ->
    let block, _block_layout = subst_var env block in
    let new_value, _new_value_layout = subst_var env new_value in
    Uprim (Psetfield (index, maybe_ptr, init), [
        check_field t block index None;
        new_value;
      ], dbg),
    Lambda.layout_unit
  | Prim (Popaque, args, dbg) ->
    let arg = match args with
      | [arg] -> arg
      | [] | _ :: _ :: _ -> assert false
    in
    let arg, arg_layout = subst_var env arg in
    Uprim (Popaque, [arg], dbg),
    arg_layout
  | Prim (p, args, dbg) ->
    let args, _args_layout = List.split (subst_vars env args) in
    let result_layout = Clambda_primitives.result_layout p in
    Uprim (p, args, dbg),
    result_layout
  | Expr expr -> to_clambda t env expr

and to_clambda_switch t env cases num_keys default kind =
  let num_keys =
    if Numbers.Int.Set.cardinal num_keys = 0 then 0
    else Numbers.Int.Set.max_elt num_keys + 1
  in
  let store = Flambda_utils.Switch_storer.mk_store () in
  let default_action =
    match default with
    | Some def when List.length cases < num_keys ->
      store.act_store () def
    | _ -> -1
  in
  let index = Array.make num_keys default_action in
  let smallest_key = ref num_keys in
  List.iter
    (fun (key, lam) ->
      index.(key) <- store.act_store () lam;
      smallest_key := Int.min key !smallest_key
    )
    cases;
  if !smallest_key < num_keys then begin
    let action = ref index.(!smallest_key) in
    Array.iteri
      (fun i act ->
         if act >= 0 then action := act else index.(i) <- !action)
      index
  end;
  let actions =
    Array.map (fun action ->
        let action, action_layout = to_clambda t env action in
        assert(Lambda.compatible_layout action_layout kind);
        action
      ) (store.act_get ())
  in
  match actions with
  | [| |] -> [| |], [| |]  (* May happen when [default] is [None]. *)
  | _ -> index, actions

and to_clambda_direct_apply t func args direct_func probe dbg pos mode result_layout env
  : Clambda.ulambda =
  let closed = is_function_constant t direct_func in
  let label =
    Symbol_utils.Flambda.for_code_of_closure direct_func
    |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  let uargs =
    let uargs, _uargs_layout = List.split (subst_vars env args) in
    (* Remove the closure argument if the closure is closed.  (Note that the
       closure argument is always a variable, so we can be sure we are not
       dropping any side effects.) *)
    if closed then uargs else
      let func, func_layout = subst_var env func in
      assert(Lambda.compatible_layout func_layout Lambda.layout_function);
      uargs @ [func]
  in
  Udirect_apply (label, uargs, probe, result_layout, (pos, mode), dbg)

(* Describe how to build a runtime closure block that corresponds to the
   given Flambda set of closures.

   For instance the closure for the following set of closures:

     let rec fun_a x =
       if x <= 0 then 0 else fun_b (x-1) v1
     and fun_b x y =
       if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

   will be represented in memory as:

     [ closure header; fun_a;
       1; infix header; fun caml_curry_2;
       2; fun_b; v1; v2 ]

   fun_a and fun_b will take an additional parameter 'env' to
   access their closure.  It will be arranged such that in the body
   of each function the env parameter points to its own code
   pointer.  For example, in fun_b it will be shifted by 3 words.

   Hence accessing v1 in the body of fun_a is accessing the
   6th field of 'env' and in the body of fun_b the 1st field.
*)
and to_clambda_set_of_closures t env
      (({ function_decls; free_vars } : Flambda.set_of_closures)
        as set_of_closures) : Clambda.ulambda =
  let all_functions = Variable.Map.bindings function_decls.funs in
  let env_var = V.create_local "env" in
  let to_clambda_function
        (closure_id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    let closure_id = Closure_id.wrap closure_id in
    let fun_offset =
      Closure_id.Map.find closure_id t.current_unit.fun_offset_table
    in
    let env =
      (* Inside the body of the function, we cannot access variables
         declared outside, so start with a suitably clean environment.
         Note that we must not forget the information about which allocated
         constants contain which unboxed values. *)
      let env = Env.keep_only_symbols env in
      (* Add the Clambda expressions for the free variables of the function
         to the environment. *)
      let add_env_free_variable id (spec_to : Flambda.specialised_to) env =
        let var_offset =
          try
            Var_within_closure.Map.find
              (Var_within_closure.wrap id) t.current_unit.fv_offset_table
          with Not_found ->
            Misc.fatal_errorf "Clambda.to_clambda_set_of_closures: offset for \
                free variable %a is unknown.  Set of closures: %a"
              Variable.print id
              Flambda.print_set_of_closures set_of_closures
        in
        let pos = var_offset - fun_offset in
        Env.add_subst env id
          (Uprim (Pfield (pos, spec_to.kind, Pointer, Mutable),
                  [Clambda.Uvar env_var], Debuginfo.none))
          spec_to.kind
      in
      let env = Variable.Map.fold add_env_free_variable free_vars env in
      (* Add the Clambda expressions for all functions defined in the current
         set of closures to the environment.  The various functions may be
         retrieved by moving within the runtime closure, starting from the
         current function's closure. *)
      let add_env_function pos env (id, _) =
        let offset =
          Closure_id.Map.find (Closure_id.wrap id)
            t.current_unit.fun_offset_table
        in
        let exp : Clambda.ulambda = Uoffset (Uvar env_var, offset - pos) in
        Env.add_subst env id exp Lambda.layout_function
      in
      List.fold_left (add_env_function fun_offset) env all_functions
    in
    let env_body, params =
      List.fold_right (fun param (env, params) ->
          let id, env =
            Env.add_fresh_ident env
              (Parameter.var param) (Parameter.kind param)
          in
          env, VP.create id :: params)
        function_decl.params (env, [])
    in
    let label =
      Symbol_utils.Flambda.for_code_of_closure closure_id
      |> Symbol.linkage_name
      |> Linkage_name.to_string
    in
    let body, _body_layout = to_clambda t env_body function_decl.body in
    { label;
      arity = clambda_arity function_decl;
      params = params @ [VP.create env_var];
      body;
      dbg = function_decl.dbg;
      env = Some env_var;
      mode = set_of_closures.alloc_mode;
      poll = function_decl.poll;
    }
  in
  let functions = List.map to_clambda_function all_functions in
  let not_scanned_fv, scanned_fv =
    Variable.Map.partition (fun _ (free_var : Flambda.specialised_to) ->
        match free_var.kind with
        | Ptop -> Misc.fatal_error "[Ptop] can't be stored in a closure."
        | Pbottom ->
          Misc.fatal_error
            "[Pbottom] should have been eliminated as dead code \
             and not stored in a closure."
        | Punboxed_float _ -> true
        | Punboxed_int _ -> true
        | Punboxed_vector _ -> true
        | Pvalue Pintval -> true
        | Pvalue _ -> false
        | Punboxed_product _ -> Misc.fatal_error "TODO")
      free_vars
  in
  let to_closure_args free_vars =
    List.map snd (
      Variable.Map.bindings (Variable.Map.map (
          fun (free_var : Flambda.specialised_to) ->
            let var, var_layout = subst_var env free_var.var in
            assert(Lambda.compatible_layout var_layout free_var.kind);
            var
        ) free_vars))
  in
  Uclosure {
    functions ;
    not_scanned_slots = to_closure_args not_scanned_fv ;
    scanned_slots = to_closure_args scanned_fv
  }

and to_clambda_closed_set_of_closures t env symbol
      ({ function_decls; } : Flambda.set_of_closures)
      : Clambda.ustructured_constant =
  let functions = Variable.Map.bindings function_decls.funs in
  let to_clambda_function (id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    (* All that we need in the environment, for translating one closure from
       a closed set of closures, is the substitutions for variables bound to
       the various closures in the set.  Such closures will always be
       referenced via symbols. *)
    let env =
      List.fold_left (fun env (var, _) ->
          let closure_id = Closure_id.wrap var in
          let symbol = Symbol_utils.Flambda.for_closure closure_id in
          Env.add_subst env var (to_clambda_symbol env symbol)
            Lambda.layout_function)
        (Env.keep_only_symbols env)
        functions
    in
    let env_body, params =
      List.fold_right (fun param (env, params) ->
          let id, env =
            Env.add_fresh_ident env
              (Parameter.var param) (Parameter.kind param)
          in
          env, VP.create id :: params)
        function_decl.params (env, [])
    in
    let body =
      let body, body_layout = to_clambda t env_body function_decl.body in
      if not (Lambda.compatible_layout body_layout function_decl.return_layout) then
        Misc.fatal_errorf "Incompatible layouts:@.body: %a@.function: %a@.%a@."
          Printlambda.layout body_layout
          Printlambda.layout function_decl.return_layout
          Printclambda.clambda body;
      Un_anf.apply ~ppf_dump:t.ppf_dump ~what:symbol body
    in
    let label =
      Symbol_utils.Flambda.for_code_of_closure (Closure_id.wrap id)
      |> Symbol.linkage_name
      |> Linkage_name.to_string
    in
    { label;
      arity = clambda_arity function_decl;
      params;
      body;
      dbg = function_decl.dbg;
      env = None;
      mode = Lambda.alloc_heap;
      poll = function_decl.poll;
    }
  in
  let ufunct = List.map to_clambda_function functions in
  let closure_lbl = Symbol.linkage_name symbol |> Linkage_name.to_string in
  Uconst_closure (ufunct, closure_lbl, [])

let to_clambda_initialize_symbol t env symbol fields : Clambda.ulambda =
  let fields =
    List.map (fun (index, expr) ->
        let expr, expr_layout = to_clambda t env expr in
        assert(Lambda.compatible_layout expr_layout Lambda.layout_any_value);
        index, expr
      ) fields
  in
  let build_setfield (index, field) : Clambda.ulambda =
    (* Note that this will never cause a write barrier hit, owing to
       the [Initialization]. *)
    Uprim (Psetfield (index, Pointer, Root_initialization),
      [to_clambda_symbol env symbol; field],
      Debuginfo.none)
  in
  match fields with
  | [] -> Uconst (Uconst_int 0)
  | h :: t ->
    List.fold_left (fun acc (p, field) ->
        Clambda.Usequence (build_setfield (p, field), acc))
      (build_setfield h) t

let accumulate_structured_constants t env symbol
      (c : Flambda.constant_defining_value) acc =
  match c with
  | Allocated_const c ->
    Symbol.Map.add symbol (to_clambda_allocated_constant c) acc
  | Block (tag, fields) ->
    let fields = List.map (to_clambda_const env) fields in
    Symbol.Map.add symbol (Clambda.Uconst_block (Tag.to_int tag, fields)) acc
  | Set_of_closures set_of_closures ->
    let to_clambda_set_of_closures =
      to_clambda_closed_set_of_closures t env symbol set_of_closures
    in
    Symbol.Map.add symbol to_clambda_set_of_closures acc
  | Project_closure _ -> acc

let to_clambda_program t env constants (program : Flambda.program) =
  let rec loop env constants (program : Flambda.program_body)
        : Clambda.ulambda *
          Clambda.ustructured_constant Symbol.Map.t *
          Clambda.preallocated_block list =
    match program with
    | Let_symbol (symbol, alloc, program) ->
      (* Useful only for unboxing. Since floats and boxed integers will
         never be part of a Let_rec_symbol, handling only the Let_symbol
         is sufficient. *)
      let env =
        match alloc with
        | Allocated_const const -> Env.add_allocated_const env symbol const
        | _ -> env
      in
      let constants =
        accumulate_structured_constants t env symbol alloc constants
      in
      loop env constants program
    | Let_rec_symbol (defs, program) ->
      let constants =
        List.fold_left (fun constants (symbol, alloc) ->
            accumulate_structured_constants t env symbol alloc constants)
          constants defs
      in
      loop env constants program
    | Initialize_symbol (symbol, tag, fields, program) ->
      let fields =
        List.mapi (fun i field ->
            i, field,
            Initialize_symbol_to_let_symbol.constant_field field)
          fields
      in
      let init_fields =
        List.filter_map (function
            | (i, field, None) -> Some (i, field)
            | (_, _, Some _) -> None)
          fields
      in
      let constant_fields =
        List.map (fun (_, _, constant_field) ->
            match constant_field with
            | None -> None
            | Some (Flambda.Const const) ->
                let n =
                  match const with
                  | Int i -> i
                  | Char c -> Char.code c
                in
                Some (Clambda.Uconst_field_int n)
            | Some (Flambda.Symbol sym) ->
                let lbl = Symbol.linkage_name sym |> Linkage_name.to_string in
                Some (Clambda.Uconst_field_ref lbl))
          fields
      in
      let e1 = to_clambda_initialize_symbol t env symbol init_fields in
      let preallocated_block : Clambda.preallocated_block =
        { symbol = Symbol.linkage_name symbol |> Linkage_name.to_string;
          exported = true;
          tag = Tag.to_int tag;
          fields = constant_fields;
          provenance = None;
        }
      in
      let e2, constants, preallocated_blocks = loop env constants program in
      Usequence (e1, e2), constants, preallocated_block :: preallocated_blocks
    | Effect (expr, program) ->
      let e1, _e1_layout = to_clambda t env expr in
      let e2, constants, preallocated_blocks = loop env constants program in
      Usequence (e1, e2), constants, preallocated_blocks
    | End _ ->
      Uconst (Uconst_int 0), constants, []
  in
  loop env constants program.program_body

type result = {
  expr : Clambda.ulambda;
  preallocated_blocks : Clambda.preallocated_block list;
  structured_constants : Clambda.ustructured_constant Symbol.Map.t;
  exported : Export_info.t;
}

let convert ~ppf_dump (program, exported_transient) : result =
  let current_unit =
    let closures =
      Closure_id.Map.keys (Flambda_utils.make_closure_map program)
    in
    let constant_closures =
      Flambda_utils.all_lifted_constant_closures program
    in
    let offsets = Closure_offsets.compute program in
    { fun_offset_table = offsets.function_offsets;
      fv_offset_table = offsets.free_variable_offsets;
      constant_closures;
      closures;
    }
  in
  let imported_units =
    let imported = Compilenv.approx_env () in
    let closures =
      Set_of_closures_id.Map.fold
        (fun (_ : Set_of_closures_id.t) fun_decls acc ->
           Variable.Map.fold
             (fun var (_ : Simple_value_approx.function_declaration) acc ->
               let closure_id = Closure_id.wrap var in
               Closure_id.Set.add closure_id acc)
             fun_decls.Simple_value_approx.funs
             acc)
        imported.sets_of_closures
        Closure_id.Set.empty
    in
    { fun_offset_table = imported.offset_fun;
      fv_offset_table = imported.offset_fv;
      constant_closures = imported.constant_closures;
      closures;
    }
  in
  let t =
    { current_unit;
      imported_units;
      constants_for_instrumentation = Symbol.Map.empty;
      ppf_dump;
    }
  in
  let expr, structured_constants, preallocated_blocks =
    to_clambda_program t Env.empty Symbol.Map.empty program
  in
  let structured_constants =
    Symbol.Map.disjoint_union structured_constants
      t.constants_for_instrumentation
  in
  let exported =
    Export_info.t_of_transient exported_transient
      ~program
      ~local_offset_fun:current_unit.fun_offset_table
      ~local_offset_fv:current_unit.fv_offset_table
      ~imported_offset_fun:imported_units.fun_offset_table
      ~imported_offset_fv:imported_units.fv_offset_table
      ~constant_closures:current_unit.constant_closures
  in
  { expr; preallocated_blocks; structured_constants; exported; }
