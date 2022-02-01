(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42-66"]

open! Int_replace_polymorphic_compare
open! Flambda
module IR = Closure_conversion_aux.IR
module Acc = Closure_conversion_aux.Acc
module Env = Closure_conversion_aux.Env
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Apply_cont_with_acc = Closure_conversion_aux.Apply_cont_with_acc
module Let_cont_with_acc = Closure_conversion_aux.Let_cont_with_acc
module Let_with_acc = Closure_conversion_aux.Let_with_acc
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module K = Flambda_kind
module LC = Lambda_conversions
module P = Flambda_primitive
module VB = Bound_var

(* Do not use [Simple.symbol], use this function instead, to ensure that we
   correctly compute the free names of [Code]. *)
let use_of_symbol_as_simple acc symbol = acc, Simple.symbol symbol

let symbol_for_ident acc env id =
  let symbol = Env.symbol_for_global env id in
  use_of_symbol_as_simple acc symbol

let register_const0 acc constant name =
  match Static_const.Map.find constant (Acc.shareable_constants acc) with
  | exception Not_found ->
    (* Create a variable to ensure uniqueness of the symbol. *)
    let var = Variable.create name in
    let symbol =
      Symbol.create
        (Compilation_unit.get_current_exn ())
        (Linkage_name.create (Variable.unique_name (Variable.rename var)))
    in
    let acc = Acc.add_declared_symbol ~symbol ~constant acc in
    let acc =
      if Static_const.can_share constant
      then Acc.add_shareable_constant ~symbol ~constant acc
      else acc
    in
    acc, symbol
  | symbol -> acc, symbol

let register_const acc constant name : Acc.t * Field_of_static_block.t * string
    =
  let acc, symbol = register_const0 acc constant name in
  acc, Symbol symbol, name

let register_const_string acc str =
  register_const0 acc (Static_const.Immutable_string str) "string"

let rec declare_const acc (const : Lambda.structured_constant) :
    Acc.t * Field_of_static_block.t * string =
  match const with
  | Const_base (Const_int c) ->
    ( acc,
      Tagged_immediate (Targetint_31_63.int (Targetint_31_63.Imm.of_int c)),
      "int" )
  | Const_base (Const_char c) ->
    acc, Tagged_immediate (Targetint_31_63.char c), "char"
  | Const_base (Const_string (s, _, _)) ->
    let const, name =
      if Flambda_features.safe_string ()
      then Static_const.Immutable_string s, "immstring"
      else Static_const.Mutable_string { initial_value = s }, "string"
    in
    register_const acc const name
  | Const_base (Const_float c) ->
    let c = Numeric_types.Float_by_bit_pattern.create (float_of_string c) in
    register_const acc (Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const acc (Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const acc (Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint_32_64.of_int64 (Int64.of_nativeint c) in
    register_const acc (Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c -> register_const acc (Immutable_string c) "immstring"
  | Const_float_block c ->
    register_const acc
      (Immutable_float_block
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_block"
  | Const_float_array c ->
    register_const acc
      (Immutable_float_array
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_array"
  | Const_block (tag, consts) ->
    let acc, field_of_blocks =
      List.fold_left_map
        (fun acc c ->
          let acc, f, _ = declare_const acc c in
          acc, f)
        acc consts
    in
    let const : Static_const.t =
      Block (Tag.Scannable.create_exn tag, Immutable, field_of_blocks)
    in
    register_const acc const "const_block"

let close_const0 acc (const : Lambda.structured_constant) =
  let acc, const, name = declare_const acc const in
  match const with
  | Tagged_immediate c ->
    acc, Simple.const (Reg_width_const.tagged_immediate c), name
  | Symbol s ->
    let acc, simple = use_of_symbol_as_simple acc s in
    acc, simple, name
  | Dynamically_computed _ ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_const acc const =
  let acc, simple, name = close_const0 acc const in
  let named = Named.create_simple simple in
  acc, named, name

let find_simple_from_id env id =
  match Env.find_simple_to_substitute_exn env id with
  | simple -> simple
  | exception Not_found -> (
    match Env.find_var_exn env id with
    | exception Not_found ->
      Misc.fatal_errorf
        "find_simple_from_id: Cannot find [Ident] %a in environment" Ident.print
        id
    | var -> Simple.var var)

(* CR mshinwell: Avoid the double lookup *)
let find_simple acc env (simple : IR.simple) =
  match simple with
  | Const const ->
    let acc, simple, _ = close_const0 acc const in
    acc, simple
  | Var id -> acc, find_simple_from_id env id

let find_simples acc env ids =
  List.fold_left_map (fun acc id -> find_simple acc env id) acc ids

module Inlining = struct
  type inlinable_result =
    | Not_inlinable
    | Inlinable of Code.t

  let threshold () =
    let inline_threshold =
      Clflags.Float_arg_helper.get ~key:0 !Clflags.inline_threshold
    in
    let magic_scale_constant = 8. in
    int_of_float (inline_threshold *. magic_scale_constant)

  (* CR keryan: we need to emit warnings *)
  let inlinable env apply =
    let callee = Apply_expr.callee apply in
    let dbg = Apply_expr.dbg apply in
    match Env.find_value_approximation env callee with
    | Value_unknown ->
      Inlining_report.(record_decision ~dbg (At_call_site Unknown_function));
      Not_inlinable
    | Block_approximation _ -> assert false
    | Closure_approximation (code_id, None) ->
      Inlining_report.record_decision ~dbg
        (At_call_site
           (Inlining_report.Known_function
              { code_id = Code_id.export code_id;
                decision = Definition_says_not_to_inline
              }));
      Not_inlinable
    | Closure_approximation (code_id, Some code) ->
      let fun_params_length =
        Code.params_arity code |> Flambda_arity.With_subkinds.to_arity
        |> Flambda_arity.length
      in
      if fun_params_length > List.length (Apply_expr.args apply)
      then begin
        Inlining_report.record_decision ~dbg
          (At_call_site
             (Inlining_report.Known_function
                { code_id = Code_id.export code_id;
                  decision = Definition_says_not_to_inline
                }));
        Not_inlinable
      end
      else
        let inlined_call = Apply_expr.inlined apply in
        let decision, res =
          match inlined_call with
          | Never_inlined ->
            ( Call_site_inlining_decision_type.Never_inlined_attribute,
              Not_inlinable )
          | Always_inlined | Hint_inlined ->
            Call_site_inlining_decision_type.Attribute_always, Inlinable code
          | Default_inlined ->
            ( Call_site_inlining_decision_type.Definition_says_inline,
              Inlinable code )
          | Unroll _ -> assert false
        in
        Inlining_report.record_decision ~dbg
          (At_call_site
             (Inlining_report.Known_function
                { code_id = Code_id.export code_id; decision }));
        res

  let make_inlined_body acc ~callee ~params ~args ~my_closure ~my_depth ~body
      ~free_names_of_body ~exn_continuation ~return_continuation
      ~apply_exn_continuation ~apply_return_continuation ~apply_depth =
    let params = List.map Bound_parameter.var params in
    let rec_info =
      match apply_depth with
      | None -> Rec_info_expr.initial
      | Some depth -> Rec_info_expr.var depth
    in
    let bind_params ~params ~args ~body:(acc, body) =
      let acc = Acc.with_free_names free_names_of_body acc in
      List.fold_left2
        (fun (acc, body) param arg ->
          Let_with_acc.create acc
            (Bound_pattern.singleton (VB.create param Name_mode.normal))
            (Named.create_simple arg) ~body
          |> Expr_with_acc.create_let)
        (acc, body) params args
    in
    let bind_depth ~my_depth ~rec_info ~body:(acc, body) =
      Let_with_acc.create acc
        (Bound_pattern.singleton (VB.create my_depth Name_mode.normal))
        (Named.create_rec_info rec_info)
        ~body
      |> Expr_with_acc.create_let
    in
    let apply_renaming (acc, body) perm =
      let acc =
        Acc.with_free_names
          (Name_occurrences.apply_renaming (Acc.free_names acc) perm)
          acc
      in
      acc, Expr.apply_renaming body perm
    in
    Inlining_helpers.make_inlined_body ~callee ~params ~args ~my_closure
      ~my_depth ~rec_info ~body:(acc, body) ~exn_continuation
      ~return_continuation ~apply_exn_continuation ~apply_return_continuation
      ~bind_params ~bind_depth ~apply_renaming

  let wrap_inlined_body_for_exn_support acc ~extra_args ~apply_exn_continuation
      ~apply_return_continuation ~result_arity ~make_inlined_body =
    let apply_cont_create acc ~trap_action cont ~args ~dbg =
      let acc, apply_cont =
        Apply_cont_with_acc.create acc ~trap_action cont ~args ~dbg
      in
      Expr_with_acc.create_apply_cont acc apply_cont
    in
    let let_cont_create acc cont ~handler_params ~handler ~body ~is_exn_handler
        =
      Let_cont_with_acc.build_non_recursive acc cont ~handler_params ~handler
        ~body ~is_exn_handler
    in
    Inlining_helpers.wrap_inlined_body_for_exn_support acc ~extra_args
      ~apply_exn_continuation ~apply_return_continuation ~result_arity
      ~make_inlined_body ~apply_cont_create ~let_cont_create

  let inline acc ~apply ~apply_depth ~func_desc:code =
    let callee = Apply.callee apply in
    let args = Apply.args apply in
    let apply_return_continuation = Apply.continuation apply in
    let apply_exn_continuation = Apply.exn_continuation apply in
    let params_and_body = Code.params_and_body code in
    Function_params_and_body.pattern_match params_and_body
      ~f:(fun
           ~return_continuation
           ~exn_continuation
           params
           ~body
           ~my_closure
           ~is_my_closure_used:_
           ~my_depth
           ~free_names_of_body
         ->
        let args, remain_args =
          let rec split l1 l2 =
            match l1, l2 with
            | _, [] -> [], l1
            | [], _ -> assert false
            | e1 :: l1, _ :: l2 ->
              let args, remains = split l1 l2 in
              e1 :: args, remains
          in
          split args params
        in
        let free_names_of_body =
          match free_names_of_body with
          | Unknown ->
            Misc.fatal_error
              "Params_and_body needs free_names_of_body in [Closure_conversion]"
          | Known free_names -> free_names
        in
        let make_inlined_body =
          make_inlined_body ~callee ~params ~args ~my_closure ~my_depth ~body
            ~free_names_of_body ~exn_continuation ~return_continuation
            ~apply_depth
        in
        let acc = Acc.with_free_names Name_occurrences.empty acc in
        let body apply_return_continuation acc =
          match Exn_continuation.extra_args apply_exn_continuation with
          | [] ->
            make_inlined_body acc
              ~apply_exn_continuation:
                (Exn_continuation.exn_handler apply_exn_continuation)
              ~apply_return_continuation
          | extra_args ->
            wrap_inlined_body_for_exn_support acc ~extra_args
              ~apply_exn_continuation ~apply_return_continuation
              ~result_arity:(Code.result_arity code) ~make_inlined_body
        in
        match remain_args with
        | [] -> body apply_return_continuation acc
        | args ->
          let wrapper_cont = Continuation.create () in
          let continuation = Apply.Result_continuation.Return wrapper_cont in
          let returned_func = Variable.create "func" in
          let call_kind = Call_kind.indirect_function_call_unknown_arity () in
          let handler acc =
            let over_apply =
              Apply.create ~callee:(Simple.var returned_func)
                ~continuation:apply_return_continuation apply_exn_continuation
                ~args ~call_kind (Apply.dbg apply)
                ~inlined:(Apply.inlined apply)
                ~inlining_state:(Inlining_state.default ~round:0)
                ~probe_name:(Apply.probe_name apply)
            in
            Expr_with_acc.create_apply acc over_apply
          in
          let body = body continuation in
          Let_cont_with_acc.build_non_recursive acc wrapper_cont
            ~handler_params:
              [Bound_parameter.create returned_func K.With_subkind.any_value]
            ~handler ~body ~is_exn_handler:false)
end

let close_c_call acc ~let_bound_var
    ({ prim_name;
       prim_arity;
       prim_alloc;
       prim_c_builtin;
       prim_effects = _;
       prim_coeffects = _;
       prim_native_name;
       prim_native_repr_args;
       prim_native_repr_res
     } :
      Primitive.description) ~(args : Simple.t list) exn_continuation dbg
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  (* We always replace the original let-binding with an Flambda expression, so
     we call [k] with [None], to get just the closure-converted body of that
     binding. *)
  let cost_metrics_of_body, free_names_of_body, acc, body =
    Acc.measure_cost_metrics acc ~f:(fun acc -> k acc None)
  in
  let box_return_value =
    match prim_native_repr_res with
    | _, Same_as_ocaml_repr -> None
    | _, Unboxed_float -> Some (P.Box_number Naked_float)
    | _, Unboxed_integer Pnativeint -> Some (P.Box_number Naked_nativeint)
    | _, Unboxed_integer Pint32 -> Some (P.Box_number Naked_int32)
    | _, Unboxed_integer Pint64 -> Some (P.Box_number Naked_int64)
    | _, Untagged_int -> Some (P.Box_number Untagged_immediate)
  in
  let return_continuation, needs_wrapper =
    match Expr.descr body with
    | Apply_cont apply_cont
      when Simple.List.equal
             (Apply_cont_expr.args apply_cont)
             [Simple.var let_bound_var]
           && Option.is_none (Apply_cont_expr.trap_action apply_cont)
           && Option.is_none box_return_value ->
      Apply_cont_expr.continuation apply_cont, false
    | _ -> Continuation.create (), true
  in
  let param_arity =
    List.map LC.kind_of_primitive_native_repr prim_native_repr_args
  in
  let return_kind = LC.kind_of_primitive_native_repr prim_native_repr_res in
  let return_arity = [return_kind] in
  let call_kind =
    Call_kind.c_call ~alloc:prim_alloc ~param_arity ~return_arity
      ~is_c_builtin:prim_c_builtin
  in
  let call_symbol =
    let prim_name =
      if String.equal prim_native_name "" then prim_name else prim_native_name
    in
    (* CR mshinwell: fix "extern" mess (see To_cmm) *)
    Symbol.create
      (Compilation_unit.external_symbols ())
      (Linkage_name.create prim_name)
  in
  let call args acc =
    (* Some C primitives have implementations within Flambda itself. *)
    match prim_native_name with
    | "caml_int64_float_of_bits_unboxed"
    (* There is only one case where this operation is not the identity: on
       32-bit pre-EABI ARM platforms. It is very unlikely anyone would still be
       using one of those, but just in case, we only optimise this primitive on
       64-bit systems. (There is no easy way here of detecting just the specific
       ARM case in question.) *)
      when match Targetint_32_64.num_bits with
           | Thirty_two -> false
           | Sixty_four -> true -> (
      if prim_arity <> 1
      then Misc.fatal_errorf "Expected arity one for %s" prim_native_name
      else
        match prim_native_repr_args, prim_native_repr_res with
        | [(_, Unboxed_integer Pint64)], (_, Unboxed_float) -> begin
          match args with
          | [arg] ->
            let result = Variable.create "reinterpreted_int64" in
            let result' = Bound_var.create result Name_mode.normal in
            let bindable = Bound_pattern.singleton result' in
            let prim = P.Unary (Reinterpret_int64_as_float, arg) in
            let acc, return_result =
              Apply_cont_with_acc.create acc return_continuation
                ~args:[Simple.var result] ~dbg
            in
            let acc, return_result_expr =
              Expr_with_acc.create_apply_cont acc return_result
            in
            Let_with_acc.create acc bindable
              (Named.create_prim prim dbg)
              ~body:return_result_expr
            |> Expr_with_acc.create_let
          | [] | _ :: _ ->
            Misc.fatal_errorf "Expected one arg for %s" prim_native_name
        end
        | _, _ ->
          Misc.fatal_errorf "Wrong argument and/or result kind(s) for %s"
            prim_native_name)
    | _ ->
      let acc, callee = use_of_symbol_as_simple acc call_symbol in
      let apply =
        Apply.create ~callee ~continuation:(Return return_continuation)
          exn_continuation ~args ~call_kind dbg ~inlined:Default_inlined
          ~inlining_state:(Inlining_state.default ~round:0)
          ~probe_name:None
      in
      Expr_with_acc.create_apply acc apply
  in
  let call : Acc.t -> Acc.t * Expr_with_acc.t =
    List.fold_left2
      (fun (call : Simple.t list -> Acc.t -> Acc.t * Expr_with_acc.t) arg
           (arg_repr : Primitive.mode * Primitive.native_repr) ->
        let unbox_arg : P.unary_primitive option =
          match arg_repr with
          | _, Same_as_ocaml_repr -> None
          | _, Unboxed_float -> Some (P.Unbox_number Naked_float)
          | _, Unboxed_integer Pnativeint ->
            Some (P.Unbox_number Naked_nativeint)
          | _, Unboxed_integer Pint32 -> Some (P.Unbox_number Naked_int32)
          | _, Unboxed_integer Pint64 -> Some (P.Unbox_number Naked_int64)
          | _, Untagged_int -> Some (P.Unbox_number Untagged_immediate)
        in
        match unbox_arg with
        | None -> fun args acc -> call (arg :: args) acc
        | Some named ->
          fun args acc ->
            let unboxed_arg = Variable.create "unboxed" in
            let unboxed_arg' = VB.create unboxed_arg Name_mode.normal in
            let acc, body = call (Simple.var unboxed_arg :: args) acc in
            let named = Named.create_prim (Unary (named, arg)) dbg in
            Let_with_acc.create acc
              (Bound_pattern.singleton unboxed_arg')
              named ~body
            |> Expr_with_acc.create_let)
      call args prim_native_repr_args []
  in
  let wrap_c_call acc ~handler_param ~code_after_call c_call =
    let return_kind = Flambda_kind.With_subkind.create return_kind Anything in
    let params = [Bound_parameter.create handler_param return_kind] in
    Let_cont_with_acc.build_non_recursive acc return_continuation
      ~handler_params:params ~handler:code_after_call ~body:c_call
      ~is_exn_handler:false
  in
  let keep_body acc =
    ( Acc.with_cost_metrics
        (Cost_metrics.( + ) (Acc.cost_metrics acc) cost_metrics_of_body)
        (Acc.with_free_names free_names_of_body acc),
      body )
  in
  let box_unboxed_returns ~let_bound_var ~box_return_value =
    let let_bound_var' = VB.create let_bound_var Name_mode.normal in
    let handler_param = Variable.rename let_bound_var in
    let body acc =
      let acc, body = keep_body acc in
      let named =
        Named.create_prim
          (Unary (box_return_value, Simple.var handler_param))
          dbg
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton let_bound_var')
        named ~body
      |> Expr_with_acc.create_let
    in
    body, handler_param
  in
  match box_return_value with
  | None ->
    if needs_wrapper
    then
      wrap_c_call acc ~handler_param:let_bound_var ~code_after_call:keep_body
        call
    else
      (* Here the body is discarded. It might be useful to explicitly remove
         anything that has been added to the acc while converting the body.
         However, as we are hitting this code only when body is a goto
         continuation where the only parameter is [let_bound_var] this operation
         would be a noop and we can skip it. *)
      call acc
  | Some box_return_value ->
    let code_after_call, handler_param =
      box_unboxed_returns ~let_bound_var ~box_return_value
    in
    wrap_c_call acc ~handler_param ~code_after_call call

let close_exn_continuation acc env (exn_continuation : IR.exn_continuation) =
  let acc, extra_args =
    List.fold_left_map
      (fun acc (simple, kind) ->
        let acc, simple = find_simple acc env simple in
        acc, (simple, LC.value_kind kind))
      acc exn_continuation.extra_args
  in
  ( acc,
    Exn_continuation.create ~exn_handler:exn_continuation.exn_handler
      ~extra_args )

let close_primitive acc env ~let_bound_var named (prim : Lambda.primitive) ~args
    loc (exn_continuation : IR.exn_continuation option)
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  let acc, exn_continuation =
    match exn_continuation with
    | None -> acc, None
    | Some exn_continuation ->
      let acc, cont = close_exn_continuation acc env exn_continuation in
      acc, Some cont
  in
  let acc, args = find_simples acc env args in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Pccall is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_c_call acc ~let_bound_var prim ~args exn_continuation dbg k
  | Pgetglobal id, [] ->
    let is_predef_exn = Ident.is_predef id in
    if not (is_predef_exn || not (Ident.same id (Env.current_unit_id env)))
    then
      Misc.fatal_errorf "Non-predef Pgetglobal %a in the same unit" Ident.print
        id;
    let acc, simple = symbol_for_ident acc env id in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Praise raise_kind, [_] ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    let exn_handler = Exn_continuation.exn_handler exn_continuation in
    let args =
      (* CR mshinwell: Share with [Lambda_to_flambda_primitives_helpers] *)
      let extra_args =
        List.map
          (fun (simple, _kind) -> simple)
          (Exn_continuation.extra_args exn_continuation)
      in
      args @ extra_args
    in
    let raise_kind = Some (LC.raise_kind raise_kind) in
    let trap_action = Trap_action.Pop { exn_handler; raise_kind } in
    let acc, apply_cont =
      Apply_cont_with_acc.create acc ~trap_action exn_handler ~args ~dbg
    in
    (* Since raising of an exception doesn't terminate, we don't call [k]. *)
    Expr_with_acc.create_apply_cont acc apply_cont
  | (Pmakeblock _ | Pmakefloatblock _ | Pmakearray _), [] ->
    (* Special case for liftable empty block or array *)
    let acc, sym =
      match prim with
      | Pmakeblock (tag, _, _, mode) ->
        LC.alloc_mode mode;
        if tag <> 0
        then
          (* There should not be any way to reach this from Ocaml code. *)
          Misc.fatal_error
            "Non-zero tag on empty block allocation in [Closure_conversion]"
        else
          register_const0 acc
            (Static_const.Block (Tag.Scannable.zero, Immutable, []))
            "empty_block"
      | Pmakefloatblock _ ->
        Misc.fatal_error "Unexpected empty float block in [Closure_conversion]"
      | Pmakearray (_, _, mode) ->
        LC.alloc_mode mode;
        register_const0 acc Static_const.Empty_array "empty_array"
      | Pidentity | Pbytes_to_string | Pbytes_of_string | Pignore | Prevapply _
      | Pdirapply _ | Pgetglobal _ | Psetglobal _ | Pfield _ | Pfield_computed _
      | Psetfield _ | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _
      | Pduprecord _ | Pccall _ | Praise _ | Psequand | Psequor | Pnot | Pnegint
      | Paddint | Psubint | Pmulint | Pdivint _ | Pmodint _ | Pandint | Porint
      | Pxorint | Plslint | Plsrint | Pasrint | Pintcomp _ | Pcompare_ints
      | Pcompare_floats | Pcompare_bints _ | Poffsetint _ | Poffsetref _
      | Pintoffloat | Pfloatofint _ | Pnegfloat _ | Pabsfloat _ | Paddfloat _
      | Psubfloat _ | Pmulfloat _ | Pdivfloat _ | Pfloatcomp _ | Pstringlength
      | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
      | Pbytesrefs | Pbytessets | Pduparray _ | Parraylength _ | Parrayrefu _
      | Parraysetu _ | Parrayrefs _ | Parraysets _ | Pisint | Pisout
      | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _ | Paddbint _
      | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _
      | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
      | Pbintcomp _ | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _
      | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_64 _
      | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_64 _ | Pbytes_set_16 _
      | Pbytes_set_32 _ | Pbytes_set_64 _ | Pbigstring_load_16 _
      | Pbigstring_load_32 _ | Pbigstring_load_64 _ | Pbigstring_set_16 _
      | Pbigstring_set_32 _ | Pbigstring_set_64 _ | Pctconst _ | Pbswap16
      | Pbbswap _ | Pint_as_pointer | Popaque | Pprobe_is_enabled _ ->
        (* Inconsistent with outer match *)
        assert false
    in
    k acc (Some (Named.create_simple (Simple.symbol sym)))
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind acc exn_continuation
      ~big_endian:(Env.big_endian env)
      ~register_const_string:(fun acc -> register_const_string acc)
      prim ~args dbg k

let close_trap_action_opt trap_action =
  Option.map
    (fun (trap_action : IR.trap_action) : Trap_action.t ->
      match trap_action with
      | Push { exn_handler } -> Push { exn_handler }
      | Pop { exn_handler } -> Pop { exn_handler; raise_kind = None })
    trap_action

let close_named acc env ~let_bound_var (named : IR.named)
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  match named with
  | Simple (Var id) ->
    let acc, simple =
      if not (Ident.is_predef id)
      then find_simple acc env (Var id)
      else symbol_for_ident acc env id
    in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Simple (Const cst) ->
    let acc, named, _name = close_const acc cst in
    k acc (Some named)
  | Get_tag var ->
    let named = find_simple_from_id env var in
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Unary (Box_number Untagged_immediate, Prim (Unary (Get_tag, Simple named)))
    in
    Lambda_to_flambda_primitives_helpers.bind_rec acc None
      ~register_const_string:(fun acc -> register_const_string acc)
      prim Debuginfo.none
      (fun acc named -> k acc (Some named))
  | Prim { prim; args; loc; exn_continuation } ->
    close_primitive acc env ~let_bound_var named prim ~args loc exn_continuation
      k

let close_let acc env id user_visible defining_expr
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  let body_env, var = Env.add_var_like env id user_visible in
  let cont acc (defining_expr : Named.t option) =
    match defining_expr with
    | Some (Simple simple) ->
      let body_env = Env.add_simple_to_substitute env id simple in
      body acc body_env
    | None -> body acc body_env
    | Some defining_expr -> (
      (* CR pchambart: Not tail ! *)
      let body_env =
        match defining_expr with
        | Prim (Variadic (Make_block (_, Immutable), fields), _) ->
          let approxs =
            List.map (Env.find_value_approximation body_env) fields
            |> Array.of_list
          in
          Some (Env.add_block_approximation body_env (Name.var var) approxs)
        | Prim (Binary (Block_load _, block, field), _) -> begin
          match Env.find_value_approximation body_env block with
          | Value_unknown -> Some body_env
          | Closure_approximation _ ->
            if Flambda_features.check_invariants ()
            then
              (* CR keryan: This is hidden behind invariants check because it
                 can appear on correct code using Lazy or GADT. It might warrant
                 a proper warning at some point. *)
              Misc.fatal_errorf
                "Closure approximation found when block approximation was \
                 expected in [Closure_conversion]: %a"
                Named.print defining_expr
            else None
          | Block_approximation approx ->
            let approx : Env.value_approximation =
              Simple.pattern_match field
                ~const:(fun const ->
                  match Reg_width_things.Const.descr const with
                  | Tagged_immediate i ->
                    let i = Targetint_31_63.(Imm.to_int (to_targetint i)) in
                    if i >= Array.length approx
                    then
                      Misc.fatal_errorf
                        "Trying to access the %dth field of a block \
                         approximation of length %d."
                        i (Array.length approx);
                    approx.(i)
                  | _ -> Env.Value_unknown)
                ~name:(fun _ ~coercion:_ -> Env.Value_unknown)
            in
            Some (Env.add_value_approximation body_env (Name.var var) approx)
        end
        | _ -> Some body_env
      in
      match body_env with
      | Some body_env ->
        let acc, body = body acc body_env in
        let var = VB.create var Name_mode.normal in
        Let_with_acc.create acc
          (Bound_pattern.singleton var)
          defining_expr ~body
        |> Expr_with_acc.create_let
      | None -> acc, Expr.create_invalid ~semantics:Treat_as_unreachable ())
  in
  close_named acc env ~let_bound_var:var defining_expr cont

let close_let_cont acc env ~name ~is_exn_handler ~params
    ~(recursive : Asttypes.rec_flag)
    ~(handler : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  (if is_exn_handler
  then
    match recursive with
    | Nonrecursive -> ()
    | Recursive ->
      Misc.fatal_errorf
        "[Let_cont]s marked as exception handlers must be [Nonrecursive]: %a"
        Continuation.print name);
  let params_with_kinds = params in
  let handler_env, params =
    Env.add_vars_like env
      (List.map
         (fun (param, user_visible, _kind) -> param, user_visible)
         params)
  in
  let handler_params =
    List.map2
      (fun param (_, _, kind) ->
        Bound_parameter.create param (LC.value_kind kind))
      params params_with_kinds
  in
  let handler acc =
    let handler_env =
      match Acc.continuation_known_arguments ~cont:name acc with
      | None -> handler_env
      | Some args ->
        List.fold_left2
          (fun env arg_approx param ->
            Env.add_value_approximation env (Name.var param) arg_approx)
          handler_env args params
    in
    handler acc handler_env
  in
  let body acc = body acc env in
  match recursive with
  | Nonrecursive ->
    Let_cont_with_acc.build_non_recursive acc name ~handler_params ~handler
      ~body ~is_exn_handler
  | Recursive ->
    let handlers =
      Continuation.Map.singleton name (handler, handler_params, is_exn_handler)
    in
    Let_cont_with_acc.build_recursive acc ~handlers ~body

let close_apply acc env
    ({ kind;
       func;
       args;
       continuation;
       exn_continuation;
       loc;
       tailcall = _;
       inlined;
       specialised = _;
       probe
     } :
      IR.apply) : Acc.t * Expr_with_acc.t =
  let acc, call_kind =
    match kind with
    | Function -> acc, Call_kind.indirect_function_call_unknown_arity ()
    | Method { kind; obj } ->
      let acc, obj = find_simple acc env obj in
      acc, Call_kind.method_call (LC.method_kind kind) ~obj
  in
  let acc, apply_exn_continuation =
    close_exn_continuation acc env exn_continuation
  in
  let callee = find_simple_from_id env func in
  let acc, args = find_simples acc env args in
  let inlined_call = LC.inlined_attribute inlined in
  let probe_name =
    match probe with None -> None | Some { name } -> Some name
  in
  let apply =
    Apply.create ~callee ~continuation:(Return continuation)
      apply_exn_continuation ~args ~call_kind
      (Debuginfo.from_location loc)
      ~inlined:inlined_call
      ~inlining_state:(Inlining_state.default ~round:0)
      ~probe_name
  in
  if Flambda_features.classic_mode ()
  then
    match Inlining.inlinable env apply with
    | Not_inlinable -> Expr_with_acc.create_apply acc apply
    | Inlinable func_desc ->
      Inlining.inline acc ~apply ~apply_depth:(Env.current_depth env) ~func_desc
  else Expr_with_acc.create_apply acc apply

let close_apply_cont acc env cont trap_action args : Acc.t * Expr_with_acc.t =
  let acc, args = find_simples acc env args in
  let trap_action = close_trap_action_opt trap_action in
  let args_approx = List.map (Env.find_value_approximation env) args in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ?trap_action ~args_approx cont ~args
      ~dbg:Debuginfo.none
  in
  Expr_with_acc.create_apply_cont acc apply_cont

let close_switch acc env scrutinee (sw : IR.switch) : Acc.t * Expr_with_acc.t =
  let scrutinee = find_simple_from_id env scrutinee in
  let untagged_scrutinee = Variable.create "untagged" in
  let untagged_scrutinee' = VB.create untagged_scrutinee Name_mode.normal in
  let untag =
    Named.create_prim
      (Unary (Unbox_number Untagged_immediate, scrutinee))
      Debuginfo.none
  in
  let acc, arms =
    List.fold_left_map
      (fun acc (case, cont, trap_action, args) ->
        let trap_action = close_trap_action_opt trap_action in
        let acc, args = find_simples acc env args in
        let acc, action =
          Apply_cont_with_acc.create acc ?trap_action cont ~args
            ~dbg:Debuginfo.none
        in
        acc, (Targetint_31_63.int (Targetint_31_63.Imm.of_int case), action))
      acc sw.consts
  in
  match arms, sw.failaction with
  | [(case, action)], Some (default_action, default_trap_action, default_args)
    when sw.numconsts >= 3 ->
    (* Avoid enormous switches, where every arm goes to the same place except
       one, that arise from single-arm [Lambda] switches with a default case.
       (Seen in code generated by ppx_compare for variants, which exhibited
       quadratic size blowup.) *)
    let compare =
      Named.create_prim
        (Binary
           ( Phys_equal (Flambda_kind.naked_immediate, Eq),
             Simple.var untagged_scrutinee,
             Simple.const (Reg_width_const.naked_immediate case) ))
        Debuginfo.none
    in
    let comparison_result = Variable.create "eq" in
    let comparison_result' = VB.create comparison_result Name_mode.normal in
    let acc, default_action =
      let acc, args = find_simples acc env default_args in
      let trap_action = close_trap_action_opt default_trap_action in
      Apply_cont_with_acc.create acc ?trap_action default_action ~args
        ~dbg:Debuginfo.none
    in
    let acc, switch =
      let scrutinee = Simple.var comparison_result in
      Expr_with_acc.create_switch acc
        (Switch.if_then_else ~scrutinee ~if_true:action ~if_false:default_action)
    in
    let acc, body =
      Let_with_acc.create acc
        (Bound_pattern.singleton comparison_result')
        compare ~body:switch
      |> Expr_with_acc.create_let
    in
    Let_with_acc.create acc
      (Bound_pattern.singleton untagged_scrutinee')
      untag ~body
    |> Expr_with_acc.create_let
  | _, _ ->
    let acc, arms =
      match sw.failaction with
      | None -> acc, Targetint_31_63.Map.of_list arms
      | Some (default, trap_action, args) ->
        Numeric_types.Int.Set.fold
          (fun case (acc, cases) ->
            let case = Targetint_31_63.int (Targetint_31_63.Imm.of_int case) in
            if Targetint_31_63.Map.mem case cases
            then acc, cases
            else
              let acc, args = find_simples acc env args in
              let trap_action = close_trap_action_opt trap_action in
              let acc, default =
                Apply_cont_with_acc.create acc ?trap_action default ~args
                  ~dbg:Debuginfo.none
              in
              acc, Targetint_31_63.Map.add case default cases)
          (Numeric_types.Int.zero_to_n (sw.numconsts - 1))
          (acc, Targetint_31_63.Map.of_list arms)
    in
    if Targetint_31_63.Map.is_empty arms
    then Expr_with_acc.create_invalid acc ()
    else
      let scrutinee = Simple.var untagged_scrutinee in
      let acc, body =
        match Targetint_31_63.Map.get_singleton arms with
        | Some (_discriminant, action) ->
          Expr_with_acc.create_apply_cont acc action
        | None ->
          Expr_with_acc.create_switch acc (Switch.create ~scrutinee ~arms)
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton untagged_scrutinee')
        untag ~body
      |> Expr_with_acc.create_let

let close_one_function acc ~external_env ~by_closure_id decl
    ~var_within_closures_from_idents ~closure_ids_from_idents
    function_declarations =
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let params = Function_decl.params decl in
  let return = Function_decl.return decl in
  let return_continuation = Function_decl.return_continuation decl in
  let recursive = Function_decl.recursive decl in
  let my_closure = Variable.create "my_closure" in
  let closure_id = Function_decl.closure_id decl in
  let my_closure_id = closure_id in
  let my_depth = Variable.create "my_depth" in
  let next_depth = Variable.create "next_depth" in
  let our_let_rec_ident = Function_decl.let_rec_ident decl in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let code_id =
    Code_id.create ~name:(Closure_id.to_string closure_id) compilation_unit
  in
  let is_curried =
    match Function_decl.kind decl with Curried _ -> true | Tupled -> false
  in
  (* The free variables are: - The parameters: direct substitution by
     [Variable]s - The function being defined: accessible through [my_closure] -
     Other functions in the set being defined: accessible from [my_closure] then
     a [Select_closure] - Other free variables: accessible using [Project_var]
     from [my_closure]. Note that free variables corresponding to predefined
     exception identifiers have been filtered out by [close_functions],
     above. *)
  let var_within_closures_to_bind, var_within_closures_for_idents =
    Ident.Map.fold
      (fun id var_within_closures_for_idents (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        ( Variable.Map.add var var_within_closures_for_idents to_bind,
          Ident.Map.add id var var_for_ident ))
      var_within_closures_from_idents
      (Variable.Map.empty, Ident.Map.empty)
  in
  let coerce_to_deeper =
    Coercion.change_depth
      ~from:(Rec_info_expr.var my_depth)
      ~to_:(Rec_info_expr.var next_depth)
  in
  (* CR mshinwell: Remove "project_closure" names *)
  let project_closure_to_bind, simples_for_project_closure =
    List.fold_left
      (fun (to_bind, simples_for_idents) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same our_let_rec_ident let_rec_ident && is_curried
          then
            (* When the function being compiled is tupled, my_closure points to
               the curried version but let_rec_ident is called with tuple
               arguments, so the correct closure to bind is the one in the
               closure_ids_from_idents map. *)
            to_bind, my_closure
            (* my_closure is already bound *)
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_ids_from_idents
            in
            Variable.Map.add variable closure_id to_bind, variable
        in
        let simple = Simple.with_coercion (Simple.var var) coerce_to_deeper in
        to_bind, Ident.Map.add let_rec_ident simple simples_for_idents)
      (Variable.Map.empty, Ident.Map.empty)
      (Function_decls.to_list function_declarations)
  in
  let closure_env_without_parameters =
    let empty_env = Env.clear_local_bindings external_env in
    let env_with_vars =
      Ident.Map.fold
        (fun id var env ->
          Simple.pattern_match
            (find_simple_from_id external_env id)
            ~const:(fun _ -> assert false)
            ~name:(fun name ~coercion:_ ->
              Env.add_approximation_alias (Env.add_var env id var) name
                (Name.var var)))
        var_within_closures_for_idents empty_env
    in
    Env.add_simple_to_substitute_map env_with_vars simples_for_project_closure
  in
  let closure_env =
    List.fold_right
      (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      params closure_env_without_parameters
  in
  let closure_env = Env.with_depth closure_env my_depth in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are not marked
     as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (id, kind) -> Env.find_var closure_env id, kind) params
  in
  let params =
    List.map
      (fun (var, kind) -> Bound_parameter.create var (LC.value_kind kind))
      param_vars
  in
  let acc = Acc.with_seen_a_function acc false in
  let acc, body =
    try body acc closure_env
    with Misc.Fatal_error ->
      let bt = Printexc.get_raw_backtrace () in
      Format.eprintf
        "\n\
         %sContext is:%s closure converting function@ with [our_let_rec_ident] \
         %a (closure ID %a)"
        (* @ \ *)
        (* and body:@ %a *)
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Ident.print our_let_rec_ident Closure_id.print closure_id;
      (* print body *)
      Printexc.raise_with_backtrace Misc.Fatal_error bt
  in
  let contains_subfunctions = Acc.seen_a_function acc in
  let my_closure' = Simple.var my_closure in
  let acc, body =
    (* CR mshinwell: These Select_closure operations should maybe be inserted at
       the point of use rather than at the top of the function. We should also
       check the behaviour of the backend w.r.t. CSE of projections from
       closures. *)
    Variable.Map.fold
      (fun var closure_id (acc, body) ->
        let move : Flambda_primitive.unary_primitive =
          Select_closure { move_from = my_closure_id; move_to = closure_id }
        in
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim (Unary (move, my_closure')) Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body
        |> Expr_with_acc.create_let)
      project_closure_to_bind (acc, body)
  in
  let acc, body =
    Variable.Map.fold
      (fun var var_within_closure (acc, body) ->
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim
            (Unary
               ( Project_var
                   { project_from = my_closure_id; var = var_within_closure },
                 my_closure' ))
            Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body
        |> Expr_with_acc.create_let)
      var_within_closures_to_bind (acc, body)
  in
  let next_depth_expr = Rec_info_expr.succ (Rec_info_expr.var my_depth) in
  let bound =
    Bound_pattern.singleton (Bound_var.create next_depth Name_mode.normal)
  in
  let acc, body =
    Let_with_acc.create acc bound (Named.create_rec_info next_depth_expr) ~body
    |> Expr_with_acc.create_let
  in
  let cost_metrics = Acc.cost_metrics acc in
  let acc, exn_continuation =
    close_exn_continuation acc external_env
      (Function_decl.exn_continuation decl)
  in
  assert (
    match Exn_continuation.extra_args exn_continuation with
    | [] -> true
    | _ :: _ -> false);
  let inline : Inline_attribute.t =
    (* We make a decision based on [fallback_inlining_heuristic] here to try to
       mimic Closure's behaviour as closely as possible, particularly when there
       are functions involving constant closures, which are not lifted during
       Closure (but will prevent inlining) but will likely have been lifted by
       our other check in [Inlining_cost] (thus preventing us seeing they were
       originally there). *)
    if contains_subfunctions
       && Flambda_features.Expert.fallback_inlining_heuristic ()
    then Never_inline
    else LC.inline_attribute (Function_decl.inline decl)
  in
  let params_and_body =
    Function_params_and_body.create ~return_continuation
      ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      params ~body ~my_closure ~my_depth
      ~free_names_of_body:(Known (Acc.free_names acc))
  in
  let acc =
    List.fold_left
      (fun acc param ->
        Acc.remove_var_from_free_names (Bound_parameter.var param) acc)
      acc params
    |> Acc.remove_var_from_free_names my_closure
    |> Acc.remove_var_from_free_names my_depth
    |> Acc.remove_continuation_from_free_names return_continuation
    |> Acc.remove_continuation_from_free_names
         (Exn_continuation.exn_handler exn_continuation)
  in
  let params_arity = Bound_parameter.List.arity_with_subkinds params in
  let is_tupled =
    match Function_decl.kind decl with Curried _ -> false | Tupled -> true
  in
  let code_size = Cost_metrics.size cost_metrics in
  let inline_threshold = Inlining.threshold () in
  let inlining_decision =
    if Flambda_features.classic_mode ()
    then
      match inline with
      | Never_inline ->
        Function_decl_inlining_decision_type.Never_inline_attribute
      | Always_inline | Available_inline ->
        Function_decl_inlining_decision_type.Attribute_inline
      | _ ->
        if Code_size.to_int code_size <= inline_threshold
        then
          Function_decl_inlining_decision_type.Small_function
            { size = code_size;
              small_function_size = Code_size.of_int inline_threshold
            }
        else
          Function_decl_inlining_decision_type.Function_body_too_large
            (Code_size.of_int inline_threshold)
    else Function_decl_inlining_decision_type.Not_yet_decided
  in
  let code =
    Code.create code_id ~params_and_body
      ~free_names_of_params_and_body:(Acc.free_names acc) ~params_arity
      ~result_arity:[LC.value_kind return]
      ~result_types:
        (Result_types.create_unknown ~params ~result_arity:[LC.value_kind return])
      ~stub ~inline
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~recursive ~newer_version_of:None ~cost_metrics
      ~inlining_arguments:(Inlining_arguments.create ~round:0)
      ~dbg ~is_tupled
      ~is_my_closure_used:
        (Function_params_and_body.is_my_closure_used params_and_body)
      ~inlining_decision
  in
  let approx =
    if Flambda_features.classic_mode ()
    then begin
      Inlining_report.record_decision ~dbg
        (At_function_declaration
           { pass = After_closure_conversion;
             code_id = Code_id.export code_id;
             decision = inlining_decision
           });
      if Function_decl_inlining_decision_type.must_be_inlined inlining_decision
      then Some code
      else None
    end
    else None
  in
  let acc = Acc.add_code ~code_id ~code acc in
  let acc = Acc.with_seen_a_function acc true in
  acc, Closure_id.Map.add my_closure_id (code_id, approx) by_closure_id

let close_functions acc external_env function_declarations =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let var_within_closures_from_idents =
    Ident.Set.fold
      (fun id map ->
        (* Filter out predefined exception identifiers and simple substitutions.
           The former will be turned into symbols, and the latter substituted
           when we closure-convert the body *)
        let has_non_var_subst, subst_var =
          match Env.find_simple_to_substitute_exn external_env id with
          | exception Not_found -> false, None
          | simple ->
            Simple.pattern_match simple
              ~const:(fun _ -> true, None)
              ~name:(fun name ~coercion:_ ->
                Name.pattern_match name
                  ~var:(fun var -> false, Some var)
                  ~symbol:(fun _ -> true, None))
        in
        if has_non_var_subst || Ident.is_predef id
        then map
        else
          let var =
            match subst_var with
            | None -> Variable.create_with_same_name_as_ident id
            | Some var -> Variable.rename var
          in
          Ident.Map.add id (Var_within_closure.wrap compilation_unit var) map)
      (Function_decls.all_free_idents function_declarations)
      Ident.Map.empty
  in
  let func_decl_list = Function_decls.to_list function_declarations in
  let closure_ids_from_idents =
    List.fold_left
      (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let closure_id = Function_decl.closure_id decl in
        Ident.Map.add id closure_id map)
      Ident.Map.empty func_decl_list
  in
  let acc, approximations =
    List.fold_left
      (fun (acc, by_closure_id) function_decl ->
        let _, _, acc, expr =
          Acc.measure_cost_metrics acc ~f:(fun acc ->
              close_one_function acc ~external_env ~by_closure_id function_decl
                ~var_within_closures_from_idents ~closure_ids_from_idents
                function_declarations)
        in
        acc, expr)
      (acc, Closure_id.Map.empty)
      func_decl_list
  in
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  (* CR lmaurer: funs has arbitrary order (ultimately coming from
     function_declarations) *)
  let funs, approximations =
    let funs, approxs =
      Closure_id.Map.fold
        (fun cid (code_id, desc) (funs, approxs) ->
          (cid, code_id) :: funs, (code_id, desc) :: approxs)
        approximations ([], [])
    in
    Closure_id.Lmap.of_list (List.rev funs), List.rev approxs
  in
  let function_decls = Function_declarations.create funs in
  let closure_elements =
    Ident.Map.fold
      (fun id var_within_closure map ->
        let external_simple = find_simple_from_id external_env id in
        (* We're sure [external_simple] is a variable since
           [var_within_closure_from_idents] has already filtered constants and
           symbols out. *)
        Var_within_closure.Map.add var_within_closure external_simple map)
      var_within_closures_from_idents Var_within_closure.Map.empty
  in
  let set_of_closures =
    Set_of_closures.create function_decls ~closure_elements
  in
  let acc =
    Acc.add_set_of_closures_offsets ~is_phantom:false acc set_of_closures
  in
  acc, set_of_closures, approximations

let close_let_rec acc env ~function_declarations
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) =
  let env =
    List.fold_right
      (fun decl env ->
        let id = Function_decl.let_rec_ident decl in
        let env, _var = Env.add_var_like env id User_visible in
        env)
      function_declarations env
  in
  let closure_vars =
    List.fold_left
      (fun closure_vars decl ->
        let closure_var =
          VB.create
            (Env.find_var env (Function_decl.let_rec_ident decl))
            Name_mode.normal
        in
        let closure_id = Function_decl.closure_id decl in
        Closure_id.Map.add closure_id closure_var closure_vars)
      Closure_id.Map.empty function_declarations
  in
  let acc, set_of_closures, approximations =
    close_functions acc env (Function_decls.create function_declarations)
  in
  (* CR mshinwell: We should maybe have something more elegant here *)
  let generated_closures =
    Closure_id.Set.diff
      (Closure_id.Map.keys
         (Function_declarations.funs
            (Set_of_closures.function_decls set_of_closures)))
      (Closure_id.Map.keys closure_vars)
  in
  let closure_vars =
    Closure_id.Set.fold
      (fun closure_id closure_vars ->
        let closure_var =
          VB.create (Variable.create "generated") Name_mode.normal
        in
        Closure_id.Map.add closure_id closure_var closure_vars)
      generated_closures closure_vars
  in
  let closure_vars =
    List.map
      (fun (closure_id, _) -> Closure_id.Map.find closure_id closure_vars)
      (Function_declarations.funs_in_order
         (Set_of_closures.function_decls set_of_closures)
      |> Closure_id.Lmap.bindings)
  in
  let env =
    List.fold_left2
      (fun env var approx ->
        Env.add_closure_approximation env (Name.var (VB.var var)) approx)
      env closure_vars approximations
  in
  let acc, body = body acc env in
  let named = Named.create_set_of_closures set_of_closures in
  Let_with_acc.create acc
    (Bound_pattern.set_of_closures ~closure_vars)
    named ~body
  |> Expr_with_acc.create_let

let close_program ~symbol_for_global ~big_endian ~module_ident
    ~module_block_size_in_words ~program ~prog_return_cont ~exn_continuation =
  let symbol_for_global ident = symbol_for_global ?comp_unit:None ident in
  let env = Env.create ~symbol_for_global ~big_endian in
  let module_symbol =
    symbol_for_global (Ident.create_persistent (Ident.name module_ident))
  in
  let module_block_tag = Tag.Scannable.zero in
  let module_block_var = Variable.create "module_block" in
  let return_cont = Continuation.create ~sort:Toplevel_return () in
  let closure_offsets : _ Or_unknown.t =
    if Flambda_features.classic_mode ()
    then Known (Closure_offsets.create ())
    else Unknown
  in
  let acc = Acc.create ~symbol_for_global ~closure_offsets in
  let load_fields_body acc =
    let field_vars =
      List.init module_block_size_in_words (fun pos ->
          let pos_str = string_of_int pos in
          pos, Variable.create ("field_" ^ pos_str))
    in
    let acc, body =
      let static_const : Static_const.t =
        let field_vars =
          List.map
            (fun (_, var) : Field_of_static_block.t -> Dynamically_computed var)
            field_vars
        in
        Block (module_block_tag, Immutable, field_vars)
      in
      let acc, arg = use_of_symbol_as_simple acc module_symbol in
      let acc, apply_cont =
        (* Module initialisers return unit, but since that is taken care of
           during Cmm generation, we can instead "return" [module_symbol] here
           to ensure that its associated "let symbol" doesn't get deleted. *)
        Apply_cont_with_acc.create acc return_cont ~args:[arg]
          ~dbg:Debuginfo.none
      in
      let acc, return = Expr_with_acc.create_apply_cont acc apply_cont in
      let bound_symbols =
        Bound_symbols.singleton (Bound_symbols.Pattern.block_like module_symbol)
      in
      let named =
        Named.create_static_consts
          (Static_const_group.create
             [Static_const_or_code.create_static_const static_const])
      in
      Let_with_acc.create acc
        (Bound_pattern.symbols bound_symbols)
        named ~body:return
      |> Expr_with_acc.create_let
    in
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known (Targetint_31_63.Imm.of_int module_block_size_in_words);
          field_kind = Any_value
        }
    in
    List.fold_left
      (fun (acc, body) (pos, var) ->
        let var = VB.create var Name_mode.normal in
        let pos = Targetint_31_63.int (Targetint_31_63.Imm.of_int pos) in
        let named =
          Named.create_prim
            (Binary
               ( Block_load (block_access, Immutable),
                 Simple.var module_block_var,
                 Simple.const (Reg_width_const.tagged_immediate pos) ))
            Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body
        |> Expr_with_acc.create_let)
      (acc, body) (List.rev field_vars)
  in
  let load_fields_handler_param =
    [Bound_parameter.create module_block_var K.With_subkind.any_value]
  in
  let acc, body =
    (* This binds the return continuation that is free (or, at least, not bound)
       in the incoming code. The handler for the continuation receives a tuple
       with fields indexed from zero to [module_block_size_in_words]. The
       handler extracts the fields; the variables bound to such fields are then
       used to define the module block symbol. *)
    let body acc = program acc env in
    Let_cont_with_acc.build_non_recursive acc prog_return_cont
      ~handler_params:load_fields_handler_param ~handler:load_fields_body ~body
      ~is_exn_handler:false
  in
  let acc, body =
    Code_id.Map.fold
      (fun code_id code (acc, body) ->
        let bound_symbols =
          Bound_symbols.singleton (Bound_symbols.Pattern.code code_id)
        in
        let static_const = Static_const_or_code.create_code code in
        let defining_expr =
          Static_const_group.create [static_const] |> Named.create_static_consts
        in
        Let_with_acc.create acc
          (Bound_pattern.symbols bound_symbols)
          defining_expr ~body
        |> Expr_with_acc.create_let)
      (Acc.code acc) (acc, body)
  in
  (* We must make sure there is always an outer [Let_symbol] binding so that
     lifted constants not in the scope of any other [Let_symbol] binding get put
     into the term and not dropped. Adding this extra binding, which will
     actually be removed by the simplifier, avoids a special case. *)
  let acc =
    match Acc.declared_symbols acc with
    | _ :: _ -> acc
    | [] ->
      let acc, (_sym : Symbol.t) =
        register_const0 acc
          (Static_const.Block (Tag.Scannable.zero, Immutable, []))
          "first_const"
      in
      acc
  in
  let acc, body =
    List.fold_left
      (fun (acc, body) (symbol, static_const) ->
        let bound_symbols =
          Bound_symbols.singleton (Bound_symbols.Pattern.block_like symbol)
        in
        let defining_expr =
          Static_const_group.create
            [Static_const_or_code.create_static_const static_const]
          |> Named.create_static_consts
        in
        Let_with_acc.create acc
          (Bound_pattern.symbols bound_symbols)
          defining_expr ~body
        |> Expr_with_acc.create_let)
      (acc, body) (Acc.declared_symbols acc)
  in
  let exported_offsets =
    Or_unknown.map (Acc.closure_offsets acc) ~f:(fun offsets ->
        (* CR gbury: would it be possible to use the free_names from the acc to
           compute the used closure vars ? *)
        Closure_offsets.finalize_offsets offsets ~used_closure_vars:Unknown
          ~used_closure_ids:Unknown)
  in
  ( Flambda_unit.create ~return_continuation:return_cont ~exn_continuation ~body
      ~module_symbol ~used_closure_vars:Unknown,
    Exported_code.add_code
      ~keep_code:(fun _ -> false)
      (Acc.code acc) Exported_code.empty,
    exported_offsets )
