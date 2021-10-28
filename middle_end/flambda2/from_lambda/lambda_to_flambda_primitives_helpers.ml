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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
open Closure_conversion_aux
module P = Flambda_primitive
module VB = Bound_var

(* May be useful for compiling out bounds checks: type bounds_check_result = |
   In_range | Out_of_range

   let bounds_check ~width ~string_length_in_bytes ~index_in_bytes :
   bounds_check_result = let index_in_bytes = Targetint_31_63.to_targetint
   index_in_bytes in if Targetint_31_63.Imm.compare index_in_bytes
   Targetint_31_63.Imm.zero < 0 then Out_of_range else let result_size_in_bytes
   = Targetint_31_63.Imm.of_int
   (Flambda_primitive.byte_width_of_string_accessor_width width) in (* We are
   careful here to avoid overflow for ease of reasoning. *) let
   highest_index_allowed = Targetint_31_63.Imm.sub string_length_in_bytes
   result_size_in_bytes in if Targetint_31_63.Imm.compare index_in_bytes
   highest_index_allowed >= 0 then Out_of_range else In_range *)

type failure =
  | Division_by_zero
  | Index_out_of_bounds

type expr_primitive =
  | Simple of Simple.t
  | Nullary of Flambda_primitive.nullary_primitive
  | Unary of P.unary_primitive * simple_or_prim
  | Binary of P.binary_primitive * simple_or_prim * simple_or_prim
  | Ternary of
      P.ternary_primitive * simple_or_prim * simple_or_prim * simple_or_prim
  | Variadic of P.variadic_primitive * simple_or_prim list
  | Checked of
      { validity_conditions : expr_primitive list;
        primitive : expr_primitive;
        failure : failure;
        (* Predefined exception *)
        dbg : Debuginfo.t
      }

and simple_or_prim =
  | Simple of Simple.t
  | Prim of expr_primitive

let rec print_expr_primitive ppf expr_primitive =
  let module W = Flambda_primitive.Without_args in
  match expr_primitive with
  | Simple simple -> Simple.print ppf simple
  | Nullary prim -> W.print ppf (Nullary prim)
  | Unary (prim, _) -> W.print ppf (Unary prim)
  | Binary (prim, _, _) -> W.print ppf (Binary prim)
  | Ternary (prim, _, _, _) -> W.print ppf (Ternary prim)
  | Variadic (prim, _) -> W.print ppf (Variadic prim)
  | Checked { primitive; _ } ->
    Format.fprintf ppf "@[<hov 1>(Checked@ %a)@]" print_expr_primitive primitive

let print_simple_or_prim ppf (simple_or_prim : simple_or_prim) =
  match simple_or_prim with
  | Simple simple -> Simple.print ppf simple
  | Prim _ -> Format.pp_print_string ppf "<prim>"

let print_list_of_simple_or_prim ppf simple_or_prim_list =
  Format.fprintf ppf "@[(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple_or_prim)
    simple_or_prim_list

let caml_ml_array_bound_error =
  let name = Linkage_name.create "caml_ml_array_bound_error" in
  Symbol.create (Compilation_unit.external_symbols ()) name

let raise_exn_for_failure acc ~dbg exn_cont exn_bucket extra_let_binding =
  let exn_handler = Exn_continuation.exn_handler exn_cont in
  let trap_action =
    Trap_action.Pop { exn_handler; raise_kind = Some Regular }
  in
  let args =
    let extra_args =
      List.map
        (fun (simple, _kind) -> simple)
        (Exn_continuation.extra_args exn_cont)
    in
    [exn_bucket] @ extra_args
  in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ~trap_action exn_handler ~args ~dbg
  in
  let acc, apply_cont = Expr_with_acc.create_apply_cont acc apply_cont in
  match extra_let_binding with
  | None -> acc, apply_cont
  | Some (bound_var, defining_expr) ->
    Let_with_acc.create acc
      (Bound_pattern.singleton bound_var)
      defining_expr ~body:apply_cont
    |> Expr_with_acc.create_let

let expression_for_failure acc exn_cont ~register_const_string primitive dbg
    (failure : failure) =
  let exn_cont =
    match exn_cont with
    | Some exn_cont -> exn_cont
    | None ->
      Misc.fatal_errorf
        "Validity checks for primitive@ %a@ may raise, but no exception \
         continuation was supplied with the Lambda primitive"
        print_expr_primitive primitive
  in
  match failure with
  | Division_by_zero ->
    let division_by_zero =
      (Acc.symbol_for_global acc) Predef.ident_division_by_zero
    in
    raise_exn_for_failure acc ~dbg exn_cont
      (Simple.symbol division_by_zero)
      None
  | Index_out_of_bounds ->
    if true
    then
      let call =
        let callee = Simple.symbol caml_ml_array_bound_error in
        let continuation = Apply.Result_continuation.Never_returns in
        let args = [] in
        let call_kind =
          Call_kind.c_call ~alloc:false ~param_arity:[] ~return_arity:[]
            ~is_c_builtin:false
        in
        (* These inlining fields should not be used for C calls since they can't
           really be inlined anyway. *)
        let inlined = Inlined_attribute.Never_inlined in
        let inlining_state = Inlining_state.default ~round:0 in
        Apply.create ~callee ~continuation exn_cont ~args ~call_kind dbg
          ~inlined ~inlining_state ~probe_name:None
      in
      Expr_with_acc.create_apply acc call
    else
      let exn_bucket = Variable.create "exn_bucket" in
      (* CR mshinwell: Share this text with elsewhere. *)
      let acc, error_text = register_const_string acc "index out of bounds" in
      let invalid_argument =
        (* [Predef.invalid_argument] is not exposed; the following avoids a
           change to the frontend. *)
        let matches ident =
          String.equal (Ident.name ident) "Invalid_argument"
        in
        let invalid_argument =
          match List.find matches Predef.all_predef_exns with
          | exception Not_found ->
            Misc.fatal_error "Cannot find Invalid_argument exception in Predef"
          | ident -> ident
        in
        (Acc.symbol_for_global acc) invalid_argument
      in
      let contents_of_exn_bucket =
        [Simple.symbol invalid_argument; Simple.symbol error_text]
      in
      let named =
        Named.create_prim
          (Variadic
             ( Make_block
                 (Values (Tag.Scannable.zero, [Any_value; Any_value]), Immutable),
               contents_of_exn_bucket ))
          dbg
      in
      let extra_let_binding =
        Bound_var.create exn_bucket Name_mode.normal, named
      in
      raise_exn_for_failure acc ~dbg exn_cont (Simple.var exn_bucket)
        (Some extra_let_binding)

let rec bind_rec acc exn_cont ~register_const_string (prim : expr_primitive)
    (dbg : Debuginfo.t) (cont : Acc.t -> Named.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  match prim with
  | Simple simple ->
    let named = Named.create_simple simple in
    cont acc named
  | Nullary prim ->
    let named = Named.create_prim (Nullary prim) dbg in
    cont acc named
  | Unary (prim, arg) ->
    let cont acc (arg : Simple.t) =
      let named = Named.create_prim (Unary (prim, arg)) dbg in
      cont acc named
    in
    bind_rec_primitive acc exn_cont ~register_const_string arg dbg cont
  | Binary (prim, arg1, arg2) ->
    let cont acc (arg2 : Simple.t) =
      let cont acc (arg1 : Simple.t) =
        let named = Named.create_prim (Binary (prim, arg1, arg2)) dbg in
        cont acc named
      in
      bind_rec_primitive acc exn_cont ~register_const_string arg1 dbg cont
    in
    bind_rec_primitive acc exn_cont ~register_const_string arg2 dbg cont
  | Ternary (prim, arg1, arg2, arg3) ->
    let cont acc (arg3 : Simple.t) =
      let cont acc (arg2 : Simple.t) =
        let cont acc (arg1 : Simple.t) =
          let named =
            Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
          in
          cont acc named
        in
        bind_rec_primitive acc exn_cont ~register_const_string arg1 dbg cont
      in
      bind_rec_primitive acc exn_cont ~register_const_string arg2 dbg cont
    in
    bind_rec_primitive acc exn_cont ~register_const_string arg3 dbg cont
  | Variadic (prim, args) ->
    let cont acc args =
      let named = Named.create_prim (Variadic (prim, args)) dbg in
      cont acc named
    in
    let rec build_cont acc args_to_convert converted_args =
      match args_to_convert with
      | [] -> cont acc converted_args
      | arg :: args_to_convert ->
        let cont acc arg =
          build_cont acc args_to_convert (arg :: converted_args)
        in
        bind_rec_primitive acc exn_cont ~register_const_string arg dbg cont
    in
    build_cont acc (List.rev args) []
  | Checked { validity_conditions; primitive; failure; dbg } ->
    let primitive_cont = Continuation.create () in
    let primitive_handler_expr acc =
      bind_rec acc exn_cont ~register_const_string primitive dbg cont
    in
    let failure_cont = Continuation.create () in
    let failure_handler_expr acc =
      expression_for_failure acc exn_cont ~register_const_string primitive dbg
        failure
    in
    let check_validity_conditions =
      let prim_apply_cont acc =
        let acc, expr = Apply_cont_with_acc.goto acc primitive_cont in
        Expr_with_acc.create_apply_cont acc expr
      in
      List.fold_left
        (fun condition_passed_expr expr_primitive acc ->
          let condition_passed_cont = Continuation.create () in
          let body acc =
            bind_rec_primitive acc exn_cont ~register_const_string
              (Prim expr_primitive) dbg (fun acc prim_result ->
                let acc, condition_passed =
                  Apply_cont_with_acc.goto acc condition_passed_cont
                in
                let acc, failure = Apply_cont_with_acc.goto acc failure_cont in
                Expr_with_acc.create_switch acc
                  (Switch.create ~scrutinee:prim_result
                     ~arms:
                       (Targetint_31_63.Map.of_list
                          [ Targetint_31_63.bool_true, condition_passed;
                            Targetint_31_63.bool_false, failure ])))
          in
          Let_cont_with_acc.build_non_recursive acc condition_passed_cont
            ~handler_params:[] ~handler:condition_passed_expr ~body
            ~is_exn_handler:false)
        prim_apply_cont validity_conditions
    in
    let body acc =
      Let_cont_with_acc.build_non_recursive acc failure_cont ~handler_params:[]
        ~handler:failure_handler_expr ~body:check_validity_conditions
        ~is_exn_handler:false
    in
    Let_cont_with_acc.build_non_recursive acc primitive_cont ~handler_params:[]
      ~handler:primitive_handler_expr ~body ~is_exn_handler:false

and bind_rec_primitive acc exn_cont ~register_const_string
    (prim : simple_or_prim) (dbg : Debuginfo.t)
    (cont : Acc.t -> Simple.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  match prim with
  | Simple s -> cont acc s
  | Prim p ->
    let var = Variable.create "prim" in
    let var' = VB.create var Name_mode.normal in
    let cont acc (named : Named.t) =
      let acc, body = cont acc (Simple.var var) in
      Let_with_acc.create acc (Bound_pattern.singleton var') named ~body
      |> Expr_with_acc.create_let
    in
    bind_rec acc exn_cont ~register_const_string p dbg cont
