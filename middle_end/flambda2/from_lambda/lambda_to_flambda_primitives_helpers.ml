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

open! Flambda.Import
open Closure_conversion_aux
module P = Flambda_primitive
module VB = Bound_var

type failure =
  | Division_by_zero
  | Index_out_of_bounds
  | Address_was_misaligned

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
  | If_then_else of expr_primitive * expr_primitive * expr_primitive

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
  | If_then_else (cond, ifso, ifnot) ->
    Format.fprintf ppf
      "@[<hov 1>(If_then_else@ (cond@ %a)@ (ifso@ %a)@ (ifnot@ %a))@]"
      print_expr_primitive cond print_expr_primitive ifso print_expr_primitive
      ifnot

let print_simple_or_prim ppf (simple_or_prim : simple_or_prim) =
  match simple_or_prim with
  | Simple simple -> Simple.print ppf simple
  | Prim _ -> Format.pp_print_string ppf "<prim>"

let print_list_of_simple_or_prim ppf simple_or_prim_list =
  Format.fprintf ppf "@[(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple_or_prim)
    simple_or_prim_list

let print_list_of_lists_of_simple_or_prim ppf simple_or_prim_list_list =
  Format.fprintf ppf "@[(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       print_list_of_simple_or_prim)
    simple_or_prim_list_list

let raise_exn_for_failure acc ~dbg exn_cont exn_bucket =
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
    exn_bucket :: extra_args
  in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ~trap_action exn_handler ~args ~dbg
  in
  Expr_with_acc.create_apply_cont acc apply_cont

let symbol_for_prim id =
  Flambda2_import.Symbol.for_predef_ident id |> Symbol.create_wrapped

let register_invalid_argument ~register_const0 acc error_text =
  let invalid_argument =
    (* [Predef.invalid_argument] is not exposed; the following avoids a change
       to the frontend. *)
    let matches ident = String.equal (Ident.name ident) "Invalid_argument" in
    let invalid_argument =
      match List.find matches Predef.all_predef_exns with
      | exception Not_found ->
        Misc.fatal_error "Cannot find Invalid_argument exception in Predef"
      | ident -> ident
    in
    symbol_for_prim invalid_argument
  in
  let dbg = Debuginfo.none in
  register_const0 acc
    (Static_const.block Tag.Scannable.zero Immutable Value_only
       [ Simple.With_debuginfo.create (Simple.symbol invalid_argument) dbg;
         Simple.With_debuginfo.create (Simple.symbol error_text) dbg ])
    "block"

let expression_for_failure acc exn_cont ~register_const0 primitive dbg
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
    let division_by_zero = symbol_for_prim Predef.ident_division_by_zero in
    raise_exn_for_failure acc ~dbg exn_cont (Simple.symbol division_by_zero)
  | Index_out_of_bounds ->
    (* CR mshinwell: Share this text with elsewhere. *)
    let acc, error_text =
      register_const0 acc
        (Static_const.immutable_string "index out of bounds")
        "string"
    in
    let acc, exn_bucket =
      register_invalid_argument ~register_const0 acc error_text
    in
    raise_exn_for_failure acc ~dbg exn_cont (Simple.symbol exn_bucket)
  | Address_was_misaligned ->
    (* CR mshinwell: Share this text with elsewhere. *)
    let acc, error_text =
      register_const0 acc
        (Static_const.immutable_string "address was misaligned")
        "string"
    in
    let acc, exn_bucket =
      register_invalid_argument ~register_const0 acc error_text
    in
    raise_exn_for_failure acc ~dbg exn_cont (Simple.symbol exn_bucket)

let rec bind_rec acc exn_cont ~register_const0 (prim : expr_primitive)
    (dbg : Debuginfo.t) (cont : Acc.t -> Named.t -> Expr_with_acc.t) :
    Expr_with_acc.t =
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
    bind_rec_primitive acc exn_cont ~register_const0 arg dbg cont
  | Binary (prim, arg1, arg2) ->
    let cont acc (arg2 : Simple.t) =
      let cont acc (arg1 : Simple.t) =
        let named = Named.create_prim (Binary (prim, arg1, arg2)) dbg in
        cont acc named
      in
      bind_rec_primitive acc exn_cont ~register_const0 arg1 dbg cont
    in
    bind_rec_primitive acc exn_cont ~register_const0 arg2 dbg cont
  | Ternary (prim, arg1, arg2, arg3) ->
    let cont acc (arg3 : Simple.t) =
      let cont acc (arg2 : Simple.t) =
        let cont acc (arg1 : Simple.t) =
          let named =
            Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg
          in
          cont acc named
        in
        bind_rec_primitive acc exn_cont ~register_const0 arg1 dbg cont
      in
      bind_rec_primitive acc exn_cont ~register_const0 arg2 dbg cont
    in
    bind_rec_primitive acc exn_cont ~register_const0 arg3 dbg cont
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
        bind_rec_primitive acc exn_cont ~register_const0 arg dbg cont
    in
    build_cont acc (List.rev args) []
  | Checked { validity_conditions; primitive; failure; dbg } ->
    let primitive_cont = Continuation.create () in
    let primitive_handler_expr acc =
      bind_rec acc exn_cont ~register_const0 primitive dbg cont
    in
    let failure_cont = Continuation.create () in
    let failure_handler_expr acc =
      expression_for_failure acc exn_cont ~register_const0 primitive dbg failure
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
            bind_rec_primitive acc exn_cont ~register_const0
              (Prim expr_primitive) dbg (fun acc prim_result ->
                let acc, condition_passed =
                  Apply_cont_with_acc.goto acc condition_passed_cont
                in
                let acc, failure = Apply_cont_with_acc.goto acc failure_cont in
                Expr_with_acc.create_switch acc
                  (Switch.create ~condition_dbg:dbg ~scrutinee:prim_result
                     ~arms:
                       (Targetint_31_63.Map.of_list
                          [ Targetint_31_63.bool_true, condition_passed;
                            Targetint_31_63.bool_false, failure ])))
          in
          Let_cont_with_acc.build_non_recursive acc condition_passed_cont
            ~handler_params:Bound_parameters.empty
            ~handler:condition_passed_expr ~body ~is_exn_handler:false
            ~is_cold:false)
        prim_apply_cont validity_conditions
    in
    let body acc =
      Let_cont_with_acc.build_non_recursive acc failure_cont
        ~handler_params:Bound_parameters.empty ~handler:failure_handler_expr
        ~body:check_validity_conditions ~is_exn_handler:false ~is_cold:true
    in
    Let_cont_with_acc.build_non_recursive acc primitive_cont
      ~handler_params:Bound_parameters.empty ~handler:primitive_handler_expr
      ~body ~is_exn_handler:false ~is_cold:false
  | If_then_else (cond, ifso, ifnot) ->
    let cond_result = Variable.create "cond_result" in
    let cond_result_pat = Bound_var.create cond_result Name_mode.normal in
    let ifso_cont = Continuation.create () in
    let ifso_result = Variable.create "ifso_result" in
    let ifso_result_pat = Bound_var.create ifso_result Name_mode.normal in
    let ifnot_cont = Continuation.create () in
    let ifnot_result = Variable.create "ifnot_result" in
    let ifnot_result_pat = Bound_var.create ifnot_result Name_mode.normal in
    let join_point_cont = Continuation.create () in
    let result_var = Variable.create "if_then_else_result" in
    let result_param =
      Bound_parameter.create result_var Flambda_kind.With_subkind.any_value
    in
    bind_rec acc exn_cont ~register_const0 cond dbg @@ fun acc cond ->
    let compute_cond_and_switch acc =
      let acc, ifso_cont = Apply_cont_with_acc.goto acc ifso_cont in
      let acc, ifnot_cont = Apply_cont_with_acc.goto acc ifnot_cont in
      let acc, switch =
        Expr_with_acc.create_switch acc
          (Switch.create ~condition_dbg:dbg ~scrutinee:(Simple.var cond_result)
             ~arms:
               (Targetint_31_63.Map.of_list
                  [ Targetint_31_63.bool_true, ifso_cont;
                    Targetint_31_63.bool_false, ifnot_cont ]))
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton cond_result_pat)
        cond ~body:switch
    in
    let join_handler_expr acc =
      cont acc (Named.create_simple (Simple.var result_var))
    in
    let ifso_handler_expr acc =
      bind_rec acc exn_cont ~register_const0 ifso dbg @@ fun acc ifso ->
      let acc, apply_cont =
        Apply_cont_with_acc.create acc join_point_cont
          ~args:[Simple.var ifso_result]
          ~dbg
      in
      let acc, body = Expr_with_acc.create_apply_cont acc apply_cont in
      Let_with_acc.create acc
        (Bound_pattern.singleton ifso_result_pat)
        ifso ~body
    in
    let ifnot_handler_expr acc =
      bind_rec acc exn_cont ~register_const0 ifnot dbg @@ fun acc ifnot ->
      let acc, apply_cont =
        Apply_cont_with_acc.create acc join_point_cont
          ~args:[Simple.var ifnot_result]
          ~dbg
      in
      let acc, body = Expr_with_acc.create_apply_cont acc apply_cont in
      Let_with_acc.create acc
        (Bound_pattern.singleton ifnot_result_pat)
        ifnot ~body
    in
    let body acc =
      Let_cont_with_acc.build_non_recursive acc ifnot_cont
        ~handler_params:Bound_parameters.empty ~handler:ifnot_handler_expr
        ~body:compute_cond_and_switch ~is_exn_handler:false ~is_cold:false
    in
    let body acc =
      Let_cont_with_acc.build_non_recursive acc ifso_cont
        ~handler_params:Bound_parameters.empty ~handler:ifso_handler_expr ~body
        ~is_exn_handler:false ~is_cold:false
    in
    Let_cont_with_acc.build_non_recursive acc join_point_cont
      ~handler_params:(Bound_parameters.create [result_param])
      ~handler:join_handler_expr ~body ~is_exn_handler:false ~is_cold:false

and bind_rec_primitive acc exn_cont ~register_const0 (prim : simple_or_prim)
    (dbg : Debuginfo.t) (cont : Acc.t -> Simple.t -> Expr_with_acc.t) :
    Expr_with_acc.t =
  match prim with
  | Simple s -> cont acc s
  | Prim p ->
    let var = Variable.create "prim" in
    let var' = VB.create var Name_mode.normal in
    let cont acc (named : Named.t) =
      let acc, body = cont acc (Simple.var var) in
      Let_with_acc.create acc (Bound_pattern.singleton var') named ~body
    in
    bind_rec acc exn_cont ~register_const0 p dbg cont

let rec bind_recs acc exn_cont ~register_const0 (prims : expr_primitive list)
    (dbg : Debuginfo.t) (cont : Acc.t -> Named.t list -> Expr_with_acc.t) :
    Expr_with_acc.t =
  match prims with
  | [] -> cont acc []
  | prim :: prims ->
    bind_rec acc exn_cont ~register_const0 prim dbg (fun acc named ->
        bind_recs acc exn_cont ~register_const0 prims dbg (fun acc nameds ->
            cont acc (named :: nameds)))
