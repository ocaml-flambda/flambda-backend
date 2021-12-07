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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda
module DA = Downwards_acc
module DE = Downwards_env
module K = Flambda_kind
module BP = Bound_parameter
module P = Flambda_primitive
module T = Flambda2_types
module TEE = T.Typing_env_extension
module UA = Upwards_acc
module UE = Upwards_env
module AC = Apply_cont_expr

type 'a after_rebuild = Rebuilt_expr.t -> Upwards_acc.t -> 'a

type 'a rebuild = Upwards_acc.t -> after_rebuild:'a after_rebuild -> 'a

type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

type 'a expr_simplifier =
  Downwards_acc.t ->
  'a ->
  down_to_up:
    (Rebuilt_expr.t * Upwards_acc.t, Rebuilt_expr.t * Upwards_acc.t) down_to_up ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_toplevel =
  Downwards_acc.t ->
  Expr.t ->
  return_continuation:Continuation.t ->
  return_arity:Flambda_arity.With_subkinds.t ->
  exn_continuation:Continuation.t ->
  return_cont_scope:Scope.t ->
  exn_cont_scope:Scope.t ->
  Rebuilt_expr.t * Upwards_acc.t

let is_self_tail_call dacc apply =
  let denv = DA.denv dacc in
  match DE.closure_info denv with
  | Not_in_a_closure -> false
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    (* It's safe to return false here (even, though this should not happen) *)
    false
  | Closure
      { code_id = fun_code_id;
        return_continuation = fun_cont;
        exn_continuation = fun_exn_cont
      } -> (
    (* 1st check: exn continuations match *)
    let apply_exn_cont = Apply.exn_continuation apply in
    Exn_continuation.equal
      (Exn_continuation.create ~exn_handler:fun_exn_cont ~extra_args:[])
      apply_exn_cont
    (* 2nd check: return continuations match *)
    && begin
         match Apply.continuation apply with
         (* a function that raises unconditionally can be a tail-call *)
         | Never_returns -> true
         | Return apply_cont -> Continuation.equal fun_cont apply_cont
       end
    &&
    (* 3rd check: check this is a self-call. *)
    match Apply.call_kind apply with
    | Function (Direct { code_id = apply_code_id; _ }) ->
      Code_id.equal fun_code_id apply_code_id
    | Method _ | C_call _
    | Function (Indirect_known_arity _ | Indirect_unknown_arity) ->
      false)

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
    ~result_kind =
  let env = DA.typing_env dacc in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Simplified_named.invalid (), TEE.empty, dacc
  | Ok env_extension ->
    Simplified_named.reachable original_term, env_extension, dacc

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Expr_builder.rewrite_exn_continuation rewrite exn_cont_use_id
         (Apply.exn_continuation apply))

(* generate the projection of the i-th field of a n-tuple *)
let project_tuple ~dbg ~size ~field tuple =
  let module BAK = P.Block_access_kind in
  let bak : BAK.t =
    Values
      { field_kind = Any_value;
        tag = Known Tag.Scannable.zero;
        size = Known (Targetint_31_63.Imm.of_int size)
      }
  in
  let mutability : Mutability.t = Immutable in
  let index = Simple.const_int (Targetint_31_63.Imm.of_int field) in
  let prim = P.Binary (Block_load (bak, mutability), tuple, index) in
  Named.create_prim prim dbg

let split_direct_over_application apply ~param_arity =
  let arity = List.length param_arity in
  let args = Apply.args apply in
  assert (arity < List.length args);
  let full_app_args, remaining_args = Misc.Stdlib.List.split_at arity args in
  let func_var = Variable.create "full_apply" in
  let perform_over_application =
    Apply.create ~callee:(Simple.var func_var)
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args:remaining_args
      ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
      (Apply.dbg apply) ~inlined:(Apply.inlined apply)
      ~inlining_state:(Apply.inlining_state apply)
      ~probe_name:(Apply.probe_name apply)
  in
  let after_full_application = Continuation.create () in
  let after_full_application_handler =
    let func_param = BP.create func_var K.With_subkind.any_value in
    let free_names_of_expr = Apply.free_names perform_over_application in
    Continuation_handler.create [func_param]
      ~handler:(Expr.create_apply perform_over_application)
      ~free_names_of_handler:(Known free_names_of_expr) ~is_exn_handler:false
  in
  let full_apply =
    Apply.with_continuation_callee_and_args apply
      (Return after_full_application) ~callee:(Apply.callee apply)
      ~args:full_app_args
  in
  let expr =
    Let_cont.create_non_recursive after_full_application
      after_full_application_handler
      ~body:(Expr.create_apply full_apply)
      ~free_names_of_body:(Known (Apply.free_names full_apply))
  in
  expr

type apply_cont_context =
  | Apply_cont_expr
  | Switch_branch

let apply_cont_use_kind ~context apply_cont : Continuation_use_kind.t =
  let default : Continuation_use_kind.t =
    match context with
    | Apply_cont_expr -> Inlinable
    | Switch_branch -> Non_inlinable { escaping = false }
  in
  match Continuation.sort (AC.continuation apply_cont) with
  | Normal_or_exn -> begin
    match Apply_cont.trap_action apply_cont with
    | None -> default
    | Some (Push _) -> Non_inlinable { escaping = false }
    | Some (Pop { raise_kind; _ }) -> (
      match raise_kind with
      | None | Some Regular | Some Reraise ->
        (* Until such time as we can manually add to the backtrace buffer, we
           only convert "raise_notrace" into jumps, except if debugging
           information generation is disabled. (This matches the handling at Cmm
           level; see [Cmm_helpers.raise_prim].)

           We set [escaping = true] for the cases we do not want to convert into
           jumps. *)
        if Flambda_features.debug ()
        then Non_inlinable { escaping = true }
        else Non_inlinable { escaping = false }
      | Some No_trace -> Non_inlinable { escaping = false })
  end
  | Return | Toplevel_return -> Non_inlinable { escaping = false }
  | Define_root_symbol ->
    assert (Option.is_none (Apply_cont.trap_action apply_cont));
    default

let clear_demoted_trap_action uacc apply_cont : AC.t =
  match AC.trap_action apply_cont with
  | None -> apply_cont
  | Some (Push { exn_handler } | Pop { exn_handler; _ }) ->
    if UE.mem_continuation (UA.uenv uacc) exn_handler
       && not (UA.is_demoted_exn_handler uacc exn_handler)
    then apply_cont
    else AC.clear_trap_action apply_cont

let patch_unused_exn_bucket uacc apply_cont =
  if AC.is_raise apply_cont
  then
    match AC.args apply_cont with
    | [] -> assert false
    | exn_bucket :: other_args ->
      let exn_bucket_is_used =
        Simple.pattern_match
          ~const:(fun _ -> true)
          ~name:(fun name ~coercion:_ ->
            Name.Set.mem name (UA.required_names uacc))
          exn_bucket
      in
      if exn_bucket_is_used
      then apply_cont
      else
        (* The raise argument must be present, if it is unused, we replace it by
           a dummy value to avoid keeping a useless value alive *)
        let dummy_value = Simple.const_zero in
        AC.update_args ~args:(dummy_value :: other_args) apply_cont
  else apply_cont

let clear_demoted_trap_action_and_patch_unused_exn_bucket uacc apply_cont =
  let apply_cont = clear_demoted_trap_action uacc apply_cont in
  patch_unused_exn_bucket uacc apply_cont

let specialise_array_kind dacc (array_kind : P.Array_kind.t) ~array_ty :
    _ Or_bottom.t =
  let typing_env = DA.typing_env dacc in
  match array_kind with
  | Naked_floats -> (
    match
      T.prove_is_array_with_element_kind typing_env array_ty
        ~element_kind:K.With_subkind.naked_float
    with
    | Proved (Exact | Compatible) | Unknown -> Ok array_kind
    | Proved Incompatible | Invalid -> Bottom)
  | Immediates -> (
    match
      T.prove_is_array_with_element_kind typing_env array_ty
        ~element_kind:K.With_subkind.tagged_immediate
    with
    | Proved (Exact | Compatible) | Unknown -> Ok array_kind
    | Proved Incompatible | Invalid -> Bottom)
  | Values -> (
    match
      T.prove_is_array_with_element_kind typing_env array_ty
        ~element_kind:K.With_subkind.tagged_immediate
    with
    | Proved Exact ->
      (* Specialise the array operation to [Immediates]. *)
      Ok P.Array_kind.Immediates
    | Proved Compatible | Unknown -> Ok array_kind
    | Proved Incompatible | Invalid -> Bottom)
