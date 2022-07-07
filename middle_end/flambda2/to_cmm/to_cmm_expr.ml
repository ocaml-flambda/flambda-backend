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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module Env = To_cmm_env
module Ece = Effects_and_coeffects
module K = Flambda_kind

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Note about flushing of environments: this module treats the delayed bindings
   in [To_cmm_env] environments (see to_cmm_env.mli for more information) in a
   linear manner. Flushes are inserted to preserve this property. This ensures
   in particular that effectful bindings get placed exactly once and that other
   bindings are not duplicated. *)

(* Bind a Cmm variable to the result of translating a [Simple] into Cmm. *)

let bind_var_to_simple ~dbg env v ~num_normal_occurrences_of_bound_vars s =
  let defining_expr, env, effects_and_coeffects_of_defining_expr =
    C.simple ~dbg env s
  in
  Env.bind_variable env v
    ~num_normal_occurrences_of_bound_vars:
      (Known num_normal_occurrences_of_bound_vars)
    ~effects_and_coeffects_of_defining_expr ~defining_expr

(* Helpers for the translation of [Apply] expressions. *)

let translate_apply0 env apply =
  let callee_simple = Apply.callee apply in
  let args = Apply.args apply in
  let dbg = Apply.dbg apply in
  (* CR mshinwell: When we fix the problem that [prim_effects] and
     [prim_coeffects] are ignored for C calls, we need to take into account the
     effects/coeffects values currently ignored on the following two lines. At
     the moment they can be ignored as we always deem all calls to have
     arbitrary effects and coeffects. *)
  let callee, env, _ = C.simple ~dbg env callee_simple in
  let args, env, _ = C.simple_list ~dbg env args in
  let fail_if_probe apply =
    match Apply.probe_name apply with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf
        "[Apply] terms with a [probe_name] (i.e. that call a tracing probe) \
         must always be direct applications of an OCaml function:@ %a"
        Apply.print apply
  in
  let pos =
    match Apply.position apply with
    | Normal ->
      (* We always use [Rc_normal] since the [Lambda_to_flambda] pass has
         already taken care of the placement of region begin/end primitives. *)
      Lambda.Rc_normal
    | Nontail -> Lambda.Rc_nontail
  in
  match Apply.call_kind apply with
  | Function
      { function_call = Direct { code_id; return_arity }; alloc_mode = _ } -> (
    let code_metadata = Env.get_code_metadata env code_id in
    let params_arity = Code_metadata.params_arity code_metadata in
    if not (C.check_arity params_arity args)
    then Misc.fatal_errorf "Wrong arity for direct call";
    let ty =
      return_arity |> Flambda_arity.With_subkinds.to_arity
      |> C.machtype_of_return_arity
    in
    let args =
      if Code_metadata.is_my_closure_used code_metadata
      then args @ [callee]
      else args
    in
    let code_linkage_name = Code_id.linkage_name code_id in
    match Apply.probe_name apply with
    | None ->
      ( C.direct_call ~dbg ty pos
          (C.symbol_from_linkage_name ~dbg code_linkage_name)
          args,
        env,
        Ece.all )
    | Some name ->
      ( C.probe ~dbg ~name
          ~handler_code_linkage_name:(Linkage_name.to_string code_linkage_name)
          ~args
        |> C.return_unit dbg,
        env,
        Ece.all ))
  | Function { function_call = Indirect_unknown_arity; alloc_mode } ->
    fail_if_probe apply;
    ( C.indirect_call ~dbg Cmm.typ_val pos
        (Alloc_mode.to_lambda alloc_mode)
        callee args,
      env,
      Ece.all )
  | Function
      { function_call = Indirect_known_arity { return_arity; param_arity };
        alloc_mode
      } ->
    fail_if_probe apply;
    if not (C.check_arity param_arity args)
    then
      Misc.fatal_errorf
        "To_cmm expects indirect_known_arity calls to be full applications in \
         order to translate it"
    else
      let ty =
        return_arity |> Flambda_arity.With_subkinds.to_arity
        |> C.machtype_of_return_arity
      in
      ( C.indirect_full_call ~dbg ty pos
          (Alloc_mode.to_lambda alloc_mode)
          callee args,
        env,
        Ece.all )
  | Call_kind.C_call { alloc; return_arity; param_arity; is_c_builtin } ->
    fail_if_probe apply;
    let callee =
      match Simple.must_be_symbol callee_simple with
      | Some (sym, _) -> Symbol.linkage_name sym |> Linkage_name.to_string
      | None ->
        Misc.fatal_errorf "Expected a function symbol instead of:@ %a"
          Simple.print callee_simple
    in
    let returns = Apply.returns apply in
    let ty = C.machtype_of_return_arity return_arity in
    let wrap =
      match Flambda_arity.to_list return_arity with
      (* Returned int32 values need to be sign_extended because it's not clear
         whether C code that returns an int32 returns one that is sign extended
         or not. There is no need to wrap other return arities. Note that
         extcalls of arity 0 are allowed (these never return). *)
      | [] -> fun _dbg cmm -> cmm
      | [kind] -> (
        match kind with
        | Naked_number Naked_int32 -> C.sign_extend_32
        | Naked_number
            (Naked_float | Naked_immediate | Naked_int64 | Naked_nativeint)
        | Value | Rec_info | Region ->
          fun _dbg cmm -> cmm)
      | _ ->
        (* CR gbury: update when unboxed tuples are used *)
        Misc.fatal_errorf
          "C functions are currently limited to a single return value"
    in
    let ty_args =
      List.map C.exttype_of_kind (Flambda_arity.to_list param_arity)
    in
    ( wrap dbg
        (C.extcall ~dbg ~alloc ~is_c_builtin ~returns ~ty_args callee ty args),
      env,
      Ece.all )
  | Call_kind.Method { kind; obj; alloc_mode } ->
    fail_if_probe apply;
    let obj, env, _ = C.simple ~dbg env obj in
    let kind = Call_kind.Method_kind.to_lambda kind in
    let alloc_mode = Alloc_mode.to_lambda alloc_mode in
    C.send kind callee obj args (pos, alloc_mode) dbg, env, Ece.all

(* Function calls that have an exn continuation with extra arguments must be
   wrapped with assignments for the mutable variables used to pass the extra
   arguments. *)
(* CR mshinwell: Add first-class support in Cmm for the concept of an exception
   handler with extra arguments. *)
let translate_apply env apply =
  let call, env, effs = translate_apply0 env apply in
  let dbg = Apply.dbg apply in
  let k_exn = Apply.exn_continuation apply in
  let mut_vars =
    Exn_continuation.exn_handler k_exn |> Env.get_exn_extra_args env
  in
  let extra_args = Exn_continuation.extra_args k_exn in
  if List.compare_lengths extra_args mut_vars = 0
  then
    let aux (call, env) (arg, _k) v =
      let arg, env, _ = C.simple ~dbg env arg in
      C.sequence (C.assign v arg) call, env
    in
    let call, env = List.fold_left2 aux (call, env) extra_args mut_vars in
    call, env, effs
  else
    Misc.fatal_errorf
      "Length of [extra_args] in exception continuation %a@ does not match \
       those in the environment (%a)@ for application expression:@ %a"
      Exn_continuation.print k_exn
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Ident.print)
      mut_vars Apply.print apply

(* Helpers for translating [Apply_cont] expressions *)

(* Exception continuations always receive the exception value in their first
   argument. Additionally, they may have extra arguments that are passed to the
   handler via mutable variables (expected to be spilled to the stack). *)
let translate_raise env apply exn_handler args =
  match args with
  | exn :: extra ->
    let raise_kind =
      match Apply_cont.trap_action apply with
      | Some (Pop { raise_kind; _ }) ->
        Trap_action.Raise_kind.option_to_lambda raise_kind
      | Some (Push _) | None ->
        Misc.fatal_errorf
          "Apply_cont calls an exception handler without a Pop trap action:@ %a"
          Apply_cont.print apply
    in
    let dbg = Apply_cont.debuginfo apply in
    let exn, env, _ = C.simple ~dbg env exn in
    let extra, env, _ = C.simple_list ~dbg env extra in
    let mut_vars = Env.get_exn_extra_args env exn_handler in
    let wrap, _ = Env.flush_delayed_lets env in
    let cmm =
      List.fold_left2
        (fun expr arg v -> C.sequence (C.assign v arg) expr)
        (C.raise_prim raise_kind exn dbg)
        extra mut_vars
    in
    wrap cmm
  | [] ->
    Misc.fatal_errorf "Exception continuation %a has no arguments:@ \n%a"
      Continuation.print exn_handler Apply_cont.print apply

let translate_jump_to_continuation env res apply types cont args =
  if List.compare_lengths types args = 0
  then
    let trap_actions =
      match Apply_cont.trap_action apply with
      | None -> []
      | Some (Pop _) -> [Cmm.Pop]
      | Some (Push { exn_handler }) ->
        let cont = Env.get_cmm_continuation env exn_handler in
        [Cmm.Push cont]
    in
    let dbg = Apply_cont.debuginfo apply in
    let args, env, _ = C.simple_list ~dbg env args in
    let wrap, _ = Env.flush_delayed_lets env in
    wrap (C.cexit cont args trap_actions), res
  else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Printcmm.machtype)
      types Apply_cont.print apply

(* A call to the return continuation of the current block simply is the return
   value for the current block being translated. *)
let translate_jump_to_return_continuation env apply return_cont args =
  match args with
  | [return_value] -> (
    let dbg = Apply_cont.debuginfo apply in
    let return_value, env, _ = C.simple ~dbg env return_value in
    let wrap, _ = Env.flush_delayed_lets env in
    match Apply_cont.trap_action apply with
    | None -> wrap return_value
    | Some (Pop _) -> wrap (C.trap_return return_value [Cmm.Pop])
    | Some (Push _) ->
      Misc.fatal_errorf
        "Return continuation %a should not be applied with a Push trap action"
        Continuation.print return_cont)
  | _ ->
    (* CR gbury: add support using unboxed tuples *)
    Misc.fatal_errorf
      "Return continuation %a should be applied to a single argument in@\n\
       %a@\n\
       Multiple return values from functions are not yet supported"
      Continuation.print return_cont Apply_cont.print apply

(* The main set of translation functions for expressions *)

let rec expr env res e =
  match Expr.descr e with
  | Let e' -> let_expr env res e'
  | Let_cont e' -> let_cont env res e'
  | Apply e' -> apply_expr env res e'
  | Apply_cont e' -> apply_cont env res e'
  | Switch e' -> switch env res e'
  | Invalid { message } -> C.invalid res ~message

and let_expr0 env res let_expr (bound_pattern : Bound_pattern.t)
    ~num_normal_occurrences_of_bound_vars ~body =
  match bound_pattern, Let.defining_expr let_expr with
  | Singleton v, Simple s ->
    let v = Bound_var.var v in
    (* CR mshinwell: Try to get a proper [dbg] here (although the majority of
       these bindings should have been substituted out). *)
    let dbg = Debuginfo.none in
    let env =
      bind_var_to_simple ~dbg env v ~num_normal_occurrences_of_bound_vars s
    in
    expr env res body
  | Singleton v, Prim (p, dbg) ->
    let v = Bound_var.var v in
    let defining_expr, extra, env, res, effs =
      To_cmm_primitive.prim env res dbg p
    in
    let effects_and_coeffects_of_defining_expr =
      Ece.join effs (Flambda_primitive.effects_and_coeffects p)
    in
    let env =
      Env.bind_variable ?extra env v
        ~num_normal_occurrences_of_bound_vars:
          (Known num_normal_occurrences_of_bound_vars)
        ~effects_and_coeffects_of_defining_expr ~defining_expr
    in
    expr env res body
  | Set_of_closures bound_vars, Set_of_closures soc ->
    To_cmm_set_of_closures.let_dynamic_set_of_closures env res ~body ~bound_vars
      ~num_normal_occurrences_of_bound_vars soc ~translate_expr:expr
  | Static bound_static, Static_consts consts -> (
    let env, res, update_opt =
      To_cmm_static.static_consts env res
        ~params_and_body:
          (To_cmm_set_of_closures.params_and_body ~translate_expr:expr)
        bound_static consts
    in
    match update_opt with
    | None -> expr env res body
    | Some update ->
      let wrap, env = Env.flush_delayed_lets env in
      let body, res = expr env res body in
      wrap (C.sequence update body), res)
  | Singleton _, Rec_info _ -> expr env res body
  | Singleton _, (Set_of_closures _ | Static_consts _)
  | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
  | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
    Misc.fatal_errorf "Mismatch between pattern and defining expression:@ %a"
      Let.print let_expr

and let_expr env res let_expr =
  Let.pattern_match' let_expr
    ~f:(fun bound_pattern ~num_normal_occurrences_of_bound_vars ~body ->
      match Bound_pattern.name_mode bound_pattern with
      | Normal ->
        let_expr0 env res let_expr bound_pattern
          ~num_normal_occurrences_of_bound_vars ~body
      | Phantom -> expr env res body
      | In_types ->
        Misc.fatal_errorf "Cannot bind In_types variables in terms:@ %a"
          Let.print let_expr)

and let_cont env res (let_cont : Flambda.Let_cont.t) =
  match let_cont with
  | Non_recursive { handler; num_free_occurrences; is_applied_with_traps } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
        let handler = Non_recursive_let_cont_handler.handler handler in
        match
          To_cmm_effects.classify_continuation_handler k handler
            ~num_free_occurrences ~is_applied_with_traps
        with
        | May_inline -> let_cont_inlined env res k handler body
        | Regular -> let_cont_not_inlined env res k handler body)
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
        if Continuation_handlers.contains_exn_handler conts
        then
          Misc.fatal_errorf
            "Recursive continuation bindings cannot involve exception \
             handlers:@ %a"
            Let_cont.print let_cont;
        let_cont_rec env res conts body)

(* The bound continuation [k] will be inlined. *)
and let_cont_inlined env res k handler body =
  Continuation_handler.pattern_match' handler
    ~f:(fun handler_params ~num_normal_occurrences_of_params ~handler ->
      let env =
        Env.add_inline_cont env k ~handler_params
          ~handler_params_occurrences:num_normal_occurrences_of_params
          ~handler_body:handler
      in
      expr env res body)

and let_cont_not_inlined env res k handler body =
  (* The environment must be flushed to ensure that expressions are not
     duplicated into both the body and the handler. *)
  (* CR gbury: "split" the environment according to which variables the handler
     and the body uses, to allow for inlining to proceed within each
     expression. *)
  let wrap, env = Env.flush_delayed_lets env in
  let is_exn_handler = Continuation_handler.is_exn_handler handler in
  let vars, arity, handler, res = continuation_handler env res handler in
  let catch_id, env =
    Env.add_jump_cont env k ~param_types:(List.map snd vars)
  in
  let cmm, res =
    (* Exception continuations are translated specially -- these will be reached
       via the raising of exceptions, whereas other continuations are reached
       using a normal jump. *)
    if is_exn_handler
    then let_cont_exn_handler env res k body vars handler ~catch_id arity
    else
      let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
      let body, res = expr env res body in
      ( C.create_ccatch ~rec_flag:false ~body
          ~handlers:[C.handler ~dbg catch_id vars handler],
        res )
  in
  wrap cmm, res

(* Exception continuations are translated using delayed Ctrywith blocks. The
   exception handler parts of these blocks are identified by the [catch_id]s.

   Additionally, exception continuations can have extra args, which are passed
   through the try-with using mutable Cmm variables. Thus the exception handler
   must first read the contents of those extra args (eagerly, in order to
   minmize the lifetime of the mutable variables). *)
and let_cont_exn_handler env res k body vars handler ~catch_id arity =
  let exn_var, extra_params =
    match vars with
    | (v, _) :: rest -> v, rest
    | [] ->
      (* See comment on [translate_raise], above. *)
      Misc.fatal_errorf
        "Exception continuation %a should have at least one argument"
        Continuation.print k
  in
  let env_body, mut_vars = Env.add_exn_handler env k arity in
  let handler =
    (* Wrap the exn handler with reads of the mutable variables *)
    List.fold_left2
      (fun handler (mut_var, _) (extra_param, _) ->
        C.letin extra_param ~defining_expr:(C.var mut_var) ~body:handler)
      handler mut_vars extra_params
  in
  let body, res = expr env_body res body in
  let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
  let trywith =
    C.trywith ~dbg ~kind:(Delayed catch_id) ~body ~exn_var ~handler ()
  in
  (* Define and initialize the mutable Cmm variables for extra args *)
  let cmm =
    List.fold_left
      (fun cmm (mut_var, (kind : K.t)) ->
        (* CR mshinwell: Fix [provenance] *)
        let mut_var =
          Backend_var.With_provenance.create ?provenance:None mut_var
        in
        let dummy_value =
          match kind with
          | Value -> C.int ~dbg 1
          | Naked_number Naked_float -> C.float ~dbg 0.
          | Naked_number
              (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) ->
            C.int ~dbg 0
          | Region | Rec_info ->
            Misc.fatal_errorf "No dummy value available for kind %a" K.print
              kind
        in
        C.letin_mut mut_var (C.machtype_of_kind kind) dummy_value cmm)
      trywith mut_vars
  in
  cmm, res

and let_cont_rec env res conts body =
  (* Flush the env now to avoid inlining something inside of a recursive
     continuation (aka a loop), as it would increase the number of times the
     computation is performed (even if there is only one syntactic
     occurrence) *)
  (* CR-someday mshinwell: As discussed, the tradeoff here is not clear, since
     flushing might increase register pressure. *)
  let wrap, env = Env.flush_delayed_lets ~entering_loop:true env in
  (* Compute the environment for Ccatch ids *)
  let conts_to_handlers = Continuation_handlers.to_map conts in
  let env =
    Continuation.Map.fold
      (fun k handler acc ->
        let continuation_arg_tys =
          Continuation_handler.pattern_match' handler
            ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler:_ ->
              List.map C.machtype_of_kinded_parameter
                (Bound_parameters.to_list params))
        in
        snd (Env.add_jump_cont acc k ~param_types:continuation_arg_tys))
      conts_to_handlers env
  in
  (* Translate each continuation handler *)
  let conts_to_handlers, res =
    Continuation.Map.fold
      (fun k handler (conts_to_handlers, res) ->
        let vars, _arity, handler, res = continuation_handler env res handler in
        Continuation.Map.add k (vars, handler) conts_to_handlers, res)
      conts_to_handlers
      (Continuation.Map.empty, res)
  in
  let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
  (* Setup the Cmm handlers for the Ccatch *)
  let handlers =
    Continuation.Map.fold
      (fun k (vars, handler) acc ->
        let id = Env.get_cmm_continuation env k in
        C.handler ~dbg id vars handler :: acc)
      conts_to_handlers []
  in
  let body, res = expr env res body in
  wrap (C.create_ccatch ~rec_flag:true ~body ~handlers), res

and continuation_handler env res handler =
  Continuation_handler.pattern_match' handler
    ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler ->
      let arity = Bound_parameters.arity params in
      let env, vars = C.bound_parameters env params in
      let expr, res = expr env res handler in
      vars, arity, expr, res)

and apply_expr env res apply =
  let call, env, effs = translate_apply env apply in
  (* With respect to flushing the environment we have three cases:

     1. The call never returns or jumps to another function

     2. The call jumps to somewhere else in the current function, but there is
     more than one incoming control flow edge to that point

     3. The call jumps to somewhere else in the current function and is the only
     thing that jumps to that point. In this case we will inline the return
     continuation of the [Apply].

     In case 1 we can't affect the code generation at the jump target: there is
     no option but to flush now.

     In case 2 we also flush to ensure that effectful bindings are not pushed
     past join points and that no binding is duplicated.

     In case 3 we can avoid flushing the environment due to the linearity of the
     control flow. We know that flushing will eventually happen by virtue of the
     recursive call to [expr]. *)
  match Apply.continuation apply with
  | Never_returns ->
    (* Case 1 *)
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k when Continuation.equal (Env.return_continuation env) k ->
    (* Case 1 *)
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k -> (
    let[@inline always] unsupported () =
      (* CR gbury: add support using unboxed tuples *)
      Misc.fatal_errorf
        "Return continuation %a should be applied to a single argument in@\n\
         %a@\n\
         Multiple return values from functions are not yet supported"
        Continuation.print k Apply.print apply
    in
    match Env.get_continuation env k with
    | Jump { param_types = []; cont } ->
      (* Case 2 *)
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.sequence call (C.cexit cont [] [])), res
    | Jump { param_types = [_]; cont } ->
      (* Case 2 *)
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.cexit cont [call] []), res
    | Inline { handler_params; handler_body = body; handler_params_occurrences }
      ->
      (* Case 3 *)
      let handler_params = Bound_parameters.to_list handler_params in
      let var, num_normal_occurrences_of_bound_vars =
        match handler_params with
        | [] ->
          let var = Variable.create "*apply_res*" in
          var, Variable.Map.singleton var Num_occurrences.Zero
        | [param] -> Bound_parameter.var param, handler_params_occurrences
        | _ :: _ -> unsupported ()
      in
      let env =
        Env.bind_variable env var
          ~num_normal_occurrences_of_bound_vars:
            (Known num_normal_occurrences_of_bound_vars)
          ~effects_and_coeffects_of_defining_expr:effs ~defining_expr:call
      in
      expr env res body
    | Jump _ -> unsupported ())

and apply_cont env res apply_cont =
  let k = Apply_cont.continuation apply_cont in
  let args = Apply_cont.args apply_cont in
  if Env.is_exn_handler env k
  then translate_raise env apply_cont k args, res
  else if Continuation.equal (Env.return_continuation env) k
  then translate_jump_to_return_continuation env apply_cont k args, res
  else
    match Env.get_continuation env k with
    | Jump { param_types; cont } ->
      translate_jump_to_continuation env res apply_cont param_types cont args
    | Inline { handler_params; handler_body; handler_params_occurrences } ->
      if Option.is_some (Apply_cont.trap_action apply_cont)
      then
        Misc.fatal_errorf "This [Apply_cont] should not have a trap action:@ %a"
          Apply_cont.print apply_cont;
      (* Inlining a continuation call simply needs to bind the arguments to the
         variables that the continuation's handler expects. *)
      let handler_params = Bound_parameters.to_list handler_params in
      if List.compare_lengths args handler_params = 0
      then
        let env =
          List.fold_left2
            (fun env param ->
              bind_var_to_simple
                ~dbg:(Apply_cont.debuginfo apply_cont)
                env
                (Bound_parameter.var param)
                ~num_normal_occurrences_of_bound_vars:handler_params_occurrences)
            env handler_params args
        in
        expr env res handler_body
      else
        Misc.fatal_errorf
          "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
          Continuation.print k Apply_cont.print apply_cont
          (List.length handler_params)
          Apply_cont.print apply_cont

and switch env res switch =
  let scrutinee = Switch.scrutinee switch in
  let dbg = Switch.condition_dbg switch in
  let untagged_scrutinee_cmm, env, _ = C.simple ~dbg env scrutinee in
  let arms = Switch.arms switch in
  (* For binary switches, which can be translated to an if-then-else, it can be
     interesting for the scrutinee to be tagged (particularly for switches
     coming from a source level if-then-else on booleans) as that way the
     translation can use 2 instructions instead of 3.

     However, this is only useful to do if the tagged expression is smaller then
     the untagged one (which is not always true due to arithmetic
     simplifications performed by Cmm_helpers).

     Additionally for switches with more than 2 arms, not untagging and
     adjusting the switch to work on tagged integers might be worse. The
     discriminants of the arms might not be successive machine integers anymore,
     thus preventing the use of a table. Alternatively it might not be worth it
     given the already high number of instructions needed for big switches (but
     this might be debatable for small switches with 3 to 5 arms). *)
  let scrutinee, must_tag_discriminant =
    match Targetint_31_63.Map.cardinal arms with
    | 2 -> (
      match Env.extra_info env scrutinee with
      | None | Some Boxed_number -> untagged_scrutinee_cmm, false
      | Some (Untag tagged_scrutinee_cmm) ->
        let size_untagged =
          Option.value
            (C.cmm_arith_size untagged_scrutinee_cmm)
            ~default:max_int
        in
        let size_tagged =
          Option.value (C.cmm_arith_size tagged_scrutinee_cmm) ~default:max_int
        in
        if size_tagged < size_untagged
        then tagged_scrutinee_cmm, true
        else untagged_scrutinee_cmm, false)
    | _ -> untagged_scrutinee_cmm, false
  in
  let wrap, env = Env.flush_delayed_lets env in
  let prepare_discriminant ~must_tag d =
    let targetint_d = Targetint_31_63.to_targetint d in
    Targetint_32_64.to_int_checked
      (if must_tag then C.tag_targetint targetint_d else targetint_d)
  in
  let make_arm ~must_tag_discriminant env res (d, action) =
    let d = prepare_discriminant ~must_tag:must_tag_discriminant d in
    let cmm_action, res = apply_cont env res action in
    (d, cmm_action, Apply_cont.debuginfo action), res
  in
  match Targetint_31_63.Map.cardinal arms with
  (* Binary case: if-then-else *)
  | 2 -> (
    let aux = make_arm ~must_tag_discriminant env in
    let first_arm, res = aux res (Targetint_31_63.Map.min_binding arms) in
    let second_arm, res = aux res (Targetint_31_63.Map.max_binding arms) in
    match first_arm, second_arm with
    (* These switches are actually if-then-elses. On such switches,
       transl_switch_clambda will introduce a let-binding of the scrutinee
       before creating an if-then-else, introducing an indirection that might
       prevent some optimizations performed by Selectgen/Emit when the condition
       is inlined in the if-then-else. Instead we use [C.ite]. *)
    | (0, else_, else_dbg), (_, then_, then_dbg)
    | (_, then_, then_dbg), (0, else_, else_dbg) ->
      wrap (C.ite ~dbg scrutinee ~then_dbg ~then_ ~else_dbg ~else_), res
    (* Similar case to the previous but none of the arms match 0, so we have to
       generate an equality test, and make sure it is inside the condition to
       ensure Selectgen and Emit can take advantage of it. *)
    | (x, if_x, if_x_dbg), (_, if_not, if_not_dbg) ->
      let expr =
        C.ite ~dbg
          (C.eq ~dbg (C.int ~dbg x) scrutinee)
          ~then_dbg:if_x_dbg ~then_:if_x ~else_dbg:if_not_dbg ~else_:if_not
      in
      wrap expr, res)
  (* General case *)
  | n ->
    (* transl_switch_clambda expects an [index] array such that index.(d) is the
       index in [cases] of the expression to execute when [e] matches [d]. *)
    let max_d, _ = Targetint_31_63.Map.max_binding arms in
    let m = prepare_discriminant ~must_tag:must_tag_discriminant max_d in
    let unreachable, res = C.invalid res ~message:"unreachable switch case" in
    let cases = Array.make (n + 1) unreachable in
    let index = Array.make (m + 1) n in
    let _, res =
      Targetint_31_63.Map.fold
        (fun discriminant action (i, res) ->
          let (d, cmm_action, _dbg), res =
            make_arm ~must_tag_discriminant env res (discriminant, action)
          in
          cases.(i) <- cmm_action;
          index.(d) <- i;
          i + 1, res)
        arms (0, res)
    in
    (* CR-someday poechsel: Put a more precise value kind here *)
    let expr =
      C.transl_switch_clambda dbg (Vval Pgenval) scrutinee index cases
    in
    wrap expr, res
