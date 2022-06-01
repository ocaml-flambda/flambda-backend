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

(* Notes:

   - an int64 on a 32-bit host is represented across two registers, hence most
   operations on them will actually need to call C primitive that can handle
   them.

   - int32 on 64 bits are represented as an int64 in the range of 32-bit
   integers. Thus we insert sign extensions after every operation on 32-bits
   integers that may have a result outside of the range. *)

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

(* Shortcuts for useful cmm machtypes *)

let typ_val = Cmm.typ_val

let typ_void = Cmm.typ_void

let check_arity arity args =
  Flambda_arity.With_subkinds.cardinal arity = List.length args

(* CR gbury: {Targetint_32_64.to_int} should raise an error when converting an
   out-of-range integer. *)
let int_of_targetint t =
  let i = Targetint_32_64.to_int t in
  let t' = Targetint_32_64.of_int i in
  if not (Targetint_32_64.equal t t')
  then Misc.fatal_errorf "Cannot translate targetint to caml int";
  i

let default_of_kind ~dbg (k : Flambda_kind.t) =
  match k with
  | Value -> C.int ~dbg 1
  | Naked_number Naked_immediate -> C.int ~dbg 0
  | Naked_number Naked_float -> C.float ~dbg 0.
  | Naked_number Naked_int32 -> C.int ~dbg 0
  | Naked_number Naked_int64 when Target_system.is_32_bit ->
    C.unsupported_32_bit ()
  | Naked_number Naked_int64 -> C.int ~dbg 0
  | Naked_number Naked_nativeint -> C.int ~dbg 0
  | Region -> Misc.fatal_error "Region_kind have no default value"
  | Rec_info -> Misc.fatal_error "Rec_info has no default value"

(* Function symbol *)

let function_name simple =
  let fail simple =
    Misc.fatal_errorf "Expected a function symbol, instead of@ %a" Simple.print
      simple
  in
  Simple.pattern_match simple
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun _ ~coercion:_ -> fail simple)
        ~symbol:(fun sym ~coercion:_ ->
          Symbol.linkage_name sym |> Linkage_name.to_string))
    ~const:(fun _ -> fail simple)

let machtype_of_return_arity arity =
  (* Functions that never return have arity 0. In that case, we use the most
     restrictive machtype to ensure that the return value of the function is not
     used. *)
  match Flambda_arity.to_list arity with
  | [] -> typ_void
  (* Regular functions with a single return value *)
  | [k] -> C.machtype_of_kind k
  | _ ->
    (* CR gbury: update when unboxed tuples are used *)
    Misc.fatal_errorf "Functions are currently limited to a single return value"

let meth_kind k =
  match (k : Call_kind.method_kind) with
  | Self -> (Self : Lambda.meth_kind)
  | Public -> (Public : Lambda.meth_kind)
  | Cached -> (Cached : Lambda.meth_kind)

let apply_returns (e : Apply.t) =
  match Apply.continuation e with Return _ -> true | Never_returns -> false

let wrap_extcall_result arity =
  match Flambda_arity.to_list arity with
  (* Int32 need to be sign_extended because it's not clear whether C code that
     returns an int32 returns one that is sign extended or not *)
  | [Naked_number Naked_int32] -> C.sign_extend_32
  (* No need to wrap other return arities.

     Note that extcall of arity 0 are allowed (these are extcalls that never
     return, such as caml_ml_array_bound_error) *)
  | [] | [_] -> fun _dbg cmm -> cmm
  | _ ->
    (* CR gbury: update when unboxed tuples are used *)
    Misc.fatal_errorf
      "C functions are currently limited to a single return value"

(* Helpers for exception continuations *)

let split_exn_cont_args k = function
  | (v, _) :: rest -> v, rest
  | [] ->
    Misc.fatal_errorf
      "Exception continuation %a should have at least one argument"
      Continuation.print k

(* Small function to estimate the number of arithmetic instructions in a cmm
   expression. This is currently used to determine whether untagging an
   expression resulted in a smaller expression or not (as can happen because of
   some arithmetic simplifications performed by cmm_helpers.ml) *)
let rec cmm_arith_size e =
  match (e : Cmm.expression) with
  | Cop
      ( ( Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor
        | Clsl | Clsr | Casr ),
        l,
        _ ) ->
    List.fold_left ( + ) 1 (List.map cmm_arith_size l)
  | _ -> 0

let match_var_with_extra_info env simple : Env.extra_info option =
  Simple.pattern_match simple
    ~const:(fun _ -> None)
    ~name:(fun n ~coercion:_ ->
      Name.pattern_match n
        ~symbol:(fun _ -> None)
        ~var:(fun var -> Env.extra_info env var))

(* Helper for the translation of [Simple]s. *)

let bind_simple ~dbg env r v ~num_normal_occurrences_of_bound_vars s =
  let defining_expr, env, effects_and_coeffects_of_defining_expr =
    C.simple ~dbg env r s
  in
  Env.bind_variable env v
    ~num_normal_occurrences_of_bound_vars:
      (Known num_normal_occurrences_of_bound_vars)
    ~effects_and_coeffects_of_defining_expr ~defining_expr

(* Helpers for the translation of [Apply] expressions. *)

let apply_call env r e =
  let f = Apply.callee e in
  let args = Apply.args e in
  let dbg = Apply.dbg e in
  let effs = Ece.all in
  let fail_if_probe apply =
    match Apply.probe_name apply with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf
        "[Apply] terms with a [probe_name] (i.e. that call a tracing probe) \
         must always be direct applications of an OCaml function:@ %a"
        Apply.print apply
  in
  match Apply.call_kind e with
  (* Effects from arguments are ignored since a function call will always be
     given arbitrary effects and coeffects. *)
  | Function
      { function_call = Direct { code_id; return_arity }; alloc_mode = _ } -> (
    let info = Env.get_code_metadata env code_id in
    let params_arity = Code_metadata.params_arity info in
    if not (check_arity params_arity args)
    then Misc.fatal_errorf "Wrong arity for direct call";
    let ty =
      return_arity |> Flambda_arity.With_subkinds.to_arity
      |> machtype_of_return_arity
    in
    let args, env, _ = C.simple_list ~dbg env r args in
    let args, env =
      if Code_metadata.is_my_closure_used info
      then
        let f, env, _ = C.simple ~dbg env r f in
        args @ [f], env
      else args, env
    in
    let code_linkage_name = Code_id.linkage_name code_id in
    match Apply.probe_name e with
    | None ->
      ( C.direct_call ~dbg ty
          (C.symbol_from_linkage_name ~dbg code_linkage_name)
          args,
        env,
        effs )
    | Some name ->
      ( C.probe ~dbg ~name
          ~handler_code_linkage_name:(Linkage_name.to_string code_linkage_name)
          ~args,
        env,
        effs ))
  | Function { function_call = Indirect_unknown_arity; alloc_mode } ->
    fail_if_probe e;
    let f, env, _ = C.simple ~dbg env r f in
    let args, env, _ = C.simple_list ~dbg env r args in
    ( C.indirect_call ~dbg typ_val (Alloc_mode.to_lambda alloc_mode) f args,
      env,
      effs )
  | Function
      { function_call = Indirect_known_arity { return_arity; param_arity };
        alloc_mode
      } ->
    fail_if_probe e;
    if not (check_arity param_arity args)
    then
      Misc.fatal_errorf
        "To_cmm expects indirect_known_arity calls to be full applications in \
         order to translate it"
    else
      let f, env, _ = C.simple ~dbg env r f in
      let args, env, _ = C.simple_list ~dbg env r args in
      let ty =
        return_arity |> Flambda_arity.With_subkinds.to_arity
        |> machtype_of_return_arity
      in
      ( C.indirect_full_call ~dbg ty (Alloc_mode.to_lambda alloc_mode) f args,
        env,
        effs )
  | Call_kind.C_call { alloc; return_arity; param_arity; is_c_builtin } ->
    fail_if_probe e;
    let f = function_name f in
    (* CR vlaviron: temporary hack to recover the right symbol *)
    let len = String.length f in
    assert (len >= 9);
    assert (String.sub f 0 9 = ".extern__");
    let f = String.sub f 9 (len - 9) in
    let returns = apply_returns e in
    let args, env, _ = C.simple_list ~dbg env r args in
    let ty = machtype_of_return_arity return_arity in
    let wrap = wrap_extcall_result return_arity in
    let ty_args =
      List.map C.exttype_of_kind (Flambda_arity.to_list param_arity)
    in
    ( wrap dbg (C.extcall ~dbg ~alloc ~is_c_builtin ~returns ~ty_args f ty args),
      env,
      effs )
  | Call_kind.Method { kind; obj; alloc_mode } ->
    fail_if_probe e;
    let obj, env, _ = C.simple ~dbg env r obj in
    let meth, env, _ = C.simple ~dbg env r f in
    let kind = meth_kind kind in
    let args, env, _ = C.simple_list ~dbg env r args in
    let alloc_mode = Alloc_mode.to_lambda alloc_mode in
    C.send kind meth obj args (Rc_normal, alloc_mode) dbg, env, effs

(* Function calls that have an exn continuation with extra arguments must be
   wrapped with assignments for the mutable variables used to pass the extra
   arguments. *)
(* CR mshinwell: Add first-class support in Cmm for the concept of an exception
   handler with extra arguments. *)
let wrap_call_exn ~dbg env r e call k_exn =
  let h_exn = Exn_continuation.exn_handler k_exn in
  let mut_vars = Env.get_exn_extra_args env h_exn in
  let extra_args = Exn_continuation.extra_args k_exn in
  if List.compare_lengths extra_args mut_vars = 0
  then
    let aux (call, env) (arg, _k) v =
      let arg, env, _ = C.simple ~dbg env r arg in
      C.sequence (C.assign v arg) call, env
    in
    List.fold_left2 aux (call, env) extra_args mut_vars
  else
    Misc.fatal_errorf
      "Length of [extra_args] in exception continuation %a@ does not match \
       those in the environment (%a)@ for application expression:@ %a"
      Exn_continuation.print k_exn
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Ident.print)
      mut_vars Apply.print e

(* Helpers for translating [Apply_cont] expressions *)

(* Exception continuations always raise their first argument (which is supposed
   to be an exception). Additionally, they may have extra arguments that are
   passed to the handler via mutables variables (which are expected to be
   spilled on the stack). *)
let apply_cont_exn env r e k = function
  | exn :: extra ->
    let raise_kind =
      match Apply_cont.trap_action e with
      | Some (Pop { raise_kind; _ }) ->
        Trap_action.Raise_kind.option_to_lambda raise_kind
      | _ ->
        Misc.fatal_errorf
          "Apply cont %a calls an exception cont without a Pop trap action"
          Apply_cont.print e
    in
    let dbg = Apply_cont.debuginfo e in
    let exn, env, _ = C.simple ~dbg env r exn in
    let extra, env, _ = C.simple_list ~dbg env r extra in
    let mut_vars = Env.get_exn_extra_args env k in
    let wrap, _ = Env.flush_delayed_lets env in
    let cmm = C.raise_prim raise_kind exn (Apply_cont.debuginfo e) in
    let cmm =
      List.fold_left2
        (fun expr arg v -> C.sequence (C.assign v arg) expr)
        cmm extra mut_vars
    in
    wrap cmm
  | [] ->
    Misc.fatal_errorf "Exception continuation %a has no arguments in@\n%a"
      Continuation.print k Apply_cont.print e

let apply_cont_trap_actions env e =
  match Apply_cont.trap_action e with
  | None -> []
  | Some (Pop _) -> [Cmm.Pop]
  | Some (Push { exn_handler }) ->
    let cont = Env.get_cmm_continuation env exn_handler in
    [Cmm.Push cont]

(* Continuation calls need to also translate the associated trap actions. *)
let apply_cont_jump env res e types cont args =
  if List.compare_lengths types args = 0
  then
    let trap_actions = apply_cont_trap_actions env e in
    let dbg = Apply_cont.debuginfo e in
    let args, env, _ = C.simple_list ~dbg env res args in
    let wrap, _ = Env.flush_delayed_lets env in
    wrap (C.cexit cont args trap_actions), res
  else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Printcmm.machtype)
      types Apply_cont.print e

(* A call to the return continuation of the current block simply is the return
   value for the current block being translated. *)
let apply_cont_ret env res e k = function
  | [ret] -> (
    let dbg = Apply_cont.debuginfo e in
    let ret, env, _ = C.simple ~dbg env res ret in
    let wrap, _ = Env.flush_delayed_lets env in
    match Apply_cont.trap_action e with
    | None -> wrap ret
    | Some (Pop _) -> wrap (C.trap_return ret [Cmm.Pop])
    | Some (Push _) ->
      Misc.fatal_errorf
        "Continuation %a (return cont) should not be applied with a push trap \
         action"
        Continuation.print k)
  | _ ->
    (* CR gbury: add support using unboxed tuples *)
    Misc.fatal_errorf
      "Continuation %a (return cont) should be applied to a single argument in@\n\
       %a@\n\
       %s"
      Continuation.print k Apply_cont.print e
      "Multi-arguments continuation across function calls are not yet supported"

(* The main set of translation functions for expressions *)

let rec expr env res e =
  match Expr.descr e with
  | Let e' -> let_expr env res e'
  | Let_cont e' -> let_cont env res e'
  | Apply e' -> apply_expr env res e'
  | Apply_cont e' -> apply_cont env res e'
  | Switch e' -> switch env res e'
  | Invalid { message } -> C.invalid res ~message

and let_expr env res let_expr =
  Let.pattern_match' let_expr
    ~f:(fun bound_pattern ~num_normal_occurrences_of_bound_vars ~body ->
      match Bound_pattern.name_mode bound_pattern with
      | Normal -> (
        match bound_pattern, Let.defining_expr let_expr with
        | Singleton v, Simple s ->
          let v = Bound_var.var v in
          (* CR mshinwell: Try to get a proper [dbg] here (although the majority
             of these bindings should have been substituted out). *)
          let dbg = Debuginfo.none in
          let env =
            bind_simple ~dbg env res v ~num_normal_occurrences_of_bound_vars s
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
          To_cmm_set_of_closures.let_dynamic_set_of_closures env res ~body
            ~bound_vars ~num_normal_occurrences_of_bound_vars soc
            ~translate_expr:expr
        | Static bound_static, Static_consts consts -> (
          let env, res, update_opt =
            To_cmm_static.static_consts env res
              ~params_and_body:
                (To_cmm_set_of_closures.params_and_body ~translate_expr:expr)
              bound_static consts
          in
          match update_opt with
          | None ->
            expr env res body
            (* trying to preserve tail calls whenever we can *)
          | Some update ->
            let wrap, env = Env.flush_delayed_lets env in
            let body, res = expr env res body in
            wrap (C.sequence update body), res)
        | Singleton _, Rec_info _ -> expr env res body
        | Singleton _, (Set_of_closures _ | Static_consts _)
        | Set_of_closures _, (Simple _ | Prim _ | Static_consts _ | Rec_info _)
        | Static _, (Simple _ | Prim _ | Set_of_closures _ | Rec_info _) ->
          Misc.fatal_errorf
            "Mismatch between pattern and defining expression:@ %a" Let.print
            let_expr)
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
        | May_inline -> let_cont_inline env res k handler body
        | Regular -> let_cont_jump env res k handler body)
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
        assert (not (Continuation_handlers.contains_exn_handler conts));
        let_cont_rec env res conts body)

(* The bound continuation [k] will be inlined. *)
and let_cont_inline env res k h body =
  Continuation_handler.pattern_match' h
    ~f:(fun handler_params ~num_normal_occurrences_of_params ~handler ->
      let env =
        Env.add_inline_cont env k ~handler_params
          ~handler_params_occurrences:num_normal_occurrences_of_params
          ~handler_body:handler
      in
      expr env res body)

(* Continuations that are not inlined are translated using a jump:

   - exceptions continuations use "dynamic" jumps using the raise/trywith cmm
   mechanism

   - regular continuations use static jumps, through the exit/catch cmm
   mechanism *)
(* CR Gbury: "split" the environment according to which variables the handler
   and the body uses, to allow for inlining to proceed within each
   expression. *)
and let_cont_jump env res k h body =
  let wrap, env = Env.flush_delayed_lets env in
  let vars, arity, handle, res = continuation_handler env res h in
  let id, env = Env.add_jump_cont env k ~param_types:(List.map snd vars) in
  if Continuation_handler.is_exn_handler h
  then
    let body, res = let_cont_exn env res k body vars handle id arity in
    wrap body, res
  else
    let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
    let body, res = expr env res body in
    ( wrap
        (C.create_ccatch ~rec_flag:false ~body
           ~handlers:[C.handler ~dbg id vars handle]),
      res )

(* Exception continuations, translated using delayed trywith blocks.

   Additionally, exn continuations in flambda can have extra args, which are
   passed through the trywith using mutable cmm variables. Thus the exn handler
   must first read the contents of thos extra args (eagerly in order to minmize
   the lifetime of the mutable variables) *)
and let_cont_exn env res k body vars handle id arity =
  let exn_var, extra_params = split_exn_cont_args k vars in
  let env_body, extra_vars = Env.add_exn_handler env k arity in
  let handler =
    (* wrap the exn handler with reads of the mutable variables *)
    List.fold_left2
      (fun handler (v, _) (p, _) ->
        C.letin p ~defining_expr:(C.var v) ~body:handler)
      handle extra_vars extra_params
  in
  let body, res = expr env_body res body in
  let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
  let trywith = C.trywith ~dbg ~kind:(Delayed id) ~body ~exn_var ~handler () in
  (* define and initialize the mutable cmm variables used by an exn extra
     args *)
  let cmm =
    List.fold_left
      (fun cmm (v, k) ->
        (* CR mshinwell: Fix [provenance] *)
        let v = Backend_var.With_provenance.create ?provenance:None v in
        C.letin_mut v (C.machtype_of_kind k) (default_of_kind ~dbg k) cmm)
      trywith extra_vars
  in
  cmm, res

and let_cont_rec env res conts body =
  (* Flush the env before anything to avoid inlining something inside of a
     recursive cont (aka a loop), as it would increase the number of times the
     computation is performed (even if there is only one syntactic
     occurrence) *)
  let wrap, env = Env.flush_delayed_lets ~entering_loop:true env in
  (* Compute the environment for jump ids *)
  let map = Continuation_handlers.to_map conts in
  let env =
    Continuation.Map.fold
      (fun k h acc ->
        let continuation_arg_tys =
          Continuation_handler.pattern_match' h
            ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler:_ ->
              List.map C.machtype_of_kinded_parameter
                (Bound_parameters.to_list params))
        in
        snd (Env.add_jump_cont acc k ~param_types:continuation_arg_tys))
      map env
  in
  (* Translate each continuation handler *)
  let map, res =
    Continuation.Map.fold
      (fun k h (map, res) ->
        let vars, _arity, handler, res = continuation_handler env res h in
        Continuation.Map.add k (vars, handler) map, res)
      map
      (Continuation.Map.empty, res)
  in
  let dbg = Debuginfo.none (* CR mshinwell: fix debuginfo *) in
  (* Setup the cmm handlers for the static catch *)
  let handlers =
    Continuation.Map.fold
      (fun k (vars, handle) acc ->
        let id = Env.get_cmm_continuation env k in
        C.handler ~dbg id vars handle :: acc)
      map []
  in
  let body, res = expr env res body in
  wrap (C.create_ccatch ~rec_flag:true ~body ~handlers), res

and continuation_handler env res h =
  Continuation_handler.pattern_match' h
    ~f:(fun params ~num_normal_occurrences_of_params:_ ~handler ->
      let arity = Bound_parameters.arity params in
      let env, vars = C.bound_parameters env params in
      let e, res = expr env res handler in
      vars, arity, e, res)

(* Function calls: besides the function calls, there are a few things to do:

   - setup the mutable variables for the exn cont extra args if needed

   - translate the call continuation (either through a jump, or inlining). *)
and apply_expr env res e =
  let call, env, effs = apply_call env res e in
  let k_exn = Apply.exn_continuation e in
  let dbg = Apply.dbg e in
  let call, env = wrap_call_exn ~dbg env res e call k_exn in
  match Apply.continuation e with
  | Never_returns ->
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k when Continuation.equal (Env.return_continuation env) k ->
    let wrap, _ = Env.flush_delayed_lets env in
    wrap call, res
  | Return k -> (
    let[@inline always] unsupported () =
      (* CR gbury: add support using unboxed tuples *)
      Misc.fatal_errorf
        "Continuation %a should not handle multiple return values in@\n%a@\n%s"
        Continuation.print k Apply.print e
        "Multi-arguments continuation across function calls are not yet \
         supported"
    in
    match Env.get_continuation env k with
    | Jump { param_types = []; cont } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.sequence call (C.cexit cont [] [])), res
    | Jump { param_types = [_]; cont } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.cexit cont [call] []), res
    | Inline { handler_params; handler_body = body; handler_params_occurrences }
      -> (
      let handler_params = Bound_parameters.to_list handler_params in
      match handler_params with
      | [] ->
        let var = Variable.create "*apply_res*" in
        let num_normal_occurrences_of_bound_vars =
          Variable.Map.singleton var Num_occurrences.Zero
        in
        let env =
          Env.bind_variable env var
            ~num_normal_occurrences_of_bound_vars:
              (Known num_normal_occurrences_of_bound_vars)
            ~effects_and_coeffects_of_defining_expr:effs ~defining_expr:call
        in
        expr env res body
      | [param] ->
        let var = Bound_parameter.var param in
        let env =
          Env.bind_variable env var
            ~num_normal_occurrences_of_bound_vars:
              (Known handler_params_occurrences)
            ~effects_and_coeffects_of_defining_expr:effs ~defining_expr:call
        in
        expr env res body
      | _ :: _ -> unsupported ())
    | Jump _ -> unsupported ())

and apply_cont env res e =
  let k = Apply_cont.continuation e in
  let args = Apply_cont.args e in
  if Env.is_exn_handler env k
  then apply_cont_exn env res e k args, res
  else if Continuation.equal (Env.return_continuation env) k
  then apply_cont_ret env res e k args, res
  else
    match Env.get_continuation env k with
    | Jump { param_types; cont } ->
      apply_cont_jump env res e param_types cont args
    | Inline { handler_params; handler_body; handler_params_occurrences } ->
      (* CR mshinwell: We should fix this. See comment in apply_cont_expr.ml *)
      if not (Apply_cont.trap_action e = None)
      then
        Misc.fatal_errorf "This [Apply_cont] should not have a trap action:@ %a"
          Apply_cont.print e;
      (* Inlining a continuation call simply needs to bind the arguments to the
         variables that the continuation's body expects. The delayed lets in the
         environment enables that translation to be tail-rec. *)
      let handler_params = Bound_parameters.to_list handler_params in
      if List.compare_lengths args handler_params = 0
      then
        let env =
          List.fold_left2
            (fun env param ->
              bind_simple ~dbg:(Apply_cont.debuginfo e) env res
                (Bound_parameter.var param)
                ~num_normal_occurrences_of_bound_vars:handler_params_occurrences)
            env handler_params args
        in
        expr env res handler_body
      else
        Misc.fatal_errorf
          "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
          Continuation.print k Apply_cont.print e
          (List.length handler_params)
          Apply_cont.print e

and switch env res s =
  let scrutinee = Switch.scrutinee s in
  let dbg = Switch.condition_dbg s in
  let e, env, _ = C.simple ~dbg env res scrutinee in
  let arms = Switch.arms s in
  (* For binary switches, which can be translated to an if-then-else, it can be
     interesting to *not* untag the scrutinee (particularly for those coming
     from a source if-then-else on booleans) as that way the translation can use
     2 instructions instead of 3.

     However, this is only useful to do if the tagged expression is smaller then
     the untagged one (which is not always true due to arithmetic
     simplifications performed by cmm_helpers).

     Additionally for switches with more than 2 arms, not untagging and lifting
     the switch to perform on tagged integer might be worse (because the
     discriminant of the arms may not be successive anymore, thus preventing the
     use of a table), or simply not worth it given the already high number of
     instructions needed for big switches (but that might be up-to-debate on
     small switches with 3-5 arms). *)
  let scrutinee, tag_discriminant =
    match Targetint_31_63.Map.cardinal arms with
    | 2 -> begin
      match match_var_with_extra_info env scrutinee with
      | None | Some Boxed_number -> e, false
      | Some (Untag e') ->
        let size_e = cmm_arith_size e in
        let size_e' = cmm_arith_size e' in
        if size_e' < size_e then e', true else e, false
    end
    | _ -> e, false
  in
  let wrap, env = Env.flush_delayed_lets env in
  let prepare_discriminant ~tag d =
    let targetint_d = Targetint_31_63.to_targetint' d in
    let prepared_d = if tag then C.tag_targetint targetint_d else targetint_d in
    int_of_targetint prepared_d
  in
  let make_arm ~tag_discriminant env res (d, action) =
    let d = prepare_discriminant ~tag:tag_discriminant d in
    let cmm_action, res = apply_cont env res action in
    (d, cmm_action, Apply_cont.debuginfo action), res
  in
  match Targetint_31_63.Map.cardinal arms with
  (* Binary case: if-then-else *)
  | 2 -> (
    let aux = make_arm ~tag_discriminant env in
    let first_arm, res = aux res (Targetint_31_63.Map.min_binding arms) in
    let second_arm, res = aux res (Targetint_31_63.Map.max_binding arms) in
    match first_arm, second_arm with
    (* These switchs are actually if-then-elses. On such switches,
       transl_switch_clambda will introduce a let-binding to the scrutinee
       before creating an if-then-else, introducing an indirection that might
       prevent some optimizations performed by selectgen/emit when the condition
       is inlined in the if-then-else. *)
    | (0, else_, else_dbg), (_, then_, then_dbg)
    | (_, then_, then_dbg), (0, else_, else_dbg) ->
      wrap (C.ite ~dbg scrutinee ~then_dbg ~then_ ~else_dbg ~else_), res
    (* Similar case to the if/then/else but none of the arms match 0, so we have
       to generate an equality test, and make sure it is inside the condition to
       ensure selectgen and emit can take advantage of it. *)
    | (x, if_x, if_x_dbg), (_, if_not, if_not_dbg) ->
      ( wrap
          (C.ite ~dbg
             (C.eq ~dbg (C.int ~dbg x) scrutinee)
             ~then_dbg:if_x_dbg ~then_:if_x ~else_dbg:if_not_dbg ~else_:if_not),
        res ))
  (* General case *)
  | n ->
    (* The transl_switch_clambda expects an index array such that index.(d) is
       the index in [cases] of the expression to execute when [e] matches
       [d]. *)
    let max_d, _ = Targetint_31_63.Map.max_binding arms in
    let m = prepare_discriminant ~tag:tag_discriminant max_d in
    let unreachable, res = C.invalid res ~message:"unreachable switch case" in
    let cases = Array.make (n + 1) unreachable in
    let index = Array.make (m + 1) n in
    let _, res =
      Targetint_31_63.Map.fold
        (fun discriminant action (i, res) ->
          let (d, cmm_action, _dbg), res =
            make_arm ~tag_discriminant env res (discriminant, action)
          in
          cases.(i) <- cmm_action;
          index.(d) <- i;
          i + 1, res)
        arms (0, res)
    in
    (* CR-someday poechsel: Put a more precise value kind here *)
    (* The cases of the switch must have a value kind (i.e. they cannot be
       unboxed). This is because each case will either end in a `Cexit` or end
       by "calling" the return continuation, in which case it will just return
       the argument to that continuation. Currently functions cannot return
       unboxed values, so that argument must have a value kind. *)
    ( wrap
        (C.transl_switch_clambda Debuginfo.none (Vval Pgenval) scrutinee index
           cases),
      res )
