(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cmm_helpers

type cont =
  | Jump of
      { cont : Cmm.label;
        param_types : Cmm.machtype list
      }
  | Inline of
      { handler_params : Bound_parameters.t;
        handler_params_occurrences : Num_occurrences.t Variable.Map.t;
        handler_body : Flambda.Expr.t
      }

type extra_info =
  | Untag of Cmm.expression
  | Boxed_number

type binding =
  { order : int;
    inline : bool;
    effs : Effects_and_coeffects.t;
    cmm_var : Backend_var.With_provenance.t;
    cmm_expr : Cmm.expression
  }

type stage =
  | Effect of Variable.t * binding
  | Coeffect_only of binding Variable.Map.t

type t =
  { (* Global information. This is computed once and remains valid for a whole
       compilation unit. *)
    offsets : Exported_offsets.t;
    (* Offsets for function and value slots. *)
    functions_info : Exported_code.t;
    (* Code and metadata of functions. *)
    (* Local information.

       This is relative to the flambda expression being currently translated,
       i.e. either the unit initialization code, or the body of a function. This
       information is reset when entering a new function. *)
    return_continuation : Continuation.t;
    (* The continuation of the current context (used to determine which calls
       are tail-calls) *)
    exn_continuation : Continuation.t;
    (* The exception continuation of the current context (used to determine
       where to insert try-with blocks) *)
    vars : Cmm.expression Variable.Map.t;
    (* Map from flambda variables to cmm expressions *)
    vars_extra : extra_info Variable.Map.t;
    (* Map from flambda variables to extra info *)
    conts : cont Continuation.Map.t;
    (* Map from continuations to handlers (i.e variables bound by the
       continuation and expression of the continuation handler). *)
    exn_handlers : Continuation.Set.t;
    (* All continuations that act as exception handlers. *)
    exn_conts_extra_args : Backend_var.t list Continuation.Map.t;
    (* Mutable variables used for compiling extra arguments to exception
       handlers *)
    pures : binding Variable.Map.t;
    (* pure bindings that can be inlined across stages. *)
    stages : stage list (* archived stages, in reverse chronological order. *)
  }

let create offsets functions_info ~return_continuation ~exn_continuation =
  { return_continuation;
    exn_continuation;
    offsets;
    functions_info;
    stages = [];
    pures = Variable.Map.empty;
    vars = Variable.Map.empty;
    vars_extra = Variable.Map.empty;
    conts = Continuation.Map.empty;
    exn_handlers = Continuation.Set.singleton exn_continuation;
    exn_conts_extra_args = Continuation.Map.empty
  }

let enter_function_body env ~return_continuation ~exn_continuation =
  create env.offsets env.functions_info ~return_continuation ~exn_continuation

let return_continuation env = env.return_continuation

let exn_continuation env = env.exn_continuation

(* Code and closures *)

let get_code_metadata env code_id =
  match Exported_code.find_exn env.functions_info code_id with
  | code_or_metadata -> Code_or_metadata.code_metadata code_or_metadata
  | exception Not_found ->
    Misc.fatal_errorf "To_cmm_env.get_code_metadata: code ID %a not bound"
      Code_id.print code_id

let exported_offsets t = t.offsets

(* Variables *)

let gen_variable v =
  let name = Variable.unique_name v in
  let v = Backend_var.create_local name in
  let v = Backend_var.With_provenance.create v in
  v

let add_variable env v v' =
  let v'' = Backend_var.With_provenance.var v' in
  let vars = Variable.Map.add v (C.var v'') env.vars in
  { env with vars }

let create_variable env v =
  assert (not (Variable.Map.mem v env.vars));
  let v' = gen_variable v in
  let env = add_variable env v v' in
  env, v'

let create_variables env l =
  let env, l' =
    List.fold_left
      (fun (env, l) v ->
        let env', v' = create_variable env v in
        env', v' :: l)
      (env, []) l
  in
  env, List.rev l'

let extra_info env v =
  match Variable.Map.find v env.vars_extra with
  | extra_info -> Some extra_info
  | exception Not_found -> None

(* Continuations *)

let get_cmm_continuation env k =
  match Continuation.Map.find k env.conts with
  | Jump { cont; _ } -> cont
  | Inline _ ->
    Misc.fatal_errorf "Continuation %a is registered for inlining, not a jump"
      Continuation.print k
  | exception Not_found ->
    Misc.fatal_errorf "Continuation %a not found in env" Continuation.print k

let get_continuation env k =
  match Continuation.Map.find k env.conts with
  | exception Not_found ->
    Misc.fatal_errorf "Could not find continuation %a in env during to_cmm"
      Continuation.print k
  | res -> res

let new_cmm_continuation = Lambda.next_raise_count

let add_jump_cont env k ~param_types =
  let cont = new_cmm_continuation () in
  let conts = Continuation.Map.add k (Jump { param_types; cont }) env.conts in
  cont, { env with conts }

let add_inline_cont env k ~handler_params ~handler_params_occurrences
    ~handler_body =
  let info =
    Inline { handler_params; handler_body; handler_params_occurrences }
  in
  let conts = Continuation.Map.add k info env.conts in
  { env with conts }

let add_exn_handler env k arity =
  let env =
    { env with exn_handlers = Continuation.Set.add k env.exn_handlers }
  in
  match Flambda_arity.to_list arity with
  | [] -> Misc.fatal_error "Exception handler with no arguments"
  | [_] -> env, []
  | _ :: extra_args ->
    let mut_vars =
      List.map
        (fun kind -> Backend_var.create_local "exn_extra_arg", kind)
        extra_args
    in
    let vars_only = List.map fst mut_vars in
    ( { env with
        exn_conts_extra_args =
          Continuation.Map.add k vars_only env.exn_conts_extra_args
      },
      mut_vars )

let is_exn_handler t cont = Continuation.Set.mem cont t.exn_handlers

let get_exn_extra_args env k =
  match Continuation.Map.find_opt k env.exn_conts_extra_args with
  | Some l -> l
  | None -> []

(* Variable binding (for potential inlining) *)

let is_inlinable_box effs ~extra =
  (* [effs] is the effects and coeffects of some primitive operation, arising
     either from the primitive itself or its arguments. If this is a boxing
     operation (as indicated by [extra]), then we want to inline the box, but
     this involves moving the arguments, so they must be pure (or at most
     generative). *)
  match (effs : Effects_and_coeffects.t), (extra : extra_info option) with
  | ((No_effects | Only_generative_effects _), No_coeffects), Some Boxed_number
    ->
    true
  | ( ( (No_effects | Only_generative_effects _ | Arbitrary_effects),
        (No_coeffects | Has_coeffects) ),
      (None | Some Boxed_number | Some (Untag _)) ) ->
    false

let create_binding =
  let next_order = ref (-1) in
  fun ?extra env inline effs var cmm_expr ->
    let order =
      incr next_order;
      !next_order
    in
    let cmm_var = gen_variable var in
    let binding = { order; inline; effs; cmm_var; cmm_expr } in
    let cmm_expr = C.var (Backend_var.With_provenance.var cmm_var) in
    let env = { env with vars = Variable.Map.add var cmm_expr env.vars } in
    let env =
      match extra with
      | None -> env
      | Some info ->
        { env with vars_extra = Variable.Map.add var info env.vars_extra }
    in
    env, binding

let bind_variable env var ?extra effs inline cmm_expr =
  let env, binding = create_binding ?extra env inline effs var cmm_expr in
  if inline && is_inlinable_box effs ~extra
  then
    (* CR-someday lmaurer: This violates our rule about not moving allocations
       past function calls. We should either fix it (not clear how) or be rid of
       that rule. *)
    { env with pures = Variable.Map.add var binding env.pures }
  else
    match To_cmm_effects.classify_by_effects_and_coeffects effs with
    | Pure -> { env with pures = Variable.Map.add var binding env.pures }
    | Effect -> { env with stages = Effect (var, binding) :: env.stages }
    | Coeffect_only ->
      let stages =
        match env.stages with
        | Coeffect_only bindings :: stages ->
          (* Multiple coeffect-only bindings may be accumulated in the same
             stage. *)
          Coeffect_only (Variable.Map.add var binding bindings) :: stages
        | [] | Effect _ :: _ ->
          Coeffect_only (Variable.Map.singleton var binding) :: env.stages
      in
      { env with stages }

(* Variable lookup (for potential inlining) *)

let inline_res env b = b.cmm_expr, env, b.effs

let non_inlined_var env b =
  let v' = Backend_var.With_provenance.var b.cmm_var in
  C.var v', env, Effects_and_coeffects.pure

let inline_not_found env v =
  match Variable.Map.find v env.vars with
  | exception Not_found ->
    Misc.fatal_errorf "Variable %a not found in env" Variable.print v
  | e -> e, env, Effects_and_coeffects.pure

let inline_found_pure env var b =
  if b.inline
  then
    let pures = Variable.Map.remove var env.pures in
    let env = { env with pures } in
    inline_res env b
  else non_inlined_var env b

let inline_found_effect env var v b r =
  if not (Variable.equal var v)
  then inline_not_found env var
  else if b.inline
  then
    let env = { env with stages = r } in
    inline_res env b
  else non_inlined_var env b

let inline_found_coeffect_only env var m r =
  match Variable.Map.find var m with
  | exception Not_found -> inline_not_found env var
  | b ->
    if b.inline
    then
      let m' = Variable.Map.remove var m in
      let env =
        if Variable.Map.is_empty m'
        then { env with stages = r }
        else { env with stages = Coeffect_only m' :: r }
      in
      inline_res env b
    else non_inlined_var env b

let inline_variable env var =
  match Variable.Map.find var env.pures with
  | b -> inline_found_pure env var b
  | exception Not_found -> begin
    match env.stages with
    | [] -> inline_not_found env var
    | Effect (v, b) :: r -> inline_found_effect env var v b r
    | Coeffect_only m :: r -> inline_found_coeffect_only env var m r
  end

(* Flushing delayed bindings *)

(* Map on integers in descending order *)
module M = Map.Make (struct
  type t = int

  let compare x y = compare y x
end)

let order_add b acc = M.add b.order b acc

let order_add_map m acc =
  Variable.Map.fold (fun _ b acc -> order_add b acc) m acc

let flush_delayed_lets ?(entering_loop = false) env =
  (* generate a wrapper function to introduce the delayed let-bindings. *)
  let wrap_aux pures stages e =
    let order_map = order_add_map pures M.empty in
    let order_map =
      List.fold_left
        (fun acc -> function
          | Effect (_, b) -> order_add b acc
          | Coeffect_only m -> order_add_map m acc)
        order_map stages
    in
    M.fold
      (fun _ b acc ->
        Cmm_helpers.letin b.cmm_var ~defining_expr:b.cmm_expr ~body:acc)
      order_map e
  in
  (* Unless entering a loop, only pure bindings that are not to be inlined are
     flushed now. The remainder are preserved, ensuring that the corresponding
     expressions are sunk down as far as possible. *)
  (* CR-someday mshinwell: work out a criterion for allowing substitutions into
     loops. *)
  let pures_to_keep, pures_to_flush =
    if entering_loop
    then Variable.Map.empty, env.pures
    else Variable.Map.partition (fun _ binding -> binding.inline) env.pures
  in
  let wrap e = wrap_aux pures_to_flush env.stages e in
  wrap, { env with stages = []; pures = pures_to_keep }
