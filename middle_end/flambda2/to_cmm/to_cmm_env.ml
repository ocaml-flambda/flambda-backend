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

(* Continuation use. A continuation can be translated one of two ways:

   - by a static jump (Cmm jump, using a unique integer)

   - by inlining the continuation's body at the call site. *)

type cont =
  | Jump of
      { types : Cmm.machtype list;
        cont : int
      }
  | Inline of
      { handler_params : Kinded_parameter.t list;
        handler_body : Flambda.Expr.t;
        handler_params_occurrences : Num_occurrences.t Variable.Map.t
      }

(* Extra information about bound variables. These extra information help keep
   track of some extra semantics that are useful to implement some optimization
   in the translation to cmm. *)

type extra_info = Untag of Cmm.expression

(* Delayed let-bindings. Let bindings are delayed in stages in order to allow
   for potential reordering and inlining of variables that are bound and used
   exactly once, (without changing semantics), in order to optimize the
   generated cmm code. There are two main optimizations that are targeted :
   arithmetic optimization of nested expressions (mainly tagging/untagging), and
   potential optimizations performed later on function applications which work
   better when arguments are not let-bound. Non-linear let bindings are also
   delayed to allow linear let-bound vars to be permuted with non-linear
   let-bound vars.

   Let-bound variables can be one of three kinds: pure, coeffect and effect
   (effectful variables can also have coeffects). Each binding is given an
   id/order which are strictly increasing, in order to be able to get back the
   chronological defintion order of bindings.

   Pure variables are put in a map, given that they can commute with everything.
   Effectful and coeffectful variables, are organised into stages. A stage is a
   set of (non-pure) bindings that can all commute with each other.

   Concretely, a stage is either:

   - a series of consecutive bindings with only coeffects

   - a single effectful binding

   Whenever a new binding that doesn't match the current stage is added, the
   current stage is archived, and replaced by a new stage.

   Only bindings in the current stage, or in the map of pure bindings are
   candidates to inlining. When inlined, a binding is removed from its stage (as
   only linear bindings are supposed to be inlined), and if the current stage
   becomes empty, the last archived stage is "un-archived". *)

type kind =
  | Pure
  | Effect
  | Coeffect

type binding =
  { order : int;
    inline : bool;
    effs : Effects_and_coeffects.t;
    cmm_var : Backend_var.With_provenance.t;
    cmm_expr : Cmm.expression
  }

type stage =
  | Eff of Variable.t * binding
  | Coeff of binding Variable.Map.t

(* Translation environment *)

type t =
  { (* Global information. Those are computed once and valid for a whole unit.*)
    offsets : Exported_offsets.t;
    (* Offsets for closure_ids and var_within_closures. *)
    used_closure_vars : Var_within_closure.Set.t Or_unknown.t;
    (* Closure variables that are used by the context begin translated. (used to
       remove unused closure variables). *)
    functions_info : Exported_code.t;
    (* Information about known functions *)
    (* Semi-global information.

       Those are relative to the unit being translated, and are dependant on the
       scope inside the unit being translated. *)
    names_in_scope : Code_id_or_symbol.Set.t;
    (* Code ids and symbols bound in this scope, for invariant checking *)
    deleted : Code_id.Set.t;
    used_code_ids : Code_id.Set.t;
    (* Code ids marked as deleted are only allowed in the newer_version_of field
       of code definitions.

       Due to the order in which the checks are made, it is possible that a code
       id is checked before we know whether it is deleted or not, so the
       used_code_ids records all code ids that were checked.*)
    (* Local information.

       These are relative to the flambda expression being currently translated,
       i.e. either the unit initialization code, or the body of a function.

       Thus they are reset when entering a new function. *)
    k_return : Continuation.t;
    (* The continuation of the current context (used to determine which calls
       are tail-calls) *)
    k_exn : Continuation.t;
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

let mk offsets functions_info k_return ~exn_continuation:k_exn
    ~used_closure_vars =
  { k_return;
    k_exn;
    used_closure_vars;
    offsets;
    functions_info;
    stages = [];
    pures = Variable.Map.empty;
    vars = Variable.Map.empty;
    vars_extra = Variable.Map.empty;
    conts = Continuation.Map.empty;
    exn_handlers = Continuation.Set.singleton k_exn;
    exn_conts_extra_args = Continuation.Map.empty;
    names_in_scope = Code_id_or_symbol.Set.empty;
    deleted = Code_id.Set.empty;
    used_code_ids = Code_id.Set.empty
  }

let enter_function_def env k_return k_exn =
  { (* global info *)
    offsets = env.offsets;
    used_closure_vars = env.used_closure_vars;
    (* semi-global info *)
    names_in_scope = env.names_in_scope;
    functions_info = env.functions_info;
    deleted = env.deleted;
    used_code_ids = env.used_code_ids;
    (* local info *)
    k_return;
    k_exn;
    exn_handlers = Continuation.Set.singleton k_exn;
    stages = [];
    pures = Variable.Map.empty;
    vars = Variable.Map.empty;
    vars_extra = Variable.Map.empty;
    conts = Continuation.Map.empty;
    exn_conts_extra_args = Continuation.Map.empty
  }

let return_cont env = env.k_return

let exn_cont env = env.k_exn

(* Function info *)

let get_function_info env code_id =
  Exported_code.find_calling_convention env.functions_info code_id

let get_func_decl_params_arity t code_id =
  let info = get_function_info t code_id in
  let l = Exported_code.Calling_convention.params_arity info in
  if Exported_code.Calling_convention.is_tupled info
  then ~-(List.length l)
  else List.length l

(* Variables *)

let gen_variable v =
  let name = Variable.unique_name v in
  let v = Backend_var.create_local name in
  let v = Backend_var.With_provenance.create v in
  v

let add_variable env v v' =
  let v'' = Backend_var.With_provenance.var v' in
  let vars = Variable.Map.add v (To_cmm_helper.var v'') env.vars in
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

let get_variable env v =
  try Variable.Map.find v env.vars
  with Not_found ->
    Misc.fatal_errorf "Variable %a not found in env" Variable.print v

let extra_info env v =
  try Some (Variable.Map.find v env.vars_extra) with Not_found -> None

(* Continuations *)

let get_jump_id env k =
  match Continuation.Map.find k env.conts with
  | Jump { cont; _ } -> cont
  | Inline _ | (exception Not_found) ->
    Misc.fatal_errorf "Continuation %a not found in env" Continuation.print k

let get_k env k =
  match Continuation.Map.find k env.conts with
  | exception Not_found ->
    Misc.fatal_errorf "Could not find continuation %a in env during to_cmm"
      Continuation.print k
  | res -> res

let new_jump_id = Lambda.next_raise_count

let add_jump_cont env types k =
  let cont = new_jump_id () in
  let conts = Continuation.Map.add k (Jump { types; cont }) env.conts in
  cont, { env with conts }

let add_inline_cont env k vars ~handler_params_occurrences e =
  let info =
    Inline
      { handler_params = vars; handler_body = e; handler_params_occurrences }
  in
  let conts = Continuation.Map.add k info env.conts in
  { env with conts }

let add_exn_handler env k arity =
  let env =
    { env with exn_handlers = Continuation.Set.add k env.exn_handlers }
  in
  match arity with
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

(* Offsets *)

let closure_offset env closure =
  Exported_offsets.closure_offset env.offsets closure

let env_var_offset env env_var =
  Exported_offsets.env_var_offset env.offsets env_var

let layout env closures env_vars =
  To_cmm_closure.layout env.offsets closures env_vars

(* Printing

   let print_binding fmt b = Format.fprintf fmt "@[<hv>[%a : %a ->@ %a@
   (%a)@,]@]" Variable.print b.var Backend_var.With_provenance.print b.cmm_var
   Printcmm.expression b.cmm_expr Effects_and_coeffects.print b.effs

   let print_binding_list fmt l = Format.fprintf fmt "@[<v>"; List.iter (fun b
   -> Format.fprintf fmt "%a@," print_binding b ) l; Format.fprintf fmt "@]" *)

(* Variable binding (for potential inlining) *)

let next_order =
  let r = ref 0 in
  fun () ->
    incr r;
    !r

let classify effs =
  match (effs : Effects_and_coeffects.t) with
  (* For the purpose of to_cmm, generative effects, i.e. allocations, will be
     considered to have effects because the mutable state of the gc that
     allocations actually effect can be observed by coeffects performed by
     function calls (particularly coming from the Gc module). *)
  | Arbitrary_effects, (Has_coeffects | No_coeffects)
  | Only_generative_effects _, (Has_coeffects | No_coeffects) ->
    Effect
  (* Coeffects without any effect. These expression can commute with other
     coeffectful expressions (and pure expressions), but cannot commut with an
     effectful expression. *)
  | No_effects, Has_coeffects -> Coeffect
  (* Pure expressions: these can be commuted with *everything*, including
     effectful expressions such as function calls. *)
  | No_effects, No_coeffects -> Pure

let mk_binding ?extra env inline effs var cmm_expr =
  let order = next_order () in
  let cmm_var = gen_variable var in
  let b = { order; inline; effs; cmm_var; cmm_expr } in
  let v = Backend_var.With_provenance.var cmm_var in
  let e = To_cmm_helper.var v in
  let env = { env with vars = Variable.Map.add var e env.vars } in
  let env =
    match extra with
    | None -> env
    | Some info ->
      { env with vars_extra = Variable.Map.add var info env.vars_extra }
  in
  env, b

let bind_pure env var b = { env with pures = Variable.Map.add var b env.pures }

let bind_eff env var b = { env with stages = Eff (var, b) :: env.stages }

let bind_coeff env var b =
  match env.stages with
  | Coeff m :: r ->
    let m' = Variable.Map.add var b m in
    { env with stages = Coeff m' :: r }
  | ([] as r) | (Eff _ :: _ as r) ->
    let m = Variable.Map.singleton var b in
    { env with stages = Coeff m :: r }

let bind_variable env var ?extra effs inline cmm_expr =
  let env, b = mk_binding ?extra env inline effs var cmm_expr in
  match classify effs with
  | Pure -> bind_pure env var b
  | Effect -> bind_eff env var b
  | Coeffect -> bind_coeff env var b

(* Variable lookup (for potential inlining) *)

let inline_res env b = b.cmm_expr, env, b.effs

let inline_not env b =
  let v' = Backend_var.With_provenance.var b.cmm_var in
  To_cmm_helper.var v', env, Effects_and_coeffects.pure

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
  else inline_not env b

let inline_found_eff env var v b r =
  if not (Variable.equal var v)
  then inline_not_found env var
  else if b.inline
  then
    let env = { env with stages = r } in
    inline_res env b
  else inline_not env b

let inline_found_coeff env var m r =
  match Variable.Map.find var m with
  | exception Not_found -> inline_not_found env var
  | b ->
    if b.inline
    then
      let m' = Variable.Map.remove var m in
      let env =
        if Variable.Map.is_empty m'
        then { env with stages = r }
        else { env with stages = Coeff m' :: r }
      in
      inline_res env b
    else inline_not env b

let inline_variable env var =
  match Variable.Map.find var env.pures with
  | b -> inline_found_pure env var b
  | exception Not_found -> begin
    match env.stages with
    | [] -> inline_not_found env var
    | Eff (v, b) :: r -> inline_found_eff env var v b r
    | Coeff m :: r -> inline_found_coeff env var m r
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
          | Eff (_, b) -> order_add b acc
          | Coeff m -> order_add_map m acc)
        order_map stages
    in
    M.fold
      (fun _ b acc -> To_cmm_helper.letin b.cmm_var b.cmm_expr acc)
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

(* Use and Scoping checks *)

let used_closure_vars t = t.used_closure_vars

let add_to_scope env names =
  { env with
    names_in_scope = Code_id_or_symbol.Set.union env.names_in_scope names
  }

let mark_code_id_as_deleted env code_id =
  if Code_id.Set.mem code_id env.used_code_ids
  then Misc.fatal_errorf "Use of deleted code id %a" Code_id.print code_id
  else { env with deleted = Code_id.Set.add code_id env.deleted }

let check_scope ~allow_deleted env code_id_or_symbol =
  let in_scope =
    Code_id_or_symbol.Set.mem code_id_or_symbol env.names_in_scope
  in
  let in_another_unit =
    not
      (Compilation_unit.equal
         (Code_id_or_symbol.compilation_unit code_id_or_symbol)
         (Compilation_unit.get_current_exn ()))
  in
  let updated_env =
    match (code_id_or_symbol : Code_id_or_symbol.t) with
    | Code_id code_id ->
      if allow_deleted
      then env
      else if Code_id.Set.mem code_id env.deleted
      then Misc.fatal_errorf "Use of deleted code id %a" Code_id.print code_id
      else
        { env with used_code_ids = Code_id.Set.add code_id env.used_code_ids }
    | Symbol _ -> env
  in
  if in_scope || in_another_unit
     || not (Flambda_features.Expert.code_id_and_symbol_scoping_checks ())
  then updated_env
  else
    Misc.fatal_errorf "Use out of scope of %a@.Known names:@.%a@."
      Code_id_or_symbol.print code_id_or_symbol Code_id_or_symbol.Set.print
      env.names_in_scope
