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

module C = Cmm_helpers
module R = To_cmm_result
module P = Flambda_primitive
module Ece = Effects_and_coeffects

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

type extra_info = Untag of Cmm.expression

(* Since to_cmm_primitive.ml depends on this file, and in this file, we need to
   translate delayed/split primitives, we need to have access to the translation
   primitive from to_cmm_primitive.ml, and we'll get them through this
   record. *)

type prim_res = extra_info option * R.t * Cmm.expression

type ('env, 'prim, 'arity) prim_helper =
  'env -> R.t -> Debuginfo.t -> 'prim -> 'arity

type 'env trans_prim =
  { nullary : ('env, P.nullary_primitive, prim_res) prim_helper;
    unary : ('env, P.unary_primitive, Cmm.expression -> prim_res) prim_helper;
    binary :
      ( 'env,
        P.binary_primitive,
        Cmm.expression -> Cmm.expression -> prim_res )
      prim_helper;
    ternary :
      ( 'env,
        P.ternary_primitive,
        Cmm.expression -> Cmm.expression -> Cmm.expression -> prim_res )
      prim_helper;
    variadic :
      ('env, P.variadic_primitive, Cmm.expression list -> prim_res) prim_helper
  }

(* Delayed let-bindings (see the .mli) *)

(* the binding kinds *)
type simple = Simple

type complex = Complex

type _ inline =
  | Do_not_inline : simple inline
  | May_inline_once : simple inline
  | Must_inline_once : complex inline
  | Must_inline_and_duplicate : complex inline

(* Note on the effects of splittable bindings:

   The arguments are stored with their effects. This means that if we need to
   split the binding, we can re-bind each argument with its correct effects. *)
type _ bound_expr =
  | Simple : { cmm_expr : Cmm.expression } -> simple bound_expr
  | Split : { cmm_expr : Cmm.expression } -> complex bound_expr
  | Splittable_prim :
      { dbg : Debuginfo.t;
        prim : Flambda_primitive.Without_args.t;
        args : (Cmm.expression * Ece.t) list
      }
      -> complex bound_expr

type 'kind binding =
  { order : int;
    effs : Ece.t;
    inline : 'kind inline;
    bound_expr : 'kind bound_expr;
    cmm_var : Backend_var.With_provenance.t
  }

type any_binding = Binding : _ binding -> any_binding [@@unboxed]

type stage =
  | Effect of Variable.t
  | Coeffect_only of Variable.Set.t

type t =
  { (* Global information.

       This is computed once and remains valid for a whole compilation unit. *)
    trans_prim : t trans_prim;
    (* Primitive translation functions. *)
    offsets : Exported_offsets.t;
    (* Offsets for function and value slots. *)
    functions_info : Exported_code.t;
    (* Code and metadata of functions. *)
    (* Local information.

       This is relative to the flambda expression being currently translated,
       i.e. either the unit initialization code, or the body of a function. This
       information is reset when entering a new function. *)
    return_continuation : Continuation.t;
    (* The (non-exceptional) return continuation of the current context (used to
       determine which calls are tail-calls). *)
    exn_continuation : Continuation.t;
    (* The exception continuation of the current context (used to determine
       where to insert try-with blocks). *)
    conts : cont Continuation.Map.t;
    (* Information about whether each continuation in scope should have its
       handler inlined, or else reached via a jump. *)
    exn_handlers : Continuation.Set.t;
    (* All continuations that act as exception handlers. *)
    exn_conts_extra_args : Backend_var.t list Continuation.Map.t;
    (* Mutable variables used for compiling the "extra arguments" to exception
       handlers. *)
    vars_extra : extra_info Variable.Map.t;
    (* Extra information associated with Flambda variables. *)
    vars : Cmm.expression Variable.Map.t;
    (* Cmm expressions (of the form [Cvar ...]) for all bound variables in
       scope. *)
    bindings : any_binding Variable.Map.t;
    (* All bindings currently in env. *)
    stages : stage list (* Stages of let-bindings, most recent at the head. *)
  }

let create offsets functions_info ~trans_prim ~return_continuation
    ~exn_continuation =
  { return_continuation;
    exn_continuation;
    offsets;
    functions_info;
    trans_prim;
    stages = [];
    bindings = Variable.Map.empty;
    vars_extra = Variable.Map.empty;
    vars = Variable.Map.empty;
    conts = Continuation.Map.empty;
    exn_handlers = Continuation.Set.singleton exn_continuation;
    exn_conts_extra_args = Continuation.Map.empty
  }

let enter_function_body env ~return_continuation ~exn_continuation =
  create env.offsets env.functions_info ~trans_prim:env.trans_prim
    ~return_continuation ~exn_continuation

let return_continuation env = env.return_continuation

let exn_continuation env = env.exn_continuation

let print_extra_info ppf = function
  | Untag e -> Format.fprintf ppf "Untag(%a)" Printcmm.expression e

let [@ocamlformat "disable"] print_inline (type a) ppf (inline : a inline) =
  match inline with
  | Do_not_inline -> Format.fprintf ppf "do_not_inline"
  | May_inline_once -> Format.fprintf ppf "may_inline_once"
  | Must_inline_once -> Format.fprintf ppf "must_inline_once"
  | Must_inline_and_duplicate -> Format.fprintf ppf "must_inline_and_duplicate"

let [@ocamlformat "disable"] print_bound_expr (type a) ppf (b : a bound_expr) =
  match b with
  | Simple { cmm_expr; } | Split { cmm_expr; } ->
    Printcmm.expression ppf cmm_expr
  | Splittable_prim { prim; args; dbg; } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(dbg@ %a)@]@ \
      @[<hov 1>(prim@ %a)@]@ \
      @[<hov 1>(args@ @[<hov 1>(%a)@])@]\
      )@]"
      Debuginfo.print_compact dbg
      Flambda_primitive.Without_args.print prim
      (Format.pp_print_list (fun ppf (cmm, _) -> Printcmm.expression ppf cmm)) args

let [@ocamlformat "disable"] print_binding (type a) ppf
    ({ order; inline; effs; cmm_var; bound_expr; } : a binding) =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(order@ %d)@]@ \
      @[<hov 1>(inline@ %a)@]@ \
      @[<hov 1>(effs@ %a)@]@ \
      @[<hov 1>(var@ %a)@]@ \
      @[<hov 1>(expr@ %a)@]\
      )@]"
    order
    print_inline inline
    Ece.print effs
    Backend_var.With_provenance.print cmm_var
    print_bound_expr bound_expr

let print_any_binding ppf (Binding binding) = print_binding ppf binding

let print_stage ppf = function
  | Effect v -> Format.fprintf ppf "(Effect %a)" Variable.print v
  | Coeffect_only s ->
    Format.fprintf ppf "(Coeffect_only %a)" Variable.Set.print s

let print_stages ppf stages =
  let pp_sep ppf () = Format.fprintf ppf "@," in
  Format.fprintf ppf "(@[<v>%a@])"
    (Format.pp_print_list ~pp_sep print_stage)
    stages

let print ppf t =
  Format.fprintf ppf "@[<hov 1>(@[<hov 1>(stages %a)@]@ )@]" print_stages
    t.stages

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
  (* CR mshinwell: Fix [provenance] *)
  Backend_var.With_provenance.create ?provenance:None v

let add_bound_param env v v' =
  let v'' = Backend_var.With_provenance.var v' in
  let vars = Variable.Map.add v (C.var v'') env.vars in
  { env with vars }

let create_bound_parameter env v =
  if Variable.Map.mem v env.vars
  then
    Misc.fatal_errorf "Cannot rebind variable %a in To_cmm environment"
      Variable.print v;
  let v' = gen_variable v in
  let env = add_bound_param env v v' in
  env, v'

let create_bound_parameters env vs =
  List.fold_left_map create_bound_parameter env vs

let extra_info env simple =
  match Simple.must_be_var simple with
  | None -> None
  | Some (var, _coercion) -> (
    match Variable.Map.find var env.vars_extra with
    | extra_info -> Some extra_info
    | exception Not_found -> None)

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
  | [] -> Misc.fatal_error "Exception handlers must have at least one parameter"
  | [_] -> env, []
  | _ :: extra_args ->
    let mut_vars =
      List.map
        (fun kind -> Backend_var.create_local "exn_extra_arg", kind)
        extra_args
    in
    let vars_only = List.map fst mut_vars in
    let exn_conts_extra_args =
      Continuation.Map.add k vars_only env.exn_conts_extra_args
    in
    { env with exn_conts_extra_args }, mut_vars

let is_exn_handler t cont = Continuation.Set.mem cont t.exn_handlers

let get_exn_extra_args env k =
  match Continuation.Map.find k env.exn_conts_extra_args with
  | exception Not_found -> []
  | extra_args -> extra_args

(* Variable binding (for potential inlining). Also see [To_cmm_effects]. *)

let next_order = ref (-1)

let simple cmm_expr = Simple { cmm_expr }

let complex_no_split cmm_expr = Split { cmm_expr }

let splittable_primitive dbg prim args = Splittable_prim { dbg; prim; args }

let is_cmm_simple cmm =
  match (cmm : Cmm.expression) with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _ | Cvar _
    ->
    true
  | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _ | Ctuple _ | Cop _
  | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _
  | Cregion _ | Ctail _ ->
    false

let create_binding_aux (type a) ?extra env effs var ~(inline : a inline)
    (bound_expr : a bound_expr) =
  let order =
    let incr =
      match bound_expr with
      | Simple _ | Split _ -> 1
      | Splittable_prim { args; _ } -> List.length args + 1
    in
    next_order := !next_order + incr;
    !next_order
  in
  let cmm_var = gen_variable var in
  let binding = Binding { order; inline; effs; cmm_var; bound_expr } in
  let bindings = Variable.Map.add var binding env.bindings in
  let cmm_expr = C.var (Backend_var.With_provenance.var cmm_var) in
  let vars = Variable.Map.add var cmm_expr env.vars in
  let vars_extra =
    match extra with
    | None -> env.vars_extra
    | Some info -> Variable.Map.add var info env.vars_extra
  in
  let env = { env with bindings; vars; vars_extra } in
  env, binding

let create_binding (type a) ?extra env effs var ~(inline : a inline)
    (bound_expr : a bound_expr) =
  (* In order to avoid generating binding of the form: "let x = y in ...", when
     'y' is trivial i.e. is a value that fits in a register, we mark 'x' as a
     must_inline_and_duplicate (since it basically replaces a variable by either
     another variable, a constant, or a symbol). *)
  match bound_expr with
  | (Split { cmm_expr } | Simple { cmm_expr }) when is_cmm_simple cmm_expr ->
    create_binding_aux ?extra env effs var ~inline:Must_inline_and_duplicate
      (Split { cmm_expr })
  | Simple _ | Split _ | Splittable_prim _ ->
    create_binding_aux ?extra env effs var ~inline bound_expr

let bind_variable_with_decision (type a) ?extra env var ~inline
    ~(defining_expr : a bound_expr) ~effects_and_coeffects_of_defining_expr:effs
    =
  (* CR gbury: we actually need to "lie" about the effects and coeffects of
     allocations that we must inline, or else we end up with expressions with
     effects that will not be inlined (see [to_cmm_primitive.ml] and in
     particular ~consider_inllining_effectful_expressions). For instance,
     consider: *)
  (*
   * let x = box_number 1. in
   * let y = unbox_number x in
   * let z = y +. 1. in
   * ...
   *)
  (* because we only ever join effects when translating primitives, and that we
     only use the effects and coeffects from the flambda code, we generate:

     - a binding "x -> box_number 1." with generative effects (as expected)

     - a binding "y -> 1." (because we inlined "x", and cmm_helper functions
     eliminated the unbox-of-box), but this binding has generative effects,
     because it effects are computed as : ece(prim:unbox_number) ∪ ece(x), and
     since "x" has generative effects, we end up with a binding for "y" that has
     generative effects.

     - when we translate the addition, we explicitly do not consider inlining
     effectful expressions, and thus we do not inline "y", since it has effects.

     To counteract that, we instead consider that generative effects arising
     from Delay/"must inline" primitives do not count, and that we consider the
     binding to be pure.

     CR gbury: this allows to move allocations marked as `Must_inline_once` past
     function calls (and other effectful expressions), which can break some
     allocation-counting tests (the same also applies to allocations marked as
     `Must_inline_and_duplicate` but that is more expected). *)
  let effs, classification =
    let classification =
      To_cmm_effects.classify_by_effects_and_coeffects effs
    in
    match[@ocaml.warning "-4"] (inline : a inline), classification with
    | (Must_inline_once | Must_inline_and_duplicate), Generative_immutable ->
      Ece.pure_can_be_duplicated, To_cmm_effects.Pure
    | _, _ -> effs, classification
  in
  let env, binding = create_binding ?extra env ~inline effs var defining_expr in
  match (inline : a inline) with
  | Must_inline_and_duplicate ->
    (* check that the effects and coeffects allow the expression to be
       duplicated without changing semantics *)
    (match classification with
    | Pure -> ()
    | Generative_immutable | Coeffect_only | Effect ->
      Misc.fatal_errorf
        "Incorrect effects and/or coeffects for a duplicated binding: %a"
        print_any_binding binding);
    env
  | May_inline_once | Must_inline_once | Do_not_inline -> (
    match classification with
    | Pure -> env
    | Generative_immutable -> (
      match (inline : a inline) with
      | Must_inline_once -> env
      | May_inline_once | Do_not_inline ->
        (* Generative expressions not marked as `must_inline` are treated as
           having effects, since function from the `Gc` module can read counters
           that are increased by allocations. *)
        { env with stages = Effect var :: env.stages }
      | Must_inline_and_duplicate -> assert false (* impossible to reach *))
    | Effect -> { env with stages = Effect var :: env.stages }
    | Coeffect_only ->
      let stages =
        match env.stages with
        | Coeffect_only vars :: stages ->
          (* Multiple coeffect-only bindings may be accumulated in the same
             stage. *)
          Coeffect_only (Variable.Set.add var vars) :: stages
        | [] | Effect _ :: _ ->
          Coeffect_only (Variable.Set.singleton var) :: env.stages
      in
      { env with stages })

let bind_variable ?extra env var ~defining_expr
    ~num_normal_occurrences_of_bound_vars
    ~effects_and_coeffects_of_defining_expr =
  let inline =
    To_cmm_effects.classify_let_binding var
      ~effects_and_coeffects_of_defining_expr
      ~num_normal_occurrences_of_bound_vars
  in
  match inline with
  | Drop_defining_expr -> env
  | Regular ->
    let defining_expr = simple defining_expr in
    bind_variable_with_decision ?extra env var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Do_not_inline
  | May_inline_once ->
    let defining_expr = simple defining_expr in
    bind_variable_with_decision ?extra env var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:May_inline_once
  | Must_inline_once ->
    let defining_expr = complex_no_split defining_expr in
    bind_variable_with_decision ?extra env var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Must_inline_once
  | Must_inline_and_duplicate ->
    let defining_expr = complex_no_split defining_expr in
    bind_variable_with_decision ?extra env var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Must_inline_and_duplicate

let bind_variable_to_primitive = bind_variable_with_decision

(* Variable lookup (for potential inlining) *)

type split_result =
  | Already_split
  | Split of
      { new_bindings : any_binding list;
        split_binding : complex binding
      }

let new_bindings_for_splitting order args =
  let (new_bindings, _), new_cmm_args =
    List.fold_left_map
      (fun (new_bindings, order) (cmm_arg, arg_effs) ->
        if is_cmm_simple cmm_arg
        then (new_bindings, order), cmm_arg
        else
          (* we need to rebind the argument *)
          let new_cmm_var =
            Backend_var.With_provenance.create ?provenance:None
              (Backend_var.create_local (Format.asprintf "split_tmp_%d" order))
          in
          let binding =
            Binding
              { order;
                effs = arg_effs;
                inline = Do_not_inline;
                bound_expr = Simple { cmm_expr = cmm_arg };
                cmm_var = new_cmm_var
              }
          in
          ( (binding :: new_bindings, order - 1),
            C.var (Backend_var.With_provenance.var new_cmm_var) ))
      ([], order - 1)
      args
  in
  new_bindings, new_cmm_args

let rebuild_prim ~dbg ~env ~res prim args =
  let extra_info, res, cmm =
    match (prim, args : Flambda_primitive.Without_args.t * _) with
    | Nullary nullary, [] -> env.trans_prim.nullary env res dbg nullary
    | Unary unary, [x] -> env.trans_prim.unary env res dbg unary x
    | Binary binary, [x; y] -> env.trans_prim.binary env res dbg binary x y
    | Ternary ternary, [x; y; z] ->
      env.trans_prim.ternary env res dbg ternary x y z
    | Variadic variadic, args ->
      env.trans_prim.variadic env res dbg variadic args
    | (Nullary _ | Unary _ | Binary _ | Ternary _), _ ->
      Misc.fatal_errorf
        "Mismatched arity when splitting a binding in to_cmm_env:@\n%a@\n%a"
        Flambda_primitive.Without_args.print prim
        (Format.pp_print_list Printcmm.expression)
        args
  in
  (* CR gbury: this assert should currently hold, as 1) very few primitives
     actually generate an [extra_info], 2) very few primitives are marked as
     must_inline, and 3) these two do not overlap. However, we could relax that
     restriction in the future, and record the extra_info adequately. *)
  (match extra_info with
  | None -> ()
  | Some extra_info ->
    Misc.fatal_errorf
      "Unexpected extra_info in to_cmm_env during prim_rebuild:@\n\
       %a@ in@\n\
       %a(%a)[%a]"
      print_extra_info extra_info P.Without_args.print prim
      (Format.pp_print_list Printcmm.expression)
      args Debuginfo.print_compact dbg);
  cmm, res

let split_complex_binding ~env ~res (binding : complex binding) =
  match binding.bound_expr with
  | Split _ -> res, Already_split
  | Splittable_prim { dbg; prim; args } ->
    let new_bindings, new_cmm_args =
      new_bindings_for_splitting binding.order args
    in
    let new_cmm_expr, res = rebuild_prim ~dbg ~env ~res prim new_cmm_args in
    let prim_effects =
      Flambda_primitive.Without_args.effects_and_coeffects prim
    in
    (match To_cmm_effects.classify_by_effects_and_coeffects prim_effects with
    | Pure | Generative_immutable -> ()
    | Effect | Coeffect_only ->
      Misc.fatal_errorf
        "Primitive %a was marked as `must_inline`, but is has the following \
         effects and coeffects: %a. This would lead to errors when moving the \
         primitive application to substitute it."
        Flambda_primitive.Without_args.print prim Ece.print prim_effects);
    let split_binding =
      { order = binding.order;
        effs = prim_effects;
        inline = binding.inline;
        bound_expr = Split { cmm_expr = new_cmm_expr };
        cmm_var = binding.cmm_var
      }
    in
    res, Split { new_bindings; split_binding }

let remove_binding env var =
  { env with bindings = Variable.Map.remove var env.bindings }

let will_inline_simple env res { effs; bound_expr = Simple { cmm_expr }; _ } =
  cmm_expr, env, res, effs

let will_inline_complex env res { effs; bound_expr; _ } =
  match bound_expr with
  | Split { cmm_expr } -> cmm_expr, env, res, effs
  | Splittable_prim { dbg; prim; args } ->
    let cmm_expr, res = rebuild_prim ~dbg ~env ~res prim (List.map fst args) in
    cmm_expr, env, res, effs

let will_not_inline_simple env res { cmm_var; bound_expr = Simple _; _ } =
  ( C.var (Backend_var.With_provenance.var cmm_var),
    env,
    res,
    Ece.pure_can_be_duplicated )

let split_and_inline env res var binding =
  let res, split_result = split_complex_binding ~env ~res binding in
  match split_result with
  | Already_split -> will_inline_complex env res binding
  | Split { new_bindings; split_binding } ->
    let env =
      (* for duplicated bindings, we need to replace the original splittable
         binding with the new split binding in the bindings map of the env *)
      match split_binding.inline with
      | Must_inline_once -> env
      | Must_inline_and_duplicate ->
        { env with
          bindings = Variable.Map.add var (Binding split_binding) env.bindings
        }
    in
    let env =
      List.fold_left
        (fun env new_binding ->
          let flambda_var = Variable.create "to_cmm_tmp" in
          { env with
            bindings = Variable.Map.add flambda_var new_binding env.bindings
          })
        env new_bindings
    in
    will_inline_complex env res split_binding

let pop_if_in_top_stage ?consider_inlining_effectful_expressions env var =
  match env.stages with
  | [] -> None
  | Effect var_from_stage :: prev_stages ->
    (* In this case [var_from_stage] corresponds to an effectful binding forming
       the most recent stage. We also know that [var] doesn't have an available
       pure defining expression (either because that expression isn't pure, or
       because the corresponding binding has already been flushed). As such, we
       can't move the defining expression for [var] past that of
       [var_from_stage], in the case where these variables are different.
       However if these two variables are in fact the same, we can consider
       inlining the defining expression. *)
    let consider_inlining_effectful_expressions =
      match consider_inlining_effectful_expressions with
      | Some consider -> consider
      | None -> Flambda_features.Expert.inline_effects_in_cmm ()
    in
    if Variable.equal var var_from_stage
       && consider_inlining_effectful_expressions
    then Some { env with stages = prev_stages }
    else None
  | Coeffect_only vars_from_stage :: prev_stages ->
    (* Here we see if [var] has a coeffect-only defining expression on the most
       recent stage. If so, then we can commute it with any other expression on
       the stage, since they all only have coeffects. The defining expression
       for [var] may then be considered for inlining. *)
    if Variable.Set.mem var vars_from_stage
    then
      let new_vars_in_stage = Variable.Set.remove var vars_from_stage in
      let stages =
        if Variable.Set.is_empty new_vars_in_stage
        then prev_stages
        else Coeffect_only new_vars_in_stage :: prev_stages
      in
      Some { env with stages }
    else None

let inline_variable ?consider_inlining_effectful_expressions env res var =
  match Variable.Map.find var env.bindings with
  | exception Not_found -> (
    (* this happens for continuation parameters and bindings that have been
       flushed *)
    match Variable.Map.find var env.vars with
    | exception Not_found ->
      Misc.fatal_errorf "Variable %a not found in env" Variable.print var
    | e ->
      (* the env.vars map only contain bindings to expressions of the form
         [Cmm.Cvar _], hence the effects. *)
      e, env, res, Ece.pure_can_be_duplicated)
  | Binding binding -> (
    match binding.inline with
    | Do_not_inline -> will_not_inline_simple env res binding
    | Must_inline_and_duplicate -> split_and_inline env res var binding
    | Must_inline_once -> (
      let env = remove_binding env var in
      match To_cmm_effects.classify_by_effects_and_coeffects binding.effs with
      | Pure | Generative_immutable -> will_inline_complex env res binding
      | Effect | Coeffect_only -> (
        match
          pop_if_in_top_stage ?consider_inlining_effectful_expressions env var
        with
        | None -> split_and_inline env res var binding
        | Some env -> will_inline_complex env res binding))
    | May_inline_once -> (
      match To_cmm_effects.classify_by_effects_and_coeffects binding.effs with
      | Pure ->
        let env = remove_binding env var in
        will_inline_simple env res binding
      | Generative_immutable | Effect | Coeffect_only -> (
        match
          pop_if_in_top_stage ?consider_inlining_effectful_expressions env var
        with
        | None -> will_not_inline_simple env res binding
        | Some env ->
          let env = remove_binding env var in
          will_inline_simple env res binding)))

(* Flushing delayed bindings *)

(* Map on integers in descending order *)
module M = Map.Make (struct
  type t = int

  let compare x y = compare y x
end)

type flush_mode =
  | Entering_loop
  | Branching_point
  | Flush_everything

let flush_delayed_lets ~mode env res =
  (* Generate a wrapper function to introduce the delayed let-bindings. *)
  let wrap_flush order_map e =
    M.fold
      (fun _ (Binding b) acc ->
        match b.bound_expr with
        | Splittable_prim _ ->
          Misc.fatal_errorf
            "Complex bindings should have been split prior to being flushed."
        | Split { cmm_expr } | Simple { cmm_expr } ->
          Cmm_helpers.letin b.cmm_var ~defining_expr:cmm_expr ~body:acc)
      order_map e
  in
  (* CR-someday mshinwell: work out a criterion for allowing substitutions into
     loops. CR gbury: this is now done by creating a binding with the inline
     status `Must_inline_and_duplicate`, so the caller of `to_cmm_env` has to
     make that decision of whether to substitute inside loops. *)
  let res = ref res in
  let bindings_to_flush = ref M.empty in
  let flush (Binding b as binding) =
    if M.mem b.order !bindings_to_flush
    then Misc.fatal_errorf "Duplicate order for bindings when flushing";
    bindings_to_flush := M.add b.order binding !bindings_to_flush
  in
  let bindings_to_keep =
    Variable.Map.filter_map
      (fun _ (Binding b as binding) ->
        match b.inline with
        | Do_not_inline ->
          flush binding;
          None
        | Must_inline_and_duplicate -> (
          let r, split_res = split_complex_binding ~env ~res:!res b in
          res := r;
          let split_binding =
            match split_res with
            | Already_split -> binding
            | Split { new_bindings; split_binding } ->
              List.iter flush new_bindings;
              Binding split_binding
          in
          match mode with
          | Flush_everything ->
            flush split_binding;
            None
          | Branching_point | Entering_loop -> Some split_binding)
        | Must_inline_once -> (
          match
            mode, To_cmm_effects.classify_by_effects_and_coeffects b.effs
          with
          (* when not entering a loop, and with pure/generative effects at most,
             we can wait to split the binding, so that we can have a chance to
             try and push the arguments down the branch (otherwise, when we
             split, the arguments of the splittable binding would be flushed
             before the branch in control flow). *)
          | Branching_point, (Pure | Generative_immutable) -> Some binding
          | ( (Branching_point | Entering_loop | Flush_everything),
              (Pure | Generative_immutable | Coeffect_only | Effect) ) -> (
            let r, split_res = split_complex_binding ~env ~res:!res b in
            res := r;
            let split_binding =
              match split_res with
              | Already_split -> binding
              | Split { new_bindings; split_binding } ->
                List.iter flush new_bindings;
                Binding split_binding
            in
            match mode with
            | Flush_everything ->
              flush split_binding;
              None
            | Branching_point | Entering_loop -> Some split_binding))
        | May_inline_once -> (
          match To_cmm_effects.classify_by_effects_and_coeffects b.effs with
          (* Unless entering a loop, we do not flush pure bindings that can be
             inlined, ensuring that the corresponding expressions are sunk down
             as far as possible, including past control flow branching
             points. *)
          | Pure -> (
            match mode with
            | Flush_everything | Entering_loop ->
              flush binding;
              None
            | Branching_point -> Some binding)
          | Generative_immutable | Coeffect_only | Effect ->
            flush binding;
            None))
      env.bindings
  in
  let flush e = wrap_flush !bindings_to_flush e in
  flush, { env with stages = []; bindings = bindings_to_keep }, !res
