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

type free_vars = Backend_var.Set.t

type expr_with_info =
  { cmm : Cmm.expression;
    effs : Effects_and_coeffects.t;
    free_vars : free_vars
  }

type 'a param_type =
  | Param of 'a
  | Skip_param

type cont =
  | Jump of
      { cont : Lambda.static_label;
        param_types : Cmm.machtype param_type list
      }
  | Inline of
      { handler_params : Bound_parameters.t;
        handler_params_occurrences : Num_occurrences.t Variable.Map.t;
        handler_body : Flambda.Expr.t;
        handler_body_inlined_debuginfo : Inlined_debuginfo.t
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
    unary :
      ( 'env,
        P.unary_primitive,
        Simple.t * Cmm.expression -> prim_res )
      prim_helper;
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
  | Simple :
      { cmm_expr : Cmm.expression;
        free_vars : free_vars
      }
      -> simple bound_expr
  | Split :
      { cmm_expr : Cmm.expression;
        free_vars : free_vars
      }
      -> complex bound_expr
  | Splittable_prim :
      { dbg : Debuginfo.t;
        prim : Flambda_primitive.Without_args.t;
        arg_simples : Simple.t list;
        args : expr_with_info list
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
    inlined_debuginfo : Inlined_debuginfo.t;
    (* Debuginfo corresponding to inlined functions. *)
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
    vars : (Cmm.expression * free_vars) Variable.Map.t;
    (* Cmm expressions (of the form [Cvar ...]) for all bound variables in
       scope. *)
    bindings : any_binding Variable.Map.t;
    (* All bindings currently in env. *)
    inline_once_aliases : Variable.t Variable.Map.t;
    (* Maps for `Must_inline_once` variable that end up aliased. *)
    stages : stage list (* Stages of let-bindings, most recent at the head. *)
  }

type translation_result =
  { env : t;
    res : To_cmm_result.t;
    expr : expr_with_info
  }

(* Printing *)

let print_param_type print_typ ppf = function
  | Param typ -> print_typ ppf typ
  | Skip_param -> Format.fprintf ppf "skip"

let print_extra_info ppf = function
  | Untag e -> Format.fprintf ppf "Untag(%a)" Printcmm.expression e

let [@ocamlformat "disable"] print_inline (type a) ppf (inline : a inline) =
  match inline with
  | Do_not_inline -> Format.fprintf ppf "do_not_inline"
  | May_inline_once -> Format.fprintf ppf "may_inline_once"
  | Must_inline_once -> Format.fprintf ppf "must_inline_once"
  | Must_inline_and_duplicate -> Format.fprintf ppf "must_inline_and_duplicate"

let print_cmm_expr_with_free_vars ppf (cmm_expr, free_vars) =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(expr@ %a)@]@ @[<hov 1>(free_vars@ %a)@]@ )@]"
    Printcmm.expression cmm_expr Backend_var.Set.print free_vars

let [@ocamlformat "disable"] print_bound_expr (type a) ppf (b : a bound_expr) =
  match b with
  | Simple { cmm_expr; free_vars; } | Split { cmm_expr; free_vars; } ->
    print_cmm_expr_with_free_vars ppf (cmm_expr, free_vars)
  | Splittable_prim { prim; arg_simples = _; args; dbg; } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(dbg@ %a)@]@ \
      @[<hov 1>(prim@ %a)@]@ \
      @[<hov 1>(args@ @[<hov 1>(%a)@])@]\
      )@]"
      Debuginfo.print_compact dbg
      Flambda_primitive.Without_args.print prim
      (Format.pp_print_list (fun ppf { cmm; effs = _; free_vars; } ->
           print_cmm_expr_with_free_vars ppf (cmm, free_vars))) args

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

let _print_any_binding ppf (Binding binding) = print_binding ppf binding

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

(* Creation *)

let create offsets functions_info ~trans_prim ~return_continuation
    ~exn_continuation =
  { return_continuation;
    exn_continuation;
    offsets;
    functions_info;
    trans_prim;
    inlined_debuginfo = Inlined_debuginfo.none;
    stages = [];
    bindings = Variable.Map.empty;
    inline_once_aliases = Variable.Map.empty;
    vars_extra = Variable.Map.empty;
    vars = Variable.Map.empty;
    conts = Continuation.Map.empty;
    exn_handlers = Continuation.Set.singleton exn_continuation;
    exn_conts_extra_args = Continuation.Map.empty
  }

let enter_function_body env ~return_continuation ~exn_continuation =
  create env.offsets env.functions_info ~trans_prim:env.trans_prim
    ~return_continuation ~exn_continuation

(* Debuginfo *)

let enter_inlined_apply t new_inlined_debuginfo =
  { t with
    inlined_debuginfo =
      Inlined_debuginfo.merge t.inlined_debuginfo
        ~from_apply_expr:new_inlined_debuginfo
  }

let set_inlined_debuginfo t inlined_debuginfo = { t with inlined_debuginfo }

let add_inlined_debuginfo t dbg =
  Inlined_debuginfo.rewrite t.inlined_debuginfo dbg

let currently_in_inlined_body t =
  not (Inlined_debuginfo.is_none t.inlined_debuginfo)

(* Continuations *)

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
  let user_visible = Variable.user_visible v in
  let name = Variable.name v in
  let v = Backend_var.create_local name in
  let provenance =
    if not (!Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf)
    then None
    else if not user_visible
    then None
    else
      (* CR mshinwell: this is a temporary hack, the provenance information will
         be reworked soon *)
      Some
        (Backend_var.Provenance.create ~module_path:(Path.Pident v)
           ~location:Debuginfo.none ~original_ident:v)
  in
  Backend_var.With_provenance.create ?provenance v

let add_bound_param env v v' =
  let v'' = Backend_var.With_provenance.var v' in
  let free_vars = Backend_var.Set.singleton v'' in
  let vars = Variable.Map.add v (C.var v'', free_vars) env.vars in
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

let resolve_alias env var =
  match Variable.Map.find var env.inline_once_aliases with
  | exception Not_found -> var
  | v -> v

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

let add_jump_cont env k ~param_types =
  let cont = Lambda.next_raise_count () in
  let conts = Continuation.Map.add k (Jump { param_types; cont }) env.conts in
  cont, { env with conts }

let add_inline_cont env k ~handler_params ~handler_params_occurrences
    ~handler_body =
  let handler_body_inlined_debuginfo = env.inlined_debuginfo in
  let info =
    Inline
      { handler_params;
        handler_body;
        handler_params_occurrences;
        handler_body_inlined_debuginfo
      }
  in
  let conts = Continuation.Map.add k info env.conts in
  { env with conts }

let add_exn_handler env k arity =
  let env =
    { env with exn_handlers = Continuation.Set.add k env.exn_handlers }
  in
  match Flambda_arity.unarized_components arity with
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

let simple cmm_expr free_vars = Simple { cmm_expr; free_vars }

let complex_no_split cmm_expr free_vars = Split { cmm_expr; free_vars }

let splittable_primitive dbg prim arg_simples args =
  Splittable_prim { dbg; prim; arg_simples; args }

let is_cmm_simple cmm =
  match (cmm : Cmm.expression) with
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_symbol _ | Cvar _ ->
    true
  | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _ | Ctuple _ | Cop _
  | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    false

(* Helper function to create bindings *)

let create_binding_aux (type a) effs var ~(inline : a inline)
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
  binding

let create_binding (type a) effs var ~(inline : a inline)
    (bound_expr : a bound_expr) =
  (* In order to avoid generating binding of the form: "let x = y in ...", when
     'y' is trivial i.e. is a value that fits in a register, we mark 'x' as a
     must_inline_and_duplicate (since it basically replaces a variable by either
     another variable, a constant, or a symbol). *)
  match bound_expr with
  | (Split { cmm_expr; free_vars } | Simple { cmm_expr; free_vars })
    when is_cmm_simple cmm_expr ->
    (* trivial/simple cmm expression (as decided by [is_cmm_simple]) do not have
       effects and coeffects *)
    let effs = Ece.pure_can_be_duplicated in
    create_binding_aux effs var ~inline:Must_inline_and_duplicate
      (Split { cmm_expr; free_vars })
  | Simple _ | Split _ | Splittable_prim _ ->
    create_binding_aux effs var ~inline bound_expr

(* Binding splitting *)

let remove_binding env var =
  { env with bindings = Variable.Map.remove var env.bindings }

type split_result =
  | Already_split
  | Split of
      { new_bindings : any_binding list;
        split_binding : complex binding
      }

let new_bindings_for_splitting order args =
  let (new_bindings, _, free_vars_of_new_cmm_args), new_cmm_args =
    List.fold_left_map
      (fun (new_bindings, order, free_vars)
           { cmm = cmm_arg; effs = arg_effs; free_vars = arg_free_vars } ->
        (* CR gbury: here, instead of using [is_cmm_simple], we could instead
           look at [arg_effs] and not create a new binding if it has
           `pure_can_be_duplicated` effects (or any ece that allows
           duplication). *)
        if is_cmm_simple cmm_arg
        then
          ( (new_bindings, order, Backend_var.Set.union free_vars arg_free_vars),
            cmm_arg )
        else
          (* we need to rebind the argument *)
          (* CR gbury: we should try and store the flambda/cmm variable
             initially associated to this expression when it was built (and
             before it was inlined during the to_cmm translation), instead of
             using a fresh one here. *)
          let backend_var =
            Backend_var.create_local (Format.asprintf "to_cmm_split_%d" order)
          in
          let new_cmm_var =
            Backend_var.With_provenance.create ?provenance:None backend_var
          in
          let binding =
            Binding
              { order;
                effs = arg_effs;
                inline = Do_not_inline;
                bound_expr =
                  Simple { cmm_expr = cmm_arg; free_vars = arg_free_vars };
                cmm_var = new_cmm_var
              }
          in
          ( ( binding :: new_bindings,
              order - 1,
              Backend_var.Set.add backend_var free_vars ),
            C.var backend_var ))
      ([], order - 1, Backend_var.Set.empty)
      args
  in
  new_bindings, new_cmm_args, free_vars_of_new_cmm_args

let rebuild_prim ~dbg ~env ~res prim arg_simples args =
  let extra_info, res, cmm =
    match
      (prim, arg_simples, args : Flambda_primitive.Without_args.t * _ * _)
    with
    | Nullary nullary, [], [] -> env.trans_prim.nullary env res dbg nullary
    | Unary unary, [x_simple], [x] ->
      env.trans_prim.unary env res dbg unary (x_simple, x)
    | Binary binary, [_; _], [x; y] ->
      env.trans_prim.binary env res dbg binary x y
    | Ternary ternary, [_; _; _], [x; y; z] ->
      env.trans_prim.ternary env res dbg ternary x y z
    | Variadic variadic, _, args ->
      env.trans_prim.variadic env res dbg variadic args
    | (Nullary _ | Unary _ | Binary _ | Ternary _), _, _ ->
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
  | Splittable_prim { dbg; prim; arg_simples; args } ->
    (* We will be using the free vars of the new cmm args as the free vars for
       the new cmm expr for the binding (note that the same is done in
       [To_cmm_primitive]). This is correct because the cmm helpers to build
       expressions can introduce locally closed variables (through e.g. [bind]),
       but it will not create new free variables. It might be a slight
       over-approximation since some primitives may drop some of their
       arguments, but that should be extremely rare, and should not affect code
       generation much. *)
    let new_bindings, new_cmm_args, free_vars_of_new_cmm_args =
      new_bindings_for_splitting binding.order args
    in
    let new_cmm_expr, res =
      rebuild_prim ~dbg ~env ~res prim arg_simples new_cmm_args
    in
    let prim_effects =
      Flambda_primitive.Without_args.effects_and_coeffects prim
    in
    let () =
      match To_cmm_effects.classify_by_effects_and_coeffects prim_effects with
      | Pure | Generative_immutable -> ()
      | Effect | Coeffect_only ->
        Misc.fatal_errorf
          "Primitive %a was marked as `must_inline`, but is has the following \
           effects and coeffects: %a. This would lead to errors when moving \
           the primitive application to substitute it."
          Flambda_primitive.Without_args.print prim Ece.print prim_effects
    in
    let split_binding =
      { order = binding.order;
        effs = prim_effects;
        inline = binding.inline;
        bound_expr =
          Split
            { cmm_expr = new_cmm_expr; free_vars = free_vars_of_new_cmm_args };
        cmm_var = binding.cmm_var
      }
    in
    res, Split { new_bindings; split_binding }

(* Adding binding to the env and split them *)

let rec add_binding_to_env ?extra env res var (Binding binding as b) =
  let env =
    let bindings = Variable.Map.add var b env.bindings in
    let cmm_var = Backend_var.With_provenance.var binding.cmm_var in
    let free_vars = Backend_var.Set.singleton cmm_var in
    let vars = Variable.Map.add var (C.var cmm_var, free_vars) env.vars in
    let vars_extra =
      match extra with
      | None -> env.vars_extra
      | Some info -> Variable.Map.add var info env.vars_extra
    in
    { env with bindings; vars; vars_extra }
  in
  let classification =
    To_cmm_effects.classify_by_effects_and_coeffects binding.effs
  in
  let inline : _ inline = binding.inline in
  match inline with
  | Must_inline_and_duplicate -> (
    (* Bindings containing expressions that have effects/coeffects must be split
       at creation time, to ensure that the effectful/coeffectful expressions go
       on the stage stack (whereas the `must_inline_and_duplicate` bindings are
       *not* on the stage stack).

       Note that it would be correct to split all `must_inline_and_duplicate`
       bindings, regardless of its effects. However, we will always need to
       split some bindings late (particularly `must_inline_once` bindings), so
       always splitting `must_inline_and_duplicate` would not simplify the rest
       of the code much. *)
    match classification with
    | Pure | Generative_immutable -> env, res
    | Coeffect_only | Effect ->
      let env, res, _ = split_in_env env res var (binding : complex binding) in
      env, res)
  | May_inline_once | Must_inline_once | Do_not_inline -> (
    match classification with
    (* CR gbury: Generative_immutable bindings are treated the same as pure
       bindings. In particular this means that they can be moved across some
       function calls, including GC ones, which may break some allocation tests.
       However, that allows to sink down allocations that only occur in one
       branch (when they are used linearly), which can help a lot. *)
    | Pure | Generative_immutable -> env, res
    | Effect -> { env with stages = Effect var :: env.stages }, res
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
      { env with stages }, res)

(* CR gbury: find a better name for this function *)
and split_in_env env res var binding =
  let res, split_result = split_complex_binding ~env ~res binding in
  match split_result with
  | Already_split -> env, res, binding
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
    let env, res =
      List.fold_left
        (fun (env, res) new_binding ->
          let flambda_var = Variable.create "to_cmm_tmp" in
          add_binding_to_env env res flambda_var new_binding)
        (env, res) new_bindings
    in
    env, res, split_binding

let bind_variable_with_decision (type a) ?extra env res var ~inline
    ~(defining_expr : a bound_expr) ~effects_and_coeffects_of_defining_expr:effs
    =
  let binding = create_binding ~inline effs var defining_expr in
  add_binding_to_env ?extra env res var binding

let bind_variable ?extra env res var ~defining_expr ~free_vars_of_defining_expr
    ~num_normal_occurrences_of_bound_vars
    ~effects_and_coeffects_of_defining_expr =
  let inline =
    To_cmm_effects.classify_let_binding var
      ~effects_and_coeffects_of_defining_expr
      ~num_normal_occurrences_of_bound_vars
  in
  match inline with
  | Drop_defining_expr -> env, res
  | Regular ->
    let defining_expr = simple defining_expr free_vars_of_defining_expr in
    bind_variable_with_decision ?extra env res var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Do_not_inline
  | May_inline_once ->
    let defining_expr = simple defining_expr free_vars_of_defining_expr in
    bind_variable_with_decision ?extra env res var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:May_inline_once
  | Must_inline_once ->
    let defining_expr =
      complex_no_split defining_expr free_vars_of_defining_expr
    in
    bind_variable_with_decision ?extra env res var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Must_inline_once
  | Must_inline_and_duplicate ->
    let defining_expr =
      complex_no_split defining_expr free_vars_of_defining_expr
    in
    bind_variable_with_decision ?extra env res var
      ~effects_and_coeffects_of_defining_expr ~defining_expr
      ~inline:Must_inline_and_duplicate

let bind_variable_to_primitive = bind_variable_with_decision

(* Variable lookup (for potential inlining) *)

let will_inline_simple env res
    { effs; bound_expr = Simple { cmm_expr; free_vars }; _ } =
  { env; res; expr = { cmm = cmm_expr; free_vars; effs } }

let will_inline_complex env res { effs; bound_expr; _ } =
  match bound_expr with
  | Split { cmm_expr; free_vars } ->
    { env; res; expr = { cmm = cmm_expr; free_vars; effs } }
  | Splittable_prim { dbg; prim; arg_simples; args } ->
    let free_vars, cmm_args =
      List.fold_left_map
        (fun free_vars { cmm = cmm_arg; effs = _; free_vars = arg_free_vars } ->
          Backend_var.Set.union free_vars arg_free_vars, cmm_arg)
        Backend_var.Set.empty args
    in
    let cmm_expr, res = rebuild_prim ~dbg ~env ~res prim arg_simples cmm_args in
    { env; res; expr = { cmm = cmm_expr; free_vars; effs } }

let will_not_inline_simple env res { cmm_var; bound_expr = Simple _; _ } =
  let var = Backend_var.With_provenance.var cmm_var in
  let free_vars = Backend_var.Set.singleton var in
  { env;
    res;
    expr = { cmm = C.var var; free_vars; effs = Ece.pure_can_be_duplicated }
  }

let split_and_inline env res var binding =
  let env, res, split_binding = split_in_env env res var binding in
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
  let var = resolve_alias env var in
  match Variable.Map.find var env.bindings with
  | exception Not_found -> (
    (* this happens for continuation parameters and bindings that have been
       flushed *)
    match Variable.Map.find var env.vars with
    | exception Not_found ->
      Misc.fatal_errorf "Variable %a not found in env" Variable.print var
    | cmm, free_vars ->
      (* the env.vars map only contain bindings to expressions of the form
         [Cmm.Cvar _], hence the effects. *)
      { env; res; expr = { cmm; free_vars; effs = Ece.pure_can_be_duplicated } }
    )
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
      | Pure | Generative_immutable ->
        let env = remove_binding env var in
        will_inline_simple env res binding
      | Effect | Coeffect_only -> (
        match
          pop_if_in_top_stage ?consider_inlining_effectful_expressions env var
        with
        | None -> will_not_inline_simple env res binding
        | Some env ->
          let env = remove_binding env var in
          will_inline_simple env res binding)))

(* Handling of aliases between variables *)

(* Situation: [alias_of] is a `must_inline_once` and [var] is used exactly once

   In this case, we do not want to split the binding in this case, but instead
   just transfer the binding to the new variable, so that we can decide whether
   to split it at its effective use (and not here where we rebind it to a
   used-only-once variable).

   Since `Must_inline_once` bindings (or rather the bound variable) can be
   arbitrarily deep in the stage stack, it would be too costly to change the
   whole stack, so we use an alias map on the side instead. *)
let make_alias env res var alias_of =
  let inline_once_aliases =
    Variable.Map.add var alias_of env.inline_once_aliases
  in
  let env = { env with inline_once_aliases } in
  env, res

(* Situation: [alias_of] is a must_inline (once or duplicate), and [var] is used
   more than once.

   In this case, we want to force splitting of the original binding, and then
   bind the new variable with a `must_inline` inline status *)
let split_binding_and_rebind ~num_occurrences_of_var env res ~var ~alias_of
    binding =
  let { env; res; expr = { cmm; free_vars; effs } } =
    split_and_inline env res alias_of binding
  in
  let defining_expr : _ bound_expr = Split { cmm_expr = cmm; free_vars } in
  let inline =
    match (num_occurrences_of_var : Num_occurrences.t) with
    | Zero | One -> Must_inline_once
    | More_than_one -> Must_inline_and_duplicate
  in
  bind_variable_with_decision env res var ~inline ~defining_expr
    ~effects_and_coeffects_of_defining_expr:effs

let add_alias env res ~var ~alias_of ~num_normal_occurrences_of_bound_vars =
  let alias_of = resolve_alias env alias_of in
  let num_occurrences_of_var : Num_occurrences.t =
    match Variable.Map.find var num_normal_occurrences_of_bound_vars with
    | exception Not_found ->
      Misc.fatal_errorf
        "Missing occurrence in to_cmm for variable %a aliased to %a"
        Variable.print var Variable.print alias_of
    | num_occurrences -> num_occurrences
  in
  match Variable.Map.find alias_of env.bindings with
  | Binding ({ inline = Must_inline_once; _ } as b) -> (
    match num_occurrences_of_var with
    | Zero ->
      let env = remove_binding env alias_of in
      env, res
    | One -> make_alias env res var alias_of
    | More_than_one ->
      let env = remove_binding env alias_of in
      split_binding_and_rebind ~num_occurrences_of_var env res ~var ~alias_of b)
  | Binding ({ inline = Must_inline_and_duplicate; _ } as b) ->
    split_binding_and_rebind ~num_occurrences_of_var env res ~var ~alias_of b
  | (exception Not_found)
  | Binding { inline = Do_not_inline | May_inline_once; _ } ->
    (* generic case, we just inline the var/binding, and rebind it *)
    let { env; res; expr = { cmm; free_vars; effs } } =
      inline_variable env res alias_of
    in
    bind_variable env res var ~defining_expr:cmm
      ~free_vars_of_defining_expr:free_vars
      ~effects_and_coeffects_of_defining_expr:effs
      ~num_normal_occurrences_of_bound_vars

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

let can_be_removed effs =
  match (effs : Effects_and_coeffects.t) with
  | Arbitrary_effects, _, _ -> false
  | (Only_generative_effects _ | No_effects), _, _ -> true

let flush_delayed_lets ~mode env res =
  (* Generate a wrapper function to introduce the delayed let-bindings. *)
  let wrap_flush order_map e free_vars =
    M.fold
      (fun _ (Binding b) (acc, acc_free_vars) ->
        match b.bound_expr with
        | Splittable_prim _ ->
          Misc.fatal_errorf
            "Complex bindings should have been split prior to being flushed."
        | Split { cmm_expr; free_vars } | Simple { cmm_expr; free_vars } ->
          let v = Backend_var.With_provenance.var b.cmm_var in
          if (not (Backend_var.Set.mem v acc_free_vars))
             && can_be_removed b.effs
          then acc, acc_free_vars
          else
            let expr =
              Cmm_helpers.letin b.cmm_var ~defining_expr:cmm_expr ~body:acc
            in
            let free_vars =
              Backend_var.Set.union free_vars
                (Backend_var.Set.remove v acc_free_vars)
            in
            expr, free_vars)
      order_map (e, free_vars)
  in
  (* CR-someday mshinwell: work out a criterion for allowing substitutions into
     loops. CR gbury: this is now done by creating a binding with the inline
     status `Must_inline_and_duplicate`, so the caller of `to_cmm_env` has to
     make that decision of whether to substitute inside loops. *)
  let res = ref res in
  let bindings_to_flush = ref M.empty in
  let flush (Binding b as binding) =
    if M.mem b.order !bindings_to_flush
    then
      Misc.fatal_errorf "Duplicate order for bindings when flushing: %a = %a"
        Backend_var.With_provenance.print b.cmm_var print_bound_expr
        b.bound_expr;
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
          (* Unless entering a loop, we do not flush pure/generative_immutable
             bindings that can be inlined, ensuring that the corresponding
             expressions are sunk down as far as possible, including past
             control flow branching points. *)
          | Pure | Generative_immutable -> (
            match mode with
            | Flush_everything | Entering_loop ->
              flush binding;
              None
            | Branching_point -> Some binding)
          | Coeffect_only | Effect ->
            flush binding;
            None))
      env.bindings
  in
  let flush e = wrap_flush !bindings_to_flush e in
  flush, { env with stages = []; bindings = bindings_to_keep }, !res
