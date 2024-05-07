(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart, Vincent Laviron and Louis Gesbert, OCamlPro      *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lambda

(* Converts let-rec containing values into an initialization then assignment
   sequence.

   We assume that the typechecker correctly validated that the letrec is
   compilable (See typing/Rec_check.is_valid_recursive_expression).

   That is, for every expression to which a variable is bound in the let-rec
   verify:

   * It does not inspect any variable bound by the let-rec. This implies that
   the value of a variable bound by the let-rec does not need to be valid during
   the computation of the other bindings. Only the address is needed. This means
   that we can preallocate values for which we know the size.

   * If the value can't be preallocated (it can be because the size can't be
   statically known, or because the value is not allocated, like integers), then
   the value does not depend on any other rec-bound variables. This implies that
   any value that can't be preallocated can be computed before any other
   binding. Note that this does not mean that other variables can't be refered
   to by the expression, but if it happens, then the value is useless.

   We consider two cases for expressions that we consider of known size (the
   [Static] case in [is_valid_recursive_expression]): the makeblock primitive
   and function declarations. Makeblocks will be preallocated, while all the
   functions will be turned into a single letrec.

   The structure of the generated code will be: *)
(* {[
 *   let c = const in
 *   (* [consts]: constants *)
 *   ...
 *   let v = caml_alloc_dummy n in
 *   (* [blocks]: With n the staticaly known size of blocks *)
 *   ...
 *   let p = expr in
 *   (* [pre]: Values that do not depend on any other from the let-rec in
 *      [Rec_check.Dereference] position *)
 *   ...
 *   let rec f x = ...
 *   and g x = ...
 *   and ...
 *   in
 *   (* [functions]: All the functions from the let-rec *)
 *   caml_update_dummy v v_contents;
 *   (* Initialisation ([effects]) *)
 *   ...
 * ]}
 *)
(* Special care is taken to handle nested [let rec]s: the recursive values of a
   letrec are often more than the ones bound by the letrec expression itself.
   For instance: *)
(*
 * let rec f =
 *   let rec g x = h (x+1)
 *   and h x = i x
 *   in
 *   fun x -> g x
 * and i x =
 *   if x > 33 then x
 *   else f x
 *
 * in this expression every function is recursively defined with
 * any other. Hence this is equivalent to
 *
 * let rec f = fun x -> g x
 * and i x =
 *   if x > 33 then x
 *   else f x
 * and g x = h (x+1)
 * and h x = i x
 *)
(* However, there might be (a subset of) a local [let rec] that is indeed
   internally recursive, but that is used by the top-level [let rec] in
   [Dereference] positions. We carefully lift those within [pre], and handle
   them recursively in the same pass to preserve the order of evaluation.

   The analysis for which variables should remain in the inner [let rec], and
   which are indeed part of the outer [let rec] is equivalent to the
   [Rec_check.value_bindings] case. *)

module Ident = struct
  include Ident

  module Set = struct
    include Ident.Set

    (* CR-someday mshinwell: For the moment duplicated from [Container_types] to
       avoid changing [Identifiable] *)
    let fixpoint f set =
      let rec aux acc set =
        if is_empty set
        then acc
        else
          let set' = fold (fun x -> union (f x)) set empty in
          let acc = union acc set in
          aux acc (diff set' acc)
      in
      aux empty set
  end
end

type block_type =
  | Normal of int
  (* tag *)
  | Flat_float_record
  | Mixed of Lambda.mixed_block_shape

type block =
  { block_type : block_type;
    size : int
  }

type letrec =
  { blocks : (Ident.t * block) list;
    (* Pre-allocated blocks. Will result in [let id = caml_alloc_dummy size] or
       [let id = caml_alloc_dummy_float size] *)
    consts : (Ident.t * Lambda.structured_constant) list;
    (* Statically known values *)
    pre : tail:Lambda.lambda -> Lambda.lambda;
    (* Prefix of the expression that does not depends on any recursive part.
       This is presented as a function for easy 'concatenation': to append
       'expr': [fun ~tail -> Lsequence (expr, letrec.pre ~tail)] *)
    effects : Lambda.lambda;
    (* Effects that are applied afterwards. *)
    functions : (Ident.t * Lambda.lfunction) list;
    substitution : Ident.t Ident.Map.t;
    (* Alias to recursive variables should be forbidden, to prevent
       non-productive definition like 'let rec a = a'. But some aliases are
       allowed, for instance 'let rec a = let c = b in 1 :: b and b = 2 :: a'.
       The simplest way to handle those aliases is simply to apply a substitute
       of all these aliases afterward. *)
    letbound : Ident.Set.t;
    (* Set of known immutable variables. Mutable ones cannot define aliases *)
    needs_region : bool
        (* Set to [true] if the defining expressions of the [let rec] need to be
           surrounded by a local allocation region. *)
  }

type let_def =
  { let_kind : Lambda.let_kind;
    layout : Lambda.layout;
    ident : Ident.t
  }

exception Bug

let () = ignore Bug

let lsequence (lam1, lam2) =
  match lam1 with
  | Lsequence (lam, Lconst (Const_base (Const_int 0))) -> Lsequence (lam, lam2)
  | Lconst (Const_base (Const_int 0)) -> lam2
  | _ -> Lsequence (lam1, lam2)
  [@@ocaml.warning "-fragile-match"]

let caml_update_dummy_prim =
  Lambda.simple_prim_on_values ~name:"caml_update_dummy" ~arity:2 ~alloc:true

let update_dummy var expr =
  Lprim (Pccall caml_update_dummy_prim, [Lvar var; expr], Loc_unknown)

let build_block let_def size block_type expr letrec =
  { letrec with
    blocks = (let_def.ident, { block_type; size }) :: letrec.blocks;
    effects = Lsequence (update_dummy let_def.ident expr, letrec.effects)
  }

let is_simple (lam : Lambda.lambda) =
  match lam with Lvar _ | Lconst _ -> true | _ -> false
  [@@ocaml.warning "-fragile-match"]

let assert_not_local ~lam : Lambda.alloc_mode -> unit = function
  | Alloc_heap -> ()
  | Alloc_local ->
    Misc.fatal_errorf "Invalid stack allocation found in %a" Printlambda.lambda
      lam

let dead_code lam letrec =
  (* Some cases generate code without effects, and bound to nothing. We use this
     function to insert it as [Lsequence] in [effects], for documentation. It
     would be correct to discard and just return [letrec] instead. *)
  { letrec with effects = lsequence (lam, letrec.effects) }

(* We desconstruct the let-rec into a description *)

let rec prepare_letrec (recursive_set : Ident.Set.t)
    (* Variables that depends on the let-rec bound variables *)
      (current_let : let_def option)
    (* The variable to which the current expression is bound. current_let.ident
       is part of recursive_set *)
      (lam : Lambda.lambda) (letrec : letrec) =
  match lam with
  | Lfunction funct -> (
    assert_not_local ~lam funct.mode;
    match current_let with
    | Some current_let when Ident.Set.mem current_let.ident recursive_set ->
      { letrec with functions = (current_let.ident, funct) :: letrec.functions }
    | Some current_let ->
      (* If the currently bound function does not depend on any recursive
         variable *)
      let pre ~tail : Lambda.lambda =
        Llet
          ( current_let.let_kind,
            current_let.layout,
            current_let.ident,
            lam,
            letrec.pre ~tail )
      in
      { letrec with pre }
    | None -> dead_code lam letrec)
  | Lprim (((Pmakeblock _ | Pmakearray _ | Pduprecord _) as prim), args, dbg)
    when not (List.for_all is_simple args) ->
    (match prim with
    | Pmakeblock (_, _, _, mode) | Pmakearray (_, _, mode) ->
      assert_not_local ~lam mode
    | _ -> ());
    (* If there are some non-trivial expressions as arguments, we first extract
       the arguments (to let-bound variables) before deconstructing. Arguments
       could contain side effects and other blocks declarations. *)
    let defs, args =
      List.fold_right
        (fun (def : Lambda.lambda) (defs, args) ->
          (* Fold-right to preserve the list order *)
          if is_simple def
          then (* This prevents looping on variables *)
            defs, def :: args
          else
            let id = Ident.create_local "lift_in_letrec" in
            (id, def) :: defs, Lambda.Lvar id :: args)
        args ([], [])
    in
    (* [arg_layout] is more general than any of the possible layouts of [args]:
       The arguments to [Pmakeblock]/[Pmakearray] are value fields, and the
       argument to [Pduprecord] is a block. *)
    let arg_layout = Typeopt.layout_union layout_value_field layout_block in
    (* Bytecode evaluates effects in blocks from right to left, so reverse defs
       to preserve evaluation order. Relevant test: letrec/evaluation_order_3 *)
    let lam =
      List.fold_left
        (fun body (id, def) : Lambda.lambda ->
          Llet (Strict, arg_layout, id, def, body))
        (Lambda.Lprim (prim, args, dbg))
        defs
    in
    prepare_letrec recursive_set current_let lam letrec
  | Lprim (Pmakeblock (_, _, _, mode), args, _)
  | Lprim (Pmakearray ((Paddrarray | Pintarray), _, mode), args, _) -> (
    assert_not_local ~lam mode;
    match current_let with
    | Some cl -> build_block cl (List.length args) (Normal 0) lam letrec
    | None -> dead_code lam letrec
    (* We know that [args] are all "simple" at this point, so no effects *))
  | Lprim (Pmakearray (Pfloatarray, _, mode), args, _)
  | Lprim (Pmakefloatblock (_, mode), args, _) -> (
    assert_not_local ~lam mode;
    match current_let with
    | Some cl -> build_block cl (List.length args) Flat_float_record lam letrec
    | None -> dead_code lam letrec)
  | Lprim (Pmakemixedblock (_, _, shape, mode), args, _) -> (
    assert_not_local ~lam mode;
    match current_let with
    | Some cl -> build_block cl (List.length args) (Mixed shape) lam letrec
    | None -> dead_code lam letrec)
  | Lprim (Pduprecord (kind, size), args, _) -> (
    match current_let with
    | Some cl -> (
      let arg =
        match args with
        | [arg] -> arg
        | _ -> Misc.fatal_error "Dissect_letrec.prepare_letrec duprecord"
      in
      match[@ocaml.warning "fragile-match"] kind with
      | Record_boxed _ -> build_block cl size (Normal 0) arg letrec
      | Record_inlined (Ordinary { runtime_tag; _ }, Variant_boxed _) ->
        build_block cl size (Normal runtime_tag) arg letrec
      | Record_inlined (Extension _, Variant_extensible) ->
        build_block cl (size + 1) (Normal 0) arg letrec
      | Record_float | Record_ufloat ->
        build_block cl size Flat_float_record arg letrec
      | Record_mixed mixed ->
        let mixed = Lambda.transl_mixed_product_shape mixed in
        build_block cl size (Mixed mixed) arg letrec
      | Record_inlined (Extension _, _)
      | Record_inlined (Ordinary _, (Variant_unboxed | Variant_extensible))
      | Record_unboxed ->
        Misc.fatal_errorf "Unexpected record kind:@ %a" Printlambda.lambda lam)
    | None -> dead_code lam letrec)
  | Lconst const -> (
    match current_let with
    | Some current_let ->
      { letrec with consts = (current_let.ident, const) :: letrec.consts }
    | None -> dead_code lam letrec)
  | Lmutlet (k, id, def, body) ->
    let letrec = prepare_letrec recursive_set current_let body letrec in
    (* Variable let comes from mutable values, and reading from it is considered
       as inspections by Typecore.check_recursive_expression.

       This means that either:

       - the value does not depend on any recursive value,

       - or it is not read in the let-rec *)

    (* TODO: binder dans une variable temporaire *)
    let free_vars_def = Lambda.free_variables def in
    if Ident.Set.disjoint free_vars_def recursive_set
    then
      let pre ~tail : Lambda.lambda = Lmutlet (k, id, def, letrec.pre ~tail) in
      { letrec with pre }
    else
      let free_vars_body = Lambda.free_variables body in
      (* This is infrequent enough for not caring about performances *)
      assert (not (Ident.Set.mem id free_vars_body));
      (* It is not used, we only keep the effect *)
      { letrec with effects = Lsequence (def, letrec.effects) }
  | Llet (((Strict | Alias | StrictOpt) as let_kind), layout, id, def, body) ->
    let letbound = Ident.Set.add id letrec.letbound in
    let letrec = { letrec with letbound } in
    let free_vars = Lambda.free_variables def in
    if Ident.Set.disjoint free_vars recursive_set
    then
      (* Non recursive let *)
      let letrec = prepare_letrec recursive_set current_let body letrec in
      let pre ~tail : Lambda.lambda =
        Llet (let_kind, layout, id, def, letrec.pre ~tail)
      in
      { letrec with pre }
    else
      let recursive_set = Ident.Set.add id recursive_set in
      let letrec = prepare_letrec recursive_set current_let body letrec in
      let let_def = { let_kind; layout; ident = id } in
      prepare_letrec recursive_set (Some let_def) def letrec
  | Lsequence (lam1, lam2) ->
    let letrec = prepare_letrec recursive_set current_let lam2 letrec in
    prepare_letrec recursive_set None lam1 letrec
  | Levent (body, event) ->
    let letrec = prepare_letrec recursive_set current_let body letrec in
    { letrec with effects = Levent (letrec.effects, event) }
  | Lletrec (bindings, body) ->
    (* Inner letrecs need some special care: We split between _outer_ bindings
       that are recursive with the current [recursive_set], and therefore need
       to be merged into it, and _inner_ bindings that are not (but can still be
       recursive between themselves). This corresponds to the [Recursive] case
       in [Rec_check.value_bindings].

       One solution would be to handle now the outer bindings, and lift the
       inner ones into [letrec.pre], to be handled in a second pass. That would
       change the evaluation order, though, so we instead descend recursively in
       a single pass. This requires separate accumulators that we re-integrate
       in the right place in [letrec] afterwards, so that the outer bindings can
       safely depend upon them. *)
    let deps =
      List.fold_left
        (fun acc (x, def) -> Ident.Map.add x (Lambda.free_variables def) acc)
        Ident.Map.empty bindings
    in
    let vars = Ident.Map.keys deps in
    let reverse_deps =
      (* Set.t Map.t to Set.t Map.t transposition to get reverse dependencies *)
      let add_opt x = function
        | None -> Some (Ident.Set.singleton x)
        | Some s -> Some (Ident.Set.add x s)
      in
      Ident.Map.fold
        (fun x deps reverse_deps ->
          Ident.Set.fold
            (fun d reverse_deps -> Ident.Map.update d (add_opt x) reverse_deps)
            deps reverse_deps)
        deps Ident.Map.empty
    in
    let recursive_set =
      (* and a fixpoint to get their transitive counterpart *)
      Ident.Set.fixpoint
        (fun x ->
          try Ident.Map.find x reverse_deps with Not_found -> Ident.Set.empty)
        recursive_set
    in
    let outer_vars = Ident.Set.inter vars recursive_set in
    if Ident.Set.is_empty outer_vars
    then
      (* Non recursive relative to top-level letrec, we can avoid dissecting it
         right now. Its turn will come later. *)
      let letrec = prepare_letrec recursive_set current_let body letrec in
      let pre ~tail : Lambda.lambda = Lletrec (bindings, letrec.pre ~tail) in
      { letrec with pre }
    else
      let letrec =
        { letrec with letbound = Ident.Set.union letrec.letbound vars }
      in
      let letrec = prepare_letrec recursive_set current_let body letrec in
      let pre, letrec =
        (* extract the current [pre], so that definitions from the inner letrec
           can be re-inserted in the middle *)
        letrec.pre, { letrec with pre = (fun ~tail -> tail) }
      in
      let letrec, inner_effects, inner_functions =
        List.fold_right
          (fun (id, def) (letrec, inner_effects, inner_functions) ->
            let let_def =
              { let_kind = Strict; layout = Lambda.layout_letrec; ident = id }
            in
            if Ident.Set.mem id outer_vars
            then
              ( prepare_letrec recursive_set (Some let_def) def letrec,
                inner_effects,
                inner_functions )
            else
              let { blocks;
                    consts;
                    pre;
                    effects;
                    functions;
                    substitution;
                    letbound;
                    needs_region
                  } =
                letrec
              in
              let inner_letrec =
                { effects = Lconst (Const_base (Const_int 0));
                  functions = [];
                  (* these can be safely handled in common *)
                  blocks;
                  consts;
                  pre;
                  substitution;
                  letbound;
                  needs_region
                }
              in
              let inner_letrec =
                prepare_letrec
                  (Ident.Set.diff vars outer_vars)
                  (Some let_def) def inner_letrec
              in
              ( { inner_letrec with effects; functions; needs_region },
                inner_letrec.effects :: inner_effects,
                inner_letrec.functions @ inner_functions ))
          bindings (letrec, [], [])
      in
      let pre =
        List.fold_left
          (fun pre -> function
            | Lconst (Const_base (Const_int 0)) -> pre
            | eff -> fun ~tail -> Lsequence (eff, pre ~tail))
          pre inner_effects
      in
      let pre =
        match inner_functions with
        | [] -> pre
        | _ :: _ ->
          let functions =
            List.map (fun (id, lfun) -> id, Lfunction lfun) inner_functions
          in
          fun ~tail -> Lletrec (functions, pre ~tail)
      in
      let pre ~tail = letrec.pre ~tail:(pre ~tail) in
      { letrec with pre }
  | Lvar id when Ident.Set.mem id letrec.letbound -> (
    (* This is not a mutable variable: it is ok to copy it *)
    match current_let with
    | Some cl ->
      let substitute_from =
        Ident.Map.fold
          (fun x y acc ->
            if Ident.same y cl.ident then Ident.Set.add x acc else acc)
          letrec.substitution
          (Ident.Set.singleton cl.ident)
      in
      let substitution =
        Ident.Set.fold
          (fun from -> Ident.Map.add from id)
          substitute_from letrec.substitution
      in
      let letbound = Ident.Set.add cl.ident letrec.letbound in
      { letrec with substitution; letbound }
    | None -> dead_code lam letrec)
  | Lifused (_v, lam) -> prepare_letrec recursive_set current_let lam letrec
  | Lwhile _ | Lfor _ | Lassign (_, _) ->
    (* Effect expressions returning unit. The result can be pre-declared. *)
    let consts =
      match current_let with
      | Some cl -> (cl.ident, Const_base (Const_int 0)) :: letrec.consts
      | None -> letrec.consts
    in
    { letrec with effects = Lsequence (lam, letrec.effects); consts }
  | Lapply _
  | Lswitch (_, _, _, _)
  | Lstringswitch (_, _, _, _, _)
  | Lstaticraise (_, _)
  | Lstaticcatch (_, _, _, _)
  | Ltrywith (_, _, _, _)
  | Lifthenelse (_, _, _, _)
  | Lsend _ | Lvar _ | Lmutvar _
  | Lprim (_, _, _) ->
    (* This cannot be recursive, otherwise it should have been caught by the
       well formedness check. Hence it is ok to evaluate it before anything
       else. *)
    (* Check that no recursive variable appears in a position where it is
       inspected (appearances in guarded positions in other cases are OK) *)
    let no_recurse =
      match lam with
      | Lstaticcatch (_, _, _, _) | Ltrywith (_, _, _, _) -> None
      | Lswitch (lam1, _, _, _)
      | Lstringswitch (lam1, _, _, _, _)
      | Lifthenelse (lam1, _, _, _) ->
        Some lam1
      | Lapply _ | Lstaticraise _ | Lsend _ | Lvar _ | Lmutvar _ | Lprim _ ->
        Some lam
      | _ -> assert false
    in
    Option.iter
      (fun lam ->
        let free_vars = Lambda.free_variables lam in
        if not (Ident.Set.disjoint free_vars recursive_set)
        then
          Misc.fatal_errorf "Unallowed recursive access to %a in:@.%a@."
            Ident.Set.print
            (Ident.Set.inter free_vars recursive_set)
            Printlambda.lambda lam)
      no_recurse;
    let pre =
      match current_let with
      | Some cl ->
        fun ~tail : Lambda.lambda ->
          Llet (cl.let_kind, cl.layout, cl.ident, lam, letrec.pre ~tail)
      | None -> fun ~tail : Lambda.lambda -> Lsequence (lam, letrec.pre ~tail)
    in
    { letrec with pre }
  | Lregion (body, _) ->
    let letrec = prepare_letrec recursive_set current_let body letrec in
    { letrec with needs_region = true }
  | Lexclave _ ->
    Misc.fatal_errorf
      "Cannot yet handle Lexclave directly under let rec with Flambda 2:@ %a"
      Printlambda.lambda lam
  [@@ocaml.warning "-fragile-match"]

let dissect_letrec ~bindings ~body ~free_vars_kind =
  let letbound = Ident.Set.of_list (List.map fst bindings) in
  let letrec =
    List.fold_right
      (fun (id, def) letrec ->
        let let_def =
          { let_kind = Strict; layout = Lambda.layout_letrec; ident = id }
        in
        prepare_letrec letbound (Some let_def) def letrec)
      bindings
      { blocks = [];
        consts = [];
        pre = (fun ~tail -> tail);
        effects = Lconst (Const_base (Const_int 0));
        functions = [];
        substitution = Ident.Map.empty;
        letbound;
        needs_region = false
      }
  in
  let preallocations =
    let alloc_normal_dummy cfun size =
      let desc = Lambda.simple_prim_on_values ~name:cfun ~arity:1 ~alloc:true in
      let size : lambda = Lconst (Const_base (Const_int size)) in
      Lprim (Pccall desc, [size], Loc_unknown)
    in
    let alloc_mixed_dummy cfun (shape : Lambda.mixed_block_shape) size =
      let size = Lconst (Const_base (Const_int size)) in
      let value_prefix_len =
        Lconst (Const_base (Const_int shape.value_prefix_len))
      in
      let desc = Lambda.simple_prim_on_values ~name:cfun ~arity:2 ~alloc:true in
      Lprim (Pccall desc, [size; value_prefix_len], Loc_unknown)
    in
    List.map
      (fun (id, { block_type; size }) ->
        let ccall =
          match block_type with
          | Normal _tag -> alloc_normal_dummy "caml_alloc_dummy" size
          | Flat_float_record ->
            alloc_normal_dummy "caml_alloc_dummy_float" size
          | Mixed shape -> alloc_mixed_dummy "caml_alloc_dummy_mixed" shape size
        in
        id, ccall)
      letrec.blocks
  in
  let body = if not letrec.needs_region then body else Lexclave body in
  let effects_then_body = lsequence (letrec.effects, body) in
  let functions =
    match letrec.functions with
    | [] -> effects_then_body
    | _ :: _ ->
      let functions =
        List.map (fun (id, lfun) -> id, Lfunction lfun) letrec.functions
      in
      Lletrec (functions, effects_then_body)
  in
  let with_non_rec = letrec.pre ~tail:functions in
  let with_preallocations =
    List.fold_left
      (fun body (id, binding) ->
        (* Preallocations can only be blocks *)
        Llet (Strict, Lambda.layout_block, id, binding, body))
      with_non_rec preallocations
  in
  let with_constants =
    List.fold_left
      (fun body (id, const) ->
        Llet
          ( Strict,
            Lambda.structured_constant_layout const,
            id,
            Lconst const,
            body ))
      with_preallocations letrec.consts
  in
  let substituted = Lambda.rename letrec.substitution with_constants in
  if letrec.needs_region
  then
    let body_layout =
      let bindings =
        Ident.Map.map (fun _ -> Lambda.layout_letrec)
        @@ Ident.Map.of_list bindings
      in
      let free_vars_kind id : Lambda.layout option =
        try Some (Ident.Map.find id bindings)
        with Not_found -> free_vars_kind id
      in
      Lambda.compute_expr_layout free_vars_kind body
    in
    Lregion (substituted, body_layout)
  else substituted

type dissected =
  | Dissected of Lambda.lambda
  | Unchanged

let dissect_letrec ~bindings ~body ~free_vars_kind =
  let is_a_function = function _, Lfunction _ -> true | _, _ -> false in
  if List.for_all is_a_function bindings
  then Unchanged
  else
    try Dissected (dissect_letrec ~bindings ~body ~free_vars_kind)
    with Bug ->
      Misc.fatal_errorf "let-rec@.%a@." Printlambda.lambda
        (Lletrec (bindings, body))
  [@@ocaml.warning "-fragile-match"]
