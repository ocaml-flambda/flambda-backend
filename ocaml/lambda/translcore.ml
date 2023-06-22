(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Layouts
open Types
open Typedtree
open Typeopt
open Lambda
open Translmode
open Debuginfo.Scoped_location

type error =
    Free_super_var
  | Unreachable_reached
  | Bad_probe_layout of Ident.t
  | Non_value_layout of Layout.Violation.t

exception Error of Location.t * error

let use_dup_for_constant_mutable_arrays_bigger_than = 4

(* CR layouts v2: When we're ready to allow non-values, these can be deleted or
   changed to check for void. *)
let sort_must_be_value ~why loc sort =
  if not Sort.(equate sort value) then
    let violation = Layout.(Violation.of_ (Not_a_sublayout
                                             (of_sort ~why sort,
                                              value ~why:V1_safety_check))) in
    raise (Error (loc, Non_value_layout violation))

let layout_must_be_value loc layout =
  match Layout.(sub layout (value ~why:V1_safety_check)) with
  | Ok _ -> ()
  | Error e -> raise (Error (loc, Non_value_layout e))

(* CR layouts v2: In the places where this is used, we want to allow any (the
   left of a semicolon and loop bodies).  So we want this instead of the usual
   sanity check for value.  But we still default to value before checking for
   void, to allow for sort variables arising in situations like

     let foo () = raise Foo; ()

   When this sanity check is removed, consider whether we are still defaulting
   appropriately.
*)
let layout_must_not_be_void loc layout =
  Layout.default_to_value layout;
  match Layout.(sub layout (void ~why:V1_safety_check)) with
  | Ok _ ->
     let violation = Layout.(Violation.of_ (Not_a_sublayout
                               (layout, value ~why:V1_safety_check))) in
    raise (Error (loc, Non_value_layout violation))
  | Error _ -> ()

let layout_exp e = layout e.exp_env e.exp_loc e.exp_type

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun ~scopes:_ _cc _rootpath _modl -> assert false) :
      scopes:scopes -> module_coercion -> Longident.t option ->
      module_expr -> lambda)

let transl_object =
  ref (fun ~scopes:_ _id _s _cl -> assert false :
       scopes:scopes -> Ident.t -> string list -> class_expr -> lambda)

(* Probe handlers are generated from %probe as closed functions
   during transl_exp and immediately lifted to top level. *)
let probe_handlers = ref []
let clear_probe_handlers () = probe_handlers := []
let declare_probe_handlers lam =
  List.fold_left (fun acc (funcid, func) ->
      Llet(Strict, Lambda.layout_function, funcid, func, acc))
    lam
    !probe_handlers

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor ~scopes env path ext =
  let path =
    Printtyp.wrap_printing_env env ~error:true (fun () ->
      Option.map (Printtyp.rewrite_double_underscore_longidents env) path)
  in
  let name =
    match path with
    | None -> Ident.name ext.ext_id
    | Some path -> Format.asprintf "%a" Pprintast.longident path
  in
  let loc = of_location ~scopes ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      (* Extension constructors are currently always Alloc_heap.
         They could be Alloc_local, but that would require changes
         to pattern typing, as patterns can close over them. *)
      Lprim (Pmakeblock (Obj.object_tag, Immutable_unique, None, alloc_heap),
        [Lconst (Const_base (Const_string (name, ext.ext_loc, None)));
         Lprim (prim_fresh_oo_id, [Lconst (const_int 0)], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_extension_path loc env path

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

let transl_apply_position position =
  match position with
  | Default -> Rc_normal
  | Nontail -> Rc_nontail
  | Tail ->
    if Config.stack_allocation then Rc_close_at_apply
    else Rc_normal

let may_allocate_in_region lam =
  (* loop_region raises, if the lambda might allocate in parent region *)
  let rec loop_region lam =
    shallow_iter ~tail:(function
      | Lexclave body -> loop body
      | _ -> ()
    ) ~non_tail:(fun _ -> ()) lam
  and loop = function
    | Lvar _ | Lmutvar _ | Lconst _ -> ()

    | Lfunction {mode=Alloc_heap} -> ()
    | Lfunction {mode=Alloc_local} -> raise Exit

    | Lapply {ap_mode=Alloc_local}
    | Lsend (_,_,_,_,_,Alloc_local,_,_) -> raise Exit

    | Lprim (prim, args, _) ->
       begin match Lambda.primitive_may_allocate prim with
       | Some Alloc_local -> raise Exit
       | None | Some Alloc_heap ->
          List.iter loop args
       end
    | Lregion (body, _layout) ->
       (* [body] might allocate in the parent region because of exclave, and thus
          [Lregion body] might allocate in the current region *)
      loop_region body
    | Lexclave _body ->
      (* [_body] might do local allocations, but not in the current region;
        rather, it's in the parent region *)
      ()
    | Lwhile {wh_cond; wh_body} -> loop wh_cond; loop wh_body
    | Lfor {for_from; for_to; for_body} -> loop for_from; loop for_to; loop for_body
    | ( Lapply _ | Llet _ | Lmutlet _ | Lletrec _ | Lswitch _ | Lstringswitch _
      | Lstaticraise _ | Lstaticcatch _ | Ltrywith _
      | Lifthenelse _ | Lsequence _ | Lassign _ | Lsend _
      | Levent _ | Lifused _) as lam ->
       Lambda.iter_head_constructor loop lam
  in
  if not Config.stack_allocation then false
  else begin
    match loop lam with
    | () -> false
    | exception Exit -> true
  end

let maybe_region get_layout lam =
  let rec remove_tail_markers_and_exclave = function
    | Lapply ({ap_region_close = Rc_close_at_apply} as ap) ->
       Lapply ({ap with ap_region_close = Rc_normal})
    | Lsend (k, lmet, lobj, largs, Rc_close_at_apply, mode, loc, layout) ->
       Lsend (k, lmet, lobj, largs, Rc_normal, mode, loc, layout)
    | Lregion _ as lam -> lam
    | Lexclave lam -> lam
    | lam ->
       Lambda.shallow_map ~tail:remove_tail_markers_and_exclave ~non_tail:Fun.id lam
  in
  if not Config.stack_allocation then lam
  else if may_allocate_in_region lam then Lregion (lam, get_layout ())
  else remove_tail_markers_and_exclave lam

let maybe_region_layout layout lam =
  maybe_region (fun () -> layout) lam

let maybe_region_exp exp lam =
  maybe_region (fun () -> layout_exp exp) lam

(* Push the default values under the functional abstractions *)

let wrap_bindings bindings exp =
  List.fold_left
    (fun exp binds ->
      {exp with exp_desc = Texp_let(Nonrecursive, binds, exp)})
    exp bindings

let rec trivial_pat pat =
  match pat.pat_desc with
    Tpat_var _
  | Tpat_any -> true
  | Tpat_alias (p, _, _, _) ->
      trivial_pat p
  | Tpat_construct (_, cd, [], _) ->
      not cd.cstr_generalized && cd.cstr_consts = 1 && cd.cstr_nonconsts = 0
  | Tpat_tuple patl ->
      List.for_all (fun (_, p) -> trivial_pat p) patl
  | _ -> false

let rec push_defaults loc bindings use_lhs arg_mode cases partial warnings =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label; param; cases; partial;
                                        region; curry; warnings; arg_mode; alloc_mode } }
        as exp}] when bindings = [] || trivial_pat pat ->
      let cases = push_defaults exp.exp_loc bindings false arg_mode cases partial warnings in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function { arg_label; param; cases;
          partial; region; curry; warnings; arg_mode; alloc_mode }}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{Parsetree.attr_name = {txt="#default"};_}];
             exp_desc = Texp_let
               (Nonrecursive, binds,
                ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (binds :: bindings) true
                   arg_mode [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial warnings
  | [{c_lhs=pat; c_guard=None; c_rhs=exp} as case]
    when use_lhs || trivial_pat pat && exp.exp_desc <> Texp_unreachable ->
      [{case with c_rhs = wrap_bindings bindings exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let mode = Value_mode.of_alloc arg_mode in
      let param = Typecore.name_cases "param" cases in
      let desc =
        {val_type = pat.pat_type; val_kind = Val_reg;
         val_attributes = []; Types.val_loc = Location.none;
         val_uid = Types.Uid.internal_not_actually_unique; }
      in
      let env = Env.add_value ~mode param desc exp.exp_env in
      let name = Ident.name param in
      let exp =
        let cases =
          let pure_case ({c_lhs; _} as case) =
            {case with c_lhs = as_computation_pattern c_lhs} in
          List.map pure_case cases in
        { exp with exp_loc = loc; exp_env = env; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_env = env; exp_desc =
              Texp_ident
                (Path.Pident param, mknoloc (Longident.Lident name),
                 desc, Id_value)},
             Sort.value,
             (* CR layouts v2: Value here will changes when functions take other
                layouts.  Maybe we need a sort in [Typedtree.case]? *)
             cases, partial) }
      in
      [{c_lhs = {pat with pat_desc = Tpat_var (param, mknoloc name, mode)};
        c_guard = None; c_rhs= wrap_bindings bindings exp}]
  | _ ->
      cases

let push_defaults loc = push_defaults loc [] false

(* Insertion of debugging events *)

let event_before ~scopes exp lam =
  Translprim.event_before (of_location ~scopes exp.exp_loc) exp lam

let event_after ~scopes exp lam =
  Translprim.event_after (of_location ~scopes exp.exp_loc) exp lam

let event_function ~scopes exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = of_location ~scopes exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = exp.exp_env}))
  else
    lam None

(* Assertions *)

let assert_failed ~scopes exp =
  let slot =
    transl_extension_path Loc_unknown
      Env.initial_safe_string Predef.path_assert_failure
  in
  let loc = exp.exp_loc in
  let (fname, line, char) =
    Location.get_pos_info loc.Location.loc_start
  in
  let loc = of_location ~scopes exp.exp_loc in
  Lprim(Praise Raise_regular, [event_after ~scopes exp
    (Lprim(Pmakeblock(0, Immutable, None, alloc_heap),
          [slot;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, exp.exp_loc, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], loc))], loc)
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let rec iter_exn_names f pat =
  match pat.pat_desc with
  | Tpat_var (id, _, _) -> f id
  | Tpat_alias (p, id, _, _) ->
      f id;
      iter_exn_names f p
  | _ -> ()

let transl_ident loc env ty path desc kind =
  match desc.val_kind, kind with
  | Val_prim p, Id_prim poly_mode ->
      Translprim.transl_primitive loc p env ty ~poly_mode (Some path)
  | Val_anc _, Id_value ->
      raise(Error(to_location loc, Free_super_var))
  | (Val_reg | Val_self _), Id_value ->
      transl_value_path loc env path
  |  _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"

let can_apply_primitive p pmode pos args =
  let is_omitted = function
    | Arg _ -> false
    | Omitted _ -> true
  in
  if List.exists (fun (_, arg) -> is_omitted arg) args then false
  else begin
    let nargs = List.length args in
    if nargs = p.prim_arity then true
    else if nargs < p.prim_arity then false
    else if pos <> Typedtree.Tail then true
    else begin
      let return_mode = Ctype.prim_mode pmode p.prim_native_repr_res in
      is_heap_mode (transl_alloc_mode return_mode)
    end
  end

(* CR layouts v2: Invariant - this is only called on values.  Relax that. *)
let rec transl_exp ~scopes e =
  transl_exp1 ~scopes ~in_new_scope:false e

(* ~in_new_scope tracks whether we just opened a new scope.

   We go to some trouble to avoid introducing many new anonymous function
   scopes, as `let f a b = ...` is desugared to several Pexp_fun.
*)
(* CR layouts v2: Invariant - this is only called on values.  Relax that. *)
and transl_exp1 ~scopes ~in_new_scope e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 ~scopes ~in_new_scope  e else
  Translobj.oo_wrap e.exp_env true (transl_exp0 ~scopes ~in_new_scope) e

and transl_exp0 ~in_new_scope ~scopes e =
  match e.exp_desc with
  | Texp_ident(path, _, desc, kind) ->
      transl_ident (of_location ~scopes e.exp_loc)
        e.exp_env e.exp_type path desc kind
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      let body_layout = layout_exp body in
      transl_let ~scopes rec_flag pat_expr_list
        body_layout (event_before ~scopes body (transl_exp ~scopes body))
  | Texp_function { arg_label = _; param; cases; partial;
                    region; curry; warnings; arg_mode; alloc_mode } ->
      (* CR ncourant: it would be better if we had [arg_layout] here *)
      let arg_layout =
        match is_function_type e.exp_env e.exp_type with
        | None -> Misc.fatal_error "Translcore.transl_exp0: Type of a function is not a function type"
        | Some (arg_type, _) ->
            Typeopt.layout e.exp_env e.exp_loc arg_type
      in
      let scopes =
        if in_new_scope then scopes
        else enter_anonymous_function ~scopes
      in
      transl_function ~scopes e alloc_mode param arg_mode arg_layout cases partial warnings region curry
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p},
                                       Id_prim pmode);
                exp_type = prim_type; } as funct, oargs, pos, alloc_mode)
    when can_apply_primitive p pmode pos oargs ->
      let argl, extra_args = cut p.prim_arity oargs in
      let arg_exps =
         List.map (function _, Arg x -> x | _ -> assert false) argl
      in
      let args = transl_list ~scopes arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let position =
        if extra_args = [] then transl_apply_position pos
        else Rc_normal
      in
      let lam =
        Translprim.transl_primitive_application
          (of_location ~scopes e.exp_loc) p e.exp_env prim_type pmode
          path prim_exp args arg_exps position
      in
      if extra_args = [] then lam
      else begin
        let tailcall = Translattribute.get_tailcall_attribute funct in
        let inlined = Translattribute.get_inlined_attribute funct in
        let specialised = Translattribute.get_specialised_attribute funct in
        let position = transl_apply_position pos in
        let mode = transl_alloc_mode alloc_mode in
        let result_layout = layout_exp e in
        event_after ~scopes e
          (transl_apply ~scopes ~tailcall ~inlined ~specialised ~position ~mode
             ~result_layout lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs, position, alloc_mode) ->
      let tailcall = Translattribute.get_tailcall_attribute funct in
      let inlined = Translattribute.get_inlined_attribute funct in
      let specialised = Translattribute.get_specialised_attribute funct in
      let result_layout = layout_exp e in
      let position = transl_apply_position position in
      let mode = transl_alloc_mode alloc_mode in
      event_after ~scopes e
        (transl_apply ~scopes ~tailcall ~inlined ~specialised ~result_layout
           ~position ~mode (transl_exp ~scopes funct)
           oargs (of_location ~scopes e.exp_loc))
  | Texp_match(arg, sort, pat_expr_list, partial) ->
      transl_match ~scopes e arg sort pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_cases "exn" pat_expr_list in
      let layout = layout_exp e in
      Ltrywith(transl_exp ~scopes body, id,
               Matching.for_trywith ~scopes layout e.exp_loc (Lvar id)
                 (transl_cases_try ~scopes pat_expr_list),
               layout)
  | Texp_tuple (el, alloc_mode) -> 
      (* CR labeled tuples: this assumes that the orders of [Tpat_tuple]s match
         their type. Double-check this upon adding reordering *)
      let ll, shape = transl_list_with_shape ~scopes (List.map snd el) in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape,
                         transl_alloc_mode alloc_mode),
              ll,
              (of_location ~scopes e.exp_loc))
      end
  | Texp_construct(_, cstr, args, alloc_mode) ->
      let ll, shape = transl_list_with_shape ~scopes args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag, cstr.cstr_repr with
      | Ordinary {runtime_tag}, _ when cstr.cstr_constant ->
          (* CR layouts v5: This could have void args, but for now we've ruled
             that out with the layout check in transl_list_with_shape *)
          Lconst(const_int runtime_tag)
      | Ordinary _, Variant_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Ordinary {runtime_tag}, Variant_boxed _ ->
          begin try
            Lconst(Const_block(runtime_tag, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(runtime_tag, Immutable, Some shape,
                             transl_alloc_mode (Option.get alloc_mode)),
                  ll,
                  of_location ~scopes e.exp_loc)
          end
      | Extension (path, _), Variant_extensible ->
          let lam = transl_extension_path
                      (of_location ~scopes e.exp_loc) e.exp_env path in
          if cstr.cstr_constant
          then
            (* CR layouts v5: This could have void args, but for now we've ruled
               that out with the layout check in transl_list_with_shape. *)
            lam
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape),
                             transl_alloc_mode (Option.get alloc_mode)),
                  lam :: ll, of_location ~scopes e.exp_loc)
      | Extension _, (Variant_boxed _ | Variant_unboxed)
      | Ordinary _, Variant_extensible -> assert false
      end
  | Texp_extension_constructor (_, path) ->
      transl_extension_path (of_location ~scopes e.exp_loc) e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(const_int tag)
      | Some (arg, alloc_mode) ->
          let lam = transl_exp ~scopes arg in
          try
            Lconst(Const_block(0, [const_int tag;
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None,
                             transl_alloc_mode alloc_mode),
                  [Lconst(const_int tag); lam],
                  of_location ~scopes e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression; alloc_mode} ->
      transl_record ~scopes e.exp_loc e.exp_env
        (Option.map transl_alloc_mode alloc_mode)
        fields representation extended_expression
  | Texp_field(arg, _, lbl, alloc_mode) ->
      let targ = transl_exp ~scopes arg in
      let sem =
        match lbl.lbl_mut with
        | Immutable -> Reads_agree
        | Mutable -> Reads_vary
      in
      begin match lbl.lbl_repres with
          Record_boxed _ | Record_inlined (_, Variant_boxed _) ->
          Lprim (Pfield (lbl.lbl_pos, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed | Record_inlined (_, Variant_unboxed) -> targ
        | Record_float ->
          let mode = transl_alloc_mode (Option.get alloc_mode) in
          Lprim (Pfloatfield (lbl.lbl_pos, sem, mode), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_inlined (_, Variant_extensible) ->
          Lprim (Pfield (lbl.lbl_pos + 1, sem), [targ],
                 of_location ~scopes e.exp_loc)
      end
  | Texp_setfield(arg, arg_mode, id, lbl, newval) ->
      layout_must_be_value id.loc lbl.lbl_layout;
      let mode =
        Assignment (transl_modify_mode arg_mode)
      in
      let access =
        match lbl.lbl_repres with
          Record_boxed _
        | Record_inlined (_, Variant_boxed _) ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, mode)
        | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
          assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, mode)
        | Record_inlined (_, Variant_extensible) ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, mode)
      in
      Lprim(access, [transl_exp ~scopes arg; transl_exp ~scopes newval],
            of_location ~scopes e.exp_loc)
  | Texp_array (amut, expr_list, alloc_mode) ->
      let mode = transl_alloc_mode alloc_mode in
      let kind = array_kind e in
      let ll = transl_list ~scopes expr_list in
      let loc = of_location ~scopes e.exp_loc in
      let makearray mutability =
        Lprim (Pmakearray (kind, mutability, mode), ll, loc)
      in
      let duparray_to_mutable array =
        Lprim (Pduparray (kind, Mutable), [array], loc)
      in
      let imm_array = makearray Immutable in
      let lambda_arr_mut : Lambda.mutable_flag =
        match (amut : Asttypes.mutable_flag) with
        | Mutable   -> Mutable
        | Immutable -> Immutable
      in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if amut = Asttypes.Mutable &&
           List.length ll <= use_dup_for_constant_mutable_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        (* Pduparray only works in Alloc_heap mode *)
        if is_local_mode mode then raise Not_constant;
        begin match List.map extract_constant ll with
        | exception Not_constant
          when kind = Pfloatarray && amut = Asttypes.Mutable ->
            (* We cannot currently lift mutable [Pintarray] arrays safely in
               Flambda because [caml_modify] might be called upon them
               (e.g. from code operating on polymorphic arrays, or functions
               such as [caml_array_blit].
               To avoid having different Lambda code for bytecode/Closure
               vs. Flambda, we always generate [Pduparray] for mutable arrays
               here, and deal with it in [Bytegen] (or in the case of Closure,
               in [Cmmgen], which already has to handle [Pduparray Pmakearray
               Pfloatarray] in the case where the array turned out to be
               inconstant).
               When not [Pfloatarray], the exception propagates to the handler
               below. *)
            duparray_to_mutable imm_array
        | cl ->
            let const =
              if Config.flambda2 then
                imm_array
              else
                match kind with
                | Paddrarray | Pintarray ->
                  Lconst(Const_block(0, cl))
                | Pfloatarray ->
                  Lconst(Const_float_array(List.map extract_float cl))
                | Pgenarray ->
                  raise Not_constant    (* can this really happen? *)
            in
            match amut with
            | Mutable   -> duparray_to_mutable const
            | Immutable -> const
        end
      with Not_constant ->
        makearray lambda_arr_mut
      end
  | Texp_list_comprehension comp ->
      let loc = of_location ~scopes e.exp_loc in
      Transl_list_comprehension.comprehension
        ~transl_exp ~scopes ~loc comp
  | Texp_array_comprehension (_amut, comp) ->
      (* We can ignore mutability here since we've already checked in in the
         type checker; both mutable and immutable arrays are created the same
         way *)
      let loc = of_location ~scopes e.exp_loc in
      let array_kind = Typeopt.array_kind e in
      Transl_array_comprehension.comprehension
        ~transl_exp ~scopes ~loc ~array_kind comp
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  event_before ~scopes ifnot (transl_exp ~scopes ifnot),
                  layout_exp e)
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  lambda_unit,
                  Lambda.layout_unit)
  | Texp_sequence(expr1, layout, expr2) ->
      layout_must_not_be_void expr1.exp_loc layout;
      Lsequence(transl_exp ~scopes expr1,
                event_before ~scopes expr2 (transl_exp ~scopes expr2))
  | Texp_while {wh_body; wh_body_layout; wh_cond} ->
      layout_must_not_be_void wh_body.exp_loc wh_body_layout;
      let cond = transl_exp ~scopes wh_cond in
      let body = transl_exp ~scopes wh_body in
      Lwhile {
        wh_cond = maybe_region_layout layout_int cond;
        wh_body = event_before ~scopes wh_body
                    (maybe_region_layout layout_unit body);
      }
  | Texp_for {for_id; for_from; for_to; for_dir; for_body; for_body_layout} ->
      layout_must_not_be_void for_body.exp_loc for_body_layout;
      let body = transl_exp ~scopes for_body in
      Lfor {
        for_id;
        for_from = transl_exp ~scopes for_from;
        for_to = transl_exp ~scopes for_to;
        for_dir;
        for_body = event_before ~scopes for_body
                     (maybe_region_layout layout_unit body);
      }
  | Texp_send(expr, met, pos, alloc_mode) ->
      let lam =
        let pos = transl_apply_position pos in
        let mode = transl_alloc_mode alloc_mode in
        let loc = of_location ~scopes e.exp_loc in
        let layout = layout_exp e in
        match met with
        | Tmeth_val id ->
            let obj = transl_exp ~scopes expr in
            Lsend (Self, Lvar id, obj, [], pos, mode, loc, layout)
        | Tmeth_name nm ->
            let obj = transl_exp ~scopes expr in
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, pos, mode, loc, layout)
        | Tmeth_ancestor(meth, path_self) ->
            let self = transl_value_path loc e.exp_env path_self in
            Lapply {ap_loc = loc;
                    ap_func = Lvar meth;
                    ap_args = [self];
                    ap_result_layout = layout;
                    ap_mode = mode;
                    ap_region_close = pos;
                    ap_probe = None;
                    ap_tailcall = Default_tailcall;
                    ap_inlined = Default_inlined;
                    ap_specialised = Default_specialise}
      in
      event_after ~scopes e lam
  | Texp_new (cl, {Location.loc=loc}, _, pos) ->
      let loc = of_location ~scopes loc in
      let pos = transl_apply_position pos in
      Lapply{
        ap_loc=loc;
        ap_func=
          Lprim(Pfield (0, Reads_vary),
              [transl_class_path loc e.exp_env cl], loc);
        ap_args=[lambda_unit];
        ap_result_layout=layout_exp e;
        ap_region_close=pos;
        ap_mode=alloc_heap;
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inlined;
        ap_specialised=Default_specialise;
        ap_probe=None;
      }
  | Texp_instvar(path_self, path, _) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      Lprim(Pfield_computed Reads_vary, [self; var], loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      transl_setinstvar ~scopes loc self var expr
  | Texp_override(path_self, modifs) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let cpy = Ident.create_local "copy" in
      Llet(Strict, Lambda.layout_object, cpy,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=Translobj.oo_prim "copy";
             ap_args=[self];
             ap_result_layout=Lambda.layout_object;
             ap_region_close=Rc_normal;
             ap_mode=alloc_heap;
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inlined;
             ap_specialised=Default_specialise;
             ap_probe=None;
           },
           List.fold_right
             (fun (id, _, expr) rem ->
                Lsequence(transl_setinstvar ~scopes Loc_unknown
                            (Lvar cpy) (Lvar id) expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(None, loc, Mp_present, modl, body) ->
      let lam = !transl_module ~scopes Tcoerce_none None modl in
      Lsequence(Lprim(Pignore, [lam], of_location ~scopes loc.loc),
                transl_exp ~scopes body)
  | Texp_letmodule(Some id, _loc, Mp_present, modl, body) ->
      let defining_expr =
        let mod_scopes = enter_module_definition ~scopes id in
        !transl_module ~scopes:mod_scopes Tcoerce_none None modl
      in
      Llet(Strict, Lambda.layout_module, id, defining_expr, transl_exp ~scopes body)
  | Texp_letmodule(_, _, Mp_absent, _, body) ->
      transl_exp ~scopes body
  | Texp_letexception(cd, body) ->
      Llet(Strict, Lambda.layout_block,
           cd.ext_id, transl_extension_constructor ~scopes e.exp_env None cd,
           transl_exp ~scopes body)
  | Texp_pack modl ->
      !transl_module ~scopes Tcoerce_none None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _, _)} ->
      assert_failed ~scopes e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else begin
        Lifthenelse
          (transl_exp ~scopes cond,
           lambda_unit,
           assert_failed ~scopes e,
           Lambda.layout_unit)
      end
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* A constant expr (of type <> float if [Config.flat_float_array] is
           true) gets compiled as itself. *)
         transl_exp ~scopes e
      | `Float_that_cannot_be_shortcut ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float
             and Config.flat_float_array is true. *)
         Lprim(Pmakeblock(Obj.forward_tag, Immutable, None,
                          alloc_heap),
                [transl_exp ~scopes e], of_location ~scopes e.exp_loc)
      | `Identifier `Forward_value ->
         (* CR-someday mshinwell: Consider adding a new primitive
            that expresses the construction of forward_tag blocks.
            We need to use [Popaque] here to prevent unsound
            optimisation in Flambda, but the concept of a mutable
            block doesn't really match what is going on here.  This
            value may subsequently turn into an immediate... *)
         Lprim (Popaque Lambda.layout_lazy,
                [Lprim(Pmakeblock(Obj.forward_tag, Immutable, None,
                                  alloc_heap),
                       [transl_exp ~scopes e],
                       of_location ~scopes e.exp_loc)],
                of_location ~scopes e.exp_loc)
      | `Identifier `Other ->
         transl_exp ~scopes e
      | `Other ->
         (* other cases compile to a lazy block holding a function.  The
            typechecker enforces that e has layout value.  *)
         let scopes = enter_lazy ~scopes in
         let fn = lfunction ~kind:(Curried {nlocal=0})
                            ~params:[Ident.create_local "param", Lambda.layout_unit]
                            ~return:Lambda.layout_lazy_contents
                            ~attr:default_function_attribute
                            ~loc:(of_location ~scopes e.exp_loc)
                            ~mode:alloc_heap
                            ~region:true
                            ~body:(maybe_region_layout
                                     Lambda.layout_lazy_contents
                                     (transl_exp ~scopes e))
         in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, None, alloc_heap), [fn],
                of_location ~scopes e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create_local "object" in
      !transl_object ~scopes cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_letop{let_; ands; param; body; partial; warnings} ->
      event_after ~scopes e
        (transl_letop ~scopes e.exp_loc e.exp_env let_ ands
           param body partial warnings)
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))
  | Texp_open (od, e) ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to
          do it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> transl_exp ~scopes e
      | _ ->
          let oid = Ident.create_local "open" in
          let body, _ =
            (* CR layouts v2: Currently we only allow values at the top of a
               module.  When that changes, some adjustments may be needed
               here. *)
            List.fold_left (fun (body, pos) id ->
              Llet(Alias, Lambda.layout_module_field, id,
                   Lprim(mod_field pos, [Lvar oid],
                         of_location ~scopes od.open_loc), body),
              pos + 1
            ) (transl_exp ~scopes e, 0)
              (bound_value_identifiers od.open_bound_items)
          in
          Llet(pure, Lambda.layout_module, oid,
               !transl_module ~scopes Tcoerce_none None od.open_expr, body)
      end
  | Texp_probe {name; handler=exp; enabled_at_init} ->
    if !Clflags.native_code && !Clflags.probes then begin
      let lam = transl_exp ~scopes exp in
      let map =
        Ident.Set.fold (fun v acc -> Ident.Map.add v (Ident.rename v) acc)
          (free_variables lam)
          Ident.Map.empty
      in
      let arg_idents, param_idents = Ident.Map.bindings map |> List.split in
      List.iter (fun id ->
        (* CR layouts: The probe hack.

           The lambda translation wants to know the layouts of all function
           parameters.  Here we're building a function whose arguments are all
           the free variables in a probe handler.  At the moment, we just check
           that they are all values.

           It's really hacky to be doing this kind of layout check this late.
           The middle-end folks have plans to eliminate the need for it by
           reworking the way probes are compiled.  For that reason, I haven't
           bothered to give a particularly good error or handle the Not_found
           case from env.

           (We could probably calculate the layouts of these variables here
           rather than requiring them all to be value, but that would be even
           more hacky, and in any event we don't yet want to connect the
           front-end layout support to the middle-end layout support).  *)
        (* CR layouts v2: if we get close to releasing other layout somebody
           actually might put in a probe, check with the middle-end team about
           the status of fixing this. *)
        let path = Path.Pident id in
        match
          Subst.Lazy.force_value_description (Env.find_value path e.exp_env)
        with
        | {val_type; _} -> begin
            match
              Ctype.check_type_layout
                e.exp_env (Ctype.correct_levels val_type)
                (Layout.value ~why:Probe)
            with
            | Ok _ -> ()
            | Error _ -> raise (Error (e.exp_loc, Bad_probe_layout id))
          end
        | exception Not_found ->
          (* Might be a module, which are all values.  Otherwise raise. *)
          ignore (Env.find_module_lazy path e.exp_env)
      ) arg_idents;
      let body = Lambda.rename map lam in
      let attr =
        { inline = Never_inline;
          specialise = Always_specialise;
          local = Never_local;
          check = Default_check;
          loop = Never_loop;
          is_a_functor = false;
          stub = false;
          poll = Default_poll;
          tmc_candidate = false;
        } in
      let funcid = Ident.create_local ("probe_handler_" ^ name) in
      let layout = layout_exp exp in
      (* CR ncourant: how do we get the layouts for the free variables? *)
      let handler =
        let scopes = enter_value_definition ~scopes funcid in
        lfunction
          ~kind:(Curried {nlocal=0})
          ~params:(List.map (fun v -> v, Lambda.layout_top) param_idents)
          ~return:layout
          ~body
          ~loc:(of_location ~scopes exp.exp_loc)
          ~attr
          ~mode:alloc_heap
          ~region:true
      in
      let app =
        { ap_func = Lvar funcid;
          ap_args = List.map (fun id -> Lvar id) arg_idents;
          ap_result_layout = layout;
          ap_region_close = Rc_normal;
          ap_mode = alloc_heap;
          ap_loc = of_location e.exp_loc ~scopes;
          ap_tailcall = Default_tailcall;
          ap_inlined = Never_inlined;
          ap_specialised = Always_specialise;
          ap_probe = Some {name; enabled_at_init};
        }
      in
      begin match Config.flambda || Config.flambda2 with
      | true ->
          Llet(Strict, Lambda.layout_function, funcid, handler, Lapply app)
      | false ->
        (* Needs to be lifted to top level manually here,
           because functions that contain other function declarations
           are not inlined by Closure. For example, adding a probe into
           the body of function foo will prevent foo from being inlined
           into another function. *)
        probe_handlers := (funcid, handler)::!probe_handlers;
        Lapply app
      end
    end else begin
      lambda_unit
    end
  | Texp_probe_is_enabled {name} ->
    if !Clflags.native_code && !Clflags.probes then
      Lprim(Pprobe_is_enabled {name}, [], of_location ~scopes e.exp_loc)
    else
      lambda_unit
  | Texp_exclave e ->
    let l = transl_exp ~scopes e in
    if Config.stack_allocation then Lexclave l
    else l

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

(* List elements must have layout value (this does not check). *)
and transl_list ~scopes expr_list =
  List.map (transl_exp ~scopes) expr_list

(* Will raise if a list element has a non-value layout. *)
and transl_list_with_layout ~scopes expr_list =
  List.map (fun exp -> transl_exp ~scopes exp, layout_exp exp) expr_list

(* Will raise if a list element has a non-value layout. *)
and transl_list_with_shape ~scopes expr_list =
  let transl_with_shape e =
    let shape = Lambda.must_be_value (layout_exp e) in
    transl_exp ~scopes e, shape
  in
  List.split (List.map transl_with_shape expr_list)

(* Will raise if rhs has a non-value layout *)
and transl_guard ~scopes guard rhs =
  let layout = layout_exp rhs in
  let expr = event_before ~scopes rhs (transl_exp ~scopes rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before ~scopes cond
        (Lifthenelse(transl_exp ~scopes cond, expr, staticfail, layout))

(* will raise if c_rhs has a non-value layout *)
and transl_case ~scopes {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard ~scopes c_guard c_rhs

(* will raise if any cases have a non-value rhs *)
and transl_cases ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case ~scopes) cases

(* will raise if the case has a non-value rhs *)
and transl_case_try ~scopes {c_lhs; c_guard; c_rhs} =
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () -> c_lhs, transl_guard ~scopes c_guard c_rhs)
    ~always:(fun () ->
        iter_exn_names Translprim.remove_exception_ident c_lhs)

(* will raise if any cases have a non-value rhs *)
and transl_cases_try ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case_try ~scopes) cases

(* will raise if any cases have a non-value rhs *)
and transl_tupled_cases ~scopes patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map (fun (patl, guard, expr) -> (patl, transl_guard ~scopes guard expr))
    patl_expr_list

and transl_apply ~scopes
      ?(tailcall=Default_tailcall)
      ?(inlined = Default_inlined)
      ?(specialised = Default_specialise)
      ?(position=Rc_normal)
      ?(mode=alloc_heap)
      ~result_layout
      lam sargs loc
  =
  let lapply funct args loc pos mode result_layout =
    match funct, pos with
    | Lsend((Self | Public) as k, lmet, lobj, [], _, _, _, _), _ ->
        Lsend(k, lmet, lobj, args, pos, mode, loc, result_layout)
    | Lsend(Cached, lmet, lobj, ([_; _] as largs), _, _, _, _), _ ->
        Lsend(Cached, lmet, lobj, largs @ args, pos, mode, loc, result_layout)
    | Lsend(k, lmet, lobj, largs, (Rc_normal | Rc_nontail), _, _, _),
      (Rc_normal | Rc_nontail) ->
        Lsend(k, lmet, lobj, largs @ args, pos, mode, loc, result_layout)
    | Levent(
      Lsend((Self | Public) as k, lmet, lobj, [], _, _, _, _), _), _ ->
        Lsend(k, lmet, lobj, args, pos, mode, loc, result_layout)
    | Levent(
      Lsend(Cached, lmet, lobj, ([_; _] as largs), _, _, _, _), _), _ ->
        Lsend(Cached, lmet, lobj, largs @ args, pos, mode, loc, result_layout)
    | Levent(
      Lsend(k, lmet, lobj, largs, (Rc_normal | Rc_nontail), _, _, _), _),
      (Rc_normal | Rc_nontail) ->
        Lsend(k, lmet, lobj, largs @ args, pos, mode, loc, result_layout)
    | Lapply ({ ap_region_close = (Rc_normal | Rc_nontail) } as ap),
      (Rc_normal | Rc_nontail) ->
        Lapply
          {ap with ap_args = ap.ap_args @ args; ap_loc = loc;
                   ap_region_close = pos; ap_mode = mode}
    | lexp, _ ->
        Lapply {
          ap_loc=loc;
          ap_func=lexp;
          ap_args=args;
          ap_result_layout=result_layout;
          ap_region_close=pos;
          ap_mode=mode;
          ap_tailcall=tailcall;
          ap_inlined=inlined;
          ap_specialised=specialised;
          ap_probe=None;
        }
  in
  let rec build_apply lam args loc pos ap_mode = function
    | Omitted { mode_closure; mode_arg; mode_ret; ty_arg; ty_env } :: l ->
        assert (pos = Rc_normal);
        let defs = ref [] in
        let protect name (lam, layout) =
          match lam with
            Lvar _ | Lconst _ -> (lam, layout)
          | _ ->
              let id = Ident.create_local name in
              defs := (id, layout, lam) :: !defs;
              (Lvar id, layout)
        in
        let lam =
          if args = [] then
            lam
          else
            lapply lam (List.rev args) loc pos ap_mode layout_function
        in
        let handle, _ = protect "func" (lam, layout_function) in
        let l =
          List.map
            (fun arg ->
               match arg with
               | Omitted _ -> arg
               | Arg arg -> Arg (protect "arg" arg))
            l
        in
        let id_arg = Ident.create_local "param" in
        let body =
          let loc = map_scopes enter_partial_or_eta_wrapper loc in
          let mode = transl_alloc_mode mode_closure in
          let arg_mode = transl_alloc_mode mode_arg in
          let ret_mode = transl_alloc_mode mode_ret in
          let body = build_apply handle [Lvar id_arg] loc Rc_normal ret_mode l in
          let nlocal =
            match join_mode mode (join_mode arg_mode ret_mode) with
            | Alloc_local -> 1
            | Alloc_heap -> 0
          in
          let region =
            match ret_mode with
            | Alloc_local -> false
            | Alloc_heap -> true
          in
          let layout_arg = Typeopt.layout ty_env (to_location loc) ty_arg in
          lfunction ~kind:(Curried {nlocal}) ~params:[id_arg, layout_arg]
                    ~return:result_layout ~body ~mode ~region
                    ~attr:default_stub_attribute ~loc
        in
        List.fold_right
          (fun (id, layout, lam) body -> Llet(Strict, layout, id, lam, body))
          !defs body
    | Arg (arg, _) :: l -> build_apply lam (arg :: args) loc pos ap_mode l
    | [] -> lapply lam (List.rev args) loc pos ap_mode result_layout
  in
  let args =
    List.map
      (fun (_, arg) ->
         match arg with
         | Omitted _ as arg -> arg
         | Arg exp -> Arg (transl_exp ~scopes exp, layout_exp exp))
      sargs
  in
  build_apply lam [] loc position mode args

and transl_curried_function
      ~scopes loc return
      repr ~region ~curry partial warnings (param:Ident.t) arg_layout cases =
  let max_arity = Lambda.max_arity () in
  let rec loop ~scopes loc return ~arity ~region ~curry
            partial warnings (param:Ident.t) arg_layout cases =
    match curry, cases with
      More_args {partial_mode},
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp_desc =
                 Texp_function
                   { arg_label = _; param = param'; cases = cases';
                     partial = partial'; region = region';
                     curry = curry';
                     warnings = warnings' };
               exp_env; exp_type; exp_loc }}]
      when arity < max_arity ->
      (* Lfunctions must have local returns after the first local arg/ret *)
      if Parmatch.inactive ~partial pat
      then
        let partial_mode = transl_alloc_mode partial_mode in
        let return_layout = function_return_layout exp_env exp_loc exp_type in
        let arg_layout' =
          match is_function_type exp_env exp_type with
          | None ->
              Misc.fatal_error "Translcore.transl_curried_function: \
                                Type of Texp_function is not function"
          | Some (lhs, _) -> layout exp_env exp_loc lhs
        in
        let ((fnkind, params, return, region), body) =
          loop ~scopes exp_loc return_layout
            ~arity:(arity + 1) ~region:region' ~curry:curry'
            partial' warnings' param' arg_layout' cases'
        in
        let fnkind =
          match partial_mode, fnkind with
          | _, Tupled ->
             (* arity > 1 prevents this *)
             assert false
          | Alloc_heap, (Curried _ as c) -> c
          | Alloc_local, Curried {nlocal} ->
             (* all subsequent curried arrows should be local *)
             assert (nlocal = List.length params);
             Curried {nlocal = nlocal + 1}
        in
        ((fnkind, (param, arg_layout) :: params, return, region),
         Matching.for_function ~scopes return_layout loc None (Lvar param, arg_layout)
           [pat, body] partial)
      else begin
        begin match partial with
        | Total ->
            let prev = Warnings.backup () in
            Warnings.restore warnings;
            Location.prerr_warning pat.pat_loc
              Match_on_mutable_state_prevent_uncurry;
            Warnings.restore prev
        | Partial -> ()
        end;
        transl_tupled_function ~scopes ~arity ~region ~curry
          loc return repr partial param arg_layout cases
      end
    | curry, cases ->
      transl_tupled_function ~scopes ~arity ~region ~curry
        loc return repr partial param arg_layout cases
  in
  loop ~scopes loc return ~arity:1 ~region ~curry
    partial warnings param arg_layout cases

and transl_tupled_function
      ~scopes ~arity ~region ~curry loc return
      repr partial (param:Ident.t) arg_layout cases =
  let partial_mode =
    match curry with
    | More_args {partial_mode} | Final_arg {partial_mode} ->
      transl_alloc_mode partial_mode
  in
  match partial_mode, cases with
  | Alloc_heap, {c_lhs={pat_desc = Tpat_tuple pl }} :: _
    when !Clflags.native_code
      && arity = 1
      && List.length pl <= (Lambda.max_arity ()) ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let kinds =
          match arg_layout with
          | Pvalue (Pvariant { consts = []; non_consts = [0, kinds] }) ->
              (* CR layouts v2: to change when we have non-value args. *)
              List.map (fun vk -> Pvalue vk) kinds
          | _ ->
              Misc.fatal_error
                "Translcore.transl_tupled_function: \
                 Argument should be a tuple, but couldn't get the kinds"
        in
        let tparams =
          List.map (fun kind -> Ident.create_local "param", kind) kinds
        in
        let params = List.map fst tparams in
        let body =
          Matching.for_tupled_function ~scopes loc return params
            (transl_tupled_cases ~scopes pats_expr_list) partial
        in
        let region = region || not (may_allocate_in_region body) in
        ((Tupled, tparams, return, region), body)
    with Matching.Cannot_flatten ->
      transl_function0 ~scopes loc ~region ~partial_mode
        return repr partial param arg_layout cases
      end
  | _ -> transl_function0 ~scopes loc ~region ~partial_mode
           return repr partial param arg_layout cases

and transl_function0
      ~scopes loc ~region ~partial_mode return
      repr partial (param:Ident.t) arg_layout cases =
    let body =
      Matching.for_function ~scopes return loc repr (Lvar param, arg_layout)
        (transl_cases ~scopes cases) partial
    in
    let region = region || not (may_allocate_in_region body) in
    let nlocal =
      if not region then 1
      else match partial_mode with
        | Alloc_local -> 1
        | Alloc_heap -> 0
    in
    ((Curried {nlocal}, [param, arg_layout], return, region), body)

and transl_function ~scopes e alloc_mode param arg_mode arg_layout cases partial warnings region curry =
  let mode = transl_alloc_mode alloc_mode in
  let ((kind, params, return, region), body) =
    event_function ~scopes e
      (function repr ->
         let pl = push_defaults e.exp_loc arg_mode cases partial warnings in
         let return_layout =
           function_return_layout e.exp_env e.exp_loc e.exp_type
         in
         transl_curried_function ~scopes e.exp_loc return_layout
           repr ~region ~curry partial warnings param arg_layout pl)
  in
  let attr = default_function_attribute in
  let loc = of_location ~scopes e.exp_loc in
  let body = if region then maybe_region_layout return body else body in
  let lam = lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~region in
  let attrs =
    (* Collect attributes from the Pexp_newtype node for locally abstract types.
       Otherwise we'd ignore the attribute in, e.g.;
           fun [@inline] (type a) x -> ...
    *)
    List.fold_left
      (fun attrs (extra_exp, _, extra_attrs) ->
         match extra_exp with
         | Texp_newtype _ -> extra_attrs @ attrs
         | (Texp_constraint _ | Texp_coerce _ | Texp_poly _) -> attrs)
      e.exp_attributes e.exp_extra
  in
  Translattribute.add_function_attributes lam e.exp_loc attrs

(* Like transl_exp, but used when a new scope was just introduced. *)
(* CR layouts v2: Invariant - this is only called on values.  Relax that. *)
and transl_scoped_exp ~scopes expr =
  transl_exp1 ~scopes ~in_new_scope:true expr

(* Decides whether a pattern binding should introduce a new scope. *)
(* CR layouts v2: Invariant - this is only called on values.  Relax that. *)
and transl_bound_exp ~scopes ~in_structure pat expr =
  let should_introduce_scope =
    match expr.exp_desc with
    | Texp_function _ -> true
    | _ when in_structure -> true
    | _ -> false in
  match pat_bound_idents pat with
  | (id :: _) when should_introduce_scope ->
     transl_scoped_exp ~scopes:(enter_value_definition ~scopes id) expr
  | _ -> transl_exp ~scopes expr

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let ~scopes ?(add_regions=false) ?(in_structure=false)
               rec_flag pat_expr_list body_kind =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_sort=sort; vb_attributes=attr; vb_loc}
        :: rem ->
          (* CR layouts v2: allow non-values.  Either remove this or replace
             with void-specific sanity check. *)
          sort_must_be_value ~why:Let_binding expr.exp_loc sort;
          let lam = transl_bound_exp ~scopes ~in_structure pat expr in
          let lam = Translattribute.add_function_attributes lam vb_loc attr in
          let lam = if add_regions then maybe_region_exp expr lam else lam in
          let mk_body = transl rem in
          fun body ->
            Matching.for_let ~scopes pat.pat_loc lam pat body_kind (mk_body body)
      in
      transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case
            {vb_expr=expr; vb_sort; vb_attributes; vb_loc; vb_pat} id =
        (* CR layouts v2: allow non-values.  Either remove this or replace
           with void-specific sanity check. *)
        sort_must_be_value ~why:Let_binding expr.exp_loc vb_sort;
        let lam = transl_bound_exp ~scopes ~in_structure vb_pat expr in
        let lam =
          Translattribute.add_function_attributes lam vb_loc vb_attributes
        in
        let lam = if add_regions then maybe_region_exp expr lam else lam in
        (id, lam) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Lletrec(lam_bds, body)

and transl_setinstvar ~scopes loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment modify_heap),
    [self; var; transl_exp ~scopes expr], loc)

(* CR layouts v5: Invariant - this is only called on values.  Relax that. *)
and transl_record ~scopes loc env mode fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  let on_heap = match mode with
    | None -> false (* unboxed is not on heap *)
    | Some m -> is_heap_mode m
  in
  if no_init || size < Config.max_young_wosize || not on_heap
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    (* CR layouts v2: currently we raise if a non-value field is detected.
       relax that. *)
    let init_id = Ident.create_local "init" in
    let lv =
      Array.mapi
        (fun i (lbl, definition) ->
           match definition with
           | Kept typ ->
               let field_kind = must_be_value (layout env lbl.lbl_loc typ) in
               let sem =
                 match lbl.lbl_mut with
                 | Immutable -> Reads_agree
                 | Mutable -> Reads_vary
               in
               let access =
                 match repres with
                   Record_boxed _ | Record_inlined (_, Variant_boxed _) ->
                   Pfield (i, sem)
                 | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
                   assert false
                 | Record_inlined (_, Variant_extensible) -> Pfield (i + 1, sem)
                 | Record_float ->
                    (* This allocation is always deleted,
                       so it's simpler to leave it Alloc_heap *)
                    Pfloatfield (i, sem, alloc_heap) in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_kind
           | Overridden (_lid, expr) ->
               let field_kind = must_be_value (layout_exp expr) in
               transl_exp ~scopes expr, field_kind)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut : Lambda.mutable_flag =
      if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Asttypes.Mutable) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_boxed _ -> Lconst(Const_block(0, cl))
        | Record_inlined (Ordinary {runtime_tag}, Variant_boxed _) ->
            Lconst(Const_block(runtime_tag, cl))
        | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
            Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_block(List.map extract_float cl))
        | Record_inlined (_, Variant_extensible)
        | Record_inlined (Extension _, _) ->
            raise Not_constant
      with Not_constant ->
        let loc = of_location ~scopes loc in
        match repres with
          Record_boxed _ ->
            Lprim(Pmakeblock(0, mut, Some shape, Option.get mode), ll, loc)
        | Record_inlined (Ordinary {runtime_tag}, Variant_boxed _) ->
            Lprim(Pmakeblock(runtime_tag, mut, Some shape, Option.get mode),
                  ll, loc)
        | Record_unboxed | Record_inlined (Ordinary _, Variant_unboxed) ->
            (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakefloatblock (mut, Option.get mode), ll, loc)
        | Record_inlined (Extension (path, _), Variant_extensible) ->
            let slot = transl_extension_path loc env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape), Option.get mode),
                  slot :: ll, loc)
        | Record_inlined (Extension _, (Variant_unboxed | Variant_boxed _))
        | Record_inlined (Ordinary _, Variant_extensible) ->
            assert false
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Lambda.layout_block, init_id,
                             transl_exp ~scopes init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create_local "newrecord" in
    let update_field cont (lbl, definition) =
      (* CR layouts v2: remove this check to allow non-value fields.  Even
         in the current version we can reasonably skip it because if we built
         the init record, we must have already checked for void. *)
      layout_must_be_value lbl.lbl_loc lbl.lbl_layout;
      match definition with
      | Kept _type -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_boxed _ | Record_inlined (_, Variant_boxed _) ->
                let ptr = maybe_pointer expr in
                Psetfield(lbl.lbl_pos, ptr, Assignment modify_heap)
            | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
                assert false
            | Record_float ->
                Psetfloatfield (lbl.lbl_pos, Assignment modify_heap)
            | Record_inlined (_, Variant_extensible) ->
                let pos = lbl.lbl_pos + 1 in
                let ptr = maybe_pointer expr in
                Psetfield(pos, ptr, Assignment modify_heap)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp ~scopes expr],
                          of_location ~scopes loc),
                    cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        assert (is_heap_mode (Option.get mode)); (* Pduprecord must be Alloc_heap and not unboxed *)
        Llet(Strict, Lambda.layout_block, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp ~scopes init_expr],
                   of_location ~scopes loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match ~scopes e arg sort pat_expr_list partial =
  let layout = layout_exp e in
  let rewrite_case (val_cases, exn_cases, static_handlers as acc)
        ({ c_lhs; c_guard; c_rhs } as case) =
    if c_rhs.exp_desc = Texp_unreachable then acc else
    let val_pat, exn_pat = split_pattern c_lhs in
    match val_pat, exn_pat with
    | None, None -> assert false
    | Some pv, None ->
        let val_case =
          transl_case ~scopes { case with c_lhs = pv }
        in
        val_case :: val_cases, exn_cases, static_handlers
    | None, Some pe ->
        let exn_case = transl_case_try ~scopes { case with c_lhs = pe } in
        val_cases, exn_case :: exn_cases, static_handlers
    | Some pv, Some pe ->
        assert (c_guard = None);
        let lbl  = next_raise_count () in
        let static_raise ids =
          Lstaticraise (lbl, List.map (fun id -> Lvar id) ids)
        in
        (* Simplif doesn't like it if binders are not uniq, so we make sure to
           use different names in the value and the exception branches. *)
        let ids_full = Typedtree.pat_bound_idents_full sort pv in
        let ids = List.map (fun (id, _, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, {Location.loc; _}, ty, _) ->
            id, Typeopt.layout pv.pat_env loc ty)
            ids_full
        in
        let vids = List.map Ident.rename ids in
        let pv = alpha_pat (List.combine ids vids) pv in
        (* Also register the names of the exception so Re-raise happens. *)
        iter_exn_names Translprim.add_exception_ident pe;
        let rhs =
          Misc.try_finally
            (fun () -> event_before ~scopes c_rhs
                         (transl_exp ~scopes c_rhs))
            ~always:(fun () ->
                iter_exn_names Translprim.remove_exception_ident pe)
        in
        (pv, static_raise vids) :: val_cases,
        (pe, static_raise ids) :: exn_cases,
        (lbl, ids_kinds, rhs) :: static_handlers
  in
  let val_cases, exn_cases, static_handlers =
    let x, y, z = List.fold_left rewrite_case ([], [], []) pat_expr_list in
    List.rev x, List.rev y, List.rev z
  in
  (* In presence of exception patterns, the code we generate for

       match <scrutinees> with
       | <val-patterns> -> <val-actions>
       | <exn-patterns> -> <exn-actions>

     looks like

       staticcatch
         (try (exit <val-exit> <scrutinees>)
          with <exn-patterns> -> <exn-actions>)
       with <val-exit> <val-ids> ->
          match <val-ids> with <val-patterns> -> <val-actions>

     In particular, the 'exit' in the value case ensures that the
     value actions run outside the try..with exception handler.
  *)
  let static_catch scrutinees val_ids handler =
    let id = Typecore.name_pattern "exn" (List.map fst exn_cases) in
    let static_exception_id = next_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, scrutinees), id,
                 Matching.for_trywith ~scopes layout e.exp_loc (Lvar id) exn_cases,
                 layout),
       (static_exception_id, val_ids),
       handler,
      layout)
  in
  let classic =
    match arg, exn_cases with
    (* CR labeled tuples: these cases assume that the orders of [Tpat_tuple]s
       match their type. Double-check this upon adding reordering *)
    | {exp_desc = Texp_tuple (argl, alloc_mode)}, [] ->
      assert (static_handlers = []);
      let mode = transl_alloc_mode alloc_mode in
      Matching.for_multiple_match ~scopes layout e.exp_loc
        (transl_list_with_layout ~scopes (List.map snd argl)) mode val_cases partial
    | {exp_desc = Texp_tuple (argl, alloc_mode)}, _ :: _ ->
        let val_ids =
          List.map
            (fun arg -> Typecore.name_pattern "val" [], layout_exp arg)
            (* CR labeled tuples: test this case (a match with normal and
               exception cases) such as with:
                 match ~~(~x:3, 42) with
                 | ~~(~x, y) -> ...
                 | exception Foo -> ...
            *)
            (List.map snd argl)
        in
        let lvars = List.map (fun (id, layout) -> Lvar id, layout) val_ids in
        let mode = transl_alloc_mode alloc_mode in
        static_catch (transl_list ~scopes (List.map snd argl)) val_ids
          (Matching.for_multiple_match ~scopes layout e.exp_loc
             lvars mode val_cases partial)
    | arg, [] ->
      assert (static_handlers = []);
      let k = layout_exp arg in
      Matching.for_function ~scopes layout e.exp_loc
        None (transl_exp ~scopes arg, k) val_cases partial
    | arg, _ :: _ ->
        let val_id = Typecore.name_pattern "val" (List.map fst val_cases) in
        let k = layout_exp arg in
        static_catch [transl_exp ~scopes arg] [val_id, k]
          (Matching.for_function ~scopes layout e.exp_loc
             None (Lvar val_id, k) val_cases partial)
  in
  List.fold_left (fun body (static_exception_id, val_ids, handler) ->
    Lstaticcatch (body, (static_exception_id, val_ids), handler, layout)
  ) classic static_handlers

and transl_letop ~scopes loc env let_ ands param case partial warnings =
  (* CR layouts: The typechecker is currently enforcing that everything here has
     layout value, but we might want to relax that when we allow non-value
     function args and returns, and then this code would need to be
     revisited. *)
  let rec loop prev_layout prev_lam = function
    | [] -> prev_lam
    | and_ :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op =
          transl_ident (of_location ~scopes and_.bop_op_name.loc) env
            and_.bop_op_type and_.bop_op_path and_.bop_op_val Id_value
        in
        let exp = transl_exp ~scopes and_.bop_exp in
        let right_layout = layout_exp and_.bop_exp in
        let result_layout =
          function2_return_layout env and_.bop_loc and_.bop_op_type
        in
        let lam =
          bind_with_layout Strict (right_id, right_layout) exp
            (Lapply{
               ap_loc = of_location ~scopes and_.bop_loc;
               ap_func = op;
               ap_args=[Lvar left_id; Lvar right_id];
               ap_result_layout = result_layout;
               ap_region_close=Rc_normal;
               ap_mode=alloc_heap;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inlined;
               ap_specialised = Default_specialise;
               ap_probe=None;
             })
        in
        bind_with_layout Strict (left_id, prev_layout) prev_lam (loop result_layout lam rest)
  in
  let op =
    transl_ident (of_location ~scopes let_.bop_op_name.loc) env
      let_.bop_op_type let_.bop_op_path let_.bop_op_val Id_value
  in
  let exp =
    loop (layout_exp let_.bop_exp) (transl_exp ~scopes let_.bop_exp) ands
  in
  let func =
    let arg_layout =
      match Typeopt.is_function_type env let_.bop_op_type with
      | None ->
          Misc.fatal_error
            "Translcore.transl_letop: letop should be a function"
      | Some (_, rhs) ->
          match Typeopt.is_function_type env rhs with
          | None ->
              Misc.fatal_error
                "Translcore.transl_letop: letop should have at least two arguments"
          | Some (lhs, _) ->
              match Typeopt.is_function_type env lhs with
              | None ->
                  Misc.fatal_error
                    "Translcore.transl_letop: letop second argument should be a function"
              | Some (arg_type, _) ->
                  Typeopt.layout env loc arg_type
    in
    let return_layout = layout_exp case.c_rhs in
    let curry = More_args { partial_mode = Alloc_mode.global } in
    let (kind, params, return, _region), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           transl_curried_function ~scopes case.c_rhs.exp_loc return_layout
             repr ~region:true ~curry partial warnings param arg_layout [case])
    in
    let attr = default_function_attribute in
    let loc = of_location ~scopes case.c_rhs.exp_loc in
    let body = maybe_region_layout return body in
    lfunction ~kind ~params ~return ~body ~attr ~loc
              ~mode:alloc_heap ~region:true
  in
  Lapply{
    ap_loc = of_location ~scopes loc;
    ap_func = op;
    ap_args=[exp; func];
    ap_result_layout=function2_return_layout env let_.bop_loc let_.bop_op_type;
    ap_region_close=Rc_normal;
    ap_mode=alloc_heap;
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inlined;
    ap_specialised = Default_specialise;
    ap_probe=None;
  }

(* Wrapper for class/module compilation,
   that can only return global values *)

let transl_exp ~scopes exp =
  maybe_region_exp exp (transl_exp ~scopes exp)

let transl_let ~scopes ?in_structure rec_flag pat_expr_list =
  transl_let ~scopes ~add_regions:true ?in_structure rec_flag pat_expr_list

let transl_scoped_exp ~scopes exp =
  maybe_region_exp exp (transl_scoped_exp ~scopes exp)

let transl_apply
      ~scopes ?tailcall ?inlined ?specialised ?position ?mode ~result_layout fn args loc =
  maybe_region_layout result_layout (transl_apply
      ~scopes ?tailcall ?inlined ?specialised ?position ?mode ~result_layout fn args loc)

(* Error report *)

open Format

let report_error ppf = function
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unreachable_reached ->
      fprintf ppf "Unreachable expression was reached"
  | Bad_probe_layout id ->
      fprintf ppf "Variables in probe handlers must have layout value, \
                   but %s in this handler does not." (Ident.name id)
  | Non_value_layout err ->
      fprintf ppf
        "Non-value detected in translation:@ Please report this error to \
         the Jane Street compilers team.@ %a"
        (Layout.Violation.report_with_name ~name:"This expression") err

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
