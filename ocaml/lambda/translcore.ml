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
  | Illegal_record_field of Jkind.Sort.const
  | Void_sort of type_expr

exception Error of Location.t * error

let use_dup_for_constant_mutable_arrays_bigger_than = 4

(* CR layouts v7: In the places where this is used, we will want to allow
   float#, but not void yet (e.g., the left of a semicolon and loop bodies).  we
   still default to value before checking for void, to allow for sort variables
   arising in situations like

     let foo () = raise Foo; ()

   When this sanity check is removed, consider whether we are still defaulting
   appropriately.
*)
let sort_must_not_be_void loc ty sort =
  if Jkind.Sort.is_void_defaulting sort then raise (Error (loc, Void_sort ty))

let layout_exp sort e = layout e.exp_env e.exp_loc sort e.exp_type

(* This is `Lambda.must_be_value` for the special case of record fields, where
   we allow the unboxed float layout.  Its result is never actually used in that
   case - it would be fine to return garbage.
*)
let record_field_kind l =
  match l with
  | Punboxed_float -> Pfloatval
  | _ -> must_be_value l

(* CR layouts v5: This function is only used for sanity checking the
   typechecker.  When we allow arbitrary layouts in structures, it will have
   outlived its usefulness and should be deleted. *)
let check_record_field_sort loc sort repres =
  match Jkind.Sort.get_default_value sort, repres with
  | Value, _ -> ()
  | Float64, Record_ufloat -> ()
  | Float64, (Record_boxed _ | Record_inlined _
             | Record_unboxed | Record_float) ->
    raise (Error (loc, Illegal_record_field Float64))
  | Void, _ ->
    raise (Error (loc, Illegal_record_field Void))

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
  Pccall
    (Primitive.simple_on_values ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

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
      | lam -> loop_region lam
    ) ~non_tail:(fun lam -> loop_region lam) lam
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

let maybe_region_exp sort exp lam =
  maybe_region (fun () -> layout_exp sort exp) lam

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
  | Tpat_alias (p, _, _, _, _) ->
      trivial_pat p
  | Tpat_construct (_, cd, [], _) ->
      not cd.cstr_generalized && cd.cstr_consts = 1 && cd.cstr_nonconsts = 0
  | Tpat_tuple patl ->
      List.for_all (fun (_, p) -> trivial_pat p) patl
  | _ -> false

let rec push_defaults loc bindings use_lhs arg_mode arg_sort cases
          partial warnings =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label; param; cases; partial;
                                        region; curry; warnings; arg_mode;
                                        arg_sort; ret_mode; ret_sort; alloc_mode } }
        as exp}] when bindings = [] || trivial_pat pat ->
      let cases =
        push_defaults exp.exp_loc bindings false arg_mode arg_sort cases partial
          warnings
      in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc =
                          Texp_function { arg_label; param; cases; partial;
                                          region; curry; warnings; arg_mode;
                                          arg_sort; ret_mode; ret_sort; alloc_mode }}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{Parsetree.attr_name = {txt="#default"};_}];
             exp_desc = Texp_let
               (Nonrecursive, binds,
                ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (binds :: bindings) true
                   arg_mode arg_sort [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial warnings
  | [{c_lhs=pat; c_guard=None; c_rhs=exp} as case]
    when use_lhs || trivial_pat pat && exp.exp_desc <> Texp_unreachable ->
      [{case with c_rhs = wrap_bindings bindings exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let mode = Mode.Value.of_alloc arg_mode in
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
                 desc, Id_value,
                 (Mode.Value.uniqueness mode, Mode.Value.linearity mode))},
             arg_sort,
             cases, partial) }
      in
      [{c_lhs = {pat with
          pat_desc = Tpat_var (param, mknoloc name, desc.val_uid, mode)};
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

let assert_failed loc ~scopes exp =
  let slot =
    transl_extension_path Loc_unknown
      (Lazy.force Env.initial) Predef.path_assert_failure
  in
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

(* Translation of expressions *)

let rec iter_exn_names f pat =
  match pat.pat_desc with
  | Tpat_var (id, _, _, _) -> f id
  | Tpat_alias (p, id, _, _, _) ->
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
      is_heap_mode (transl_locality_mode return_mode)
    end
  end

let rec transl_exp ~scopes sort e =
  transl_exp1 ~scopes ~in_new_scope:false sort e

(* ~in_new_scope tracks whether we just opened a new scope.

   We go to some trouble to avoid introducing many new anonymous function
   scopes, as `let f a b = ...` is desugared to several Pexp_fun.
*)
and transl_exp1 ~scopes ~in_new_scope sort e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 ~scopes ~in_new_scope sort e else
  Translobj.oo_wrap e.exp_env true (transl_exp0 ~scopes ~in_new_scope sort) e

and transl_exp0 ~in_new_scope ~scopes sort e =
  match e.exp_desc with
  | Texp_ident(path, _, desc, kind, _) ->
      transl_ident (of_location ~scopes e.exp_loc)
        e.exp_env e.exp_type path desc kind
  | Texp_constant cst ->
    begin match cst with
    | Const_int c -> Lconst(Const_base (Const_int c))
    | Const_char c -> Lconst(Const_base (Const_char c))
    | Const_string (s,loc,d) -> Lconst(Const_base (Const_string (s,loc,d)))
    | Const_float c -> Lconst(Const_base (Const_float c))
    | Const_int32 c -> Lconst(Const_base (Const_int32 c))
    | Const_int64 c -> Lconst(Const_base (Const_int64 c))
    | Const_nativeint c -> Lconst(Const_base (Const_nativeint c))
    | Const_unboxed_float f ->
      Lprim (Punbox_float, [Lconst (Const_base (Const_float f))],
        of_location ~scopes e.exp_loc)
    end
  | Texp_let(rec_flag, pat_expr_list, body) ->
      let return_layout = layout_exp sort body in
      transl_let ~scopes ~return_layout rec_flag pat_expr_list
        (event_before ~scopes body (transl_exp ~scopes sort body))
  | Texp_function { arg_label = _; param; cases; partial; region; curry;
                    warnings; arg_mode; arg_sort; ret_mode; ret_sort; alloc_mode } ->
      transl_function ~in_new_scope ~scopes e alloc_mode param arg_mode arg_sort ret_mode ret_sort
        cases partial warnings region curry
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p},
                                       Id_prim pmode, _);
                exp_type = prim_type; } as funct, oargs, pos, ap_mode)
    when can_apply_primitive p pmode pos oargs ->
      let rec cut_args prim_repr oargs =
        match prim_repr, oargs with
        | [], _ -> [], oargs
        | _, [] -> failwith "Translcore cut_args"
        | ((_, arg_repr) :: prim_repr), ((_, Arg (x, _)) :: oargs) ->
          let arg_exps, extra_args = cut_args prim_repr oargs in
          let arg_sort = Jkind.Sort.of_const (sort_of_native_repr arg_repr) in
          (x, arg_sort) :: arg_exps, extra_args
        | _, ((_, Omitted _) :: _) -> assert false
      in
      let arg_exps, extra_args = cut_args p.prim_native_repr_args oargs in
      let args = transl_list ~scopes arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let position =
        if extra_args = [] then transl_apply_position pos
        else Rc_normal
      in
      let lam =
        Translprim.transl_primitive_application
          (of_location ~scopes e.exp_loc) p e.exp_env prim_type pmode
          path prim_exp args (List.map fst arg_exps) position
      in
      if extra_args = [] then lam
      else begin
        let tailcall = Translattribute.get_tailcall_attribute funct in
        let inlined = Translattribute.get_inlined_attribute funct in
        let specialised = Translattribute.get_specialised_attribute funct in
        let position = transl_apply_position pos in
        let mode = transl_locality_mode ap_mode in
        let result_layout = layout_exp sort e in
        event_after ~scopes e
          (transl_apply ~scopes ~tailcall ~inlined ~specialised ~position ~mode
             ~result_layout lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs, position, ap_mode) ->
      let tailcall = Translattribute.get_tailcall_attribute funct in
      let inlined = Translattribute.get_inlined_attribute funct in
      let specialised = Translattribute.get_specialised_attribute funct in
      let result_layout = layout_exp sort e in
      let position = transl_apply_position position in
      let mode = transl_locality_mode ap_mode in
      event_after ~scopes e
        (transl_apply ~scopes ~tailcall ~inlined ~specialised ~result_layout
           ~position ~mode (transl_exp ~scopes Jkind.Sort.for_function funct)
           oargs (of_location ~scopes e.exp_loc))
  | Texp_match(arg, arg_sort, pat_expr_list, partial) ->
      transl_match ~scopes ~arg_sort ~return_sort:sort e arg pat_expr_list
        partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_cases "exn" pat_expr_list in
      let return_layout = layout_exp sort e in
      Ltrywith(transl_exp ~scopes sort body, id,
               Matching.for_trywith ~scopes ~return_layout e.exp_loc (Lvar id)
                 (transl_cases_try ~scopes sort pat_expr_list),
               return_layout)
  | Texp_tuple (el, alloc_mode) ->
      let ll, shape =
        transl_list_with_shape ~scopes
          (List.map (fun (_, a) -> (a, Jkind.Sort.for_tuple_element)) el)
      in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape,
                         transl_alloc_mode alloc_mode),
              ll,
              (of_location ~scopes e.exp_loc))
      end
  | Texp_construct(_, cstr, args, alloc_mode) ->
      let ll, shape =
        transl_list_with_shape ~scopes
          (List.map (fun a -> (a, Jkind.Sort.for_constructor_arg)) args)
      in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag, cstr.cstr_repr with
      | Ordinary {runtime_tag}, _ when cstr.cstr_constant ->
          (* CR layouts v5: This could have void args, but for now we've ruled
             that out with the jkind check in transl_list_with_shape *)
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
               that out with the jkind check in transl_list_with_shape. *)
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
          let lam = transl_exp ~scopes Jkind.Sort.for_poly_variant arg in
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
  | Texp_field(arg, id, lbl, _, alloc_mode) ->
      let targ = transl_exp ~scopes Jkind.Sort.for_record arg in
      let sem =
        match lbl.lbl_mut with
        | Immutable -> Reads_agree
        | Mutable -> Reads_vary
      in
      let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
      check_record_field_sort id.loc lbl_sort lbl.lbl_repres;
      begin match lbl.lbl_repres with
          Record_boxed _ | Record_inlined (_, Variant_boxed _) ->
          Lprim (Pfield (lbl.lbl_pos, maybe_pointer e, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed | Record_inlined (_, Variant_unboxed) -> targ
        | Record_float ->
          let mode = transl_alloc_mode (Option.get alloc_mode) in
          Lprim (Pfloatfield (lbl.lbl_pos, sem, mode), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_ufloat ->
          Lprim (Pufloatfield (lbl.lbl_pos, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_inlined (_, Variant_extensible) ->
          Lprim (Pfield (lbl.lbl_pos + 1, maybe_pointer e, sem), [targ],
                 of_location ~scopes e.exp_loc)
      end
  | Texp_setfield(arg, arg_mode, id, lbl, newval) ->
      (* CR layouts v2.5: When we allow `any` in record fields and check
         representability on construction, [sort_of_jkind] will be unsafe here.
         Probably we should add a sort to `Texp_setfield` in the typed tree,
         then. *)
      let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
      check_record_field_sort id.loc lbl_sort lbl.lbl_repres;
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
        | Record_ufloat -> Psetufloatfield (lbl.lbl_pos, mode)
        | Record_inlined (_, Variant_extensible) ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, mode)
      in
      Lprim(access, [transl_exp ~scopes Jkind.Sort.for_record arg;
                     transl_exp ~scopes lbl_sort newval],
            of_location ~scopes e.exp_loc)
  | Texp_array (amut, expr_list, alloc_mode) ->
      let mode = transl_alloc_mode alloc_mode in
      let kind = array_kind e in
      let ll =
        transl_list ~scopes
          (List.map (fun e -> (e, Jkind.Sort.for_array_element)) expr_list)
      in
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
      Lifthenelse(transl_exp ~scopes Jkind.Sort.for_predef_value cond,
                  event_before ~scopes ifso (transl_exp ~scopes sort ifso),
                  event_before ~scopes ifnot (transl_exp ~scopes sort ifnot),
                  layout_exp sort e)
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp ~scopes Jkind.Sort.for_predef_value cond,
                  event_before ~scopes ifso (transl_exp ~scopes sort ifso),
                  lambda_unit,
                  Lambda.layout_unit)
  | Texp_sequence(expr1, sort', expr2) ->
      sort_must_not_be_void expr1.exp_loc expr1.exp_type sort';
      Lsequence(transl_exp ~scopes sort' expr1,
                event_before ~scopes expr2 (transl_exp ~scopes sort expr2))
  | Texp_while {wh_body; wh_body_sort; wh_cond} ->
      sort_must_not_be_void wh_body.exp_loc wh_body.exp_type wh_body_sort;
      let cond = transl_exp ~scopes Jkind.Sort.for_predef_value wh_cond in
      let body = transl_exp ~scopes wh_body_sort wh_body in
      Lwhile {
        wh_cond = maybe_region_layout layout_int cond;
        wh_body = event_before ~scopes wh_body
                    (maybe_region_layout layout_unit body);
      }
  | Texp_for {for_id; for_from; for_to; for_dir; for_body; for_body_sort} ->
      sort_must_not_be_void for_body.exp_loc for_body.exp_type for_body_sort;
      let body = transl_exp ~scopes for_body_sort for_body in
      Lfor {
        for_id;
        for_loc = of_location ~scopes e.exp_loc;
        for_from = transl_exp ~scopes Jkind.Sort.for_predef_value for_from;
        for_to = transl_exp ~scopes Jkind.Sort.for_predef_value for_to;
        for_dir;
        for_body = event_before ~scopes for_body
                     (maybe_region_layout layout_unit body);
      }
  | Texp_send(expr, met, pos) ->
      let lam =
        let pos = transl_apply_position pos in
        let mode = Lambda.alloc_heap in
        let loc = of_location ~scopes e.exp_loc in
        let layout = layout_exp sort e in
        match met with
        | Tmeth_val id ->
            let obj = transl_exp ~scopes Jkind.Sort.for_object expr in
            Lsend (Self, Lvar id, obj, [], pos, mode, loc, layout)
        | Tmeth_name nm ->
            let obj = transl_exp ~scopes Jkind.Sort.for_object expr in
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
          Lprim(Pfield (0, Pointer, Reads_vary),
              [transl_class_path loc e.exp_env cl], loc);
        ap_args=[lambda_unit];
        ap_result_layout=layout_exp sort e;
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
                transl_exp ~scopes sort body)
  | Texp_letmodule(Some id, _loc, Mp_present, modl, body) ->
      let defining_expr =
        let mod_scopes = enter_module_definition ~scopes id in
        !transl_module ~scopes:mod_scopes Tcoerce_none None modl
      in
      Llet(Strict, Lambda.layout_module, id, defining_expr,
           transl_exp ~scopes sort body)
  | Texp_letmodule(_, _, Mp_absent, _, body) ->
      transl_exp ~scopes sort body
  | Texp_letexception(cd, body) ->
      Llet(Strict, Lambda.layout_block,
           cd.ext_id, transl_extension_constructor ~scopes e.exp_env None cd,
           transl_exp ~scopes sort body)
  | Texp_pack modl ->
      !transl_module ~scopes Tcoerce_none None modl
  | Texp_assert ({exp_desc=Texp_construct(_, {cstr_name="false"}, _, _)}, loc) ->
      assert_failed loc ~scopes e
  | Texp_assert (cond, loc) ->
      if !Clflags.noassert
      then lambda_unit
      else begin
        Lifthenelse
          (transl_exp ~scopes Jkind.Sort.for_predef_value cond,
           lambda_unit,
           assert_failed loc ~scopes e,
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
         transl_exp ~scopes Jkind.Sort.for_lazy_body e
      | `Float_that_cannot_be_shortcut ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float
             and Config.flat_float_array is true. *)
         Lprim(Pmakeblock(Obj.forward_tag, Immutable, None,
                          alloc_heap),
                [transl_exp ~scopes Jkind.Sort.for_lazy_body e],
               of_location ~scopes e.exp_loc)
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
                       [transl_exp ~scopes Jkind.Sort.for_lazy_body e],
                       of_location ~scopes e.exp_loc)],
                of_location ~scopes e.exp_loc)
      | `Identifier `Other ->
         transl_exp ~scopes Jkind.Sort.for_lazy_body e
      | `Other ->
         (* other cases compile to a lazy block holding a function.  The
            typechecker enforces that e has jkind value.  *)
         let scopes = enter_lazy ~scopes in
         let fn = lfunction ~kind:(Curried {nlocal=0})
                            ~params:[{ name = Ident.create_local "param";
                                       layout = Lambda.layout_unit;
                                       attributes = Lambda.default_param_attribute;
                                       mode = alloc_heap}]
                            ~return:Lambda.layout_lazy_contents
                            ~attr:default_function_attribute
                            ~loc:(of_location ~scopes e.exp_loc)
                            ~mode:alloc_heap
                            ~ret_mode:alloc_heap
                            ~region:true
                            ~body:(maybe_region_layout
                                     Lambda.layout_lazy_contents
                                     (transl_exp ~scopes Jkind.Sort.for_lazy_body e))
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
  | Texp_letop{let_; ands; param; param_sort; body; body_sort; partial;
               warnings} ->
      event_after ~scopes e
        (transl_letop ~scopes e.exp_loc e.exp_env let_ ands
           param param_sort body body_sort partial warnings)
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))
  | Texp_open (od, e) ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to
          do it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> transl_exp ~scopes sort e
      | _ ->
          let oid = Ident.create_local "open" in
          let body, _ =
            (* CR layouts v5: Currently we only allow values at the top of a
               module.  When that changes, some adjustments may be needed
               here. *)
            List.fold_left (fun (body, pos) id ->
              Llet(Alias, Lambda.layout_module_field, id,
                   Lprim(mod_field pos, [Lvar oid],
                         of_location ~scopes od.open_loc), body),
              pos + 1
            ) (transl_exp ~scopes sort e, 0)
              (bound_value_identifiers od.open_bound_items)
          in
          Llet(pure, Lambda.layout_module, oid,
               !transl_module ~scopes Tcoerce_none None od.open_expr, body)
      end
  | Texp_probe {name; handler=exp; enabled_at_init} ->
    if !Clflags.native_code && !Clflags.probes then begin
      let lam = transl_exp ~scopes Jkind.Sort.for_probe_body exp in
      let map =
        Ident.Set.fold (fun v acc -> Ident.Map.add v (Ident.rename v) acc)
          (free_variables lam)
          Ident.Map.empty
      in
      let arg_idents, param_idents = Ident.Map.bindings map |> List.split in
      List.iter (fun id ->
        (* CR layouts: The probe hack.

           The lambda translation wants to know the jkinds of all function
           parameters.  Here we're building a function whose arguments are all
           the free variables in a probe handler.  At the moment, we just check
           that they are all values.

           It's really hacky to be doing this kind of jkind check this late.
           The middle-end folks have plans to eliminate the need for it by
           reworking the way probes are compiled.  For that reason, I haven't
           bothered to give a particularly good error or handle the Not_found
           case from env.

           (We could probably calculate the jkinds of these variables here
           rather than requiring them all to be value, but that would be even
           more hacky.) *)
        (* CR layouts v2.5: if we get close to releasing other jkind somebody
           actually might put in a probe, check with the middle-end team about
           the status of fixing this. *)
        let path = Path.Pident id in
        match
          Subst.Lazy.force_value_description (Env.find_value path e.exp_env)
        with
        | {val_type; _} -> begin
            match
              Ctype.check_type_jkind
                e.exp_env (Ctype.correct_levels val_type)
                (Jkind.value ~why:Probe)
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
      let return_layout = layout_unit (* Probe bodies have type unit. *) in
      let handler =
        let assume_zero_alloc = get_assume_zero_alloc ~scopes in
        let scopes = enter_value_definition ~scopes ~assume_zero_alloc funcid in
        lfunction
          ~kind:(Curried {nlocal=0})
          (* CR layouts: Adjust param layouts when we allow other things in
             probes. *)
          ~params:(List.map (fun name -> { name; layout = layout_probe_arg; attributes = Lambda.default_param_attribute; mode = alloc_heap }) param_idents)
          ~return:return_layout
          ~body
          ~loc:(of_location ~scopes exp.exp_loc)
          ~attr
          ~mode:alloc_heap
          ~ret_mode:alloc_heap
          ~region:true
      in
      let app =
        { ap_func = Lvar funcid;
          ap_args = List.map (fun id -> Lvar id) arg_idents;
          ap_result_layout = return_layout;
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
    let l = transl_exp ~scopes sort e in
    if Config.stack_allocation then Lexclave l
    else l

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

and transl_list ~scopes expr_list =
  List.map (fun (exp, sort) -> transl_exp ~scopes sort exp) expr_list

and transl_list_with_layout ~scopes expr_list =
  List.map (fun (exp, sort) -> transl_exp ~scopes sort exp,
                               sort,
                               layout_exp sort exp)
    expr_list

(* Will raise if a list element has a non-value layout. *)
and transl_list_with_shape ~scopes expr_list =
  let transl_with_shape (e, sort) =
    let shape = Lambda.must_be_value (layout_exp sort e) in
    transl_exp ~scopes sort e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard ~scopes guard rhs_sort rhs =
  let layout = layout_exp rhs_sort rhs in
  let expr = event_before ~scopes rhs (transl_exp ~scopes rhs_sort rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before ~scopes cond
        (Lifthenelse(transl_exp ~scopes Jkind.Sort.for_predef_value cond,
                     expr, staticfail, layout))

and transl_case ~scopes rhs_sort {c_lhs; c_guard; c_rhs} =
  (c_lhs, transl_guard ~scopes c_guard rhs_sort c_rhs)

and transl_cases ~scopes rhs_sort cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case ~scopes rhs_sort) cases

and transl_case_try ~scopes rhs_sort {c_lhs; c_guard; c_rhs} =
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () -> c_lhs, transl_guard ~scopes c_guard rhs_sort c_rhs)
    ~always:(fun () ->
        iter_exn_names Translprim.remove_exception_ident c_lhs)

and transl_cases_try ~scopes rhs_sort cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case_try ~scopes rhs_sort) cases

and transl_tupled_cases ~scopes rhs_sort patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map
    (fun (patl, guard, expr) ->
       (patl, transl_guard ~scopes guard rhs_sort expr))
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
                   ap_region_close = pos; ap_mode = mode; ap_result_layout = result_layout }
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
    | Omitted { mode_closure; mode_arg; mode_ret; sort_arg } :: l ->
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
          let layout_arg = layout_of_sort (to_location loc) sort_arg in
          let params = [{
              name = id_arg;
              layout = layout_arg;
              attributes = Lambda.default_param_attribute;
              mode = arg_mode
            }] in
          lfunction ~kind:(Curried {nlocal}) ~params
                    ~return:result_layout ~body ~mode ~ret_mode ~region
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
         | Arg (exp, sort_arg) ->
           Arg (transl_exp ~scopes sort_arg exp, layout_exp sort_arg exp))
      sargs
  in
  build_apply lam [] loc position mode args

and transl_curried_function
      ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort ~return_layout loc repr ~region ~return_mode
      ~curry partial warnings (param:Ident.t) cases =
  let max_arity = Lambda.max_arity () in
  let rec loop ~scopes ~arg_sort ~arg_layout ~return_sort ~return_layout loc
            ~arity ~region ~return_mode ~curry ~arg_mode partial warnings (param:Ident.t) cases =
    match curry, cases with
      More_args {partial_mode},
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp_desc =
                 Texp_function
                   { arg_label = _; param = param'; cases = cases';
                     partial = partial'; region = region'; ret_mode;
                     curry = curry';
                     warnings = warnings'; arg_mode = arg_mode'; arg_sort; ret_sort };
               exp_env; exp_type; exp_loc }}]
      when arity < max_arity ->
      (* Lfunctions must have local returns after the first local arg/ret *)
      if Parmatch.inactive ~partial pat
      then
        let partial_mode = transl_alloc_mode partial_mode in
        let ((fnkind, params, return_layout, region, return_mode), body) =
          let return_layout =
            function_return_layout exp_env exp_loc ret_sort exp_type
          in
          let arg_layout =
            function_arg_layout exp_env exp_loc arg_sort exp_type
          in
          let return_mode' = transl_alloc_mode ret_mode in
          loop ~scopes ~arg_sort ~arg_layout ~arg_mode:arg_mode' ~return_sort:ret_sort
            ~return_layout exp_loc ~arity:(arity + 1) ~region:region' ~return_mode:return_mode'
            ~curry:curry' partial' warnings' param' cases'
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
        let arg_mode = transl_alloc_mode arg_mode in
        let params = {
          name = param ;
          layout = arg_layout ;
          attributes = Lambda.default_param_attribute ;
          mode = arg_mode
        } :: params
        in
        ((fnkind, params, return_layout, region, return_mode),
         Matching.for_function ~scopes ~arg_sort ~arg_layout ~return_layout loc
           None (Lvar param) [pat, body] partial)
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
        transl_tupled_function ~scopes ~arg_sort ~arg_layout ~arg_mode
          ~return_sort:ret_sort ~return_layout ~arity ~region ~return_mode ~curry loc repr
          partial param cases
      end
    | curry, cases ->
      transl_tupled_function ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort
        ~return_layout ~arity ~region ~return_mode ~curry loc repr partial param cases
  in
  loop ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort ~return_layout loc ~arity:1
    ~region ~return_mode ~curry partial warnings param cases

and transl_tupled_function
      ~scopes ~arg_layout ~arg_sort ~arg_mode ~return_sort ~return_layout ~arity ~region ~return_mode
      ~curry loc repr partial (param:Ident.t) cases =
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
              (* CR layouts v5: to change when we have non-value tuple
                 elements. *)
              List.map (fun vk -> Pvalue vk) kinds
          | _ ->
              Misc.fatal_error
                "Translcore.transl_tupled_function: \
                 Argument should be a tuple, but couldn't get the kinds"
        in
        let tparams =
          List.map (fun kind -> {
                name = Ident.create_local "param";
                layout = kind;
                attributes = Lambda.default_param_attribute;
                mode = alloc_heap
              }) kinds
        in
        let params = List.map (fun p -> p.name) tparams in
        let body =
          Matching.for_tupled_function ~scopes ~return_layout loc params
            (transl_tupled_cases ~scopes return_sort pats_expr_list) partial
        in
        let region = region || not (may_allocate_in_region body) in
        ((Tupled, tparams, return_layout, region, return_mode), body)
    with Matching.Cannot_flatten ->
      transl_function0 ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort ~return_layout
        loc ~region ~return_mode ~partial_mode repr partial param cases
      end
  | _ -> transl_function0 ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort
           ~return_layout loc ~region ~return_mode ~partial_mode repr partial param cases

and transl_function0
      ~scopes ~arg_sort ~arg_layout ~arg_mode ~return_sort ~return_layout loc ~region ~return_mode
      ~partial_mode repr partial (param:Ident.t) cases =
    let body =
      Matching.for_function ~scopes ~arg_sort ~arg_layout ~return_layout loc
        repr (Lvar param) (transl_cases ~scopes return_sort cases) partial
    in
    let region = region || not (may_allocate_in_region body) in
    let nlocal =
      match return_mode, partial_mode with
      | Alloc_local, _ | _, Alloc_local -> 1
      | Alloc_heap, Alloc_heap -> 0
    in
    let arg_mode = transl_alloc_mode arg_mode in
    ((Curried {nlocal},
      [{ name = param;
         layout = arg_layout;
         attributes = Lambda.default_param_attribute;
         mode = arg_mode}],
      return_layout, region, return_mode), body)

and transl_function ~in_new_scope ~scopes e alloc_mode param arg_mode arg_sort ret_mode return_sort
      cases partial warnings region curry =
  let mode = transl_alloc_mode alloc_mode in
  let ret_mode = transl_alloc_mode ret_mode in
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
  let assume_zero_alloc = Translattribute.assume_zero_alloc attrs in
  let scopes =
    if in_new_scope then begin
      if assume_zero_alloc then set_assume_zero_alloc ~scopes
      else scopes
    end
    else enter_anonymous_function ~scopes ~assume_zero_alloc
  in
  let arg_layout =
    function_arg_layout e.exp_env e.exp_loc arg_sort e.exp_type
  in
  let ((kind, params, return, region, ret_mode), body) =
    event_function ~scopes e
      (function repr ->
         let pl =
           push_defaults e.exp_loc arg_mode arg_sort cases partial warnings
         in
         let return_layout =
           function_return_layout e.exp_env e.exp_loc return_sort e.exp_type
         in
         transl_curried_function ~arg_sort ~arg_layout ~arg_mode ~return_sort
           ~return_layout ~scopes e.exp_loc repr ~region ~return_mode:ret_mode ~curry partial warnings
           param pl)
  in
  let attr = default_function_attribute in
  let loc = of_location ~scopes e.exp_loc in
  let body = if region then maybe_region_layout return body else body in
  let lam = lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode ~region in
  Translattribute.add_function_attributes lam e.exp_loc attrs

(* Like transl_exp, but used when a new scope was just introduced. *)
and transl_scoped_exp ~scopes sort expr =
  transl_exp1 ~scopes ~in_new_scope:true sort expr

(* Decides whether a pattern binding should introduce a new scope. *)
and transl_bound_exp ~scopes ~in_structure pat sort expr loc attrs =
  let should_introduce_scope =
    match expr.exp_desc with
    | Texp_function _ -> true
    | _ when in_structure -> true
    | _ -> false in
  let lam =
    match pat_bound_idents pat with
    | (id :: _) when should_introduce_scope ->
      let assume_zero_alloc = Translattribute.assume_zero_alloc attrs in
      let scopes = enter_value_definition ~scopes ~assume_zero_alloc id in
      transl_scoped_exp ~scopes sort expr
    | _ -> transl_exp ~scopes sort expr
  in
  Translattribute.add_function_attributes lam loc attrs

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let ~scopes ~return_layout ?(add_regions=false) ?(in_structure=false)
               rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_sort=sort; vb_attributes; vb_loc}
        :: rem ->
          let lam =
            transl_bound_exp ~scopes ~in_structure pat sort expr vb_loc vb_attributes
          in
          let lam =
            if add_regions then maybe_region_exp sort expr lam else lam
          in
          let mk_body = transl rem in
          fun body ->
            Matching.for_let ~scopes ~arg_sort:sort ~return_layout pat.pat_loc
              lam pat (mk_body body)
      in
      transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_,_,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case
            {vb_expr=expr; vb_sort; vb_attributes; vb_loc; vb_pat} id =
        let lam =
          transl_bound_exp ~scopes ~in_structure vb_pat vb_sort expr vb_loc vb_attributes
        in
        let lam =
          if add_regions then maybe_region_exp vb_sort expr lam else lam
        in
        (id, lam) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Lletrec(lam_bds, body)

and transl_setinstvar ~scopes loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment modify_heap),
    [self; var; transl_exp ~scopes Jkind.Sort.for_instance_var expr], loc)

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
    (* CR layouts v5: allow non-value fields beyond just float# *)
    let init_id = Ident.create_local "init" in
    let lv =
      Array.mapi
        (fun i (lbl, definition) ->
           (* CR layouts v2.5: When we allow `any` in record fields and check
              representability on construction, [sort_of_layout] will be unsafe
              here.  Probably we should add sorts to record construction in the
              typed tree, then. *)
           let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
           match definition with
           | Kept (typ, mut, _) ->
               let field_kind =
                 record_field_kind (layout env lbl.lbl_loc lbl_sort typ)
               in
               let sem =
                 match mut with
                 | Immutable -> Reads_agree
                 | Mutable -> Reads_vary
               in
               let access =
                 match repres with
                   Record_boxed _ | Record_inlined (_, Variant_boxed _) ->
                   Pfield (i, maybe_pointer_type env typ, sem)
                 | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
                   assert false
                 | Record_inlined (_, Variant_extensible) ->
                     Pfield (i + 1, maybe_pointer_type env typ, sem)
                 | Record_float ->
                    (* This allocation is always deleted,
                       so it's simpler to leave it Alloc_heap *)
                    Pfloatfield (i, sem, alloc_heap)
                 | Record_ufloat -> Pufloatfield (i, sem)
               in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_kind
           | Overridden (_lid, expr) ->
               let field_kind = record_field_kind (layout_exp lbl_sort expr) in
               transl_exp ~scopes lbl_sort expr, field_kind)
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
        | Record_ufloat ->
            (* CR layouts v2.5: When we add unboxed float literals, we may need
               to do something here.  (Currrently this case isn't reachable for
               `float#` records because `extact_constant` will have raised
               `Not_constant`.) *)
            raise Not_constant
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
        | Record_ufloat ->
            Lprim(Pmakeufloatblock (mut, Option.get mode), ll, loc)
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
                             transl_exp ~scopes Jkind.Sort.for_record init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create_local "newrecord" in
    let update_field cont (lbl, definition) =
      (* CR layouts v5: allow more unboxed types here. *)
      let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
      check_record_field_sort lbl.lbl_loc lbl_sort lbl.lbl_repres;
      match definition with
      | Kept _ -> cont
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
            | Record_ufloat ->
                Psetufloatfield (lbl.lbl_pos, Assignment modify_heap)
            | Record_inlined (_, Variant_extensible) ->
                let pos = lbl.lbl_pos + 1 in
                let ptr = maybe_pointer expr in
                Psetfield(pos, ptr, Assignment modify_heap)
          in
          Lsequence(Lprim(upd, [Lvar copy_id;
                                transl_exp ~scopes lbl_sort expr],
                          of_location ~scopes loc),
                    cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        assert (is_heap_mode (Option.get mode)); (* Pduprecord must be Alloc_heap and not unboxed *)
        Llet(Strict, Lambda.layout_block, copy_id,
             Lprim(Pduprecord (repres, size),
                   [transl_exp ~scopes Jkind.Sort.for_record init_expr],
                   of_location ~scopes loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match ~scopes ~arg_sort ~return_sort e arg pat_expr_list partial =
  let return_layout = layout_exp return_sort e in
  let rewrite_case (val_cases, exn_cases, static_handlers as acc)
        ({ c_lhs; c_guard; c_rhs } as case) =
    if c_rhs.exp_desc = Texp_unreachable then acc else
    let val_pat, exn_pat = split_pattern c_lhs in
    match val_pat, exn_pat with
    | None, None -> assert false
    | Some pv, None ->
        let val_case =
          transl_case ~scopes return_sort { case with c_lhs = pv }
        in
        val_case :: val_cases, exn_cases, static_handlers
    | None, Some pe ->
        let exn_case =
          transl_case_try ~scopes return_sort { case with c_lhs = pe }
        in
        val_cases, exn_case :: exn_cases, static_handlers
    | Some pv, Some pe ->
        assert (c_guard = None);
        let lbl  = next_raise_count () in
        let static_raise ids =
          Lstaticraise (lbl, List.map (fun id -> Lvar id) ids)
        in
        (* Simplif doesn't like it if binders are not uniq, so we make sure to
           use different names in the value and the exception branches. *)
        let ids_full = Typedtree.pat_bound_idents_full arg_sort pv in
        let ids = List.map (fun (id, _, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, {Location.loc; _}, ty, s) ->
            id, Typeopt.layout pv.pat_env loc s ty)
            ids_full
        in
        let vids = List.map Ident.rename ids in
        let pv = alpha_pat (List.combine ids vids) pv in
        (* Also register the names of the exception so Re-raise happens. *)
        iter_exn_names Translprim.add_exception_ident pe;
        let rhs =
          Misc.try_finally
            (fun () -> event_before ~scopes c_rhs
                         (transl_exp ~scopes return_sort c_rhs))
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
                 Matching.for_trywith ~scopes ~return_layout e.exp_loc (Lvar id)
                   exn_cases,
                 return_layout),
       (static_exception_id, val_ids),
       handler,
      return_layout)
  in
  let classic =
    match arg, exn_cases with
    | {exp_desc = Texp_tuple (argl, alloc_mode)}, [] ->
      assert (static_handlers = []);
      let mode = transl_alloc_mode alloc_mode in
      let argl =
        List.map (fun (_, a) -> (a, Jkind.Sort.for_tuple_element)) argl
      in
      Matching.for_multiple_match ~scopes ~return_layout e.exp_loc
        (transl_list_with_layout ~scopes argl) mode val_cases partial
    | {exp_desc = Texp_tuple (argl, alloc_mode)}, _ :: _ ->
        let argl =
          List.map (fun (_, a) -> (a, Jkind.Sort.for_tuple_element)) argl
        in
        let val_ids, lvars =
          List.map
            (fun (arg,s) ->
               let layout = layout_exp s arg in
               let id = Typecore.name_pattern "val" [] in
               (id, layout), (Lvar id, s, layout))
            argl
          |> List.split
        in
        let mode = transl_alloc_mode alloc_mode in
        static_catch (transl_list ~scopes argl) val_ids
          (Matching.for_multiple_match ~scopes ~return_layout e.exp_loc
             lvars mode val_cases partial)
    | arg, [] ->
      assert (static_handlers = []);
      let arg_layout = layout_exp arg_sort arg in
      Matching.for_function ~scopes ~arg_sort ~arg_layout ~return_layout
        e.exp_loc None (transl_exp ~scopes arg_sort arg) val_cases partial
    | arg, _ :: _ ->
        let val_id = Typecore.name_pattern "val" (List.map fst val_cases) in
        let arg_layout = layout_exp arg_sort arg in
        static_catch [transl_exp ~scopes arg_sort arg] [val_id, arg_layout]
          (Matching.for_function ~scopes ~arg_sort ~arg_layout ~return_layout
             e.exp_loc None (Lvar val_id) val_cases partial)
  in
  List.fold_left (fun body (static_exception_id, val_ids, handler) ->
    Lstaticcatch (body, (static_exception_id, val_ids), handler, return_layout)
  ) classic static_handlers

and transl_letop ~scopes loc env let_ ands param param_sort case case_sort
      partial warnings =
  let rec loop prev_layout prev_lam = function
    | [] -> prev_lam
    | and_ :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op =
          transl_ident (of_location ~scopes and_.bop_op_name.loc) env
            and_.bop_op_type and_.bop_op_path and_.bop_op_val Id_value
        in
        let exp = transl_exp ~scopes and_.bop_exp_sort and_.bop_exp in
        let right_layout = layout_exp and_.bop_exp_sort and_.bop_exp in
        let result_layout =
          function2_return_layout env and_.bop_loc and_.bop_op_return_sort
            and_.bop_op_type
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
    loop (layout_exp let_.bop_exp_sort let_.bop_exp)
      (transl_exp ~scopes let_.bop_exp_sort let_.bop_exp) ands
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
          | Some (lhs, _) -> Typeopt.function_arg_layout env loc param_sort lhs
    in
    let return_layout = layout_exp case_sort case.c_rhs in
    let curry = More_args { partial_mode = Mode.Alloc.legacy } in
    let return_mode = alloc_heap (* XXX fixme: use result of is_function_type *) in
    let (kind, params, return, _region, ret_mode), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           transl_curried_function ~scopes ~arg_sort:param_sort ~arg_layout
             ~arg_mode:Mode.Alloc.legacy ~return_sort:case_sort
             ~return_layout case.c_rhs.exp_loc repr ~region:true
             ~return_mode
             ~curry partial
             warnings param [case])
    in
    let attr = default_function_attribute in
    let loc = of_location ~scopes case.c_rhs.exp_loc in
    let body = maybe_region_layout return body in
    lfunction ~kind ~params ~return ~body ~attr ~loc
              ~mode:alloc_heap ~ret_mode ~region:true
  in
  Lapply{
    ap_loc = of_location ~scopes loc;
    ap_func = op;
    ap_args=[exp; func];
    ap_result_layout=
      function2_return_layout env let_.bop_loc let_.bop_op_return_sort
        let_.bop_op_type;
    ap_region_close=Rc_normal;
    ap_mode=alloc_heap;
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inlined;
    ap_specialised = Default_specialise;
    ap_probe=None;
  }

(* Wrapper for class/module compilation,
   that can only return global values *)

let transl_exp ~scopes sort exp =
  maybe_region_exp sort exp (transl_exp ~scopes sort exp)

let transl_let ~scopes ~return_layout ?in_structure rec_flag pat_expr_list =
  transl_let ~scopes ~return_layout ~add_regions:true ?in_structure rec_flag
    pat_expr_list

let transl_scoped_exp ~scopes sort exp =
  maybe_region_exp sort exp (transl_scoped_exp ~scopes sort exp)

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
      fprintf ppf "Variables in probe handlers must have jkind value, \
                   but %s in this handler does not." (Ident.name id)
  | Illegal_record_field c ->
      fprintf ppf
        "Sort %a detected where value was expected in a record field:@ Please \
         report this error to the Jane Street compilers team."
        Jkind.Sort.format (Jkind.Sort.of_const c)
  | Void_sort ty ->
      fprintf ppf
        "Void detected in translation for type %a:@ Please report this error \
         to the Jane Street compilers team."
        Printtyp.type_expr ty

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
