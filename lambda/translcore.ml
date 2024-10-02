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
  | Illegal_void_record_field
  | Illegal_product_record_field of Jkind.Sort.Const.t
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
let layout_pat sort p = layout p.pat_env p.pat_loc sort p.pat_type

let check_record_field_sort loc sort =
  match Jkind.Sort.default_to_value_and_get sort with
  | Base (Value | Float64 | Float32 | Bits32 | Bits64 | Word) -> ()
  | Base Void -> raise (Error (loc, Illegal_void_record_field))
  | Product _ as c -> raise (Error (loc, Illegal_product_record_field c))

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
    (Lambda.simple_prim_on_values ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

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

let is_alloc_heap = function Alloc_heap -> true | Alloc_local -> false

(* In cases where we're careful to preserve syntactic arity, we disable
   the arity fusion attempted by simplif.ml *)
let function_attribute_disallowing_arity_fusion =
  { default_function_attribute with may_fuse_arity = false }

(** A well-formed function parameter list is of the form
     [G @ L @ [ Final_arg ]],
    where the values of G and L are of the form [More_args { partial_mode }],
    where [partial_mode] has locality Global in G and locality Local in L.

    [curried_function_kind p] checks the well-formedness of the list and returns
    the corresponding [curried_function_kind]. [nlocal] is populated as follows:
      - if {v |L| > 0 v}, then {v nlocal = |L| + 1 v}.
      - if {v |L| = 0 v},
        * if the function returns at mode local, the final arg has mode local,
          or the function itself is allocated locally, then {v nlocal = 1 v}.
        * otherwise, {v nlocal = 0 v}.
*)
(* CR-someday: Now that some functions' arity won't be changed downstream of
   lambda (see [may_fuse_arity = false]), we could change [nlocal] to be
   more expressive. I suggest the variant:

   {[
     type partial_application_is_local_when =
       | Applied_up_to_nth_argument_from_end of int
       | Never
   ]}

   I believe this will allow us to get rid of the complicated logic for
   |L| = 0, and help clarify how clients use this type. I plan on doing
   this in a follow-on PR.
*)
let curried_function_kind
    : (function_curry * Mode.Alloc.l) list
      -> return_mode:alloc_mode
      -> alloc_mode:alloc_mode
      -> curried_function_kind
  =
  let rec loop params ~return_mode ~alloc_mode ~running_count
      ~found_local_already
    =
    match params with
    | [] -> Misc.fatal_error "Expected to find [Final_arg] at end of list"
    | [ Final_arg, final_arg_mode ] ->
        let nlocal =
          if running_count = 0
             && is_alloc_heap return_mode
             && is_alloc_heap alloc_mode
             && is_alloc_heap (transl_alloc_mode_l final_arg_mode)
          then 0
          else running_count + 1
        in
        { nlocal }
    | (Final_arg, _) :: _ -> Misc.fatal_error "Found [Final_arg] too early"
    | (More_args { partial_mode }, _) :: params ->
        match transl_alloc_mode_l partial_mode with
        | Alloc_heap when not found_local_already ->
            loop params ~return_mode ~alloc_mode
              ~running_count:0 ~found_local_already
        | Alloc_local ->
            loop params ~return_mode ~alloc_mode
              ~running_count:(running_count + 1) ~found_local_already:true
        | Alloc_heap ->
            Misc.fatal_error
              "A function argument with a Global partial_mode unexpectedly \
              found following a function argument with a Local partial_mode"
  in
  fun params ~return_mode ~alloc_mode ->
    loop params ~return_mode ~alloc_mode ~running_count:0
      ~found_local_already:false

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

type fusable_function =
  { params : function_param list
  ; body : function_body
  ; return_sort : Jkind.sort
  ; return_mode : alloc_mode
  ; region : bool
  }

(* [fuse_method_arity] is what ensures that a n-ary method is compiled as a
   (n+1)-ary function, where the first parameter is self. It fuses together the
   self and method parameters.

   Input:  fun self -> fun method_param_1 ... method_param_n -> body
   Output: fun self method_param_1 ... method_param_n -> body

   It detects whether the AST is a method by the presence of [Texp_poly] on the
   inner function. This is only ever added to methods.
*)
let fuse_method_arity (parent : fusable_function) : fusable_function =
  match parent with
  | { params = [ self_param ];
      return_mode = Alloc_heap;
      body =
        Tfunction_body { exp_desc = Texp_function method_; exp_extra; }
    }
    when
      List.exists
        (function (Texp_poly _, _, _) -> true | _ -> false)
        exp_extra
    ->
      begin match transl_alloc_mode method_.alloc_mode with
      | Alloc_heap -> ()
      | Alloc_local ->
          (* If we support locally-allocated objects, we'll also have to
             pass the new mode back to the caller.
          *)
          Misc.fatal_error "Locally-allocated method body!"
      end;
      let self_param =
        { self_param
          with fp_curry = More_args
            { partial_mode =
              Mode.Alloc.disallow_right Mode.Alloc.legacy }
        }
      in
      { params = self_param :: method_.params;
        body = method_.body;
        return_mode = transl_alloc_mode_l method_.ret_mode;
        return_sort = method_.ret_sort;
        region = true;
      }
  | _ -> parent

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
  | Val_prim p, Id_prim (poly_mode, poly_sort) ->
      Translprim.transl_primitive loc p env ty ~poly_mode ~poly_sort (Some path)
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
      is_heap_mode (transl_locality_mode_l return_mode)
    end
  end

let zero_alloc_of_application
      ~num_args (annotation : Zero_alloc.assume option) funct =
  match annotation, funct.exp_desc with
  | Some assume, _ ->
    (* The user wrote a zero_alloc attribute on the application - keep it. *)
    Builtin_attributes.assume_zero_alloc ~inferred:false assume
  | None, Texp_ident (_, _, { val_zero_alloc; _ }, _, _) ->
    (* We assume the call is zero_alloc if the function is known to be
       zero_alloc. If the function is zero_alloc opt, then we need to be sure
       that the opt checks were run to license this assumption. We judge
       whether the opt checks were run based on the argument to the
       [-zero-alloc-check] command line flag. *)
    let use_opt =
      match !Clflags.zero_alloc_check with
      | Check_default | No_check -> false
      | Check_all | Check_opt_only -> true
    in
    begin match Zero_alloc.get val_zero_alloc with
    | Check c when c.arity = num_args && (use_opt || not c.opt) ->
      let assume : Zero_alloc.assume =
        { strict = c.strict;
          never_returns_normally = false;
          never_raises = false;
          arity = c.arity;
          loc = c.loc }
      in
      Builtin_attributes.assume_zero_alloc ~inferred:true assume
    | Check _ | Default_zero_alloc | Ignore_assert_all | Assume _ ->
      Zero_alloc_utils.Assume_info.none
    end
  | None, _ -> Zero_alloc_utils.Assume_info.none

let rec transl_exp ~scopes sort e =
  transl_exp1 ~scopes ~in_new_scope:false sort e

(* ~in_new_scope tracks whether we just opened a new scope.

   When we just opened a new scope, we avoid introducing an extraneous anonymous
   function scope and instead inherit the new scope. E.g., [let f x = ...] is
   parsed as a let-bound Pexp_function node [let f = fun x -> ...].
   We give it f's scope.
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
  | Texp_constant cst -> Lconst (Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      let return_layout = layout_exp sort body in
      transl_let ~scopes ~return_layout rec_flag pat_expr_list
        (event_before ~scopes body (transl_exp ~scopes sort body))
  | Texp_function { params; body; ret_sort; ret_mode; alloc_mode;
                    zero_alloc } ->
      transl_function ~in_new_scope ~scopes e params body
        ~alloc_mode ~ret_mode ~ret_sort ~region:true ~zero_alloc
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p},
                                       Id_prim (pmode, psort), _);
                 exp_type = prim_type; } as funct,
               oargs, pos, ap_mode, zero_alloc)
    when can_apply_primitive p pmode pos oargs ->
      let rec cut_args prim_repr oargs =
        match prim_repr, oargs with
        | [], _ -> [], oargs
        | _, [] -> failwith "Translcore cut_args"
        | ((_, arg_repr) :: prim_repr), ((_, Arg (x, _)) :: oargs) ->
          let arg_exps, extra_args = cut_args prim_repr oargs in
          let arg_sort =
            Jkind.Sort.of_const
              (Translprim.sort_of_native_repr arg_repr ~poly_sort:psort)
          in
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
      let assume_zero_alloc =
        match zero_alloc with
        | None -> Zero_alloc_utils.Assume_info.none
        | Some assume -> Builtin_attributes.assume_zero_alloc ~inferred:false assume
      in
      let lam =
        let loc =
          map_scopes (update_assume_zero_alloc ~assume_zero_alloc)
            (of_location ~scopes e.exp_loc)
        in
        Translprim.transl_primitive_application
          loc p e.exp_env prim_type
          ~poly_mode:pmode ~poly_sort:psort
          path prim_exp args (List.map fst arg_exps) position
      in
      if extra_args = [] then lam
      else begin
        let tailcall = Translattribute.get_tailcall_attribute funct in
        let inlined = Translattribute.get_inlined_attribute funct in
        let specialised = Translattribute.get_specialised_attribute funct in
        let position = transl_apply_position pos in
        let mode = transl_locality_mode_l ap_mode in
        let result_layout = layout_exp sort e in
        event_after ~scopes e
          (transl_apply ~scopes ~tailcall ~inlined ~specialised
             ~assume_zero_alloc
             ~position ~mode
             ~result_layout lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs, position, ap_mode, zero_alloc)
    ->
      let tailcall = Translattribute.get_tailcall_attribute funct in
      let inlined = Translattribute.get_inlined_attribute funct in
      let specialised = Translattribute.get_specialised_attribute funct in
      let result_layout = layout_exp sort e in
      let position = transl_apply_position position in
      let mode = transl_locality_mode_l ap_mode in
      let assume_zero_alloc =
        zero_alloc_of_application ~num_args:(List.length oargs) zero_alloc funct
      in
      event_after ~scopes e
        (transl_apply ~scopes ~tailcall ~inlined ~specialised
           ~assume_zero_alloc
           ~result_layout
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
        transl_value_list_with_shape ~scopes
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
  | Texp_unboxed_tuple el ->
      let shape = List.map (fun (_, e, s) -> layout_exp s e) el in
      let ll = List.map (fun (_, e, s) -> transl_exp ~scopes s e) el in
      Lprim(Pmake_unboxed_product shape,
            ll,
            of_location ~scopes e.exp_loc)
  | Texp_construct(_, cstr, args, alloc_mode) ->
      let args_with_sorts =
        List.mapi (fun i e ->
            let sort = Jkind.sort_of_jkind cstr.cstr_arg_jkinds.(i) in
            e, sort)
          args
      in
      let ll =
        List.map (fun (e, sort) -> transl_exp ~scopes sort e) args_with_sorts
      in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag, cstr.cstr_repr with
      | Ordinary {runtime_tag}, _ when cstr.cstr_constant ->
          assert (args_with_sorts = []);
          (* CR layouts v5: This could have void args, but for now we've ruled
             that out by checking that the sort list is empty *)
          Lconst(const_int runtime_tag)
      | Ordinary _, Variant_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Ordinary {runtime_tag}, Variant_boxed _ ->
          let constant =
            match List.map extract_constant ll with
            | exception Not_constant -> None
            | constants -> (
              match cstr.cstr_shape with
              | Constructor_mixed shape ->
                  if !Clflags.native_code then
                    let shape = transl_mixed_product_shape shape in
                    Some (Const_mixed_block(runtime_tag, shape, constants))
                  else
                    (* CR layouts v5.9: Structured constants for mixed blocks should
                       be supported in bytecode. See symtable.ml for the difficulty.
                    *)
                    None
              | Constructor_uniform_value ->
                  Some (Const_block(runtime_tag, constants)))
          in
          begin match constant with
          | Some constant -> Lconst constant
          | None ->
              let alloc_mode = transl_alloc_mode (Option.get alloc_mode) in
              let makeblock =
                match cstr.cstr_shape with
                | Constructor_uniform_value ->
                    let shape =
                      List.map (fun (e, sort) ->
                          Lambda.must_be_value (layout_exp sort e))
                        args_with_sorts
                    in
                    Pmakeblock(runtime_tag, Immutable, Some shape, alloc_mode)
                | Constructor_mixed shape ->
                    let shape = Lambda.transl_mixed_product_shape shape in
                    Pmakemixedblock(runtime_tag, Immutable, shape, alloc_mode)
              in
              Lprim (makeblock, ll, of_location ~scopes e.exp_loc)
          end
      | Extension (path, _), Variant_extensible ->
          let lam = transl_extension_path
                      (of_location ~scopes e.exp_loc) e.exp_env path in
          if cstr.cstr_constant
          then (
            assert (args_with_sorts = []);
            (* CR layouts v5: This could have void args, but for now we've ruled
               that out by checking that the sort list is empty *)
            lam)
          else
            let alloc_mode = transl_alloc_mode (Option.get alloc_mode) in
            let makeblock =
              match cstr.cstr_shape with
              | Constructor_uniform_value ->
                  let shape =
                    List.map (fun (e, sort) ->
                        Lambda.must_be_value (layout_exp sort e))
                      args_with_sorts
                  in
                  Pmakeblock(0, Immutable, Some (Pgenval :: shape),
                            alloc_mode)
              | Constructor_mixed shape ->
                  let shape = Lambda.transl_mixed_product_shape shape in
                  let shape =
                    { shape with value_prefix_len = shape.value_prefix_len + 1 }
                  in
                  Pmakemixedblock(0, Immutable, shape, alloc_mode)
            in
            Lprim (makeblock, lam :: ll, of_location ~scopes e.exp_loc)
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
  | Texp_field(arg, id, lbl, float) ->
      let targ = transl_exp ~scopes Jkind.Sort.for_record arg in
      let sem =
        if Types.is_mutable lbl.lbl_mut then Reads_vary else Reads_agree
      in
      let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
      check_record_field_sort id.loc lbl_sort;
      begin match lbl.lbl_repres with
          Record_boxed _
        | Record_inlined (_, Constructor_uniform_value, Variant_boxed _) ->
          Lprim (Pfield (lbl.lbl_pos, maybe_pointer e, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed | Record_inlined (_, _, Variant_unboxed) -> targ
        | Record_float ->
          let alloc_mode =
            match float with
            | Boxing (alloc_mode, _) -> alloc_mode
            | Non_boxing _ -> assert false
          in
          let mode = transl_alloc_mode alloc_mode in
          Lprim (Pfloatfield (lbl.lbl_pos, sem, mode), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_ufloat ->
          Lprim (Pufloatfield (lbl.lbl_pos, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_inlined (_, Constructor_uniform_value, Variant_extensible) ->
          Lprim (Pfield (lbl.lbl_pos + 1, maybe_pointer e, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_inlined (_, Constructor_mixed _, Variant_extensible) ->
            (* CR layouts v5.9: support this *)
            fatal_error
              "Mixed inlined records not supported for extensible variants"
        | Record_inlined (_, Constructor_mixed shape, Variant_boxed _)
        | Record_mixed shape ->
          let ({ value_prefix_len; flat_suffix } : mixed_product_shape) =
            shape
          in
          let read =
            if lbl.lbl_num < value_prefix_len then
              Mread_value_prefix (maybe_pointer e)
            else
              let flat_read =
                match flat_suffix.(lbl.lbl_num - value_prefix_len) with
                | Float_boxed ->
                  (match float with
                    | Boxing (mode, _) ->
                        flat_read_float_boxed (transl_alloc_mode mode)
                    | Non_boxing _ ->
                        Misc.fatal_error
                          "expected typechecking to make [float] boxing mode\
                          \ present for float field read")
                | non_float -> flat_read_non_float non_float
              in
              Mread_flat_suffix flat_read
          in
          let shape : Lambda.mixed_block_shape =
            { value_prefix_len; flat_suffix }
          in
          Lprim (Pmixedfield (lbl.lbl_pos, read, shape, sem), [targ],
                  of_location ~scopes e.exp_loc)
      end
  | Texp_setfield(arg, arg_mode, id, lbl, newval) ->
      (* CR layouts v2.5: When we allow `any` in record fields and check
         representability on construction, [sort_of_jkind] will be unsafe here.
         Probably we should add a sort to `Texp_setfield` in the typed tree,
         then. *)
      let lbl_sort = Jkind.sort_of_jkind lbl.lbl_jkind in
      check_record_field_sort id.loc lbl_sort;
      let mode =
        Assignment (transl_modify_mode arg_mode)
      in
      let access =
        match lbl.lbl_repres with
          Record_boxed _
        | Record_inlined (_, Constructor_uniform_value, Variant_boxed _) ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, mode)
        | Record_unboxed | Record_inlined (_, _, Variant_unboxed) ->
          assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, mode)
        | Record_ufloat -> Psetufloatfield (lbl.lbl_pos, mode)
        | Record_inlined (_, Constructor_uniform_value, Variant_extensible) ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, mode)
        | Record_inlined (_, Constructor_mixed _, Variant_extensible) ->
            (* CR layouts v5.9: support this *)
            fatal_error
              "Mixed inlined records not supported for extensible variants"
        | Record_inlined (_, Constructor_mixed shape, Variant_boxed _)
        | Record_mixed shape -> begin
          let ({ value_prefix_len; flat_suffix } : mixed_product_shape) =
            shape
          in
          let write =
            if lbl.lbl_num < value_prefix_len then
              Mwrite_value_prefix (maybe_pointer newval)
            else
              let flat_element = flat_suffix.(lbl.lbl_num - value_prefix_len) in
              Mwrite_flat_suffix flat_element
           in
           let shape : Lambda.mixed_block_shape =
             { value_prefix_len; flat_suffix }
           in
           Psetmixedfield(lbl.lbl_pos, write, shape, mode)
        end
      in
      Lprim(access, [transl_exp ~scopes Jkind.Sort.for_record arg;
                     transl_exp ~scopes lbl_sort newval],
            of_location ~scopes e.exp_loc)
  | Texp_array (amut, element_sort, expr_list, alloc_mode) ->
      let mode = transl_alloc_mode alloc_mode in
      let kind = array_kind e element_sort in
      let ll =
        transl_list ~scopes
          (List.map (fun e -> (e, element_sort)) expr_list)
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
        if Types.is_mutable amut then Mutable else Immutable
      in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if Types.is_mutable amut &&
           List.length ll <= use_dup_for_constant_mutable_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        (* Pduparray only works in Alloc_heap mode *)
        if is_local_mode mode then raise Not_constant;
        begin match List.map extract_constant ll with
        | exception Not_constant
          when kind = Pfloatarray && Types.is_mutable amut ->
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
                | Punboxedfloatarray _ | Punboxedintarray _ ->
                  Misc.fatal_error "Use flambda2 for unboxed arrays"
            in
            if Types.is_mutable amut then duparray_to_mutable const else const
        end
      with Not_constant ->
        makearray lambda_arr_mut
      end
  | Texp_list_comprehension comp ->
      let loc = of_location ~scopes e.exp_loc in
      Transl_list_comprehension.comprehension
        ~transl_exp ~scopes ~loc comp
  | Texp_array_comprehension (_amut, elt_sort, comp) ->
      (* We can ignore mutability here since we've already checked in in the
         type checker; both mutable and immutable arrays are created the same
         way *)
      let loc = of_location ~scopes e.exp_loc in
      let array_kind = Typeopt.array_kind e elt_sort in
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
                            (* The translation of [e] may be a function, in
                               which case disallowing arity fusion gives a very
                               small performance improvement.
                            *)
                            ~attr:function_attribute_disallowing_arity_fusion
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
  | Texp_letop{let_; ands; param; param_sort; body; body_sort; partial} ->
      event_after ~scopes e
        (transl_letop ~scopes e.exp_loc e.exp_env let_ ands
           param param_sort body body_sort partial)
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
              (* CR layouts v3: here we allow [value_or_null] because this check
                 happens too late for the typecheker to infer [non_null]. Test that
                 nothing breaks once we have null pointers. *)
                (Jkind.Builtin.value_or_null ~why:Probe)
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
          zero_alloc = Default_zero_alloc;
          loop = Never_loop;
          is_a_functor = false;
          is_opaque = false;
          stub = false;
          poll = Default_poll;
          tmc_candidate = false;
          unbox_return = false;
          may_fuse_arity = false;
        } in
      let funcid = Ident.create_local ("probe_handler_" ^ name) in
      let return_layout = layout_unit (* Probe bodies have type unit. *) in
      let handler =
        let assume_zero_alloc = get_assume_zero_alloc ~scopes in
        let scopes = enter_value_definition ~scopes ~assume_zero_alloc funcid in
        lfunction
          (* We conservatively assume that all arguments are local. This doesn't
             hurt performance as probe handlers are always applied fully. *)
          ~kind:(Curried {nlocal=List.length param_idents})
          (* CR layouts: Adjust param layouts when we allow other things in
             probes. *)
          ~params:(List.map (fun name -> { name; layout = layout_probe_arg; attributes = Lambda.default_param_attribute; mode = alloc_local }) param_idents)
          ~return:return_layout
          ~body:body
          ~loc:(of_location ~scopes exp.exp_loc)
          ~attr
          ~mode:alloc_heap
          ~ret_mode:alloc_local
          (* CR zqian: the handler function doesn't have a region. However, the
             [region] field is currently broken. *)
          ~region:true
      in
      let app =
        { ap_func = Lvar funcid;
          ap_args = List.map (fun id -> Lvar id) arg_idents;
          ap_result_layout = return_layout;
          ap_region_close = Rc_normal;
          ap_mode = alloc_local;
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
  | Texp_src_pos ->
      let pos = e.exp_loc.loc_start in
      let pos =
        match !Clflags.directory with
        | None -> pos
        | Some directory ->
          let pos_fname = directory ^ "/" ^ pos.pos_fname in
          { pos with pos_fname }
      in
      let cl =
        [ Const_base (Const_string (pos.pos_fname, e.exp_loc, None))
        ; Const_base (Const_int pos.pos_lnum)
        ; Const_base (Const_int pos.pos_bol)
        ; Const_base (Const_int pos.pos_cnum)
        ]
      in
      Lconst(Const_block(0, cl))

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
and transl_value_list_with_shape ~scopes expr_list =
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
      ?(assume_zero_alloc = Zero_alloc_utils.Assume_info.none)
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
      (* [assume_zero_alloc] is not used in the cases above but
         Misplaced_attribute won't be reported for it.
         Same for [@inlined] [@specialized] and tailcall.
         It's fine for [Lsend] cases because [assume_zero_alloc] is
         always false currently for them. *)
        let loc =
          map_scopes (update_assume_zero_alloc ~assume_zero_alloc) loc
        in
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
  (* Build a function application.
     Particular care is required for out-of-order partial applications.
     The following code guarantees that:
     * arguments are evaluated right-to-left according to their order in
       the type of the function, before the function is called;
     * side-effects occurring after receiving a parameter
       will occur exactly when all the arguments up to this parameter
       have been received.
  *)
  let rec build_apply lam args loc pos ap_mode = function
    | Omitted { mode_closure; mode_arg; mode_ret; sort_arg } :: l ->
        (* Out-of-order partial application; we will need to build a closure *)
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
        (* Evaluate the function, applied to the arguments in [args] *)
        let handle, _ = protect "func" (lam, layout_function) in
        (* Evaluate the remaining arguments;
           if we already passed here this is a no-op. *)
        let l =
          List.map
            (fun arg ->
               match arg with
               | Omitted _ -> arg
               | Arg arg -> Arg (protect "arg" arg))
            l
        in
        let id_arg = Ident.create_local "param" in
        (* Process remaining arguments and build closure *)
        let body =
          let loc = map_scopes enter_partial_or_eta_wrapper loc in
          let mode = transl_alloc_mode_r mode_closure in
          let arg_mode = transl_alloc_mode_l mode_arg in
          let ret_mode = transl_alloc_mode_l mode_ret in
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
                    ~attr:{ default_stub_attribute with may_fuse_arity = false } ~loc
        in
        (* Wrap "protected" definitions, starting from the left,
           so that evaluation is right-to-left. *)
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

(* There are two cases in function translation:
    - [Tupled]. It takes a tupled argument, and we can flatten it.
    - [Curried]. It takes each argument individually.

   We first try treating the function as taking a flattened tupled argument (in
   [trans_tupled_function]) and, if that doesn't work, we fall back to treating
   the function as taking each argument individually (in
   [trans_curried_function]).
*)
and transl_function_without_attributes
    ~scopes ~return_sort ~return_mode ~mode ~region loc repr params body =
  let return_layout =
    match body with
    | Tfunction_body exp ->
        layout_exp return_sort exp
    | Tfunction_cases cases ->
        layout cases.fc_env cases.fc_loc return_sort cases.fc_ret_type

  in
  match
    transl_tupled_function ~scopes loc params body
      ~return_sort ~return_mode ~return_layout ~mode ~region
  with
  | Some result -> result
  | None ->
      transl_curried_function ~scopes loc repr params body
        ~return_sort ~return_mode ~return_layout ~mode ~region

and transl_tupled_function
      ~scopes ~return_sort ~return_mode ~return_layout ~mode ~region loc params body
  =
  let eligible_cases =
    match params, body with
    | [],
      Tfunction_cases
        { fc_cases = { c_lhs; _ } :: _ as cases;
          fc_partial; fc_arg_mode; fc_arg_sort } ->
        Some (cases, fc_partial, c_lhs, fc_arg_mode, fc_arg_sort)
    | [{ fp_kind = Tparam_pat pat; fp_partial; fp_mode; fp_sort }],
      Tfunction_body body ->
        let case = { c_lhs = pat; c_guard = None; c_rhs = body } in
        Some ([ case ], fp_partial, pat, fp_mode, fp_sort)
    | _ -> None
  in
  (* Cases can be eligible for flattening if they belong to the only param
     (whose alloc mode must be global) and the function itself is global. It may
     actually be sound to tuplify locally-allocated functions, but we haven't
     thought it through. *)
  match eligible_cases with
  | Some
      (cases, partial,
       ({ pat_desc = Tpat_tuple pl } as arg_pat), arg_mode, arg_sort)
    when is_alloc_heap mode
      && is_alloc_heap (transl_alloc_mode_l arg_mode)
      && !Clflags.native_code
      && List.length pl <= (Lambda.max_arity ()) ->
      begin try
        let arg_layout = layout_pat arg_sort arg_pat in
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let kinds =
          match arg_layout with
          | Pvalue (Pvariant { consts = [];
                               non_consts = [0, Constructor_uniform kinds] }) ->
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
        Some
          ((Tupled, tparams, return_layout, region, return_mode), body)
    with Matching.Cannot_flatten -> None
      end
  | _ -> None

and transl_curried_function ~scopes loc repr params body
    ~return_sort ~return_layout ~return_mode ~region ~mode
  =
  let { nlocal } =
    let param_curries = List.map (fun fp -> fp.fp_curry, fp.fp_mode) params in
    curried_function_kind
      ~return_mode
      ~alloc_mode:mode
      (match body with
       | Tfunction_body _ -> param_curries
       | Tfunction_cases fc -> param_curries @ [ Final_arg, fc.fc_arg_mode ])
  in
  let cases_param, body =
    match body with
    | Tfunction_body body ->
        None, event_before ~scopes body (transl_exp ~scopes return_sort body)
    | Tfunction_cases
        { fc_cases; fc_partial; fc_param; fc_loc; fc_arg_sort; fc_arg_mode }
      ->
        let arg_layout =
          match fc_cases with
          | { c_lhs } :: _ -> layout_pat fc_arg_sort c_lhs
          | [] ->
              (* ppxes can generate empty function cases, which compiles to
                 a function that always raises Match_failure. We try less
                 hard to calculate a detailed layout that the middle-end can
                 use for optimizations. *)
              layout_of_sort fc_loc fc_arg_sort
        in
        let arg_mode = transl_alloc_mode_l fc_arg_mode in
        let attributes =
          match fc_cases with
          | [ { c_lhs }] -> Translattribute.transl_param_attributes c_lhs
          | [] | _ :: _ :: _ -> Lambda.default_param_attribute
        in
        let param =
          { name = fc_param;
            layout = arg_layout;
            attributes;
            mode = arg_mode;
          }
        in
        let body =
          Matching.for_function ~scopes fc_loc repr (Lvar fc_param)
            ~arg_sort:fc_arg_sort ~arg_layout ~return_layout
            (transl_cases ~scopes return_sort fc_cases) fc_partial
        in
        Some param, body
  in
  let body, params =
    List.fold_right
      (fun fp (body, params) ->
        let { fp_param; fp_kind; fp_mode; fp_sort; fp_partial; fp_loc } = fp in
        let arg_env, arg_type, attributes =
          match fp_kind with
          | Tparam_pat pat ->
              pat.pat_env, pat.pat_type, Translattribute.transl_param_attributes pat
          | Tparam_optional_default (pat, expr, _) ->
              expr.exp_env, Predef.type_option expr.exp_type, Translattribute.transl_param_attributes pat
        in
        let arg_layout = layout arg_env fp_loc fp_sort arg_type in
        let arg_mode = transl_alloc_mode_l fp_mode in
        let param =
          { name = fp_param;
            layout = arg_layout;
            attributes;
            mode = arg_mode;
          }
        in
        let body =
          match fp_kind with
          | Tparam_pat pat ->
              Matching.for_function ~scopes fp_loc None (Lvar fp_param)
                [ pat, body ]
                fp_partial
                ~arg_sort:fp_sort ~arg_layout
                ~return_layout
          | Tparam_optional_default (pat, default_arg, default_arg_sort) ->
              let default_arg =
                event_before ~scopes default_arg
                  (transl_exp ~scopes default_arg_sort default_arg)
              in
              Matching.for_optional_arg_default ~return_layout
                ~scopes fp_loc pat body ~default_arg ~default_arg_sort
                ~param:fp_param
        in
        body, param :: params)
      params
      (body, Option.to_list cases_param)
    in
    (* chunk params according to Lambda.max_arity. If Lambda.max_arity = n and
      N>n, then the translation of an N-ary typedtree function is an n-ary lambda
      function returning the translation of an (N-n)-ary typedtree function.
    *)
    let module Chunk = struct
      (* An [acc] is defined in respect to a "chunk" of params. This chunk
         of params together with the [body] field form a function.
      *)
      type acc =
        { body : lambda; (* The function body of those params *)
          return_layout : layout; (* The layout of [body] *)
          return_mode : alloc_mode; (* The mode of [body]. *)
          region : bool; (* Whether the function has its own region *)
          nlocal : int;
          (* An upper bound on the [nlocal] field for the function. If [nlocal]
             exceeds the length of the chunk of params, the difference will
             become the nlocal field with respect to the *enclosing* chunk
             of params.
          *)
        }

      (* Meant to be used with a [fold_right]. The returned [acc] is in
         respect to the enclosing chunk.
      *)
      let process_inner_chunk
          chunk { body; return_layout; return_mode; nlocal; region }
        =
        let chunk_length = List.length chunk in
        let loc = of_location ~scopes loc in
        (* The current function is locally-allocated (and thus its
           enclosing chunk doesn't have a region) when nlocal isn't
           yet exhausted in the current chunk.
        *)
        let current_nlocal, current_mode, enclosing_region =
          if nlocal > chunk_length
          then chunk_length, alloc_local, false
          else nlocal, mode, true
        in
        let enclosing_nlocal = nlocal - current_nlocal in
        let body =
          if region then maybe_region_layout return_layout body else body
        in
        let body =
          lfunction
            ~kind:
              (Curried { nlocal=current_nlocal })
            ~params:chunk ~mode:current_mode
            ~return:return_layout ~ret_mode:return_mode ~body
            ~attr:function_attribute_disallowing_arity_fusion
            ~loc ~region
        in
        (* we return Pgenval (for a function) after the rightmost chunk *)
        { body;
          return_layout = Pvalue Pgenval;
          return_mode = (if enclosing_region then alloc_heap else alloc_local);
          nlocal = enclosing_nlocal;
          region = enclosing_region;
        }
    end
    in
    (* The Chunk.acc is in respect to the [params] chunk. *)
    let params,
        ({ body; return_layout; return_mode; region; nlocal } : Chunk.acc) =
      match Misc.Stdlib.List.chunks_of (Lambda.max_arity ()) params with
      | [] ->
          Misc.fatal_error
            "attempted to translate a function with zero arguments"
      | first_chunk :: rest_of_chunks ->
        let region = region || not (may_allocate_in_region body) in
        let acc =
          List.fold_right
            Chunk.process_inner_chunk
            rest_of_chunks
            ({ body; return_layout; return_mode; nlocal; region } : Chunk.acc)
        in
        first_chunk, acc
    in
    ((Curried { nlocal }, params, return_layout, region, return_mode ), body)

and transl_function ~in_new_scope ~scopes e params body
      ~alloc_mode ~ret_mode:sreturn_mode ~ret_sort:sreturn_sort ~region:sregion
      ~zero_alloc =
  let attrs = e.exp_attributes in
  let mode = transl_alloc_mode alloc_mode in
  let zero_alloc = Zero_alloc.get zero_alloc in
  let assume_zero_alloc =
    match zero_alloc with
    | Default_zero_alloc | Check _ | Ignore_assert_all ->
      Zero_alloc_utils.Assume_info.none
    | Assume assume ->
      Builtin_attributes.assume_zero_alloc ~inferred:false assume
  in
  let scopes =
    if in_new_scope then
      update_assume_zero_alloc ~scopes ~assume_zero_alloc
    else enter_anonymous_function ~scopes ~assume_zero_alloc
  in
  let sreturn_mode = transl_alloc_mode_l sreturn_mode in
  let { params; body; return_sort; return_mode; region } =
    fuse_method_arity
      { params; body;
        return_sort = sreturn_sort;
        return_mode = sreturn_mode;
        region = sregion;
      }
  in
  (* [ret_mode] may differ from [sreturn_mode] if:
       - [e] is a method. (See [fuse_method_arity].)
       - [e] is a function whose arity exceeds [Lambda.max_arity].
         (See the chunking code in [transl_curried_function].)
  *)
  let ((kind, params, return, region, ret_mode), body) =
    event_function ~scopes e
      (function repr ->
         transl_function_without_attributes
           ~mode ~return_sort ~return_mode
           ~scopes e.exp_loc repr ~region params body)
  in
  let zero_alloc : Lambda.zero_alloc_attribute =
    match (zero_alloc : Builtin_attributes.zero_alloc_attribute) with
    | Default_zero_alloc ->
      if !Clflags.zero_alloc_check_assert_all &&
         Builtin_attributes.is_zero_alloc_check_enabled ~opt:false
      then Check { strict = false; loc = e.exp_loc }
      else Default_zero_alloc
    | Check { strict; opt; arity = _; loc } ->
      if Builtin_attributes.is_zero_alloc_check_enabled ~opt
      then Check { strict; loc }
      else Default_zero_alloc
    | Assume { strict; never_returns_normally; never_raises; loc; arity = _; } ->
      Assume { strict; never_returns_normally; never_raises; loc }
    | Ignore_assert_all -> Default_zero_alloc
  in
  let attr =
    { function_attribute_disallowing_arity_fusion with zero_alloc }
  in
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
      let assume_zero_alloc = Zero_alloc_utils.Assume_info.none in
      (* If this is a let-binding of a function, the scope will be updated
         with zero_alloc info in [transl_function]. *)
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
      | {vb_pat=pat; vb_expr=expr; vb_sort=sort; vb_rec_kind=_; vb_attributes; vb_loc}
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
            {vb_expr=expr; vb_sort; vb_attributes; vb_rec_kind = rkind;
             vb_loc; vb_pat} id =
        let def =
          transl_bound_exp ~scopes ~in_structure vb_pat vb_sort expr vb_loc vb_attributes
        in
        let def =
          if add_regions then maybe_region_exp vb_sort expr def else def
        in
        ( id, rkind, def ) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Value_rec_compiler.compile_letrec lam_bds body

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
               let field_layout = layout env lbl.lbl_loc lbl_sort typ in
               let sem =
                 if Types.is_mutable mut then Reads_vary else Reads_agree
               in
               let access =
                 match repres with
                   Record_boxed _
                 | Record_inlined (_, Constructor_uniform_value, Variant_boxed _) ->
                   Pfield (i, maybe_pointer_type env typ, sem)
                 | Record_unboxed | Record_inlined (_, _, Variant_unboxed) ->
                   assert false
                 | Record_inlined (_, Constructor_uniform_value, Variant_extensible) ->
                     Pfield (i + 1, maybe_pointer_type env typ, sem)
                 | Record_inlined (_, Constructor_mixed _, Variant_extensible) ->
                     (* CR layouts v5.9: support this *)
                     fatal_error
                       "Mixed inlined records not supported for extensible variants"
                 | Record_float ->
                    (* This allocation is always deleted,
                       so it's simpler to leave it Alloc_heap *)
                    Pfloatfield (i, sem, alloc_heap)
                 | Record_ufloat -> Pufloatfield (i, sem)
                 | Record_inlined (_, Constructor_mixed shape, Variant_boxed _)
                 | Record_mixed shape ->
                  let { value_prefix_len; flat_suffix } : mixed_product_shape =
                    shape
                  in
                   let read =
                    if lbl.lbl_num < value_prefix_len then
                      Mread_value_prefix (maybe_pointer_type env typ)
                    else
                      let read =
                        match flat_suffix.(lbl.lbl_num - value_prefix_len) with
                        | Float_boxed ->
                            (* See the handling of [Record_float] above for
                                why we choose Alloc_heap.
                            *)
                            flat_read_float_boxed alloc_heap
                        | non_float -> flat_read_non_float non_float
                      in
                      Mread_flat_suffix read
                   in
                   let shape : Lambda.mixed_block_shape =
                     { value_prefix_len; flat_suffix }
                   in
                   Pmixedfield (i, read, shape, sem)
               in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_layout
           | Overridden (_lid, expr) ->
               let field_layout = layout_exp lbl_sort expr in
               transl_exp ~scopes lbl_sort expr, field_layout)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut : Lambda.mutable_flag =
      if Array.exists (fun (lbl, _) -> Types.is_mutable lbl.lbl_mut) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_boxed _ -> Lconst(Const_block(0, cl))
        | Record_inlined (Ordinary {runtime_tag},
                          Constructor_uniform_value, Variant_boxed _) ->
            Lconst(Const_block(runtime_tag, cl))
        | Record_unboxed | Record_inlined (_, _, Variant_unboxed) ->
            Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_block(List.map extract_float cl))
        | Record_mixed shape ->
            if !Clflags.native_code then
              let shape = transl_mixed_product_shape shape in
              Lconst(Const_mixed_block(0, shape, cl))
            else
              (* CR layouts v5.9: Structured constants for mixed blocks should
                 be supported in bytecode. See symtable.ml for the difficulty.
              *)
              raise Not_constant
        | Record_inlined (_, Constructor_mixed _, Variant_boxed _)
        | Record_ufloat ->
            (* CR layouts v5.1: We should support structured constants for
               blocks containing unboxed float literals.
            *)
            raise Not_constant
        | Record_inlined (_, _, Variant_extensible)
        | Record_inlined (Extension _, _, _) ->
            raise Not_constant
      with Not_constant ->
        let loc = of_location ~scopes loc in
        match repres with
          Record_boxed _ ->
            let shape = List.map must_be_value shape in
            Lprim(Pmakeblock(0, mut, Some shape, Option.get mode), ll, loc)
        | Record_inlined (Ordinary {runtime_tag},
                          Constructor_uniform_value, Variant_boxed _) ->
            let shape = List.map must_be_value shape in
            Lprim(Pmakeblock(runtime_tag, mut, Some shape, Option.get mode),
                  ll, loc)
        | Record_unboxed | Record_inlined (Ordinary _, _, Variant_unboxed) ->
            (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakefloatblock (mut, Option.get mode), ll, loc)
        | Record_ufloat ->
            Lprim(Pmakeufloatblock (mut, Option.get mode), ll, loc)
        | Record_inlined (Extension _,
                          Constructor_mixed _, Variant_extensible) ->
            (* CR layouts v5.9: support this *)
            fatal_error
              "Mixed inlined records not supported for extensible variants"
        | Record_inlined (Extension (path, _),
                          Constructor_uniform_value, Variant_extensible) ->
            let shape = List.map must_be_value shape in
            let slot = transl_extension_path loc env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape), Option.get mode),
                  slot :: ll, loc)
        | Record_inlined (Extension _, _, (Variant_unboxed | Variant_boxed _))
        | Record_inlined (Ordinary _, _, Variant_extensible) ->
            assert false
        | Record_mixed shape ->
            let shape = transl_mixed_product_shape shape in
            Lprim (Pmakemixedblock (0, mut, shape, Option.get mode), ll, loc)
        | Record_inlined (Ordinary { runtime_tag },
                          Constructor_mixed shape, Variant_boxed _) ->
            let shape = transl_mixed_product_shape shape in
            Lprim (Pmakemixedblock (runtime_tag, mut, shape, Option.get mode),
                   ll, loc)
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
      check_record_field_sort lbl.lbl_loc lbl_sort;
      match definition with
      | Kept _ -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_boxed _
            | Record_inlined (_, Constructor_uniform_value, Variant_boxed _) ->
                let ptr = maybe_pointer expr in
                Psetfield(lbl.lbl_pos, ptr, Assignment modify_heap)
            | Record_unboxed | Record_inlined (_, _, Variant_unboxed) ->
                assert false
            | Record_float ->
                Psetfloatfield (lbl.lbl_pos, Assignment modify_heap)
            | Record_ufloat ->
                Psetufloatfield (lbl.lbl_pos, Assignment modify_heap)
            | Record_inlined (_, Constructor_uniform_value, Variant_extensible) ->
                let pos = lbl.lbl_pos + 1 in
                let ptr = maybe_pointer expr in
                Psetfield(pos, ptr, Assignment modify_heap)
            | Record_inlined (_, Constructor_mixed _, Variant_extensible) ->
                (* CR layouts v5.9: support this *)
                fatal_error
                  "Mixed inlined records not supported for extensible variants"
            | Record_inlined (_, Constructor_mixed shape, Variant_boxed _)
            | Record_mixed shape -> begin
                let { value_prefix_len; flat_suffix } : mixed_product_shape =
                  shape
                in
                let write =
                  if lbl.lbl_num < value_prefix_len then
                    let ptr = maybe_pointer expr in
                    Mwrite_value_prefix ptr
                  else
                    let flat_element =
                      flat_suffix.(lbl.lbl_num - value_prefix_len)
                    in
                    Mwrite_flat_suffix flat_element
                in
                let shape : Lambda.mixed_block_shape =
                  { value_prefix_len; flat_suffix }
                in
                Psetmixedfield
                  (lbl.lbl_pos, write, shape, Assignment modify_heap)
              end
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
        let ids = List.map (fun (id, _, _, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, {Location.loc; _}, ty, _, s) ->
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
      Same_region, return_layout)
  in
  let classic =
    match arg, exn_cases with
    | {exp_desc = Texp_tuple (argl, alloc_mode)}, [] ->
      (* CR layouts v7.1: This case and the one below it give special treatment
         to matching on literal tuples. This optimization is irrelevant for
         unboxed tuples in native code, but not doing it for unboxed tuples in
         bytecode means unboxed tuple are slightly worse than normal tuples
         there. Consider adding it for unboxed tuples. *)
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
    Lstaticcatch
      (body, (static_exception_id, val_ids),
       handler, Same_region, return_layout)
  ) classic static_handlers

and transl_letop ~scopes loc env let_ ands param param_sort case case_sort
      partial =
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
    let return_mode = alloc_heap (* XXX fixme: use result of is_function_type *) in
    let (kind, params, return, _region, ret_mode), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           let loc = case.c_rhs.exp_loc in
           let ghost_loc = { loc with loc_ghost = true } in
           transl_function_without_attributes ~scopes ~region:true
             ~return_sort:case_sort ~mode:alloc_heap ~return_mode
             loc repr []
             (Tfunction_cases
                { fc_cases = [case]; fc_param = param; fc_partial = partial;
                  fc_loc = ghost_loc; fc_exp_extra = None; fc_attributes = [];
                  fc_arg_mode = Mode.Alloc.disallow_right Mode.Alloc.legacy;
                  fc_arg_sort = param_sort; fc_env = env;
                  fc_ret_type = case.c_rhs.exp_type;
                }))
    in
    let attr = function_attribute_disallowing_arity_fusion in
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
      ~scopes ?tailcall ?inlined ?specialised ?position ?mode ~result_layout fn
      args loc =
  maybe_region_layout result_layout
    (transl_apply
       ~scopes ?tailcall ?inlined ?specialised
       ~assume_zero_alloc:Zero_alloc_utils.Assume_info.none ?position ?mode
       ~result_layout fn args loc)

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
  | Illegal_void_record_field ->
      fprintf ppf
        "Void sort detected where value was expected in a record field:@ Please \
         report this error to the Jane Street compilers team."
  | Illegal_product_record_field c ->
      fprintf ppf
        "Product sort %a detected in a record field:@ Please \
         report this error to the Jane Street compilers team."
        Jkind.Sort.Const.format c
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
