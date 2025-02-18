(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Instruct
open Asttypes
open Debuginfo.Scoped_location
open Lambda
open Blambda

let ccall name args = Variadic (Ccall name, args)

(**** Compilation of a lambda expression ****)

(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let is_nontail = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

module Storer = Switch.Store (struct
  type t = lambda

  type key = lambda

  let compare_key = Stdlib.compare

  let make_key = Lambda.make_key
end)

let rec comp_rec_binding ({ id; def } : Lambda.rec_binding) :
    Blambda.rec_binding =
  { id; def = comp_fun def }

and comp_fun ({ params; body; loc } : Lambda.lfunction) : Blambda.bfunction =
  (* assume kind = Curried *)
  { params = List.map (fun p -> p.name) params;
    body = comp_expr body ~tailcall:Tailcall;
    loc
  }

and comp_expr (exp : Lambda.lambda) ~(tailcall : Blambda.tailcall) : Blambda.t =
  let[@inline] comp_arg arg = comp_expr arg ~tailcall:Nontail in
  match (exp : Lambda.lambda) with
  | Lvar id | Lmutvar id -> Var id
  | Lconst cst -> Const cst
  | Lapply { ap_func = func; ap_args = args; ap_region_close = rc } ->
    Apply
      { func = comp_arg func;
        args = List.map comp_arg args;
        tailcall = (if is_nontail rc then Nontail else tailcall)
      }
  | Lsend (kind, met, obj, args, rc, _, _, _) ->
    assert (kind <> Cached);
    Send
      { kind;
        met = comp_arg met;
        obj = comp_arg obj;
        args = List.map comp_arg args;
        tailcall = (if is_nontail rc then Nontail else tailcall)
      }
  | Lfunction f -> Function (comp_fun f)
  | Llet (_, _k, id, arg, body) | Lmutlet (_k, id, arg, body) ->
    Let { id; arg = comp_arg arg; body = comp_expr body ~tailcall }
  | Lletrec (decl, body) ->
    Letrec
      { decls = List.map comp_rec_binding decl;
        body = comp_expr body ~tailcall
      }
  | Lstaticcatch (body, (i, vars), handler, _, _) ->
    Staticcatch
      { body = comp_arg body;
        args = i, List.map fst vars;
        handler = comp_expr handler ~tailcall
      }
  | Lstaticraise (i, args) -> Staticraise (i, List.map comp_arg args)
  | Ltrywith (body, id, handler, _kind) ->
    Trywith { body = comp_arg body; id; handler = comp_expr handler ~tailcall }
  | Lifthenelse (cond, ifso, ifnot, _kind) ->
    Ifthenelse
      { cond = comp_arg cond;
        ifso = comp_expr ifso ~tailcall;
        ifnot = comp_expr ifnot ~tailcall
      }
  | Lsequence (exp1, exp2) -> Sequence (comp_arg exp1, comp_expr exp2 ~tailcall)
  | Lwhile { wh_cond; wh_body } ->
    While { wh_cond = comp_arg wh_cond; wh_body = comp_arg wh_body }
  | Lfor { for_id; for_from; for_to; for_dir; for_body } ->
    For
      { for_id;
        for_from = comp_arg for_from;
        for_to = comp_arg for_to;
        for_dir;
        for_body = comp_arg for_body
      }
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        _loc,
        _kind ) ->
    let[@inline] comp_case e = comp_expr e ~tailcall in
    let comp_arm (i, v) = i, comp_case v in
    Switch
      { arg = comp_arg arg;
        sw_numconsts;
        sw_consts = List.map comp_arm sw_consts;
        sw_numblocks;
        sw_blocks = List.map comp_arm sw_blocks;
        sw_failaction = Option.map comp_case sw_failaction
      }
  | Lstringswitch (arg, sw, d, loc, kind) ->
    comp_expr (Matching.expand_stringswitch loc kind arg sw d) ~tailcall
  | Lassign (id, expr) -> Assign (id, comp_arg expr)
  | Levent (lam, lev) -> Event (comp_expr lam ~tailcall, lev)
  | Lifused (_, exp) | Lregion (exp, _) | Lexclave exp ->
    comp_expr exp ~tailcall
  | Lprim (prim, args, loc) -> (
    let wrong_arity ~expected =
      Misc.fatal_errorf "Blambda_of_lambda.comp_expr: %a takes %d %s"
        Printlambda.primitive prim expected
        (if expected = 1 then "argument" else "arguments")
    in
    let nullary prim =
      match args with
      | [] -> Prim (Nullary prim, loc)
      | _ :: _ -> wrong_arity ~expected:0
    in
    let unary prim =
      match args with
      | [arg] -> Prim (Unary (prim, comp_arg arg), loc)
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1
    in
    let binary prim =
      match args with
      | [arg1; arg2] -> Prim (Binary (prim, comp_arg arg1, comp_arg arg2), loc)
      | [] | [_] | _ :: _ :: _ :: _ -> wrong_arity ~expected:2
    in
    let ternary prim =
      match args with
      | [arg1; arg2; arg3] ->
        Prim (Ternary (prim, comp_arg arg1, comp_arg arg2, comp_arg arg3), loc)
      | [] | [_] | [_; _] | _ :: _ :: _ :: _ :: _ -> wrong_arity ~expected:3
    in
    let quaternary prim =
      match args with
      | [arg1; arg2; arg3; arg4] ->
        Prim
          ( Quaternary
              (prim, comp_arg arg1, comp_arg arg2, comp_arg arg3, comp_arg arg4),
            loc )
      | [] | [_] | [_; _] | [_; _; _] | _ :: _ :: _ :: _ :: _ :: _ ->
        wrong_arity ~expected:4
    in
    let variadic prim = Prim (Variadic (prim, List.map comp_arg args), loc) in
    let ccall name ~arity =
      if List.length args = arity
      then variadic (Ccall name)
      else wrong_arity ~expected:arity
    in
    let boolnot arg = Blambda.Prim (Unary (Boolnot, arg), loc) in
    let comp_bint_primitive bi suff ~arity =
      let pref =
        match bi with
        | Boxed_nativeint -> "caml_nativeint_"
        | Boxed_int32 -> "caml_int32_"
        | Boxed_int64 -> "caml_int64_"
      in
      ccall (pref ^ suff) ~arity
    in
    let indexing_primitive (index_kind : Lambda.array_index_kind) prefix ~arity
        =
      let suffix =
        match index_kind with
        | Ptagged_int_index -> ""
        | Punboxed_int_index Unboxed_int64 -> "_indexed_by_int64"
        | Punboxed_int_index Unboxed_int32 -> "_indexed_by_int32"
        | Punboxed_int_index Unboxed_nativeint -> "_indexed_by_nativeint"
      in
      ccall (prefix ^ suffix) ~arity
    in
    match prim with
    | Popaque _ | Pobj_magic _
    | Pbox_float ((Boxed_float64 | Boxed_float32), _)
    | Punbox_float (Boxed_float64 | Boxed_float32)
    | Pbox_int _ | Punbox_int _ -> (
      match args with
      | [arg] ->
        (* in bytecode we only deal with boxed+tagged floats and ints *)
        comp_expr arg ~tailcall
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pignore -> (
      match args with
      | [arg] ->
        (* in bytecode we only deal with boxed+tagged floats and ints *)
        Sequence (comp_arg arg, Const (Const_base (Const_int 0)))
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pnot -> unary Boolnot
    | Psequand -> binary Sequand
    | Psequor -> binary Sequor
    | Praise k -> unary (Raise k)
    | Paddint -> binary Addint
    | Psubint -> binary Subint
    | Pmulint -> binary Mulint
    | Pdivint (Safe | Unsafe) -> binary Divint
    | Pmodint (Safe | Unsafe) -> binary Modint
    | Pandint -> binary Andint
    | Porint -> binary Orint
    | Pxorint -> binary Xorint
    | Plslint -> binary Lslint
    | Plsrint -> binary Lsrint
    | Pasrint -> binary Asrint
    | Pnegint -> unary Negint
    | Poffsetint n -> unary (Offsetint n)
    | Pmakefloatblock _ | Pmakeufloatblock _ ->
      (* In bytecode, float# is boxed, so we can treat these two primitives the
         same. *)
      variadic Makefloatblock
    | Pmakemixedblock (tag, _, shape, _) ->
      (* There is no notion of a mixed block at runtime in bytecode. Further,
         source-level unboxed types are represented as boxed in bytecode, so
         no ceremony is needed to box values before inserting them into
         the (normal, unmixed) block.
      *)
      let total_len = shape.value_prefix_len + Array.length shape.flat_suffix in
      variadic (Make_faux_mixedblock { total_len; tag }) args
    | Pmakearray (kind, _, _) -> (
      match kind with
      (* arrays of unboxed types have the same representation
         as the boxed ones on bytecode *)
      | Pintarray | Paddrarray | Punboxedintarray _
      | Punboxedfloatarray Unboxed_float32
      | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
        Prim (Makeblock { tag = 0; args = List.map comp_expr args }, loc)
      | Pfloatarray | Punboxedfloatarray Unboxed_float64 ->
        variadic Makefloatblock args
      | Punboxedvectorarray _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray -> (
        match args with
        | [] as args -> variadic (Makeblock { tag = 0 }) args
        | _ :: _ as args ->
          Prim
            ( Variadic
                ( Ccall "caml_make_array",
                  [variadic (Makeblock { tag = 0 }) args] ),
              loc )))
    | Presume -> quaternary (Resume tailcall)
    | Prunstack -> ternary (Runstack tailcall)
    | Preperform -> (
      match tailcall with
      | Nontail -> Misc.fatal_error "Reperform used in non-tail position"
      | Tailcall -> ternary Reperform)
    | Pmakearray_dynamic (kind, locality, Uninitialized) -> (
      (* Use a dummy initializer to implement the "uninitialized" primitive *)
      let init =
        match kind with
        | Pgenarray | Paddrarray | Pintarray | Pfloatarray
        | Pgcscannableproductarray _ ->
          Misc.fatal_errorf
            "Array kind %s should have been ruled out by the frontend for \
             %%makearray_dynamic_uninit"
            (Printlambda.array_kind kind)
        | Punboxedfloatarray Unboxed_float32 ->
          Lconst (Const_base (Const_float32 "0.0"))
        | Punboxedfloatarray Unboxed_float64 ->
          Lconst (Const_base (Const_float "0.0"))
        | Punboxedintarray Unboxed_int32 -> Lconst (Const_base (Const_int32 0l))
        | Punboxedintarray Unboxed_int64 -> Lconst (Const_base (Const_int64 0L))
        | Punboxedintarray Unboxed_nativeint ->
          Lconst (Const_base (Const_nativeint 0n))
        | Punboxedvectorarray _ ->
          Misc.fatal_error "SIMD is not supported in bytecode mode."
        | Pgcignorableproductarray ignorables ->
          let rec convert_ignorable
              (ign : Lambda.ignorable_product_element_kind) =
            match ign with
            | Pint_ignorable -> Lconst (Const_base (Const_int 0))
            | Punboxedfloat_ignorable Unboxed_float32 ->
              Lconst (Const_base (Const_float32 "0.0"))
            | Punboxedfloat_ignorable Unboxed_float64 ->
              Lconst (Const_base (Const_float "0.0"))
            | Punboxedint_ignorable Unboxed_int32 ->
              Lconst (Const_base (Const_int32 0l))
            | Punboxedint_ignorable Unboxed_int64 ->
              Lconst (Const_base (Const_int64 0L))
            | Punboxedint_ignorable Unboxed_nativeint ->
              Lconst (Const_base (Const_nativeint 0n))
            | Pproduct_ignorable ignorables ->
              let fields = List.map convert_ignorable ignorables in
              Lprim (Pmakeblock (0, Immutable, None, alloc_heap), fields, loc)
          in
          convert_ignorable (Pproduct_ignorable ignorables)
      in
      match args with
      | [len] ->
        comp_expr
          (Lprim
             ( Pmakearray_dynamic (kind, locality, With_initializer),
               [len; init],
               loc ))
          ~tailcall
      | _ -> wrong_arity ~expected:1)
    | Pduparray (kind, mutability) -> (
      match args with
      | [Lprim (Pmakearray (kind', _, m), args, _)] ->
        assert (kind = kind');
        comp_expr stack_info env
          (Lprim (Pmakearray (kind, mutability, m), args, loc))
          sz cont
      | _ -> ccall "caml_obj_dup" ~arity:1)
    | Pfloatcomp (Boxed_float64, cmp)
    | Punboxed_float_comp (Unboxed_float64, cmp) ->
      let cont =
        match cmp with
        | CFeq -> Kccall ("caml_eq_float", 2) :: cont
        | CFneq -> Kccall ("caml_neq_float", 2) :: cont
        | CFlt -> Kccall ("caml_lt_float", 2) :: cont
        | CFnlt -> Kccall ("caml_lt_float", 2) :: Kboolnot :: cont
        | CFgt -> Kccall ("caml_gt_float", 2) :: cont
        | CFngt -> Kccall ("caml_gt_float", 2) :: Kboolnot :: cont
        | CFle -> Kccall ("caml_le_float", 2) :: cont
        | CFnle -> Kccall ("caml_le_float", 2) :: Kboolnot :: cont
        | CFge -> Kccall ("caml_ge_float", 2) :: cont
        | CFnge -> Kccall ("caml_ge_float", 2) :: Kboolnot :: cont
      in
      comp_args stack_info env args sz cont
    | Pfloatcomp (Boxed_float32, cmp)
    | Punboxed_float_comp (Unboxed_float32, cmp) ->
      let cont =
        match cmp with
        | CFeq -> Kccall ("caml_eq_float32", 2) :: cont
        | CFneq -> Kccall ("caml_neq_float32", 2) :: cont
        | CFlt -> Kccall ("caml_lt_float32", 2) :: cont
        | CFnlt -> Kccall ("caml_lt_float32", 2) :: Kboolnot :: cont
        | CFgt -> Kccall ("caml_gt_float32", 2) :: cont
        | CFngt -> Kccall ("caml_gt_float32", 2) :: Kboolnot :: cont
        | CFle -> Kccall ("caml_le_float32", 2) :: cont
        | CFnle -> Kccall ("caml_le_float32", 2) :: Kboolnot :: cont
        | CFge -> Kccall ("caml_ge_float32", 2) :: cont
        | CFnge -> Kccall ("caml_ge_float32", 2) :: Kboolnot :: cont
      in
      comp_args stack_info env args sz cont
    | Pmakeblock (tag, _mut, _, _) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args stack_info env args sz
        (Kmakeblock (List.length args, tag) :: cont)
    | Pmake_unboxed_product _ ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args stack_info env args sz (Kmakeblock (List.length args, 0) :: cont)
    | Pfloatfield (n, _, _) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args stack_info env args sz (Kgetfloatfield n :: cont)
    | Pgetglobal cu -> nullary (Getglobal cu)
    | Psetglobal cu -> unary (Setglobal cu)
    | Pgetpredef id -> nullary (Getpredef id)
    | Pintcomp cmp -> binary (Intcomp cmp)
    | Pcompare_ints -> ccall "caml_int_compare" ~arity:2
    | Pcompare_floats Boxed_float64 -> ccall "caml_float_compare" ~arity:2
    | Pcompare_floats Boxed_float32 -> ccall "caml_float32_compare" ~arity:2
    | Pcompare_bints bi -> comp_bint_primitive bi "compare" ~arity:2
    | Pfield (n, _, _) | Punboxed_product_field (n, _) -> unary (Getfield n)
    | Parray_element_size_in_bytes _array_kind ->
      let word_size : Lambda.lambda =
        Lprim (Pctconst Word_size, [Lambda.lambda_unit], loc)
      in
      let element_size : Lambda.lambda =
        Lprim (Plsrint, [word_size; Lconst (Const_base (Const_int 3))], loc)
      in
      comp_expr (Lsequence (arg, element_size)) ~tailcall
    | Pfield_computed _sem,  -> binary Getvectitem
    | Psetfield (n, _ptr, _init), [arg1; arg2] -> binary (Setfield n) arg1 arg2
    | Psetfield_computed (_ptr, _init), [arg1; arg2; arg3] ->
      ternary Setvectitem arg1 arg2 arg3
    (* In bytecode, float#s are boxed.  So, we can use the existing float
       instructions for the ufloat primitives. *)
    | (Pfloatfield (n, _, _) | Pufloatfield (n, _)), [arg] ->
      unary (Getfloatfield n) arg
    | (Psetfloatfield (n, _) | Psetufloatfield (n, _)), [arg1; arg2] ->
      binary (Setfloatfield n) arg1 arg2
    | Pmixedfield (n, _, _, _), [arg] ->
      (* CR layouts: This will need reworking if we ever want bytecode
         to unbox fields that are written with unboxed types in the source
         language. *)
      (* Note, non-value mixed fields are always boxed in bytecode; they
         aren't stored flat like they are in native code.
      *)
      unary (Getfield n) arg
    | Psetmixedfield (n, _, _shape, _init), [arg1; arg2] ->
      (* See the comment in the [Pmixedfield] case. *)
      binary (Setfield n) arg1 arg2
    | Pduprecord _, [arg] -> ccall "caml_obj_dup" [arg]
    | Pccall p, args ->
      assert (p.prim_arity = List.length args);
      ccall p.prim_name args
    | Pperform, [arg] -> unary Perform arg
    | Poffsetref n, [arg] -> unary (Offsetref n) arg
    | Pintoffloat Boxed_float64, [arg] -> ccall "caml_int_of_float" [arg]
    | Pfloatofint (Boxed_float64, _), [arg] -> ccall "caml_float_of_int" [arg]
    | Pfloatoffloat32 _, [arg] -> ccall "caml_float_of_float32" [arg]
    | Pfloat32offloat _, [arg] -> ccall "caml_float32_of_float" [arg]
    | Pnegfloat (Boxed_float64, _), [arg] -> ccall "caml_neg_float" [arg]
    | Pabsfloat (Boxed_float64, _), [arg] -> ccall "caml_abs_float" [arg]
    | Paddfloat (Boxed_float64, _), [arg1; arg2] ->
      ccall "caml_add_float" [arg1; arg2]
    | Psubfloat (Boxed_float64, _), [arg1; arg2] ->
      ccall "caml_sub_float" [arg1; arg2]
    | Pmulfloat (Boxed_float64, _), [arg1; arg2] ->
      ccall "caml_mul_float" [arg1; arg2]
    | Pdivfloat (Boxed_float64, _), [arg1; arg2] ->
      ccall "caml_div_float" [arg1; arg2]
    | Pintoffloat Boxed_float32, [arg] -> ccall "caml_int_of_float32" [arg]
    | Pfloatofint (Boxed_float32, _), [arg] -> ccall "caml_float32_of_int" [arg]
    | Pnegfloat (Boxed_float32, _), [arg] -> ccall "caml_neg_float32" [arg]
    | Pabsfloat (Boxed_float32, _), [arg] -> ccall "caml_abs_float32" [arg]
    | Paddfloat (Boxed_float32, _), [arg1; arg2] ->
      ccall "caml_add_float32" [arg1; arg2]
    | Psubfloat (Boxed_float32, _), [arg1; arg2] ->
      ccall "caml_sub_float32" [arg1; arg2]
    | Pmulfloat (Boxed_float32, _), [arg1; arg2] ->
      ccall "caml_mul_float32" [arg1; arg2]
    | Pdivfloat (Boxed_float32, _), [arg1; arg2] ->
      ccall "caml_div_float32" [arg1; arg2]
    | Pstringlength, [arg] -> ccall "caml_ml_string_length" [arg]
    | Pbyteslength, [arg] -> ccall "caml_ml_bytes_length" [arg]
    | Pstringrefs, [arg1; arg2] -> ccall "caml_string_get" [arg1; arg2]
    | Pbytesrefs, [arg1; arg2] -> ccall "caml_bytes_get" [arg1; arg2]
    | Pbytessets, [arg1; arg2; arg3] -> ccall "caml_bytes_set" [arg1; arg2; arg3]
    | Pstringrefu, [arg1; arg2] -> binary Getstringchar arg1 arg2
    | Pbytesrefu, [arg1; arg2] -> binary Getbyteschar arg1 arg2
    | Pbytessetu, [arg1; arg2; arg3] -> ternary Setbyteschar arg1 arg2 arg3
    | Pstring_load_16 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_string_get16" [arg1; arg2]
    | Pstring_load_32 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_string_get32" [arg1; arg2]
    | Pstring_load_f32 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_string_getf32" [arg1; arg2]
    | Pstring_load_64 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_string_get64" [arg1; arg2]
    | Pbytes_set_16 { index_kind; _ }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_bytes_set16" [arg1; arg2; arg3]
    | Pbytes_set_32 { index_kind; _ }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_bytes_set32" [arg1; arg2; arg3]
    | Pbytes_set_f32 { index_kind; _ }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_bytes_setf32" [arg1; arg2; arg3]
    | Pbytes_set_64 { index_kind; _ }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_bytes_set64" [arg1; arg2; arg3]
    | Pbytes_load_16 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_bytes_get16" [arg1; arg2]
    | Pbytes_load_32 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_bytes_get32" [arg1; arg2]
    | Pbytes_load_f32 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_bytes_getf32" [arg1; arg2]
    | Pbytes_load_64 { index_kind; _ }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_bytes_get64" [arg1; arg2]
    | Parraylength _, [arg] -> unary Vectlength arg
    (* In bytecode, nothing is ever actually stack-allocated, so we ignore the
       array modes (allocation for [Parrayref{s,u}], modification for
       [Parrayset{s,u}]). *)
    | ( ( Parrayrefs (Pgenarray_ref _, index_kind, _)
        | Parrayrefs
            ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
              | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
              | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
              | Pgcignorableproductarray_ref _ ),
              (Punboxed_int_index _ as index_kind),
              _ ) ),
        [arg1; arg2] ) ->
      indexing_primitive index_kind "caml_array_get" [arg1; arg2]
    | ( Parrayrefs
          ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
            Ptagged_int_index,
            _ ),
        [arg1; arg2] ) ->
      ccall "caml_floatarray_get" [arg1; arg2]
    | ( Parrayrefs
          ( ( Punboxedfloatarray_ref Unboxed_float32
            | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
            | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
            Ptagged_int_index,
            _ ),
        [arg1; arg2] ) ->
      ccall "caml_array_get_addr" [arg1; arg2]
    | ( ( Parraysets (Pgenarray_set _, index_kind)
        | Parraysets
            ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
              | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
              | Punboxedintarray_set _ | Pgcscannableproductarray_set _
              | Pgcignorableproductarray_set _ ),
              (Punboxed_int_index _ as index_kind) ) ),
        [arg1; arg2; arg3] ) ->
      indexing_primitive index_kind "caml_array_set" [arg1; arg2; arg3]
    | ( Parraysets
          ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
            Ptagged_int_index ),
        [arg1; arg2; arg3] ) ->
      ccall "caml_floatarray_set" [arg1; arg2; arg3]
    | ( Parraysets
          ( ( Punboxedfloatarray_set Unboxed_float32
            | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
            | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
            Ptagged_int_index ),
        [arg1; arg2; arg3] ) ->
      ccall "caml_array_set_addr" [arg1; arg2; arg3]
    | Parrayrefu (Pgenarray_ref _, index_kind, _), [arg1; arg2]
    | ( Parrayrefu
          ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
            | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
            | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
            | Pgcignorableproductarray_ref _ ),
            (Punboxed_int_index _ as index_kind),
            _ ),
        [arg1; arg2] ) ->
      indexing_primitive index_kind "caml_array_unsafe_get" [arg1; arg2]
    | ( Parrayrefu
          ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
            Ptagged_int_index,
            _ ),
        [arg1; arg2] ) ->
      ccall "caml_floatarray_unsafe_get" [arg1; arg2]
    | ( Parrayrefu
          ( ( Punboxedfloatarray_ref Unboxed_float32
            | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
            | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
            Ptagged_int_index,
            _ ),
        [arg1; arg2] ) ->
      binary Kgetvectitem arg1 arg2
    | Parraysetu (Pgenarray_set _, index_kind), [arg1; arg2; arg3]
    | ( Parraysetu
          ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
            | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
            | Punboxedintarray_set _ | Pgcscannableproductarray_set _
            | Pgcignorableproductarray_set _ ),
            (Punboxed_int_index _ as index_kind) ),
        [arg1; arg2; arg3] ) ->
      indexing_primitive index_kind "caml_array_unsafe_set" [arg1; arg2; arg3]
    | ( Parraysetu
          ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
            Ptagged_int_index ),
        [arg1; arg2; arg3] ) ->
      ccall "caml_floatarray_unsafe_set" [arg1; arg2; arg3]
    | ( Parraysetu
          ( ( Punboxedfloatarray_set Unboxed_float32
            | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
            | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
            Ptagged_int_index ),
        [arg1; arg2; arg3] ) ->
      Ksetvectitem
    | Parrayrefs (Punboxedvectorarray_ref _, _, _)
    | Parraysets (Punboxedvectorarray_set _, _)
    | Parrayrefu (Punboxedvectorarray_ref _, _, _)
    | Parraysetu (Punboxedvectorarray_set _, _) ->
      Misc.fatal_error "SIMD is not supported in bytecode mode."
    | Pctconst c, [arg] ->
      let const_name =
        match c with
        | Big_endian -> "big_endian"
        | Word_size -> "word_size"
        | Int_size -> "int_size"
        | Max_wosize -> "max_wosize"
        | Ostype_unix -> "ostype_unix"
        | Ostype_win32 -> "ostype_win32"
        | Ostype_cygwin -> "ostype_cygwin"
        | Backend_type -> "backend_type"
        | Runtime5 -> "runtime5"
      in
      ccall (Printf.sprintf "caml_sys_const_%s" const_name) [arg]
    | Pisint _, [arg] -> unary Isint arg
    | Pisout, [arg1; arg2] -> binary Isout arg1 arg2
    | Pbintofint (bi, _), [arg] -> comp_bint_primitive bi "of_int" [arg]
    | Pintofbint bi, [arg] -> comp_bint_primitive bi "to_int" [arg]
    | Pcvtbint (src, dst, _), [arg] -> (
      match src, dst with
      | Boxed_int32, Boxed_nativeint -> ccall "caml_nativeint_of_int32" [arg]
      | Boxed_nativeint, Boxed_int32 -> ccall "caml_nativeint_to_int32" [arg]
      | Boxed_int32, Boxed_int64 -> ccall "caml_int64_of_int32" [arg]
      | Boxed_int64, Boxed_int32 -> ccall "caml_int64_to_int32" [arg]
      | Boxed_nativeint, Boxed_int64 -> ccall "caml_int64_of_nativeint" [arg]
      | Boxed_int64, Boxed_nativeint -> ccall "caml_int64_to_nativeint" [arg]
      | (Boxed_int32 | Boxed_int64 | Boxed_nativeint), _ ->
        comp_expr arg ~tailcall)
    | Pnegbint (bi, _), [_] -> comp_bint_primitive bi "neg" args
    | Paddbint (bi, _), [_; _] -> comp_bint_primitive bi "add" args
    | Psubbint (bi, _), [_; _] -> comp_bint_primitive bi "sub" args
    | Pmulbint (bi, _), [_; _] -> comp_bint_primitive bi "mul" args
    | Pdivbint { size = bi }, [_; _] -> comp_bint_primitive bi "div" args
    | Pmodbint { size = bi }, [_; _] -> comp_bint_primitive bi "mod" args
    | Pandbint (bi, _), [_; _] -> comp_bint_primitive bi "and" args
    | Porbint (bi, _), [_; _] -> comp_bint_primitive bi "or" args
    | Pxorbint (bi, _), [_; _] -> comp_bint_primitive bi "xor" args
    | Plslbint (bi, _), [_; _] -> comp_bint_primitive bi "shift_left" args
    | Plsrbint (bi, _), [_; _] ->
      comp_bint_primitive bi "shift_right_unsigned" args
    | Pasrbint (bi, _), [_; _] -> comp_bint_primitive bi "shift_right" args
    | (Pbintcomp (_, Ceq) | Punboxed_int_comp (_, Ceq)), [_; _] ->
      ccall "caml_equal" args
    | (Pbintcomp (_, Cne) | Punboxed_int_comp (_, Cne)), [_; _] ->
      ccall "caml_notequal" args
    | Pbintcomp (_, Clt) | Punboxed_int_comp (_, Clt), [_; _] ->
      ccall "caml_lessthan" args
    | Pbintcomp (_, Cgt) | Punboxed_int_comp (_, Cgt), [_; _] ->
      ccall "caml_greaterthan" args
    | Pbintcomp (_, Cle) | Punboxed_int_comp (_, Cle), [_; _] ->
      ccall "caml_lessequal" args
    | Pbintcomp (_, Cge) | Punboxed_int_comp (_, Cge), [_; _] ->
      ccall "caml_greaterequal" args
    | Pbigarrayref (_, n, Pbigarray_float32_t, _), args
      when List.length args = n + 1 ->
      ccall ("caml_ba_float32_get_" ^ Int.to_string n) args
    | Pbigarrayset (_, n, Pbigarray_float32_t, _) when List.length args = n + 2
      ->
      ccall ("caml_ba_float32_set_" ^ Int.to_string n) args
    | Pbigarrayref (_, n, _, _) when List.length args = n + 1 ->
      ccall ("caml_ba_get_" ^ Int.to_string n) args
    | Pbigarrayset (_, n, _, _) when List.length args = n + 2 ->
      ccall ("caml_ba_set_" ^ Int.to_string n) args
    | Pbigarraydim n, [arg] -> ccall ("caml_ba_dim_" ^ Int.to_string n) arg
    | Pbigstring_load_16 { unsafe = _; index_kind }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_ba_uint8_get16" [arg1; arg2]
    | Pbigstring_load_32 { unsafe = _; mode = _; index_kind }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_ba_uint8_get32" [arg1; arg2]
    | Pbigstring_load_f32 { unsafe = _; mode = _; index_kind }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_ba_uint8_getf32" [arg1; arg2]
    | Pbigstring_load_64 { unsafe = _; mode = _; index_kind }, [arg1; arg2] ->
      indexing_primitive index_kind "caml_ba_uint8_get64" [arg1; arg2]
    | Pbigstring_set_16 { unsafe = _; index_kind }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_ba_uint8_set16" [arg1; arg2; arg3]
    | Pbigstring_set_32 { unsafe = _; index_kind }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_ba_uint8_set32" [arg1; arg2; arg3]
    | Pbigstring_set_f32 { unsafe = _; index_kind }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_ba_uint8_setf32" [arg1; arg2; arg3]
    | Pbigstring_set_64 { unsafe = _; index_kind }, [arg1; arg2; arg3] ->
      indexing_primitive index_kind "caml_ba_uint8_set64" [arg1; arg2; arg3]
    | Pbswap16, [arg] -> ccall "caml_bswap16" [arg]
    | Pbbswap (bi, _), [arg] -> comp_bint_primitive bi "bswap" [arg]
    | Pint_as_pointer _ -> Kccall ("caml_int_as_pointer", 1)
    | Pbytes_to_string -> Kccall ("caml_string_of_bytes", 1)
    | Pbytes_of_string -> Kccall ("caml_bytes_of_string", 1)
    | Parray_to_iarray -> Kccall ("caml_iarray_of_array", 1)
    | Parray_of_iarray -> Kccall ("caml_array_of_iarray", 1)
    | Pget_header _ -> Kccall ("caml_get_header", 1)
    | Pobj_dup -> Kccall ("caml_obj_dup", 1)
    | Patomic_load _ -> Kccall ("caml_atomic_load", 1)
    | Patomic_set _ | Patomic_exchange _ -> Kccall ("caml_atomic_exchange", 2)
    | Patomic_compare_exchange _ -> Kccall ("caml_atomic_compare_exchange", 3)
    | Patomic_compare_set _ -> Kccall ("caml_atomic_cas", 3)
    | Patomic_fetch_add -> Kccall ("caml_atomic_fetch_add", 2)
    | Patomic_add -> Kccall ("caml_atomic_add", 2)
    | Patomic_sub -> Kccall ("caml_atomic_sub", 2)
    | Patomic_land -> Kccall ("caml_atomic_land", 2)
    | Patomic_lor -> Kccall ("caml_atomic_lor", 2)
    | Patomic_lxor -> Kccall ("caml_atomic_lxor", 2)
    | Pdls_get -> Kccall ("caml_domain_dls_get", 1)
    | Ppoll -> Kccall ("caml_process_pending_actions_with_root", 1)
    | Pisnull -> Kccall ("caml_is_null", 1)
    | Pstring_load_128 _ | Pbytes_load_128 _ | Pbytes_set_128 _
    | Pbigstring_load_128 _ | Pbigstring_set_128 _ | Pfloatarray_load_128 _
    | Pfloat_array_load_128 _ | Pint_array_load_128 _
    | Punboxed_float_array_load_128 _ | Punboxed_float32_array_load_128 _
    | Punboxed_int32_array_load_128 _ | Punboxed_int64_array_load_128 _
    | Punboxed_nativeint_array_load_128 _ | Pfloatarray_set_128 _
    | Pfloat_array_set_128 _ | Pint_array_set_128 _
    | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
    | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
    | Punboxed_nativeint_array_set_128 _ | Pbox_vector _ | Punbox_vector _ ->
      Misc.fatal_error "SIMD is not supported in bytecode mode."
    | Preinterpret_tagged_int63_as_unboxed_int64 ->
      if not (Target_system.is_64_bit ())
      then
        Misc.fatal_error
          "Preinterpret_tagged_int63_as_unboxed_int64 can only be used on \
           64-bit targets";
      Kccall ("caml_reinterpret_tagged_int63_as_unboxed_int64", 1)
    | Preinterpret_unboxed_int64_as_tagged_int63 ->
      if not (Target_system.is_64_bit ())
      then
        Misc.fatal_error
          "Preinterpret_unboxed_int64_as_tagged_int63 can only be used on \
           64-bit targets";
      Kccall ("caml_reinterpret_unboxed_int64_as_tagged_int63", 1)
    | Pmakearray_dynamic (kind, locality, With_initializer) -> (
      if List.compare_length_with args 2 <> 0
      then
        Misc.fatal_error
          "Bytegen.comp_primitive: Pmakearray_dynamic takes two arguments for \
           [With_initializer]";
      (* CR layouts v4.0: This is "wrong" for unboxed types. It should construct
         blocks that can't be marshalled. We've decided to ignore that problem in
         the short term, as it's unlikely to cause issues - see the internal arrays
         epic for out plan to deal with it. *)
      (match kind with
      | Punboxedvectorarray _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray | Pintarray | Paddrarray | Punboxedintarray _ | Pfloatarray
      | Punboxedfloatarray _ | Pgcscannableproductarray _
      | Pgcignorableproductarray _ ->
        ());
      match locality with
      | Alloc_heap -> Kccall ("caml_make_vect", 2)
      | Alloc_local -> Kccall ("caml_make_local_vect", 2))
    | Parrayblit { src_mutability = _; dst_array_set_kind } ->
      (match dst_array_set_kind with
      | Punboxedvectorarray_set _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray_set _ | Pintarray_set | Paddrarray_set _
      | Punboxedintarray_set _ | Pfloatarray_set | Punboxedfloatarray_set _
      | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ->
        ());
      Kccall ("caml_array_blit", 5)
    | Pmakearray_dynamic (_, _, Uninitialized) ->
      Misc.fatal_error
        "Pmakearray_dynamic Uninitialized should have been translated to \
         Pmakearray_dynamic Initialized earlier on"
    (* The cases below are handled in [comp_expr] before the [comp_primitive] call
       (in the order in which they appear below),
       so they should never be reached in this function. *)
    | Prunstack | Presume | Preperform | Pignore | Popaque _ | Pobj_magic _
    | Pnot | Psequand | Psequor | Praise _ | Pmakearray _ | Pduparray _
    | Pfloatcomp (_, _)
    | Punboxed_float_comp (_, _)
    | Pmakeblock _ | Pmake_unboxed_product _ | Pmakefloatblock _
    | Pmakeufloatblock _ | Pmakemixedblock _ | Pprobe_is_enabled _
    | Punbox_float _
    | Pbox_float (_, _)
    | Punbox_int _ | Pbox_int _ ->
      Misc.fatal_error "Bytegen.comp_primitive"
    | Ppeek _ | Ppoke _ ->
      Misc.fatal_error
        "Bytegen.comp_primitive: Ppeek/Ppoke not supported in bytecode")

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args stack_info env argl sz cont =
  comp_expr_list stack_info env (List.rev argl) sz cont

and comp_expr_list stack_info env exprl sz cont =
  match exprl with
  | [] -> cont
  | [exp] -> comp_expr stack_info env exp sz cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kpush :: comp_expr_list stack_info env rem (sz + 1) cont)

and comp_exit_args stack_info env argl sz pos cont =
  comp_expr_list_assign stack_info env (List.rev argl) sz pos cont

and comp_expr_list_assign stack_info env exprl sz pos cont =
  match exprl with
  | [] -> cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kassign (sz - pos)
      :: comp_expr_list_assign stack_info env rem sz (pos - 1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test stack_info env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit
    then
      let lbl_end, cont1 = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr stack_info env ifso sz cont1
    else
      match code_as_jump stack_info ifso sz with
      | Some label ->
        let cont = comp_expr stack_info env ifnot sz cont in
        Kbranchif label :: cont
      | None -> (
        match code_as_jump stack_info ifnot sz with
        | Some label ->
          let cont = comp_expr stack_info env ifso sz cont in
          Kbranchifnot label :: cont
        | None ->
          let branch_end, cont1 = make_branch cont in
          let lbl_not, cont2 =
            label_code (comp_expr stack_info env ifnot sz cont1)
          in
          Kbranchifnot lbl_not
          :: comp_expr stack_info env ifso sz (branch_end :: cont2))
  in
  comp_expr stack_info env cond sz cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  let stack_info = create_stack_info () in
  let code = comp_expr stack_info env exp sz cont in
  let used_safe = !(stack_info.max_stack_used) + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold
  then
    Kconst (Const_base (Const_int used_safe))
    :: Kccall ("caml_ensure_stack_capacity", 1)
    :: code
  else code

(**** Compilation of functions ****)

let comp_function tc cont =
  let arity = List.length tc.params in
  let ce_stack, _last_pos =
    add_positions Ident.empty Fun.id ~pos:arity ~delta:(-1) tc.params
  in
  let env =
    { ce_stack;
      ce_closure = In_closure { entries = tc.entries; env_pos = 3 * tc.rec_pos }
    }
  in
  let cont = comp_block env tc.body arity (Kreturn arity :: cont) in
  if arity > 1
  then Krestart :: Klabel tc.label :: Kgrab (arity - 1) :: cont
  else Klabel tc.label :: cont

let comp_remainder cont =
  let c = ref cont in
  (try
     while true do
       c := comp_function (Stack.pop functions_to_compile) !c
     done
   with Stack.Empty -> ());
  !c

(**** Compilation of a lambda phrase ****)

let reset () =
  label_counter := 0;
  compunit_name := Compilation_unit.dummy;
  Stack.clear functions_to_compile

let compile_gen ?modulename ~init_stack expr =
  reset ();
  (match modulename with Some name -> compunit_name := name | None -> ());
  Fun.protect ~finally:reset (fun () ->
      let init_code = comp_block empty_env expr init_stack [] in
      if Stack.length functions_to_compile > 0
      then
        let lbl_init = new_label () in
        Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code), false
      else init_code, true)

let compile_implementation modulename expr =
  fst (compile_gen ~modulename ~init_stack:0 expr)

let compile_phrase expr = compile_gen ~init_stack:1 expr
