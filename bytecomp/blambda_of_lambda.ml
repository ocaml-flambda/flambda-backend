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

open Blambda

let is_nontail : Lambda.region_close -> bool = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

module Storer = Switch.Store (struct
  type t = Lambda.lambda

  type key = t

  let compare_key = Stdlib.compare

  let make_key = Lambda.make_key
end)

let is_immed n = Instruct.immed_min <= n && n <= Instruct.immed_max

let comp_integer_comparison : Lambda.integer_comparison -> comparison = function
  | Ceq -> Eq
  | Cne -> Neq
  | Clt -> Ltint
  | Cgt -> Gtint
  | Cle -> Leint
  | Cge -> Geint

let rec comp_expr (exp : Lambda.lambda) : Blambda.blambda =
  let comp_fun ({ params; body; loc = _ } as lfunction : Lambda.lfunction) :
      Blambda.bfunction =
    (* assume kind = Curried *)
    { params = List.map (fun (p : Lambda.lparam) -> p.name) params;
      body = comp_expr body;
      free_variables = Lambda.free_variables (Lfunction lfunction)
    }
  in
  let comp_rec_binding ({ id; def } : Lambda.rec_binding) : Blambda.rec_binding
      =
    { id; def = comp_fun def }
  in
  let[@inline] comp_arg arg = comp_expr arg in
  match (exp : Lambda.lambda) with
  | Lvar id | Lmutvar id -> Var id
  | Lconst cst -> Const cst
  | Lapply { ap_func; ap_args; ap_region_close } ->
    Apply
      { func = comp_arg ap_func;
        args = List.map comp_arg ap_args;
        nontail = is_nontail ap_region_close
      }
  | Lsend (kind, met, obj, args, rc, _, _, _) ->
    Send
      { method_kind =
          (match (kind : Lambda.meth_kind) with
          | Self -> Self
          | Public -> Public
          | Cached -> assert false);
        met = comp_arg met;
        obj = comp_arg obj;
        args = List.map comp_arg args;
        nontail = is_nontail rc
      }
  | Lfunction f -> Pseudo_event (Function (comp_fun f), f.loc)
  | Llet (_, _k, id, arg, body) | Lmutlet (_k, id, arg, body) ->
    Let { id; arg = comp_arg arg; body = comp_expr body }
  | Lletrec (decl, body) ->
    Letrec
      { decls = List.map comp_rec_binding decl;
        body = comp_expr body;
        free_variables_of_decls =
          Lambda.free_variables (Lletrec (decl, Lambda.lambda_unit))
      }
  | Lstaticcatch (body, (static_label, args), handler, _, _) ->
    Staticcatch
      { body = comp_arg body;
        id = static_label;
        args = List.map fst args;
        handler = comp_expr handler
      }
  | Lstaticraise (static_label, args) ->
    Staticraise (static_label, List.map comp_arg args)
  | Ltrywith (body, param, handler, _kind) ->
    Trywith { body = comp_arg body; param; handler = comp_expr handler }
  | Lifthenelse (cond, ifso, ifnot, _kind) ->
    Ifthenelse
      { cond = comp_arg cond; ifso = comp_expr ifso; ifnot = comp_expr ifnot }
  | Lsequence (exp1, exp2) -> Sequence (comp_arg exp1, comp_expr exp2)
  | Lwhile { wh_cond; wh_body } ->
    While { cond = comp_arg wh_cond; body = comp_arg wh_body }
  | Lfor { for_id; for_from; for_to; for_dir; for_body } ->
    For
      { id = for_id;
        from = comp_arg for_from;
        to_ = comp_arg for_to;
        dir = for_dir;
        body = comp_arg for_body
      }
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        _loc,
        _kind ) ->
    (* Build indirection vectors *)
    let store = Storer.mk_store () in
    let fail =
      match sw_failaction with
      | Some fail -> store.act_store () fail
      | None ->
        (* if there is no failaction (i.e., default action) either
           1. all of the potential cases have been covered, or
           2. Some cases have been refuted.

           In both cases, we arbitrarily pick the first case as the value to put
        *)
        0
    in
    let compile_cases src ~size =
      let dst = Array.make size fail in
      ListLabels.iter src ~f:(fun (n, case) ->
          dst.(n) <- store.act_store () case);
      dst
    in
    (* Compile and label actions *)
    let arg = comp_arg arg in
    let const_cases = compile_cases sw_consts ~size:sw_numconsts in
    let block_cases = compile_cases sw_blocks ~size:sw_numblocks in
    let cases = Array.map comp_expr (store.act_get ()) in
    Switch { arg; const_cases; block_cases; cases }
  | Lstringswitch (arg, sw, d, loc, kind) ->
    comp_expr (Matching.expand_stringswitch loc kind arg sw d)
  | Lassign (id, expr) -> Assign (id, comp_arg expr)
  | Levent (lam, lev) -> Event (comp_expr lam, lev)
  | Lifused (_, exp) | Lregion (exp, _) | Lexclave exp -> comp_expr exp
  | Lprim (primitive, args, loc) -> (
    let simd_is_not_supported () =
      Misc.fatal_error "SIMD is not supported in bytecode mode."
    in
    let wrong_arity ~expected =
      Misc.fatal_errorf "Blambda_of_lambda: %a takes exactly %d %s"
        Printlambda.primitive primitive expected
        (if expected = 1 then "argument" else "arguments")
    in
    let check_arity ~arity =
      match List.compare_length_with args arity with
      | 0 -> List.map comp_arg args
      | _ -> wrong_arity ~expected:arity
    in
    let context_switch c ~arity =
      Blambda.Context_switch (c, check_arity ~arity)
    in
    let pseudo_event t = Blambda.Pseudo_event (t, loc) in
    let variadic primitive = Blambda.Prim (primitive, List.map comp_arg args) in
    let n_ary primitive ~arity = Blambda.Prim (primitive, check_arity ~arity) in
    let nullary = n_ary ~arity:0 in
    let unary = n_ary ~arity:1 in
    let binary = n_ary ~arity:2 in
    let ternary = n_ary ~arity:3 in
    let boolnot arg = Blambda.Prim (Boolnot, [arg]) in
    let comp_bint_primitive bi suff : Blambda.primitive =
      let pref =
        match (bi : Primitive.boxed_integer) with
        | Boxed_nativeint -> "caml_nativeint_"
        | Boxed_int32 -> "caml_int32_"
        | Boxed_int64 -> "caml_int64_"
      in
      Ccall (pref ^ suff)
    in
    let indexing_primitive (index_kind : Lambda.array_index_kind) prefix :
        Blambda.primitive =
      let suffix =
        match index_kind with
        | Ptagged_int_index -> ""
        | Punboxed_int_index Unboxed_int64 -> "_indexed_by_int64"
        | Punboxed_int_index Unboxed_int32 -> "_indexed_by_int32"
        | Punboxed_int_index Unboxed_nativeint -> "_indexed_by_nativeint"
      in
      Ccall (prefix ^ suffix)
    in
    match primitive with
    | Popaque _ | Pobj_magic _
    | Pbox_float ((Boxed_float64 | Boxed_float32), _)
    | Punbox_float (Boxed_float64 | Boxed_float32)
    | Pbox_int _ | Punbox_int _ -> (
      match args with
      | [arg] ->
        (* in bytecode we only deal with boxed+tagged floats and ints *)
        comp_expr arg
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pignore -> (
      match args with
      | [arg] ->
        (* in bytecode we only deal with boxed+tagged floats and ints *)
        Sequence (comp_arg arg, Const (Const_base (Const_int 0)))
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pnot -> unary Boolnot
    | Psequand -> (
      match args with
      | [x; y] -> Sequand (comp_arg x, comp_expr y)
      | _ -> wrong_arity ~expected:2)
    | Psequor -> (
      match args with
      | [x; y] -> Sequor (comp_arg x, comp_expr y)
      | _ -> wrong_arity ~expected:2)
    | Praise k -> unary (Raise k)
    | Paddint -> (
      match args with
      | [arg; Lconst (Const_base (Const_int n))] when is_immed n ->
        Prim (Offsetint n, [comp_arg arg])
      | _ -> binary Addint)
    | Psubint -> (
      match args with
      | [arg; Lconst (Const_base (Const_int n))] when is_immed (-n) ->
        Prim (Offsetint (-n), [comp_arg arg])
      | _ -> binary Subint)
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
    | Poffsetint n ->
      assert (is_immed n);
      unary (Offsetint n)
    | Pmakefloatblock _ | Pmakeufloatblock _ ->
      (* In bytecode, float# is boxed, so we can treat these two primitives the
         same. *)
      pseudo_event (variadic Makefloatblock)
    | Pmakemixedblock (tag, _, shape, _) ->
      (* There is no notion of a mixed block at runtime in bytecode. Further,
         source-level unboxed types are represented as boxed in bytecode, so
         no ceremony is needed to box values before inserting them into
         the (normal, unmixed) block.
      *)
      let total_len = Array.length shape in
      pseudo_event (variadic (Make_faux_mixedblock { total_len; tag }))
    | Pmakearray (kind, _, _) ->
      pseudo_event
        (match kind with
        (* arrays of unboxed types have the same representation
           as the boxed ones on bytecode *)
        | Pintarray | Paddrarray | Punboxedintarray _
        | Punboxedfloatarray Unboxed_float32
        | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
          variadic (Makeblock { tag = 0 })
        | Pfloatarray | Punboxedfloatarray Unboxed_float64 ->
          variadic Makefloatblock
        | Punboxedvectorarray _ -> simd_is_not_supported ()
        | Pgenarray -> (
          let block = variadic (Makeblock { tag = 0 }) in
          match args with
          | [] -> block
          | _ :: _ ->
            (* for the floatarray hack *)
            Prim (Ccall "caml_make_array", [block])))
    | Presume -> context_switch Resume ~arity:4
    | Prunstack -> context_switch Runstack ~arity:3
    | Preperform -> context_switch Reperform ~arity:3
    | Pmakearray_dynamic (kind, locality, Uninitialized) -> (
      (* Use a dummy initializer to implement the "uninitialized" primitive *)
      let init : Lambda.lambda =
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
        | Punboxedvectorarray _ -> simd_is_not_supported ()
        | Pgcignorableproductarray ignorables ->
          let rec convert_ignorable
              (ign : Lambda.ignorable_product_element_kind) : Lambda.lambda =
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
              Lprim
                (Pmakeblock (0, Immutable, None, Lambda.alloc_heap), fields, loc)
          in
          convert_ignorable (Pproduct_ignorable ignorables)
      in
      match args with
      | [len] ->
        comp_expr
          (Lprim
             ( Pmakearray_dynamic (kind, locality, With_initializer),
               [len; init],
               loc )
            : Lambda.lambda)
      | _ -> wrong_arity ~expected:1)
    | Pduparray (kind, mutability) -> (
      match args with
      | [Lprim (Pmakearray (kind', _, m), args, _)] ->
        assert (kind = kind');
        comp_expr (Lambda.Lprim (Pmakearray (kind, mutability, m), args, loc))
      | _ -> unary (Ccall "caml_obj_dup"))
    | Pfloatcomp (Boxed_float64, cmp)
    | Punboxed_float_comp (Unboxed_float64, cmp) -> (
      match cmp with
      | CFeq -> binary (Ccall "caml_eq_float")
      | CFneq -> binary (Ccall "caml_neq_float")
      | CFlt -> binary (Ccall "caml_lt_float")
      | CFnlt -> binary (Ccall "caml_lt_float") |> boolnot
      | CFgt -> binary (Ccall "caml_gt_float")
      | CFngt -> binary (Ccall "caml_gt_float") |> boolnot
      | CFle -> binary (Ccall "caml_le_float")
      | CFnle -> binary (Ccall "caml_le_float") |> boolnot
      | CFge -> binary (Ccall "caml_ge_float")
      | CFnge -> binary (Ccall "caml_ge_float") |> boolnot)
    | Pfloatcomp (Boxed_float32, cmp)
    | Punboxed_float_comp (Unboxed_float32, cmp) -> (
      match cmp with
      | CFeq -> binary (Ccall "caml_eq_float32")
      | CFneq -> binary (Ccall "caml_neq_float32")
      | CFlt -> binary (Ccall "caml_lt_float32")
      | CFnlt -> binary (Ccall "caml_lt_float32") |> boolnot
      | CFgt -> binary (Ccall "caml_gt_float32")
      | CFngt -> binary (Ccall "caml_gt_float32") |> boolnot
      | CFle -> binary (Ccall "caml_le_float32")
      | CFnle -> binary (Ccall "caml_le_float32") |> boolnot
      | CFge -> binary (Ccall "caml_ge_float32")
      | CFnge -> binary (Ccall "caml_ge_float32") |> boolnot)
    | Pmakeblock (tag, _mut, _, _) ->
      pseudo_event (variadic (Makeblock { tag }))
    | Pmake_unboxed_product _ -> pseudo_event (variadic (Makeblock { tag = 0 }))
    | Pgetglobal cu -> nullary (Getglobal cu)
    | Psetglobal cu -> unary (Setglobal cu)
    | Pgetpredef id -> nullary (Getpredef id)
    | Pintcomp cmp -> (
      (* put constant first for enabling further optimization (cf. emitcode.ml)  *)
      match args with
      | [arg1; (Lconst _ as arg2)] ->
        let cmp = Lambda.swap_integer_comparison cmp in
        Prim
          (Intcomp (comp_integer_comparison cmp), [comp_arg arg2; comp_arg arg1])
      | _ -> binary (Intcomp (comp_integer_comparison cmp)))
    | Pcompare_ints -> binary (Ccall "caml_int_compare")
    | Pcompare_floats Boxed_float64 -> binary (Ccall "caml_float_compare")
    | Pcompare_floats Boxed_float32 -> binary (Ccall "caml_float32_compare")
    | Pcompare_bints bi -> binary (comp_bint_primitive bi "compare")
    | Pfield (n, _, _) | Punboxed_product_field (n, _) -> unary (Getfield n)
    | Parray_element_size_in_bytes _array_kind -> (
      match args with
      | [arg] ->
        let word_size : Lambda.lambda =
          Lprim (Pctconst Word_size, [Lambda.lambda_unit], loc)
        in
        let element_size : Lambda.lambda =
          Lprim (Plsrint, [word_size; Lconst (Const_base (Const_int 3))], loc)
        in
        comp_expr (Lambda.Lsequence (arg, element_size))
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pfield_computed _sem -> binary Getvectitem
    | Psetfield (n, _ptr, _init) -> binary (Setfield n)
    | Psetfield_computed (_ptr, _init) -> ternary Setvectitem
    (* In bytecode, float#s are boxed.  So, we can use the existing float
       instructions for the ufloat primitives. *)
    | Pfloatfield (n, _, _) | Pufloatfield (n, _) ->
      pseudo_event (unary (Getfloatfield n))
    | Psetfloatfield (n, _) | Psetufloatfield (n, _) -> binary (Setfloatfield n)
    | Pmixedfield (n, _, _) ->
      (* CR layouts: This will need reworking if we ever want bytecode
         to unbox fields that are written with unboxed types in the source
         language. *)
      (* Note, non-value mixed fields are always boxed in bytecode; they
         aren't stored flat like they are in native code.
      *)
      unary (Getfield n)
    | Psetmixedfield (n, _, _) ->
      (* See the comment in the [Pmixedfield] case. *)
      binary (Setfield n)
    | Pduprecord _ -> unary (Ccall "caml_obj_dup")
    | Pccall p -> n_ary (Ccall p.prim_name) ~arity:p.prim_arity
    | Pperform -> context_switch Perform ~arity:1
    | Poffsetref n -> unary (Offsetref n)
    | Pfloatoffloat32 _ -> unary (Ccall "caml_float_of_float32")
    | Pfloat32offloat _ -> unary (Ccall "caml_float32_of_float")
    | Pintoffloat Boxed_float64 -> unary (Ccall "caml_int_of_float")
    | Pfloatofint (Boxed_float64, _) -> unary (Ccall "caml_float_of_int")
    | Pnegfloat (Boxed_float64, _) -> unary (Ccall "caml_neg_float")
    | Pabsfloat (Boxed_float64, _) -> unary (Ccall "caml_abs_float")
    | Paddfloat (Boxed_float64, _) -> binary (Ccall "caml_add_float")
    | Psubfloat (Boxed_float64, _) -> binary (Ccall "caml_sub_float")
    | Pmulfloat (Boxed_float64, _) -> binary (Ccall "caml_mul_float")
    | Pdivfloat (Boxed_float64, _) -> binary (Ccall "caml_div_float")
    | Pintoffloat Boxed_float32 -> unary (Ccall "caml_int_of_float32")
    | Pfloatofint (Boxed_float32, _) -> unary (Ccall "caml_float32_of_int")
    | Pnegfloat (Boxed_float32, _) -> unary (Ccall "caml_neg_float32")
    | Pabsfloat (Boxed_float32, _) -> unary (Ccall "caml_abs_float32")
    | Paddfloat (Boxed_float32, _) -> binary (Ccall "caml_add_float32")
    | Psubfloat (Boxed_float32, _) -> binary (Ccall "caml_sub_float32")
    | Pmulfloat (Boxed_float32, _) -> binary (Ccall "caml_mul_float32")
    | Pdivfloat (Boxed_float32, _) -> binary (Ccall "caml_div_float32")
    | Pstringlength -> unary (Ccall "caml_ml_string_length")
    | Pbyteslength -> unary (Ccall "caml_ml_bytes_length")
    | Pstringrefs -> binary (Ccall "caml_string_get")
    | Pbytesrefs -> binary (Ccall "caml_bytes_get")
    | Pbytessets -> ternary (Ccall "caml_bytes_set")
    | Pstringrefu | Pbytesrefu -> binary Getbyteschar
    | Pbytessetu -> ternary Setbyteschar
    | Pstring_load_16 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get16")
    | Pstring_load_32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get32")
    | Pstring_load_f32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_getf32")
    | Pstring_load_64 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get64")
    | Pbytes_set_16 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set16")
    | Pbytes_set_32 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set32")
    | Pbytes_set_f32 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_setf32")
    | Pbytes_set_64 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set64")
    | Pbytes_load_16 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get16")
    | Pbytes_load_32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get32")
    | Pbytes_load_f32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_getf32")
    | Pbytes_load_64 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get64")
    | Parraylength _ -> unary Vectlength
    (* In bytecode, nothing is ever actually stack-allocated, so we ignore the
       array modes (allocation for [Parrayref{s,u}], modification for
       [Parrayset{s,u}]). *)
    | Parrayrefs (Pgenarray_ref _, index_kind, _)
    | Parrayrefs
        ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
          | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
          | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
          | Pgcignorableproductarray_ref _ ),
          (Punboxed_int_index _ as index_kind),
          _ ) ->
      binary (indexing_primitive index_kind "caml_array_get")
    | Parrayrefs
        ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_floatarray_get")
    | Parrayrefs
        ( ( Punboxedfloatarray_ref Unboxed_float32
          | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
          | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_array_get_addr")
    | Parraysets (Pgenarray_set _, index_kind)
    | Parraysets
        ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
          | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
          | Punboxedintarray_set _ | Pgcscannableproductarray_set _
          | Pgcignorableproductarray_set _ ),
          (Punboxed_int_index _ as index_kind) ) ->
      ternary (indexing_primitive index_kind "caml_array_set")
    | Parraysets
        ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_floatarray_set")
    | Parraysets
        ( ( Punboxedfloatarray_set Unboxed_float32
          | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
          | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_array_set_addr")
    | Parrayrefu (Pgenarray_ref _, index_kind, _)
    | Parrayrefu
        ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
          | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
          | Punboxedintarray_ref _ | Pgcscannableproductarray_ref _
          | Pgcignorableproductarray_ref _ ),
          (Punboxed_int_index _ as index_kind),
          _ ) ->
      binary (indexing_primitive index_kind "caml_array_unsafe_get")
    | Parrayrefu
        ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_floatarray_unsafe_get")
    | Parrayrefu
        ( ( Punboxedfloatarray_ref Unboxed_float32
          | Punboxedintarray_ref _ | Paddrarray_ref | Pintarray_ref
          | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
          Ptagged_int_index,
          _ ) ->
      binary Getvectitem
    | Parraysetu (Pgenarray_set _, index_kind)
    | Parraysetu
        ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
          | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
          | Punboxedintarray_set _ | Pgcscannableproductarray_set _
          | Pgcignorableproductarray_set _ ),
          (Punboxed_int_index _ as index_kind) ) ->
      ternary (indexing_primitive index_kind "caml_array_unsafe_set")
    | Parraysetu
        ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_floatarray_unsafe_set")
    | Parraysetu
        ( ( Punboxedfloatarray_set Unboxed_float32
          | Punboxedintarray_set _ | Paddrarray_set _ | Pintarray_set
          | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
          Ptagged_int_index ) ->
      ternary Setvectitem
    | Parrayrefs (Punboxedvectorarray_ref _, _, _)
    | Parraysets (Punboxedvectorarray_set _, _)
    | Parrayrefu (Punboxedvectorarray_ref _, _, _)
    | Parraysetu (Punboxedvectorarray_set _, _) ->
      simd_is_not_supported ()
    | Pctconst c ->
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
      unary (Ccall (Printf.sprintf "caml_sys_const_%s" const_name))
    | Pisint _ -> unary Isint
    | Pisout -> binary (Intcomp Ultint)
    | Pbintofint (bi, _) -> unary (comp_bint_primitive bi "of_int")
    | Pintofbint bi -> unary (comp_bint_primitive bi "to_int")
    | Pcvtbint (src, dst, _) -> (
      match src, dst with
      | Boxed_int32, Boxed_nativeint -> unary (Ccall "caml_nativeint_of_int32")
      | Boxed_nativeint, Boxed_int32 -> unary (Ccall "caml_nativeint_to_int32")
      | Boxed_int32, Boxed_int64 -> unary (Ccall "caml_int64_of_int32")
      | Boxed_int64, Boxed_int32 -> unary (Ccall "caml_int64_to_int32")
      | Boxed_nativeint, Boxed_int64 -> unary (Ccall "caml_int64_of_nativeint")
      | Boxed_int64, Boxed_nativeint -> unary (Ccall "caml_int64_to_nativeint")
      | (Boxed_int32 | Boxed_int64 | Boxed_nativeint), _ -> (
        match args with
        | [arg] -> comp_expr arg
        | [] | _ :: _ :: _ -> wrong_arity ~expected:1))
    | Pnegbint (bi, _) -> unary (comp_bint_primitive bi "neg")
    | Paddbint (bi, _) -> binary (comp_bint_primitive bi "add")
    | Psubbint (bi, _) -> binary (comp_bint_primitive bi "sub")
    | Pmulbint (bi, _) -> binary (comp_bint_primitive bi "mul")
    | Pdivbint { size = bi } -> binary (comp_bint_primitive bi "div")
    | Pmodbint { size = bi } -> binary (comp_bint_primitive bi "mod")
    | Pandbint (bi, _) -> binary (comp_bint_primitive bi "and")
    | Porbint (bi, _) -> binary (comp_bint_primitive bi "or")
    | Pxorbint (bi, _) -> binary (comp_bint_primitive bi "xor")
    | Plslbint (bi, _) -> binary (comp_bint_primitive bi "shift_left")
    | Plsrbint (bi, _) -> binary (comp_bint_primitive bi "shift_right_unsigned")
    | Pasrbint (bi, _) -> binary (comp_bint_primitive bi "shift_right")
    | Pbintcomp (_, Ceq) | Punboxed_int_comp (_, Ceq) ->
      binary (Ccall "caml_equal")
    | Pbintcomp (_, Cne) | Punboxed_int_comp (_, Cne) ->
      binary (Ccall "caml_notequal")
    | Pbintcomp (_, Clt) | Punboxed_int_comp (_, Clt) ->
      binary (Ccall "caml_lessthan")
    | Pbintcomp (_, Cgt) | Punboxed_int_comp (_, Cgt) ->
      binary (Ccall "caml_greaterthan")
    | Pbintcomp (_, Cle) | Punboxed_int_comp (_, Cle) ->
      binary (Ccall "caml_lessequal")
    | Pbintcomp (_, Cge) | Punboxed_int_comp (_, Cge) ->
      binary (Ccall "caml_greaterequal")
    | Pbigarrayref (_, n, Pbigarray_float32_t, _) ->
      n_ary (Ccall ("caml_ba_float32_get_" ^ Int.to_string n)) ~arity:(n + 1)
    | Pbigarrayset (_, n, Pbigarray_float32_t, _) ->
      n_ary (Ccall ("caml_ba_float32_set_" ^ Int.to_string n)) ~arity:(n + 2)
    | Pbigarrayref (_, n, _, _) ->
      n_ary (Ccall ("caml_ba_get_" ^ Int.to_string n)) ~arity:(n + 1)
    | Pbigarrayset (_, n, _, _) ->
      n_ary (Ccall ("caml_ba_set_" ^ Int.to_string n)) ~arity:(n + 2)
    | Pbigarraydim n -> unary (Ccall ("caml_ba_dim_" ^ Int.to_string n))
    | Pbigstring_load_16 { unsafe = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get16")
    | Pbigstring_load_32 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get32")
    | Pbigstring_load_f32 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_getf32")
    | Pbigstring_load_64 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get64")
    | Pbigstring_set_16 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set16")
    | Pbigstring_set_32 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set32")
    | Pbigstring_set_f32 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_setf32")
    | Pbigstring_set_64 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set64")
    | Pbswap16 -> unary (Ccall "caml_bswap16")
    | Pbbswap (bi, _) -> unary (comp_bint_primitive bi "bswap")
    | Pint_as_pointer _ -> unary (Ccall "caml_int_as_pointer")
    | Pbytes_to_string -> unary (Ccall "caml_string_of_bytes")
    | Pbytes_of_string -> unary (Ccall "caml_bytes_of_string")
    | Parray_to_iarray -> unary (Ccall "caml_iarray_of_array")
    | Parray_of_iarray -> unary (Ccall "caml_array_of_iarray")
    | Pget_header _ -> unary (Ccall "caml_get_header")
    | Pobj_dup -> unary (Ccall "caml_obj_dup")
    | Patomic_load _ -> unary (Ccall "caml_atomic_load")
    | Patomic_set _ | Patomic_exchange _ ->
      binary (Ccall "caml_atomic_exchange")
    | Patomic_compare_exchange _ ->
      ternary (Ccall "caml_atomic_compare_exchange")
    | Patomic_compare_set _ -> ternary (Ccall "caml_atomic_cas")
    | Patomic_fetch_add -> binary (Ccall "caml_atomic_fetch_add")
    | Patomic_add -> binary (Ccall "caml_atomic_add")
    | Patomic_sub -> binary (Ccall "caml_atomic_sub")
    | Patomic_land -> binary (Ccall "caml_atomic_land")
    | Patomic_lor -> binary (Ccall "caml_atomic_lor")
    | Patomic_lxor -> binary (Ccall "caml_atomic_lxor")
    | Pdls_get -> unary (Ccall "caml_domain_dls_get")
    | Ppoll -> unary (Ccall "caml_process_pending_actions_with_root")
    | Pisnull -> unary (Ccall "caml_is_null")
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
      simd_is_not_supported ()
    | Preinterpret_tagged_int63_as_unboxed_int64 ->
      if Target_system.is_64_bit ()
      then unary (Ccall "caml_reinterpret_tagged_int63_as_unboxed_int64")
      else
        Misc.fatal_error
          "Preinterpret_tagged_int63_as_unboxed_int64 can only be used on \
           64-bit targets"
    | Preinterpret_unboxed_int64_as_tagged_int63 ->
      if Target_system.is_64_bit ()
      then unary (Ccall "caml_reinterpret_unboxed_int64_as_tagged_int63")
      else
        Misc.fatal_error
          "Preinterpret_unboxed_int64_as_tagged_int63 can only be used on \
           64-bit targets"
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
      match kind with
      | Punboxedvectorarray _ -> simd_is_not_supported ()
      | Pgenarray | Pintarray | Paddrarray | Punboxedintarray _ | Pfloatarray
      | Punboxedfloatarray _ | Pgcscannableproductarray _
      | Pgcignorableproductarray _ -> (
        match locality with
        | Alloc_heap -> binary (Ccall "caml_make_vect")
        | Alloc_local -> binary (Ccall "caml_make_local_vect")))
    | Parrayblit { src_mutability = _; dst_array_set_kind } ->
      (match dst_array_set_kind with
      | Punboxedvectorarray_set _ -> simd_is_not_supported ()
      | Pgenarray_set _ | Pintarray_set | Paddrarray_set _
      | Punboxedintarray_set _ | Pfloatarray_set | Punboxedfloatarray_set _
      | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ->
        ());
      n_ary (Ccall "caml_array_blit") ~arity:5
    | Pprobe_is_enabled _ | Ppeek _ | Ppoke _ ->
      Misc.fatal_errorf "Blambda_of_lambda: %a is not supported in bytecode"
        Printlambda.primitive primitive
    | Pmakelazyblock Lazy_tag ->
      pseudo_event (variadic (Makeblock { tag = Config.lazy_tag }))
    | Pmakelazyblock Forward_tag ->
      pseudo_event (variadic (Makeblock { tag = Obj.forward_tag })))

let blambda_of_lambda x = comp_expr x
