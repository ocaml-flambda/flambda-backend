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

open Lambda
open Blambda

(**** Compilation of a lambda expression ****)

(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let is_nontail = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

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
  | Lprim (primitive, args, loc) -> (
    let wrong_arity ~expected =
      Misc.fatal_errorf "Blambda_of_lambda.comp_primitive: %a takes %d %s"
        Printlambda.primitive primitive expected
        (if expected = 1 then "argument" else "arguments")
    in
    let variadic primitive = Prim (primitive, List.map comp_arg args, loc) in
    let n_ary primitive ~arity =
      if List.compare_length_with args arity <> 0
      then variadic primitive
      else wrong_arity ~expected:arity
    in
    let nullary = n_ary ~arity:0 in
    let unary = n_ary ~arity:1 in
    let binary = n_ary ~arity:2 in
    let ternary = n_ary ~arity:3 in
    let ccall name ~arity = n_ary ~arity (Ccall name) in
    let boolnot arg = Blambda.Prim (Boolnot, [arg], loc) in
    let comp_bint_primitive bi suff =
      let pref =
        match bi with
        | Boxed_nativeint -> "caml_nativeint_"
        | Boxed_int32 -> "caml_int32_"
        | Boxed_int64 -> "caml_int64_"
      in
      Ccall (pref ^ suff)
    in
    let indexing_primitive (index_kind : Lambda.array_index_kind) prefix =
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
      variadic (Make_faux_mixedblock { total_len; tag })
    | Pmakearray (kind, _, _) -> (
      match kind with
      (* arrays of unboxed types have the same representation
         as the boxed ones on bytecode *)
      | Pintarray | Paddrarray | Punboxedintarray _
      | Punboxedfloatarray Unboxed_float32
      | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
        variadic (Makeblock { tag = 0 })
      | Pfloatarray | Punboxedfloatarray Unboxed_float64 ->
        variadic Makefloatblock
      | Punboxedvectorarray _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray -> (
        match args with
        | [] -> variadic (Makeblock { tag = 0 })
        | _ :: _ ->
          (* for the floatarray hack *)
          Prim (Ccall "caml_make_array", [variadic (Makeblock { tag = 0 })], loc)
        ))
    | Presume -> n_ary (Resume tailcall) ~arity:4
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
        comp_expr
          (Lprim (Pmakearray (kind, mutability, m), args, loc))
          ~tailcall
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
    | Pmakeblock (tag, _mut, _, _) -> variadic (Makeblock { tag })
    | Pmake_unboxed_product _ -> variadic (Makeblock { tag = 0 })
    | Pgetglobal cu -> nullary (Getglobal cu)
    | Psetglobal cu -> unary (Setglobal cu)
    | Pgetpredef id -> nullary (Getpredef id)
    | Pintcomp cmp -> binary (Intcomp cmp)
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
        comp_expr (Lsequence (arg, element_size)) ~tailcall
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pfield_computed _sem -> binary Getvectitem
    | Psetfield (n, _ptr, _init) -> binary (Setfield n)
    | Psetfield_computed (_ptr, _init) -> ternary Setvectitem
    (* In bytecode, float#s are boxed.  So, we can use the existing float
       instructions for the ufloat primitives. *)
    | Pfloatfield (n, _, _) | Pufloatfield (n, _) -> unary (Getfloatfield n)
    | Psetfloatfield (n, _) | Psetufloatfield (n, _) -> binary (Setfloatfield n)
    | Pmixedfield (n, _, _, _) ->
      (* CR layouts: This will need reworking if we ever want bytecode
         to unbox fields that are written with unboxed types in the source
         language. *)
      (* Note, non-value mixed fields are always boxed in bytecode; they
         aren't stored flat like they are in native code.
      *)
      unary (Getfield n)
    | Psetmixedfield (n, _, _shape, _init) ->
      (* See the comment in the [Pmixedfield] case. *)
      binary (Setfield n)
    | Pduprecord _ -> ccall "caml_obj_dup" ~arity:1
    | Pccall p -> ccall p.prim_name ~arity:p.prim_arity
    | Pperform -> unary Perform
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
    | Pdivfloat (Boxed_float64, _) -> binary (Ccall "caml_mul_float")
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
    | Pstringrefu -> binary Getstringchar
    | Pbytesrefu -> binary Getbyteschar
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
      Misc.fatal_error "SIMD is not supported in bytecode mode."
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
    | Pisout -> binary Isout
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
        | [arg] -> comp_expr arg ~tailcall
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
      Misc.fatal_error "SIMD is not supported in bytecode mode."
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
      | Punboxedvectorarray _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray | Pintarray | Paddrarray | Punboxedintarray _ | Pfloatarray
      | Punboxedfloatarray _ | Pgcscannableproductarray _
      | Pgcignorableproductarray _ -> (
        match locality with
        | Alloc_heap -> binary (Ccall "caml_make_vect")
        | Alloc_local -> binary (Ccall "caml_make_local_vect")))
    | Parrayblit { src_mutability = _; dst_array_set_kind } ->
      (match dst_array_set_kind with
      | Punboxedvectorarray_set _ ->
        Misc.fatal_error "SIMD is not supported in bytecode mode."
      | Pgenarray_set _ | Pintarray_set | Paddrarray_set _
      | Punboxedintarray_set _ | Pfloatarray_set | Punboxedfloatarray_set _
      | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ->
        ());
      n_ary (Ccall "caml_array_blit") ~arity:5
    | Pprobe_is_enabled _ | Ppeek _ | Ppoke _ ->
      Misc.fatal_errorf "Blambda_of_lambda: %a is not supported in bytecode"
        Printlambda.primitive primitive)

let blambda_of_lambda x = comp_expr x ~tailcall:Nontail
