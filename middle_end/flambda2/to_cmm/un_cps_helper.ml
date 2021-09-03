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

open Cmm_helpers
module P = Flambda_primitive

let unsupported_32_bits () =
  Misc.fatal_errorf "32 bits is currently unsupported in Flambda."

(* Are we compiling on/for a 32-bit architecture ? *)
let arch32 = Arch.size_int = 4
let arch64 = Arch.size_int = 8

(* Useful shortcut *)
let typ_int64 =
  if arch32 then [| Cmm.Int; Cmm.Int |] else [| Cmm.Int |]

let exttype_of_kind k =
  match (k  : Flambda_kind.t) with
  | Value -> Cmm.XInt
  | Naked_number Naked_float -> Cmm.XFloat
  | Naked_number Naked_int64 -> Cmm.XInt64
  | Naked_number Naked_int32 -> Cmm.XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) ->
    begin match Targetint_32_64.num_bits with
    | Thirty_two -> Cmm.XInt32
    | Sixty_four -> Cmm.XInt64
    end
  | Fabricated -> Misc.fatal_error "[Fabricated] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

(* Void *)

let void = Cmm.Ctuple []
let unit ~dbg = Cmm.Cconst_int (1, dbg)

(* Data items *)

let cint i =
  Cmm.Cint i

let cfloat f =
  Cmm.Cdouble f

let symbol_address s =
  Cmm.Csymbol_address s

let define_symbol ~global s =
  if global then
    [Cmm.Cglobal_symbol s; Cmm.Cdefine_symbol s]
  else
    [Cmm.Cdefine_symbol s]

(* Constructors for constants *)

let var v = Cmm.Cvar v

let symbol ?(dbg=Debuginfo.none) s =
  Cmm.Cconst_symbol (s, dbg)

let float ?(dbg=Debuginfo.none) f =
  Cmm.Cconst_float (f, dbg)

(* CR Gbury: this conversion int -> nativeint is potentially unsafe
   when cross-compiling for 64-bit on a 32-bit host *)
let int ?(dbg=Debuginfo.none) i =
  natint_const_untagged dbg (Nativeint.of_int i)

let int32 ?(dbg=Debuginfo.none) i =
  natint_const_untagged dbg (Nativeint.of_int32 i)

(* CR Gbury: this conversion int64 -> nativeint is potentially unsafe
     when cross-compiling for 64-bit on a 32-bit host *)
let int64 ?(dbg=Debuginfo.none) i =
  natint_const_untagged dbg (Int64.to_nativeint i)

let nativeint ?(dbg=Debuginfo.none) i =
  natint_const_untagged dbg i

let targetint ?(dbg=Debuginfo.none) t =
  match Targetint_32_64.repr t with
  | Int32 i -> int32 ~dbg i
  | Int64 i -> int64 ~dbg i

(* Infix field address.
   Contrary to regular field addresse, these addresse are valid ocaml values,
   and can be live at gc points. *)

let infix_field_address ~dbg ptr n =
  if n = 0 then
    ptr
  else
    Cmm.Cop (Cmm.Caddv, [ptr; int ~dbg (n * Arch.size_addr)], dbg)

let assign x e = Cmm.Cassign (x, e)

(* Sequence *)

let sequence x y =
  match x, y with
  | Cmm.Ctuple [], _ -> y
  | _, Cmm.Ctuple [] -> x
  | _, _ -> Cmm.Csequence (x, y)

(* Boxing/unboxing *)

let primitive_boxed_int_of_standard_int b =
  match (b : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Naked_immediate
  | Tagged_immediate ->
    Misc.fatal_errorf "No corresponding primitive boxed int type."

let primitive_boxed_int_of_boxable_number b =
  match (b : Flambda_kind.Boxable_number.t) with
  | Naked_float | Untagged_immediate -> assert false
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint

let unbox_number ?(dbg=Debuginfo.none) kind arg =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> unbox_float dbg arg
  | Untagged_immediate -> untag_int arg dbg
  | _ ->
      let primitive_kind = primitive_boxed_int_of_boxable_number kind in
      unbox_int dbg primitive_kind arg

let box_number ?(dbg=Debuginfo.none) kind arg =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> box_float dbg arg
  | Untagged_immediate -> tag_int arg dbg
  | _ ->
      let primitive_kind = primitive_boxed_int_of_boxable_number kind in
      box_int_gen dbg primitive_kind arg

let box_int64 ?dbg arg =
  box_number ?dbg Flambda_kind.Boxable_number.Naked_int64 arg


(* Constructors for operations *)

let unary op = (fun ?(dbg=Debuginfo.none) x -> Cmm.Cop (op, [x], dbg))
let binary op = (fun ?(dbg=Debuginfo.none) x y -> Cmm.Cop (op, [x; y], dbg))

let and_ = binary Cmm.Cand
let or_ = binary Cmm.Cor
let xor_ = binary Cmm.Cxor

let eq ?(dbg=Debuginfo.none) x y =
  match x, y with
  | Cmm.Cconst_int (n, _), Cmm.Cop(Csubi, [Cconst_int (m, _); c], _)
  | Cmm.Cop(Csubi, [Cconst_int (m, _); c], _), Cmm.Cconst_int (n, _) ->
    (* [n = m - c] <=> [c = m - n]

       This is typically generated by expressions of the form
       [if not expr then ...], with [not expr] being compiled to [4 - c]
       and the condition for the test becomes [1 = 4 - c]. *)
    binary Cmm.(Ccmpi Ceq) ~dbg c (Cmm.Cconst_int (m - n, dbg))
  | _, _ -> binary Cmm.(Ccmpi Ceq) ~dbg x y

let neq = binary Cmm.(Ccmpi Cne)

let lt = binary Cmm.(Ccmpi Clt)
let le = binary Cmm.(Ccmpi Cle)
let gt = binary Cmm.(Ccmpi Cgt)
let ge = binary Cmm.(Ccmpi Cge)

let ult = binary Cmm.(Ccmpa Clt)
let ule = binary Cmm.(Ccmpa Cle)
let ugt = binary Cmm.(Ccmpa Cgt)
let uge = binary Cmm.(Ccmpa Cge)

let float_abs = unary Cmm.Cabsf
let float_neg = unary Cmm.Cnegf

let float_add = binary Cmm.Caddf
let float_sub = binary Cmm.Csubf
let float_mul = binary Cmm.Cmulf
let float_div = binary Cmm.Cdivf

let float_eq = binary Cmm.(Ccmpf CFeq)
let float_neq = binary Cmm.(Ccmpf CFneq)
let float_lt = binary Cmm.(Ccmpf CFlt)
let float_le = binary Cmm.(Ccmpf CFle)
let float_gt = binary Cmm.(Ccmpf CFgt)
let float_ge = binary Cmm.(Ccmpf CFge)

let int_of_float = unary Cmm.Cintoffloat
let float_of_int = unary Cmm.Cfloatofint

let letin v e body = Cmm.Clet (v, e, body)
let letin_mut v ty e body = Cmm.Clet_mut (v, ty, e, body)

let ite
    ?(dbg=Debuginfo.none)
    ?(then_dbg=Debuginfo.none) ~then_
    ?(else_dbg=Debuginfo.none) ~else_ cond =
  Cmm.Cifthenelse(cond, then_dbg, then_, else_dbg, else_, dbg)

let load ?(dbg=Debuginfo.none) kind mut addr =
  Cmm.Cop (Cmm.Cload (kind, mut), [addr], dbg)

let store ?(dbg=Debuginfo.none) kind init addr value =
  Cmm.Cop (Cmm.Cstore (kind, init), [addr; value], dbg)

let extcall ?(dbg=Debuginfo.none) ~returns ~alloc ~is_c_builtin ~ty_args
      name typ_res
  args =
  if not returns then assert (typ_res = Cmm.typ_void);
  Cmm.Cop (Cextcall {
      func = name;
      ty = typ_res;
      alloc;
      ty_args;
      returns;
      builtin = is_c_builtin;
      effects = Arbitrary_effects;
      coeffects = Has_coeffects;
    }, args, dbg)


(* Arithmetic helpers *)

let lsl_int_caml_raw ?(dbg=Debuginfo.none) arg1 arg2 =
  incr_int(lsl_int (decr_int arg1 dbg) arg2 dbg) dbg

let lsr_int_caml_raw ?(dbg=Debuginfo.none) arg1 arg2 =
  Cmm.Cop(Cmm.Cor, [lsr_int arg1 arg2 dbg; Cmm.Cconst_int (1, dbg)], dbg)

let asr_int_caml_raw ?(dbg=Debuginfo.none) arg1 arg2 =
  Cmm.Cop(Cmm.Cor, [asr_int arg1 arg2 dbg; Cmm.Cconst_int (1, dbg)], dbg)


(* unreachable/invalid expression *)

let unreachable =
  load Cmm.Word_int Asttypes.Mutable (int 0)


(* Block creation *)

let static_atom_table =
  load Word_int Immutable (symbol "caml_atom_table")

let static_atom ?(dbg=Debuginfo.none) tag =
  Cmm.Cop (Cmm.Caddv, [static_atom_table;
                       int ~dbg ((tag + 1) * Arch.size_addr)], dbg)

let make_alloc_safe ?(dbg=Debuginfo.none) tag = function
  | [] -> static_atom ~dbg tag
  | args -> make_alloc dbg tag args

let make_array ?(dbg=Debuginfo.none) kind args =
  match (kind : Flambda_primitive.Array_kind.t) with
  | Naked_floats ->
      begin match args with
      | [] -> static_atom ~dbg 0 (* 0-size arrays, even float arrays, should have
                                    tag 0, see runtime/array.c:caml_make_vec *)
      | _ -> make_float_alloc dbg (Tag.to_int Tag.double_array_tag) args
      end
  | Float_array_opt_dynamic ->
      begin match args with
      | [] -> static_atom ~dbg 0
      | _ ->
          extcall ~dbg ~alloc:true ~is_c_builtin:false ~returns:true ~ty_args:[]
            "caml_make_array" Cmm.typ_val
            [make_alloc dbg 0 args]
      end
  | Immediates | Values ->
      make_alloc_safe ~dbg 0 args

let make_block ?(dbg=Debuginfo.none) kind args =
  match (kind : Flambda_primitive.Block_kind.t) with
  | Values (tag, _) -> make_alloc_safe ~dbg (Tag.Scannable.to_int tag) args
  | Naked_floats ->
    if List.length args < 1 then begin
      Misc.fatal_error "Don't know what tag to put on zero-sized blocks \
        of naked floats"
    end;
    make_array ~dbg Naked_floats args

let make_closure_block ?(dbg=Debuginfo.none) l =
  assert (List.compare_length_with l 0 > 0);
  let tag = Tag.(to_int closure_tag) in
  make_alloc dbg tag l

(* Block access *)

let block_length ?(dbg=Debuginfo.none) block = get_size block dbg

let block_load ?(dbg=Debuginfo.none) (kind : P.Block_access_kind.t)
      (mutability : Mutability.t) block index =
  let mutability = Mutability.to_lambda mutability in
  match kind with
  | Values { field_kind = Any_value; _ } ->
    get_field_computed Pointer mutability ~block ~index dbg
  | Values { field_kind = Immediate; _ } ->
    get_field_computed Immediate mutability ~block ~index dbg
  | Naked_floats _ -> unboxed_float_array_ref block index dbg

let block_set ?(dbg=Debuginfo.none) (kind : P.Block_access_kind.t)
      (init : P.Init_or_assign.t) block index new_value =
  let init_or_assign = P.Init_or_assign.to_lambda init in
  match kind with
  | Values { field_kind = Any_value; _ } ->
    setfield_computed Pointer init_or_assign block index new_value dbg
    |> return_unit dbg
  | Values { field_kind = Immediate; _ } ->
    setfield_computed Immediate init_or_assign block index new_value dbg
    |> return_unit dbg
  | Naked_floats _ ->
    float_array_set block index new_value dbg
    |> return_unit dbg

(* Array access *)

let array_length ?(dbg=Debuginfo.none) kind arr =
  arraylength (P.Array_kind.to_lambda kind) arr dbg

let array_load ?(dbg=Debuginfo.none) (kind : P.Array_kind.t) arr index =
  match kind with
  | Immediates -> int_array_ref arr index dbg
  | Values -> addr_array_ref arr index dbg
  | Naked_floats -> unboxed_float_array_ref arr index dbg
  | Float_array_opt_dynamic ->
    ite ~dbg (is_addr_array_ptr arr dbg)
      ~then_:(addr_array_ref arr index dbg) ~then_dbg:dbg
      ~else_:(float_array_ref arr index dbg) ~else_dbg:dbg

let addr_array_store init arr index value dbg =
  match (init : P.Init_or_assign.t) with
  | Assignment -> addr_array_set arr index value dbg
  | Initialization -> addr_array_initialize arr index value dbg

let array_set ?(dbg=Debuginfo.none) (kind : P.Array_kind.t)
      (init : P.Init_or_assign.t) arr index value =
  match kind with
  | Immediates -> return_unit dbg (int_array_set arr index value dbg)
  | Values -> return_unit dbg (addr_array_store init arr index value dbg)
  | Naked_floats -> return_unit dbg (float_array_set arr index value dbg)
  | Float_array_opt_dynamic ->
    ite ~dbg
      (is_addr_array_ptr arr dbg)
      ~then_:(addr_array_store init arr index value dbg)
      ~then_dbg:dbg
      ~else_:(float_array_set arr index (unbox_float dbg value) dbg)
      ~else_dbg:dbg
    |> return_unit dbg

(* String and bytes access *)

(* here, block and ptr are different only for bigstrings, because the
   extcall must apply to the whole bigstring block (variable [block]),
   whereas the loads apply to the bigstring data pointer (variable [ptr]).
   For regular strings, [block = ptr]. *)
let string_like_load_aux ~dbg _kind width _block ptr idx =
  match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
      let idx = untag_int idx dbg in
      load ~dbg Cmm.Byte_unsigned Asttypes.Mutable (add_int ptr idx dbg)
  | Sixteen ->
      let idx = untag_int idx dbg in
      unaligned_load_16 ptr idx dbg
  | Thirty_two ->
      let idx = untag_int idx dbg in
      sign_extend_32 dbg (unaligned_load_32 ptr idx dbg)
  | Sixty_four ->
      if arch32 then
        unsupported_32_bits ()
      else begin
        let idx = untag_int idx dbg in
        unaligned_load_64 ptr idx dbg
      end

let string_like_load ?(dbg=Debuginfo.none) kind width block index =
  match (kind : Flambda_primitive.string_like_value) with
  | String | Bytes ->
      string_like_load_aux ~dbg kind width block block index
  | Bigstring ->
      let ba_data_addr = field_address block 1 dbg in
      let ba_data = load ~dbg Cmm.Word_int Asttypes.Mutable ba_data_addr in
      bind "ba_data" ba_data (fun ptr ->
          string_like_load_aux ~dbg kind width block ptr index)

(* same as {string_like_load_aux} *)
let bytes_like_set_aux ~dbg _kind width _block ptr idx value =
  begin match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
      let idx = untag_int idx dbg in
      store ~dbg Cmm.Byte_unsigned Lambda.Assignment (add_int ptr idx dbg) value
  | Sixteen ->
      let idx = untag_int idx dbg in
      unaligned_set_16 ptr idx value dbg
  | Thirty_two ->
      let idx = untag_int idx dbg in
      unaligned_set_32 ptr idx value dbg
  | Sixty_four ->
      if arch32 then
        unsupported_32_bits ()
      else begin
        let idx = untag_int idx dbg in
        unaligned_set_64 ptr idx value dbg
      end
  end

let bytes_like_set ?(dbg=Debuginfo.none) kind width block index value =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes ->
    return_unit dbg (
      bytes_like_set_aux ~dbg kind width block block index value)
  | Bigstring ->
      let ba_data_addr = field_address block 1 dbg in
      let ba_data = load ~dbg Cmm.Word_int Asttypes.Mutable ba_data_addr in
      return_unit dbg (
        bind "ba_data" ba_data (fun ptr ->
          bytes_like_set_aux ~dbg kind width block ptr index value)
      )

(* wrappers for bigarrays *)

let lambda_ba_kind k : Lambda.bigarray_kind =
  match (k : Flambda_primitive.bigarray_kind) with
  | Float32 -> Pbigarray_float32
  | Float64 -> Pbigarray_float64
  | Sint8   -> Pbigarray_sint8
  | Uint8   -> Pbigarray_uint8
  | Sint16  -> Pbigarray_sint16
  | Uint16  -> Pbigarray_uint16
  | Int32   -> Pbigarray_int32
  | Int64   -> Pbigarray_int64
  | Int_width_int -> Pbigarray_caml_int
  | Targetint_width_int -> Pbigarray_native_int
  | Complex32 -> Pbigarray_complex32
  | Complex64 -> Pbigarray_complex64

let bigarray_load ?(dbg=Debuginfo.none) _dims kind _layout ba offset =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = bigarray_word_kind elt_kind in
  let ba_data_f = field_address ba 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p offset dbg
  in
  match (elt_kind : Lambda.bigarray_kind) with
  | Pbigarray_complex32
  | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int (elt_size / 2)) in
    box_complex dbg
      (load ~dbg elt_chunk Mutable addr)
      (load ~dbg elt_chunk Mutable addr')
  | _ ->
    load ~dbg elt_chunk Mutable addr

let bigarray_store ?(dbg=Debuginfo.none) _dims kind _layout ba offset v =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = bigarray_word_kind elt_kind in
  let ba_data_f = field_address ba 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p offset dbg
  in
  match elt_kind with
  | Pbigarray_complex32
  | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int (elt_size / 2)) in
    return_unit dbg (
      sequence
        (store ~dbg elt_chunk Assignment addr (complex_re v dbg))
        (store ~dbg elt_chunk Assignment addr' (complex_im v dbg))
    )
  | _ ->
    return_unit dbg
      (store ~dbg elt_chunk Assignment addr v)


(* try-with blocks *)

let trywith ?(dbg=Debuginfo.none) ~kind ~body ~exn_var ~handler () =
  Cmm.Ctrywith (body, kind, exn_var, handler, dbg)

let raise_kind (kind : Trap_action.raise_kind option) : Lambda.raise_kind =
  match kind with
  | Some Regular -> Raise_regular
  | Some Reraise -> Raise_reraise
  | Some No_trace -> Raise_notrace
  | None -> Raise_notrace


(* Static jumps *)

type static_handler =
  int *
  ((Backend_var.With_provenance.t * Cmm.machtype) list) *
  Cmm.expression *
  Debuginfo.t
(* Alias for static handler *)

let handler ?(dbg=Debuginfo.none) id vars body =
  (id, vars, body, dbg)

let cexit id args trap_actions =
  Cmm.Cexit (Cmm.Lbl id, args, trap_actions)

let trap_return arg trap_actions =
  Cmm.Cexit (Cmm.Return_lbl, [arg], trap_actions)

let ccatch ~rec_flag ~handlers ~body =
  let rec_flag = if rec_flag then Cmm.Recursive else Cmm.Nonrecursive in
  Cmm.Ccatch (rec_flag, handlers, body)


(* Function calls *)

let direct_call ?(dbg=Debuginfo.none) ty f_code_sym args =
  Cmm.Cop (Cmm.Capply ty, f_code_sym :: args, dbg)

let indirect_call ?(dbg=Debuginfo.none) ty f = function
  | [arg] ->
    (* Use a variable to avoid duplicating the cmm code of the closure [f]. *)
    let v = Backend_var.create_local "*closure*" in
    let v' = Backend_var.With_provenance.create v in
    letin v' f @@
    Cmm.Cop (Cmm.Capply ty,
             [load Cmm.Word_int Asttypes.Mutable (var v); arg; (var v)],
             dbg)
  | args ->
    let arity = List.length args in
    let l = symbol (apply_function_sym arity) :: args @ [f] in
    Cmm.Cop (Cmm.Capply ty, l, dbg)

let indirect_full_call ?(dbg=Debuginfo.none) ty f = function
  (* the single-argument case is already optimized by indirect_call *)
  | [_] as args -> indirect_call ~dbg ty f args
  | args ->
    (* Use a variable to avoid duplicating the cmm code of the closure [f]. *)
    let v = Backend_var.create_local "*closure*" in
    let v' = Backend_var.With_provenance.create v in
    (* get the function's code pointer *)
    let fun_ptr =
      load Cmm.Word_int Asttypes.Mutable @@
      field_address (var v) 2 dbg
    in
    letin v' f @@
    Cmm.Cop (Cmm.Capply ty, fun_ptr :: args @ [var v], dbg)



(* Cmm phrases *)

let cfunction decl =
  Cmm.Cfunction decl

let cdata d =
  Cmm.Cdata d

let fundecl fun_name fun_args fun_body fun_codegen_options fun_dbg =
  { Cmm.fun_name; fun_args; fun_body; fun_codegen_options; fun_dbg; }


(* Gc root table *)

let gc_root_table syms =
  let table_symbol = Compilenv.make_symbol (Some "gc_roots") in
  cdata (
    define_symbol ~global:true table_symbol @
    List.map symbol_address syms @
    [ cint 0n ]
  )

(* Get constant tables from cmmgen_state

   The Un_cps translation uses functions from cmm_helpers which populate some mutable
   state in cmmgen_state, so we have to get the created constants. *)

let flush_cmmgen_state () =
  let aux name cst acc =
    match (cst : Cmmgen_state.constant) with
    | Const_table (Local, l) ->
        cdata (define_symbol ~global:false name @ l) :: acc
    | Const_table (Global, l) ->
        cdata (define_symbol ~global:true name @ l) :: acc
    | Const_closure _ ->
        Misc.fatal_errorf
          "There shouldn't be any closure in cmmgen_state during flambda to cmm translation"
  in
  match Cmmgen_state.get_and_clear_data_items () with
  | [] ->
      let cst_map = Cmmgen_state.get_and_clear_constants () in
      Misc.Stdlib.String.Map.fold aux cst_map []
  | _ ->
      Misc.fatal_errorf
        "There shouldn't be any data item in cmmgen_state during flambda to cmm translation"

