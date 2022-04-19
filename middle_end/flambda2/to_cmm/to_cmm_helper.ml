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
module Ece = Effects_and_coeffects
module P = Flambda_primitive

let unsupported_32_bits () =
  Misc.fatal_errorf "32 bits is currently unsupported in Flambda."

(* Are we compiling on/for a 32-bit architecture ? *)
let arch32 = Arch.size_int = 4

let arch64 = Arch.size_int = 8

(* Useful shortcut *)
let typ_int64 = if arch32 then [| Cmm.Int; Cmm.Int |] else [| Cmm.Int |]

let exttype_of_kind k =
  match (k : Flambda_kind.t) with
  | Value -> Cmm.XInt
  | Naked_number Naked_float -> Cmm.XFloat
  | Naked_number Naked_int64 -> Cmm.XInt64
  | Naked_number Naked_int32 -> Cmm.XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) -> begin
    match Targetint_32_64.num_bits with
    | Thirty_two -> Cmm.XInt32
    | Sixty_four -> Cmm.XInt64
  end
  | Region -> Misc.fatal_error "[Region] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

(* Void *)

let void = Cmm.Ctuple []

let unit ~dbg = Cmm.Cconst_int (1, dbg)

(* Data items *)

let cint i = Cmm.Cint i

let cfloat f = Cmm.Cdouble f

let symbol_address s = Cmm.Csymbol_address s

let define_symbol ~global s =
  if global
  then [Cmm.Cglobal_symbol s; Cmm.Cdefine_symbol s]
  else [Cmm.Cdefine_symbol s]

(* Kinds and types *)

let check_arity arity args =
  Flambda_arity.With_subkinds.cardinal arity = List.length args

let machtype_of_kind k =
  match (k : Flambda_kind.t) with
  | Value -> Cmm.typ_val
  | Naked_number Naked_float -> Cmm.typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
    Cmm.typ_int
  | Region | Rec_info -> assert false

let machtype_of_kinded_parameter p =
  Bound_parameter.kind p |> Flambda_kind.With_subkind.kind |> machtype_of_kind

(* Constructors for constants *)

let var v = Cmm.Cvar v

let symbol_from_string ?(dbg = Debuginfo.none) sym = Cmm.Cconst_symbol (sym, dbg)

let symbol_from_linkage_name ?dbg ln =
  symbol_from_string ?dbg (Linkage_name.to_string ln)

let symbol ?dbg sym = symbol_from_linkage_name ?dbg (Symbol.linkage_name sym)

let name env name =
  Name.pattern_match name
    ~var:(fun v -> To_cmm_env.inline_variable env v)
    ~symbol:(fun s ->
      let env =
        To_cmm_env.check_scope ~allow_deleted:false env
          (Code_id_or_symbol.create_symbol s)
      in
      ( Cmm.Cconst_symbol
          (Linkage_name.to_string (Symbol.linkage_name s), Debuginfo.none),
        env,
        Ece.pure ))

let float ?(dbg = Debuginfo.none) f = Cmm.Cconst_float (f, dbg)

(* CR Gbury: this conversion int -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int ?(dbg = Debuginfo.none) i =
  natint_const_untagged dbg (Nativeint.of_int i)

let int32 ?(dbg = Debuginfo.none) i =
  natint_const_untagged dbg (Nativeint.of_int32 i)

(* CR Gbury: this conversion int64 -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int64 ?(dbg = Debuginfo.none) i =
  natint_const_untagged dbg (Int64.to_nativeint i)

let nativeint ?(dbg = Debuginfo.none) i = natint_const_untagged dbg i

let targetint ?(dbg = Debuginfo.none) t =
  match Targetint_32_64.repr t with
  | Int32 i -> int32 ~dbg i
  | Int64 i -> int64 ~dbg i

(* CR mshinwell: The following functions should be in [Targetint], etc. *)

let tag_targetint t = Targetint_32_64.(add (shift_left t 1) one)

let targetint_of_imm i =
  Targetint_31_63.Imm.to_targetint i.Targetint_31_63.value

let nativeint_of_targetint t =
  match Targetint_32_64.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

let const _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> targetint (targetint_of_imm i)
  | Tagged_immediate i -> targetint (tag_targetint (targetint_of_imm i))
  | Naked_float f -> float (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> int32 i
  | Naked_int64 i -> int64 i
  | Naked_nativeint t -> targetint t

(* [Simple]s and lists thereof *)

let simple env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name env n)
    ~const:(fun c -> const env c, env, Ece.pure)

let arg_list env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple env x in
    y :: list, env, Ece.join eff effs
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

let param_list env l =
  let flambda_vars = Bound_parameters.vars l in
  let env, cmm_vars = To_cmm_env.create_variables env flambda_vars in
  let vars =
    List.map2
      (fun v v' -> v, machtype_of_kinded_parameter v')
      cmm_vars
      (Bound_parameters.to_list l)
  in
  env, vars

(* Infix field address. Contrary to regular field addresse, these addresse are
   valid ocaml values, and can be live at gc points. *)

let infix_field_address ~dbg ptr n =
  if n = 0
  then ptr
  else Cmm.Cop (Cmm.Caddv, [ptr; int ~dbg (n * Arch.size_addr)], dbg)

let assign x e = Cmm.Cassign (x, e)

(* Sequence *)

let sequence x y =
  match x, y with
  | Cmm.Ctuple [], _ -> y
  | _, Cmm.Ctuple [] -> x
  | _, _ -> Cmm.Csequence (x, y)

(* Allocation modes *)

let convert_alloc_mode (alloc_mode : Alloc_mode.t) : Lambda.alloc_mode =
  match alloc_mode with Heap -> Alloc_heap | Local -> Alloc_local

(* Boxing/unboxing *)

let primitive_boxed_int_of_standard_int b =
  match (b : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Naked_immediate | Tagged_immediate ->
    Misc.fatal_errorf "No corresponding primitive boxed int type."

let primitive_boxed_int_of_boxable_number b =
  match (b : Flambda_kind.Boxable_number.t) with
  | Naked_float -> assert false
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint

let unbox_number ?(dbg = Debuginfo.none) kind arg =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> unbox_float dbg arg
  | _ ->
    let primitive_kind = primitive_boxed_int_of_boxable_number kind in
    unbox_int dbg primitive_kind arg

let box_number ?(dbg = Debuginfo.none) kind alloc_mode arg =
  let alloc_mode = convert_alloc_mode alloc_mode in
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> box_float dbg alloc_mode arg
  | _ ->
    let primitive_kind = primitive_boxed_int_of_boxable_number kind in
    box_int_gen dbg primitive_kind alloc_mode arg

let box_int64 ?dbg alloc_mode arg =
  box_number ?dbg Flambda_kind.Boxable_number.Naked_int64 alloc_mode arg

(* Constructors for operations *)

let unary op ?(dbg = Debuginfo.none) x = Cmm.Cop (op, [x], dbg)

let binary op ?(dbg = Debuginfo.none) x y = Cmm.Cop (op, [x; y], dbg)

let int_of_float = unary Cmm.Cintoffloat

let float_of_int = unary Cmm.Cfloatofint

let letin_mut v ty e body = Cmm.Clet_mut (v, ty, e, body)

let ite ?(dbg = Debuginfo.none) ?(then_dbg = Debuginfo.none) ~then_
    ?(else_dbg = Debuginfo.none) ~else_ cond =
  Cmm.Cifthenelse
    ( cond,
      then_dbg,
      then_,
      else_dbg,
      else_,
      dbg,
      (* CR-someday poechsel: Put a correct value kind here *)
      Vval Pgenval )

let extcall ?(dbg = Debuginfo.none) ~returns ~alloc ~is_c_builtin ~ty_args name
    typ_res args =
  if not returns then assert (typ_res = Cmm.typ_void);
  Cmm.Cop
    ( Cextcall
        { func = name;
          ty = typ_res;
          alloc;
          ty_args;
          returns;
          builtin = is_c_builtin;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects
        },
      args,
      dbg )

(* Arithmetic helpers *)

let lsl_int_caml_raw ?(dbg = Debuginfo.none) arg1 arg2 =
  incr_int (lsl_int (decr_int arg1 dbg) arg2 dbg) dbg

let lsr_int_caml_raw ?(dbg = Debuginfo.none) arg1 arg2 =
  Cmm.Cop (Cmm.Cor, [lsr_int arg1 arg2 dbg; Cmm.Cconst_int (1, dbg)], dbg)

let asr_int_caml_raw ?(dbg = Debuginfo.none) arg1 arg2 =
  Cmm.Cop (Cmm.Cor, [asr_int arg1 arg2 dbg; Cmm.Cconst_int (1, dbg)], dbg)

(* Block creation *)

(* Blocks of size 0 (i.e. with an empty list of fields) must be statically
   allocated, else the GC will bug (cf `make_alloc_generic` in cmm_helpers.ml).
   More precisely, blocks of size 0 must have a black header, which means they
   must either be statically allocated, or be pointers to one of the cell of the
   atom_table (see `startup_aux.c`).

   Both `make_alloc` and `make_float_alloc` from `cmm_helpers.ml` already check
   for that, but with an assertion, which do not produce helpful error
   messages. *)
let check_alloc_fields = function
  | [] ->
    Misc.fatal_error
      "Blocks dynamically allocated cannot have size 0 (empty arrays have to \
       be lifted so they can be statically allocated)"
  | _ -> ()

let make_array ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Array_kind.t) with
  | Immediates | Values ->
    make_alloc ~mode:(convert_alloc_mode alloc_mode) dbg 0 args
  | Naked_floats ->
    make_float_alloc
      ~mode:(convert_alloc_mode alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let make_block ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Block_kind.t) with
  | Values (tag, _) ->
    make_alloc
      ~mode:(convert_alloc_mode alloc_mode)
      dbg (Tag.Scannable.to_int tag) args
  | Naked_floats ->
    make_float_alloc
      ~mode:(convert_alloc_mode alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let make_closure_block ?(dbg = Debuginfo.none) alloc_mode l =
  assert (List.compare_length_with l 0 > 0);
  let tag = Tag.(to_int closure_tag) in
  make_alloc ~mode:(convert_alloc_mode alloc_mode) dbg tag l

(* Block access *)

let block_length ?(dbg = Debuginfo.none) block = get_size block dbg

let block_load ?(dbg = Debuginfo.none) (kind : P.Block_access_kind.t)
    (mutability : Mutability.t) block index =
  let mutability = Mutability.to_lambda mutability in
  match kind with
  | Values { field_kind = Any_value; _ } ->
    get_field_computed Pointer mutability ~block ~index dbg
  | Values { field_kind = Immediate; _ } ->
    get_field_computed Immediate mutability ~block ~index dbg
  | Naked_floats _ -> unboxed_float_array_ref block index dbg

let block_set ?(dbg = Debuginfo.none) (kind : P.Block_access_kind.t)
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
    float_array_set block index new_value dbg |> return_unit dbg

(* Array access *)

let array_length ?(dbg = Debuginfo.none) arr =
  (* [Paddrarray] may be a lie sometimes, but we know for certain that the bit
     width of floats is equal to the machine word width (see flambda2.ml). This
     means that [arraylength] will not use the kind information. *)
  assert (wordsize_shift = numfloat_shift);
  arraylength Paddrarray arr dbg

let array_load ?(dbg = Debuginfo.none) (kind : P.Array_kind.t) arr index =
  match kind with
  | Immediates -> int_array_ref arr index dbg
  | Values -> addr_array_ref arr index dbg
  | Naked_floats -> unboxed_float_array_ref arr index dbg

let addr_array_store init arr index value dbg =
  match (init : P.Init_or_assign.t) with
  | Assignment -> addr_array_set arr index value dbg
  | Initialization -> addr_array_initialize arr index value dbg
  | Local_assignment -> addr_array_set_local arr index value dbg

let array_set ?(dbg = Debuginfo.none) (kind : P.Array_kind.t)
    (init : P.Init_or_assign.t) arr index value =
  match kind with
  | Immediates -> return_unit dbg (int_array_set arr index value dbg)
  | Values -> return_unit dbg (addr_array_store init arr index value dbg)
  | Naked_floats -> return_unit dbg (float_array_set arr index value dbg)

(* String and bytes access *)

(* here, block and ptr are different only for bigstrings, because the extcall
   must apply to the whole bigstring block (variable [block]), whereas the loads
   apply to the bigstring data pointer (variable [ptr]). For regular strings,
   [block = ptr]. *)
let string_like_load_aux ~dbg _kind width _block ptr idx =
  match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
    let idx = untag_int idx dbg in
    load ~dbg Cmm.Byte_unsigned Asttypes.Mutable ~addr:(add_int ptr idx dbg)
  | Sixteen ->
    let idx = untag_int idx dbg in
    unaligned_load_16 ptr idx dbg
  | Thirty_two ->
    let idx = untag_int idx dbg in
    sign_extend_32 dbg (unaligned_load_32 ptr idx dbg)
  | Sixty_four ->
    if arch32
    then unsupported_32_bits ()
    else
      let idx = untag_int idx dbg in
      unaligned_load_64 ptr idx dbg

let string_like_load ?(dbg = Debuginfo.none) kind width block index =
  match (kind : Flambda_primitive.string_like_value) with
  | String | Bytes -> string_like_load_aux ~dbg kind width block block index
  | Bigstring ->
    let ba_data_addr = field_address block 1 dbg in
    let ba_data = load ~dbg Cmm.Word_int Asttypes.Mutable ~addr:ba_data_addr in
    bind "ba_data" ba_data (fun ptr ->
        string_like_load_aux ~dbg kind width block ptr index)

(* same as {string_like_load_aux} *)
let bytes_like_set_aux ~dbg _kind width _block ptr idx value =
  match (width : Flambda_primitive.string_accessor_width) with
  | Eight ->
    let idx = untag_int idx dbg in
    store ~dbg Cmm.Byte_unsigned Lambda.Assignment ~addr:(add_int ptr idx dbg)
      ~new_value:value
  | Sixteen ->
    let idx = untag_int idx dbg in
    unaligned_set_16 ptr idx value dbg
  | Thirty_two ->
    let idx = untag_int idx dbg in
    unaligned_set_32 ptr idx value dbg
  | Sixty_four ->
    if arch32
    then unsupported_32_bits ()
    else
      let idx = untag_int idx dbg in
      unaligned_set_64 ptr idx value dbg

let bytes_like_set ?(dbg = Debuginfo.none) kind width block index value =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes ->
    return_unit dbg (bytes_like_set_aux ~dbg kind width block block index value)
  | Bigstring ->
    let ba_data_addr = field_address block 1 dbg in
    let ba_data = load ~dbg Cmm.Word_int Asttypes.Mutable ~addr:ba_data_addr in
    return_unit dbg
      (bind "ba_data" ba_data (fun ptr ->
           bytes_like_set_aux ~dbg kind width block ptr index value))

(* wrappers for bigarrays *)

let lambda_ba_kind k : Lambda.bigarray_kind =
  match (k : Flambda_primitive.bigarray_kind) with
  | Float32 -> Pbigarray_float32
  | Float64 -> Pbigarray_float64
  | Sint8 -> Pbigarray_sint8
  | Uint8 -> Pbigarray_uint8
  | Sint16 -> Pbigarray_sint16
  | Uint16 -> Pbigarray_uint16
  | Int32 -> Pbigarray_int32
  | Int64 -> Pbigarray_int64
  | Int_width_int -> Pbigarray_caml_int
  | Targetint_width_int -> Pbigarray_native_int
  | Complex32 -> Pbigarray_complex32
  | Complex64 -> Pbigarray_complex64

let bigarray_load ?(dbg = Debuginfo.none) _dims kind _layout ba offset =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = bigarray_word_kind elt_kind in
  let ba_data_f = field_address ba 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p offset dbg
  in
  match (elt_kind : Lambda.bigarray_kind) with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int (elt_size / 2)) in
    box_complex dbg
      (load ~dbg elt_chunk Mutable ~addr)
      (load ~dbg elt_chunk Mutable ~addr:addr')
  | _ -> load ~dbg elt_chunk Mutable ~addr

let bigarray_store ?(dbg = Debuginfo.none) _dims kind _layout ba offset v =
  let elt_kind = lambda_ba_kind kind in
  let elt_size = bigarray_elt_size_in_bytes elt_kind in
  let elt_chunk = bigarray_word_kind elt_kind in
  let ba_data_f = field_address ba 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p offset dbg
  in
  match elt_kind with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int (elt_size / 2)) in
    return_unit dbg
      (sequence
         (store ~dbg elt_chunk Assignment ~addr ~new_value:(complex_re v dbg))
         (store ~dbg elt_chunk Assignment ~addr:addr'
            ~new_value:(complex_im v dbg)))
  | _ -> return_unit dbg (store ~dbg elt_chunk Assignment ~addr ~new_value:v)

(* try-with blocks *)

let trywith ?(dbg = Debuginfo.none) ~kind ~body ~exn_var ~handler () =
  (* CR-someday poechsel: Put a correct value kind here *)
  Cmm.Ctrywith (body, kind, exn_var, handler, dbg, Vval Pgenval)

let raise_kind (kind : Trap_action.raise_kind option) : Lambda.raise_kind =
  match kind with
  | Some Regular -> Raise_regular
  | Some Reraise -> Raise_reraise
  | Some No_trace -> Raise_notrace
  | None -> Raise_notrace

(* Static jumps *)

type static_handler =
  int
  * (Backend_var.With_provenance.t * Cmm.machtype) list
  * Cmm.expression
  * Debuginfo.t
(* Alias for static handler *)

let handler ?(dbg = Debuginfo.none) id vars body = id, vars, body, dbg

let cexit id args trap_actions = Cmm.Cexit (Cmm.Lbl id, args, trap_actions)

let trap_return arg trap_actions =
  Cmm.Cexit (Cmm.Return_lbl, [arg], trap_actions)

let ccatch ~rec_flag ~handlers ~body =
  let rec_flag = if rec_flag then Cmm.Recursive else Cmm.Nonrecursive in
  Cmm.Ccatch (rec_flag, handlers, body, Vval Pgenval)

(* Call into `caml_flambda2_invalid` for invalid/unreachable code, instead of
   simply generating code that segfaults *)
let invalid res ~message =
  let message_sym =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name (Variable.create "invalid")))
  in
  let data_items =
    Cmm_helpers.emit_string_constant
      (Symbol.linkage_name_as_string message_sym, Cmmgen_state.Global)
      message []
    |> To_cmm_result.add_data_items res
  in
  let call_expr =
    extcall ~dbg:Debuginfo.none ~alloc:false ~is_c_builtin:false ~returns:false
      ~ty_args:[XInt] "caml_flambda2_invalid" Cmm.typ_void [symbol message_sym]
  in
  call_expr, data_items

(* Cmm phrases *)

let cfunction decl = Cmm.Cfunction decl

let cdata d = Cmm.Cdata d

let fundecl fun_name fun_args fun_body fun_codegen_options fun_dbg =
  { Cmm.fun_name; fun_args; fun_body; fun_codegen_options; fun_dbg }

(* Gc root table *)

let gc_root_table ~make_symbol syms =
  let table_symbol = make_symbol ?unitname:None (Some "gc_roots") in
  cdata
    (define_symbol ~global:true table_symbol
    @ List.map symbol_address syms
    @ [cint 0n])

(* Get constant tables from cmmgen_state

   The To_cmm translation uses functions from cmm_helpers which populate some
   mutable state in cmmgen_state, so we have to get the created constants. *)

let flush_cmmgen_state () =
  let aux name cst acc =
    match (cst : Cmmgen_state.constant) with
    | Const_table (Local, l) ->
      cdata (define_symbol ~global:false name @ l) :: acc
    | Const_table (Global, l) ->
      cdata (define_symbol ~global:true name @ l) :: acc
    | Const_closure _ ->
      Misc.fatal_errorf
        "There shouldn't be any closure in cmmgen_state during flambda to cmm \
         translation"
  in
  match Cmmgen_state.get_and_clear_data_items () with
  | [] ->
    let cst_map = Cmmgen_state.get_and_clear_constants () in
    Misc.Stdlib.String.Map.fold aux cst_map []
  | _ ->
    Misc.fatal_errorf
      "There shouldn't be any data item in cmmgen_state during flambda to cmm \
       translation"

let make_update env kind ~symbol var ~index ~prev_updates =
  let e, env, _ece = To_cmm_env.inline_variable env var in
  let addr = field_address symbol index Debuginfo.none in
  let update = store kind Lambda.Root_initialization ~addr ~new_value:e in
  match prev_updates with
  | None -> env, Some update
  | Some prev_updates -> env, Some (sequence prev_updates update)

let name_static env name =
  Name.pattern_match name
    ~var:(fun v -> env, `Var v)
    ~symbol:(fun s ->
      ( To_cmm_env.check_scope ~allow_deleted:false env
          (Code_id_or_symbol.create_symbol s),
        `Data [symbol_address (Symbol.linkage_name_as_string s)] ))

let const_static cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> [cint (nativeint_of_targetint (targetint_of_imm i))]
  | Tagged_immediate i ->
    [cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))]
  | Naked_float f -> [cfloat (Numeric_types.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i -> [cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
    if arch32
    then
      Misc.fatal_error "Not implemented for 32-bit platforms"
      (* split int64 on 32-bit archs *)
    else [cint (Int64.to_nativeint i)]
  | Naked_nativeint t -> [cint (nativeint_of_targetint t)]

let simple_static env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static env n)
    ~const:(fun c -> env, `Data (const_static c))
