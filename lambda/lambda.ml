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

open Misc
open Asttypes

type constant = Typedtree.constant

type mutable_flag = Immutable | Immutable_unique | Mutable

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type
  | Runtime5

type immediate_or_pointer =
  | Immediate
  | Pointer

type is_safe =
  | Safe
  | Unsafe

type field_read_semantics =
  | Reads_agree
  | Reads_vary

type has_initializer =
  | With_initializer
  | Uninitialized

include (struct

  type locality_mode =
    | Alloc_heap
    | Alloc_local

  type modify_mode =
    | Modify_heap
    | Modify_maybe_stack

  let alloc_heap = Alloc_heap

  let alloc_local =
    if Config.stack_allocation then Alloc_local
    else Alloc_heap

  let modify_heap = Modify_heap

  let modify_maybe_stack : modify_mode =
    if Config.stack_allocation then Modify_maybe_stack
    else Modify_heap

  let join_locality_mode a b =
    match a, b with
    | Alloc_local, _ | _, Alloc_local -> Alloc_local
    | Alloc_heap, Alloc_heap -> Alloc_heap
end : sig

  type locality_mode = private
    | Alloc_heap
    | Alloc_local

  type modify_mode = private
    | Modify_heap
    | Modify_maybe_stack

  val alloc_heap : locality_mode
  val alloc_local : locality_mode

  val modify_heap : modify_mode

  val modify_maybe_stack : modify_mode

  val join_locality_mode : locality_mode -> locality_mode -> locality_mode
end)

let is_local_mode = function
  | Alloc_heap -> false
  | Alloc_local -> true

let is_heap_mode = function
  | Alloc_heap -> true
  | Alloc_local -> false

let sub_locality_mode a b =
  match a, b with
  | Alloc_heap, _ -> true
  | _, Alloc_local -> true
  | Alloc_local, Alloc_heap -> false

let eq_locality_mode a b =
  match a, b with
  | Alloc_heap, Alloc_heap -> true
  | Alloc_local, Alloc_local -> true
  | Alloc_heap, Alloc_local -> false
  | Alloc_local, Alloc_heap -> false

type initialization_or_assignment =
  | Assignment of modify_mode
  | Heap_initialization
  | Root_initialization

type region_close =
  | Rc_normal
  | Rc_nontail
  | Rc_close_at_apply

type lazy_block_tag =
  | Lazy_tag
  | Forward_tag

type pos = In_singleton | In_product of int

type primitive =
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
    (* Globals *)
  | Pgetglobal of Compilation_unit.t
  | Psetglobal of Compilation_unit.t
  | Pgetpredef of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape * locality_mode
  | Pmakefloatblock of mutable_flag * locality_mode
  | Pmakeufloatblock of mutable_flag * locality_mode
  | Pmakemixedblock of int * mutable_flag * mixed_block_shape * locality_mode
  | Pmakelazyblock of lazy_block_tag
  | Pfield of int * immediate_or_pointer * field_read_semantics
  | Pfield_computed of field_read_semantics
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int * field_read_semantics * locality_mode
  | Pufloatfield of int * field_read_semantics
  | Pmixedfield of int list * mixed_block_shape_with_locality_mode
      * field_read_semantics
  | Psetfloatfield of int * initialization_or_assignment
  | Psetufloatfield of int * initialization_or_assignment
  | Psetmixedfield of int list * mixed_block_shape * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Unboxed products *)
  | Pmake_unboxed_product of layout list
  | Punboxed_product_field of int * layout list
  | Parray_element_size_in_bytes of array_kind
  (* Block indices *)
  | Pidx_field of pos
  | Pidx_mixed_field of unit mixed_block_element * pos list
  | Pidx_array of
      array_kind * array_index_kind * unit mixed_block_element * pos list
  | Pidx_deepen of unit mixed_block_element * pos list
  (* Context switches *)
  | Prunstack
  | Pperform
  | Presume
  | Preperform
  (* External call *)
  | Pccall of external_call_description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  | Pcompare_ints
  | Pcompare_floats of boxed_float
  | Pcompare_bints of boxed_integer
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pfloatoffloat32 of locality_mode
  | Pfloat32offloat of locality_mode
  | Pintoffloat of boxed_float
  | Pfloatofint of boxed_float * locality_mode
  | Pnegfloat of boxed_float * locality_mode
  | Pabsfloat of boxed_float * locality_mode
  | Paddfloat of boxed_float * locality_mode
  | Psubfloat of boxed_float * locality_mode
  | Pmulfloat of boxed_float * locality_mode
  | Pdivfloat of boxed_float * locality_mode
  | Pfloatcomp of boxed_float * float_comparison
  | Punboxed_float_comp of unboxed_float * float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag * locality_mode
  | Pmakearray_dynamic of array_kind * locality_mode * has_initializer
  | Pduparray of array_kind * mutable_flag
  | Parrayblit of {
      src_mutability : mutable_flag;
      dst_array_set_kind : array_set_kind;
    }
  | Parraylength of array_kind
  | Parrayrefu of array_ref_kind * array_index_kind * mutable_flag
  | Parraysetu of array_set_kind * array_index_kind
  | Parrayrefs of array_ref_kind * array_index_kind * mutable_flag
  | Parraysets of array_set_kind * array_index_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint of { variant_only : bool }
  (* Test if the argument is a null pointer *)
  | Pisnull
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer * locality_mode
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
                * locality_mode
  | Pnegbint of boxed_integer * locality_mode
  | Paddbint of boxed_integer * locality_mode
  | Psubbint of boxed_integer * locality_mode
  | Pmulbint of boxed_integer * locality_mode
  | Pdivbint of { size : boxed_integer; is_safe : is_safe; mode: locality_mode }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe; mode: locality_mode }
  | Pandbint of boxed_integer * locality_mode
  | Porbint of boxed_integer * locality_mode
  | Pxorbint of boxed_integer * locality_mode
  | Plslbint of boxed_integer * locality_mode
  | Plsrbint of boxed_integer * locality_mode
  | Pasrbint of boxed_integer * locality_mode
  | Pbintcomp of boxed_integer * integer_comparison
  | Punboxed_int_comp of unboxed_integer * integer_comparison
  (* Operations on Bigarrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a Bigarray *)
  | Pbigarraydim of int
  (* load/set 16,32,64,128 bits from a string: (unsafe)*)
  | Pstring_load_16 of { unsafe : bool; index_kind : array_index_kind }
  | Pstring_load_32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pstring_load_f32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pstring_load_64 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pstring_load_128 of
      { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed: bool }
  | Pbytes_load_16 of { unsafe : bool; index_kind : array_index_kind }
  | Pbytes_load_32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbytes_load_f32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbytes_load_64 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbytes_load_128 of
      { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbytes_set_16 of { unsafe : bool; index_kind : array_index_kind }
  | Pbytes_set_32 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbytes_set_f32 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbytes_set_64 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbytes_set_128 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  (* load/set 16,32,64,128 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of { unsafe : bool; index_kind : array_index_kind }
  | Pbigstring_load_32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbigstring_load_f32 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbigstring_load_64 of { unsafe : bool; index_kind : array_index_kind;
      mode : locality_mode; boxed : bool }
  | Pbigstring_load_128 of { aligned : bool; unsafe : bool;
      index_kind : array_index_kind; mode : locality_mode; boxed : bool }
  | Pbigstring_set_16 of { unsafe : bool; index_kind : array_index_kind }
  | Pbigstring_set_32 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbigstring_set_f32 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbigstring_set_64 of { unsafe : bool; index_kind : array_index_kind;
      boxed : bool }
  | Pbigstring_set_128 of { aligned : bool; unsafe : bool;
      index_kind : array_index_kind; boxed : bool }
  (* load/set SIMD vectors in GC-managed arrays *)
  | Pfloatarray_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Pfloat_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Pint_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Punboxed_float_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Punboxed_float32_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Punboxed_int32_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Punboxed_int64_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Punboxed_nativeint_array_load_128 of { unsafe : bool; mode : locality_mode; boxed : bool }
  | Pfloatarray_set_128 of { unsafe : bool; boxed : bool }
  | Pfloat_array_set_128 of { unsafe : bool; boxed : bool }
  | Pint_array_set_128 of { unsafe : bool; boxed : bool }
  | Punboxed_float_array_set_128 of { unsafe : bool; boxed : bool }
  | Punboxed_float32_array_set_128 of { unsafe : bool; boxed : bool }
  | Punboxed_int32_array_set_128 of { unsafe : bool; boxed : bool }
  | Punboxed_int64_array_set_128 of { unsafe : bool; boxed : bool }
  | Punboxed_nativeint_array_set_128 of { unsafe : bool; boxed : bool }
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer * locality_mode
  (* Integer to external pointer *)
  | Pint_as_pointer of locality_mode
  (* Atomic operations *)
  | Patomic_load of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_set of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_exchange of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_compare_exchange of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_compare_set of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_fetch_add
  | Patomic_add
  | Patomic_sub
  | Patomic_land
  | Patomic_lor
  | Patomic_lxor
  (* Inhibition of optimisation *)
  | Popaque of layout
  (* Statically-defined probes *)
  | Pprobe_is_enabled of { name: string }
  (* Primitives for [Obj] *)
  | Pobj_dup
  | Pobj_magic of layout
  | Punbox_float of boxed_float
  | Pbox_float of boxed_float * locality_mode
  | Punbox_int of boxed_integer
  | Pbox_int of boxed_integer * locality_mode
  | Punbox_vector of boxed_vector
  | Pbox_vector of boxed_vector * locality_mode
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Preinterpret_tagged_int63_as_unboxed_int64
  (* Jane Street extensions *)
  | Parray_to_iarray
  | Parray_of_iarray
  | Pget_header of locality_mode
  | Ppeek of peek_or_poke
  | Ppoke of peek_or_poke
  (* Fetching domain-local state *)
  | Pdls_get
  (* Poll for runtime actions *)
  | Ppoll
  | Pget_idx of (layout * Asttypes.mutable_flag)
  | Pset_idx of layout

and extern_repr =
  | Same_as_ocaml_repr of Jkind.Sort.Const.t
  | Unboxed_float of boxed_float
  | Unboxed_vector of boxed_vector
  | Unboxed_integer of boxed_integer
  | Untagged_int

and external_call_description = extern_repr Primitive.description_gen

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and nullable =
  | Nullable
  | Non_nullable

and value_kind =
  (* CR vlaviron: find a better name for raw_kind *)
  { raw_kind : value_kind_non_null;
    nullable : nullable;
  }

and value_kind_non_null =
  | Pgenval
  | Pintval
  | Pboxedfloatval of boxed_float
  | Pboxedintval of boxed_integer
  | Pvariant of {
      consts : int list;
      non_consts : (int * constructor_shape) list;
    }
  | Parrayval of array_kind
  | Pboxedvectorval of boxed_vector

and layout =
  | Ptop
  | Pvalue of value_kind
  | Punboxed_float of unboxed_float
  | Punboxed_int of unboxed_integer
  | Punboxed_vector of unboxed_vector
  | Punboxed_product of layout list
  | Pbottom

and block_shape =
  value_kind list option

and 'a mixed_block_element =
  | Value of value_kind
  | Float_boxed of 'a
  | Float64
  | Float32
  | Bits32
  | Bits64
  | Vec128
  | Word
  | Product of 'a mixed_block_element array

and mixed_block_shape = unit mixed_block_element array

and mixed_block_shape_with_locality_mode
  = locality_mode mixed_block_element array

and constructor_shape =
  | Constructor_uniform of value_kind list
  | Constructor_mixed of mixed_block_shape

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray of unboxed_float
  | Punboxedintarray of unboxed_integer
  | Punboxedvectorarray of unboxed_vector
  | Pgcscannableproductarray of scannable_product_element_kind list
  | Pgcignorableproductarray of ignorable_product_element_kind list

and array_ref_kind =
  | Pgenarray_ref of locality_mode
  | Paddrarray_ref
  | Pintarray_ref
  | Pfloatarray_ref of locality_mode
  | Punboxedfloatarray_ref of unboxed_float
  | Punboxedintarray_ref of unboxed_integer
  | Punboxedvectorarray_ref of unboxed_vector
  | Pgcscannableproductarray_ref of scannable_product_element_kind list
  | Pgcignorableproductarray_ref of ignorable_product_element_kind list

and array_set_kind =
  | Pgenarray_set of modify_mode
  | Paddrarray_set of modify_mode
  | Pintarray_set
  | Pfloatarray_set
  | Punboxedfloatarray_set of unboxed_float
  | Punboxedintarray_set of unboxed_integer
  | Punboxedvectorarray_set of unboxed_vector
  | Pgcscannableproductarray_set of
      modify_mode * scannable_product_element_kind list
  | Pgcignorableproductarray_set of ignorable_product_element_kind list

and ignorable_product_element_kind =
  | Pint_ignorable
  | Punboxedfloat_ignorable of unboxed_float
  | Punboxedint_ignorable of unboxed_integer
  | Pproduct_ignorable of ignorable_product_element_kind list

and scannable_product_element_kind =
  | Pint_scannable
  | Paddr_scannable
  | Pproduct_scannable of scannable_product_element_kind list

and array_index_kind =
  | Ptagged_int_index
  | Punboxed_int_index of unboxed_integer

and unboxed_float = Primitive.unboxed_float =
  | Unboxed_float64
  | Unboxed_float32

and unboxed_integer = Primitive.unboxed_integer =
  | Unboxed_int64
  | Unboxed_nativeint
  | Unboxed_int32

and unboxed_vector = Primitive.unboxed_vector =
  | Unboxed_vec128

and boxed_float = Primitive.boxed_float =
  | Boxed_float64
  | Boxed_float32

and boxed_integer = Primitive.boxed_integer =
  | Boxed_int64
  | Boxed_nativeint
  | Boxed_int32

and boxed_vector = Primitive.boxed_vector =
  | Boxed_vec128

and peek_or_poke =
  | Ppp_tagged_immediate
  | Ppp_unboxed_float32
  | Ppp_unboxed_float
  | Ppp_unboxed_int32
  | Ppp_unboxed_int64
  | Ppp_unboxed_nativeint

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float16
  | Pbigarray_float32 | Pbigarray_float32_t
  | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

let generic_value =
  { raw_kind = Pgenval;
    nullable = Nullable;
  }

let print_boxed_vector ppf t =
  match t with
  | Boxed_vec128 -> Format.pp_print_string ppf "Vec128"

let equal_nullable x y =
  match x, y with
  | Nullable, Nullable
  | Non_nullable, Non_nullable -> true
  | Nullable, Non_nullable
  | Non_nullable, Nullable -> false

let rec equal_value_kind_non_null x y =
  match x, y with
  | Pgenval, Pgenval -> true
  | Pboxedfloatval f1, Pboxedfloatval f2 -> Primitive.equal_boxed_float f1 f2
  | Pboxedintval bi1, Pboxedintval bi2 -> Primitive.equal_boxed_integer bi1 bi2
  | Pboxedvectorval v1, Pboxedvectorval v2 -> Primitive.equal_boxed_vector v1 v2
  | Pintval, Pintval -> true
  | Parrayval elt_kind1, Parrayval elt_kind2 -> elt_kind1 = elt_kind2
  | Pvariant { consts = consts1; non_consts = non_consts1; },
    Pvariant { consts = consts2; non_consts = non_consts2; } ->
    let consts1 = List.sort Int.compare consts1 in
    let consts2 = List.sort Int.compare consts2 in
    let compare_by_tag (tag1, _) (tag2, _) = Int.compare tag1 tag2 in
    let non_consts1 = List.sort compare_by_tag non_consts1 in
    let non_consts2 = List.sort compare_by_tag non_consts2 in
    List.equal Int.equal consts1 consts2
      && List.equal (fun (tag1, cstr1) (tag2, cstr2) ->
             Int.equal tag1 tag2
             && equal_constructor_shape cstr1 cstr2)
           non_consts1 non_consts2
  | (Pgenval | Pboxedfloatval _ | Pboxedintval _ | Pintval | Pvariant _
      | Parrayval _ | Pboxedvectorval _), _ -> false

and equal_value_kind x y =
  equal_value_kind_non_null x.raw_kind y.raw_kind
  && equal_nullable x.nullable y.nullable

and equal_mixed_block_element :
  type p.
    (p -> p -> bool) -> p mixed_block_element -> p mixed_block_element
    -> bool =
  fun eq_param m1 m2 ->
  match m1, m2 with
  | Value v1, Value v2 -> equal_value_kind v1 v2
  | Float_boxed param1, Float_boxed param2 -> eq_param param1 param2
  | Float64, Float64
  | Float32, Float32
  | Bits32, Bits32
  | Bits64, Bits64
  | Vec128, Vec128
  | Word, Word -> true
  | Product es1, Product es2 ->
    Misc.Stdlib.Array.equal (equal_mixed_block_element eq_param)
      es1 es2
  | (Value _ | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Vec128
     | Word | Product _), _ -> false

and equal_mixed_block_shape shape1 shape2 =
  Misc.Stdlib.Array.equal (equal_mixed_block_element Unit.equal) shape1 shape2

and equal_constructor_shape x y =
  match x, y with
  | Constructor_uniform fields1, Constructor_uniform fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all2 equal_value_kind fields1 fields2
  | Constructor_mixed shape1, Constructor_mixed shape2 ->
      equal_mixed_block_shape shape1 shape2
  | (Constructor_uniform _ | Constructor_mixed _), _ -> false

let equal_layout x y =
  match x, y with
  | Pvalue x, Pvalue y -> equal_value_kind x y
  | Ptop, Ptop -> true
  | Pbottom, Pbottom -> true
  | _, _ -> false

let rec compatible_layout x y =
  match x, y with
  | Pbottom, _
  | _, Pbottom -> true
  | Pvalue _, Pvalue _ -> true
  | Punboxed_float f1, Punboxed_float f2 -> Primitive.equal_unboxed_float f1 f2
  | Punboxed_int bi1, Punboxed_int bi2 -> Primitive.equal_unboxed_integer bi1 bi2
  | Punboxed_vector bi1, Punboxed_vector bi2 -> Primitive.equal_unboxed_vector bi1 bi2
  | Punboxed_product layouts1, Punboxed_product layouts2 ->
      List.compare_lengths layouts1 layouts2 = 0
      && List.for_all2 compatible_layout layouts1 layouts2
  | Ptop, Ptop -> true
  | Ptop, _ | _, Ptop -> false
  | (Pvalue _ | Punboxed_float _ | Punboxed_int _ | Punboxed_vector _ |
     Punboxed_product _), _ ->
      false

let rec equal_ignorable_product_element_kind k1 k2 =
  match k1, k2 with
  | Pint_ignorable, Pint_ignorable -> true
  | Punboxedfloat_ignorable f1, Punboxedfloat_ignorable f2 ->
    Primitive.equal_unboxed_float f1 f2
  | Punboxedint_ignorable i1, Punboxedint_ignorable i2 ->
    Primitive.equal_unboxed_integer i1 i2
  | Pproduct_ignorable p1, Pproduct_ignorable p2 ->
    List.equal equal_ignorable_product_element_kind p1 p2
  | ( Pint_ignorable | Punboxedfloat_ignorable _
    | Punboxedint_ignorable _ | Pproduct_ignorable _), _ -> false

let must_be_value layout =
  match layout with
  | Pvalue v -> v
  | Pbottom ->
      (* Here, we want to get the [value_kind] corresponding to
         a [Pbottom] layout. Anything will do, we return [Pgenval]
         as a default. *)
      generic_value
  | _ -> Misc.fatal_error "Layout is not a value"

type structured_constant =
    Const_base of constant
  | Const_block of int * structured_constant list
  | Const_mixed_block of int * mixed_block_shape * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
  | Const_float_block of string list
  | Const_null

type tailcall_attribute =
  | Tailcall_expectation of bool
    (* [@tailcall] and [@tailcall true] have [true],
       [@tailcall false] has [false] *)
  | Default_tailcall (* no [@tailcall] attribute *)

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Available_inline (* [@inline available] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

type inlined_attribute =
  | Always_inlined (* [@inlined] or [@inlined always] *)
  | Never_inlined (* [@inlined never] *)
  | Hint_inlined (* [@inlined hint] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inlined (* no [@inlined] attribute *)

let equal_inline_attribute (x : inline_attribute) (y : inline_attribute) =
  match x, y with
  | Always_inline, Always_inline
  | Never_inline, Never_inline
  | Available_inline, Available_inline
  | Default_inline, Default_inline
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inline | Never_inline
    | Available_inline | Unroll _ | Default_inline), _ ->
    false

let equal_inlined_attribute (x : inlined_attribute) (y : inlined_attribute) =
  match x, y with
  | Always_inlined, Always_inlined
  | Never_inlined, Never_inlined
  | Hint_inlined, Hint_inlined
  | Default_inlined, Default_inlined
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inlined | Never_inlined
    | Hint_inlined | Unroll _ | Default_inlined), _ ->
    false

type probe_desc = { name: string; enabled_at_init: bool; }
type probe = probe_desc option

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

let equal_specialise_attribute x y =
  match x, y with
  | Always_specialise, Always_specialise
  | Never_specialise, Never_specialise
  | Default_specialise, Default_specialise ->
    true
  | (Always_specialise | Never_specialise | Default_specialise), _ ->
    false

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type poll_attribute =
  | Error_poll (* [@poll error] *)
  | Default_poll (* no [@poll] attribute *)

type zero_alloc_attribute =
  | Default_zero_alloc
  | Check of { strict: bool;
               loc: Location.t;
               custom_error_msg: string option;
             }
  | Assume of { strict: bool;
                never_returns_normally: bool;
                never_raises: bool;
                loc: Location.t;
              }

type loop_attribute =
  | Always_loop (* [@loop] or [@loop always] *)
  | Never_loop (* [@loop never] *)
  | Default_loop (* no [@loop] attribute *)

type curried_function_kind = { nlocal : int } [@@unboxed]

type function_kind = Curried of curried_function_kind | Tupled

type let_kind = Strict | Alias | StrictOpt

type unique_barrier =
  | May_be_pushed_down
  | Must_stay_here

let add_barrier_to_read ubr sem =
  match ubr with
  | May_be_pushed_down -> sem
  (* CR uniqueness: We lose some performance here since flambda2 does not
     perform certain optimizations on mutable reads. We should consider adding
     a third option between Reads_agree and Reads_vary that selectively enables
     those optimizations that are sound for reads from unique allocations. *)
  | Must_stay_here -> Reads_vary

let add_barrier_to_let_kind ubr str =
  match ubr, str with
  | May_be_pushed_down, str -> str
  | Must_stay_here, Strict -> Strict
  (* CR uniqueness: We lose some performance here since the new
     pattern-matching code in 5.3 looks at the binding_kind to determine whether
     an allocation is mutable or not. See [Matching.mut_of_binding_kind].
     This can cause the analysis to re-match on unique data. However, we ensure
     in the uniqueness analysis that guards can not change unique data during
     pattern-matching. This means that the rematches in 5.3 are unnecessary for
     unique data and it would be nice to avoid them. *)
  | Must_stay_here, (Alias|StrictOpt) -> StrictOpt

type meth_kind = Self | Public | Cached

let equal_meth_kind x y =
  match x, y with
  | Self, Self -> true
  | Public, Public -> true
  | Cached, Cached -> true
  | (Self | Public | Cached), _ -> false

type shared_code = (int * int) list

type static_label = int

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  zero_alloc : zero_alloc_attribute;
  poll: poll_attribute;
  loop: loop_attribute;
  is_a_functor: bool;
  is_opaque: bool;
  stub: bool;
  tmc_candidate: bool;
  may_fuse_arity: bool;
  unbox_return: bool;
}

type scoped_location = Debuginfo.Scoped_location.t

type parameter_attribute = {
  unbox_param: bool;
}

type lparam = {
  name : Ident.t;
  layout : layout;
  attributes : parameter_attribute;
  mode : locality_mode
}

type pop_region =
  | Popped_region
  | Same_region

type lambda =
    Lvar of Ident.t
  | Lmutvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * layout * Ident.t * lambda * lambda
  | Lmutlet of layout * Ident.t * lambda * lambda
  | Lletrec of rec_binding list * lambda
  | Lprim of primitive * lambda list * scoped_location
  | Lswitch of lambda * lambda_switch * scoped_location * layout
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * scoped_location * layout
  | Lstaticraise of static_label * lambda list
  | Lstaticcatch of
      lambda * (static_label * (Ident.t * layout) list) * lambda
      * pop_region * layout
  | Ltrywith of lambda * Ident.t * lambda * layout
  | Lifthenelse of lambda * lambda * lambda * layout
  | Lsequence of lambda * lambda
  | Lwhile of lambda_while
  | Lfor of lambda_for
  | Lassign of Ident.t * lambda
  | Lsend of
      meth_kind * lambda * lambda * lambda list
      * region_close * locality_mode * scoped_location * layout
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda
  | Lregion of lambda * layout
  | Lexclave of lambda

and rec_binding = {
  id : Ident.t;
  def : lfunction;
}

and lfunction =
  { kind: function_kind;
    params: lparam list;
    return: layout;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc: scoped_location;
    mode: locality_mode;
    ret_mode: locality_mode;
  }

and lambda_while =
  { wh_cond : lambda;
    wh_body : lambda;
  }

and lambda_for =
  { for_id : Ident.t;
    for_loc : scoped_location;
    for_from : lambda;
    for_to : lambda;
    for_dir : direction_flag;
    for_body : lambda;
  }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_result_layout : layout;
    ap_region_close : region_close;
    ap_mode : locality_mode;
    ap_loc : scoped_location;
    ap_tailcall : tailcall_attribute;
    ap_inlined : inlined_attribute;
    ap_specialised : specialise_attribute;
    ap_probe : probe;
  }

and lambda_switch =
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_failaction : lambda option}

and lambda_event =
  { lev_loc: scoped_location;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.t }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo

type runtime_param =
  | Rp_argument_block of Global_module.t
  | Rp_main_module_block of Global_module.t
  | Rp_unit

type main_module_block_format =
  | Mb_struct of { mb_size : int }
  | Mb_instantiating_functor of
      { mb_runtime_params : runtime_param list;
        mb_returned_size : int;
      }

let main_module_block_size format =
  match format with
  | Mb_struct { mb_size } -> mb_size
  | Mb_instantiating_functor _ -> 1

type program =
  { compilation_unit : Compilation_unit.t;
    main_module_block_format : main_module_block_format;
    arg_block_idx : int option;
    required_globals : Compilation_unit.Set.t;
    code : lambda }

type arg_descr =
  { arg_param: Global_module.Name.t;
    arg_block_idx: int; }

let const_int n = Const_base (Const_int n)

let const_unit = const_int 0

let dummy_constant = Lconst (const_int (0xBBBB / 2))

let max_arity () =
  if !Clflags.native_code then 126 else max_int
  (* 126 = 127 (the maximal number of parameters supported in C--)
           - 1 (the hidden parameter containing the environment) *)

let lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode =
  assert (List.length params > 0);
  assert (List.length params <= max_arity ());
  (* A curried function type with n parameters has n arrows. Of these,
     the first [n-nlocal] have return mode Heap, while the remainder
     have return mode Local, except possibly the final one.

     That is, after supplying the first [n-nlocal] arguments, further
     partial applications must be locally allocated.

     A curried function with no local parameters or returns has kind
     [Curried {nlocal=0}]. *)
  begin match mode, kind with
  | Alloc_heap, Tupled -> ()
  | Alloc_local, Tupled ->
     (* Tupled optimisation does not apply to local functions *)
     assert false
  | mode, Curried {nlocal} ->
     let nparams = List.length params in
     assert (0 <= nlocal);
     assert (nlocal <= nparams);
     if is_local_mode ret_mode then assert (nlocal >= 1);
     if is_local_mode mode then assert (nlocal = nparams)
  end;
  { kind; params; return; body; attr; loc; mode; ret_mode }

let lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode =
  Lfunction (lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode)

let lambda_unit = Lconst const_unit

let of_bool = function
  | true -> Lconst (const_int 1)
  | false -> Lconst (const_int 0)

(* CR vlaviron: review the following cases *)
let non_null_value raw_kind =
  Pvalue { raw_kind; nullable = Non_nullable }
let nullable_value raw_kind =
  Pvalue { raw_kind; nullable = Nullable }

let layout_unit = non_null_value Pintval
let layout_int = non_null_value Pintval
let layout_array kind = non_null_value (Parrayval kind)
let layout_block = non_null_value Pgenval
let layout_list =
  non_null_value
    (Pvariant
       { consts = [0];
         non_consts =
           [0,
            Constructor_uniform
              [generic_value;
               { generic_value with nullable = Non_nullable}]] })
let layout_tuple_element = nullable_value Pgenval
let layout_value_field = nullable_value Pgenval
let layout_tmc_field = nullable_value Pgenval
let layout_optional_arg = non_null_value Pgenval
let layout_variant_arg = nullable_value Pgenval
let layout_exception = non_null_value Pgenval
let layout_function = non_null_value Pgenval
let layout_object = non_null_value Pgenval
let layout_class = non_null_value Pgenval
let layout_module = non_null_value Pgenval
let layout_module_field = nullable_value Pgenval
let layout_functor = non_null_value Pgenval
let layout_boxed_float f = non_null_value (Pboxedfloatval f)
let layout_unboxed_float f = Punboxed_float f
let layout_unboxed_nativeint = Punboxed_int Unboxed_nativeint
let layout_unboxed_int32 = Punboxed_int Unboxed_int32
let layout_unboxed_int64 = Punboxed_int Unboxed_int64
let layout_string = non_null_value Pgenval
let layout_unboxed_int ubi = Punboxed_int ubi
let layout_boxed_int bi = non_null_value (Pboxedintval bi)
let layout_unboxed_vector v = Punboxed_vector v
let layout_boxed_vector v =  non_null_value (Pboxedvectorval v)

let layout_lazy = nullable_value Pgenval
let layout_lazy_contents = nullable_value Pgenval
let layout_any_value = nullable_value Pgenval
let layout_letrec = layout_any_value
let layout_probe_arg = nullable_value Pgenval
let layout_unboxed_product layouts = Punboxed_product layouts

(* CR ncourant: use [Ptop] or remove this as soon as possible. *)
let layout_top = layout_any_value
let layout_bottom = Pbottom

let default_function_attribute = {
  inline = Default_inline;
  specialise = Default_specialise;
  local = Default_local;
  zero_alloc = Default_zero_alloc ;
  poll = Default_poll;
  loop = Default_loop;
  is_a_functor = false;
  is_opaque = false;
  stub = false;
  tmc_candidate = false;
  (* Plain functions ([fun] and [function]) set [may_fuse_arity] to [false] so
     that runtime arity matches syntactic arity in more situations.

     Many things compile to functions without having a notion of syntactic arity
     that survives typechecking, e.g. functors. Multi-arg functors are compiled
     as nested unary functions, and rely on the arity fusion in simplif to make
     them multi-argument. So, we keep arity fusion turned on by default for now.
  *)
  may_fuse_arity = true;
  unbox_return = false;
}

let default_stub_attribute =
  { default_function_attribute with stub = true; zero_alloc = Default_zero_alloc }

let default_param_attribute = { unbox_param = false }

(* Build sharing keys *)
(*
   Those keys are later compared with Stdlib.compare.
   For that reason, they should not include cycles.
*)

let max_raw = 32

let make_key e =
  let exception Not_simple in
  let count = ref 0   (* Used for controlling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e with
    | Lvar id
    | Lmutvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply ap ->
        Lapply {ap with ap_func = tr_rec env ap.ap_func;
                        ap_args = tr_recs env ap.ap_args;
                        ap_loc = Loc_unknown}
    | Llet (Alias,_k,x,ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt),_k,x,ex,Lvar v) when Ident.same v x ->
        tr_rec env ex
    | Llet (str,k,x,ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        Llet (str,k,y,ex,tr_rec (Ident.add x (Lvar y) env) e)
    | Lmutlet (k,x,ex,e) ->
        let ex = tr_rec env ex in
        let y = make_key x in
        Lmutlet (k,y,ex,tr_rec (Ident.add x (Lmutvar y) env) e)
    | Lprim (p,es,_) ->
        Lprim (p,tr_recs env es, Loc_unknown)
    | Lswitch (e,sw,loc,kind) ->
        Lswitch (tr_rec env e,tr_sw env sw,loc,kind)
    | Lstringswitch (e,sw,d,_,kind) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s,e) -> s,tr_rec env e) sw,
           tr_opt env d,
          Loc_unknown,kind)
    | Lstaticraise (i,es) ->
        Lstaticraise (i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2, r, kind) ->
        Lstaticcatch (tr_rec env e1,xs,tr_rec env e2, r, kind)
    | Ltrywith (e1,x,e2,kind) ->
        Ltrywith (tr_rec env e1,x,tr_rec env e2,kind)
    | Lifthenelse (cond,ifso,ifnot,kind) ->
        Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot,kind)
    | Lsequence (e1,e2) ->
        Lsequence (tr_rec env e1,tr_rec env e2)
    | Lassign (x,e) ->
        Lassign (x,tr_rec env e)
    | Lsend (m,e1,e2,es,pos,mo,_loc,layout) ->
        Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,pos,mo,Loc_unknown,layout)
    | Lifused (id,e) -> Lifused (id,tr_rec env e)
    | Lregion (e,layout) -> Lregion (tr_rec env e,layout)
    | Lexclave e -> Lexclave (tr_rec env e)
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg layout fn =
  match arg with
    Lvar id -> fn id
  | _ ->
      let id = Ident.create_local "let" in
      Llet(strict, layout, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar _ as arg, _) :: rem ->
      name_list (arg :: names) rem
  | (arg, layout) :: rem ->
      let id = Ident.create_local "let" in
      Llet(Strict, layout, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let shallow_iter ~tail ~non_tail:f = function
    Lvar _
  | Lmutvar _
  | Lconst _ -> ()
  | Lapply{ap_func = fn; ap_args = args} ->
      f fn; List.iter f args
  | Lfunction{body} ->
      f body
  | Llet(_, _k, _id, arg, body)
  | Lmutlet(_k, _id, arg, body) ->
      f arg; tail body
  | Lletrec(decl, body) ->
      tail body;
      List.iter (fun { def } -> f (Lfunction def)) decl
  | Lprim (Psequand, [l1; l2], _)
  | Lprim (Psequor, [l1; l2], _) ->
      f l1;
      tail l2
  | Lprim(_p, args, _loc) ->
      List.iter f args
  | Lswitch(arg, sw,_,_) ->
      f arg;
      List.iter (fun (_key, case) -> tail case) sw.sw_consts;
      List.iter (fun (_key, case) -> tail case) sw.sw_blocks;
      iter_opt tail sw.sw_failaction
  | Lstringswitch (arg,cases,default,_,_) ->
      f arg ;
      List.iter (fun (_,act) -> tail act) cases ;
      iter_opt tail default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, _, e2, _, _kind) ->
      tail e1; tail e2
  | Ltrywith(e1, _, e2,_) ->
      f e1; tail e2
  | Lifthenelse(e1, e2, e3,_) ->
      f e1; tail e2; tail e3
  | Lsequence(e1, e2) ->
      f e1; tail e2
  | Lwhile {wh_cond; wh_body} ->
      f wh_cond; f wh_body
  | Lfor {for_from; for_to; for_body} ->
      f for_from; f for_to; f for_body
  | Lassign(_, e) ->
      f e
  | Lsend (_k, met, obj, args, _, _, _, _) ->
      List.iter f (met::obj::args)
  | Levent (e, _evt) ->
      tail e
  | Lifused (_v, e) ->
      tail e
  | Lregion (e, _) ->
      f e
  | Lexclave e ->
      tail e

let iter_head_constructor f l =
  shallow_iter ~tail:f ~non_tail:f l

let rec free_variables = function
  | Lvar id
  | Lmutvar id -> Ident.Set.singleton id
  | Lconst _ -> Ident.Set.empty
  | Lapply{ap_func = fn; ap_args = args} ->
      free_variables_list (free_variables fn) args
  | Lfunction{body; params} ->
      Ident.Set.diff (free_variables body)
        (Ident.Set.of_list (List.map (fun p -> p.name) params))
  | Llet(_, _k, id, arg, body)
  | Lmutlet(_k, id, arg, body) ->
      Ident.Set.union
        (free_variables arg)
        (Ident.Set.remove id (free_variables body))
  | Lletrec(decl, body) ->
      let set =
        free_variables_list (free_variables body)
          (List.map (fun { def } -> Lfunction def) decl)
      in
      Ident.Set.diff set
        (Ident.Set.of_list (List.map (fun { id } -> id) decl))
  | Lprim(_p, args, _loc) ->
      free_variables_list Ident.Set.empty args
  | Lswitch(arg, sw,_,_) ->
      let set =
        free_variables_list
          (free_variables_list (free_variables arg)
             (List.map snd sw.sw_consts))
          (List.map snd sw.sw_blocks)
      in
      begin match sw.sw_failaction with
      | None -> set
      | Some failaction -> Ident.Set.union set (free_variables failaction)
      end
  | Lstringswitch (arg,cases,default,_,_) ->
      let set =
        free_variables_list (free_variables arg)
          (List.map snd cases)
      in
      begin match default with
      | None -> set
      | Some default -> Ident.Set.union set (free_variables default)
      end
  | Lstaticraise (_,args) ->
      free_variables_list Ident.Set.empty args
  | Lstaticcatch(body, (_, params), handler, _, _kind) ->
      Ident.Set.union
        (Ident.Set.diff
           (free_variables handler)
           (Ident.Set.of_list (List.map fst params)))
        (free_variables body)
  | Ltrywith(body, param, handler, _) ->
      Ident.Set.union
        (Ident.Set.remove
           param
           (free_variables handler))
        (free_variables body)
  | Lifthenelse(e1, e2, e3, _) ->
      Ident.Set.union
        (Ident.Set.union (free_variables e1) (free_variables e2))
        (free_variables e3)
  | Lsequence(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lwhile {wh_cond; wh_body} ->
      Ident.Set.union (free_variables wh_cond) (free_variables wh_body)
  | Lfor {for_id; for_from; for_to; for_body} ->
      Ident.Set.union (free_variables for_from)
        (Ident.Set.union (free_variables for_to)
           (Ident.Set.remove for_id (free_variables for_body)))
  | Lassign(id, e) ->
      Ident.Set.add id (free_variables e)
  | Lsend (_k, met, obj, args, _, _, _, _) ->
      free_variables_list
        (Ident.Set.union (free_variables met) (free_variables obj))
        args
  | Levent (lam, _evt) ->
      free_variables lam
  | Lifused (_v, e) ->
      (* Shouldn't v be considered a free variable ? *)
      free_variables e
  | Lregion (e, _) ->
      free_variables e
  | Lexclave e ->
      free_variables e

and free_variables_list set exprs =
  List.fold_left (fun set expr -> Ident.Set.union (free_variables expr) set)
    set exprs

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0,[])

let rec is_guarded = function
  | Lifthenelse(_cond, _body, Lstaticraise (0,[]),_) -> true
  | Llet(_str, _k, _id, _lam, body) -> is_guarded body
  | Levent(lam, _ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0,[]), kind) ->
      Lifthenelse (cond, body, patch, kind)
  | Llet(str, k, id, lam, body) ->
      Llet (str, k, id, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_address loc = function
  | Env.Aunit cu -> Lprim(Pgetglobal cu, [], loc)
  | Env.Alocal id ->
      if Ident.is_predef id
      then Lprim (Pgetpredef id, [], loc)
      else Lvar id
  | Env.Adot(addr, pos) ->
      Lprim(Pfield(pos, Pointer, Reads_agree),
                   [transl_address loc addr], loc)

let transl_path find loc env path =
  match find path env with
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))
  | addr -> transl_address loc addr

(* Translation of identifiers *)

let transl_module_path loc env path =
  transl_path Env.find_module_address loc env path

let transl_value_path loc env path =
  transl_path Env.find_value_address loc env path

let transl_extension_path loc env path =
  transl_path Env.find_constructor_address loc env path

let transl_class_path loc env path =
  transl_path Env.find_class_address loc env path

let transl_prim mod_name name =
  let pers = Ident.create_persistent mod_name in
  let env = Env.add_persistent_structure pers Env.empty in
  let lid = Longident.Ldot (Longident.Lident mod_name, name) in
  match Env.find_value_by_name lid env with
  | path, _ -> transl_value_path Loc_unknown env path
  | exception Not_found ->
      fatal_error ("Primitive " ^ name ^ " not found.")

(* XXX is it still useful? *)
let rec transl_mixed_product_shape ~get_value_kind shape =
  Array.mapi (fun i (elt : Types.mixed_block_element) ->
    match elt with
    | Value -> Value (get_value_kind i)
    | Float_boxed -> Float_boxed ()
    | Float64 -> Float64
    | Float32 -> Float32
    | Bits32 -> Bits32
    | Bits64 -> Bits64
    | Vec128 -> Vec128
    | Word -> Word
    | Product shapes ->
      let get_value_kind _ = generic_value in
      Product (transl_mixed_product_shape ~get_value_kind shapes)
  ) shape

let rec transl_mixed_product_shape_for_read ~get_value_kind ~get_mode shape =
  Array.mapi (fun i (elt : Types.mixed_block_element) ->
    match elt with
    | Value -> Value (get_value_kind i)
    | Float_boxed -> Float_boxed (get_mode i)
    | Float64 -> Float64
    | Float32 -> Float32
    | Bits32 -> Bits32
    | Bits64 -> Bits64
    | Vec128 -> Vec128
    | Word -> Word
    | Product shapes ->
      let get_value_kind _ = generic_value in
      Product (transl_mixed_product_shape_for_read ~get_value_kind ~get_mode shapes)
  ) shape

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

type substitution_functions = {
  subst_lambda : lambda -> lambda;
  subst_lfunction : lfunction -> lfunction;
}

let build_substs update_env ?(freshen_bound_variables = false) s =
  (* [s] contains a partial substitution for the free variables of the
     input term.

     During our traversal of the term we maintain a second environment
     [l] with all the bound variables of the input term in the current
     scope, mapped to either themselves or freshened versions of
     themselves when [freshen_bound_variables] is set. *)
  let bind id l =
    let id' = if not freshen_bound_variables then id else Ident.rename id in
    id', Ident.Map.add id id' l
  in
  let bind_many ids l =
    List.fold_right (fun (id, rhs) (ids', l) ->
        let id', l = bind id l in
        ((id', rhs) :: ids' , l)
      ) ids ([], l)
  in
  let bind_params params l =
    List.fold_right (fun p (params', l) ->
        let name', l = bind p.name l in
        ({ p with name = name' } :: params' , l)
      ) params ([], l)
  in
  let bind_rec ids l =
    List.fold_right (fun rb (ids', l) ->
        let id', l = bind rb.id l in
        ({ rb with id = id' } :: ids' , l)
      ) ids ([], l)
  in
  let rec subst s l lam =
    match lam with
    | Lvar id as lam ->
        begin match Ident.Map.find id l with
          | id' -> Lvar id'
          | exception Not_found ->
             (* note: as this point we know [id] is not a bound
                variable of the input term, otherwise it would belong
                to [l]; it is a free variable of the input term. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lmutvar id as lam ->
       begin match Ident.Map.find id l with
          | id' -> Lmutvar id'
          | exception Not_found ->
             (* Note: a mutable [id] should not appear in [s].
                Keeping the behavior of Lvar case for now. *)
             begin try Ident.Map.find id s with Not_found -> lam end
        end
    | Lconst _ as l -> l
    | Lapply ap ->
        Lapply{ap with ap_func = subst s l ap.ap_func;
                      ap_args = subst_list s l ap.ap_args}
    | Lfunction lf ->
        Lfunction (subst_lfun s l lf)
    | Llet(str, k, id, arg, body) ->
        let id, l' = bind id l in
        Llet(str, k, id, subst s l arg, subst s l' body)
    | Lmutlet(k, id, arg, body) ->
        let id, l' = bind id l in
        Lmutlet(k, id, subst s l arg, subst s l' body)
    | Lletrec(decl, body) ->
        let decl, l' = bind_rec decl l in
        Lletrec(List.map (subst_decl s l') decl, subst s l' body)
    | Lprim(p, args, loc) -> Lprim(p, subst_list s l args, loc)
    | Lswitch(arg, sw, loc,kind) ->
        Lswitch(subst s l arg,
                {sw with sw_consts = List.map (subst_case s l) sw.sw_consts;
                        sw_blocks = List.map (subst_case s l) sw.sw_blocks;
                        sw_failaction = subst_opt s l sw.sw_failaction; },
                loc,kind)
    | Lstringswitch (arg,cases,default,loc,kind) ->
        Lstringswitch
          (subst s l arg,
           List.map (subst_strcase s l) cases,
           subst_opt s l default,
           loc,kind)
    | Lstaticraise (i,args) ->  Lstaticraise (i, subst_list s l args)
    | Lstaticcatch(body, (id, params), handler, r, kind) ->
        let params, l' = bind_many params l in
        Lstaticcatch(subst s l body, (id, params),
                     subst s l' handler, r, kind)
    | Ltrywith(body, exn, handler,kind) ->
        let exn, l' = bind exn l in
        Ltrywith(subst s l body, exn, subst s l' handler,kind)
    | Lifthenelse(e1, e2, e3,kind) ->
        Lifthenelse(subst s l e1, subst s l e2, subst s l e3,kind)
    | Lsequence(e1, e2) -> Lsequence(subst s l e1, subst s l e2)
    | Lwhile lw -> Lwhile { wh_cond = subst s l lw.wh_cond;
                            wh_body = subst s l lw.wh_body}
    | Lfor lf ->
        let for_id, l' = bind lf.for_id l in
        Lfor {lf with for_id;
                      for_from = subst s l lf.for_from;
                      for_to = subst s l lf.for_to;
                      for_body = subst s l' lf.for_body}
    | Lassign(id, e) ->
        assert (not (Ident.Map.mem id s));
        let id = try Ident.Map.find id l with Not_found -> id in
        Lassign(id, subst s l e)
    | Lsend (k, met, obj, args, pos, mode, loc, layout) ->
        Lsend (k, subst s l met, subst s l obj, subst_list s l args,
               pos, mode, loc, layout)
    | Levent (lam, evt) ->
        let old_env = evt.lev_env in
        let env_updates =
          let find_in_old id =
            (* Looking up [id] might encounter locks, which we shouldn't apply
               as we are not using the values. But adding the value to [new_env]
               with the unlocked mode is just wrong. Therefore, we set the mode
               to be [max] for conservative soundness. [new_env] is only used
               for printing in debugger. *)
            let vd = Env.find_value (Path.Pident id) old_env in
            let vd = {vd with val_modalities = Mode.Modality.Value.id} in
            let mode = Mode.Value.max |> Mode.Value.disallow_right in
            (vd, mode)
          in
          let rebind id id' new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | (vd, mode) -> Env.add_value_lazy ~mode id' vd new_env
          in
          let update_free id new_env =
            match find_in_old id with
            | exception Not_found -> new_env
            | vd_mode -> update_env id vd_mode new_env
          in
          Ident.Map.merge (fun id bound free ->
            match bound, free with
            | Some id', _ ->
                if Ident.equal id id' then None else Some (rebind id id')
            | None, Some _ -> Some (update_free id)
            | None, None -> None
          ) l s
        in
        let new_env =
          Ident.Map.fold (fun _id update env -> update env) env_updates old_env
        in
        Levent (subst s l lam, { evt with lev_env = new_env })
    | Lifused (id, e) ->
        let id = try Ident.Map.find id l with Not_found -> id in
        Lifused (id, subst s l e)
    | Lregion (e, layout) ->
        Lregion (subst s l e, layout)
    | Lexclave e ->
        Lexclave (subst s l e)
  and subst_list s l li = List.map (subst s l) li
  and subst_decl s l decl = { decl with def = subst_lfun s l decl.def }
  and subst_lfun s l lf =
    let params, l' = bind_params lf.params l in
    { lf with params; body = subst s l' lf.body }
  and subst_case s l (key, case) = (key, subst s l case)
  and subst_strcase s l (key, case) = (key, subst s l case)
  and subst_opt s l = function
    | None -> None
    | Some e -> Some (subst s l e)
  in
  { subst_lambda = (fun lam -> subst s Ident.Map.empty lam);
    subst_lfunction = (fun lfun -> subst_lfun s Ident.Map.empty lfun);
  }

let subst update_env ?freshen_bound_variables s =
  (build_substs update_env ?freshen_bound_variables s).subst_lambda

let rename idmap lam =
  let update_env oldid (vd, mode) env =
    let newid = Ident.Map.find oldid idmap in
    Env.add_value_lazy ~mode newid vd env
  in
  let s = Ident.Map.map (fun new_id -> Lvar new_id) idmap in
  subst update_env s lam

let duplicate_function =
  (build_substs
     (fun _ _ env -> env)
     ~freshen_bound_variables:true
     Ident.Map.empty).subst_lfunction

let map_lfunction f { kind; params; return; body; attr; loc;
                      mode; ret_mode } =
  let body = f body in
  { kind; params; return; body; attr; loc; mode; ret_mode }

let shallow_map ~tail ~non_tail:f = function
  | Lvar _
  | Lmutvar _
  | Lconst _ as lam -> lam
  | Lapply { ap_func; ap_args; ap_result_layout; ap_region_close; ap_mode; ap_loc; ap_tailcall;
             ap_inlined; ap_specialised; ap_probe } ->
      Lapply {
        ap_func = f ap_func;
        ap_args = List.map f ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
        ap_probe;
      }
  | Lfunction lfun ->
      Lfunction (map_lfunction f lfun)
  | Llet (str, layout, v, e1, e2) ->
      Llet (str, layout, v, f e1, tail e2)
  | Lmutlet (layout, v, e1, e2) ->
      Lmutlet (layout, v, f e1, tail e2)
  | Lletrec (idel, e2) ->
      Lletrec
        (List.map (fun rb ->
             { rb with def = map_lfunction f rb.def })
            idel,
         tail e2)
  | Lprim (Psequand as p, [l1; l2], loc)
  | Lprim (Psequor as p, [l1; l2], loc) ->
      Lprim(p, [f l1; tail l2], loc)
  | Lprim (p, el, loc) ->
      Lprim (p, List.map f el, loc)
  | Lswitch (e, sw, loc, layout) ->
      Lswitch (f e,
               { sw_numconsts = sw.sw_numconsts;
                 sw_consts = List.map (fun (n, e) -> (n, tail e)) sw.sw_consts;
                 sw_numblocks = sw.sw_numblocks;
                 sw_blocks = List.map (fun (n, e) -> (n, tail e)) sw.sw_blocks;
                 sw_failaction = Option.map tail sw.sw_failaction;
               },
               loc, layout)
  | Lstringswitch (e, sw, default, loc, layout) ->
      Lstringswitch (
        f e,
        List.map (fun (s, e) -> (s, tail e)) sw,
        Option.map tail default,
        loc, layout)
  | Lstaticraise (i, args) ->
      Lstaticraise (i, List.map f args)
  | Lstaticcatch (body, id, handler, r, layout) ->
      Lstaticcatch (tail body, id, tail handler, r, layout)
  | Ltrywith (e1, v, e2, layout) ->
      Ltrywith (f e1, v, tail e2, layout)
  | Lifthenelse (e1, e2, e3, layout) ->
      Lifthenelse (f e1, tail e2, tail e3, layout)
  | Lsequence (e1, e2) ->
      Lsequence (f e1, tail e2)
  | Lwhile lw ->
      Lwhile { wh_cond = f lw.wh_cond;
               wh_body = f lw.wh_body }
  | Lfor lf ->
      Lfor { lf with for_from = f lf.for_from;
                     for_to = f lf.for_to;
                     for_body = f lf.for_body }
  | Lassign (v, e) ->
      Lassign (v, f e)
  | Lsend (k, m, o, el, pos, mode, loc, layout) ->
      Lsend (k, f m, f o, List.map f el, pos, mode, loc, layout)
  | Levent (l, ev) ->
      Levent (tail l, ev)
  | Lifused (v, e) ->
      Lifused (v, tail e)
  | Lregion (e, layout) ->
      Lregion (f e, layout)
  | Lexclave e ->
      Lexclave (tail e)

let map f =
  let rec g lam = f (shallow_map ~tail:g ~non_tail:g lam) in
  g

(* To let-bind expressions to variables *)

let bind_with_layout str (var, layout) exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, layout, var, exp, body)

let negate_integer_comparison = function
  | Ceq -> Cne
  | Cne -> Ceq
  | Clt -> Cge
  | Cle -> Cgt
  | Cgt -> Cle
  | Cge -> Clt

let swap_integer_comparison = function
  | Ceq -> Ceq
  | Cne -> Cne
  | Clt -> Cgt
  | Cle -> Cge
  | Cgt -> Clt
  | Cge -> Cle

let negate_float_comparison = function
  | CFeq -> CFneq
  | CFneq -> CFeq
  | CFlt -> CFnlt
  | CFnlt -> CFlt
  | CFgt -> CFngt
  | CFngt -> CFgt
  | CFle -> CFnle
  | CFnle -> CFle
  | CFge -> CFnge
  | CFnge -> CFge

let swap_float_comparison = function
  | CFeq -> CFeq
  | CFneq -> CFneq
  | CFlt -> CFgt
  | CFnlt -> CFngt
  | CFle -> CFge
  | CFnle -> CFnge
  | CFgt -> CFlt
  | CFngt -> CFnlt
  | CFge -> CFle
  | CFnge -> CFnle

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let merge_inline_attributes attr1 attr2 =
  match attr1, attr2 with
  | Default_inline, _ -> Some attr2
  | _, Default_inline -> Some attr1
  | _, _ ->
    if attr1 = attr2 then Some attr1
    else None

let max_arity () =
  if !Clflags.native_code then 126 else max_int
  (* 126 = 127 (the maximal number of parameters supported in C--)
           - 1 (the hidden parameter containing the environment) *)

let find_exact_application kind ~arity args =
  match kind with
  | Curried _ ->
      if arity <> List.length args
      then None
      else Some args
  | Tupled ->
      begin match args with
      | [Lprim(Pmakeblock _, tupled_args, _)] ->
          if arity <> List.length tupled_args
          then None
          else Some tupled_args
      | [Lconst(Const_block (_, const_args))] ->
          if arity <> List.length const_args
          then None
          else Some (List.map (fun cst -> Lconst cst) const_args)
      | _ -> None
      end

let reset () =
  raise_count := 0

let mod_field ?(read_semantics=Reads_agree) pos =
  Pfield (pos, Pointer, read_semantics)

let mod_setfield pos =
  Psetfield (pos, Pointer, Root_initialization)

let locality_mode_of_primitive_description (p : external_call_description) =
  if not Config.stack_allocation then
    if p.prim_alloc then Some alloc_heap else None
  else
    match p.prim_native_repr_res with
    | Prim_local, _ ->
      (* For primitives that might allocate locally, [p.prim_alloc] just says
         whether [caml_c_call] is required, without telling us anything
         about local allocation.  (However if [p.prim_alloc = false] we
         do actually know that the primitive does not allocate on the heap.) *)
      Some alloc_local
    | (Prim_global | Prim_poly), _ ->
      (* For primitives that definitely do not allocate locally,
         [p.prim_alloc = false] actually tells us that the primitive does
         not allocate at all.

         No external call that is [Prim_poly] may allocate locally.
      *)
      if p.prim_alloc then Some alloc_heap else None

(* Changes to this function may also require changes in Flambda 2 (e.g.
   closure_conversion.ml). *)
let primitive_may_allocate : primitive -> locality_mode option = function
  | Pbytes_to_string | Pbytes_of_string
  | Parray_to_iarray | Parray_of_iarray
  | Pignore -> None
  | Pgetglobal _ | Psetglobal _ | Pgetpredef _ -> None
  | Pmakeblock (_, _, _, m) -> Some m
  | Pmakefloatblock (_, m) -> Some m
  | Pmakeufloatblock (_, m) -> Some m
  | Pmakemixedblock (_, _, _, m) -> Some m
  | Pmakelazyblock _ -> Some alloc_heap
  | Pfield _ | Pfield_computed _ | Psetfield _ | Psetfield_computed _ -> None
  | Pfloatfield (_, _, m) -> Some m
  | Pufloatfield _ -> None
  | Pmixedfield ([field], shape, _) -> (
      if field < 0 || field >= Array.length shape then
        Misc.fatal_errorf "primitive_may_allocate: field index out of bounds \
          for Pmixedfield:@ %d" field;
      match shape.(field) with
      | Float_boxed mode -> Some mode
      | Value _
      | Float64
      | Float32
      | Bits32
      | Bits64
      | Vec128
      | Word
      | Product _ -> None
    )
  | Pmixedfield (_, _shape, _) -> assert false
  | Psetfloatfield _ -> None
  | Psetufloatfield _ -> None
  | Psetmixedfield _ -> None
  | Pduprecord _ -> Some alloc_heap
  | Pmake_unboxed_product _ | Punboxed_product_field _ -> None
  | Pccall p -> locality_mode_of_primitive_description p
  | Praise _ -> None
  | Psequor | Psequand | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _
  | Poffsetint _
  | Poffsetref _ -> None
  | Pintoffloat _ -> None
  | Pfloatofint (_, m) -> Some m
  | Pfloatoffloat32 m -> Some m
  | Pfloat32offloat m -> Some m
  | Pnegfloat (_, m) | Pabsfloat (_, m)
  | Paddfloat (_, m) | Psubfloat (_, m)
  | Pmulfloat (_, m) | Pdivfloat (_, m) -> Some m
  | Pfloatcomp (_, _) | Punboxed_float_comp (_, _) -> None
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets -> None
  | Pmakearray (_, _, m) -> Some m
  | Pmakearray_dynamic (_, m, _) -> Some m
  | Pduparray _ -> Some alloc_heap
  | Parraylength _ -> None
  | Parrayblit _
  | Parraysetu _ | Parraysets _
  | Parrayrefu ((Paddrarray_ref | Pintarray_ref
      | Punboxedfloatarray_ref _ | Punboxedintarray_ref _
      | Punboxedvectorarray_ref _
      | Pgcscannableproductarray_ref _
      | Pgcignorableproductarray_ref _), _, _)
  | Parrayrefs ((Paddrarray_ref | Pintarray_ref
      | Punboxedfloatarray_ref _ | Punboxedintarray_ref _
      | Punboxedvectorarray_ref _
      | Pgcscannableproductarray_ref _
      | Pgcignorableproductarray_ref _), _, _) -> None
  | Parrayrefu ((Pgenarray_ref m | Pfloatarray_ref m), _, _)
  | Parrayrefs ((Pgenarray_ref m | Pfloatarray_ref m), _, _) -> Some m
  | Pisint _ | Pisnull | Pisout -> None
  | Pintofbint _ -> None
  | Pbintofint (_,m)
  | Pcvtbint (_,_,m)
  | Pnegbint (_, m)
  | Paddbint (_, m)
  | Psubbint (_, m)
  | Pmulbint (_, m)
  | Pdivbint {mode=m}
  | Pmodbint {mode=m}
  | Pandbint (_, m)
  | Porbint (_, m)
  | Pxorbint (_, m)
  | Plslbint (_, m)
  | Plsrbint (_, m)
  | Pasrbint (_, m) -> Some m
  | Pbintcomp _ | Punboxed_int_comp _ -> None
  | Pbigarrayset _ | Pbigarraydim _ -> None
  | Pbigarrayref (_, _, _, _) ->
     (* Boxes arising from Bigarray access are always Alloc_heap *)
     Some alloc_heap
  | Pstring_load_16 _ | Pbytes_load_16 _ -> None
  | Pstring_load_32 { mode = m; boxed = true; _ }
  | Pbytes_load_32 { mode = m; boxed = true; _ }
  | Pstring_load_f32 { mode = m; boxed = true; _ }
  | Pbytes_load_f32 { mode = m; boxed = true; _ }
  | Pstring_load_64 { mode = m; boxed = true; _ }
  | Pbytes_load_64 { mode = m; boxed = true; _ }
  | Pstring_load_128 { mode = m; boxed = true; _ }
  | Pbytes_load_128 { mode = m; boxed = true; _ }
  | Pfloatarray_load_128 { mode = m; boxed = true; _ }
  | Pfloat_array_load_128 { mode = m; boxed = true; _ }
  | Pint_array_load_128 { mode = m; boxed = true; _ }
  | Punboxed_float_array_load_128 { mode = m; boxed = true; _ }
  | Punboxed_float32_array_load_128 { mode = m; boxed = true; _ }
  | Punboxed_int32_array_load_128 { mode = m; boxed = true; _ }
  | Punboxed_int64_array_load_128 { mode = m; boxed = true; _ }
  | Punboxed_nativeint_array_load_128 { mode = m; boxed = true; _ }
  | Pget_header m -> Some m
  | Pstring_load_32 { boxed = false; _ }
  | Pstring_load_f32 { boxed = false; _ }
  | Pstring_load_64 { boxed = false; _ }
  | Pstring_load_128 { boxed = false; _ }
  | Pbytes_load_32 { boxed = false; _ }
  | Pbytes_load_f32 { boxed = false; _ }
  | Pbytes_load_64 { boxed = false; _ }
  | Pbytes_load_128 { boxed = false; _ }
  | Pfloatarray_load_128 { boxed = false; _ }
  | Pfloat_array_load_128 { boxed = false; _ }
  | Pint_array_load_128 { boxed = false; _ }
  | Punboxed_float_array_load_128 { boxed = false; _ }
  | Punboxed_float32_array_load_128 { boxed = false; _ }
  | Punboxed_int32_array_load_128 { boxed = false; _ }
  | Punboxed_int64_array_load_128 { boxed = false; _ }
  | Punboxed_nativeint_array_load_128 { boxed = false; _ } -> None
  | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _
  | Pbytes_set_64 _ | Pbytes_set_128 _ -> None
  | Pbigstring_load_16 _ -> None
  | Pbigstring_load_32 { mode = m; boxed = true; _ }
  | Pbigstring_load_f32 { mode = m; boxed = true; _ }
  | Pbigstring_load_64 { mode = m; boxed = true; _ }
  | Pbigstring_load_128 { mode = m; boxed = true; _ } -> Some m
  | Pbigstring_load_32 { boxed = false; _ }
  | Pbigstring_load_f32 { boxed = false; _ }
  | Pbigstring_load_64 { boxed = false; _ }
  | Pbigstring_load_128 { boxed = false; _ } -> None
  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_f32 _
  | Pbigstring_set_64 _ | Pbigstring_set_128 _
  | Pfloatarray_set_128 _ | Pfloat_array_set_128 _ | Pint_array_set_128 _
  | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
  | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
  | Punboxed_nativeint_array_set_128 _ -> None
  | Pctconst _ -> None
  | Pbswap16 -> None
  | Pbbswap (_, m) -> Some m
  | Pint_as_pointer m -> Some m
  | Popaque _ -> None
  | Pprobe_is_enabled _ -> None
  | Pobj_dup -> Some alloc_heap
  | Pobj_magic _ -> None
  | Punbox_float _ | Punbox_int _ | Punbox_vector _ -> None
  | Pbox_float (_, m) | Pbox_int (_, m) | Pbox_vector (_, m) -> Some m
  | Prunstack | Presume | Pperform | Preperform
    (* CR mshinwell: check *)
  | Ppoll ->
    Some alloc_heap
  | Patomic_load _
  | Patomic_set _
  | Patomic_exchange _
  | Patomic_compare_exchange _
  | Patomic_compare_set _
  | Patomic_fetch_add
  | Patomic_add
  | Patomic_sub
  | Patomic_land
  | Patomic_lor
  | Patomic_lxor
  | Pdls_get
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Parray_element_size_in_bytes _
  | Pidx_field _
  | Pidx_mixed_field _
  | Pidx_array _
  | Pidx_deepen _
  | Pget_idx _ | Pset_idx _
  | Ppeek _ | Ppoke _ ->
    None
  | Preinterpret_tagged_int63_as_unboxed_int64 ->
    if !Clflags.native_code then None
    else
      (* We don't provide a locally-allocating version of this primitive
         since it would only apply to bytecode, and code requiring performance
         at a level where these primitives are necessary is very likely going
         to be native. *)
      Some alloc_heap

let primitive_can_raise prim =
  match prim with
  | Pccall _ | Praise _ | Parrayrefs _ | Parraysets _ | Pmodint _ | Pdivint _
  | Pstringrefs | Pbytesrefs | Pbytessets
  | Pstring_load_16 { unsafe = false; _ }
  | Pstring_load_32 { unsafe = false; _ }
  | Pstring_load_f32 { unsafe = false; _ }
  | Pstring_load_64 { unsafe = false; _ }
  | Pstring_load_128 { unsafe = false; _ }
  | Pbytes_load_16 { unsafe = false; _ }
  | Pbytes_load_32 { unsafe = false; _ }
  | Pbytes_load_f32 { unsafe = false; _ }
  | Pbytes_load_64 { unsafe = false; _ }
  | Pbytes_load_128 { unsafe = false; _ }
  | Pbytes_set_16 { unsafe = false; index_kind = _ }
  | Pbytes_set_32 { unsafe = false; index_kind = _; boxed = _ }
  | Pbytes_set_f32 { unsafe = false; index_kind = _; boxed = _ }
  | Pbytes_set_64 { unsafe = false; index_kind = _; boxed = _ }
  | Pbytes_set_128 { unsafe = false; _ }
  | Pbigstring_load_16 { unsafe = false; index_kind = _ }
  | Pbigstring_load_32 { unsafe = false; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_f32 { unsafe = false; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_64 { unsafe = false; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_128 { unsafe = false; _ }
  | Pbigstring_set_16 { unsafe = false; index_kind = _ }
  | Pbigstring_set_32 { unsafe = false; index_kind = _; boxed = _ }
  | Pbigstring_set_f32 { unsafe = false; index_kind = _; boxed = _ }
  | Pbigstring_set_64 { unsafe = false; index_kind = _; boxed = _ }
  | Pbigstring_set_128 { unsafe = false; _ }
  | Pfloatarray_load_128 { unsafe = false; _ }
  | Pfloat_array_load_128 { unsafe = false; _ }
  | Pint_array_load_128 { unsafe = false; _ }
  | Punboxed_float_array_load_128 { unsafe = false; _ }
  | Punboxed_float32_array_load_128 { unsafe = false; _ }
  | Punboxed_int32_array_load_128 { unsafe = false; _ }
  | Punboxed_int64_array_load_128 { unsafe = false; _ }
  | Punboxed_nativeint_array_load_128 { unsafe = false; _ }
  | Pfloatarray_set_128 { unsafe = false; _ }
  | Pfloat_array_set_128 { unsafe = false; _ }
  | Pint_array_set_128 { unsafe = false; _ }
  | Punboxed_float_array_set_128 { unsafe = false; _ }
  | Punboxed_float32_array_set_128 { unsafe = false; _ }
  | Punboxed_int32_array_set_128 { unsafe = false; _ }
  | Punboxed_int64_array_set_128 { unsafe = false; _ }
  | Punboxed_nativeint_array_set_128 { unsafe = false; _ }
  | Pdivbint { is_safe = Safe; _ }
  | Pmodbint { is_safe = Safe; _ }
  | Pbigarrayref (false, _, _, _)
  | Pbigarrayset (false, _, _, _)
  | Parrayblit _ | Pmakearray_dynamic _
  (* These bigarray primitives are translated into c-calls which may raise even
     if the unsafe flag is true *)
  | Pbigarrayref (_, _, Pbigarray_unknown, _)
  | Pbigarrayset (_, _, Pbigarray_unknown, _)
  | Pbigarrayref (_, _, _, Pbigarray_unknown_layout)
  | Pbigarrayset (_, _, _, Pbigarray_unknown_layout) ->
    true
  | Pbytes_to_string | Pbytes_of_string | Parray_of_iarray | Parray_to_iarray
  | Pignore | Pgetglobal _ | Psetglobal _ | Pgetpredef _ | Pmakeblock _
  | Pmakefloatblock _ | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Pmakeufloatblock _ | Pufloatfield _ | Psetufloatfield _ | Psequand | Psequor
  | Pmixedfield _ | Psetmixedfield _ | Pmakemixedblock _ | Pmakelazyblock _ | Pnot | Pnegint
  | Paddint | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _
  | Poffsetint _ | Poffsetref _ | Pintoffloat _
  | Pfloatofint (_, _)
  | Pfloatoffloat32 _ | Pfloat32offloat _
  | Pnegfloat (_, _)
  | Pabsfloat (_, _)
  | Paddfloat (_, _)
  | Psubfloat (_, _)
  | Pmulfloat (_, _)
  | Pdivfloat (_, _)
  | Pfloatcomp (_, _)
  | Punboxed_float_comp (_, _)
  | Pstringlength | Pstringrefu | Pbyteslength | Pbytesrefu | Pbytessetu
  | Pmakearray _ | Pduparray _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Pisint _ | Pisout | Pisnull | Pbintofint _ | Pintofbint _ | Pcvtbint _
  | Pnegbint _ | Paddbint _ | Psubbint _ | Pmulbint _
  | Pdivbint { is_safe = Unsafe; _ }
  | Pmodbint { is_safe = Unsafe; _ }
  | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
  | Pbintcomp _ | Punboxed_int_comp _ | Pbigarraydim _
  | Pbigarrayref
      ( true,
        _,
        ( Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float32_t
        | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
        | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
        | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
        | Pbigarray_complex32 | Pbigarray_complex64 ),
        _ )
  | Pbigarrayset
      ( true,
        _,
        ( Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float32_t
        | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
        | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
        | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
        | Pbigarray_complex32 | Pbigarray_complex64 ),
        (Pbigarray_c_layout | Pbigarray_fortran_layout) )
  | Pstring_load_16 { unsafe = true; _ }
  | Pstring_load_32 { unsafe = true; _ }
  | Pstring_load_f32 { unsafe = true; _ }
  | Pstring_load_64 { unsafe = true; _ }
  | Pstring_load_128 { unsafe = true; _ }
  | Pbytes_load_16 { unsafe = true; _ }
  | Pbytes_load_32 { unsafe = true; _ }
  | Pbytes_load_f32 { unsafe = true; _ }
  | Pbytes_load_64 { unsafe = true; _ }
  | Pbytes_load_128 { unsafe = true; _ }
  | Pbytes_set_16 { unsafe = true; index_kind = _ }
  | Pbytes_set_32 { unsafe = true; index_kind = _; boxed = _ }
  | Pbytes_set_f32 { unsafe = true; index_kind = _; boxed = _ }
  | Pbytes_set_64 { unsafe = true; index_kind = _; boxed = _ }
  | Pbytes_set_128 { unsafe = true; _ }
  | Pbigstring_load_16 { unsafe = true; index_kind = _ }
  | Pbigstring_load_32 { unsafe = true; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_f32 { unsafe = true; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_64 { unsafe = true; index_kind = _; mode = _; boxed = _ }
  | Pbigstring_load_128 { unsafe = true; _ }
  | Pbigstring_set_16 { unsafe = true; _ }
  | Pbigstring_set_32 { unsafe = true; index_kind = _; boxed = _ }
  | Pbigstring_set_f32 { unsafe = true; index_kind = _; boxed = _ }
  | Pbigstring_set_64 { unsafe = true; index_kind = _; boxed = _ }
  | Pbigstring_set_128 { unsafe = true; _ }
  | Pfloatarray_load_128 { unsafe = true; _ }
  | Pfloat_array_load_128 { unsafe = true; _ }
  | Pint_array_load_128 { unsafe = true; _ }
  | Punboxed_float_array_load_128 { unsafe = true; _ }
  | Punboxed_float32_array_load_128 { unsafe = true; _ }
  | Punboxed_int32_array_load_128 { unsafe = true; _ }
  | Punboxed_int64_array_load_128 { unsafe = true; _ }
  | Punboxed_nativeint_array_load_128 { unsafe = true; _ }
  | Pfloatarray_set_128 { unsafe = true; _ }
  | Pfloat_array_set_128 { unsafe = true; _ }
  | Pint_array_set_128 { unsafe = true; _ }
  | Punboxed_float_array_set_128 { unsafe = true; _ }
  | Punboxed_float32_array_set_128 { unsafe = true; _ }
  | Punboxed_int32_array_set_128 { unsafe = true; _ }
  | Punboxed_int64_array_set_128 { unsafe = true; _ }
  | Punboxed_nativeint_array_set_128 { unsafe = true; _ }
  | Pctconst _ | Pbswap16 | Pbbswap _ | Pint_as_pointer _ | Popaque _
  | Pprobe_is_enabled _ | Pobj_dup | Pobj_magic _
  | Pbox_float (_, _)
  | Punbox_float _
  | Pbox_vector (_, _)
  | Punbox_vector _ | Punbox_int _ | Pbox_int _ | Pmake_unboxed_product _
  | Punboxed_product_field _ | Pget_header _ ->
    false
  | Patomic_exchange _ | Patomic_compare_exchange _
  | Patomic_compare_set _ | Patomic_fetch_add | Patomic_add
  | Patomic_sub | Patomic_land | Patomic_lor
  | Patomic_lxor | Patomic_load _ | Patomic_set _ -> false
  | Prunstack | Pperform | Presume | Preperform -> true (* XXX! *)
  | Pdls_get | Ppoll | Preinterpret_tagged_int63_as_unboxed_int64
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Parray_element_size_in_bytes _
  | Pidx_field _ | Pidx_mixed_field _ | Pidx_array _ | Pidx_deepen _
  | Pget_idx _ | Pset_idx _
  | Ppeek _ | Ppoke _ ->
    false

let constant_layout: constant -> layout = function
  | Const_int _ | Const_char _ -> non_null_value Pintval
  | Const_string _ -> non_null_value Pgenval
  | Const_int32 _ -> non_null_value (Pboxedintval Boxed_int32)
  | Const_int64 _ -> non_null_value (Pboxedintval Boxed_int64)
  | Const_nativeint _ -> non_null_value (Pboxedintval Boxed_nativeint)
  | Const_unboxed_int32 _ -> Punboxed_int Unboxed_int32
  | Const_unboxed_int64 _ -> Punboxed_int Unboxed_int64
  | Const_unboxed_nativeint _ -> Punboxed_int Unboxed_nativeint
  | Const_float _ -> non_null_value (Pboxedfloatval Boxed_float64)
  | Const_float32 _ -> non_null_value (Pboxedfloatval Boxed_float32)
  | Const_unboxed_float _ -> Punboxed_float Unboxed_float64
  | Const_unboxed_float32 _ -> Punboxed_float Unboxed_float32

let structured_constant_layout = function
  | Const_base const -> constant_layout const
  | Const_mixed_block _ | Const_block _ | Const_immstring _ ->
    non_null_value Pgenval
  | Const_float_array _ | Const_float_block _ ->
    non_null_value (Parrayval Pfloatarray)
  | Const_null -> nullable_value Pgenval

let rec layout_of_const_sort (c : Jkind.Sort.Const.t) : layout =
  match c with
  | Base Value -> layout_any_value
  | Base Float64 -> layout_unboxed_float Unboxed_float64
  | Base Float32 -> layout_unboxed_float Unboxed_float32
  | Base Word -> layout_unboxed_nativeint
  | Base Bits32 -> layout_unboxed_int32
  | Base Bits64 -> layout_unboxed_int64
  | Base Vec128 -> layout_unboxed_vector Unboxed_vec128
  | Base Void -> assert false
  | Product sorts ->
    layout_unboxed_product (List.map layout_of_const_sort sorts)

let layout_of_extern_repr : extern_repr -> _ = function
  | Untagged_int ->  layout_int
  | Unboxed_vector v -> layout_boxed_vector v
  | Unboxed_float bf -> layout_boxed_float bf
  | Unboxed_integer bi -> layout_boxed_int bi
  | Same_as_ocaml_repr s -> layout_of_const_sort s

let rec layout_of_scannable_kinds kinds =
  Punboxed_product (List.map layout_of_scannable_kind kinds)

and layout_of_scannable_kind = function
  | Pint_scannable -> layout_int
  | Paddr_scannable -> layout_value_field
  | Pproduct_scannable kinds -> layout_of_scannable_kinds kinds

let rec layout_of_ignorable_kinds kinds =
  Punboxed_product (List.map layout_of_ignorable_kind kinds)

and layout_of_ignorable_kind = function
  | Pint_ignorable -> layout_int
  | Punboxedfloat_ignorable f -> layout_unboxed_float f
  | Punboxedint_ignorable i -> layout_unboxed_int i
  | Pproduct_ignorable kinds -> layout_of_ignorable_kinds kinds

let array_ref_kind_result_layout = function
  | Pintarray_ref -> layout_int
  | Pfloatarray_ref _ -> layout_boxed_float Boxed_float64
  | Punboxedfloatarray_ref bf -> layout_unboxed_float bf
  | Pgenarray_ref _ | Paddrarray_ref -> layout_value_field
  | Punboxedintarray_ref i -> layout_unboxed_int i
  | Punboxedvectorarray_ref bv -> layout_unboxed_vector bv
  | Pgcscannableproductarray_ref kinds -> layout_of_scannable_kinds kinds
  | Pgcignorableproductarray_ref kinds -> layout_of_ignorable_kinds kinds

let rec layout_of_mixed_block_element : 'a. 'a mixed_block_element -> layout =
  function
  | Value value_kind -> Pvalue value_kind
  | Float_boxed _ -> layout_boxed_float Boxed_float64
  | Float64 -> layout_unboxed_float Unboxed_float64
  | Float32 -> layout_unboxed_float Unboxed_float32
  | Bits32 -> layout_unboxed_int32
  | Bits64 -> layout_unboxed_int64
  | Word -> layout_unboxed_nativeint
  | Vec128 -> layout_unboxed_vector Unboxed_vec128
  | Product shape ->
    Punboxed_product
      (Array.to_list (Array.map layout_of_mixed_block_element shape))

let rec mixed_block_element_of_layout (layout : layout) :
    unit mixed_block_element =
  match layout with
  | Punboxed_product layouts ->
    Product (List.map mixed_block_element_of_layout layouts |> Array.of_list)
  | Ptop | Pbottom -> Misc.fatal_error "Pidxdeepen"
  | Pvalue value_kind -> Value value_kind
  | Punboxed_float Unboxed_float64 -> Float64
  | Punboxed_float Unboxed_float32 -> Float32
  | Punboxed_int Unboxed_int64 -> Bits64
  | Punboxed_int Unboxed_int32 -> Bits32
  | Punboxed_int Unboxed_nativeint -> Word
  | Punboxed_vector Unboxed_vec128 -> Vec128

let rec mixed_block_element_leaves (el : _ mixed_block_element)
  : _ mixed_block_element list =
  match el with
  | Product els ->
    List.concat_map mixed_block_element_leaves (Array.to_list els)
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Word
  | Vec128 ->
    [el]

type will_be_reordered_acc = { seen_flat : bool; last_value_after_flat : bool }
let will_be_reordered (mbe : _ mixed_block_element) =
  let acc =
    List.fold_left
      (fun acc el ->
        match el with
        | Product _ -> assert false
        | Value _ -> { acc with last_value_after_flat = acc.seen_flat }
        | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Word | Vec128 ->
          { acc with seen_flat = true })
      { seen_flat = false; last_value_after_flat = false }
      (mixed_block_element_leaves mbe)
  in
  acc.last_value_after_flat

let primitive_result_layout (p : primitive) =
  assert !Clflags.native_code;
  match p with
  | Popaque layout | Pobj_magic layout -> layout
  | Pbytes_to_string | Pbytes_of_string -> layout_string
  | Pignore | Psetfield _ | Psetfield_computed _ | Psetfloatfield _ | Poffsetref _
  | Psetufloatfield _ | Psetmixedfield _
  | Pbytessetu | Pbytessets | Parraysetu _ | Parraysets _ | Pbigarrayset _
  | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _
  | Pbytes_set_128 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_f32 _
  | Pbigstring_set_64 _ | Pbigstring_set_128 _
  | Pfloatarray_set_128 _ | Pfloat_array_set_128 _ | Pint_array_set_128 _
  | Punboxed_float_array_set_128 _ | Punboxed_float32_array_set_128 _
  | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
  | Punboxed_nativeint_array_set_128 _
  | Parrayblit _
    -> layout_unit
  | Pgetglobal _ | Psetglobal _ | Pgetpredef _ -> layout_module_field
  | Pmakeblock _ | Pmakefloatblock _ | Pmakearray _ | Pmakearray_dynamic _
  | Pduprecord _ | Pmakeufloatblock _ | Pmakemixedblock _ | Pmakelazyblock _
  | Pduparray _ | Pbigarraydim _ | Pobj_dup -> layout_block
  | Pfield _ | Pfield_computed _ -> layout_value_field
  | Punboxed_product_field (field, layouts) -> (Array.of_list layouts).(field)
  | Pmake_unboxed_product layouts -> layout_unboxed_product layouts
  | Parray_element_size_in_bytes _ -> layout_int
  | Pidx_field _ | Pidx_mixed_field _ | Pidx_array _ | Pidx_deepen _ ->
    Punboxed_int Unboxed_int64
  | Pfloatfield _ -> layout_boxed_float Boxed_float64
  | Pfloatoffloat32 _ -> layout_boxed_float Boxed_float64
  | Pfloat32offloat _ -> layout_boxed_float Boxed_float32
  | Pfloatofint (f, _) | Pnegfloat (f, _) | Pabsfloat (f, _)
  | Paddfloat (f, _) | Psubfloat (f, _) | Pmulfloat (f, _) | Pdivfloat (f, _)
  | Pbox_float (f, _) -> layout_boxed_float f
  | Pufloatfield _ -> Punboxed_float Unboxed_float64
  | Punbox_float f -> layout_unboxed_float (Primitive.unboxed_float f)
  | Pbox_vector (v, _) -> layout_boxed_vector v
  | Punbox_vector v -> layout_unboxed_vector (Primitive.unboxed_vector v)
  | Pmixedfield ([i], shape, _) -> layout_of_mixed_block_element shape.(i)
  | Pmixedfield (_, _shape, _) -> assert false
  | Pccall { prim_native_repr_res = _, repr_res } -> layout_of_extern_repr repr_res
  | Praise _ -> layout_bottom
  | Psequor | Psequand | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _
  | Poffsetint _ | Pintoffloat _
  | Pfloatcomp (_, _) | Punboxed_float_comp (_, _)
  | Pstringlength | Pstringrefu | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytesrefs
  | Parraylength _ | Pisint _ | Pisnull | Pisout | Pintofbint _
  | Pbintcomp _ | Punboxed_int_comp _
  | Pstring_load_16 _ | Pbytes_load_16 _ | Pbigstring_load_16 _
  | Pprobe_is_enabled _ | Pbswap16
    -> layout_int
  | Parrayrefu (array_ref_kind, _, _) | Parrayrefs (array_ref_kind, _, _) ->
    array_ref_kind_result_layout array_ref_kind
  | Pbintofint (bi, _) | Pcvtbint (_,bi,_)
  | Pnegbint (bi, _) | Paddbint (bi, _) | Psubbint (bi, _)
  | Pmulbint (bi, _) | Pdivbint {size = bi} | Pmodbint {size = bi}
  | Pandbint (bi, _) | Porbint (bi, _) | Pxorbint (bi, _)
  | Plslbint (bi, _) | Plsrbint (bi, _) | Pasrbint (bi, _)
  | Pbbswap (bi, _) | Pbox_int (bi, _) ->
      layout_boxed_int bi
  | Punbox_int bi -> Punboxed_int (Primitive.unboxed_integer bi)
  | Pstring_load_32 { boxed = true; _ } | Pbytes_load_32 { boxed = true; _ }
  | Pbigstring_load_32 { boxed = true; _ } ->
      layout_boxed_int Boxed_int32
  | Pstring_load_f32 { boxed = true; _ } | Pbytes_load_f32 { boxed = true; _ }
  | Pbigstring_load_f32 { boxed = true; _ } ->
      layout_boxed_float Boxed_float32
  | Pstring_load_64 { boxed = true; _ } | Pbytes_load_64 { boxed = true; _ }
  | Pbigstring_load_64 { boxed = true; _ } ->
      layout_boxed_int Boxed_int64
  | Pstring_load_128 { boxed = true; _ } | Pbytes_load_128 { boxed = true; _ }
  | Pbigstring_load_128 { boxed = true; _ } ->
      layout_boxed_vector Boxed_vec128
  | Pbigstring_load_32 { boxed = false; _ }
  | Pstring_load_32 { boxed = false; _ }
  | Pbytes_load_32 { boxed = false; _ } -> layout_unboxed_int Unboxed_int32
  | Pbigstring_load_f32 { boxed = false; _ }
  | Pstring_load_f32 { boxed = false; _ }
  | Pbytes_load_f32 { boxed = false; _ } -> layout_unboxed_float Unboxed_float32
  | Pbigstring_load_64 { boxed = false; _ }
  | Pstring_load_64 { boxed = false; _ }
  | Pbytes_load_64 { boxed = false; _ } -> layout_unboxed_int Unboxed_int64
  | Pstring_load_128 { boxed = false; _ } | Pbytes_load_128 { boxed = false; _ }
  | Pbigstring_load_128 { boxed = false; _ } ->
      layout_unboxed_vector Unboxed_vec128
  | Pfloatarray_load_128 { boxed = true; _ }
  | Pfloat_array_load_128 { boxed = true; _ }
  | Punboxed_float_array_load_128 { boxed = true; _ }
  | Punboxed_float32_array_load_128 { boxed = true; _ }
  | Pint_array_load_128 { boxed = true; _ }
  | Punboxed_int64_array_load_128 { boxed = true; _ }
  | Punboxed_nativeint_array_load_128 { boxed = true; _ }
  | Punboxed_int32_array_load_128 { boxed = true; _ } ->
      layout_boxed_vector Boxed_vec128
  | Pfloatarray_load_128 { boxed = false; _ }
  | Pfloat_array_load_128 { boxed = false; _ }
  | Punboxed_float_array_load_128 { boxed = false; _ }
  | Punboxed_float32_array_load_128 { boxed = false; _ }
  | Pint_array_load_128 { boxed = false; _ }
  | Punboxed_int64_array_load_128 { boxed = false; _ }
  | Punboxed_nativeint_array_load_128 { boxed = false; _ }
  | Punboxed_int32_array_load_128 { boxed = false; _ } ->
      layout_unboxed_vector Unboxed_vec128
  | Pbigarrayref (_, _, kind, _) ->
      begin match kind with
      | Pbigarray_unknown -> layout_any_value
      | Pbigarray_float16 | Pbigarray_float32 ->
        (* float32 bigarrays return 64-bit floats for backward compatibility.
           Likewise for float16. *)
        layout_boxed_float Boxed_float64
      | Pbigarray_float32_t -> layout_boxed_float Boxed_float32
      | Pbigarray_float64 -> layout_boxed_float Boxed_float64
      | Pbigarray_sint8 | Pbigarray_uint8
      | Pbigarray_sint16 | Pbigarray_uint16
      | Pbigarray_caml_int -> layout_int
      | Pbigarray_int32 -> layout_boxed_int Boxed_int32
      | Pbigarray_int64 -> layout_boxed_int Boxed_int64
      | Pbigarray_native_int -> layout_boxed_int Boxed_nativeint
      | Pbigarray_complex32 | Pbigarray_complex64 ->
          layout_block
      end
  | Pctconst (
      Big_endian | Word_size | Int_size | Max_wosize
      | Ostype_unix | Ostype_cygwin | Ostype_win32 | Backend_type | Runtime5
    ) ->
      (* Compile-time constants only ever return ints for now,
         enumerate them all to be sure to modify this if it becomes wrong. *)
      layout_int
  | Pint_as_pointer _ ->
      (* CR ncourant: use an unboxed int64 here when it exists *)
      layout_any_value
  | (Parray_to_iarray | Parray_of_iarray) -> layout_any_value
  | Pget_header _ -> layout_boxed_int Boxed_nativeint
  | Prunstack | Presume | Pperform | Preperform -> layout_any_value
  | Patomic_load { immediate_or_pointer = Immediate } -> layout_int
  | Patomic_load { immediate_or_pointer = Pointer } -> layout_any_value
  | Patomic_set _ -> layout_unit
  | Patomic_exchange { immediate_or_pointer = Immediate } -> layout_int
  | Patomic_exchange { immediate_or_pointer = Pointer } -> layout_any_value
  | Patomic_compare_exchange { immediate_or_pointer = Immediate } -> layout_int
  | Patomic_compare_exchange { immediate_or_pointer = Pointer } -> layout_any_value
  | Patomic_compare_set _
  | Patomic_fetch_add -> layout_int
  | Pdls_get -> layout_any_value
  | Patomic_add
  | Patomic_sub
  | Patomic_land
  | Patomic_lor
  | Patomic_lxor
  | Ppoll -> layout_unit
  | Preinterpret_tagged_int63_as_unboxed_int64 -> layout_unboxed_int64
  | Preinterpret_unboxed_int64_as_tagged_int63 -> layout_int
  | Ppeek layout -> (
      match layout with
      | Ppp_tagged_immediate -> layout_int
      | Ppp_unboxed_float32 -> layout_unboxed_float Unboxed_float32
      | Ppp_unboxed_float -> layout_unboxed_float Unboxed_float64
      | Ppp_unboxed_int32 -> layout_unboxed_int32
      | Ppp_unboxed_int64 -> layout_unboxed_int64
      | Ppp_unboxed_nativeint -> layout_unboxed_nativeint
    )
  | Ppoke _ -> layout_unit
  | Pget_idx (layout, _) -> layout
  | Pset_idx _ -> layout_unit

let compute_expr_layout free_vars_kind lam =
  let rec compute_expr_layout kinds = function
    | Lvar id | Lmutvar id -> begin
        try Ident.Map.find id kinds
        with Not_found ->
        match free_vars_kind id with
        | Some kind -> kind
        | None ->
            Misc.fatal_errorf "Unbound layout for variable %a" Ident.print id
      end
    | Lconst cst -> structured_constant_layout cst
    | Lfunction _ -> layout_function
    | Lapply { ap_result_layout; _ } -> ap_result_layout
    | Lsend (_, _, _, _, _, _, _, layout) -> layout
    | Llet(_, kind, id, _, body) | Lmutlet(kind, id, _, body) ->
      compute_expr_layout (Ident.Map.add id kind kinds) body
    | Lletrec(defs, body) ->
      let kinds =
        List.fold_left (fun kinds { id } -> Ident.Map.add id layout_letrec kinds)
          kinds defs
      in
      compute_expr_layout kinds body
    | Lprim(p, _, _) ->
      primitive_result_layout p
    | Lswitch(_, _, _, kind) | Lstringswitch(_, _, _, _, kind)
    | Lstaticcatch(_, _, _, _, kind) | Ltrywith(_, _, _, kind)
    | Lifthenelse(_, _, _, kind) | Lregion (_, kind) ->
      kind
    | Lstaticraise (_, _) ->
      layout_bottom
    | Lsequence(_, body) | Levent(body, _) -> compute_expr_layout kinds body
    | Lwhile _ | Lfor _ | Lassign _ -> layout_unit
    | Lifused _ ->
        assert false
    | Lexclave e -> compute_expr_layout kinds e
  in
  compute_expr_layout Ident.Map.empty lam

let array_ref_kind mode = function
  | Pgenarray -> Pgenarray_ref mode
  | Paddrarray -> Paddrarray_ref
  | Pintarray -> Pintarray_ref
  | Pfloatarray -> Pfloatarray_ref mode
  | Punboxedintarray int_kind -> Punboxedintarray_ref int_kind
  | Punboxedfloatarray float_kind -> Punboxedfloatarray_ref float_kind
  | Punboxedvectorarray vec_kind -> Punboxedvectorarray_ref vec_kind
  | Pgcscannableproductarray kinds -> Pgcscannableproductarray_ref kinds
  | Pgcignorableproductarray kinds -> Pgcignorableproductarray_ref kinds

let array_set_kind mode = function
  | Pgenarray -> Pgenarray_set mode
  | Paddrarray -> Paddrarray_set mode
  | Pintarray -> Pintarray_set
  | Pfloatarray -> Pfloatarray_set
  | Punboxedintarray int_kind -> Punboxedintarray_set int_kind
  | Punboxedfloatarray float_kind -> Punboxedfloatarray_set float_kind
  | Punboxedvectorarray vec_kind -> Punboxedvectorarray_set vec_kind
  | Pgcscannableproductarray kinds -> Pgcscannableproductarray_set (mode, kinds)
  | Pgcignorableproductarray kinds -> Pgcignorableproductarray_set kinds

let array_ref_kind_of_array_set_kind (kind : array_set_kind) mode
      : array_ref_kind =
  match kind with
  | Pintarray_set -> Pintarray_ref
  | Punboxedfloatarray_set uf -> Punboxedfloatarray_ref uf
  | Punboxedintarray_set ui -> Punboxedintarray_ref ui
  | Punboxedvectorarray_set uv -> Punboxedvectorarray_ref uv
  | Pgcscannableproductarray_set (_, scannables) ->
    Pgcscannableproductarray_ref scannables
  | Pgcignorableproductarray_set ignorables ->
    Pgcignorableproductarray_ref ignorables
  | Pgenarray_set _ -> Pgenarray_ref mode
  | Paddrarray_set _ -> Paddrarray_ref
  | Pfloatarray_set -> Pfloatarray_ref mode

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
       begin match primitive_may_allocate prim with
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
       iter_head_constructor loop lam
  in
  if not Config.stack_allocation then false
  else begin
    match loop lam with
    | () -> false
    | exception Exit -> true
  end

let simple_prim_on_values ~name ~arity ~alloc =
  Primitive.make
    ~name
    ~alloc
    ~c_builtin:false
    ~effects:Arbitrary_effects
    ~coeffects:Has_coeffects
    ~native_name:""
    ~native_repr_args:
      (Primitive.make_prim_repr_args arity
        (Primitive.Prim_global,Same_as_ocaml_repr Jkind.Sort.Const.value))
    ~native_repr_res:(Prim_global, Same_as_ocaml_repr Jkind.Sort.Const.value)
    ~is_layout_poly:false

let rec try_to_find_location lam =
  (* This is very much best-effort and may overshoot, but will still likely be
     better than nothing. *)
  match lam with
  | Lprim (_, _, loc)
  | Lfunction { loc; _ }
  | Lletrec ({ def = { loc; _ }; _ } :: _, _)
  | Lapply { ap_loc = loc; _ }
  | Lfor { for_loc = loc; _ }
  | Lswitch (_, _, loc, _)
  | Lstringswitch (_, _, _, loc, _)
  | Lsend (_, _, _, _, _, _, loc, _)
  | Levent (_, { lev_loc = loc; _ }) ->
    loc
  | Llet (_, _, _, lam, _)
  | Lmutlet (_, _, lam, _)
  | Lifthenelse (lam, _, _, _)
  | Lstaticcatch (lam, _, _, _, _)
  | Lstaticraise (_, lam :: _)
  | Lwhile { wh_cond = lam; _ }
  | Lsequence (lam, _)
  | Lassign (_, lam)
  | Lifused (_, lam)
  | Lregion (lam, _)
  | Lexclave lam
  | Ltrywith (lam, _, _, _) ->
    try_to_find_location lam
  | Lvar _ | Lmutvar _ | Lconst _ | Lletrec _ | Lstaticraise (_, []) ->
    Debuginfo.Scoped_location.Loc_unknown

let try_to_find_debuginfo lam =
  Debuginfo.from_location (try_to_find_location lam)

(* The "count_initializers_*" functions count the number of individual
   components in an initializer for the corresponding array kind _after_
   unarization.  These are used to implement the "%array_element_size_in_bytes"
   primitives for products, as each such component takes a full word in product
   arrays. *)
let rec count_initializers_scannable
      (scannable : scannable_product_element_kind) =
  match scannable with
  | Pint_scannable | Paddr_scannable -> 1
  | Pproduct_scannable scannables ->
    List.fold_left
      (fun acc scannable -> acc + count_initializers_scannable scannable)
      0 scannables

let rec count_initializers_ignorable
    (ignorable : ignorable_product_element_kind) =
  match ignorable with
  | Pint_ignorable | Punboxedfloat_ignorable _ | Punboxedint_ignorable _ -> 1
  | Pproduct_ignorable ignorables ->
    List.fold_left
      (fun acc ignorable -> acc + count_initializers_ignorable ignorable)
      0 ignorables

let count_initializers_array_kind (lambda_array_kind : array_kind) =
  match lambda_array_kind with
  | Pgenarray | Paddrarray | Pintarray | Pfloatarray | Punboxedfloatarray _
  | Punboxedintarray _ | Punboxedvectorarray _ -> 1
  | Pgcscannableproductarray scannables ->
    List.fold_left
      (fun acc scannable -> acc + count_initializers_scannable scannable)
      0 scannables
  | Pgcignorableproductarray ignorables ->
    List.fold_left
      (fun acc ignorable -> acc + count_initializers_ignorable ignorable)
      0 ignorables

let rec ignorable_product_element_kind_involves_int
    (kind : ignorable_product_element_kind) =
  match kind with
  | Pint_ignorable -> true
  | Punboxedfloat_ignorable _ | Punboxedint_ignorable _ -> false
  | Pproduct_ignorable kinds ->
    List.exists ignorable_product_element_kind_involves_int kinds
