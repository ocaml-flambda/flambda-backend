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

type mutable_flag = Lambda.mutable_flag

type immediate_or_pointer = Lambda.immediate_or_pointer

type initialization_or_assignment = Lambda.initialization_or_assignment

type is_safe = Lambda.is_safe

type boxed =
  | Boxed
  | Unboxed

type memory_access_size =
  | Sixteen
  | Thirty_two
  | Sixty_four

type alloc_mode = Lambda.alloc_mode

type modify_mode = Lambda.modify_mode

type primitive =
  | Pread_symbol of string
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape * alloc_mode
  | Pmakeufloatblock of mutable_flag * alloc_mode
  | Pfield of int * layout * immediate_or_pointer * mutable_flag
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int * alloc_mode
  | Psetfloatfield of int * initialization_or_assignment
  | Pufloatfield of int
  | Psetufloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Context switches *)
  | Prunstack
  | Pperform
  | Presume
  | Preperform
  (* External call *)
  | Pccall of Primitive.description
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
  | Pcompare_ints | Pcompare_floats | Pcompare_bints of boxed_integer
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint of alloc_mode
  | Pnegfloat of alloc_mode | Pabsfloat of alloc_mode
  | Paddfloat of alloc_mode | Psubfloat of alloc_mode
  | Pmulfloat of alloc_mode | Pdivfloat of alloc_mode
  | Pfloatcomp of float_comparison
  | Punboxed_float_comp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag * alloc_mode
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_ref_kind
  | Parraysetu of array_set_kind
  | Parrayrefs of array_ref_kind
  | Parraysets of array_set_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer * alloc_mode
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
                * alloc_mode
  | Pnegbint of boxed_integer * alloc_mode
  | Paddbint of boxed_integer * alloc_mode
  | Psubbint of boxed_integer * alloc_mode
  | Pmulbint of boxed_integer * alloc_mode
  | Pdivbint of { size : boxed_integer; is_safe : is_safe; mode: alloc_mode }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe; mode: alloc_mode }
  | Pandbint of boxed_integer * alloc_mode
  | Porbint of boxed_integer * alloc_mode
  | Pxorbint of boxed_integer * alloc_mode
  | Plslbint of boxed_integer * alloc_mode
  | Plsrbint of boxed_integer * alloc_mode
  | Pasrbint of boxed_integer * alloc_mode
  | Pbintcomp of boxed_integer * integer_comparison
  | Punboxed_int_comp of boxed_integer * integer_comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load of (memory_access_size * is_safe * alloc_mode)
  | Pbytes_load of (memory_access_size * is_safe * alloc_mode)
  | Pbytes_set of (memory_access_size * is_safe)
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load of (memory_access_size * is_safe * alloc_mode)
  | Pbigstring_set of (memory_access_size * is_safe)
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer * alloc_mode
  (* Integer to external pointer *)
  | Pint_as_pointer of alloc_mode
  (* Atomic operations *)
  | Patomic_load of {immediate_or_pointer : immediate_or_pointer}
  | Patomic_exchange
  | Patomic_cas
  | Patomic_fetch_add
  (* Inhibition of optimisation *)
  | Popaque
  (* Probes *)
  | Pprobe_is_enabled of { name : string }
  | Punbox_float
  | Pbox_float of alloc_mode
  | Punbox_int of boxed_integer
  | Pbox_int of boxed_integer * alloc_mode
  | Pget_header of alloc_mode
  (* Fetch domain-local state *)
  | Pdls_get

and integer_comparison = Lambda.integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison = Lambda.float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and array_kind = Lambda.array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and array_ref_kind = Lambda.array_ref_kind =
  | Pgenarray_ref of alloc_mode
  | Paddrarray_ref
  | Pintarray_ref
  | Pfloatarray_ref of alloc_mode

and array_set_kind = Lambda.array_set_kind =
  | Pgenarray_set of modify_mode
  | Paddrarray_set of modify_mode
  | Pintarray_set
  | Pfloatarray_set

and value_kind = Lambda.value_kind =
  (* CR mshinwell: Pfloatval should be renamed to Pboxedfloatval *)
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval
  | Pvariant of {
      consts : int list;
      non_consts : (int * value_kind list) list;
    }
  | Parrayval of array_kind
  | Pboxedvectorval of boxed_vector

and layout = Lambda.layout =
  | Ptop
  | Pvalue of value_kind
  | Punboxed_float
  | Punboxed_int of boxed_integer
  | Punboxed_vector of boxed_vector
  | Punboxed_product of layout list
  | Pbottom

and block_shape = Lambda.block_shape

and boxed_integer = Lambda.boxed_integer =
    Pnativeint | Pint32 | Pint64

and vec128_type = Lambda.vec128_type =
  | Unknown128
  | Int8x16
  | Int16x8
  | Int32x4
  | Int64x2
  | Float32x4
  | Float64x2

and boxed_vector = Lambda.boxed_vector =
  | Pvec128 of vec128_type

and bigarray_kind = Lambda.bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout = Lambda.bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind = Lambda.raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

let equal (x: primitive) (y: primitive) = x = y

let result_layout (p : primitive) =
  match p with
  | Psetfield _ | Psetfield_computed _ | Psetfloatfield _ | Poffsetref _
  | Psetufloatfield _
  | Pbytessetu | Pbytessets | Parraysetu _ | Parraysets _ | Pbigarrayset _
    -> Lambda.layout_unit
  | Pmakeblock _ | Pmakearray _ | Pduprecord _
  | Pmakeufloatblock _
  | Pduparray _ | Pbigarraydim _ -> Lambda.layout_block
  | Pfield _ | Pfield_computed -> Lambda.layout_field
  | Pfloatfield _ | Pfloatofint _ | Pnegfloat _ | Pabsfloat _
  | Paddfloat _ | Psubfloat _ | Pmulfloat _ | Pdivfloat _
  | Pbox_float _ -> Lambda.layout_boxed_float
  | Pufloatfield _ | Punbox_float -> Punboxed_float
  | Pccall { prim_native_repr_res = _, repr_res } -> Lambda.layout_of_native_repr repr_res
  | Praise _ -> Lambda.layout_bottom
  | Psequor | Psequand | Pnot
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Poffsetint _ | Pintoffloat | Pfloatcomp _ | Punboxed_float_comp _
  | Pstringlength | Pstringrefu | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytesrefs
  | Parraylength _ | Pisint | Pisout | Pintofbint _
  | Pbintcomp _ | Punboxed_int_comp _
  | Pprobe_is_enabled _ | Pbswap16
    -> Lambda.layout_int
  | Parrayrefu array_ref_kind | Parrayrefs array_ref_kind ->
    Lambda.array_ref_kind_result_layout array_ref_kind
  | Pbintofint (bi, _) | Pcvtbint (_,bi,_)
  | Pnegbint (bi, _) | Paddbint (bi, _) | Psubbint (bi, _)
  | Pmulbint (bi, _) | Pdivbint {size = bi} | Pmodbint {size = bi}
  | Pandbint (bi, _) | Porbint (bi, _) | Pxorbint (bi, _)
  | Plslbint (bi, _) | Plsrbint (bi, _) | Pasrbint (bi, _)
  | Pbbswap (bi, _) | Pbox_int (bi, _) ->
      Lambda.layout_boxedint bi
  | Punbox_int bi -> Punboxed_int bi
  | Pbigarrayref (_, _, kind, _) ->
      begin match kind with
      | Pbigarray_unknown -> Lambda.layout_any_value
      | Pbigarray_float32 | Pbigarray_float64 -> Lambda.layout_boxed_float
      | Pbigarray_sint8 | Pbigarray_uint8
      | Pbigarray_sint16 | Pbigarray_uint16
      | Pbigarray_caml_int -> Lambda.layout_int
      | Pbigarray_int32 -> Lambda.layout_boxedint Pint32
      | Pbigarray_int64 -> Lambda.layout_boxedint Pint64
      | Pbigarray_native_int -> Lambda.layout_boxedint Pnativeint
      | Pbigarray_complex32 | Pbigarray_complex64 ->
          Lambda.layout_block
      end
  | Pint_as_pointer _ ->
    (* CR ncourant: use an unboxed int64 here when it exists *)
    Lambda.layout_any_value
  | Pget_header _ -> Lambda.layout_boxedint Pnativeint
  | Prunstack | Presume | Pperform | Preperform ->
    (* CR mshinwell/ncourant: to be thought about later *)
    Misc.fatal_error "Effects-related primitives are not yet supported"
  | Patomic_load { immediate_or_pointer = Immediate } -> Lambda.layout_int
  | Patomic_load { immediate_or_pointer = Pointer } -> Lambda.layout_any_value
  | Patomic_exchange
  | Patomic_cas
  | Patomic_fetch_add
  | Pdls_get
  | Popaque | Pread_symbol _
  | Pstring_load _ | Pbytes_load _ | Pbytes_set _ | Pbigstring_load _
  | Pbigstring_set _  -> Lambda.layout_any_value
