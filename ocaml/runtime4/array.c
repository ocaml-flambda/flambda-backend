/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Operations on arrays */
#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/eventlog.h"
#include "caml/custom.h"

static const mlsize_t mlsize_t_max = -1;

/* Unboxed arrays */

static int no_polymorphic_compare(value v1, value v2)
{
  caml_failwith("Polymorphic comparison is not permitted for unboxed arrays");
}

static intnat no_polymorphic_hash(value v)
{
  caml_failwith("Polymorphic hash is not permitted for unboxed arrays");
}

static void unboxed_array_serialize(value v, uintnat* bsize_32,
                                    uintnat* bsize_64)
{
  caml_failwith("Marshalling is not yet implemented for unboxed arrays");
}

static uintnat unboxed_array_deserialize(void* dst)
{
  caml_failwith("Marshalling is not yet implemented for unboxed arrays");
}

// Note: if polymorphic comparison and/or hashing are implemented for
// the int32 unboxed arrays, care needs to be taken with the last word
// when the array is of odd length -- this is not currently initialized.

CAMLexport struct custom_operations caml_unboxed_int32_array_ops[2] = {
  { "_unboxed_int32_even_array",
    custom_finalize_default,
    no_polymorphic_compare,
    no_polymorphic_hash,
    unboxed_array_serialize,
    unboxed_array_deserialize,
    custom_compare_ext_default,
    custom_fixed_length_default },
  { "_unboxed_int32_odd_array",
    custom_finalize_default,
    no_polymorphic_compare,
    no_polymorphic_hash,
    unboxed_array_serialize,
    unboxed_array_deserialize,
    custom_compare_ext_default,
    custom_fixed_length_default },
};

CAMLexport struct custom_operations caml_unboxed_int64_array_ops = {
  "_unboxed_int64_array",
  custom_finalize_default,
  no_polymorphic_compare,
  no_polymorphic_hash,
  unboxed_array_serialize,
  unboxed_array_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLexport struct custom_operations caml_unboxed_nativeint_array_ops = {
  "_unboxed_nativeint_array",
  custom_finalize_default,
  no_polymorphic_compare,
  no_polymorphic_hash,
  unboxed_array_serialize,
  unboxed_array_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* returns number of elements (either fields or floats) */
/* [ 'a array -> int ] */
CAMLexport mlsize_t caml_array_length(value array)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return Wosize_val(array) / Double_wosize;
  else
#endif
    return Wosize_val(array);
}

CAMLexport int caml_is_double_array(value array)
{
  return (Tag_val(array) == Double_array_tag);
}

/* Note: the OCaml types on the following primitives will work both with
   and without the -no-flat-float-array configure-time option. If you
   respect them, your C code should work in both configurations.
*/

/* [ 'a array -> int -> 'a ] where 'a != float */
CAMLprim value caml_array_get_addr(value array, value index)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  return Field(array, idx);
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  // [caml_floatarray_get] may be called on a floatarray
  // or a mixed block.
  CAMLassert (  Tag_val(array) == Double_array_tag
                || index > Scannable_wosize_val(array) );

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

/* [ floatarray -> int -> local_ float ] */
CAMLprim value caml_floatarray_get_local(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  // [caml_floatarray_get] may be called on a floatarray
  // or a mixed block.
  CAMLassert (  Tag_val(array) == Double_array_tag
                || index > Scannable_wosize_val(array) );

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  d = Double_flat_field(array, idx);
  res = caml_alloc_local(Double_wosize, Double_tag);
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_get(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_get_addr(array, index);
}

/* [ local_ 'a array -> int -> local_ 'a ] */
CAMLprim value caml_array_get_local(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_get_local(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_get_addr(array, index);
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
CAMLprim value caml_array_set_addr(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  caml_modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ local_ 'a array -> int -> local_ 'a -> unit ] where 'a != float

   Must be used carefully, as it can violate the "no forward pointers"
   restriction on the local stack. */
CAMLprim value caml_array_set_addr_local(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) caml_array_bound_error();
  caml_modify_local(array, idx, newval);
  return Val_unit;
}

/* [ floatarray -> int -> float -> unit ]
   [ local_ floatarray -> int -> local_ float -> unit ] */
CAMLprim value caml_floatarray_set(value array, value index, value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  CAMLassert (Tag_val(array) == Double_array_tag);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    caml_array_bound_error();
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_set_addr(array, index, newval);
}

/* [ local_ 'a array -> int -> local_ 'a -> unit ]

   Must be used carefully, as it can violate the "no forward pointers"
   restriction on the local stack if the array contains pointers (vs. [int]s or
   unboxed floats). */
CAMLprim value caml_array_set_local(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_set_addr_local(array, index, newval);
}

/* [ floatarray -> int -> float ] */
CAMLprim value caml_floatarray_unsafe_get(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  d = Double_flat_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

/* [ floatarray -> int -> local_ float ] */
CAMLprim value caml_floatarray_unsafe_get_local(value array, value index)
{
  intnat idx = Long_val(index);
  double d;
  value res;

  CAMLassert (Tag_val(array) == Double_array_tag);
  d = Double_flat_field(array, idx);
  res = caml_alloc_local(Double_wosize, Double_tag);
  Store_double_val(res, d);
  return res;
}

/* [ 'a array -> int -> 'a ] */
CAMLprim value caml_array_unsafe_get(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_get(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return Field(array, Long_val(index));
}

/* [ local_ 'a array -> int -> local_ 'a ] */
CAMLprim value caml_array_unsafe_get_local(value array, value index)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_get_local(array, index);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return Field(array, Long_val(index));
}

/* [ 'a array -> int -> 'a -> unit ] where 'a != float */
static value caml_array_unsafe_set_addr(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  caml_modify(&Field(array, idx), newval);
  return Val_unit;
}

/* [ local_ 'a array -> int -> local_ 'a -> unit ] where 'a != float

   Must be used carefully, as it can violate the "no forward pointers"
   restriction on the local stack. */
static value caml_array_unsafe_set_addr_local(value array, value index,
                                              value newval)
{
  intnat idx = Long_val(index);
  caml_modify_local(array, idx, newval);
  return Val_unit;
}

/* [ floatarray -> int -> float -> unit ]
   [ local_ floatarray -> int -> local_ float -> unit ] */
CAMLprim value caml_floatarray_unsafe_set(value array, value index,value newval)
{
  intnat idx = Long_val(index);
  double d = Double_val (newval);
  Store_double_flat_field(array, idx, d);
  return Val_unit;
}

/* [ 'a array -> int -> 'a -> unit ] */
CAMLprim value caml_array_unsafe_set(value array, value index, value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_unsafe_set_addr(array, index, newval);
}

/* [ local_ 'a array -> int -> local_ 'a -> unit ]

   Must be used carefully, as it can violate the "no forward pointers"
   restriction on the local stack if the array contains pointers (vs. [int]s or
   unboxed floats). */
CAMLprim value caml_array_unsafe_set_local(value array, value index,
                                           value newval)
{
#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag)
    return caml_floatarray_unsafe_set(array, index, newval);
#else
  CAMLassert (Tag_val(array) != Double_array_tag);
#endif
  return caml_array_unsafe_set_addr_local(array, index, newval);
}

/* [len] is a [value] representing number of floats. */
/* [ int -> floatarray ] */
CAMLprim value caml_floatarray_create(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  value result;
  if (wosize <= Max_young_wosize){
    if (wosize == 0)
      return Atom(0);
    else
#define Setup_for_gc
#define Restore_after_gc
      Alloc_small (result, wosize, Double_array_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  }else if (wosize > Max_wosize)
    caml_invalid_argument("Float.Array.create");
  else {
    result = caml_alloc_shr (wosize, Double_array_tag);
  }
  // Give the GC a chance to run, and run memprof callbacks
  return caml_process_pending_actions_with_root (result);
}

CAMLprim value caml_floatarray_create_local(value len)
{
  mlsize_t wosize = Long_val(len) * Double_wosize;
  return caml_alloc_local (wosize, Double_array_tag);
}

/* [len] is a [value] representing number of words or floats */
static value make_vect_gen(value len, value init, int local)
{
  CAMLparam2 (len, init);
  CAMLlocal1 (res);
  mlsize_t size, i;

  size = Long_val(len);
  if (size == 0) {
    res = Atom(0);
#ifdef FLAT_FLOAT_ARRAY
  } else if (Is_block(init)
           && Is_in_value_area(init)
           && Tag_val(init) == Double_tag) {
    mlsize_t wsize;
    double d;
    d = Double_val(init);
    wsize = size * Double_wosize;
    if (wsize > Max_wosize) caml_invalid_argument("Array.make");
    res = local ?
      caml_alloc_local(wsize, Double_array_tag) :
      caml_alloc(wsize, Double_array_tag);
    for (i = 0; i < size; i++) {
      Store_double_flat_field(res, i, d);
    }
#endif
  } else {
    if (size > Max_wosize) caml_invalid_argument("Array.make");
    else if (local) {
      res = caml_alloc_local(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    } else if (size <= Max_young_wosize) {
      res = caml_alloc_small(size, 0);
      for (i = 0; i < size; i++) Field(res, i) = init;
    } else {
      if (Is_block(init) && Is_young(init)) {
        /* We don't want to create so many major-to-minor references,
           so [init] is moved to the major heap by doing a minor GC. */
        CAML_EV_COUNTER (EV_C_FORCE_MINOR_MAKE_VECT, 1);
        caml_minor_collection ();
      }
      CAMLassert(!(Is_block(init) && Is_young(init)));
      res = caml_alloc_shr(size, 0);
      /* We now know that [init] is not in the minor heap, so there is
         no need to call [caml_initialize]. */
      for (i = 0; i < size; i++) Field(res, i) = init;
    }
  }
  // Give the GC a chance to run, and run memprof callbacks
  if (!local) caml_process_pending_actions ();
  CAMLreturn (res);
}


CAMLprim value caml_make_vect(value len, value init)
{
  return make_vect_gen(len, init, 0);
}

CAMLprim value caml_make_local_vect(value len, value init)
{
  return make_vect_gen(len, init, 1);
}

/* [len] is a [value] representing number of floats */
/* [ int -> float array ] */
CAMLprim value caml_make_float_vect(value len)
{
#ifdef FLAT_FLOAT_ARRAY
  return caml_floatarray_create (len);
#else
  static value uninitialized_float = Val_unit;
  if (uninitialized_float == Val_unit){
    uninitialized_float = caml_alloc_shr (Double_wosize, Double_tag);
    caml_register_generational_global_root (&uninitialized_float);
  }
  return caml_make_vect (len, uninitialized_float);
#endif
}

CAMLprim value caml_make_unboxed_int32_vect(value len)
{
  /* This is only used on 64-bit targets. */

  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_wosize) caml_invalid_argument("Array.make");

  /* [num_fields] does not include the custom operations field. */
  mlsize_t num_fields = (num_elements + 1) / 2;

  return caml_alloc_custom(&caml_unboxed_int32_array_ops[num_elements % 2],
                           num_fields * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_int32_vect_bytecode(value len)
{
  return caml_make_vect(len, caml_copy_int32(0));
}

CAMLprim value caml_make_unboxed_int64_vect(value len)
{
  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_wosize) caml_invalid_argument("Array.make");

  struct custom_operations* ops = &caml_unboxed_int64_array_ops;

  return caml_alloc_custom(ops, num_elements * sizeof(value), 0, 0);
}


CAMLprim value caml_make_unboxed_int64_vect_bytecode(value len)
{
  return caml_make_vect(len, caml_copy_int64(0));
}

CAMLprim value caml_make_unboxed_nativeint_vect(value len)
{
  /* This is only used on 64-bit targets. */

  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_wosize) caml_invalid_argument("Array.make");

  struct custom_operations* ops = &caml_unboxed_nativeint_array_ops;

  return caml_alloc_custom(ops, num_elements * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_nativeint_vect_bytecode(value len)
{
  return caml_make_vect(len, caml_copy_nativeint(0));
}

/* This primitive is used internally by the compiler to compile
   explicit array expressions.
   For float arrays when FLAT_FLOAT_ARRAY is true, it takes an array of
   boxed floats and returns the corresponding flat-allocated [float array].
   In all other cases, it just returns its argument unchanged.
*/
static value make_array_gen(value init, int local)
{
#ifdef FLAT_FLOAT_ARRAY
  CAMLparam1 (init);
  mlsize_t wsize, size, i;
  CAMLlocal2 (v, res);

  size = Wosize_val(init);
  if (size == 0) {
    CAMLreturn (init);
  } else {
    v = Field(init, 0);
    if (Is_long(v)
        || ! Is_in_value_area(v)
        || Tag_val(v) != Double_tag) {
      CAMLreturn (init);
    } else {
      wsize = size * Double_wosize;
      if (local) {
        res = caml_alloc_local(wsize, Double_array_tag);
      } else if (wsize <= Max_young_wosize) {
        res = caml_alloc_small(wsize, Double_array_tag);
      } else {
        res = caml_alloc_shr(wsize, Double_array_tag);
      }
      for (i = 0; i < size; i++) {
        double d = Double_val(Field(init, i));
        Store_double_flat_field(res, i, d);
      }
      // run memprof callbacks
      if (!local)
        caml_process_pending_actions();
      CAMLreturn (res);
    }
  }
#else
  return init;
#endif
}

CAMLprim value caml_make_array(value init)
{
  return make_array_gen(init, 0);
}

CAMLprim value caml_make_array_local(value init)
{
  return make_array_gen(init, 1);
}

/* Blitting */

CAMLprim value caml_floatarray_blit(value a1, value ofs1, value a2, value ofs2,
                                    value n)
{
  memmove((double *)a2 + Long_val(ofs2),
          (double *)a1 + Long_val(ofs1),
          Long_val(n) * sizeof(double));
  return Val_unit;
}

CAMLprim value caml_unboxed_int32_vect_blit(value a1, value ofs1, value a2,
                                            value ofs2, value n)
{
  // Need to skip the custom_operations field
  memmove((int32_t *)((uintnat *)a2 + 1) + Long_val(ofs2),
          (int32_t *)((uintnat *)a1 + 1) + Long_val(ofs1),
          Long_val(n) * sizeof(int32_t));
  return Val_unit;
}

CAMLprim value caml_unboxed_int64_vect_blit(value a1, value ofs1, value a2, value ofs2,
                                            value n)
{
  // Need to skip the custom_operations field
  memmove((int64_t *)((uintnat *)a2 + 1) + Long_val(ofs2),
          (int64_t *)((uintnat *)a1 + 1) + Long_val(ofs1),
          Long_val(n) * sizeof(int64_t));
  return Val_unit;
}

CAMLprim value caml_unboxed_nativeint_vect_blit(value a1, value ofs1, value a2,
                                                value ofs2, value n)
{
  // Need to skip the custom_operations field
  memmove((uintnat *)((uintnat *)a2 + 1) + Long_val(ofs2),
          (uintnat *)((uintnat *)a1 + 1) + Long_val(ofs1),
          Long_val(n) * sizeof(uintnat));
  return Val_unit;
}

CAMLprim value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                               value n)
{
  value * src, * dst;
  intnat count;

#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(a2) == Double_array_tag)
    return caml_floatarray_blit(a1, ofs1, a2, ofs2, n);
#endif
  CAMLassert (Tag_val(a2) != Double_array_tag);
  if (Is_young(a2) ||
      Color_hd(Hd_val(a2)) == Local_unmarked) {
    /* Arrays of values, destination is local or in young generation.
       Here too we can do a direct copy since this cannot create
       old-to-young pointers, nor mess up with the incremental major GC.
       Again, memmove takes care of overlap. */
    memmove(&Field(a2, Long_val(ofs2)),
            &Field(a1, Long_val(ofs1)),
            Long_val(n) * sizeof(value));
    return Val_unit;
  }
  /* Array of values, destination is in old generation.
     We must use caml_modify.  */
  count = Long_val(n);
  if (a1 == a2 && Long_val(ofs1) < Long_val(ofs2)) {
    /* Copy in descending order */
    for (dst = &Field(a2, Long_val(ofs2) + count - 1),
           src = &Field(a1, Long_val(ofs1) + count - 1);
         count > 0;
         count--, src--, dst--) {
      caml_modify(dst, *src);
    }
  } else {
    /* Copy in ascending order */
    for (dst = &Field(a2, Long_val(ofs2)), src = &Field(a1, Long_val(ofs1));
         count > 0;
         count--, src++, dst++) {
      caml_modify(dst, *src);
    }
  }
  /* Many caml_modify in a row can create a lot of old-to-young refs.
     Give the minor GC a chance to run if it needs to. */
  caml_check_urgent_gc(Val_unit);
  return Val_unit;
}

/* A generic function for extraction and concatenation of sub-arrays */

static value caml_array_gather(intnat num_arrays,
                               value arrays[/*num_arrays*/],
                               intnat offsets[/*num_arrays*/],
                               intnat lengths[/*num_arrays*/],
                               int local)
{
  CAMLparamN(arrays, num_arrays);
  value res;                    /* no need to register it as a root */
#ifdef FLAT_FLOAT_ARRAY
  int isfloat = 0;
  mlsize_t wsize;
#endif
  mlsize_t i, size, count, pos;
  value * src;

  /* Determine total size and whether result array is an array of floats */
  size = 0;
  for (i = 0; i < num_arrays; i++) {
    if (mlsize_t_max - lengths[i] < size) caml_invalid_argument("Array.concat");
    size += lengths[i];
#ifdef FLAT_FLOAT_ARRAY
    if (Tag_val(arrays[i]) == Double_array_tag) isfloat = 1;
#endif
  }
  if (size == 0) {
    /* If total size = 0, just return empty array */
    res = Atom(0);
  }
#ifdef FLAT_FLOAT_ARRAY
  else if (isfloat) {
    /* This is an array of floats.  We can use memcpy directly. */
    if (size > Max_wosize/Double_wosize) caml_invalid_argument("Array.concat");
    wsize = size * Double_wosize;
    res = local ?
      caml_alloc_local(wsize, Double_array_tag) :
      caml_alloc(wsize, Double_array_tag);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy((double *)res + pos,
             (double *)arrays[i] + offsets[i],
             lengths[i] * sizeof(double));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  }
#endif
  else if (size > Max_wosize) {
    /* Array of values, too big. */
    caml_invalid_argument("Array.concat");
  } else if (size <= Max_young_wosize || local) {
    /* Array of values, local or small enough to fit in young generation.
       We can use memcpy directly. */
    res = local ?
      caml_alloc_local(size, 0) :
      caml_alloc_small(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      memcpy(&Field(res, pos),
             &Field(arrays[i], offsets[i]),
             lengths[i] * sizeof(value));
      pos += lengths[i];
    }
    CAMLassert(pos == size);
  } else {
    /* Array of values, must be allocated in old generation and filled
       using caml_initialize. */
    res = caml_alloc_shr(size, 0);
    for (i = 0, pos = 0; i < num_arrays; i++) {
      for (src = &Field(arrays[i], offsets[i]), count = lengths[i];
           count > 0;
           count--, src++, pos++) {
        caml_initialize(&Field(res, pos), *src);
      }
    }
    CAMLassert(pos == size);

    /* Many caml_initialize in a row can create a lot of old-to-young
       refs.  Give the minor GC a chance to run if it needs to.
       Run memprof callbacks for the major allocation. */
    res = caml_process_pending_actions_with_root (res);
  }
  CAMLreturn (res);
}

CAMLprim value caml_array_sub(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_array_gather(1, arrays, offsets, lengths, 0);
}

CAMLprim value caml_array_sub_local(value a, value ofs, value len)
{
  value arrays[1] = { a };
  intnat offsets[1] = { Long_val(ofs) };
  intnat lengths[1] = { Long_val(len) };
  return caml_array_gather(1, arrays, offsets, lengths, 1);
}

CAMLprim value caml_array_append(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_array_gather(2, arrays, offsets, lengths, 0);
}

CAMLprim value caml_array_append_local(value a1, value a2)
{
  value arrays[2] = { a1, a2 };
  intnat offsets[2] = { 0, 0 };
  intnat lengths[2] = { caml_array_length(a1), caml_array_length(a2) };
  return caml_array_gather(2, arrays, offsets, lengths, 1);
}

static value array_concat_gen(value al, int local)
{
#define STATIC_SIZE 16
  value static_arrays[STATIC_SIZE], * arrays;
  intnat static_offsets[STATIC_SIZE], * offsets;
  intnat static_lengths[STATIC_SIZE], * lengths;
  intnat n, i;
  value l, res;

  /* Length of list = number of arrays */
  for (n = 0, l = al; l != Val_int(0); l = Field(l, 1)) n++;
  /* Allocate extra storage if too many arrays */
  if (n <= STATIC_SIZE) {
    arrays = static_arrays;
    offsets = static_offsets;
    lengths = static_lengths;
  } else {
    arrays = caml_stat_alloc(n * sizeof(value));
    offsets = caml_stat_alloc_noexc(n * sizeof(intnat));
    if (offsets == NULL) {
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
    lengths = caml_stat_alloc_noexc(n * sizeof(value));
    if (lengths == NULL) {
      caml_stat_free(offsets);
      caml_stat_free(arrays);
      caml_raise_out_of_memory();
    }
  }
  /* Build the parameters to caml_array_gather */
  for (i = 0, l = al; l != Val_int(0); l = Field(l, 1), i++) {
    arrays[i] = Field(l, 0);
    offsets[i] = 0;
    lengths[i] = caml_array_length(Field(l, 0));
  }
  /* Do the concatenation */
  res = caml_array_gather(n, arrays, offsets, lengths, local);
  /* Free the extra storage if needed */
  if (n > STATIC_SIZE) {
    caml_stat_free(arrays);
    caml_stat_free(offsets);
    caml_stat_free(lengths);
  }
  return res;
}

CAMLprim value caml_array_concat(value al)
{
  return array_concat_gen(al, 0);
}

CAMLprim value caml_array_concat_local(value al)
{
  return array_concat_gen(al, 1);
}

CAMLprim value caml_array_fill(value array,
                               value v_ofs,
                               value v_len,
                               value val)
{
  intnat ofs = Long_val(v_ofs);
  intnat len = Long_val(v_len);
  value* fp;

  /* This duplicates the logic of caml_modify.  Please refer to the
     implementation of that function for a description of GC
     invariants we need to enforce.*/

#ifdef FLAT_FLOAT_ARRAY
  if (Tag_val(array) == Double_array_tag) {
    double d = Double_val (val);
    for (; len > 0; len--, ofs++)
      Store_double_flat_field(array, ofs, d);
    return Val_unit;
  }
#endif
  fp = &Field(array, ofs);
  if (Is_young(array) ||
      Color_hd(Hd_val(array)) == Local_unmarked) {
    for (; len > 0; len--, fp++) *fp = val;
  } else {
    int is_val_young_block = Is_block(val) && Is_young(val);
    CAMLassert(Is_in_heap(fp));
    for (; len > 0; len--, fp++) {
      value old = *fp;
      if (old == val) continue;
      *fp = val;
      if (Is_block(old)) {
        if (Is_young(old)) continue;
        if (caml_gc_phase == Phase_mark) caml_darken(old, NULL);
      }
      if (is_val_young_block)
        add_to_ref_table (Caml_state->ref_table, fp);
    }
    if (is_val_young_block) caml_check_urgent_gc (Val_unit);
  }
  return Val_unit;
}

CAMLprim value caml_iarray_of_array(value a)
{
  return a;
}

CAMLprim value caml_array_of_iarray(value a)
{
  return a;
}


/* We need these pre-declared for [gen_primitives.sh] to work. */
CAMLprim value caml_array_get_indexed_by_int64(value, value);
CAMLprim value caml_array_unsafe_get_indexed_by_int64(value, value);
CAMLprim value caml_array_set_indexed_by_int64(value, value, value);
CAMLprim value caml_array_unsafe_set_indexed_by_int64(value, value, value);

CAMLprim value caml_array_get_indexed_by_int32(value, value);
CAMLprim value caml_array_unsafe_get_indexed_by_int32(value, value);
CAMLprim value caml_array_set_indexed_by_int32(value, value, value);
CAMLprim value caml_array_unsafe_set_indexed_by_int32(value, value, value);

CAMLprim value caml_array_get_indexed_by_nativeint(value, value);
CAMLprim value caml_array_unsafe_get_indexed_by_nativeint(value, value);
CAMLprim value caml_array_set_indexed_by_nativeint(value, value, value);
CAMLprim value caml_array_unsafe_set_indexed_by_nativeint(value, value, value);

#define CAMLprim_indexed_by(name, index_type, val_func)                     \
  CAMLprim value caml_array_get_indexed_by_##name(value array, value index) \
  {                                                                         \
    index_type idx = val_func(index);                                       \
    if (idx != Long_val(Val_long(idx))) caml_array_bound_error();           \
    return caml_array_get(array, Val_long(idx));                            \
  }                                                                         \
  CAMLprim value caml_array_unsafe_get_indexed_by_##name(value array,       \
                                                         value index)       \
  {                                                                         \
    return caml_array_unsafe_get(array, Val_long(val_func(index)));         \
  }                                                                         \
  CAMLprim value caml_array_set_indexed_by_##name(value array,              \
                                                  value index,              \
                                                  value newval)             \
  {                                                                         \
    index_type idx = val_func(index);                                       \
    if (idx != Long_val(Val_long(idx))) caml_array_bound_error();           \
    return caml_array_set(array, Val_long(idx), newval);                    \
  }                                                                         \
  CAMLprim value caml_array_unsafe_set_indexed_by_##name(value array,       \
                                                         value index,       \
                                                         value newval)      \
  {                                                                         \
    return caml_array_unsafe_set(array, Val_long(val_func(index)), newval); \
  }

CAMLprim_indexed_by(int64, int64_t, Int64_val)
CAMLprim_indexed_by(int32, int32_t, Int32_val)
CAMLprim_indexed_by(nativeint, intnat, Nativeint_val)
