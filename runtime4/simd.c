/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2023 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/simd.h"

#define Max_array_wosize                   (Max_wosize)
#define Max_custom_array_wosize            (Max_wosize - 1)

#define Max_unboxed_vec128_array_wosize    (Max_custom_array_wosize / Words_per_vec128)
#define Max_unboxed_vec256_array_wosize    (Max_custom_array_wosize / Words_per_vec256)

// Defined in array.c
CAMLextern int caml_unboxed_array_no_polymorphic_compare(value v1, value v2);
CAMLextern intnat caml_unboxed_array_no_polymorphic_hash(value v);
CAMLextern void caml_unboxed_array_serialize(value v, uintnat* bsize_32, uintnat* bsize_64);
CAMLextern uintnat caml_unboxed_array_deserialize(void* dst);

CAMLexport struct custom_operations caml_unboxed_vec128_array_ops = {
  "_unboxed_vec128_array",
  custom_finalize_default,
  caml_unboxed_array_no_polymorphic_compare,
  caml_unboxed_array_no_polymorphic_hash,
  caml_unboxed_array_serialize,
  caml_unboxed_array_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_unboxed_vec128_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
    // Need to skip the custom_operations field
    memmove(((uintnat *)a2 + 1) + Long_val(ofs2) * Words_per_vec128,
            ((uintnat *)a1 + 1) + Long_val(ofs1) * Words_per_vec128,
            Long_val(n) * sizeof(uintnat) * Words_per_vec128);
    return Val_unit;
}

static value caml_make_unboxed_vec128_vect0(value len, int local)
{
  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_vec128_array_wosize)
    caml_invalid_argument("Array.make");

  /* [num_fields] does not include the custom operations field. */
  mlsize_t num_fields = num_elements * Words_per_vec128;

  if (local)
    return caml_alloc_custom_local(&caml_unboxed_vec128_array_ops,
      num_fields * sizeof(value), 0, 0);
  else
    return caml_alloc_custom(&caml_unboxed_vec128_array_ops,
      num_fields * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 0);
}

CAMLprim value caml_make_local_unboxed_vec128_vect(value len)
{
  return caml_make_unboxed_vec128_vect0(len, 1);
}

CAMLprim value caml_make_unboxed_vec128_vect_bytecode(value len) {
  caml_failwith("128-bit SIMD is not supported on this platform.");
}

CAMLexport struct custom_operations caml_unboxed_vec256_array_ops = {
  "_unboxed_vec256_array",
  custom_finalize_default,
  caml_unboxed_array_no_polymorphic_compare,
  caml_unboxed_array_no_polymorphic_hash,
  caml_unboxed_array_serialize,
  caml_unboxed_array_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_unboxed_vec256_vect_blit(value a1, value ofs1, value a2,
                                             value ofs2, value n) {
    // Need to skip the custom_operations field
    memmove(((uintnat *)a2 + 1) + Long_val(ofs2) * Words_per_vec256,
            ((uintnat *)a1 + 1) + Long_val(ofs1) * Words_per_vec256,
            Long_val(n) * sizeof(uintnat) * Words_per_vec256);
    return Val_unit;
}

static value caml_make_unboxed_vec256_vect0(value len, int local)
{
  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_vec256_array_wosize)
    caml_invalid_argument("Array.make");

  /* [num_fields] does not include the custom operations field. */
  mlsize_t num_fields = num_elements * Words_per_vec256;

  if (local)
    return caml_alloc_custom_local(&caml_unboxed_vec256_array_ops,
      num_fields * sizeof(value), 0, 0);
  else
    return caml_alloc_custom(&caml_unboxed_vec256_array_ops,
      num_fields * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_vec256_vect(value len)
{
  return caml_make_unboxed_vec256_vect0(len, 0);
}

CAMLprim value caml_make_local_unboxed_vec256_vect(value len)
{
  return caml_make_unboxed_vec256_vect0(len, 1);
}

CAMLprim value caml_make_unboxed_vec256_vect_bytecode(value len) {
  caml_failwith("256-bit SIMD is not supported on this platform.");
}
