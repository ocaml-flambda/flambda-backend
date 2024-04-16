/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <math.h>
#include <float.h>

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/float32.h"
#include "caml/memory.h"
#include "caml/intext.h"
#include "caml/mlvalues.h"

#define CAML_INTERNALS

CAML_STATIC_ASSERT(sizeof(float) == sizeof(int32_t));

intnat caml_float32_compare_unboxed(float f, float g)
{
  /* If one or both of f and g is NaN, order according to the convention
     NaN = NaN and NaN < x for all other floats x. */
  /* This branchless implementation is from GPR#164.
     Note that [f == f] if and only if f is not NaN.
     We expand each subresult of the expression to
     avoid sign-extension on 64bit. GPR#2250.  */
  intnat res =
      (intnat)(f > g) - (intnat)(f < g) + (intnat)(f == f) - (intnat)(g == g);
  return res;
}

static int float32_cmp(value v1, value v2)
{
  return caml_float32_compare_unboxed(Float32_val(v1), Float32_val(v2));
}

static intnat float32_hash(value v)
{
  union {
    float f;
    uint32_t i;
  } u;
  uint32_t n;
  u.f = Float32_val(v);  n = u.i;
  /* Normalize NaNs */
  if ((n & 0x7F800000) == 0x7F800000 && (n & 0x007FFFFF) != 0) {
    n = 0x7F800001;
  }
  /* Normalize -0 into +0 */
  else if (n == 0x80000000) {
    n = 0;
  }
  return n;
}

static uintnat float32_deserialize(void *dst)
{
  *((float *)dst) = caml_deserialize_float_4();
  return 4;
}

static void float32_serialize(value v, uintnat *bsize_32,
                              uintnat *bsize_64)
{
  caml_serialize_float_4(Float32_val(v));
  *bsize_32 = *bsize_64 = 4;
}

static const struct custom_fixed_length float32_length = {4, 4};

CAMLexport struct custom_operations caml_float32_ops = {
  "_f32",
  custom_finalize_default,
  float32_cmp,
  float32_hash,
  float32_serialize,
  float32_deserialize,
  custom_compare_ext_default,
  &float32_length
};

CAMLexport value caml_copy_float32(float f)
{
  value res = caml_alloc_custom(&caml_float32_ops, 4, 0, 1);
  Float32_val(res) = f;
  return res;
}

CAMLprim value caml_float32_of_float(value d)
{
  return caml_copy_float32((float)Double_val(d));
}

CAMLprim value caml_float_of_float32(value f)
{
  return caml_copy_double((double)Float32_val(f));
}

CAMLprim value caml_int_of_float32(value f)
{
  return Val_long((intnat)Float32_val(f));
}

CAMLprim value caml_float32_of_int(value n)
{
  return caml_copy_float32((float)Long_val(n));
}

CAMLprim value caml_neg_float32(value f)
{
  return caml_copy_float32(-Float32_val(f));
}

CAMLprim value caml_abs_float32(value f)
{
  return caml_copy_float32(fabsf(Float32_val(f)));
}

CAMLprim value caml_add_float32(value f, value g)
{
  return caml_copy_float32(Float32_val(f) + Float32_val(g));
}

CAMLprim value caml_sub_float32(value f, value g)
{
  return caml_copy_float32(Float32_val(f) - Float32_val(g));
}

CAMLprim value caml_mul_float32(value f, value g)
{
  return caml_copy_float32(Float32_val(f) * Float32_val(g));
}

CAMLprim value caml_div_float32(value f, value g)
{
  return caml_copy_float32(Float32_val(f) / Float32_val(g));
}

CAMLprim value caml_sqrt_float32(value f)
{
  return caml_copy_float32(sqrtf(Float32_val(f)));
}

CAMLprim value caml_float32_compare(value vf, value vg)
{
  return Val_int(caml_float32_compare_unboxed(Float32_val(vf), Float32_val(vg)));
}

#define DEFINE_NAN_CMP(op)                             \
  (value f, value g)                                   \
  {                                                    \
    return Val_bool(Float32_val(f) op Float32_val(g)); \
  }

CAMLprim value caml_eq_float32 DEFINE_NAN_CMP(==)
CAMLprim value caml_neq_float32 DEFINE_NAN_CMP(!=)
CAMLprim value caml_le_float32 DEFINE_NAN_CMP(<=)
CAMLprim value caml_lt_float32 DEFINE_NAN_CMP(<)
CAMLprim value caml_ge_float32 DEFINE_NAN_CMP(>=)
CAMLprim value caml_gt_float32 DEFINE_NAN_CMP(>)
