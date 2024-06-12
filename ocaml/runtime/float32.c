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

#define CAML_INTERNALS

/* Needed for uselocale */
#define _XOPEN_SOURCE 700

/* Needed for strtof_l */
#define _GNU_SOURCE

#include <math.h>
#include <float.h>
#include <limits.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/float32.h"
#include "caml/memory.h"
#include "caml/intext.h"
#include "caml/mlvalues.h"

#if defined(HAS_LOCALE) || defined(__MINGW32__)

#if defined(HAS_LOCALE_H) || defined(__MINGW32__)
#include <locale.h>
#endif

#if defined(HAS_XLOCALE_H)
#include <xlocale.h>
#endif

#if defined(_MSC_VER)
#ifndef locale_t
#define locale_t _locale_t
#endif
#ifndef freelocale
#define freelocale _free_locale
#endif
#ifndef strtof_l
#define strtof_l _strtof_l
#endif
#endif

#endif /* defined(HAS_LOCALE) */

CAML_STATIC_ASSERT(sizeof(float) == sizeof(int32_t));

#define Max_custom_array_wosize          (Max_wosize - 1)
#define Max_unboxed_float32_array_wosize (Max_custom_array_wosize * (sizeof(intnat) / sizeof(float)))

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

CAMLexport const struct custom_operations caml_float32_ops = {
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

#define DEFINE_BYTE_UNOP(op)                      \
  (value f)                                       \
  {                                               \
    return caml_copy_float32(op(Float32_val(f))); \
  }

CAMLprim value caml_sqrt_float32_bytecode DEFINE_BYTE_UNOP(sqrtf)
CAMLprim value caml_cbrt_float32_bytecode DEFINE_BYTE_UNOP(cbrtf)
CAMLprim value caml_exp_float32_bytecode DEFINE_BYTE_UNOP(expf)
CAMLprim value caml_exp2_float32_bytecode DEFINE_BYTE_UNOP(exp2f)
CAMLprim value caml_log_float32_bytecode DEFINE_BYTE_UNOP(logf)
CAMLprim value caml_log10_float32_bytecode DEFINE_BYTE_UNOP(log10f)
CAMLprim value caml_log2_float32_bytecode DEFINE_BYTE_UNOP(log2f)
CAMLprim value caml_expm1_float32_bytecode DEFINE_BYTE_UNOP(expm1f)
CAMLprim value caml_log1p_float32_bytecode DEFINE_BYTE_UNOP(log1pf)
CAMLprim value caml_cos_float32_bytecode DEFINE_BYTE_UNOP(cosf)
CAMLprim value caml_sin_float32_bytecode DEFINE_BYTE_UNOP(sinf)
CAMLprim value caml_tan_float32_bytecode DEFINE_BYTE_UNOP(tanf)
CAMLprim value caml_acos_float32_bytecode DEFINE_BYTE_UNOP(acosf)
CAMLprim value caml_asin_float32_bytecode DEFINE_BYTE_UNOP(asinf)
CAMLprim value caml_atan_float32_bytecode DEFINE_BYTE_UNOP(atanf)
CAMLprim value caml_cosh_float32_bytecode DEFINE_BYTE_UNOP(coshf)
CAMLprim value caml_sinh_float32_bytecode DEFINE_BYTE_UNOP(sinhf)
CAMLprim value caml_tanh_float32_bytecode DEFINE_BYTE_UNOP(tanhf)
CAMLprim value caml_acosh_float32_bytecode DEFINE_BYTE_UNOP(acoshf)
CAMLprim value caml_asinh_float32_bytecode DEFINE_BYTE_UNOP(asinhf)
CAMLprim value caml_atanh_float32_bytecode DEFINE_BYTE_UNOP(atanhf)
CAMLprim value caml_erf_float32_bytecode DEFINE_BYTE_UNOP(erff)
CAMLprim value caml_erfc_float32_bytecode DEFINE_BYTE_UNOP(erfcf)
CAMLprim value caml_trunc_float32_bytecode DEFINE_BYTE_UNOP(truncf)
CAMLprim value caml_round_float32_bytecode DEFINE_BYTE_UNOP(roundf)
CAMLprim value caml_ceil_float32_bytecode DEFINE_BYTE_UNOP(ceilf)
CAMLprim value caml_floor_float32_bytecode DEFINE_BYTE_UNOP(floorf)

#define DEFINE_BYTE_BINOP(op)                                    \
  (value f, value g)                                             \
  {                                                              \
    return caml_copy_float32(op(Float32_val(f),Float32_val(g))); \
  }

CAMLprim value caml_atan2_float32_bytecode DEFINE_BYTE_BINOP(atan2f)
CAMLprim value caml_hypot_float32_bytecode DEFINE_BYTE_BINOP(hypotf)
CAMLprim value caml_nextafter_float32_bytecode DEFINE_BYTE_BINOP(nextafterf)
CAMLprim value caml_copysign_float32_bytecode DEFINE_BYTE_BINOP(copysignf)
CAMLprim value caml_fmod_float32_bytecode DEFINE_BYTE_BINOP(fmodf)
CAMLprim value caml_power_float32_bytecode DEFINE_BYTE_BINOP(powf)

CAMLprim value caml_fma_float32_bytecode(value f, value g, value h)
{
  return caml_copy_float32(fmaf(Float32_val(f), Float32_val(g), Float32_val(h)));
}

float caml_float32_of_int64(int64_t i) {
  return (float)i;
}

CAMLprim value caml_float32_of_int64_bytecode(value i) {
  return caml_copy_float32(caml_float32_of_int64(Int64_val(i)));
}

int64_t caml_float32_to_int64(float f) {
  return (int64_t)f;
}

CAMLprim value caml_float32_to_int64_bytecode(value f) {
  return caml_copy_int64(caml_float32_to_int64(Float32_val(f)));
}

float caml_float32_of_bits(int32_t bits)
{
  union { float f; int32_t i; } u;
  u.i = bits;
  return u.f;
}

CAMLprim value caml_float32_of_bits_bytecode(value bits)
{
  return caml_copy_float32(caml_float32_of_bits(Int32_val(bits)));
}

int32_t caml_float32_to_bits(float f)
{
  union { float f; int32_t i; } u;
  u.f = f;
  return u.i;
}

CAMLprim value caml_float32_to_bits_bytecode(value f)
{
  return caml_copy_int32(caml_float32_to_bits(Float32_val(f)));
}

float caml_ldexp_float32(float f, intnat i)
{
  return ldexpf(f, (int)i);
}

CAMLprim value caml_ldexp_float32_bytecode(value f, value i)
{
  return caml_copy_float32(caml_ldexp_float32(Float32_val(f), Int_val(i)));
}

enum { FP_normal, FP_subnormal, FP_zero, FP_infinite, FP_nan };

value caml_classify_float32(float vf)
{
  union { float f; uint32_t i; } u;
  uint32_t n;
  uint32_t e;
  u.f = vf;
  n = u.i << 1;                 /* shift sign bit off */
  if (n == 0) return Val_int(FP_zero);
  e = n >> 24;                  /* extract exponent */
  if (e == 0) return Val_int(FP_subnormal);
  if (e == 0xff) {
    if (n << 8 == 0)            /* shift exponent off */
      return Val_int(FP_infinite);
    else
      return Val_int(FP_nan);
  }
  return Val_int(FP_normal);
}

CAMLprim value caml_classify_float32_bytecode(value f)
{
  return caml_classify_float32(Float32_val(f));
}

value caml_signbit_float32(float f)
{
  return Val_bool(signbit(f));
}

CAMLprim value caml_signbit_float32_bytecode(value f)
{
  return caml_signbit_float32(Float32_val(f));
}

CAMLprim value caml_frexp_float32(value f)
{
  CAMLparam0 ();
  CAMLlocal1 (mantissa);
  value res;
  int exponent;

  mantissa = caml_copy_float32(frexpf(Float32_val(f), &exponent));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = mantissa;
  Field(res, 1) = Val_int(exponent);
  CAMLreturn (res);
}

CAMLprim value caml_modf_float32(value f)
{
  CAMLparam0 ();
  CAMLlocal2 (quo, rem);
  value res;
  float frem;

  quo = caml_copy_float32(modff(Float32_val(f), &frem));
  rem = caml_copy_float32(frem);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = quo;
  Field(res, 1) = rem;
  CAMLreturn (res);
}

/*
 OCaml runtime itself doesn't call setlocale, i.e. it is using
 standard "C" locale by default, but it is possible that
 third-party code loaded into process does.
*/
#ifdef HAS_LOCALE
extern locale_t caml_locale;
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
/* there is no analogue to uselocale in MSVC so just set locale for thread */
#define USE_LOCALE setlocale(LC_NUMERIC,"C")
#define RESTORE_LOCALE do {} while(0)
#elif defined(HAS_LOCALE)
#define USE_LOCALE locale_t saved_locale = uselocale(caml_locale)
#define RESTORE_LOCALE uselocale(saved_locale)
#else
#define USE_LOCALE do {} while(0)
#define RESTORE_LOCALE do {} while(0)
#endif

CAMLprim value caml_format_float32(value fmt, value arg)
{
  /* See caml_format_float */
  value res;
  float f = Float32_val(arg);

#ifdef HAS_BROKEN_PRINTF
  if (isfinite(f)) {
#endif
    USE_LOCALE;
    res = caml_alloc_sprintf(String_val(fmt), f);
    RESTORE_LOCALE;
#ifdef HAS_BROKEN_PRINTF
  } else {
    if (isnan(f)) {
      res = caml_copy_string("nan");
    } else {
      if (f > 0)
        res = caml_copy_string("inf");
      else
        res = caml_copy_string("-inf");
    }
  }
#endif
  return res;
}

static int caml_float32_of_hex(const char * s, const char * end, float * res)
{
  /* See caml_float_of_hex */
  int64_t m = 0;                /* the mantissa - top 60 bits at most */
  int n_bits = 0;               /* total number of bits read */
  int m_bits = 0;               /* number of bits in mantissa */
  int x_bits = 0;               /* number of bits after mantissa */
  int dec_point = -1;           /* bit count corresponding to decimal point */
                                /* -1 if no decimal point seen */
  int exp = 0;                  /* exponent */
  char * p;                     /* for converting the exponent */
  float f;

  while (s < end) {
    char c = *s++;
    switch (c) {
    case '.':
      if (dec_point >= 0) return -1; /* multiple decimal points */
      dec_point = n_bits;
      break;
    case 'p': case 'P': {
      long e;
      if (*s == 0) return -1;   /* nothing after exponent mark */
      e = strtol(s, &p, 10);
      if (p != end) return -1;  /* ill-formed exponent */
      /* Handle exponents larger than int by returning 0/infinity directly.
         Mind that INT_MIN/INT_MAX are included in the test so as to capture
         the overflow case of strtol on Win64 -- long and int have the same
         size there. */
      if (e <= INT_MIN) {
        *res = 0.f;
        return 0;
      }
      else if (e >= INT_MAX) {
        *res = m == 0 ? 0.f : HUGE_VALF;
        return 0;
      }
      /* regular exponent value */
      exp = e;
      s = p;                    /* stop at next loop iteration */
      break;
    }
    default: {                  /* Nonzero digit */
      int d;
      if (c >= '0' && c <= '9') d = c - '0';
      else if (c >= 'A' && c <= 'F') d = c - 'A' + 10;
      else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
      else return -1;           /* bad digit */
      n_bits += 4;
      if (d == 0 && m == 0) break; /* leading zeros are skipped */
      if (m_bits < 60) {
        /* There is still room in m.  Add this digit to the mantissa. */
        m = (m << 4) + d;
        m_bits += 4;
      } else {
        /* We've already collected 60 significant bits in m.
           Now all we care about is whether there is a nonzero bit
           after. In this case, round m to odd so that the later
           rounding of m to FP produces the correct result. */
        if (d != 0) m |= 1;        /* round to odd */
        x_bits += 4;
      }
      break;
    }
    }
  }
  if (n_bits == 0) return -1;
  /* Convert mantissa to FP.  We use a signed conversion because we can
     (m has 60 bits at most) and because it is faster
     on several architectures. */
  f = (float) (int64_t) m;
  /* Adjust exponent to take decimal point and extra digits into account */
  {
    int adj = x_bits;
    if (dec_point >= 0) adj = adj + (dec_point - n_bits);
    /* saturated addition exp + adj */
    if (adj > 0 && exp > INT_MAX - adj)
      exp = INT_MAX;
    else if (adj < 0 && exp < INT_MIN - adj)
      exp = INT_MIN;
    else
      exp = exp + adj;
  }
  /* Apply exponent if needed */
  if (exp != 0) f = ldexpf(f, exp);
  /* Done! */
  *res = f;
  return 0;
}

CAMLprim value caml_float32_of_string(value vs)
{
  /* See caml_float_of_string */
  char parse_buffer[64];
  char * buf, * dst, * end;
  const char *src;
  mlsize_t len;
  int sign;
  float f;

  /* Remove '_' characters before conversion */
  len = caml_string_length(vs);
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs);
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) goto error;
  /* Check for hexadecimal FP constant */
  src = buf;
  sign = 1;
  if (*src == '-') { sign = -1; src++; }
  else if (*src == '+') { src++; };
  if (src[0] == '0' && (src[1] == 'x' || src[1] == 'X')) {
    /* Convert using our hexadecimal FP parser */
    if (caml_float32_of_hex(src + 2, dst, &f) == -1) goto error;
    if (sign < 0) f = -f;
  } else {
    /* Convert using strtof, which is available when strtod is. */
#if defined(HAS_STRTOD_L) && defined(HAS_LOCALE)
    f = strtof_l((const char *) buf, &end, caml_locale);
#else
    USE_LOCALE;
    f = strtof((const char *) buf, &end);
    RESTORE_LOCALE;
#endif /* HAS_STRTOD_L */
    if (end != dst) goto error;
  }
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_float32(f);
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float32_of_string");
  return Val_unit; /* not reached */
}

/* Defined in array.c */

CAMLextern int caml_unboxed_array_no_polymorphic_compare(value v1, value v2);
CAMLextern intnat caml_unboxed_array_no_polymorphic_hash(value v);
CAMLextern void caml_unboxed_array_serialize(value v, uintnat* bsize_32, uintnat* bsize_64);
CAMLextern uintnat caml_unboxed_array_deserialize(void* dst);
CAMLextern value caml_make_vect(value len, value init);

CAMLexport const struct custom_operations caml_unboxed_float32_array_ops[2] = {
  { "_unboxed_float32_even_array",
    custom_finalize_default,
    caml_unboxed_array_no_polymorphic_compare,
    caml_unboxed_array_no_polymorphic_hash,
    caml_unboxed_array_serialize,
    caml_unboxed_array_deserialize,
    custom_compare_ext_default,
    custom_fixed_length_default },
  { "_unboxed_float32_odd_array",
    custom_finalize_default,
    caml_unboxed_array_no_polymorphic_compare,
    caml_unboxed_array_no_polymorphic_hash,
    caml_unboxed_array_serialize,
    caml_unboxed_array_deserialize,
    custom_compare_ext_default,
    custom_fixed_length_default },
};

CAMLprim value caml_make_unboxed_float32_vect(value len)
{
  /* This is only used on 64-bit targets. */

  mlsize_t num_elements = Long_val(len);
  if (num_elements > Max_unboxed_float32_array_wosize) caml_invalid_argument("Array.make");

  /* [num_fields] does not include the custom operations field. */
  mlsize_t num_fields = num_elements / 2 + num_elements % 2;

  return caml_alloc_custom(&caml_unboxed_float32_array_ops[num_elements % 2],
                           num_fields * sizeof(value), 0, 0);
}

CAMLprim value caml_make_unboxed_float32_vect_bytecode(value len)
{
  return caml_make_vect(len, caml_copy_float32(0.0f));
}

/* [MM] [TODO]: Not consistent with the memory model. See the discussion in
   https://github.com/ocaml-multicore/ocaml-multicore/pull/822. */
CAMLprim value caml_unboxed_float32_vect_blit(value a1, value ofs1, value a2,
                                              value ofs2, value n)
{
  /* See memory model [MM] notes in memory.c */
  atomic_thread_fence(memory_order_acquire);
  // Need to skip the custom_operations field
  memmove((float *)((uintnat *)a2 + 1) + Long_val(ofs2),
          (float *)((uintnat *)a1 + 1) + Long_val(ofs1),
          Long_val(n) * sizeof(float));
  return Val_unit;
}
