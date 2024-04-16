/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                       Pierre Chambart, OCamlPro                        */
/*           Mark Shinwell and Leo White, Jane Street Europe              */
/*                        Max Slater, Jane Street                         */
/*                                                                        */
/*   Copyright 2013--2024 OCamlPro SAS                                    */
/*   Copyright 2014--2024 Jane Street Group LLC                           */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Needed for strtof_l */
#define _GNU_SOURCE
#ifdef __APPLE__
#include <xlocale.h>
#endif

#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <locale.h>
#include <limits.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"

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

extern locale_t caml_locale;

#define USE_LOCALE locale_t saved_locale = uselocale(caml_locale)
#define RESTORE_LOCALE uselocale(saved_locale)

static float float32_of_int32(int32_t i)
{
    union
    {
        int32_t i;
        float f;
    } u;
    u.i = i;
    return u.f;
}

static int32_t int32_of_float32(float f)
{
    union
    {
        int32_t i;
        float f;
    } u;
    u.f = f;
    return u.i;
}

int32_t compiler_float32_neg(int32_t i)
{
    return int32_of_float32(-float32_of_int32(i));
}

int32_t compiler_float32_abs(int32_t i)
{
    return int32_of_float32(fabsf(float32_of_int32(i)));
}

int32_t compiler_float32_add(int32_t i, int32_t j)
{
    return int32_of_float32(float32_of_int32(i) + float32_of_int32(j));
}

int32_t compiler_float32_sub(int32_t i, int32_t j)
{
    return int32_of_float32(float32_of_int32(i) - float32_of_int32(j));
}

int32_t compiler_float32_mul(int32_t i, int32_t j)
{
    return int32_of_float32(float32_of_int32(i) * float32_of_int32(j));
}

int32_t compiler_float32_div(int32_t i, int32_t j)
{
    return int32_of_float32(float32_of_int32(i) / float32_of_int32(j));
}

int32_t compiler_float32_mod(int32_t i, int32_t j)
{
    return int32_of_float32(fmodf(float32_of_int32(i), float32_of_int32(j)));
}

value compiler_float32_compare(int32_t i, int32_t j)
{
    /* See caml_float_compare_unboxed */
    float f = float32_of_int32(i);
    float g = float32_of_int32(j);
    intnat res =
        (intnat)(f > g) - (intnat)(f < g) + (intnat)(f == f) - (intnat)(g == g);
    return Val_int(res);
}

bool compiler_float32_equal(int32_t i, int32_t j)
{
    return float32_of_int32(i) == float32_of_int32(j);
}

int32_t compiler_float32_of_float(double d)
{
    return int32_of_float32((float)d);
}

double compiler_float32_to_float(int32_t i)
{
    return (double)float32_of_int32(i);
}

value compiler_float32_neg_boxed(value i)
{
    return caml_copy_int32(compiler_float32_neg(Int32_val(i)));
}

value compiler_float32_abs_boxed(value i)
{
    return caml_copy_int32(compiler_float32_abs(Int32_val(i)));
}

value compiler_float32_add_boxed(value i, value j)
{
    return caml_copy_int32(compiler_float32_add(Int32_val(i), Int32_val(j)));
}

value compiler_float32_sub_boxed(value i, value j)
{
    return caml_copy_int32(compiler_float32_sub(Int32_val(i), Int32_val(j)));
}

value compiler_float32_mul_boxed(value i, value j)
{
    return caml_copy_int32(compiler_float32_mul(Int32_val(i), Int32_val(j)));
}

value compiler_float32_div_boxed(value i, value j)
{
    return caml_copy_int32(compiler_float32_div(Int32_val(i), Int32_val(j)));
}

value compiler_float32_mod_boxed(value i, value j)
{
    return caml_copy_int32(compiler_float32_mod(Int32_val(i), Int32_val(j)));
}

value compiler_float32_compare_boxed(value i, value j)
{
    return compiler_float32_compare(Int32_val(i), Int32_val(j));
}

value compiler_float32_equal_boxed(value i, value j)
{
    return Val_bool(compiler_float32_equal(Int32_val(i), Int32_val(j)));
}

value compiler_float32_of_float_boxed(value d)
{
    return caml_copy_int32(compiler_float32_of_float(Double_val(d)));
}

value compiler_float32_to_float_boxed(value i)
{
    return caml_copy_double(compiler_float32_to_float(Int32_val(i)));
}

static int compiler_float32_of_hex(const char * s, const char * end, float * res)
{
  /* See caml_float_of_hex */
  int64_t m = 0;                /* the mantissa - top 24 bits at most */
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
      if (m_bits < 24) {
        /* There is still room in m.  Add this digit to the mantissa. */
        m = (m << 4) + d;
        m_bits += 4;
      } else {
        /* We've already collected 24 significant bits in m.
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
     (m has 24 bits at most) and because it is faster
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

CAMLprim value compiler_float32_of_string(value vs)
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
    if (compiler_float32_of_hex(src + 2, dst, &f) == -1) goto error;
    if (sign < 0) f = -f;
  } else {
    /* Convert using strtof */
    f = strtof_l((const char *) buf, &end, caml_locale);
    if (end != dst) goto error;
  }
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_int32(int32_of_float32(f));
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float32_of_string");
  return Val_unit; /* not reached */
}

CAMLprim value compiler_float32_format(value fmt, value arg)
{
  value res;
  float f = float32_of_int32(Int32_val(arg));

  USE_LOCALE;
  res = caml_alloc_sprintf(String_val(fmt), f);
  RESTORE_LOCALE;

  return res;
}
