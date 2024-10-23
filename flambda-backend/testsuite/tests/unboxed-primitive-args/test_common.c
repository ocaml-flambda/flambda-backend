/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*                  Jeremie Dimino, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2015 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <smmintrin.h>
#include <emmintrin.h>

char *ocaml_buffer;
char *c_buffer;

value test_set_buffers(value v_ocaml_buffer, value v_c_buffer)
{
  ocaml_buffer = Caml_ba_data_val(v_ocaml_buffer);
  c_buffer = Caml_ba_data_val(v_c_buffer);
  return Val_unit;
}

value test_cleanup_normal(void)
{
  return Val_int(0);
}

double test_cleanup_float(void)
{
  return 0.;
}

int64_t vec128_low_int64(__m128i v)
{
  return _mm_extract_epi64(v, 0);
}

int64_t vec128_high_int64(__m128i v)
{
  return _mm_extract_epi64(v, 1);
}

__m128i vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}
