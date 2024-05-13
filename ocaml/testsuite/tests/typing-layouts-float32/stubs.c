#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/float32.h>

#include <math.h>

CAMLprim float sin_U_U (float u1) {
  return sin(u1);
}

CAMLprim float sin_B_U (value u1) {
  return sin(Float32_val (u1));
}

CAMLprim value sin_U_B (float u1) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_float32(sin(u1));
  CAMLreturn(result);
}

CAMLprim value sin_byte (value u1) {
  CAMLparam1(u1);
  CAMLlocal1(result);
  result = caml_copy_float32(sin_U_U (Float32_val (u1)));
  CAMLreturn(result);
}

CAMLprim float sum_7(float x1, value x2b, float x3, value x4b, float x5,
                      value x6b, float x7) {
  float x2 = Float32_val(x2b);
  float x4 = Float32_val(x4b);
  float x6 = Float32_val(x6b);
  return (x1 + x2 + x3 + x4 + x5 + x6 + x7);
}

CAMLprim value sum_7_byte(value* argv, int argn) {
  CAMLparam0();
  float x1 = Float32_val(argv[0]);
  float x2 = Float32_val(argv[1]);
  float x3 = Float32_val(argv[2]);
  float x4 = Float32_val(argv[3]);
  float x5 = Float32_val(argv[4]);
  float x6 = Float32_val(argv[5]);
  float x7 = Float32_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_float32(x1 + x2 + x3 + x4 + x5 + x6 + x7);
  CAMLreturn(result);
}
