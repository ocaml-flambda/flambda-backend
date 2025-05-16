#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


static int8_t Int8_val(value v) {
  return (int8_t)Long_val(v);
}

static value caml_copy_int8(int8_t x) {
  return Val_long(x);
}

CAMLprim int8_t lognot_UtoU(int8_t u) {
  return ~u;
}

CAMLprim int8_t lognot_BtoU(value u) {
  return ~Int8_val(u);
}

CAMLprim value lognot_UtoB(int8_t u) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_int8(~u);
  CAMLreturn(result);
}

CAMLprim value lognot_bytecode(value u) {
  CAMLparam1(u);
  CAMLlocal1(result);
  result = caml_copy_int8(~Int8_val(u));
  CAMLreturn(result);
}

CAMLprim int8_t sum_7_UBUBUBUtoU(int8_t u1, value b2, int8_t u3, value b4,
                                  int8_t u5, value b6, int8_t u7) {
  int8_t u2 = Int8_val(b2);
  int8_t u4 = Int8_val(b4);
  int8_t u6 = Int8_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  if (argn != 7) CAMLreturn(caml_copy_int8(-1));
  int8_t u1 = Int8_val(argv[0]);
  int8_t u2 = Int8_val(argv[1]);
  int8_t u3 = Int8_val(argv[2]);
  int8_t u4 = Int8_val(argv[3]);
  int8_t u5 = Int8_val(argv[4]);
  int8_t u6 = Int8_val(argv[5]);
  int8_t u7 = Int8_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_int8(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
