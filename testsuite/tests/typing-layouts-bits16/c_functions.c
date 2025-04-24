#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


static int16_t Int16_val(value v) {
  return (int16_t)Long_val(v);
}

static value caml_copy_int16(int16_t x) {
  return Val_long(x);
}

CAMLprim int16_t lognot_UtoU(int16_t u) {
  return ~u;
}

CAMLprim int16_t lognot_BtoU(value u) {
  return ~Int16_val(u);
}

CAMLprim value lognot_UtoB(int16_t u) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_int16(~u);
  CAMLreturn(result);
}

CAMLprim value lognot_bytecode(value u) {
  CAMLparam1(u);
  CAMLlocal1(result);
  result = caml_copy_int16(~Int16_val(u));
  CAMLreturn(result);
}

CAMLprim int16_t sum_7_UBUBUBUtoU(int16_t u1, value b2, int16_t u3, value b4,
                                  int16_t u5, value b6, int16_t u7) {
  int16_t u2 = Int16_val(b2);
  int16_t u4 = Int16_val(b4);
  int16_t u6 = Int16_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  if (argn != 7) CAMLreturn(caml_copy_int16(-1));
  int16_t u1 = Int16_val(argv[0]);
  int16_t u2 = Int16_val(argv[1]);
  int16_t u3 = Int16_val(argv[2]);
  int16_t u4 = Int16_val(argv[3]);
  int16_t u5 = Int16_val(argv[4]);
  int16_t u6 = Int16_val(argv[5]);
  int16_t u7 = Int16_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_int16(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
