#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim int32_t lognot_UtoU(int32_t u) {
  return ~u;
}

CAMLprim int32_t lognot_BtoU(value u) {
  return ~Int32_val(u);
}

CAMLprim value lognot_UtoB(int32_t u) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_int32(~u);
  CAMLreturn(result);
}

CAMLprim value lognot_bytecode(value u) {
  CAMLparam1(u);
  CAMLlocal1(result);
  result = caml_copy_int32(~Int32_val(u));
  CAMLreturn(result);
}

CAMLprim int32_t sum_7_UBUBUBUtoU(int32_t u1, value b2, int32_t u3, value b4,
                                  int32_t u5, value b6, int32_t u7) {
  int32_t u2 = Int32_val(b2);
  int32_t u4 = Int32_val(b4);
  int32_t u6 = Int32_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  if (argn != 7) CAMLreturn(caml_copy_int32(-1));
  int32_t u1 = Int32_val(argv[0]);
  int32_t u2 = Int32_val(argv[1]);
  int32_t u3 = Int32_val(argv[2]);
  int32_t u4 = Int32_val(argv[3]);
  int32_t u5 = Int32_val(argv[4]);
  int32_t u6 = Int32_val(argv[5]);
  int32_t u7 = Int32_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_int32(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
