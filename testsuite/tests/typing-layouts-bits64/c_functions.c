#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim int64_t lognot_UtoU(int64_t u) {
  return ~u;
}

CAMLprim int64_t lognot_BtoU(value u) {
  return ~Int64_val(u);
}

CAMLprim value lognot_UtoB(int64_t u) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_int64(~u);
  CAMLreturn(result);
}

CAMLprim value lognot_bytecode(value u) {
  CAMLparam1(u);
  CAMLlocal1(result);
  result = caml_copy_int64(~Int64_val(u));
  CAMLreturn(result);
}

CAMLprim int64_t sum_7_UBUBUBUtoU(int64_t u1, value b2, int64_t u3, value b4,
                                  int64_t u5, value b6, int64_t u7) {
  int64_t u2 = Int64_val(b2);
  int64_t u4 = Int64_val(b4);
  int64_t u6 = Int64_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  if (argn != 7) CAMLreturn(caml_copy_int64(INT64_LITERAL(-1)));
  int64_t u1 = Int64_val(argv[0]);
  int64_t u2 = Int64_val(argv[1]);
  int64_t u3 = Int64_val(argv[2]);
  int64_t u4 = Int64_val(argv[3]);
  int64_t u5 = Int64_val(argv[4]);
  int64_t u6 = Int64_val(argv[5]);
  int64_t u7 = Int64_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_int64(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
