#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim intnat lognot_UtoU(intnat u) {
  return ~u;
}

CAMLprim intnat lognot_BtoU(value u) {
  return ~Int64_val(u);
}

CAMLprim value lognot_UtoB(intnat u) {
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

CAMLprim double sum_7_UBUBUBUtoU(intnat u1, value b2, intnat u3, value b4,
                                 intnat u5, value b6, intnat u7) {
  intnat u2 = Int64_val(b2);
  intnat u4 = Int64_val(b4);
  intnat u6 = Int64_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  intnat u1 = Int64_val(argv[0]);
  intnat u2 = Int64_val(argv[1]);
  intnat u3 = Int64_val(argv[2]);
  intnat u4 = Int64_val(argv[3]);
  intnat u5 = Int64_val(argv[4]);
  intnat u6 = Int64_val(argv[5]);
  intnat u7 = Int64_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_int64(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
