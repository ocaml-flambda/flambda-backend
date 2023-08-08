#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim intnat lognot_UtoU(intnat u) {
  return ~u;
}

CAMLprim intnat lognot_BtoU(value u) {
  return ~Nativeint_val(u);
}

CAMLprim value lognot_UtoB(intnat u) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_nativeint(~u);
  CAMLreturn(result);
}

CAMLprim value lognot_bytecode(value u) {
  CAMLparam1(u);
  CAMLlocal1(result);
  result = caml_copy_nativeint(~Nativeint_val(u));
  CAMLreturn(result);
}

CAMLprim intnat sum_7_UBUBUBUtoU(intnat u1, value b2, intnat u3, value b4,
                                 intnat u5, value b6, intnat u7) {
  intnat u2 = Nativeint_val(b2);
  intnat u4 = Nativeint_val(b4);
  intnat u6 = Nativeint_val(b6);
  return (u1 + u2 + u3 + u4 + u5 + u6 + u7);
}

CAMLprim value sum_7_bytecode(value* argv, int argn) {
  CAMLparam0();
  CAMLassert(argn == 7);
  if (argn != 7) CAMLreturn(caml_copy_nativeint(-1));
  intnat u1 = Nativeint_val(argv[0]);
  intnat u2 = Nativeint_val(argv[1]);
  intnat u3 = Nativeint_val(argv[2]);
  intnat u4 = Nativeint_val(argv[3]);
  intnat u5 = Nativeint_val(argv[4]);
  intnat u6 = Nativeint_val(argv[5]);
  intnat u7 = Nativeint_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_nativeint(u1 + u2 + u3 + u4 + u5 + u6 + u7);
  CAMLreturn(result);
}
