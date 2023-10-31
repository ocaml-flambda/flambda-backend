#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <math.h>

CAMLprim double sin_U_U (double u1) {
  return sin(u1);
}

CAMLprim double sin_B_U (value u1) {
  return sin(Double_val (u1));
}

CAMLprim value sin_U_B (double u1) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_copy_double(sin(u1));
  CAMLreturn(result);
}

CAMLprim value sin_byte (value u1) {
  CAMLparam1(u1);
  CAMLlocal1(result);
  result = caml_copy_double (sin_U_U (Double_val (u1)));
  CAMLreturn(result);
}

CAMLprim double sum_7(double x1, value x2b, double x3, value x4b, double x5,
                      value x6b, double x7) {
  double x2 = Double_val(x2b);
  double x4 = Double_val(x4b);
  double x6 = Double_val(x6b);
  return (x1 + x2 + x3 + x4 + x5 + x6 + x7);
}

CAMLprim value sum_7_byte(value* argv, int argn) {
  CAMLparam0();
  double x1 = Double_val(argv[0]);
  double x2 = Double_val(argv[1]);
  double x3 = Double_val(argv[2]);
  double x4 = Double_val(argv[3]);
  double x5 = Double_val(argv[4]);
  double x6 = Double_val(argv[5]);
  double x7 = Double_val(argv[6]);
  CAMLlocal1(result);
  result = caml_copy_double(x1 + x2 + x3 + x4 + x5 + x6 + x7);
  CAMLreturn(result);
}

