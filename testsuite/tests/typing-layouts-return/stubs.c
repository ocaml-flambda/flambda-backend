#include <stdint.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalue.h"

typedef struct {
  uint64_t a;
  uint64_t b;
} ui64_ui64_struct;

ui64_ui64_struct ui64_ui64_make(void) {
  uint64_t a = 123;
  uint64_t b = 456;
  ui64_ui64_struct res = { a, b };
  return res;
}

value ui64_ui64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int64(123);
  b = caml_copy_int64(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  uint64_t a;
  double b;
} ui64_f64_struct;

ui64_f64_struct ui64_f64_make(void) {
  uint64_t a = 123;
  double b = 456;
  ui64_f64_struct res = { a, b };
  return res;
}

value ui64_f64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int64(123);
  b = caml_copy_double(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  double a;
  uint64_t b;
} f64_ui64_struct;

f64_ui64_struct f64_ui64_make(void) {
  double a = 123;
  uint64_t b = 456;
  f64_ui64_struct res = { a, b };
  return res;
}

value f64_ui64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(123);
  b = caml_copy_int64(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  double a;
  double b;
} f64_f64_struct;

f64_f64_struct f64_f64_make(void) {
  double a = 123;
  double b = 456;
  f64_f64_struct res = { a, b };
  return res;
}

value f64_f64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(123);
  b = caml_copy_double(456);
  res = caml_alloc_float_array(2);
  Store_double_array_field(res, 0, a);
  Store_double_array_field(res, 1, b);
  CAMLreturn (res);
}
