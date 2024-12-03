#include <stdint.h>

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
