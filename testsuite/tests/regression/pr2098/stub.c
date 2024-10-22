#include <caml/custom.h>
#include <caml/signals.h>

static void caml_test_finalize(value v)
{
  caml_enter_blocking_section();
  caml_leave_blocking_section();
}

static struct custom_operations caml_test_ops = {
  "_test",
  caml_test_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value caml_test_alloc(value unit)
{
  return caml_alloc_custom(&caml_test_ops, 0, 0, 1);
}