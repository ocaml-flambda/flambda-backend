#define CAML_INTERNALS

#include <caml/custom.h>

static void caml_test_finalize(value v)
{
  if (Caml_state -> _in_minor_collection)
    caml_fatal_error("Thread switch from inside minor GC");
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