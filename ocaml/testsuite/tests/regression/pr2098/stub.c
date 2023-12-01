#define CAML_INTERNALS

#include <caml/custom.h>

static void caml_test_finalize(value v)
{
  if (Caml_state -> in_minor_collection)
    caml_fatal_error("Thread switch from inside minor GC");
}

static void caml_test_serialize(value v,
                                uintnat * wsize_32,
                                uintnat * wsize_64)
{
  *wsize_32 = 0;
  *wsize_64 = 0;
}

uintnat caml_test_deserialize(void * dst)
{
  return 0;
}

static struct custom_operations caml_test_ops = {
  "_test",
  caml_test_finalize,
  custom_compare_default,
  custom_hash_default,
  caml_test_serialize,
  caml_test_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value caml_test_alloc(value unit)
{
  return caml_alloc_custom(&caml_test_ops, 0, 0, 1);
}

CAMLprim value caml_test_init(value unit)
{
  caml_register_custom_operations(&caml_test_ops);
  return Val_unit;
}
