#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value get_register(long x) {
  return caml_copy_nativeint(x);
}

CAMLprim value get_register_bytecode(value x) {
  return get_register(Long_val(x));
}
