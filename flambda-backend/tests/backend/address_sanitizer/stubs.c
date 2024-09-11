#include <stdlib.h>
#include <assert.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/gc.h>

CAMLprim value ocaml_address_sanitizer_test_alloc(value v_len) {
  int64_t len = Long_val(v_len);
  assert(len > 0);
  header_t* block = malloc((len * sizeof(value)) + sizeof(header_t));
  *block = Caml_out_of_heap_header(/*wosize=*/len, /*tag=*/Abstract_tag);
  return Val_hp(block);
}

CAMLprim value ocaml_address_sanitizer_test_free(value block) {
  free(Hp_val(block));
  return Val_unit;
}
