#include <assert.h>
#include <smmintrin.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/simd.h>

CAMLprim value ocaml_address_sanitizer_test_alloc(size_t len, int64_t tag) {
  assert(len > 0);
  assert(tag >= No_scan_tag);
  header_t* block = malloc((len * sizeof(value)) + sizeof(header_t));
  *block = Caml_out_of_heap_header(/*wosize=*/len, /*tag=*/tag);
  return Val_hp(block);
}

CAMLprim value ocaml_address_sanitizer_test_free(value block) {
  // The explicit cast to [void*] here is necessary to avoid compiler warnings
  // under runtime5.
  free((void*)(Hp_val(block)));
  return Val_unit;
}

CAMLprim __m128i ocaml_address_sanitizer_test_vec128_of_int64s(int64_t low, int64_t high) {
  return _mm_set_epi64x(high, low);
}
