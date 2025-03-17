#include <assert.h>
#include <smmintrin.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/simd.h>

#define DO_NOT_SANITIZE __attribute__((no_sanitize("address")))

CAMLprim value ocaml_address_sanitizer_test_alloc(size_t len, int64_t tag) {
  assert(len > 0);
  assert(!Scannable_tag(tag));
  header_t *block = malloc((len * sizeof(value)) + sizeof(header_t));
  *block = Caml_out_of_heap_header(/*wosize=*/len, /*tag=*/tag);
  return Val_hp(block);
}

CAMLprim value ocaml_address_sanitizer_test_free(value block) {
  // The explicit cast to [void*] here is necessary to avoid compiler warnings
  // under runtime5.
  free((void *)(Hp_val(block)));
  return Val_unit;
}

CAMLprim __m128i ocaml_address_sanitizer_test_vec128_of_int64s(int64_t low,
                                                               int64_t high) {
  return _mm_set_epi64x(high, low);
}

CAMLprim value DO_NOT_SANITIZE caml_prefetch_read_low(const void *ptr) {
  __builtin_prefetch(ptr, /*is_write=*/0, /*locality=*/1);
  return Val_unit;
}

CAMLprim value DO_NOT_SANITIZE caml_prefetch_write_low(const void *ptr) {
  __builtin_prefetch(ptr, /*is_write=*/1, /*locality=*/1);
  return Val_unit;
}

CAMLprim value DO_NOT_SANITIZE caml_cldemote(const void *ptr) {
#if (defined(__i386__) || defined(__x86_64__))
  asm volatile("cldemote (%0)\n" : : "r"(ptr));
#endif
  return Val_unit;
}

static char *DO_NOT_SANITIZE bigstring_element_at_pos(value v_bstr, intnat pos) {
  return ((char *)Caml_ba_data_val(v_bstr)) + pos;
}

CAMLprim intnat DO_NOT_SANITIZE caml_bigstring_fetch_and_add_int_untagged(value v_bstr, intnat pos, intnat n) {
  intnat *decode = (intnat *)bigstring_element_at_pos(v_bstr, pos);
  return __sync_fetch_and_add(decode, n);
}
