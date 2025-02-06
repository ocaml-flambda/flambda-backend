#include <caml/config.h>
#include <caml/mlvalues.h>

#ifdef WITH_ADDRESS_SANITIZER

#define CREATE_ASAN_REPORT_WRAPPER(memory_access, size) \
void __asan_report_ ## memory_access ## size ## _noabort(const void* addr); \
CAMLexport void __attribute__((preserve_all)) caml_asan_report_ ## memory_access ## size ## _noabort(const void* addr) { \
  return __asan_report_ ## memory_access ## size ## _noabort(addr); \
}

CREATE_ASAN_REPORT_WRAPPER(load, 1)
CREATE_ASAN_REPORT_WRAPPER(load, 2)
CREATE_ASAN_REPORT_WRAPPER(load, 4)
CREATE_ASAN_REPORT_WRAPPER(load, 8)
CREATE_ASAN_REPORT_WRAPPER(load, 16)
CREATE_ASAN_REPORT_WRAPPER(store, 1)
CREATE_ASAN_REPORT_WRAPPER(store, 2)
CREATE_ASAN_REPORT_WRAPPER(store, 4)
CREATE_ASAN_REPORT_WRAPPER(store, 8)
CREATE_ASAN_REPORT_WRAPPER(store, 16)

#undef CREATE_ASAN_REPORT_WRAPPER

#else
// Prevents triggering [-Wempty-translation-unit], which is enabled by [-Wpedantic].
static void __attribute__((used)) unused_function_to_avoid_c_compiler_warning(void) {}
#endif
