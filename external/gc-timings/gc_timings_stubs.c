#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/gc.h>

#ifdef _WIN32
#include <wtypes.h>
#include <process.h>
#elif defined(HAS_UNISTD)
#include <unistd.h>
#endif

#ifdef HAS_MACH_ABSOLUTE_TIME
#include <mach/mach_time.h>
#elif HAS_POSIX_MONOTONIC_CLOCK
#include <time.h>
#endif

/* [time_counter] is extracted from runtime/eventlog.c */
int64_t time_counter(void)
{
#ifdef _WIN32
  static double clock_freq = 0;
  static LARGE_INTEGER now;

  if (clock_freq == 0) {
    LARGE_INTEGER f;
    if (!QueryPerformanceFrequency(&f))
      return 0;
    clock_freq = (1000000000.0 / f.QuadPart);
  };

  if (!QueryPerformanceCounter(&now))
    return 0;
  return (int64_t)(now.QuadPart * clock_freq);

#elif defined(HAS_MACH_ABSOLUTE_TIME)
  static mach_timebase_info_data_t time_base = {0};
  uint64_t now;

  if (time_base.denom == 0) {
    if (mach_timebase_info (&time_base) != KERN_SUCCESS)
      return 0;

    if (time_base.denom == 0)
      return 0;
  }

  now = mach_absolute_time ();
  return (int64_t)((now * time_base.numer) / time_base.denom);

#elif defined(HAS_POSIX_MONOTONIC_CLOCK)
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return
    (int64_t)t.tv_sec  * (int64_t)1000000000 +
    (int64_t)t.tv_nsec;
#else
  return 0;
#endif
}

int64_t caml_timing_major_gc = 0.;
int64_t caml_timing_minor_gc = 0.;

void caml_timing_collect_gc_minor_begin_hook() {
  caml_timing_minor_gc -= time_counter();
}

void caml_timing_collect_gc_minor_end_hook() {
  caml_timing_minor_gc += time_counter();
}

void caml_timing_collect_gc_major_begin_hook() {
  caml_timing_major_gc -= time_counter();
}

void caml_timing_collect_gc_major_end_hook() {
  caml_timing_major_gc += time_counter();
}

CAMLprim value caml_timing_collect_gc(value unit) {
  CAMLparam1 (unit);

  caml_major_slice_begin_hook = caml_timing_collect_gc_major_begin_hook;
  caml_major_slice_end_hook = caml_timing_collect_gc_major_end_hook;

  caml_minor_gc_begin_hook = caml_timing_collect_gc_minor_begin_hook;
  caml_minor_gc_end_hook = caml_timing_collect_gc_minor_end_hook;
  CAMLreturn (Val_unit);
}

CAMLprim value caml_timing_gc_time_spent_minor(value unit) {
  CAMLparam1 (unit);
  CAMLreturn (caml_copy_double (caml_timing_minor_gc));
}

CAMLprim value caml_timing_gc_time_spent_major(value unit) {
  CAMLparam1 (unit);
  CAMLreturn (caml_copy_double (caml_timing_major_gc));
}
