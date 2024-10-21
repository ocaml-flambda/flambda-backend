/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Stephen Dolan, University of Cambridge               */
/*                                                                        */
/*   Copyright 2016 Indian Institute of Technology, Madras                */
/*   Copyright 2016 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#define CAML_INTERNALS

#include "caml/config.h"
#include <string.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <errno.h>
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/fail.h"
#include "caml/signals.h"
#ifdef HAS_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif

#include "caml/alloc.h"
#include "caml/lf_skiplist.h"
#include "sync_posix.h"

#ifdef _WIN32
/* CR ocaml 5 compactor:

   The runtime does not currently guarantee that memory is released to the OS in
   the same block sizes as it was allocated, making it incompatible with
   Windows.

   This incompatibility arises from the batch-mmap patch at:

       https://github.com/ocaml-flambda/flambda-backend/pull/2248

   which does large memory allocations to acquire new pools. However, the
   compactor releases pools one at a time. Until the compactor is updated
   to be aware of large mappings, this will not work on Windows.

   So, for now, Windows compatibility is broken. The assertions ensuring that
   mapping and unmapping sizes agree (ocaml/ocaml PR#10908) have been reverted,
   and should be restored once the compactor is updated */
#error "Windows compatibility currently broken due to mmap sizing"
#endif

/* Error reporting */

void caml_plat_fatal_error(const char * action, int err)
{
  char buf[1024];
  caml_fatal_error("Fatal error during %s: %s\n",
                   action, caml_strerror(err, buf, sizeof(buf)));
}

/* Mutexes */

CAMLexport void caml_plat_mutex_init(caml_plat_mutex * m)
{
  int rc;
  pthread_mutexattr_t attr;
  rc = pthread_mutexattr_init(&attr);
  if (rc != 0) goto error1;
  rc = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  if (rc != 0) goto error2;
  rc = pthread_mutex_init(m, &attr);
  // fall through
error2:
  pthread_mutexattr_destroy(&attr);
error1:
  check_err("mutex_init", rc);
}

void caml_plat_assert_locked(caml_plat_mutex* m)
{
#ifdef DEBUG
  int r = pthread_mutex_trylock(m);
  if (r == EBUSY) {
    /* ok, it was locked */
    return;
  } else if (r == 0) {
    caml_fatal_error("Required mutex not locked");
  } else {
    check_err("assert_locked", r);
  }
#endif
}

CAMLexport CAMLthread_local int caml_lockdepth = 0;

void caml_plat_assert_all_locks_unlocked(void)
{
#ifdef DEBUG
  if (caml_lockdepth) caml_fatal_error("Locks still locked at termination");
#endif
}

CAMLexport void caml_plat_lock_non_blocking_actual(caml_plat_mutex* m)
{
  /* Avoid exceptions */
  caml_enter_blocking_section_no_pending();
  int rc = pthread_mutex_lock(m);
  caml_leave_blocking_section();
  check_err("lock_non_blocking", rc);
  DEBUG_LOCK(m);
}

void caml_plat_mutex_free(caml_plat_mutex* m)
{
  check_err("mutex_free", pthread_mutex_destroy(m));
}

static void caml_plat_cond_init_aux(caml_plat_cond *cond)
{
  custom_condvar_init(cond);
}

/* Condition variables */
void caml_plat_cond_init(caml_plat_cond* cond)
{
  caml_plat_cond_init_aux(cond);
}

void caml_plat_wait(caml_plat_cond* cond, caml_plat_mutex* mut)
{
  caml_plat_assert_locked(mut);
  check_err("wait", custom_condvar_wait(cond, mut));
}

void caml_plat_broadcast(caml_plat_cond* cond)
{
  check_err("cond_broadcast", custom_condvar_broadcast(cond));
}

void caml_plat_signal(caml_plat_cond* cond)
{
  check_err("cond_signal", custom_condvar_signal(cond));
}

void caml_plat_cond_free(caml_plat_cond* cond)
{
  check_err("cond_free", custom_condvar_destroy(cond));
}

/* Futexes */

#ifdef CAML_PLAT_FUTEX_FALLBACK

/* Condition-variable-based futex implementation, for when a native OS
   version isn't available. This also illustrates the semantics of the
   [wait()] and [wake_all()] operations. */

void caml_plat_futex_wait(caml_plat_futex* futex,
                          caml_plat_futex_value undesired) {
  caml_plat_lock_blocking(&futex->mutex);
  while (atomic_load_acquire(&futex->value) == undesired) {
    caml_plat_wait(&futex->cond, &futex->mutex);
  }
  caml_plat_unlock(&futex->mutex);
}

void caml_plat_futex_wake_all(caml_plat_futex* futex) {
  caml_plat_lock_blocking(&futex->mutex);
  caml_plat_broadcast(&futex->cond);
  caml_plat_unlock(&futex->mutex);
}

void caml_plat_futex_init(caml_plat_futex* ftx, caml_plat_futex_value value) {
  ftx->value = value;
  caml_plat_mutex_init(&ftx->mutex);
  caml_plat_cond_init(&ftx->cond);
}

void caml_plat_futex_free(caml_plat_futex* ftx) {
  caml_plat_mutex_free(&ftx->mutex);
  caml_plat_cond_free(&ftx->cond);
}

#else /* ! CAML_PLAT_FUTEX_FALLBACK */

/* Platform-specific futex implementation.

   For each platform we define [WAIT(futex_word* ftx, futex_value
   undesired)] and [WAKE(futex_word* ftx)] in terms of
   platform-specific syscalls. The exact semantics vary, but these are
   the weakest expected guarantees:

   - [WAIT()] compares the value at [ftx] to [undesired], and if they
     are equal, goes to sleep on [ftx].

   - [WAKE()] wakes up all [WAIT()]-ers on [ftx].

   - [WAIT()] must be atomic with respect to [WAKE()], in that if the
     [WAIT()]-ing thread observes the undesired value and goes to
     sleep, it will not miss a wakeup from the [WAKE()]-ing thread
     between the comparison and sleep.

   - [WAIT()]'s initial read of [ftx] is to be treated as being atomic
     with [memory_order_relaxed]. That is, no memory ordering is
     guaranteed around it.

   - Spurious wakeups of [WAIT()] may be possible.
*/

#  if defined(_WIN32)
#    include <synchapi.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)  \
  WaitOnAddress((volatile void *)ftx, &undesired, \
                sizeof(undesired), INFINITE)
#    define CAML_PLAT_FUTEX_WAKE(ftx)           \
  WakeByAddressAll((void *)ftx)

#  elif defined(__linux__)
#    include <linux/futex.h>
#    include <sys/syscall.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)    \
  syscall(SYS_futex, ftx, FUTEX_WAIT_PRIVATE,       \
          /* expected */ undesired,                 \
          /* timeout */ NULL,                       \
          /* ignored */ NULL, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)           \
  syscall(SYS_futex, ftx, FUTEX_WAKE_PRIVATE,   \
          /* count */ INT_MAX,                  \
          /* timeout */ NULL,                   \
          /* ignored */ NULL, 0)

#  elif 0 /* defined(__APPLE__)
   macOS has [__ulock_(wait|wake)()] which is used in implementations
   of libc++, (e.g. by LLVM) but the API is private and unstable.
   Therefore, we currently use the condition variable fallback on
   macOS. */

#  elif defined(__FreeBSD__)
#    include <sys/umtx.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired) \
  _umtx_op(ftx, UMTX_OP_WAIT_UINT_PRIVATE,       \
           /* expected */ undesired,             \
           /* timeout */ NULL, NULL)
#    define CAML_PLAT_FUTEX_WAKE(ftx) \
  _umtx_op(ftx, UMTX_OP_WAKE_PRIVATE, \
           /* count */ INT_MAX,       \
           /* unused */ NULL, NULL)

#  elif defined(__OpenBSD__)
#    include <sys/futex.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)      \
  futex((volatile uint32_t*)ftx, FUTEX_WAIT_PRIVATE,  \
        /* expected */ undesired,                     \
        /* timeout */ NULL,                           \
        /* ignored */ NULL)
#    define CAML_PLAT_FUTEX_WAKE(ftx)                \
  futex((volatile uint32_t*)ftx, FUTEX_WAKE_PRIVATE, \
        /* count */ INT_MAX,                         \
        /* ignored */ NULL, NULL)

#  elif 0 /* defined(__NetBSD__)
   TODO The following code for NetBSD is untested,
   we currently use the fallback instead. */
#    include <sys/futex.h>
#    include <sys/syscall.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)    \
  syscall(SYS___futex, ftx,                         \
          FUTEX_WAIT | FUTEX_PRIVATE_FLAG,          \
          /* expected */ undesired,                 \
          /* timeout */ NULL,                       \
          /* ignored */ NULL, 0, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)            \
  sycall(SYS___futex, ftx,                       \
         FUTEX_WAKE | FUTEX_PRIVATE_FLAG,        \
         /* count */ INT_MAX,                    \
         /* ignored */ NULL, NULL, 0, 0)

#  elif 0 /* defined(__DragonFly__)
   TODO The following code for DragonFly is untested,
   we currently use the fallback instead. */ */
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)        \
  umtx_sleep((volatile const int*)ftx, undesired, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)               \
  umtx_wakeup((volatile const int*)ftx, INT_MAX)

#  else
#    error "No futex implementation available"
#  endif

void caml_plat_futex_wait(caml_plat_futex* ftx,
                          caml_plat_futex_value undesired) {
  while (atomic_load_acquire(&ftx->value) == undesired) {
    CAML_PLAT_FUTEX_WAIT(&ftx->value, undesired);
  }
}

void caml_plat_futex_wake_all(caml_plat_futex* ftx) {
  CAML_PLAT_FUTEX_WAKE(&ftx->value);
}

void caml_plat_futex_init(caml_plat_futex* ftx,
                          caml_plat_futex_value value) {
  ftx->value = value;
}

void caml_plat_futex_free(caml_plat_futex* ftx) {
  (void) ftx; /* noop */
}

#endif /* CAML_PLAT_FUTEX_FALLBACK */

/* Latches */

void caml_plat_latch_release(caml_plat_binary_latch* latch) {
  /* if nobody is blocking, release in user-space */
  if (atomic_exchange(&latch->value, Latch_released)
      != Latch_unreleased) {
    /* at least one thread is (going to be) blocked on the futex, notify */
    caml_plat_futex_wake_all(latch);
  }
}

Caml_inline void latchlike_wait(caml_plat_futex *ftx,
                                caml_plat_futex_value unreleased,
                                caml_plat_futex_value contested) {
  /* indicate that we are about to block */
  caml_plat_futex_value expected = unreleased;
  (void)atomic_compare_exchange_strong
    (&ftx->value, &expected, contested);
  /* ftx is either already released (neither [unreleased] nor
     [contested]), or we are going to block (== [contested]),
     [futex_wait()] here will take care of both */
  caml_plat_futex_wait(ftx, contested);
}

void caml_plat_latch_wait(caml_plat_binary_latch* latch) {
  latchlike_wait(latch, Latch_unreleased, Latch_contested);
}

/* Sense-reversing barrier */
/* futex states:
   - X...0 if nobody is blocking (but they may be spinning)
   - X...1 if anybody is blocking (or about to)

   where X is the sense bit
 */

void caml_plat_barrier_flip(caml_plat_barrier* barrier,
                            barrier_status current_sense) {
  uintnat new_sense = current_sense ^ BARRIER_SENSE_BIT;
  atomic_store_relaxed(&barrier->arrived, new_sense);
  /* if a thread observes the flip below, it will also observe the
     reset counter, since any currently waiting threads will check the
     futex before leaving, they will see the counter correctly */

  caml_plat_futex_value
    current_sense_word = (caml_plat_futex_value) current_sense,
    new_sense_word = (caml_plat_futex_value) new_sense;

  /* if nobody is blocking, flip in user-space */
  if (atomic_exchange(&barrier->futex.value, new_sense_word)
      != current_sense_word) {
    /* a thread is (about to be) blocked, notify */
    caml_plat_futex_wake_all(&barrier->futex);
  }
}

void caml_plat_barrier_wait_sense(caml_plat_barrier* barrier,
                                  barrier_status sense_bit) {
  latchlike_wait(&barrier->futex, sense_bit, sense_bit | 1);
}

/* Memory management */

static uintnat round_up(uintnat size, uintnat align) {
  CAMLassert(Is_power_of_2(align));
  return (size + align - 1) & ~(align - 1);
}

intnat caml_plat_pagesize = 0;
intnat caml_plat_mmap_alignment = 0;

uintnat caml_mem_round_up_pages(uintnat size)
{
  return round_up(size, caml_plat_pagesize);
}

#define Is_page_aligned(size) ((size & (caml_plat_pagesize - 1)) == 0)

void* caml_mem_map(uintnat size, int reserve_only)
{
  void* mem = caml_plat_mem_map(size, reserve_only);

  if (mem == 0) {
    caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d bytes failed",
                            size);
    return 0;
  }

  caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);

  return mem;
}

void* caml_mem_commit(void* mem, uintnat size)
{
  CAMLassert(Is_page_aligned(size));
  caml_gc_message(0x1000, "commit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);
  return caml_plat_mem_commit(mem, size);
}

void caml_mem_decommit(void* mem, uintnat size)
{
  if (size) {
    caml_gc_message(0x1000, "decommit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                            " bytes at %p for heaps\n", size, mem);
    caml_plat_mem_decommit(mem, size);
  }
}

void caml_mem_unmap(void* mem, uintnat size)
{
  caml_gc_message(0x1000, "munmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);
  caml_plat_mem_unmap(mem, size);
}

#define Min_sleep_ns       10000 // 10 us
#define Slow_sleep_ns    1000000 //  1 ms
#define Max_sleep_ns  1000000000 //  1 s

unsigned caml_plat_spin_back_off(unsigned sleep_ns,
                                 const struct caml_plat_srcloc* loc)
{
  if (sleep_ns < Min_sleep_ns) sleep_ns = Min_sleep_ns;
  if (sleep_ns > Max_sleep_ns) sleep_ns = Max_sleep_ns;
  unsigned next_sleep_ns = sleep_ns + sleep_ns / 4;
  if (sleep_ns < Slow_sleep_ns && Slow_sleep_ns <= next_sleep_ns) {
    caml_gc_log("Slow spin-wait loop in %s at %s:%d",
                loc->function, loc->file, loc->line);
  }
#ifdef _WIN32
  Sleep(sleep_ns/1000000);
#else
  usleep(sleep_ns/1000);
#endif
  return next_sleep_ns;
}
