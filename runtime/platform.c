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
#ifdef HAS_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif

#include "caml/alloc.h"
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

void caml_plat_assert_all_locks_unlocked(void)
{
#ifdef DEBUG
  if (lockdepth) caml_fatal_error("Locks still locked at termination");
#endif
}

void caml_plat_mutex_free(caml_plat_mutex* m)
{
  check_err("mutex_free", pthread_mutex_destroy(m));
}

static void caml_plat_cond_init_aux(caml_plat_cond *cond)
{
  custom_condvar_init(&cond->cond);
}

/* Condition variables */
void caml_plat_cond_init(caml_plat_cond* cond, caml_plat_mutex* m)
{
  caml_plat_cond_init_aux(cond);
  cond->mutex = m;
}

void caml_plat_wait(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("wait", custom_condvar_wait(&cond->cond, cond->mutex));
}

void caml_plat_broadcast(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("cond_broadcast", custom_condvar_broadcast(&cond->cond));
}

void caml_plat_signal(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("cond_signal", custom_condvar_signal(&cond->cond));
}

void caml_plat_cond_free(caml_plat_cond* cond)
{
  check_err("cond_free", custom_condvar_destroy(&cond->cond));
  cond->mutex=0;
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

<<<<<<< HEAD
void* caml_mem_map(uintnat size, int reserve_only)
||||||| 121bedcfd2
#ifdef DEBUG
static struct lf_skiplist mmap_blocks = {NULL};
#endif

#ifndef _WIN32
#endif

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
=======
#ifdef DEBUG
static struct lf_skiplist mmap_blocks = {NULL};
#endif

#ifndef _WIN32
#endif

void* caml_mem_map(uintnat size, int reserve_only)
>>>>>>> 5.2.0
{
<<<<<<< HEAD
  void* mem = caml_plat_mem_map(size, reserve_only);
||||||| 121bedcfd2
  CAMLassert(Is_power_of_2(alignment));
  CAMLassert(Is_page_aligned(size));
  alignment = round_up(alignment, caml_plat_mmap_alignment);

#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acq(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

  void* mem = caml_plat_mem_map(size, alignment, reserve_only);
=======
#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acquire(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

  void* mem = caml_plat_mem_map(size, reserve_only);
>>>>>>> 5.2.0

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

unsigned caml_plat_spin_wait(unsigned spins,
                             const char* file, int line,
                             const char* function)
{
  unsigned next_spins;
  if (spins < Min_sleep_ns) spins = Min_sleep_ns;
  if (spins > Max_sleep_ns) spins = Max_sleep_ns;
  next_spins = spins + spins / 4;
  if (spins < Slow_sleep_ns && Slow_sleep_ns <= next_spins) {
    caml_gc_log("Slow spin-wait loop in %s at %s:%d", function, file, line);
  }
#ifdef _WIN32
  Sleep(spins/1000000);
#else
  usleep(spins/1000);
#endif
  return next_spins;
}
