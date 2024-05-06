/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <unistd.h>
#define __USE_GNU
#include <sys/ucontext.h>

/* Signal handling, code specific to the native-code compiler */

#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/frame_descriptors.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/stack.h"

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to OCaml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection(void)
{
  frame_descr* d;
  caml_domain_state * dom_st = Caml_state;
  caml_frame_descrs fds = caml_get_frame_descrs();
  struct stack_info* stack = dom_st->current_stack;

  char * sp = (char*)stack->sp;
  Pop_frame_pointer(sp);
  uintnat retaddr = *(uintnat*)sp;

  /* Synchronise for the case when [young_limit] was used to interrupt
     us. */
  atomic_thread_fence(memory_order_acquire);

  { /* Find the frame descriptor for the current allocation */
    d = caml_find_frame_descr(fds, retaddr);
    /* Must be an allocation frame */
    CAMLassert(d && !frame_return_to_C(d) && frame_has_allocs(d));
  }

  { /* Compute the total allocation size at this point,
       including allocations combined by Comballoc */
    unsigned char* alloc_len = frame_end_of_live_ofs(d);
    int i, nallocs = *alloc_len++;
    intnat allocsz = 0;

    if (nallocs == 0) {
      /* This is a poll */
      caml_process_pending_actions();
      return;
    }
    else
    {
      for (i = 0; i < nallocs; i++) {
        allocsz += Whsize_wosize(Wosize_encoded_alloc_len(alloc_len[i]));
      }
      /* We have computed whsize (including header)
         but need wosize (without) */
      allocsz -= 1;
    }

    caml_alloc_small_dispatch(dom_st, allocsz, CAML_DO_TRACK | CAML_FROM_CAML,
                              nallocs, alloc_len);
  }
}

#if defined(NATIVE_CODE) && !defined(STACK_CHECKS_ENABLED)

#if !defined(POSIX_SIGNALS)
#error "stack checks cannot be disabled if POSIX signals are not available"
#endif

#define DECLARE_SIGNAL_HANDLER(name) \
  static void name(int sig, siginfo_t * info, ucontext_t * context)

#define SET_SIGACT(sigact,name)                                       \
  sigact.sa_sigaction = (void (*)(int,siginfo_t *,void *)) (name);    \
  sigact.sa_flags = SA_SIGINFO

CAMLextern void caml_raise_stack_overflow_nat(void);

DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct sigaction act;
  struct stack_info *block = Caml_state->current_stack;
  char* fault_addr = info->si_addr;
  int page_size = getpagesize();
  char* protected_low = (char *) block + page_size;
  char* protected_high = protected_low + page_size;
  if ((fault_addr >= protected_low) && (fault_addr < protected_high)) {
    context->uc_mcontext.gregs[REG_RIP]= (greg_t) &caml_raise_stack_overflow_nat;
  } else {
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
  }
}

void caml_init_nat_signals(void)
{
  struct sigaction act;
  SET_SIGACT(act, segv_handler);
  act.sa_flags |= SA_ONSTACK | SA_NODEFER;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, NULL);
}

#else

void caml_init_nat_signals(void)
{
}

#endif
