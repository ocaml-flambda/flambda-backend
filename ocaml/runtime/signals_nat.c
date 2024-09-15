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

#include <unistd.h>
#define __USE_GNU
#include <sys/ucontext.h>

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

#define DECLARE_SIGNAL_HANDLER(name) \
  static void name(int sig, siginfo_t * info, ucontext_t * context)

#define SET_SIGACT(sigact,name)                                       \
  sigact.sa_sigaction = (void (*)(int,siginfo_t *,void *)) (name);    \
  sigact.sa_flags = SA_SIGINFO

#if !defined(STACK_CHECKS_ENABLED)
CAMLextern void caml_raise_stack_overflow_nat(void);
#endif

#if defined(NEW_SAFEPOINTS_STRATEGY)

// There is currently only an x86-64 implementation of the new strategy.

CAMLextern void caml_call_gc(void);
CAMLextern void caml_call_gc_16_alignment(void);

/*
  with -nodynlink:
  445974:       4c 8b 1d 85 06 0d 00    mov    0xd0685(%rip),%r11        # 516000 <caml_safepoint_trigger_page>

  without:
  4459a7:       49 c7 c3 00 60 51 00    mov    $0x516000,%r11
  4459ae:       4d 8b 1b                mov    (%r11),%r11
*/

static void safepoint_triggered(ucontext_t* context)
{
  // Make caml_call_gc return to the instruction after the faulting one,
  // i.e. directly after the safepoint.
  char* fault_addr = (char*) context->uc_mcontext.gregs[REG_RIP];
  int len = 2; // 32-bit load from address in a register
  void* return_addr = (void*) (fault_addr + len);
  uintnat stack_ptr = (uintnat) context->uc_mcontext.gregs[REG_RSP];
  stack_ptr -= 8;
  context->uc_mcontext.gregs[REG_RSP] = stack_ptr;
  *(void**) stack_ptr = return_addr;

  // Make this signal handler return to caml_call_gc.  There are two
  // versions depending on what the current stack alignment is (we need
  // to match the normal situation for a call, where the stack pointer is
  // 0 mod 16 at the call site).  This logic means that the backend doesn't
  // need to worry about the positioning of polling instructions.
  if (stack_ptr % 16 != 0) {
    context->uc_mcontext.gregs[REG_RIP] = (greg_t) &caml_call_gc;
  }
  else {
    context->uc_mcontext.gregs[REG_RIP] = (greg_t) &caml_call_gc_16_alignment;
  }
}

#endif

DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct sigaction act;
#if defined(NEW_SAFEPOINTS_STRATEGY) || !defined(STACK_CHECKS_ENABLED)
  char* fault_addr = info->si_addr;
#endif
#if !defined(STACK_CHECKS_ENABLED)
  struct stack_info *block = Caml_state->current_stack;
  int page_size = getpagesize();
  char* protected_low = Protected_stack_page(block, page_size);
  char* protected_high = protected_low + page_size;
#endif

#if defined(NEW_SAFEPOINTS_STRATEGY)
  if (fault_addr == caml_safepoint_trigger_page) {
    safepoint_triggered (context);
    // This will return to one of the caml_call_gc veneers (see above).
  } else
#endif
#if !defined(STACK_CHECKS_ENABLED)
  if ((fault_addr >= protected_low) && (fault_addr < protected_high)) {
    context->uc_mcontext.gregs[REG_RIP]= (greg_t) &caml_raise_stack_overflow_nat;
  } else
#endif
  {
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
  }
}

void caml_init_nat_signals(void)
{
#if defined(NEW_SAFEPOINTS_STRATEGY) || !defined(STACK_CHECKS_ENABLED)
#if !defined(POSIX_SIGNALS)
#error "stack checks cannot be disabled nor the new safepoint strategy used \
if POSIX signals are not available"
#endif
  struct sigaction act;
  SET_SIGACT(act, segv_handler);
  act.sa_flags |= SA_ONSTACK | SA_NODEFER;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, NULL);
#endif
}
