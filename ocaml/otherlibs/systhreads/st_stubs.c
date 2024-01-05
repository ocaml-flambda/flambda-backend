/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1995 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#if defined(_WIN32) && !defined(NATIVE_CODE)
/* Ensure that pthread.h marks symbols __declspec(dllimport) so that they can be
   picked up from the runtime (which will have linked winpthreads statically).
   mingw-w64 11.0.0 introduced WINPTHREADS_USE_DLLIMPORT to do this explicitly;
   prior versions co-opted this on the internal DLL_EXPORT, but this is ignored
   in 11.0 and later unless IN_WINPTHREAD is also defined, so we can safely
   define both to support both versions. */
#define WINPTHREADS_USE_DLLIMPORT
#define DLL_EXPORT
#endif

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sync.h"
#include "caml/sys.h"
#include "caml/memprof.h"

#include "../../runtime/sync_posix.h"

#define CAMLextern_libthreads
#include "threads.h"

/* Max computation time before rescheduling, in milliseconds */
#define Thread_timeout 50

/* OS-specific code */
#ifdef _WIN32
#include "st_win32.h"
#else
#include "st_posix.h"
#endif

/* Atomics */
#if defined(__GNUC__) && __GNUC__ == 4 && __GNUC_MINOR__ == 8
  /* GCC 4.8 shipped with a working implementation of atomics, but no
     stdatomic.h header, so we need to use GCC-specific intrinsics. */

  #define _Atomic /* GCC intrinsics work on normal variables */
  #define atomic_store(v, x) \
    __atomic_store_n((v), (x), __ATOMIC_SEQ_CST)
  #define atomic_load(v) \
    __atomic_load_n((v), __ATOMIC_SEQ_CST)
  #define atomic_exchange(v, x) \
    __atomic_exchange_n((v), (x), __ATOMIC_SEQ_CST)
#else
  #include <stdatomic.h>
#endif

/* The ML value describing a thread (heap-allocated) */

#define Ident(v) Field(v, 0)
#define Start_closure(v) Field(v, 1)
#define Terminated(v) Field(v, 2)

/* The infos on threads (allocated via caml_stat_alloc()) */

struct caml_thread_struct {

  value descr;              /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next; /* Doubly-linked list of running threads */
  struct caml_thread_struct * prev;
  int domain_id;      /* The id of the domain to which this thread belongs */
  struct stack_info* current_stack;      /* saved Caml_state->current_stack */
  struct c_stack_link* c_stack;          /* saved Caml_state->c_stack */
  /* Note: we do not save Caml_state->stack_cache, because it can
     safely be shared between all threads on the same domain. */
  struct caml__roots_block *local_roots; /* saved value of local_roots */
  struct caml_local_arenas *local_arenas;
  int backtrace_pos;           /* saved value of Caml_state->backtrace_pos */
  backtrace_slot * backtrace_buffer;
    /* saved value of Caml_state->backtrace_buffer */
  value backtrace_last_exn;
    /* saved value of Caml_state->backtrace_last_exn (root) */
  value * gc_regs;           /* saved value of Caml_state->gc_regs */
  value * gc_regs_buckets;   /* saved value of Caml_state->gc_regs_buckets */
  void * exn_handler;        /* saved value of Caml_state->exn_handler */
  char * async_exn_handler;  /* saved value of Caml_state->async_exn_handler */
#ifndef NATIVE_CODE
  intnat trap_sp_off;      /* saved value of Caml_state->trap_sp_off */
  intnat trap_barrier_off; /* saved value of Caml_state->trap_barrier_off */
  struct caml_exception_context* external_raise;
    /* saved value of Caml_state->external_raise */
  struct caml_exception_context* external_raise_async;
    /* saved value of Caml_state->external_raise_async */
#endif

#ifdef POSIX_SIGNALS
  sigset_t init_mask;
#endif
};

typedef struct caml_thread_struct* caml_thread_t;

/* Thread-local key for accessing the current thread's [caml_thread_t] */
st_tlskey caml_thread_key;

#define This_thread ((caml_thread_t) st_tls_get(caml_thread_key))

/* overall table for threads across domains */
struct caml_thread_table {
  caml_thread_t active_thread;
  struct caml_locking_scheme * _Atomic locking_scheme;
  st_masterlock default_lock;
  struct caml_locking_scheme default_locking_scheme;
  int tick_thread_running;
  int tick_thread_disabled;
  st_thread_id tick_thread_id;
};

/* thread_table instance, up to Max_domains */
static struct caml_thread_table thread_table[Max_domains];

#define Locking_scheme(dom_id) (thread_table[dom_id].locking_scheme)
#define Default_lock(dom_id) (&thread_table[dom_id].default_lock)
#define Default_locking_scheme(dom_id) (&thread_table[dom_id].default_locking_scheme)

static void thread_lock_acquire(int dom_id)
{
  struct caml_locking_scheme* s;

  /* The locking scheme may be changed by the thread that currently
     holds it. This means that it may change while we're waiting to
     acquire it, so by the time we acquire it it may no longer be the
     right scheme. */

 retry:
  s = atomic_load(&Locking_scheme(dom_id));
  s->lock(s->context);
  if (atomic_load(&Locking_scheme(dom_id)) != s) {
    /* This is no longer the right scheme. Unlock and try again */
    s->unlock(s->context);
    goto retry;
  }
}

static void thread_lock_release(int dom_id)
{
  /* There is no tricky case here like in acquire, as only the holder
     of the lock can change it. (Here, that's us) */
  struct caml_locking_scheme *s;
  s = atomic_load(&Locking_scheme(dom_id));
  s->unlock(s->context);
}

/* The remaining fields are accessed while holding the domain lock */

/* The descriptor for the currently executing thread for this domain;
   also the head of a circular list of thread descriptors for this
   domain. Invariant: at every safe point, either Active_thread is
   NULL, or Caml_state is setup for Active_thread. */
#define Active_thread thread_table[Caml_state->id].active_thread

/* Whether the "tick" thread is already running for this domain */
#define Tick_thread_running thread_table[Caml_state->id].tick_thread_running

/* Whether the "tick" thread is disabled for this domain */
#define Tick_thread_disabled thread_table[Caml_state->id].tick_thread_disabled

/* The thread identifier of the "tick" thread for this domain */
#define Tick_thread_id thread_table[Caml_state->id].tick_thread_id

/* Identifier for next thread creation */
static atomic_uintnat thread_next_id = 0;

/* Forward declarations */
static value caml_threadstatus_new (void);
static void caml_threadstatus_terminate (value);
static st_retcode caml_threadstatus_wait (value);

static int default_can_skip_yield(st_masterlock *m)
{
  return st_masterlock_waiters(m) == 0;
}

static void default_reinitialize_after_fork(st_masterlock *m)
{
  m->init = 0; /* force initialization */
  st_masterlock_init(m);
}

/* Hook for scanning the stacks of the other threads */

static scan_roots_hook prev_scan_roots_hook;

static void caml_thread_scan_roots(
  scanning_action action, scanning_action_flags fflags, void *fdata,
  caml_domain_state *domain_state)
{
  caml_thread_t th;

  th = Active_thread;

  /* GC could be triggered before [Active_thread] is initialized */
  if (th != NULL) {
    do {
      (*action)(fdata, th->descr, &th->descr);
      (*action)(fdata, th->backtrace_last_exn, &th->backtrace_last_exn);
      if (th != Active_thread) {
        if (th->current_stack != NULL)
          caml_do_local_roots(action, fflags, fdata,
                              th->local_roots, th->current_stack, th->gc_regs,
                              th->local_arenas);
      }
      th = th->next;
    } while (th != Active_thread);

  };

  if (prev_scan_roots_hook != NULL)
    (*prev_scan_roots_hook)(action, fflags, fdata, domain_state);

  return;
}

static void save_runtime_state(void)
{
  CAMLassert(Active_thread == This_thread);
  caml_thread_t th = Active_thread;
  CAMLassert(th != NULL);
  th->current_stack = Caml_state->current_stack;
  th->c_stack = Caml_state->c_stack;
  th->gc_regs = Caml_state->gc_regs;
  th->gc_regs_buckets = Caml_state->gc_regs_buckets;
  th->exn_handler = Caml_state->exn_handler;
  th->async_exn_handler = Caml_state->async_exn_handler;
  th->local_roots = Caml_state->local_roots;
  th->local_arenas = caml_get_local_arenas(Caml_state);
  th->backtrace_pos = Caml_state->backtrace_pos;
  th->backtrace_buffer = Caml_state->backtrace_buffer;
  th->backtrace_last_exn = Caml_state->backtrace_last_exn;
#ifndef NATIVE_CODE
  th->trap_sp_off = Caml_state->trap_sp_off;
  th->trap_barrier_off = Caml_state->trap_barrier_off;
  th->external_raise = Caml_state->external_raise;
  th->external_raise_async = Caml_state->external_raise_async;
#endif
}

CAMLexport void caml_thread_save_runtime_state(void)
{
  save_runtime_state();
}

static void restore_runtime_state(caml_thread_t th)
{
  CAMLassert(th != NULL);
  Active_thread = th;
  Caml_state->current_stack = th->current_stack;
  Caml_state->c_stack = th->c_stack;
  Caml_state->gc_regs = th->gc_regs;
  Caml_state->gc_regs_buckets = th->gc_regs_buckets;
  Caml_state->exn_handler = th->exn_handler;
  Caml_state->async_exn_handler = th->async_exn_handler;
  Caml_state->local_roots = th->local_roots;
  caml_set_local_arenas(Caml_state, th->local_arenas);
  Caml_state->backtrace_pos = th->backtrace_pos;
  Caml_state->backtrace_buffer = th->backtrace_buffer;
  caml_modify_generational_global_root
    (&Caml_state->backtrace_last_exn, th->backtrace_last_exn);
#ifndef NATIVE_CODE
  Caml_state->trap_sp_off = th->trap_sp_off;
  Caml_state->trap_barrier_off = th->trap_barrier_off;
  Caml_state->external_raise = th->external_raise;
  Caml_state->external_raise_async = th->external_raise_async;
#endif
}

CAMLexport void caml_thread_restore_runtime_state(void)
{
  restore_runtime_state(This_thread);
}


CAMLexport void caml_switch_runtime_locking_scheme(struct caml_locking_scheme* new)
{
  struct caml_locking_scheme* old;
  int dom_id = Caml_state->id;
  save_runtime_state();
  old = atomic_exchange(&Locking_scheme(dom_id), new);
  /* We hold 'old', but it is no longer the runtime lock */
  old->unlock(old->context);
  thread_lock_acquire(dom_id);
  restore_runtime_state(This_thread);
}

CAMLexport struct caml_locking_scheme* caml_get_default_locking_scheme(void)
{
  return Default_locking_scheme(Caml_state->id);
}

CAMLprim value caml_thread_cleanup(value unit);

static void reset_active(void)
{
  Active_thread = NULL;
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  caml_thread_cleanup(Val_unit);
}

/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */

static void caml_thread_enter_blocking_section(void)
{
  /* Save the current runtime state in the thread descriptor
     of the current thread */
  save_runtime_state();
  /* Tell other threads that the runtime is free */
  thread_lock_release(Caml_state->id);
}

static void caml_thread_leave_blocking_section(void)
{
  caml_thread_t th = This_thread;
  /* Wait until the runtime is free */
  thread_lock_acquire(th->domain_id);
  /* Update Active_thread to point to the thread descriptor
     corresponding to the thread currently executing and restore the
     runtime state */
  restore_runtime_state(th);
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
  caml_domain_state *domain_state;
  uintnat stack_wsize = caml_get_init_stack_wsize();

  domain_state = Caml_state;
  th = NULL;
  th = (caml_thread_t)caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;
  th->descr = Val_unit;
  th->next = NULL;
  th->prev = NULL;
  th->domain_id = domain_state->id;
  th->current_stack = caml_alloc_main_stack(stack_wsize);
  if (th->current_stack == NULL) {
    caml_stat_free(th);
    return NULL;
  }
  th->c_stack = NULL;
  th->local_roots = NULL;
  th->local_arenas = NULL;
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  th->backtrace_last_exn = Val_unit;
  th->gc_regs = NULL;
  th->gc_regs_buckets = NULL;
  th->exn_handler = NULL;
  th->async_exn_handler = NULL;

#ifndef NATIVE_CODE
  th->trap_sp_off = 1;
  th->trap_barrier_off = 2;
  th->external_raise = NULL;
  th->external_raise_async = NULL;
#endif

  return th;
}

/* Free the resources held by a thread. */
void caml_thread_free_info(caml_thread_t th)
{
  /* the following fields do not need any specific cleanup:
     descr: heap-allocated
     c_stack: stack-allocated
     local_roots: stack-allocated
     backtrace_last_exn: heap-allocated
     gc_regs:
       must be empty for a terminated thread
       (we assume that the C call stack must be empty at
        thread-termination point, so there are no gc_regs buckets in
        use in this variable nor on the stack)
     exn_handler: stack-allocated
     external_raise: stack-allocated
     async_exn_handler: stack-allocated
     external_raise_async: stack-allocated
     init_mask: stack-allocated
  */
  caml_free_stack(th->current_stack);
  caml_free_backtrace_buffer(th->backtrace_buffer);

  // CR sdolan: free local arenas

  /* Remark: we could share gc_regs_buckets between threads on a same
     domain, but this might break the invariant that it is always
     non-empty at the point where we switch from OCaml to C, so we
     would need to do something more complex when activating a thread
     to restore this invariant. */
  caml_free_gc_regs_buckets(th->gc_regs_buckets);

  caml_stat_free(th);
}

/* Allocate a thread descriptor block. */

static value caml_thread_new_descriptor(value clos)
{
  CAMLparam1(clos);
  CAMLlocal1(mu);
  value descr;
  /* Create and initialize the termination semaphore */
  mu = caml_threadstatus_new();
  /* Create a descriptor for the new thread */
  descr = caml_alloc_3(0, Val_long(atomic_fetch_add(&thread_next_id, +1)),
                       clos, mu);
  CAMLreturn(descr);
}

/* Remove a thread info block from the list of threads
   and free its resources. */
static void caml_thread_remove_and_free(caml_thread_t th)
{
  if (th->next == th)
    reset_active(); /* last OCaml thread exiting */
  else if (Active_thread == th)
    restore_runtime_state(th->next); /* PR#5295 */
  th->next->prev = th->prev;
  th->prev->next = th->next;

  caml_thread_free_info(th);
  return;
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */
/* TODO(engil): more work on the multicore fork machinery. */

static void caml_thread_reinitialize(void)
{
  struct channel * chan;
  caml_thread_t th, next;

  th = Active_thread->next;
  while (th != Active_thread) {
    next = th->next;
    caml_thread_free_info(th);
    th = next;
  }
  Active_thread->next = Active_thread;
  Active_thread->prev = Active_thread;

  // CR ocaml 5 domains: systhreads doesn't maintain domain lock
  /* Within the child, the domain_lock needs to be reset and acquired. */
  // caml_reset_domain_lock();
  // caml_acquire_domain_lock();

  /* The lock needs to be initialized again. This process will also be
     the effective owner of the lock. So there is no need to run
     s->lock (busy = 1) */
  struct caml_locking_scheme *s = atomic_load(&Locking_scheme(Caml_state->id));
  s->reinitialize_after_fork(s->context);

  /* Reinitialize IO mutexes, in case the fork happened while another thread
     had locked the channel. If so, we're likely in an inconsistent state,
     but we may be able to proceed anyway. */
  caml_plat_mutex_init(&caml_all_opened_channels_mutex);
  for (chan = caml_all_opened_channels;
       chan != NULL;
       chan = chan->next) {
    caml_plat_mutex_init(&chan->mutex);
  }
}

CAMLprim value caml_thread_join(value th);

/* This hook is run when a domain shuts down (see domains.c).

   When a domain shuts down, the state must be cleared to allow proper reuse of
   the domain slot the next time a domain is started on this slot. If a program
   is single-domain, we mimic OCaml 4's behavior and do not care about ongoing
   thread: the program will exit. */
static void caml_thread_domain_stop_hook(void) {
  /* If the program runs multiple domains, we should not let systhreads to hang
     around when a domain exit. If the domain is not the last one (and the last
     one will always be domain 0) we force the domain to join on every thread
     on its chain before wrapping up. */
  if (!caml_domain_alone()) {

    while (Active_thread->next != Active_thread) {
      caml_thread_join(Active_thread->next->descr);
    }

    /* another domain thread may be joining on this domain's descriptor */
    caml_threadstatus_terminate(Terminated(Active_thread->descr));
    /* Shut down the tick thread */
    reset_active();
    /* We free the thread info but not its resources: they are owned
       by Caml_state at this point, and will be cleaned-up later. */
    caml_stat_free(This_thread);
  };
}

static void caml_thread_domain_initialize_hook(void)
{

  caml_thread_t new_thread;

  /* OS-specific initialization */
  st_initialize();

  st_masterlock *default_lock = Default_lock(Caml_state->id);
  st_masterlock_init(default_lock);
  struct caml_locking_scheme *ls = Default_locking_scheme(Caml_state->id);
  ls->context = default_lock;
  ls->lock = (void (*)(void*))&st_masterlock_acquire;
  ls->unlock = (void (*)(void*))&st_masterlock_release;
  ls->thread_start = NULL;
  ls->thread_stop = NULL;
  ls->reinitialize_after_fork = (void (*)(void*))&default_reinitialize_after_fork;
  ls->can_skip_yield = (int (*)(void*))&default_can_skip_yield;
  ls->yield = (void (*)(void*))&st_thread_yield;

  Locking_scheme(Caml_state->id) = ls;

  new_thread =
    (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));

  new_thread->domain_id = Caml_state->id;
  new_thread->descr = caml_thread_new_descriptor(Val_unit);
  new_thread->next = new_thread;
  new_thread->prev = new_thread;
  new_thread->backtrace_last_exn = Val_unit;

  st_tls_set(caml_thread_key, new_thread);

  Active_thread = new_thread;

}

CAMLprim value caml_thread_yield(value unit);

void caml_thread_interrupt_hook(void)
{
  /* Do not attempt to yield from the backup thread */
  if (caml_bt_is_self()) return;

  uintnat is_on = 1;
  atomic_uintnat* req_external_interrupt =
    &Caml_state->requested_external_interrupt;

  if (atomic_compare_exchange_strong(req_external_interrupt, &is_on, 0)) {
    caml_thread_yield(Val_unit);
  }

  return;
}

/* [caml_thread_initialize] initialises the systhreads infrastructure. This
   function first sets up the chain for systhreads on this domain, then setup
   the global variables and hooks for systhreads to cooperate with the runtime
   system. */
CAMLprim value caml_thread_initialize(value unit)
{
  /* Protect against repeated initialization (PR#3532) */
  if (Active_thread != NULL) return Val_unit;

  if (!caml_domain_alone())
    caml_failwith("caml_thread_initialize: cannot initialize Thread "
                  "while several domains are running.");

  /* Initialize the key to the [caml_thread_t] structure */
  st_tls_newkey(&caml_thread_key);

  /* First initialise the systhread chain on this domain */
  caml_thread_domain_initialize_hook();

  prev_scan_roots_hook = atomic_exchange(&caml_scan_roots_hook,
                                         caml_thread_scan_roots);
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
  caml_domain_external_interrupt_hook = caml_thread_interrupt_hook;
  caml_domain_initialize_hook = caml_thread_domain_initialize_hook;
  caml_domain_stop_hook = caml_thread_domain_stop_hook;

  caml_atfork_hook = caml_thread_reinitialize;

  return Val_unit;
}

static void stop_tick_thread(void)
{
  if (!Tick_thread_running) return;
  atomic_store_release(&Tick_thread_stop, 1);
  st_thread_join(Tick_thread_id);
  atomic_store_release(&Tick_thread_stop, 0);
  Tick_thread_running = 0;
}

CAMLprim value caml_thread_cleanup(value unit)
{
  stop_tick_thread();
  return Val_unit;
}

static void caml_thread_stop(void)
{
  /* PR#5188, PR#7220: some of the global runtime state may have
     changed as the thread was running, so we save it in the
     This_thread data to make sure that the cleanup logic
     below uses accurate information. */
  save_runtime_state();

  /* The main domain thread does not go through [caml_thread_stop]. There is
     always one more thread in the chain at this point in time. */
  CAMLassert(Active_thread->next != Active_thread);

  caml_threadstatus_terminate(Terminated(Active_thread->descr));

  /* The following also sets Active_thread to a sane value in case the
     backup thread does a GC before the domain lock is acquired
     again. */
  caml_thread_remove_and_free(Active_thread);
  thread_lock_release(Caml_state->id);
}

/* Create a thread */

/* the thread lock is not held when entering */
static void * caml_thread_start(void * v)
{
  caml_thread_t th = (caml_thread_t) v;
  int dom_id = th->domain_id;
  value clos;
  void * signal_stack;

  caml_init_domain_self(dom_id);

  st_tls_set(caml_thread_key, th);
  struct caml_locking_scheme *s = atomic_load(&Locking_scheme(dom_id));
  if (s -> thread_start != NULL)
    s->thread_start(s->context, Thread_type_caml);

  thread_lock_acquire(dom_id);
  restore_runtime_state(th);
  signal_stack = caml_init_signal_stack();

#ifdef POSIX_SIGNALS
  /* restore the signal mask from the spawning thread, now it is safe for the
     signal handler to run (as Caml_state is initialised) */
  pthread_sigmask(SIG_SETMASK, &th->init_mask, NULL);
#endif

  clos = Start_closure(Active_thread->descr);
  caml_modify(&(Start_closure(Active_thread->descr)), Val_unit);
  caml_callback_exn(clos, Val_unit);
  caml_thread_stop();
  s = atomic_load(&Locking_scheme(dom_id));
  if (s->thread_stop != NULL)
    s->thread_stop(s->context, Thread_type_caml);
  caml_free_signal_stack(signal_stack);
  return 0;
}

static int create_tick_thread(void)
{
  int err;
#ifdef POSIX_SIGNALS
  sigset_t mask, old_mask;

  /* Block all signals so that we don't try to execute an OCaml signal
     handler in the new tick thread */
  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

  err = st_thread_create(&Tick_thread_id, caml_thread_tick,
                         (void *) &Caml_state->id);

#ifdef POSIX_SIGNALS
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  return err;
}

static st_retcode start_tick_thread(void)
{
  if (Tick_thread_running) return 0;
  st_retcode err = create_tick_thread();
  if (err == 0) Tick_thread_running = 1;
  return err;
}

CAMLprim value caml_enable_tick_thread(value v_enable)
{
  int enable = Long_val(v_enable) ? 1 : 0;

  if (enable) {
    st_retcode err = start_tick_thread();
    sync_check_error(err, "caml_enable_tick_thread");
  } else {
    stop_tick_thread();
  }

  Tick_thread_disabled = !enable;
  return Val_unit;
}

CAMLprim value caml_thread_new(value clos)
{
  CAMLparam1(clos);
  caml_thread_t th;
  st_retcode err;
#ifdef POSIX_SIGNALS
  sigset_t mask, old_mask;

  sigfillset(&mask);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
#endif

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support multithreaded programs");
#endif
  /* Create a thread info block */
  th = caml_thread_new_info();

  if (th == NULL)
    caml_raise_out_of_memory();

  th->descr = caml_thread_new_descriptor(clos);

#ifdef POSIX_SIGNALS
  th->init_mask = old_mask;
#endif

  th->next = Active_thread->next;
  th->prev = Active_thread;

  Active_thread->next->prev = th;
  Active_thread->next = th;

  err = st_thread_create(NULL, caml_thread_start, (void *) th);

#ifdef POSIX_SIGNALS
  /* regardless of error, return our sigmask to the original state */
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif

  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_and_free(th);
    sync_check_error(err, "Thread.create");
  }

  if (!Tick_thread_disabled) {
    err = start_tick_thread();
    sync_check_error(err, "Thread.create");
  }
  CAMLreturn(th->descr);
}

/* Register a thread already created from C */

#define Dom_c_threads 0

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_register(void)
{
  /* Already registered? */
  if (This_thread != NULL) return 0;

  struct caml_locking_scheme *s = atomic_load(&Locking_scheme(Dom_c_threads));
  if (s->thread_start != NULL)
    s->thread_start(s->context, Thread_type_c_registered);

  CAMLassert(Caml_state_opt == NULL);
  caml_init_domain_self(Dom_c_threads);

  /* Take master lock to protect access to the runtime */
  thread_lock_acquire(Dom_c_threads);
  /* Create a thread info block */
  caml_thread_t th = caml_thread_new_info();
  /* If it fails, we release the lock and return an error. */
  if (th == NULL) {
    thread_lock_release(Dom_c_threads);
    return 0;
  }
  /* Add thread info block to the list of threads */
  if (Active_thread == NULL) {
    th->next = th;
    th->prev = th;
    Active_thread = th;
  } else {
    th->next = Active_thread->next;
    th->prev = Active_thread;
    Active_thread->next->prev = th;
    Active_thread->next = th;
  }
  /* Associate the thread descriptor with the thread */
  st_tls_set(caml_thread_key, (void *) th);
  /* Allocate the thread descriptor on the heap */
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */

  if (!Tick_thread_disabled) {
    st_retcode err = start_tick_thread();
    sync_check_error(err, "caml_register_c_thread");
  }

  /* Release the master lock */
  thread_lock_release(Dom_c_threads);
  return 1;
}

/* Unregister a thread that was created from C and registered with
   the function above */

/* the thread lock is not held when entering */
CAMLexport int caml_c_thread_unregister(void)
{
  caml_thread_t th = This_thread;

  /* If this thread is not set, then it was not registered */
  if (th == NULL) return 0;
  /* Wait until the runtime is available */
  thread_lock_acquire(Dom_c_threads);
  /*  Forget the thread descriptor */
  st_tls_set(caml_thread_key, NULL);
  /* Remove thread info block from list of threads, and free it */
  caml_thread_remove_and_free(th);
  /* Release the runtime */
  thread_lock_release(Dom_c_threads);
  struct caml_locking_scheme *s = atomic_load(&Locking_scheme(Dom_c_threads));
  if (s->thread_stop != NULL)
    s->thread_stop(s->context, Thread_type_c_registered);
  return 1;
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)
{
  return Active_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)
{
  char * msg = caml_format_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(Active_thread->descr)), msg);
  caml_stat_free(msg);
  if (Caml_state->backtrace_active) caml_print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Allow re-scheduling */

CAMLprim value caml_thread_yield(value unit)
{
  struct caml_locking_scheme *s;
  s = atomic_load(&Locking_scheme(Caml_state->id));
  if (s->can_skip_yield != NULL && s -> can_skip_yield(s->context))
    return Val_unit;

  /* Do all the parts of a blocking section enter/leave except lock
     manipulation, which we'll do more efficiently in st_thread_yield. (Since
     our blocking section doesn't contain anything interesting, don't bother
     with saving errno.)
  */

  (void) caml_raise_async_if_exception(caml_process_pending_signals_exn (), "");

  // s may have changed in caml_process_pending_signals_exn
  s = atomic_load(&Locking_scheme(Caml_state->id));
  save_runtime_state();
  s->yield(s->context);
  if (atomic_load(&Locking_scheme(Caml_state->id)) != s) {
    // The lock we own is no longer the runtime lock
    s->unlock(s->context);
    thread_lock_acquire(Caml_state->id);
  }
  restore_runtime_state(This_thread);
  (void) caml_raise_async_if_exception(caml_process_pending_signals_exn (), "");

  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)
{
  st_retcode rc = caml_threadstatus_wait(Terminated(th));
  sync_check_error(rc, "Thread.join");
  return Val_unit;
}

/* Thread status blocks */

#define Threadstatus_val(v) (* ((st_event *) Data_custom_val(v)))

static void caml_threadstatus_finalize(value wrapper)
{
  st_event_destroy(Threadstatus_val(wrapper));
}

static int caml_threadstatus_compare(value wrapper1, value wrapper2)
{
  st_event ts1 = Threadstatus_val(wrapper1);
  st_event ts2 = Threadstatus_val(wrapper2);
  return ts1 == ts2 ? 0 : ts1 < ts2 ? -1 : 1;
}

static struct custom_operations caml_threadstatus_ops = {
  "_threadstatus",
  caml_threadstatus_finalize,
  caml_threadstatus_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value caml_threadstatus_new (void)
{
  st_event ts = NULL;           /* suppress warning */
  value wrapper;
  sync_check_error(st_event_create(&ts), "Thread.create");
  wrapper = caml_alloc_custom(&caml_threadstatus_ops,
                              sizeof(st_event *),
                              0, 1);
  Threadstatus_val(wrapper) = ts;
  return wrapper;
}

static void caml_threadstatus_terminate (value wrapper)
{
  st_event_trigger(Threadstatus_val(wrapper));
}

static st_retcode caml_threadstatus_wait (value wrapper)
{
  CAMLparam1(wrapper); /* prevent deallocation of ts */
  st_event ts = Threadstatus_val(wrapper);
  st_retcode retcode;

  caml_enter_blocking_section();
  retcode = st_event_wait(ts);
  caml_leave_blocking_section();

  CAMLreturnT(st_retcode, retcode);
}
