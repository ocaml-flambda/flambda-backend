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

#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif
#include "caml/compatibility.h"
#undef CAML_NAME_SPACE

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/roots.h"
#include "caml/signals.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#else
#include "caml/stacks.h"
#endif
#include "caml/sys.h"
#include "caml/memprof.h"

#define CAMLextern_libthreads
#include "../../otherlibs/systhreads4/threads.h"

#ifndef NATIVE_CODE
/* Initial size of bytecode stack when a thread is created (4 Ko) */
#define Thread_stack_size (Stack_size / 4)
#endif

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

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  value terminated;             /* Triggered event for thread termination */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Start_closure(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Terminated(v) (((struct caml_thread_descr *)(v))->terminated)

/* The infos on threads (allocated via caml_stat_alloc()) */

struct caml_thread_struct {
  value descr;              /* The heap-allocated descriptor (root) */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;
#ifdef NATIVE_CODE
  char * top_of_stack;      /* Top of stack for this thread (approx.) */
  char * bottom_of_stack;   /* Saved value of Caml_state->_bottom_of_stack */
  uintnat last_retaddr;     /* Saved value of Caml_state->_last_return_address */
  value * gc_regs;          /* Saved value of Caml_state->_gc_regs */
  char * exn_handler;       /* Saved value of Caml_state->_exn_handler */
  char * async_exception_pointer;
                       /* Saved value of Caml_state->_async_exception_pointer */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
  struct caml_local_arenas * local_arenas;
  struct longjmp_buffer * exit_buf; /* For thread exit */
#else
  value * stack_low; /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;        /* Saved value of Caml_state->_extern_sp for this thread */
  value * trapsp;    /* Saved value of Caml_state->_trapsp for this thread */
  /* Saved value of Caml_state->_local_roots */
  struct caml__roots_block * local_roots;
  struct longjmp_buffer * external_raise; /* Saved Caml_state->_external_raise */
  struct longjmp_buffer * external_raise_async;
                                    /* Saved Caml_state->_external_raise_async */
#endif
  int backtrace_pos; /* Saved Caml_state->_backtrace_pos */
  backtrace_slot * backtrace_buffer; /* Saved Caml_state->_backtrace_buffer */
  value backtrace_last_exn;  /* Saved Caml_state->_backtrace_last_exn (root) */
  struct caml_memprof_th_ctx *memprof_ctx;
};

typedef struct caml_thread_struct * caml_thread_t;

/* The "head" of the circular list of thread descriptors */
static caml_thread_t all_threads = NULL;

/* The descriptor for the currently executing thread */
static caml_thread_t curr_thread = NULL;

/* The master lock protecting the OCaml runtime system */
static struct caml_locking_scheme* _Atomic caml_locking_scheme;

/* Whether the "tick" thread is already running */
static int caml_tick_thread_running = 0;

/* Whether the "tick" thread is enabled */
static int caml_tick_thread_enabled = 1;

/* The thread identifier of the "tick" thread */
static st_thread_id caml_tick_thread_id;

/* The key used for storing the thread descriptor in the specific data
   of the corresponding system thread. */
static st_tlskey thread_descriptor_key;

/* The key used for unlocking I/O channels on exceptions */
static st_tlskey last_channel_locked_key;

/* Identifier for next thread creation */
static intnat thread_next_ident = 0;

/* Forward declarations */
static value caml_threadstatus_new (void);
static void caml_threadstatus_terminate (value);
static st_retcode caml_threadstatus_wait (value);

/* Imports from the native-code runtime system */
#ifdef NATIVE_CODE
extern struct longjmp_buffer caml_termination_jmpbuf;
extern void (*caml_termination_hook)(void);
#endif

/* The default locking scheme */
static st_masterlock default_master_lock;

static int default_can_skip_yield(void* m)
{
  return st_masterlock_waiters(m) == 0;
}

struct caml_locking_scheme caml_default_locking_scheme =
  { &default_master_lock,
    (void (*)(void*))&st_masterlock_acquire,
    (void (*)(void*))&st_masterlock_release,
    NULL,
    NULL,
    (void (*)(void*))&st_masterlock_init,
    default_can_skip_yield,
    (void (*)(void*))&st_thread_yield };

CAMLexport struct caml_locking_scheme *caml_get_default_locking_scheme(void)
{
  return &caml_default_locking_scheme;
}

static void acquire_runtime_lock(void)
{
  struct caml_locking_scheme* s;

  /* The locking scheme may be changed by the thread that currently
     holds it. This means that it may change while we're waiting to
     acquire it, so by the time we acquire it it may no longer be the
     right scheme. */

 retry:
  s = atomic_load(&caml_locking_scheme);
  s->lock(s->context);
  if (atomic_load(&caml_locking_scheme) != s) {
    /* This is no longer the right scheme. Unlock and try again */
    s->unlock(s->context);
    goto retry;
  }
}

static void release_runtime_lock(void)
{
  /* There is no tricky case here like in acquire, as only the holder
     of the lock can change it. (Here, that's us) */
  struct caml_locking_scheme* s;
  s = atomic_load(&caml_locking_scheme);
  s->unlock(s->context);
}

/* Hook for scanning the stacks of the other threads */

static void (*prev_scan_roots_hook) (scanning_action);

static void caml_thread_scan_roots(scanning_action action)
{
  caml_thread_t th = curr_thread;
  do {
    (*action)(th->descr, &th->descr);
    (*action)(th->backtrace_last_exn, &th->backtrace_last_exn);
    /* Don't rescan the stack of the current thread, it was done already */
    if (th != curr_thread) {
#ifdef NATIVE_CODE
      if (th->bottom_of_stack != NULL)
        caml_do_local_roots(action, action, th->bottom_of_stack, th->last_retaddr,
                       th->gc_regs, th->local_roots, th->local_arenas);
#else
      caml_do_local_roots(action, th->sp, th->stack_high, th->local_roots);
#endif
    }
    th = th->next;
  } while (th != curr_thread);
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Hook for iterating over Memprof's entries arrays */

static void memprof_ctx_iter(th_ctx_action f, void* data)
{
  caml_thread_t th = curr_thread;
  do {
    f(th->memprof_ctx, data);
    th = th->next;
  } while (th != curr_thread);
}

/* Saving and restoring runtime state in curr_thread */

CAMLexport void caml_thread_save_runtime_state(void)
{
#ifdef NATIVE_CODE
  curr_thread->top_of_stack = Caml_state->_top_of_stack;
  curr_thread->bottom_of_stack = Caml_state->_bottom_of_stack;
  curr_thread->last_retaddr = Caml_state->_last_return_address;
  curr_thread->gc_regs = Caml_state->_gc_regs;
  curr_thread->exn_handler = Caml_state->_exn_handler;
  curr_thread->async_exception_pointer = Caml_state->_async_exception_pointer;
  curr_thread->local_arenas = caml_get_local_arenas();
#else
  curr_thread->stack_low = Caml_state->_stack_low;
  curr_thread->stack_high = Caml_state->_stack_high;
  curr_thread->stack_threshold = Caml_state->_stack_threshold;
  curr_thread->sp = Caml_state->_extern_sp;
  curr_thread->trapsp = Caml_state->_trapsp;
  curr_thread->external_raise = Caml_state->_external_raise;
  curr_thread->external_raise_async = Caml_state->_external_raise_async;
#endif
  curr_thread->local_roots = Caml_state->_local_roots;
  curr_thread->backtrace_pos = Caml_state->_backtrace_pos;
  curr_thread->backtrace_buffer = Caml_state->_backtrace_buffer;
  curr_thread->backtrace_last_exn = Caml_state->_backtrace_last_exn;
  caml_memprof_leave_thread();
}

CAMLexport void caml_thread_restore_runtime_state(void)
{
  /* Update curr_thread to point to the thread descriptor corresponding
     to the thread currently executing */
  curr_thread = st_tls_get(thread_descriptor_key);

#ifdef NATIVE_CODE
  Caml_state->_top_of_stack = curr_thread->top_of_stack;
  Caml_state->_bottom_of_stack= curr_thread->bottom_of_stack;
  Caml_state->_last_return_address = curr_thread->last_retaddr;
  Caml_state->_gc_regs = curr_thread->gc_regs;
  Caml_state->_exn_handler = curr_thread->exn_handler;
  Caml_state->_async_exception_pointer = curr_thread->async_exception_pointer;
  caml_set_local_arenas(curr_thread->local_arenas);
#else
  Caml_state->_stack_low = curr_thread->stack_low;
  Caml_state->_stack_high = curr_thread->stack_high;
  Caml_state->_stack_threshold = curr_thread->stack_threshold;
  Caml_state->_extern_sp = curr_thread->sp;
  Caml_state->_trapsp = curr_thread->trapsp;
  Caml_state->_external_raise = curr_thread->external_raise;
  Caml_state->_external_raise_async = curr_thread->external_raise_async;
#endif
  Caml_state->_local_roots = curr_thread->local_roots;
  Caml_state->_backtrace_pos = curr_thread->backtrace_pos;
  Caml_state->_backtrace_buffer = curr_thread->backtrace_buffer;
  Caml_state->_backtrace_last_exn = curr_thread->backtrace_last_exn;
  caml_memprof_enter_thread(curr_thread->memprof_ctx);
}

CAMLexport void caml_switch_runtime_locking_scheme(struct caml_locking_scheme* new)
{
  struct caml_locking_scheme* old;

  caml_thread_save_runtime_state();
  old = atomic_exchange(&caml_locking_scheme, new);
  /* We hold 'old', but it is no longer the runtime lock */
  old->unlock(old->context);
  acquire_runtime_lock();
  caml_thread_restore_runtime_state();
}


/* Hooks for caml_enter_blocking_section and caml_leave_blocking_section */


static void caml_thread_enter_blocking_section(void)
{
  /* Save the current runtime state in the thread descriptor
     of the current thread */
  caml_thread_save_runtime_state();
  /* Tell other threads that the runtime is free */
  release_runtime_lock();
}

static void caml_thread_leave_blocking_section(void)
{
#ifdef _WIN32
  /* TlsGetValue calls SetLastError which will mask any error which occurred
     prior to the caml_thread_leave_blocking_section call. EnterCriticalSection
     does not do this. */
  DWORD error = GetLastError();
#endif
  /* Wait until the runtime is free */
  acquire_runtime_lock();
  caml_thread_restore_runtime_state();
#ifdef _WIN32
  SetLastError(error);
#endif
}

/* Hooks for I/O locking */

static void caml_io_mutex_free(struct channel *chan)
{
  st_mutex mutex = chan->mutex;
  if (mutex != NULL) {
    st_mutex_destroy(mutex);
    chan->mutex = NULL;
  }
}

static void caml_io_mutex_lock(struct channel *chan)
{
  st_mutex mutex = chan->mutex;

  if (mutex == NULL) {
    st_check_error(st_mutex_create(&mutex), "channel locking"); /*PR#7038*/
    chan->mutex = mutex;
  }
  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (st_mutex_trylock(mutex) == MUTEX_PREVIOUSLY_UNLOCKED) {
    st_tls_set(last_channel_locked_key, (void *) chan);
    return;
  }
  /* If unsuccessful, block on mutex */
  caml_enter_blocking_section();
  st_mutex_lock(mutex);
  /* Problem: if a signal occurs at this point,
     and the signal handler raises an exception, we will not
     unlock the mutex.  The alternative (doing the setspecific
     before locking the mutex is also incorrect, since we could
     then unlock a mutex that is unlocked or locked by someone else. */
  st_tls_set(last_channel_locked_key, (void *) chan);
  caml_leave_blocking_section();
}

static void caml_io_mutex_unlock(struct channel *chan)
{
  st_mutex_unlock(chan->mutex);
  st_tls_set(last_channel_locked_key, NULL);
}

static void caml_io_mutex_unlock_exn(void)
{
  struct channel * chan = st_tls_get(last_channel_locked_key);
  if (chan != NULL) caml_io_mutex_unlock(chan);
}

/* Hook for estimating stack usage */

static uintnat (*prev_stack_usage_hook)(void);

static uintnat caml_thread_stack_usage(void)
{
  uintnat sz;
  caml_thread_t th;

  /* Don't add stack for current thread, this is done elsewhere */
  for (sz = 0, th = curr_thread->next;
       th != curr_thread;
       th = th->next) {
#ifdef NATIVE_CODE
  if(th->top_of_stack != NULL && th->bottom_of_stack != NULL &&
     th->top_of_stack > th->bottom_of_stack)
       sz += (value *) th->top_of_stack - (value *) th->bottom_of_stack;
#else
    sz += th->stack_high - th->sp;
#endif
  }
  if (prev_stack_usage_hook != NULL)
    sz += prev_stack_usage_hook();
  return sz;
}

/* Create and setup a new thread info block.
   This block has no associated thread descriptor and
   is not inserted in the list of threads. */

static caml_thread_t caml_thread_new_info(void)
{
  caml_thread_t th;
  th = (caml_thread_t) caml_stat_alloc_noexc(sizeof(struct caml_thread_struct));
  if (th == NULL) return NULL;
  th->descr = Val_unit;         /* filled later */
#ifdef NATIVE_CODE
  th->bottom_of_stack = NULL;
  th->top_of_stack = NULL;
  th->last_retaddr = 1;
  th->exn_handler = NULL;
  th->async_exception_pointer = NULL;
  th->local_roots = NULL;
  th->local_arenas = NULL;
  th->exit_buf = NULL;
#else
  /* Allocate the stacks */
  th->stack_low = (value *) caml_stat_alloc(Thread_stack_size);
  th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
  th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
  th->sp = th->stack_high;
  th->trapsp = th->stack_high;
  th->local_roots = NULL;
  th->external_raise = NULL;
  th->external_raise_async = NULL;
#endif
  th->backtrace_pos = 0;
  th->backtrace_buffer = NULL;
  th->backtrace_last_exn = Val_unit;
  th->memprof_ctx = caml_memprof_new_th_ctx();
  return th;
}

/* Allocate a thread descriptor block. */

static value caml_thread_new_descriptor(value clos)
{
  value mu = Val_unit;
  value descr;
  Begin_roots2 (clos, mu)
    /* Create and initialize the termination semaphore */
    mu = caml_threadstatus_new();
    /* Create a descriptor for the new thread */
    descr = caml_alloc_small(3, 0);
    Ident(descr) = Val_long(thread_next_ident);
    Start_closure(descr) = clos;
    Terminated(descr) = mu;
    thread_next_ident++;
  End_roots();
  return descr;
}

/* Remove a thread info block from the list of threads.
   Free it and its stack resources. */

static void caml_thread_remove_info(caml_thread_t th)
{
  if (th->next == th)
    all_threads = NULL; /* last OCaml thread exiting */
  else if (all_threads == th)
    all_threads = th->next;     /* PR#5295 */
  th->next->prev = th->prev;
  th->prev->next = th->next;
#ifndef NATIVE_CODE
  caml_stat_free(th->stack_low);
#endif
  if (th->backtrace_buffer != NULL) caml_stat_free(th->backtrace_buffer);
  caml_stat_free(th);
}

/* Reinitialize the thread machinery after a fork() (PR#4577) */

static void caml_thread_reinitialize(void)
{
  struct channel * chan;
  struct caml_locking_scheme* s;

  /* Remove all other threads (now nonexistent)
     from the doubly-linked list of threads */
  while (curr_thread->next != curr_thread) {
    caml_memprof_delete_th_ctx(curr_thread->next->memprof_ctx);
    caml_thread_remove_info(curr_thread->next);
  }

  /* Reinitialize the master lock machinery,
     just in case the fork happened while other threads were doing
     caml_leave_blocking_section */
  s = atomic_load(&caml_locking_scheme);
  s->reinitialize_after_fork(s->context);
  /* Tick thread is not currently running in child process, will be
     re-created at next Thread.create */
  caml_tick_thread_running = 0;
  /* Destroy all IO mutexes; will be reinitialized on demand */
  for (chan = caml_all_opened_channels;
       chan != NULL;
       chan = chan->next) {
    if (chan->mutex != NULL) {
      st_mutex_destroy(chan->mutex);
      chan->mutex = NULL;
    }
  }
}

/* Initialize the thread machinery */

CAMLprim value caml_thread_initialize(value unit)   /* ML */
{
  /* Protect against repeated initialization (PR#3532) */
  if (curr_thread != NULL) return Val_unit;
  /* OS-specific initialization */
  st_initialize();
  /* Initialize and acquire the master lock */
  st_masterlock_init(&default_master_lock);
  caml_locking_scheme = &caml_default_locking_scheme;
  /* Initialize the keys */
  st_tls_newkey(&thread_descriptor_key);
  st_tls_newkey(&last_channel_locked_key);
  /* Set up a thread info block for the current thread */
  curr_thread =
    (caml_thread_t) caml_stat_alloc(sizeof(struct caml_thread_struct));
  curr_thread->descr = caml_thread_new_descriptor(Val_unit);
  curr_thread->next = curr_thread;
  curr_thread->prev = curr_thread;
  all_threads = curr_thread;
  curr_thread->backtrace_last_exn = Val_unit;
#ifdef NATIVE_CODE
  curr_thread->exit_buf = &caml_termination_jmpbuf;
#endif
  curr_thread->memprof_ctx = &caml_memprof_main_ctx;
  /* The stack-related fields will be filled in at the next
     caml_enter_blocking_section */
  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) curr_thread);
  st_thread_set_id(Ident(curr_thread->descr));
  /* Set up the hooks */
  prev_scan_roots_hook = caml_scan_roots_hook;
  caml_scan_roots_hook = caml_thread_scan_roots;
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
#ifdef NATIVE_CODE
  caml_termination_hook = st_thread_exit;
#endif
  caml_channel_mutex_free = caml_io_mutex_free;
  caml_channel_mutex_lock = caml_io_mutex_lock;
  caml_channel_mutex_unlock = caml_io_mutex_unlock;
  caml_channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;
  prev_stack_usage_hook = caml_stack_usage_hook;
  caml_stack_usage_hook = caml_thread_stack_usage;
  caml_memprof_th_ctx_iter_hook = memprof_ctx_iter;
  /* Set up fork() to reinitialize the thread machinery in the child
     (PR#4577) */
  st_atfork(caml_thread_reinitialize);
  return Val_unit;
}

/* Start tick thread, if not already running */
static st_retcode start_tick_thread(void)
{
  st_retcode err;
  if (caml_tick_thread_running) return 0;
  err = st_thread_create(&caml_tick_thread_id, caml_thread_tick, NULL);
  if (err == 0) caml_tick_thread_running = 1;
  return err;
}

/* Stop tick thread, if currently running */
static void stop_tick_thread(void)
{
  if (!caml_tick_thread_running) return;
  caml_tick_thread_stop = 1;
  st_thread_join(caml_tick_thread_id);
  caml_tick_thread_stop = 0;
  caml_tick_thread_running = 0;
}

CAMLprim value caml_enable_tick_thread(value v_enable)
{
  int enable = Long_val(v_enable) ? 1 : 0;

  if (enable) {
    st_retcode err = start_tick_thread();
    st_check_error(err, "caml_enable_tick_thread");
  } else {
    stop_tick_thread();
  }

  caml_tick_thread_enabled = enable;
  return Val_unit;
}

/* Cleanup the thread machinery when the runtime is shut down. Joining the tick
   thread take 25ms on average / 50ms in the worst case, so we don't do it on
   program exit. */

CAMLprim value caml_thread_cleanup(value unit)   /* ML */
{
  stop_tick_thread();
  return Val_unit;
}

/* Thread cleanup at termination */

static void caml_thread_stop(void)
{
  /* PR#5188, PR#7220: some of the global runtime state may have
     changed as the thread was running, so we save it in the
     curr_thread data to make sure that the cleanup logic
     below uses accurate information. */
  caml_thread_save_runtime_state();
  /* Tell memprof that this thread is terminating. */
  caml_memprof_delete_th_ctx(curr_thread->memprof_ctx);
  /* Signal that the thread has terminated */
  caml_threadstatus_terminate(Terminated(curr_thread->descr));
  /* Remove th from the doubly-linked list of threads and free its info block */
  caml_thread_remove_info(curr_thread);
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (all_threads == NULL) caml_thread_cleanup(Val_unit);
  /* OS-specific cleanups */
  st_thread_cleanup();
  /* Release the runtime system */
  release_runtime_lock();
}

/* Create a thread */

static ST_THREAD_FUNCTION caml_thread_start(void * arg)
{
  caml_thread_t th = (caml_thread_t) arg;
  value clos;
  void * signal_stack;
  struct caml_locking_scheme* sch;
#ifdef NATIVE_CODE
  struct longjmp_buffer termination_buf;
  char tos;
  /* Record top of stack (approximative) */
  th->top_of_stack = &tos;
#endif

  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) th);
  sch = atomic_load(&caml_locking_scheme);
  if (sch->thread_start != NULL)
    sch->thread_start(sch->context, Thread_type_caml);
  /* Acquire the global mutex */
  caml_leave_blocking_section();
  st_thread_set_id(Ident(th->descr));
  signal_stack = caml_setup_stack_overflow_detection();
#ifdef NATIVE_CODE
  /* Setup termination handler (for caml_thread_exit) */
  if (sigsetjmp(termination_buf.buf, 0) == 0) {
    th->exit_buf = &termination_buf;
#endif
    /* Callback the closure */
    clos = Start_closure(th->descr);
    caml_modify(&(Start_closure(th->descr)), Val_unit);
    caml_callback_exn(clos, Val_unit);
    caml_thread_stop();
    sch = atomic_load(&caml_locking_scheme);
    if (sch->thread_stop != NULL)
      sch->thread_stop(sch->context, Thread_type_caml);
#ifdef NATIVE_CODE
  }
#endif
  caml_stop_stack_overflow_detection(signal_stack);
  /* The thread now stops running */
  return 0;
}

CAMLprim value caml_thread_new(value clos)          /* ML */
{
  caml_thread_t th;
  st_retcode err;

#ifndef NATIVE_CODE
  if (caml_debugger_in_use)
    caml_fatal_error("ocamldebug does not support multithreaded programs");
#endif
  /* Create a thread info block */
  th = caml_thread_new_info();
  if (th == NULL) caml_raise_out_of_memory();
  /* Equip it with a thread descriptor */
  th->descr = caml_thread_new_descriptor(clos);
  /* Add thread info block to the list of threads */
  th->next = curr_thread->next;
  th->prev = curr_thread;
  curr_thread->next->prev = th;
  curr_thread->next = th;
  /* Create the new thread */
  err = st_thread_create(NULL, caml_thread_start, (void *) th);
  if (err != 0) {
    /* Creation failed, remove thread info block from list of threads */
    caml_thread_remove_info(th);
    st_check_error(err, "Thread.create");
  }
  /* Create the tick thread if not already done.
     Because of PR#4666, we start the tick thread late, only when we create
     the first additional thread in the current process*/
  if (caml_tick_thread_enabled) {
    err = start_tick_thread();
    st_check_error(err, "Thread.create");
  }
  return th->descr;
}

/* Register a thread already created from C */

CAMLexport int caml_c_thread_register(void)
{
  caml_thread_t th;
  struct caml_locking_scheme* sch;
#ifdef NATIVE_CODE
  st_retcode err;
#endif

  sch = atomic_load(&caml_locking_scheme);
  if (sch->thread_start != NULL)
    sch->thread_start(sch->context, Thread_type_c_registered);

  /* Already registered? */
  if (st_tls_get(thread_descriptor_key) != NULL) return 0;
  /* Create a thread info block */
  th = caml_thread_new_info();
  if (th == NULL) return 0;
#ifdef NATIVE_CODE
  th->top_of_stack = (char *) &err;
#endif
  /* Take master lock to protect access to the chaining of threads */
  acquire_runtime_lock();
  /* Add thread info block to the list of threads */
  if (all_threads == NULL) {
    th->next = th;
    th->prev = th;
    all_threads = th;
  } else {
    th->next = all_threads->next;
    th->prev = all_threads;
    all_threads->next->prev = th;
    all_threads->next = th;
  }
  /* Associate the thread descriptor with the thread */
  st_tls_set(thread_descriptor_key, (void *) th);
  /* Release the master lock */
  release_runtime_lock();
  /* Now we can re-enter the run-time system and heap-allocate the descriptor */
  caml_leave_blocking_section();
  th->descr = caml_thread_new_descriptor(Val_unit);  /* no closure */
  st_thread_set_id(Ident(th->descr));
  /* Create the tick thread if not already done.  */
  if (caml_tick_thread_enabled) start_tick_thread();
  /* Exit the run-time system */
  caml_enter_blocking_section();
  return 1;
}

/* Unregister a thread that was created from C and registered with
   the function above */

CAMLexport int caml_c_thread_unregister(void)
{
  struct caml_locking_scheme* sch;
  caml_thread_t th = st_tls_get(thread_descriptor_key);
  /* Not registered? */
  if (th == NULL) return 0;
  /* Wait until the runtime is available */
  acquire_runtime_lock();
  /* Forget the thread descriptor */
  st_tls_set(thread_descriptor_key, NULL);
  /* Remove thread info block from list of threads, and free it */
  caml_thread_remove_info(th);
  /* If no other OCaml thread remains, ask the tick thread to stop
     so that it does not prevent the whole process from exiting (#9971) */
  if (all_threads == NULL) caml_thread_cleanup(Val_unit);
  /* Release the runtime */
  release_runtime_lock();
  sch = atomic_load(&caml_locking_scheme);
  if (sch->thread_stop != NULL)
    sch->thread_stop(sch->context, Thread_type_c_registered);
  return 1;
}

/* Return the current thread */

CAMLprim value caml_thread_self(value unit)         /* ML */
{
  if (curr_thread == NULL)
    caml_invalid_argument("Thread.self: not initialized");
  return curr_thread->descr;
}

/* Return the identifier of a thread */

CAMLprim value caml_thread_id(value th)          /* ML */
{
  return Ident(th);
}

/* Print uncaught exception and backtrace */

CAMLprim value caml_thread_uncaught_exception(value exn)  /* ML */
{
  char * msg = caml_format_exception(exn);
  fprintf(stderr, "Thread %d killed on uncaught exception %s\n",
          Int_val(Ident(curr_thread->descr)), msg);
  caml_stat_free(msg);
  if (Caml_state->_backtrace_active) caml_print_exception_backtrace();
  fflush(stderr);
  return Val_unit;
}

/* Terminate current thread */

CAMLprim value caml_thread_exit(value unit)   /* ML */
{
  struct longjmp_buffer * exit_buf = NULL;

  if (curr_thread == NULL)
    caml_invalid_argument("Thread.exit: not initialized");

  /* In native code, we cannot call pthread_exit here because on some
     systems this raises a C++ exception, and ocamlopt-generated stack
     frames cannot be unwound.  Instead, we longjmp to the thread
     creation point (in caml_thread_start) or to the point in
     caml_main where caml_termination_hook will be called.
     Note that threads created in C then registered do not have
     a creation point (exit_buf == NULL).
 */
#ifdef NATIVE_CODE
  exit_buf = curr_thread->exit_buf;
#endif
  caml_thread_stop();
  if (exit_buf != NULL) {
    /* Native-code and (main thread or thread created by OCaml) */
    siglongjmp(exit_buf->buf, 1);
  } else {
    /* Bytecode, or thread created from C */
    st_thread_exit();
  }
  return Val_unit;  /* not reached */
}

/* Allow re-scheduling */

CAMLprim value caml_thread_yield(value unit)        /* ML */
{
  struct caml_locking_scheme* s;

  s = atomic_load(&caml_locking_scheme);
  if (s->can_skip_yield != NULL && s->can_skip_yield(s->context))
    return Val_unit;

  /* Do all the parts of a blocking section enter/leave except lock
     manipulation, which we'll do more efficiently in st_thread_yield. (Since
     our blocking section doesn't contain anything interesting, don't bother
     with saving errno.)
  */
  caml_raise_async_if_exception(caml_process_pending_signals_exn(),
                                "signal handler");
  caml_thread_save_runtime_state();
  /* caml_locking_scheme may have changed in caml_process_pending_signals_exn */
  s = atomic_load(&caml_locking_scheme);
  s->yield(s->context);
  if (atomic_load(&caml_locking_scheme) != s) {
    /* The lock we have is no longer the runtime lock */
    s->unlock(s->context);
    acquire_runtime_lock();
  }
  caml_thread_restore_runtime_state();
  caml_raise_async_if_exception(caml_process_pending_signals_exn(),
                                "signal handler");

  return Val_unit;
}

/* Suspend the current thread until another thread terminates */

CAMLprim value caml_thread_join(value th)          /* ML */
{
  st_retcode rc = caml_threadstatus_wait(Terminated(th));
  st_check_error(rc, "Thread.join");
  return Val_unit;
}

/* Mutex operations */

#define Mutex_val(v) (* ((st_mutex *) Data_custom_val(v)))

static void caml_mutex_finalize(value wrapper)
{
  st_mutex_destroy(Mutex_val(wrapper));
}

static int caml_mutex_compare(value wrapper1, value wrapper2)
{
  st_mutex mut1 = Mutex_val(wrapper1);
  st_mutex mut2 = Mutex_val(wrapper2);
  return mut1 == mut2 ? 0 : mut1 < mut2 ? -1 : 1;
}

static intnat caml_mutex_hash(value wrapper)
{
  return (intnat) (Mutex_val(wrapper));
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_compare,
  caml_mutex_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_ml_mutex_new(value unit)        /* ML */
{
  st_mutex mut = NULL;          /* suppress warning */
  value wrapper;
  st_check_error(st_mutex_create(&mut), "Mutex.create");
  wrapper = caml_alloc_custom(&caml_mutex_ops, sizeof(st_mutex *),
                              0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_ml_mutex_lock(value wrapper)     /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;

  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (st_mutex_trylock(mut) == MUTEX_PREVIOUSLY_UNLOCKED) return Val_unit;
  /* If unsuccessful, block on mutex */
  Begin_root(wrapper)           /* prevent the deallocation of mutex */
    caml_enter_blocking_section();
    retcode = st_mutex_lock(mut);
    caml_leave_blocking_section();
  End_roots();
  st_check_error(retcode, "Mutex.lock");
  return Val_unit;
}

CAMLprim value caml_ml_mutex_unlock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;
  /* PR#4351: no need to release and reacquire master lock */
  retcode = st_mutex_unlock(mut);
  st_check_error(retcode, "Mutex.unlock");
  return Val_unit;
}

CAMLprim value caml_ml_mutex_try_lock(value wrapper)           /* ML */
{
  st_mutex mut = Mutex_val(wrapper);
  st_retcode retcode;
  retcode = st_mutex_trylock(mut);
  if (retcode == MUTEX_ALREADY_LOCKED) return Val_false;
  st_check_error(retcode, "Mutex.try_lock");
  return Val_true;
}

/* Conditions operations */

#define Condition_val(v) (* (st_condvar *) Data_custom_val(v))

static void caml_condition_finalize(value wrapper)
{
  st_condvar_destroy(Condition_val(wrapper));
}

static int caml_condition_compare(value wrapper1, value wrapper2)
{
  st_condvar cond1 = Condition_val(wrapper1);
  st_condvar cond2 = Condition_val(wrapper2);
  return cond1 == cond2 ? 0 : cond1 < cond2 ? -1 : 1;
}

static intnat caml_condition_hash(value wrapper)
{
  return (intnat) (Condition_val(wrapper));
}

static struct custom_operations caml_condition_ops = {
  "_condition",
  caml_condition_finalize,
  caml_condition_compare,
  caml_condition_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_ml_condition_new(value unit)        /* ML */
{
  st_condvar cond = NULL;       /* suppress warning */
  value wrapper;
  st_check_error(st_condvar_create(&cond), "Condition.create");
  wrapper = caml_alloc_custom(&caml_condition_ops, sizeof(st_condvar *),
                              0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_ml_condition_wait(value wcond, value wmut)         /* ML */
{
  st_condvar cond = Condition_val(wcond);
  st_mutex mut = Mutex_val(wmut);
  st_retcode retcode;

  Begin_roots2(wcond, wmut)     /* prevent deallocation of cond and mutex */
    caml_enter_blocking_section();
    retcode = st_condvar_wait(cond, mut);
    caml_leave_blocking_section();
  End_roots();
  st_check_error(retcode, "Condition.wait");
  return Val_unit;
}

CAMLprim value caml_ml_condition_signal(value wrapper)           /* ML */
{
  st_check_error(st_condvar_signal(Condition_val(wrapper)),
                 "Condition.signal");
  return Val_unit;
}

CAMLprim value caml_ml_condition_broadcast(value wrapper)           /* ML */
{
  st_check_error(st_condvar_broadcast(Condition_val(wrapper)),
                 "Condition.broadcast");
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
  st_check_error(st_event_create(&ts), "Thread.create");
  wrapper = caml_alloc_custom(&caml_threadstatus_ops, sizeof(st_event *),
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
  st_event ts = Threadstatus_val(wrapper);
  st_retcode retcode;

  Begin_roots1(wrapper)         /* prevent deallocation of ts */
    caml_enter_blocking_section();
    retcode = st_event_wait(ts);
    caml_leave_blocking_section();
  End_roots();
  return retcode;
}
