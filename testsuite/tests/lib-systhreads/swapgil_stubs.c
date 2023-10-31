#define CAML_NAME_SPACE
#include <stdlib.h>
#include <pthread.h>
#include <sys/select.h>
#include <sched.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/signals.h>

/* should be caml/threads.h, but this is what ocamltest needs */
#include <threads.h>


value blocking_section(value unused)
{
  caml_enter_blocking_section();
  caml_leave_blocking_section();
  return Val_unit;
}


struct c_thread {
  value callback;
  pthread_t thread;
};

static __thread enum caml_thread_type thread_ty = Thread_type_caml;
static __thread int started = 0;
void* threadfn(struct c_thread* th)
{
  thread_ty = Thread_type_c_registered;
  caml_c_thread_register();
  caml_leave_blocking_section();
  caml_callback(th->callback, Val_unit);
  caml_enter_blocking_section();
  if (!started) abort();
  caml_c_thread_unregister();
  if (started) abort();
  return NULL;
}

value create_c_thread(value callback)
{
  struct c_thread* th = malloc(sizeof(struct c_thread));
  th->callback = callback;
  caml_register_global_root(&th->callback);
  pthread_create(&th->thread, NULL, (void*(*)(void*))threadfn, th);
  return 1 | (uintnat)th;
}

value join_c_thread(value vth)
{
  struct c_thread* th = (void*)(vth & ~1);
  caml_enter_blocking_section();
  pthread_join(th->thread, NULL);
  caml_leave_blocking_section();
  return Val_unit;
}

static void runtime_lock(void* m)
{
  /* A short timeout encourages the kernel to context-switch here,
     making it easier to trigger locking bugs */
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 1;
  select(0, NULL, NULL, NULL, &timeout);
  if (!started) abort();
  if (pthread_mutex_lock(m) != 0) abort();
}

static void runtime_unlock(void* m)
{
  if (!started) abort();
  if (pthread_mutex_unlock(m) != 0) abort();
}

static void runtime_yield(void* m)
{
  if (!started) abort();
  if (pthread_mutex_unlock(m) != 0) abort();
#ifdef __linux__
  /* sched_yield() doesn't do what we want in Linux 2.6 and up (PR#2663) */
  /* but not doing anything here would actually disable preemption (PR#7669) */
  struct timespec t;
  t.tv_sec = 0;
  t.tv_nsec = 1;
  nanosleep(&t, NULL);
#else
  sched_yield();
#endif
  if (pthread_mutex_lock(m) != 0) abort();
}

static void runtime_thread_start(void* m, enum caml_thread_type ty)
{
  if (ty != thread_ty) abort();
  started = 1;
}

static void runtime_thread_stop(void* m, enum caml_thread_type ty)
{
  if (ty != thread_ty) abort();
  started = 0;
}

static void runtime_reinitialize(void* m)
{
  /* This test doesn't fork, so this never runs. */
  abort();
}

value swap_gil_setup(value unused)
{
  caml_default_locking_scheme.thread_start = runtime_thread_start;
  caml_default_locking_scheme.thread_stop = runtime_thread_stop;
  started = 1;
  return Val_unit;
}

value swap_gil(value unused)
{
  struct caml_locking_scheme* s;
  pthread_mutex_t* m;
  pthread_mutexattr_t attr;

  s = malloc(sizeof(*s));
  m = malloc(sizeof(*m));
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init(m, &attr);
  s->context = m;
  s->lock = runtime_lock;
  s->unlock = runtime_unlock;
  s->thread_start = runtime_thread_start;
  s->thread_stop = runtime_thread_stop;
  s->reinitialize_after_fork = runtime_reinitialize;
  s->can_skip_yield = NULL;
  s->yield = runtime_yield;
  caml_switch_runtime_locking_scheme(s);
  return Val_unit;
}
