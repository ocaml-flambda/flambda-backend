#define CAML_NAME_SPACE
#include <stdlib.h>
#include <pthread.h>
#include <sys/select.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/threads.h>


value blocking_section(value unused)
{
  caml_enter_blocking_section();
  caml_leave_blocking_section();
}


struct c_thread {
  value callback;
  pthread_t thread;
};

void* threadfn(struct c_thread* th)
{
  caml_c_thread_register();
  caml_leave_blocking_section();
  caml_callback(th->callback, Val_unit);
  caml_enter_blocking_section();
  caml_c_thread_unregister();
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
  if (pthread_mutex_lock(m) != 0) abort();
}

static void runtime_unlock(void* m)
{
  if (pthread_mutex_unlock(m) != 0) abort();
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
  s->reinitialize_after_fork = NULL;
  s->count_waiters = NULL;
  s->yield = NULL;
  caml_switch_runtime_locking_scheme(s);
  return Val_unit;
}
