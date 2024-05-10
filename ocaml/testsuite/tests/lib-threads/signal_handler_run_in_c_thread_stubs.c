#include <pthread.h>
#include <signal.h>
#include <caml/mlvalues.h>

static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void* in_thread(void* unused)
{
  (void) pthread_cond_signal(&cond);
  /* Signal to be received in this thread by the OCaml signal handler */
  while (1);
}

value test_signal_handler_run_in_c_thread(value unit)
{
  pthread_t thread;
  pthread_create(&thread, NULL, &in_thread, NULL);
  pthread_cond_wait(&cond, &mutex);
  pthread_kill(thread, SIGUSR1);
  return Val_unit;
}
