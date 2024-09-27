/* Operations on mutexes from the OCaml stdlib */

#include <pthread.h>
#include <string.h>
#include <errno.h>

#ifdef CAML_INTERNALS
#include "caml/mlvalues.h"
#include "caml/memory.h"

typedef pthread_rwlock_t * sync_rwlock;
#define Rwlock_val(v) (* ((sync_rwlock *) Data_custom_val(v)))

typedef int sync_retcode;

#define RWLOCK_PREVIOUSLY_UNLOCKED 0

Caml_inline int sync_rwlock_rdlock(sync_rwlock m)
{
  return pthread_rwlock_rdlock(m);
}

Caml_inline int sync_rwlock_wrlock(sync_rwlock m)
{
  return pthread_rwlock_wrlock(m);
}

Caml_inline int sync_rwlock_unlock(sync_rwlock m)
{
  return pthread_rwlock_unlock(m);
}

Caml_inline int sync_rwlock_tryrdlock(sync_rwlock m)
{
  return pthread_rwlock_tryrdlock(m);
}

Caml_inline int sync_rwlock_trywrlock(sync_rwlock m)
{
  return pthread_rwlock_trywrlock(m);
}

/* c code to allocate a rwlock */
Caml_inline int sync_rwlock_create(sync_rwlock * res)
{
  int rc;
  sync_rwlock m;
  m = caml_stat_alloc_noexc(sizeof(pthread_rwlock_t));
  if (m == NULL) { rc = ENOMEM; goto error2; }
  rc = pthread_rwlock_init(m, NULL); // default read-write lock attributes are used
  if (rc != 0) { goto error1; }
  *res = m;
  return 0;
error1:
  caml_stat_free(m);
error2:
  return rc;
}

Caml_inline int sync_rwlock_destroy(sync_rwlock m)
{
  int rc;
  rc = pthread_rwlock_destroy(m);
  caml_stat_free(m);
  return rc;
}

#endif /* CAML_INTERNALS */
