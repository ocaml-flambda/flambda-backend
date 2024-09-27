#define CAML_INTERNALS

#include "rwlock.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/domain_state.h"

#include "caml/signals.h"
#include "caml/sys.h"

#include "../../runtime/sync_posix.h"

/* Rwlock ops */
static void caml_rwlock_finalize(value wrapper)
{
  sync_rwlock_destroy(Rwlock_val(wrapper));
}

static int caml_rwlock_compare(value wrapper1, value wrapper2)
{
  sync_rwlock rwl1 = Rwlock_val(wrapper1);
  sync_rwlock rwl2 = Rwlock_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_rwlock_hash(value wrapper)
{
  return (intnat) (Rwlock_val(wrapper));
}

static struct custom_operations caml_rwlock_ops = {
  "_rwlock",
  caml_rwlock_finalize,
  caml_rwlock_compare,
  caml_rwlock_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_ml_rwlock_new(value unit)
{
  sync_rwlock rwl = NULL;
  value wrapper;
  sync_check_error(sync_rwlock_create(&rwl), "Rwlock.create");
  wrapper = caml_alloc_custom(&caml_rwlock_ops, sizeof(pthread_rwlock_t *), 0, 1);
  Rwlock_val(wrapper) = rwl;
  return wrapper;
}

CAMLprim value caml_ml_rwlock_rdlock(value wrapper)
{
  CAMLparam1(wrapper);
  sync_retcode retcode;
  sync_rwlock rwl = Rwlock_val(wrapper);
  if (sync_rwlock_tryrdlock(rwl) != RWLOCK_PREVIOUSLY_UNLOCKED) {
    caml_enter_blocking_section();
    retcode = sync_rwlock_rdlock(rwl);
    caml_leave_blocking_section();
    sync_check_error(retcode, "Rwlock.rdlock");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_rwlock_wrlock(value wrapper)
{
  CAMLparam1(wrapper);
  sync_retcode retcode;
  sync_rwlock rwl = Rwlock_val(wrapper);
  if (sync_rwlock_trywrlock(rwl) != RWLOCK_PREVIOUSLY_UNLOCKED) {
    caml_enter_blocking_section();
    retcode = sync_rwlock_wrlock(rwl);
    caml_leave_blocking_section();
    sync_check_error(retcode, "Rwlock.wrlock");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_rwlock_unlock(value wrapper)
{
  sync_rwlock rwl = Rwlock_val(wrapper);
  sync_retcode retcode;
  retcode = sync_rwlock_unlock(rwl);
  sync_check_error(retcode, "Rwlock.unlock");
  return Val_unit;
}
