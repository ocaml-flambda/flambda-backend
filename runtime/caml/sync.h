/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on mutexes from the OCaml stdlib */

#ifndef CAML_SYNC_H
#define CAML_SYNC_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"

typedef pthread_mutex_t * sync_mutex;

#define Mutex_val(v) (* ((sync_mutex *) Data_custom_val(v)))

CAMLextern int caml_mutex_lock(sync_mutex mut);
CAMLextern int caml_mutex_unlock(sync_mutex mut);

<<<<<<< HEAD
/* If we're using glibc, use a custom condition variable implementation to
   avoid this bug: https://sourceware.org/bugzilla/show_bug.cgi?id=25847

   For now we only have this on linux because it directly uses the linux futex
   syscalls. */
#if defined(__linux__) && defined(__GNU_LIBRARY__) && defined(__GLIBC__) && defined(__GLIBC_MINOR__)
typedef struct {
  volatile unsigned counter;
} custom_condvar;
#define CUSTOM_COND_INITIALIZER {0}
#else
typedef pthread_cond_t custom_condvar;
#define CUSTOM_COND_INITIALIZER PTHREAD_COND_INITIALIZER
#endif

||||||| 121bedcfd2
=======
value caml_ml_mutex_lock(value wrapper);
value caml_ml_mutex_unlock(value wrapper);
value caml_ml_condition_broadcast(value wrapper);

>>>>>>> 5.2.0
#endif /* CAML_INTERNALS */

#endif /* CAML_SYNC_H */
