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

#ifndef CAML_THREADS_H
#define CAML_THREADS_H

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern void caml_enter_blocking_section (void);
CAMLextern void caml_leave_blocking_section (void);
#define caml_acquire_runtime_system caml_leave_blocking_section
#define caml_release_runtime_system caml_enter_blocking_section

/* Manage the master lock around the OCaml run-time system.
   Only one thread at a time can execute OCaml compiled code or
   OCaml run-time system functions.

   When OCaml calls a C function, the current thread holds the master
   lock.  The C function can release it by calling
   [caml_release_runtime_system].  Then, another thread can execute OCaml
   code.  However, the calling thread must not access any OCaml data,
   nor call any runtime system function, nor call back into OCaml.

   Before returning to its OCaml caller, or accessing OCaml data,
   or call runtime system functions, the current thread must
   re-acquire the master lock by calling [caml_acquire_runtime_system].

   Symmetrically, if a C function (not called from OCaml) wishes to
   call back into OCaml code, it should invoke [caml_acquire_runtime_system]
   first, then do the callback, then invoke [caml_release_runtime_system].

   For historical reasons, alternate names can be used:
     [caml_enter_blocking_section]  instead of  [caml_release_runtime_system]
     [caml_leave_blocking_section]  instead of  [caml_acquire_runtime_system]
   Intuition: a ``blocking section'' is a piece of C code that does not
   use the runtime system (typically, a blocking I/O operation).
*/

/* These functions are defined in the threads library, not the runtime */
#ifndef CAMLextern_libthreads
#define CAMLextern_libthreads CAMLextern
#endif
CAMLextern_libthreads int caml_c_thread_register(void);
CAMLextern_libthreads int caml_c_thread_unregister(void);

/* If a thread is created by C code (instead of by OCaml itself),
   it must be registered with the OCaml runtime system before
   being able to call back into OCaml code or use other runtime system
   functions.  Just call [caml_c_thread_register] once.
   Before the thread finishes, it must call [caml_c_thread_unregister].
   Both functions return 1 on success, 0 on error.
*/

enum caml_thread_type { Thread_type_caml, Thread_type_c_registered };
struct caml_locking_scheme {
  void* context;
  void (*lock)(void*);
  void (*unlock)(void*);

  /* If non-NULL, these functions are called when threads start and stop.
     For threads created by OCaml, that's at creation and termination.
     For threads created by C, that's at caml_c_thread_register/unregister.
     The lock is not held when these functions are called. */
  void (*thread_start)(void*, enum caml_thread_type);
  void (*thread_stop)(void*, enum caml_thread_type);

  /* Called after fork().
     The lock should be held after this function returns. */
  void (*reinitialize_after_fork)(void*);

  /* can_skip_yield and yield are both called with the lock held,
     and expect it held on return */
  int (*can_skip_yield)(void*);
  void (*yield)(void*);
};

CAMLextern_libthreads
struct caml_locking_scheme *caml_get_default_locking_scheme(void);

/* Switch to a new runtime locking scheme.

   The old runtime lock must be held (i.e. not in a blocking section),
   and the new runtime lock must not be held. After this function
   returns, the old lock is released and the new one is held.

   There is a period during this function when neither lock is held,
   so context-switches may occur. */
CAMLextern_libthreads
void caml_switch_runtime_locking_scheme(struct caml_locking_scheme*);

CAMLextern_libthreads
void caml_thread_save_runtime_state(void);

CAMLextern_libthreads
void caml_thread_restore_runtime_state(void);


#ifdef __cplusplus
}
#endif

#endif /* CAML_THREADS_H */
