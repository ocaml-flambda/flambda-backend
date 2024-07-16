/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Raising exceptions from C. */

#include <stdio.h>
#include <signal.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/io.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/roots.h"
#include "caml/callback.h"
#include "caml/signals.h"
#include "caml/tsan.h"

/* The globals holding predefined exceptions */

typedef value caml_generated_constant[1];

extern caml_generated_constant
  caml_exn_Out_of_memory,
  caml_exn_Sys_error,
  caml_exn_Failure,
  caml_exn_Invalid_argument,
  caml_exn_End_of_file,
  caml_exn_Division_by_zero,
  caml_exn_Not_found,
  caml_exn_Match_failure,
  caml_exn_Sys_blocked_io,
  caml_exn_Stack_overflow,
  caml_exn_Assert_failure,
  caml_exn_Undefined_recursive_module;

/* Exception raising */

CAMLnoret extern
void caml_raise_exception (caml_domain_state* state, value bucket);

CAMLnoreturn_start
  extern void caml_raise_async_exception (caml_domain_state* state, value bucket)
CAMLnoreturn_end;

static void unwind_local_roots(char *limit_of_current_c_stack_chunk)
{
  while (Caml_state->local_roots != NULL &&
         (char *)Caml_state->local_roots < limit_of_current_c_stack_chunk)
  {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }
}

void caml_raise(value v)
{
  Caml_check_caml_state();
<<<<<<< HEAD
  char* limit_of_current_c_stack_chunk;

  Unlock_exn();

||||||| 121bedcfd2
  char* exception_pointer;

  Unlock_exn();

=======
  char* exception_pointer;
>>>>>>> 5.2.0
  CAMLassert(!Is_exception_result(v));

<<<<<<< HEAD
  /* Run callbacks here, so that a signal handler that arrived during
     a blocking call has a chance to interrupt the raising of EINTR */
  v = caml_process_pending_actions_with_root(v);
||||||| 121bedcfd2
  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
    v = Extract_exception(v);
=======
  caml_channel_cleanup_on_raise();

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
    v = Extract_exception(v);
>>>>>>> 5.2.0

  limit_of_current_c_stack_chunk = (char*)Caml_state->c_stack;

  if (limit_of_current_c_stack_chunk == NULL) {
    caml_terminate_signals();
    caml_fatal_uncaught_exception(v);
  }

<<<<<<< HEAD
  unwind_local_roots(limit_of_current_c_stack_chunk);
||||||| 121bedcfd2
  while (Caml_state->local_roots != NULL &&
         (char *) Caml_state->local_roots < exception_pointer) {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }

=======
  while (Caml_state->local_roots != NULL &&
         (char *) Caml_state->local_roots < exception_pointer) {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }

#if defined(WITH_THREAD_SANITIZER)
  caml_tsan_exit_on_raise_c(exception_pointer);
#endif

>>>>>>> 5.2.0
  caml_raise_exception(Caml_state, v);
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan void caml_raise_async(value v)
{
  Caml_check_caml_state();
  char* limit_of_current_c_stack_chunk;

  Unlock_exn();

  CAMLassert(!Is_exception_result(v));

  if (Stack_parent(Caml_state->current_stack) != NULL) {
    /* CR ocaml 5 effects: implement async exns + effects */
    caml_fatal_error("Effects not supported in conjunction with async exns");
  }

  /* Do not run callbacks here: we are already raising an async exn,
     so no need to check for another one, and avoiding polling here
     removes the risk of recursion in caml_raise */

  limit_of_current_c_stack_chunk = (char*)Caml_state->c_stack;

  if (limit_of_current_c_stack_chunk == NULL) {
    caml_terminate_signals();
    caml_fatal_uncaught_exception(v);
  }

  unwind_local_roots(limit_of_current_c_stack_chunk);
  Caml_state->exn_handler = Caml_state->async_exn_handler;
  Caml_state->raising_async_exn = 1;
  caml_raise_exception(Caml_state, v);
}

CAMLno_asan
void caml_raise_constant(value tag)
{
  caml_raise(tag);
}

void caml_raise_with_arg(value tag, value arg)
{
  CAMLparam2 (tag, arg);
  CAMLlocal1 (bucket);

  bucket = caml_alloc_small (2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  caml_raise(bucket);
  CAMLnoreturn;
}

void caml_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  bucket = caml_alloc (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  caml_raise(bucket);
  CAMLnoreturn;
}

void caml_raise_with_string(value tag, char const *msg)
{
  CAMLparam1(tag);
  value v_msg = caml_copy_string(msg);
  caml_raise_with_arg(tag, v_msg);
  CAMLnoreturn;
}

void caml_failwith (char const *msg)
{
  caml_raise_with_string((value) caml_exn_Failure, msg);
}

void caml_failwith_value (value msg)
{
  caml_raise_with_arg((value) caml_exn_Failure, msg);
}

void caml_invalid_argument (char const *msg)
{
  caml_raise_with_string((value) caml_exn_Invalid_argument, msg);
}

void caml_invalid_argument_value (value msg)
{
  caml_raise_with_arg((value) caml_exn_Invalid_argument, msg);
}

void caml_raise_out_of_memory(void)
{
  /* Note that this is not an async exn. */
  caml_raise_constant((value) caml_exn_Out_of_memory);
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan
void caml_raise_stack_overflow(void)
{
  caml_raise_async((value) caml_exn_Stack_overflow);
}

void caml_raise_sys_error(value msg)
{
  caml_raise_with_arg((value) caml_exn_Sys_error, msg);
}

void caml_raise_end_of_file(void)
{
  caml_raise_constant((value) caml_exn_End_of_file);
}

void caml_raise_zero_divide(void)
{
  caml_raise_constant((value) caml_exn_Division_by_zero);
}

void caml_raise_not_found(void)
{
  caml_raise_constant((value) caml_exn_Not_found);
}

void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant((value) caml_exn_Sys_blocked_io);
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */
static value array_bound_exn(void)
{
  static atomic_uintnat exn_cache = ATOMIC_UINTNAT_INIT(0);
  const value* exn = (const value*)atomic_load_acquire(&exn_cache);
  if (!exn) {
    exn = caml_named_value("Pervasives.array_bound_error");
    if (!exn) {
      fprintf(stderr, "Fatal error: exception "
        "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
    atomic_store_release(&exn_cache, (uintnat)exn);
  }
  return *exn;
}

void caml_array_bound_error(void)
{
  caml_raise(array_bound_exn());
}

void caml_array_bound_error_asm(void)
{
#if defined(WITH_THREAD_SANITIZER)
  char* exception_pointer = (char*)Caml_state->c_stack;
  caml_tsan_exit_on_raise_c(exception_pointer);
#endif

  /* This exception is raised directly from ocamlopt-compiled OCaml,
     not C, so we jump directly to the OCaml handler (and avoid GC) */
  caml_raise_exception(Caml_state, array_bound_exn());
}

static value array_align_exn(void)
{
  static atomic_uintnat exn_cache = ATOMIC_UINTNAT_INIT(0);
  const value* exn = (const value*)atomic_load_acquire(&exn_cache);
  if (!exn) {
    exn = caml_named_value("Pervasives.array_align_error");
    if (!exn) {
      fprintf(stderr, "Fatal error: exception "
        "Invalid_argument(\"address was misaligned\")\n");
      exit(2);
    }
    atomic_store_release(&exn_cache, (uintnat)exn);
  }
  return *exn;
}

void caml_array_align_error(void)
{
  caml_raise(array_align_exn());
}

void caml_array_align_error_asm(void)
{
  /* This exception is raised directly from ocamlopt-compiled OCaml,
     not C, so we jump directly to the OCaml handler (and avoid GC) */
  caml_raise_exception(Caml_state, array_align_exn());
}

int caml_is_special_exception(value exn) {
  return exn == (value) caml_exn_Match_failure
    || exn == (value) caml_exn_Assert_failure
    || exn == (value) caml_exn_Undefined_recursive_module;
}
