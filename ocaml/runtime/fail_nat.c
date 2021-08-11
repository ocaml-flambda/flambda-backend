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
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/roots.h"
#include "caml/callback.h"

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
    caml_exn_Undefined_recursive_module,
    caml_exn_Async_exn;

/* Exception raising */

CAMLnoreturn_start extern void caml_raise_exception(caml_domain_state *state, value bucket)
    CAMLnoreturn_end;

CAMLnoreturn_start extern void caml_raise_async_exception(
  caml_domain_state *state, value bucket)
    CAMLnoreturn_end;

CAMLno_asan static value prepare_for_raise(value v, char *exception_pointer)
{
  Unlock_exn();

  CAMLassert(!Is_exception_result(v));

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
    v = Extract_exception(v);

  if (exception_pointer == NULL)
    caml_fatal_uncaught_exception(v);

  return v;
}

CAMLno_asan static void unwind_local_roots(char *exception_pointer)
{
  while (Caml_state->local_roots != NULL &&
         (char *)Caml_state->local_roots < exception_pointer)
  {
    Caml_state->local_roots = Caml_state->local_roots->next;
  }
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan void caml_raise(value v)
{
  v = prepare_for_raise(v, Caml_state->exception_pointer);
  unwind_local_roots(Caml_state->exception_pointer);
  caml_raise_exception(Caml_state, v);
}

CAMLnoreturn_start void caml_raise_async(value v) CAMLnoreturn_end;

static value wrap_async_exception(value exn)
{
  value wrapped;

  /* [Stack_overflow] can be raised from a signal handler, so we cannot
     rely on being able to allocate. */
  if (exn == (value) caml_exn_Stack_overflow) return exn;

  wrapped = caml_alloc_small(2, 0);
  Field(wrapped, 0) = (value) caml_exn_Async_exn;
  Field(wrapped, 1) = exn;

  return wrapped;
}

CAMLno_asan CAMLexport void caml_raise_async(value exn_unwrapped)
{
  value exn = wrap_async_exception(exn_unwrapped);

  exn = prepare_for_raise(wrap_async_exception(exn_unwrapped),
          Caml_state->async_exception_pointer);

  unwind_local_roots(Caml_state->async_exception_pointer);

  caml_raise_async_exception(Caml_state, exn);
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan void caml_raise_constant(value tag)
{
  caml_raise(tag);
}

void caml_raise_with_arg(value tag, value arg)
{
  CAMLparam2(tag, arg);
  CAMLlocal1(bucket);

  bucket = caml_alloc_small(2, 0);
  Field(bucket, 0) = tag;
  Field(bucket, 1) = arg;
  caml_raise(bucket);
  CAMLnoreturn;
}

void caml_raise_with_args(value tag, int nargs, value args[])
{
  CAMLparam1(tag);
  CAMLxparamN(args, nargs);
  value bucket;
  int i;

  CAMLassert(1 + nargs <= Max_young_wosize);
  bucket = caml_alloc_small(1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++)
    Field(bucket, 1 + i) = args[i];
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

void caml_failwith(char const *msg)
{
  caml_raise_with_string((value)caml_exn_Failure, msg);
}

void caml_failwith_value(value msg)
{
  caml_raise_with_arg((value)caml_exn_Failure, msg);
}

void caml_invalid_argument(char const *msg)
{
  caml_raise_with_string((value)caml_exn_Invalid_argument, msg);
}

void caml_invalid_argument_value(value msg)
{
  caml_raise_with_arg((value)caml_exn_Invalid_argument, msg);
}

void caml_raise_out_of_memory(void)
{
  caml_raise_async((value)caml_exn_Out_of_memory);
}

void caml_raise_out_of_memory_fatal(void)
{
  fprintf(stderr, "[ocaml] Out of memory\n");
  abort();
}

/* Used by the stack overflow handler -> deactivate ASAN (see
   segv_handler in signals_nat.c). */
CAMLno_asan void caml_raise_stack_overflow(void)
{
  caml_raise_async((value)caml_exn_Stack_overflow);
}

void caml_raise_sys_error(value msg)
{
  caml_raise_with_arg((value)caml_exn_Sys_error, msg);
}

void caml_raise_end_of_file(void)
{
  caml_raise_constant((value)caml_exn_End_of_file);
}

void caml_raise_zero_divide(void)
{
  caml_raise_constant((value)caml_exn_Division_by_zero);
}

void caml_raise_not_found(void)
{
  caml_raise_constant((value)caml_exn_Not_found);
}

void caml_raise_sys_blocked_io(void)
{
  caml_raise_constant((value)caml_exn_Sys_blocked_io);
}

CAMLexport value caml_raise_if_exception(value res)
{
  if (Is_exception_result(res))
    caml_raise(Extract_exception(res));
  return res;
}

/* We use a pre-allocated exception because we can't
   do a GC before the exception is raised (lack of stack descriptors
   for the ccall to [caml_array_bound_error]).  */

static const value *caml_array_bound_error_exn = NULL;

void caml_array_bound_error(void)
{
  if (caml_array_bound_error_exn == NULL)
  {
    caml_array_bound_error_exn =
        caml_named_value("Pervasives.array_bound_error");
    if (caml_array_bound_error_exn == NULL)
    {
      fprintf(stderr, "Fatal error: exception "
                      "Invalid_argument(\"index out of bounds\")\n");
      exit(2);
    }
  }
  /* This exception is raised directly from OCaml, not C,
     so we should not do the C-specific processing in caml_raise.
     (In particular, we must not invoke GC, even if signals are pending) */
  caml_raise_exception(Caml_state, *caml_array_bound_error_exn);
}

int caml_is_special_exception(value exn)
{
  return exn == (value)caml_exn_Match_failure || exn == (value)caml_exn_Assert_failure || exn == (value)caml_exn_Undefined_recursive_module;
}

CAMLprim value caml_with_async_exns(value body_callback)
{
  value exn, result;

  result = caml_callback_exn(body_callback, Val_unit);

  if (!Is_exception_result(result))
    return result;

  exn = Extract_exception(result);

  /* This shouldn't clobber a backtrace (leading back to a finaliser,
     signal handler, etc) because of the equality check in
     [caml_stash_backtrace]. */
  if (exn == (value) caml_exn_Stack_overflow
      || (Is_block(exn)
          && Field(exn, 0) == (value) caml_exn_Async_exn))
    caml_raise(exn);

  return result;
}
