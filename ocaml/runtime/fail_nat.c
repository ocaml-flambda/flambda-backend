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
    caml_exn_Finaliser_raised,
    caml_exn_Memprof_callback_raised,
    caml_exn_Signal_handler_raised;

/* Exception raising */

CAMLnoreturn_start extern void caml_raise_exception(caml_domain_state *state, value bucket)
    CAMLnoreturn_end;

CAMLnoreturn_start extern void caml_raise_async_exception(
  caml_domain_state *state, value bucket)
    CAMLnoreturn_end;

CAMLno_asan static value prepare_for_raise(value v, char *exception_pointer,
  int *turned_into_async_exn)
{
  Unlock_exn();

  CAMLassert(!Is_exception_result(v));

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
  {
    v = Extract_exception(v);

    /* [v] should now be raised as an asynchronous exception. */

    if (Caml_state->async_exception_pointer == NULL)
      caml_fatal_uncaught_exception(v);

    *turned_into_async_exn = 1;
    return v;
  }

  if (exception_pointer == NULL)
    caml_fatal_uncaught_exception(v);

  *turned_into_async_exn = 0;
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
  int is_async_exn;
  v = prepare_for_raise(v, Caml_state->exception_pointer, &is_async_exn);

  if (is_async_exn)
  {
    unwind_local_roots(Caml_state->async_exception_pointer);
    caml_raise_async_exception(Caml_state, v);
  }
  else
  {
    unwind_local_roots(Caml_state->exception_pointer);
    caml_raise_exception(Caml_state, v);
  }
}

CAMLno_asan void caml_raise_never_async(value v)
{
  int is_async_exn;
  v = prepare_for_raise(v, Caml_state->exception_pointer, &is_async_exn);

  unwind_local_roots(Caml_state->exception_pointer);
  caml_raise_exception(Caml_state, v);
}

CAMLexport value caml_wrap_if_async_exn(value result,
  pending_action_type action)
{
  CAMLparam1(result);
  value exn;
  value tag = Val_unit;
  value wrapped;

  if (!Is_exception_result(result)) CAMLreturn(result);

  /* CR mshinwell: think more about these cases (the eintr.ml gc test can
     trigger them -- signal handler executed from signal handler,
     presumably) */

  exn = Extract_exception(result);

  if (Is_block(exn)
      && Wosize_val(exn) == 2
      && (Field(exn, 0) == (value) caml_exn_Signal_handler_raised
          || Field(exn, 1) == (value) caml_exn_Finaliser_raised
          || Field(exn, 2) == (value) caml_exn_Memprof_callback_raised))
  {
    /* Don't double-wrap exceptions. */
    CAMLreturn(result);
  }

  wrapped = caml_alloc_small(2, 0);

  switch (action)
  {
    case pending_SIGNAL_HANDLER:
      tag = (value) caml_exn_Signal_handler_raised;
      break;

    case pending_FINALISER:
      tag = (value) caml_exn_Finaliser_raised;
      break;

    case pending_MEMPROF_CALLBACK:
      tag = (value) caml_exn_Memprof_callback_raised;
      break;

    default:
      CAMLassert(0);
  }

  Field(wrapped, 0) = tag;
  Field(wrapped, 1) = exn;

  CAMLreturn(Make_exception_result(wrapped));
}

CAMLno_asan
CAMLprim value caml_raise_async(value exn)
{
  int turned_into_async_exn;

  if (Caml_state->async_exceptions_masked)
  {
    if (exn == (value) caml_exn_Stack_overflow)
    {
      fprintf(stderr, "[ocaml] Stack overflow\n");
      abort();
    }
    return Val_unit;
  }

  exn = prepare_for_raise(exn, Caml_state->async_exception_pointer,
    &turned_into_async_exn);

  unwind_local_roots(Caml_state->async_exception_pointer);

  caml_raise_async_exception(Caml_state, exn);

  return Val_unit;
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
  /* Note that this is not an async exn. */
  caml_raise_constant((value)caml_exn_Out_of_memory);
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
  abort();  /* CR mshinwell: what are we supposed to use for "unreachable"? */
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

CAMLexport value caml_raise_async_if_exception(value result)
{
  if (Is_exception_result(result))
    caml_raise_async(Extract_exception(result));

  return result;
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

extern value (caml_callback_asm_async_exn)
  (caml_domain_state* state, value closure, value* args);

CAMLprim value caml_with_async_exns(value body_callback)
{
  value unit = Val_unit;
  value result = caml_callback_asm_async_exn(Caml_state, body_callback, &unit);

  if (!Is_exception_result(result))
    return result;

  /* All exceptions, including any async ones that arise during processing
     in [caml_raise], must be raised as non-async exceptions from this
     point. */
  caml_raise_never_async(Extract_exception(result));
  abort(); /* unreachable */
}

CAMLprim value caml_mask_async_exns(value unit)
{
  Caml_state->async_exceptions_masked = 1;
  return Val_unit;
}

CAMLprim value caml_unmask_async_exns(value unit)
{
  Caml_state->async_exceptions_masked = 0;
  return Val_unit;
}
