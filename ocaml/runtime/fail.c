/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                   Mark Shinwell, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*   Copyright 2022 Jane Street Group LLC.                                */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Code related to the raising of exceptions that is shared between
   the bytecode and native code runtimes. */

#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/io.h"
#include "caml/callback.h"

CAMLno_asan
CAMLexport value caml_prepare_for_raise(value v, int *turned_into_async_exn)
{
  Unlock_exn();
  CAMLassert(!Is_exception_result(v));

  // avoid calling caml_raise recursively
  v = caml_process_pending_actions_with_root_exn(v);
  if (Is_exception_result(v))
  {
    v = Extract_exception(v);

    // [v] should now be raised as an asynchronous exception.

    if (turned_into_async_exn != NULL)
      *turned_into_async_exn = 1;
  }
  else
  {
    if (turned_into_async_exn != NULL)
      *turned_into_async_exn = 0;
  }

  return v;
}

CAMLexport value caml_check_async_exn0(value res, const char *msg,
                                       value stack_overflow_exn)
{
  value exn;
  const value *break_exn;

  if (!Is_exception_result(res))
    return res;

  exn = Extract_exception(res);

  if (exn == stack_overflow_exn)
    return res;

  /* [Break] is not introduced as a predefined exception (in predef.ml and
     stdlib.ml) since it causes trouble in conjunction with warnings about
     constructor shadowing e.g. in format.ml.
     "Sys.Break" must match stdlib/sys.mlp. */
  break_exn = caml_named_value("Sys.Break");
  if (break_exn != NULL && exn == *break_exn)
    return res;

  caml_fatal_uncaught_exception_with_message(exn, msg);
}
