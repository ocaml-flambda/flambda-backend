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

#define CAML_INTERNALS

/* To walk the memory roots for garbage collection */

#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"
#include "caml/fiber.h"

CAMLexport _Atomic scan_roots_hook caml_scan_roots_hook =
  (scan_roots_hook)NULL;

void caml_do_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  caml_domain_state* d,
  int do_final_val)
{
  scan_roots_hook hook;
  caml_do_local_roots(f, fflags, fdata,
                      d->local_roots, d->current_stack, d->gc_regs);
  hook = atomic_load(&caml_scan_roots_hook);
  if (hook != NULL) (*hook)(f, fflags, fdata, d);
  caml_final_do_roots(f, fflags, fdata, d, do_final_val);

}
