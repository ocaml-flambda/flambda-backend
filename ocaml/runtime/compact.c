/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
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

#include <string.h>

#include "caml/address_class.h"
#include "caml/config.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/weak.h"
#include "caml/compact.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"

extern uintnat caml_percent_free;                   /* major_gc.c */
extern void caml_shrink_heap (char *);              /* memory.c */

// No compaction
