/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2015 Indian Institute of Technology, Madras                */
/*   Copyright 2015 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "roots.h"
#include "domain.h"
#include "misc.h"

struct caml_heap_state;
struct pool;

struct caml_heap_state* caml_init_shared_heap(void);
void caml_teardown_shared_heap(struct caml_heap_state* heap);

value* caml_shared_try_alloc(struct caml_heap_state*, mlsize_t, tag_t, int);

struct pool* caml_pool_of_shared_block(value v);

void caml_shared_unpin(value v);

/* always readable by all threads
   written only by a single thread during STW periods */
typedef uintnat status;

Caml_inline int Has_status_hd(header_t hd, status s) {
  return (hd & (3 << 8)) == s;
}

Caml_inline header_t With_status_hd(header_t hd, status s) {
  return (hd & ~(3 << 8)) | s;
}

void caml_redarken_pool(struct pool*, scanning_action, void*);

intnat caml_sweep(struct caml_heap_state*, intnat);


/* must be called during STW */
void caml_cycle_heap_stw(void);

/* must be called on each domain
   (after caml_cycle_heap_stw) */
void caml_cycle_heap(struct caml_heap_state*);

/* Heap invariant verification (for debugging) */

/* caml_verify_heap must only be called while all domains are paused */
void caml_verify_heap(caml_domain_state *domain);

#endif /* CAML_INTERNALS */

#endif /* CAML_SHARED_HEAP_H */

