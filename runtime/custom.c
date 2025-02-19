/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include <assert.h>

#include "caml/alloc.h"
#include "caml/camlatomic.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc_ctrl.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/memprof.h"

static_assert(sizeof(struct custom_operations) == CUSTOM_OPS_STRUCT_SIZE, "");

uintnat caml_custom_major_ratio = Custom_major_ratio_def;
uintnat caml_custom_minor_ratio = Custom_minor_ratio_def;
uintnat caml_custom_minor_max_bsz = Custom_minor_max_bsz_def;

mlsize_t caml_custom_get_max_major (void)
{
  /* The major ratio is a percentage relative to the major heap size.
     A complete GC cycle will be done every time 2/3 of that much
     memory is allocated for blocks in the major heap.  Assuming
     constant allocation and deallocation rates, this means there are
     at most [M/100 * major-heap-size] bytes of floating garbage at
     any time.  The reason for a factor of 2/3 (or 1.5) is, roughly
     speaking, because the major GC takes 1.5 cycles (previous cycle +
     marking phase) before it starts to deallocate dead blocks
     allocated during the previous cycle.  [heap_size / 150] is really
     [heap_size * (2/3) / 100] (but faster). */
  return caml_heap_size(Caml_state->shared_heap) / 150
         * caml_custom_major_ratio;
}

static value alloc_custom_gen (const struct custom_operations * ops,
                               uintnat bsz,
                               int minor_ok,
                               int local)
{
  mlsize_t wosize;
  CAMLparam0();
  CAMLlocal1(result);

  wosize = 1 + (bsz + sizeof(value) - 1) / sizeof(value);
  if (local) {
    CAMLassert(ops->finalize == NULL);
    result = caml_alloc_local(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
  }
  else if (wosize <= Max_young_wosize && minor_ok) {
    result = caml_alloc_small(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
    if (ops->finalize != NULL) {
      /* Record the extra resources in case the block gets promoted. */
      add_to_custom_table (&Caml_state->minor_tables->custom, result);
    }
  } else {
    result = caml_alloc_shr(wosize, Custom_tag);
    Custom_ops_val(result) = ops;
  }
  CAMLreturn(result);
}

Caml_inline mlsize_t get_max_minor (void)
{
  return
    Bsize_wsize (Caml_state->minor_heap_wsz) / 100 * caml_custom_minor_ratio;
}

CAMLexport value caml_alloc_custom(const struct custom_operations * ops,
                                   uintnat bsz,
                                   mlsize_t mem,
                                   mlsize_t max)
{
  return alloc_custom_gen(ops, bsz, /* minor_ok: */ 1, /* local: */ 0);
}

CAMLexport value caml_alloc_custom_local(const struct custom_operations * ops,
                                         uintnat bsz,
                                         mlsize_t mem,
                                         mlsize_t max)
{
  if (ops->finalize != NULL)
    caml_invalid_argument(
      "caml_alloc_custom_local: finalizers not supported");

  return alloc_custom_gen(ops, bsz, /* minor_ok: */ 1, /* local: */ 1);
}

CAMLexport value caml_alloc_custom_mem(const struct custom_operations * ops,
                                       uintnat bsz,
                                       mlsize_t mem)
{
  mlsize_t max_minor = get_max_minor (); /* total allocs before minor GC */
  mlsize_t max_minor_single;      /* largest allowed alloc on minor heap */
  if (caml_custom_minor_max_bsz > 100) {
    max_minor_single = caml_custom_minor_max_bsz;
  } else {
    max_minor_single = max_minor * caml_custom_minor_max_bsz / 100;
  }

  value v = alloc_custom_gen (ops, bsz,
                              /* minor_ok: */ (mem <= max_minor_single),
                              /* local: */ 0);
  size_t mem_words = (mem + sizeof(value) - 1) / sizeof(value);
  caml_memprof_sample_block(v, mem_words, mem_words, CAML_MEMPROF_SRC_CUSTOM);
  return v;
}

/* For each block allocated with [caml_alloc_custom_dep],
   the finalizer must call [caml_free_dependent_memory].
   [bsz] is the size in bytes of the payload inside the heap-allocated
   block, and [mem] is the size in bytes of the external memory
   held by this block.
*/
CAMLexport value caml_alloc_custom_dep (const struct custom_operations * ops,
                                        uintnat bsz,
                                        mlsize_t mem)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc_custom_mem(ops, bsz, mem);
  caml_alloc_dependent_memory (result, mem);
  CAMLreturn(result);
}

struct custom_operations_list {
  const struct custom_operations * ops;
  struct custom_operations_list * next;
};

typedef _Atomic(struct custom_operations_list *) custom_operations_table;

/* Thread-safety: the tables are append-only lists, hence we only need
   a CAS loop update them. */
static void push_custom_ops(custom_operations_table * table,
                            const struct custom_operations * ops)
{
  struct custom_operations_list * l =
    caml_stat_alloc(sizeof(struct custom_operations_list));
  l->ops = ops;
  struct custom_operations_list * prev = atomic_load(table);
  do {
    l->next = prev;
  } while (!atomic_compare_exchange_weak(table, &prev, l));
}

static custom_operations_table custom_ops_table = NULL;

CAMLexport void
caml_register_custom_operations(const struct custom_operations * ops)
{
  CAMLassert(ops->identifier != NULL);
  CAMLassert(ops->deserialize != NULL);
  push_custom_ops(&custom_ops_table, ops);
}

struct custom_operations * caml_find_custom_operations(const char * ident)
{
  struct custom_operations_list * l;
  for (l = atomic_load(&custom_ops_table); l != NULL; l = l->next)
    if (strcmp(l->ops->identifier, ident) == 0)
      return (struct custom_operations*)l->ops;
  return NULL;
}

static custom_operations_table custom_ops_final_table = NULL;

struct custom_operations * caml_final_custom_operations(final_fun fn)
{
  struct custom_operations_list * l;
  struct custom_operations * ops;
  for (l = atomic_load(&custom_ops_final_table); l != NULL; l = l->next)
    if (l->ops->finalize == fn) return (struct custom_operations*)l->ops;
  ops = caml_stat_alloc(sizeof(struct custom_operations));
  ops->identifier = "_final";
  ops->finalize = fn;
  ops->compare = custom_compare_default;
  ops->hash = custom_hash_default;
  ops->serialize = custom_serialize_default;
  ops->deserialize = custom_deserialize_default;
  ops->compare_ext = custom_compare_ext_default;
  ops->fixed_length = custom_fixed_length_default;
  push_custom_ops(&custom_ops_final_table, ops);
  return ops;
}

void caml_init_custom_operations(void)
{
  caml_register_custom_operations(&caml_int32_ops);
  caml_register_custom_operations(&caml_nativeint_ops);
  caml_register_custom_operations(&caml_int64_ops);
  caml_register_custom_operations(&caml_ba_ops);
  caml_register_custom_operations(&caml_float32_ops);
}
