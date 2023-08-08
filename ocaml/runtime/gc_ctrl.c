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

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/compact.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/eventlog.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#else
#include "caml/stacks.h"
#endif
#include "caml/startup_aux.h"

#ifndef NATIVE_CODE
extern uintnat caml_max_stack_size;    /* defined in stacks.c */
#endif

extern uintnat caml_major_heap_increment; /* percent or words; see major_gc.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_custom_major_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_max_bsz; /* see custom.c */

#define Next(hp) ((header_t *)(hp) + Whsize_hp (hp))

/* Check the heap structure (if compiled in debug mode) and
   gather statistics; return the stats if [returnstats] is true,
   otherwise return [Val_unit].
*/
static value heap_stats (int returnstats)
{
  CAMLparam0 ();
  intnat live_words = 0, live_blocks = 0,
         free_words = 0, free_blocks = 0, largest_free = 0,
         fragments = 0, heap_chunks = 0;

#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: heap check ###\n");
#endif

#ifdef DEBUG
  caml_final_invariant_check();
#endif

  if (returnstats){
    CAMLlocal1 (res);

    /* get a copy of these before allocating anything... */
    double minwords =
      Caml_state->stat_minor_words
      + (double) (Caml_state->young_alloc_end - Caml_state->young_ptr);
    double prowords = Caml_state->stat_promoted_words;
    double majwords =
      Caml_state->stat_major_words + (double) caml_allocated_words;
    intnat mincoll = Caml_state->stat_minor_collections;
    intnat majcoll = Caml_state->stat_major_collections;
    intnat heap_words = Caml_state->stat_heap_wsz;
    intnat cpct = Caml_state->stat_compactions;
    intnat forcmajcoll = Caml_state->stat_forced_major_collections;
    intnat top_heap_words = Caml_state->stat_top_heap_wsz;

    res = caml_alloc_tuple (17);
    Store_field (res, 0, caml_copy_double (minwords));
    Store_field (res, 1, caml_copy_double (prowords));
    Store_field (res, 2, caml_copy_double (majwords));
    Store_field (res, 3, Val_long (mincoll));
    Store_field (res, 4, Val_long (majcoll));
    Store_field (res, 5, Val_long (heap_words));
    Store_field (res, 6, Val_long (heap_chunks));
    Store_field (res, 7, Val_long (live_words));
    Store_field (res, 8, Val_long (live_blocks));
    Store_field (res, 9, Val_long (free_words));
    Store_field (res, 10, Val_long (free_blocks));
    Store_field (res, 11, Val_long (largest_free));
    Store_field (res, 12, Val_long (fragments));
    Store_field (res, 13, Val_long (cpct));
    Store_field (res, 14, Val_long (top_heap_words));
    Store_field (res, 15, Val_long (caml_stack_usage()));
    Store_field (res, 16, Val_long (forcmajcoll));
    CAMLreturn (res);
  }else{
    CAMLreturn (Val_unit);
  }
}

#ifdef DEBUG
void caml_heap_check (void)
{
  heap_stats (0);
}
#endif

CAMLprim value caml_gc_stat(value v)
{
  value result;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_STAT);
  CAMLassert (v == Val_unit);
  result = heap_stats (1);
  CAML_EV_END(EV_EXPLICIT_GC_STAT);
  return result;
}

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords =
    Caml_state->stat_minor_words
    + (double) (Caml_state->young_alloc_end - Caml_state->young_ptr);
  double prowords = Caml_state->stat_promoted_words;
  double majwords =
    Caml_state->stat_major_words + (double) caml_allocated_words;
  intnat mincoll = Caml_state->stat_minor_collections;
  intnat majcoll = Caml_state->stat_major_collections;
  intnat heap_words = Caml_state->stat_heap_wsz;
  intnat top_heap_words = Caml_state->stat_top_heap_wsz;
  intnat cpct = Caml_state->stat_compactions;
  intnat forcmajcoll = Caml_state->stat_forced_major_collections;
  intnat heap_chunks = Caml_state->stat_heap_chunks;

  res = caml_alloc_tuple (17);
  Store_field (res, 0, caml_copy_double (minwords));
  Store_field (res, 1, caml_copy_double (prowords));
  Store_field (res, 2, caml_copy_double (majwords));
  Store_field (res, 3, Val_long (mincoll));
  Store_field (res, 4, Val_long (majcoll));
  Store_field (res, 5, Val_long (heap_words));
  Store_field (res, 6, Val_long (heap_chunks));
  Store_field (res, 7, Val_long (0));
  Store_field (res, 8, Val_long (0));
  Store_field (res, 9, Val_long (0));
  Store_field (res, 10, Val_long (0));
  Store_field (res, 11, Val_long (0));
  Store_field (res, 12, Val_long (0));
  Store_field (res, 13, Val_long (cpct));
  Store_field (res, 14, Val_long (top_heap_words));
  Store_field (res, 15, Val_long (caml_stack_usage()));
  Store_field (res, 16, Val_long (forcmajcoll));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed()
{
  return (Caml_state->stat_minor_words
          + (double) (Caml_state->young_alloc_end - Caml_state->young_ptr));
}

CAMLprim value caml_gc_minor_words(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLreturn(caml_copy_double(caml_gc_minor_words_unboxed()));
}

CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords =
    Caml_state->stat_minor_words
    + (double) (Caml_state->young_alloc_end - Caml_state->young_ptr);
  double prowords = Caml_state->stat_promoted_words;
  double majwords =
    Caml_state->stat_major_words + (double) caml_allocated_words;

  res = caml_alloc_tuple (3);
  Store_field (res, 0, caml_copy_double (minwords));
  Store_field (res, 1, caml_copy_double (prowords));
  Store_field (res, 2, caml_copy_double (majwords));
  CAMLreturn (res);
}

CAMLprim value caml_gc_huge_fallback_count (value v)
{
  return Val_long (caml_huge_fallback_count);
}

CAMLprim value caml_gc_get(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  res = caml_alloc_tuple (11);
  Store_field (res, 0, Val_long (Caml_state->minor_heap_wsz));          /* s */
  Store_field (res, 1, Val_long (caml_major_heap_increment));           /* i */
  Store_field (res, 2, Val_long (caml_percent_free));                   /* o */
  Store_field (res, 3, Val_long (caml_verb_gc));                        /* v */
  Store_field (res, 4, Val_long (0));                    /* O */
#ifndef NATIVE_CODE
  Store_field (res, 5, Val_long (caml_max_stack_size));                 /* l */
#else
  Store_field (res, 5, Val_long (0));
#endif
  Store_field (res, 6, Val_long (0));              /* a */
  Store_field (res, 7, Val_long (caml_major_window));                   /* w */
  Store_field (res, 8, Val_long (caml_custom_major_ratio));             /* M */
  Store_field (res, 9, Val_long (caml_custom_minor_ratio));             /* m */
  Store_field (res, 10, Val_long (caml_custom_minor_max_bsz));          /* n */
  CAMLreturn (res);
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

static intnat norm_minsize (intnat s)
{
  intnat page_wsize = Wsize_bsize(Page_size);
  if (s < Minor_heap_min) s = Minor_heap_min;
  if (s > Minor_heap_max) s = Minor_heap_max;
  /* PR#9128 : Make sure the minor heap occupies an integral number of
     pages, so that no page contains both bytecode and OCaml
     values. This would confuse, e.g., caml_hash. */
  s = (s + page_wsize - 1) / page_wsize * page_wsize;
  return s;
}

static uintnat norm_window (intnat w)
{
  if (w < 1) w = 1;
  if (w > Max_major_window) w = Max_major_window;
  return w;
}

static uintnat norm_custom_maj (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_min (uintnat p)
{
  return Max (p, 1);
}

CAMLprim value caml_gc_set(value v)
{
  uintnat newpf;
  asize_t newheapincr;
  asize_t newminwsz;
  uintnat new_custom_maj, new_custom_min, new_custom_sz;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_SET);

  caml_verb_gc = Long_val (Field (v, 3));

#ifndef NATIVE_CODE
  caml_change_max_stack_size (Long_val (Field (v, 5)));
#endif

  newpf = norm_pfree (Long_val (Field (v, 2)));
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    caml_gc_message (0x20, "New space overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  }

  newheapincr = Long_val (Field (v, 1));
  if (newheapincr != caml_major_heap_increment){
    caml_major_heap_increment = newheapincr;
    if (newheapincr > 1000){
      caml_gc_message (0x20, "New heap increment size: %"
                       ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                       caml_major_heap_increment/1024);
    }else{
      caml_gc_message (0x20, "New heap increment size: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_major_heap_increment);
    }
  }

  /* This field was added in 4.03.0. */
  if (Wosize_val (v) >= 8){
    int old_window = caml_major_window;
    caml_set_major_window (norm_window (Long_val (Field (v, 7))));
    if (old_window != caml_major_window){
      caml_gc_message (0x20, "New smoothing window size: %d\n",
                       caml_major_window);
    }
  }

  /* These fields were added in 4.08.0. */
  if (Wosize_val (v) >= 11){
    new_custom_maj = norm_custom_maj (Long_val (Field (v, 8)));
    if (new_custom_maj != caml_custom_major_ratio){
      caml_custom_major_ratio = new_custom_maj;
      caml_gc_message (0x20, "New custom major ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_major_ratio);
    }
    new_custom_min = norm_custom_min (Long_val (Field (v, 9)));
    if (new_custom_min != caml_custom_minor_ratio){
      caml_custom_minor_ratio = new_custom_min;
      caml_gc_message (0x20, "New custom minor ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_ratio);
    }
    new_custom_sz = Long_val (Field (v, 10));
    if (new_custom_sz != caml_custom_minor_max_bsz){
      caml_custom_minor_max_bsz = new_custom_sz;
      caml_gc_message (0x20, "New custom minor size limit: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_max_bsz);
    }
  }

  /* Save field 0 before [v] is invalidated. */
  newminwsz = norm_minsize (Long_val (Field (v, 0)));

  /* ignore switching policies */

  /* Minor heap size comes last because it can raise [Out_of_memory]. */
  if (newminwsz != Caml_state->minor_heap_wsz){
    caml_gc_message (0x20, "New minor heap size: %"
                     ARCH_SIZET_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
    caml_set_minor_heap_size (Bsize_wsize (newminwsz));
  }
  CAML_EV_END(EV_EXPLICIT_GC_SET);

  /* The compaction may have triggered some finalizers that we need to call. */
  caml_process_pending_actions();

  return Val_unit;
}

CAMLprim value caml_gc_minor(value v)
{
  value exn;

  CAML_EV_BEGIN(EV_EXPLICIT_GC_MINOR);
  CAMLassert (v == Val_unit);
  caml_request_minor_gc ();
  // call the gc and call finalisers
  exn = caml_process_pending_actions_exn();
  CAML_EV_END(EV_EXPLICIT_GC_MINOR);
  caml_raise_async_if_exception(exn, "");
  return Val_unit;
}

static void test_and_compact (void)
{
  // No compaction
}

CAMLprim value caml_gc_major(value v)
{
  value exn;

  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR);
  CAMLassert (v == Val_unit);
  caml_gc_message (0x1, "Finishing major GC cycle (requested by user)\n");
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  test_and_compact ();
  // call finalisers
  exn = caml_process_pending_actions_exn();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR);
  caml_raise_async_if_exception(exn, "");
  return Val_unit;
}

CAMLprim value caml_gc_full_major(value v)
{
  value exn;

  CAML_EV_BEGIN(EV_EXPLICIT_GC_FULL_MAJOR);
  CAMLassert (v == Val_unit);
  caml_gc_message (0x1, "Full major GC cycle (requested by user)\n");
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  // call finalisers
  exn = caml_process_pending_actions_exn();
  if (Is_exception_result(exn)) goto cleanup;
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  ++ Caml_state->stat_forced_major_collections;
  test_and_compact ();
  // call finalisers
  exn = caml_process_pending_actions_exn();

cleanup:
  CAML_EV_END(EV_EXPLICIT_GC_FULL_MAJOR);
  caml_raise_async_if_exception(exn, "");

  return Val_unit;
}

CAMLprim value caml_gc_major_slice (value v)
{
  value exn = Val_unit;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR_SLICE);
  CAMLassert (Is_long (v));
  if (caml_gc_phase == Phase_idle){
    /* We need to start a new major GC cycle. Go through the pending_action
       machinery. */
    caml_request_major_slice ();
    exn = caml_process_pending_actions_exn ();
      /* Calls the major GC without passing [v] but the initial slice
         ignores this parameter anyway. */
  }else{
    caml_major_collection_slice (Long_val (v));
  }
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR_SLICE);
  caml_raise_async_if_exception(exn, "");
  return Val_long (0);
}

CAMLprim value caml_gc_compaction(value v)
{
  value exn;

  CAML_EV_BEGIN(EV_EXPLICIT_GC_COMPACT);
  CAMLassert (v == Val_unit);
  caml_gc_message (0x10, "Heap compaction requested\n");
  caml_empty_minor_heap ();
  caml_gc_message (0x1, "Full major GC cycle (compaction)\n");
  caml_finish_major_cycle ();
  // call finalisers
  exn = caml_process_pending_actions_exn();
  if (Is_exception_result(exn)) goto cleanup;
  caml_empty_minor_heap ();
  caml_finish_major_cycle ();
  ++ Caml_state->stat_forced_major_collections;
  // caml_compact_heap (-1); (no compaction)
  // call finalisers
  exn = caml_process_pending_actions_exn();

 cleanup:
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
  caml_raise_async_if_exception(exn, "");
  return Val_unit;
}

CAMLprim value caml_get_minor_free (value v)
{
  return Val_int (Caml_state->young_ptr - Caml_state->young_alloc_start);
}

CAMLprim value caml_get_major_bucket (value v)
{
  long i = Long_val (v);
  if (i < 0) caml_invalid_argument ("Gc.get_bucket");
  if (i < caml_major_window){
    i += caml_major_ring_index;
    if (i >= caml_major_window) i -= caml_major_window;
    CAMLassert (0 <= i && i < caml_major_window);
    return Val_long ((long) (caml_major_ring[i] * 1e6));
  }else{
    return Val_long (0);
  }
}

CAMLprim value caml_get_major_credit (value v)
{
  CAMLassert (v == Val_unit);
  return Val_long ((long) (caml_major_work_credit * 1e6));
}

/* [minor_size] and [major_size] are numbers of words
   [major_incr] is either a percentage or a number of words */
void caml_init_gc (uintnat minor_size, uintnat major_size,
                   uintnat major_incr, uintnat percent_fr,
                   uintnat percent_m, uintnat window,
                   uintnat custom_maj, uintnat custom_min,
                   uintnat custom_bsz, uintnat policy)
{
  uintnat major_bsize;

  major_bsize = Bsize_wsize(major_size);
  major_bsize = ((major_bsize + Page_size - 1) >> Page_log) << Page_log;

  caml_set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
  caml_major_heap_increment = major_incr;
  caml_percent_free = norm_pfree (percent_fr);
  // caml_set_allocation_policy (policy);
  caml_init_major_heap (major_bsize);
  caml_major_window = norm_window (window);
  caml_custom_major_ratio = norm_custom_maj (custom_maj);
  caml_custom_minor_ratio = norm_custom_min (custom_min);
  caml_custom_minor_max_bsz = custom_bsz;
  caml_gc_message (0x20, "Initial minor heap size: %"
                   ARCH_SIZET_PRINTF_FORMAT "uk words\n",
                   Caml_state->minor_heap_wsz / 1024);
  caml_gc_message (0x20, "Initial major heap size: %"
                   ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                   major_bsize / 1024);
  caml_gc_message (0x20, "Initial space overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  if (caml_major_heap_increment > 1000){
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                     caml_major_heap_increment / 1024);
  }else{
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     caml_major_heap_increment);
  }
  caml_gc_message (0x20, "Initial smoothing window: %d\n",
                   caml_major_window);
}


/* FIXME After the startup_aux.c unification, move these functions there. */

CAMLprim value caml_runtime_variant (value unit)
{
  CAMLassert (unit == Val_unit);
#if defined (DEBUG)
  return caml_copy_string ("d");
#elif defined (CAML_INSTR)
  return caml_copy_string ("i");
#else
  return caml_copy_string ("");
#endif
}

extern int caml_parser_trace;

CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  return caml_alloc_sprintf
    ("b=%d,H=%"F_Z"u,i=%"F_Z"u,l=%"F_Z"u,o=%"F_Z"u,p=%d,"
     "s=%"F_S"u,t=%"F_Z"u,v=%"F_Z"u,w=%d,W=%"F_Z"u",
     /* b */ (int) Caml_state->backtrace_active,
     /* h */ /* missing */ /* FIXME add when changed to min_heap_size */
     /* H */ caml_use_huge_pages,
     /* i */ caml_major_heap_increment,
#ifdef NATIVE_CODE
     /* l */ (uintnat) 0,
#else
     /* l */ caml_max_stack_size,
#endif
     /* o */ caml_percent_free,
     /* p */ caml_parser_trace,
     /* R */ /* missing */
     /* s */ Caml_state->minor_heap_wsz,
     /* t */ caml_trace_level,
     /* v */ caml_verb_gc,
     /* w */ caml_major_window,
     /* W */ caml_runtime_warnings
     );
#undef F_Z
#undef F_S
}

/* Control runtime warnings */

CAMLprim value caml_ml_enable_runtime_warnings(value vbool)
{
  caml_runtime_warnings = Bool_val(vbool);
  return Val_unit;
}

CAMLprim value caml_ml_runtime_warnings_enabled(value unit)
{
  CAMLassert (unit == Val_unit);
  return Val_bool(caml_runtime_warnings);
}
