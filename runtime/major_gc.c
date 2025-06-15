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

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

#include "caml/addrmap.h"
#include "caml/config.h"
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/gc_stats.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/shared_heap.h"
#include "caml/startup_aux.h"
#include "caml/weak.h"
#include "caml/custom.h"
#include "caml/minor_gc.h"

/* NB the MARK_STACK_INIT_SIZE must be larger than the number of objects
   that can be in a pool, see POOL_WSIZE */
#define MARK_STACK_INIT_SIZE (1 << 12)

/* The mark stack consists of two parts:
   1. the stack - a dynamic array of spans of fields that need to be marked, and
   2. the compressed stack - a bitset of fields that need to be marked.

   The stack is bounded relative to the heap size. When the stack
   overflows the bound, then entries from the stack are compressed and
   transferred into the compressed stack, expect for "large" entries,
   spans of more than BITS_PER_WORD entries, that are more compactly
   represented as spans and remain on the uncompressed stack.

   When the stack is empty, the compressed stack is processed.
   The compressed stack iterator marks the point up to which
   compressed stack entries have already been processed.
*/

typedef struct {
  value_ptr start;
  value_ptr end;
} mark_entry; /* represents fields in the span [start, end) */

struct mark_stack {
  mark_entry* stack;
  uintnat count;
  uintnat size;
  struct addrmap compressed_stack;
  addrmap_iterator compressed_stack_iter;
};

uintnat caml_percent_free = Percent_free_def;
uintnat caml_max_percent_free = Max_percent_free_def;
uintnat caml_percent_sweep_per_mark = 120; /* TODO: benchmark this value */

/* Allowable values of caml_gc_pacing_policy */

#define GC_PACING_OCAML_53        0 /* Pacing as in OCaml 5.3 (plus mark-delay) */
#define GC_PACING_2025            1 /* 2025 pacing */

uintnat caml_gc_pacing_policy = GC_PACING_2025;

/* The degree to which space_overhead should be dynamically adjusted according
   to the promotion rate.

   (0 = disabled, 50 = theoretically optimal space/time tradeoff) */
uintnat caml_gc_overhead_adjustment = 0;

/* This variable is only written with the world stopped, so it need not be
   atomic */
uintnat caml_major_cycles_completed = 0;

/* Custom blocks allocations (e.g. Bigarray) cause the GC to accelerate.
   This parameter bounds the maximum such acceleration to a multiple of the
   rate at which it would otherwise work. */
uintnat caml_custom_work_max_multiplier = 10;

/* [num_domains_to_sweep] records the number of domains to sweep in the current
   major cycle. The number is set to the [num_domains_in_stw] at the start of
   the cycle and _strictly decreases_ to 0.

   Domains created in a given cycle will not have any sweep work in that cycle.
   Sweep changes GARBAGE coloured objects in the domain's own pools to FREE
   (not a distinct colour; object header is set to 0) and adds them to the free
   list. No object will have the GARBAGE colour in the domain's own pools since
   the domain starts with an empty pool with no objects and new objects are
   allocated with colour MARKED. Hence, they do not affect
   [num_domains_to_sweep].

   Terminating domains terminate after sweeping is complete for their domain.
   */
static atomic_uintnat num_domains_to_sweep;

/* [num_domains_to_mark] records the number of domains to mark in the current
   major cycle. The number is set to the [num_domains_in_stw] at the start of
   the cycle. The value of [num_domains_to_mark] may decrease or increase.

   [num_domains_to_mark] may grow larger than the value of [num_domains_in_stw]
   at the start of the cycle. This is because [caml_modify] may push a block
   into a potentially empty mark stack of the newly spawned domain.

   Terminating domains empty their mark stack before terminating. */
static atomic_uintnat num_domains_to_mark;

/* [num_domains_to_ephe_sweep] is set to the [participating_count] at the start
   of the [Phase_sweep_ephe] and strictly decreases. */
static atomic_uintnat num_domains_to_ephe_sweep;

/* [num_domains_to_final_update_first] and [num_domains_to_final_update_last]
   are initialised to [num_domains_in_stw] at the start of the cycle. Whenever
   a domain finishes processing its first or last finalisers, it decrements the
   appropriate counter.

   Newly created domains increment both the counters. Terminating domain
   orphans its finalisers and then decrements the counters. See
   [caml_final_domain_terminate]. */
static atomic_uintnat num_domains_to_final_update_first;
static atomic_uintnat num_domains_to_final_update_last;

/* When domains terminate, they will orphan their finalisers. As mentioned in
   the comment attached to [num_domains_to_final_update_*] counters, a domain
   will decrement the counters when the corresponding finalisers are processed
   for that domain. We would like to preserve this invariant when adopting
   orphaned finalisers. To this end, we orphan and adopt finalisers only in
   [Phase_sweep_and_mark_main] when [num_domains_to_final_update_*] counters
   have not been decremented for the domain yet.

   [num_domains_orphaning_finalisers] keeps a count of the number of domains
   currently orphaning finalisers. This counter is only used in the
   [Phase_sweep_and_mark_main] to determine whether to proceed to
   [Phase_mark_final]. If domains are currently orphaning finalisers, we remain
   in [Phase_sweep_and_mark_main] so that the orphaned finalisers can be
   adopted before moving onto [Phase_mark_final] where the [GC.finalise]
   (finalise first) finalisers are processed. */
static atomic_uintnat num_domains_orphaning_finalisers = 0;

/* The GC uses separate units to count marking work and sweeping work, which are
   converted with these functions (following the Dest_source convention).
   In general, sweep units are the default, with conversions to/from mark units
   only used locally during marking */
static intnat Markwork_sweepwork(intnat sweep_work)
{
  if (caml_gc_pacing_policy == GC_PACING_OCAML_53) {
    return sweep_work;
  } else {
    return sweep_work * 100 / (intnat)caml_percent_sweep_per_mark;
  }
}

static intnat Sweepwork_markwork(intnat mark_work)
{
  if (caml_gc_pacing_policy == GC_PACING_OCAML_53) {
    return mark_work;
  } else {
    return mark_work * (intnat)caml_percent_sweep_per_mark / 100;
  }
}

/* These two counters keep track of how much work the GC is supposed to
   do in order to keep up with allocation. Both are in sweep work units.
   `total_work_incurred` increases when we allocate: the number of words
   allocated is converted to sweep work units and added to this counter.
   `total_work_completed` increases when the GC has done some work.
   The difference between the two is how much the GC is lagging behind
   (or in advance of) allocations.
   These counters can wrap around (see function `diffmod`) as long as they
   don't get too far apart, which is guaranteed by the limited size of
   memory.
*/
static atomic_uintnat total_work_incurred;
static atomic_uintnat total_work_completed;

gc_phase_t caml_gc_phase;

/* The caml_gc_phase global is only ever updated at the end of the STW
   section, by the last domain leaving a barrier. This means that no
   synchronization is required on most accesses.

   We know of two situations in the runtime that could run in parallel
   with a phase update, and cannot safely access the gc phase:

   - The domain_terminate logic runs after the thread has un-registered
     itself as a STW participant, so it may race with a STW section.

   - Opportunistic collections may happen while a domain is waiting on
     a STW barrier, so it might race with the code running inside
     another in-STW barrier. (It is possible that a deeper analysis of
     the current runtime code would in fact rule out such a race, but
     it is simpler to avoid phase accesses during opportunistic
     collections.)
 */

Caml_inline char caml_gc_phase_char(int may_access_gc_phase) {
  if (!may_access_gc_phase)
    return 'U';
  switch (caml_gc_phase) {
    case Phase_sweep_main:
      return 'S';
    case Phase_sweep_and_mark_main:
      return 'M';
    case Phase_mark_final:
      return 'F';
    case Phase_sweep_ephe:
      return 'E';
    default:
      return 'U';
  }
}

/* True when some domain wants to enter Phase_sweep_and_mark_main */
atomic_uintnat caml_gc_mark_phase_requested;

extern value caml_ephe_none; /* See weak.c */

static struct ephe_cycle_info_t {
  atomic_uintnat num_domains_todo;
  /* Number of domains that need to scan their ephemerons in the current major
   * GC cycle. This field is decremented when ephe_info->todo list at a domain
   * becomes empty.  */
  atomic_uintnat ephe_cycle;
  /* Ephemeron cycle count */
  atomic_uintnat num_domains_done;
  /* Number of domains that have marked their ephemerons in the current
   * ephemeron cycle. */
} ephe_cycle_info;
  /* In the first major cycle, there is no ephemeron marking to be done. */

/* ephe_cycle_info is always updated with the critical section protected by
 * ephe_lock or in the global barrier. However, the fields may be read without
 * the lock. */
static caml_plat_mutex ephe_lock = CAML_PLAT_MUTEX_INITIALIZER;

#define PREFETCH_BUFFER_SIZE  (1 << 8)
#define PREFETCH_BUFFER_MIN   64 /* keep pb at least this full */
#define PREFETCH_BUFFER_MASK  (PREFETCH_BUFFER_SIZE - 1)

typedef struct prefetch_buffer {
  uintnat enqueued;
  uintnat dequeued;
  uintnat waterline;
  value   buffer[PREFETCH_BUFFER_SIZE];
} prefetch_buffer_t;

Caml_inline bool pb_full(const prefetch_buffer_t *pb)
{
  return pb->enqueued == (pb->dequeued + PREFETCH_BUFFER_SIZE);
}

Caml_inline uintnat pb_size(const prefetch_buffer_t *pb)
{
  return pb->enqueued - pb->dequeued;
}

Caml_inline bool pb_above_waterline(const prefetch_buffer_t *pb)
{
  return ((pb->enqueued - pb->dequeued) > pb->waterline);
}

Caml_inline void pb_drain_mode(prefetch_buffer_t *pb)
{
  pb->waterline = 0;
}

Caml_inline void pb_fill_mode(prefetch_buffer_t *pb)
{
  pb->waterline = PREFETCH_BUFFER_MIN;
}

Caml_inline void pb_push(prefetch_buffer_t* pb, value v)
{
  CAMLassert(Is_block(v) && !Is_young(v));
  CAMLassert(v != Debug_free_major);
  CAMLassert(pb->enqueued < pb->dequeued + PREFETCH_BUFFER_SIZE);

  pb->buffer[pb->enqueued & PREFETCH_BUFFER_MASK] = v;
  pb->enqueued += 1;
}

Caml_inline value pb_pop(prefetch_buffer_t *pb)
{
  CAMLassert(pb->enqueued > pb->dequeued);

  value v = pb->buffer[pb->dequeued & PREFETCH_BUFFER_MASK];
  pb->dequeued += 1;
  return v;
}

Caml_inline void prefetch_block(value v)
{
  /* Prefetch a block so that scanning it later avoids cache misses.
     We will access at least the header, but we don't yet know how
     many of the fields we will access - the block might be already
     marked, not scannable, or very short. The compromise here is to
     prefetch the header and the first few fields.

     We issue two prefetches, with the second being a few words ahead
     of the first. Most of the time, these will land in the same
     cacheline, be coalesced by hardware, and so not cost any more
     than a single prefetch. Two memory operations are issued only
     when the two prefetches land in different cachelines.

     In the case where the block is not already in cache, and yet is
     already marked, not markable, or extremely short, then we waste
     somewhere between 1/8-1/2 of a prefetch operation (in expectation,
     depending on alignment, word size, and cache line size), which is
     cheap enough to make this worthwhile. */
  caml_prefetch((const void *)Hp_val(v));
  caml_prefetch((const void *)&Field(v, 3));
}

static void ephe_next_cycle (void)
{
  caml_plat_lock_blocking(&ephe_lock);

  (void)caml_atomic_counter_incr(&ephe_cycle_info.ephe_cycle);
  CAMLassert(atomic_load_acquire(&ephe_cycle_info.num_domains_done) <=
             atomic_load_acquire(&ephe_cycle_info.num_domains_todo));
  (void)caml_atomic_counter_init(&ephe_cycle_info.num_domains_done, 0);

  caml_plat_unlock(&ephe_lock);
}

static void ephe_todo_list_emptied (void)
{
  /* If we haven't started marking, the todo list can grow (during ephemeron
     allocation), so we should not yet announce that it has emptied */
  CAMLassert (caml_marking_started());
  caml_plat_lock_blocking(&ephe_lock);

  /* Force next ephemeron marking cycle in order to avoid reasoning about
   * whether the domain has already incremented
   * [ephe_cycle_info.num_domains_done] counter. */
  caml_atomic_counter_init(&ephe_cycle_info.num_domains_done, 0);
  (void)caml_atomic_counter_incr(&ephe_cycle_info.ephe_cycle);

  /* Since the todo list is empty, this domain does not need to participate in
   * further ephemeron cycles. */
  (void)caml_atomic_counter_decr(&ephe_cycle_info.num_domains_todo);
  CAMLassert(atomic_load_acquire(&ephe_cycle_info.num_domains_done) <=
             atomic_load_acquire(&ephe_cycle_info.num_domains_todo));

  caml_plat_unlock(&ephe_lock);
}

/* Begin ephemeron marking by making all 'live' ephes become 'todo' */
static void begin_ephe_marking(void)
{
  caml_domain_state* domain = Caml_state;
  CAMLassert(domain->ephe_info->todo == (value) NULL);
  domain->ephe_info->todo = domain->ephe_info->live;
  domain->ephe_info->live = (value) NULL;
  domain->ephe_info->must_sweep_ephe = 0;
  domain->ephe_info->cycle = 0;
  domain->ephe_info->cursor.todop = NULL;
  domain->ephe_info->cursor.cycle = 0;
}

/* Record that ephemeron marking was done for the given ephemeron cycle. */
static void record_ephe_marking_done (uintnat ephe_cycle)
{
  CAMLassert (ephe_cycle <= atomic_load_acquire(&ephe_cycle_info.ephe_cycle));
  CAMLassert (Caml_state->marking_done);

  if (ephe_cycle < atomic_load_acquire(&ephe_cycle_info.ephe_cycle))
    return;

  caml_plat_lock_blocking(&ephe_lock);
  if (ephe_cycle == atomic_load(&ephe_cycle_info.ephe_cycle)) {
    Caml_state->ephe_info->cycle = ephe_cycle;
    (void)caml_atomic_counter_incr(&ephe_cycle_info.num_domains_done);
    CAMLassert(atomic_load_acquire(&ephe_cycle_info.num_domains_done) <=
               atomic_load_acquire(&ephe_cycle_info.num_domains_todo));
  }
  caml_plat_unlock(&ephe_lock);
}

/*******************************************************************************
 * Orphaning and adoption
 ******************************************************************************/

/* These are biased data structures left over from terminating domains.

   Synchronization:
   - operations that mutate the structure
     (adding new orphaned values or adopting orphans)
     are protected from each other using [orphaned_lock];
     this is simpler than using atomic lists, and not performance-sensitive
   - the read-only function [no_orphaned_work()] uses atomic accesses
     to avoid taking a lock (it is called more often)
 */
static struct {
  value _Atomic ephe_list_live;
  struct caml_final_info * _Atomic final_info;
} orph_structs = {0, NULL};

static caml_plat_mutex orphaned_lock = CAML_PLAT_MUTEX_INITIALIZER;

Caml_inline value ephe_list_tail(value e)
{
  value last = 0;
  while (e != 0) {
    CAMLassert (Tag_val(e) == Abstract_tag);
    last = e;
    e = Ephe_link(e);
  }
  return last;
}

#ifdef DEBUG
static void orph_ephe_list_verify_status (int status)
{
  caml_plat_lock_blocking(&orphaned_lock);

  value v = orph_structs.ephe_list_live;

  while (v) {
    CAMLassert (Tag_val(v) == Abstract_tag);
    CAMLassert (Has_status_val(v, status));
    v = Ephe_link(v);
  }
  caml_plat_unlock(&orphaned_lock);
}
#endif

#define EPHE_MARK_DEFAULT 0
#define EPHE_MARK_FORCE_ALIVE 1

static intnat ephe_mark (intnat budget, uintnat for_cycle, int force_alive);

void caml_orphan_ephemerons (caml_domain_state* domain_state)
{
  struct caml_ephe_info* ephe_info = domain_state->ephe_info;
  if (ephe_info->todo == 0 &&
      ephe_info->live == 0 &&
      ephe_info->must_sweep_ephe == 0)
    return;

  /* Force all ephemerons and their data on todo list to be alive */
  if (ephe_info->todo) {
    while (ephe_info->todo) {
      ephe_mark (100000, 0, EPHE_MARK_FORCE_ALIVE);
    }
    ephe_todo_list_emptied ();
  }
  CAMLassert (ephe_info->todo == 0);

  if (ephe_info->live) {
    value live_tail = ephe_list_tail(ephe_info->live);
    CAMLassert(Ephe_link(live_tail) == 0);

    caml_plat_lock_blocking(&orphaned_lock);
    Ephe_link(live_tail) = orph_structs.ephe_list_live;
    orph_structs.ephe_list_live = ephe_info->live;
    ephe_info->live = 0;
    caml_plat_unlock(&orphaned_lock);
  }

  if (ephe_info->must_sweep_ephe) {
    ephe_info->must_sweep_ephe = 0;
    (void)caml_atomic_counter_decr(&num_domains_to_ephe_sweep);
  }
  CAMLassert (ephe_info->must_sweep_ephe == 0);
  CAMLassert (ephe_info->live == 0);
  CAMLassert (ephe_info->todo == 0);
}

void caml_orphan_finalisers (caml_domain_state* domain_state)
{
  struct caml_final_info* f = domain_state->final_info;

  if (f->todo_head != NULL || f->first.size != 0 || f->last.size != 0) {
    /* have some final structures */
    (void)caml_atomic_counter_incr(&num_domains_orphaning_finalisers);
    if (caml_gc_phase != Phase_sweep_and_mark_main) {
      /* Force a major GC cycle to simplify constraints for orphaning
         finalisers. See note attached to the declaration of
         [num_domains_orphaning_finalisers] variable in major_gc.c */
      caml_finish_major_cycle(Compaction_none);
    }
    CAMLassert(caml_gc_phase == Phase_sweep_and_mark_main);
    CAMLassert (!f->updated_first);
    CAMLassert (!f->updated_last);

    /* Add the finalisers to [orph_structs] */
    caml_plat_lock_blocking(&orphaned_lock);
    f->next = orph_structs.final_info;
    orph_structs.final_info = f;
    caml_plat_unlock(&orphaned_lock);

    /* Create a dummy final info */
    f = domain_state->final_info = caml_alloc_final_info();
    (void)caml_atomic_counter_decr(&num_domains_orphaning_finalisers);
  }

  /* [caml_orphan_finalisers] is called in a while loop in [domain_terminate].
     We take care to decrement the [num_domains_to_final_update*] counters only
     if we have not already decremented it for the current cycle. */
  if(!f->updated_first) {
    (void)caml_atomic_counter_decr(&num_domains_to_final_update_first);
    f->updated_first = 1;
  }
  if(!f->updated_last) {
    (void)caml_atomic_counter_decr(&num_domains_to_final_update_last);
    f->updated_last = 1;
  }
}

static int no_orphaned_work (void)
{
  return
    atomic_load_acquire(&orph_structs.ephe_list_live) == 0 &&
    atomic_load_acquire(&orph_structs.final_info) == NULL;
}

static void adopt_orphaned_work (int expected_status)
{
  caml_domain_state* domain_state = Caml_state;
  value orph_ephe_list_live, last;
  struct caml_final_info *f, *myf, *temp;

#ifdef DEBUG
  orph_ephe_list_verify_status(expected_status);
#endif

  if (no_orphaned_work() || caml_domain_is_terminating())
    return;

  caml_plat_lock_blocking(&orphaned_lock);

  orph_ephe_list_live = orph_structs.ephe_list_live;
  orph_structs.ephe_list_live = 0;

  f = orph_structs.final_info;
  orph_structs.final_info = NULL;

  caml_plat_unlock(&orphaned_lock);

  if (orph_ephe_list_live) {
    last = ephe_list_tail(orph_ephe_list_live);
    CAMLassert(Ephe_link(last) == 0);
    Ephe_link(last) = domain_state->ephe_info->live;
    domain_state->ephe_info->live = orph_ephe_list_live;
  }

  while (f != NULL) {
    myf = domain_state->final_info;
    CAMLassert (caml_gc_phase == Phase_sweep_and_mark_main);
    /* Since we are in [Phase_sweep_and_mark_main], the current domain has not
       updated its finalisers. */
    CAMLassert (!myf->updated_first);
    CAMLassert (!myf->updated_last);
    if (f->todo_head) {
      /* Adopt the finalising set. */
      if (myf->todo_tail == NULL) {
        CAMLassert(myf->todo_head == NULL);
        myf->todo_head = f->todo_head;
        myf->todo_tail = f->todo_tail;
      } else {
        myf->todo_tail->next = f->todo_head;
        myf->todo_tail = f->todo_tail;
      }
    }

    /* Adopt the finalisable set */
    if (f->first.young > 0) {
      caml_final_merge_finalisable (&f->first, &myf->first);
    }
    if (f->last.young > 0) {
      caml_final_merge_finalisable (&f->last, &myf->last);
    }

    temp = f;
    f = f->next;
    caml_stat_free (temp);
  }
}

static inline intnat max2 (intnat a, intnat b)
{
  if (a > b){
    return a;
  }else{
    return b;
  }
}

static inline intnat min2 (intnat a, intnat b)
{
  if (a < b){
    return a;
  }else{
    return b;
  }
}

/* Take two natural numbers n1 and n2 and let N = 2^{64}.
   Assume that n1 and n2 are not too far apart (less than N/2).
   Given unsigned numbers x1 = n1 modulo N and x2 = n2 modulo N, return
   the (signed) difference between n1 and n2.
*/
static inline intnat diffmod (uintnat x1, uintnat x2)
{
  return (intnat) (x1 - x2);
}

/* Reset the work and alloc counters to be equal to each other, by
 * setting them both equal to the "larger" (in the wrapping-around
 * sense we are using here for total_work_completed/incurred).
 *
 * For use at times when we have disturbed the major GC from its usual
 * pacing and tempo, for example, after any synchronous major
 * collection.
 */

void caml_reset_major_pacing(void)
{
  bool res;
  do {
    uintnat incurred = atomic_load(&total_work_incurred);
    uintnat completed = atomic_load(&total_work_completed);
    uintnat target = incurred;
    if (diffmod(completed, incurred) > 0) {
      target = completed;
    }
    res = (atomic_compare_exchange_strong(&total_work_incurred, &incurred, target) &&
           atomic_compare_exchange_strong(&total_work_completed, &completed, target));
  } while (!res);
}

static uintnat mark_work_done_between_slices(void)
{
  uintnat work = Caml_state->mark_work_done_between_slices;
  Caml_state->mark_work_done_between_slices = 0;
  return work;
}

static uintnat sweep_work_done_between_slices(void)
{
  uintnat work = Caml_state->sweep_work_done_between_slices;
  Caml_state->sweep_work_done_between_slices = 0;
  return work;
}

/* Apply the GC pacing policy to determine how much work this domain
 * should do on a slice, measured in words of sweep-work. Parameters:
 * - `heap_words` is the total allocated size of this domain's heap.
 * - `allocated_words` is the number of words allocated on-heap by this
 *    domain since the last slice.
 * - `allocated_direct_words` is the number of words allocated directly to
 *   the major heap (not promoted) since the last slice.
 * - `dependent_words` is the number of words allocated off-heap by this
 *    domain since the last slice. */

static uintnat gc_slice_work(uintnat heap_words,
                             uintnat allocated_words,
                             uintnat allocated_direct_words,
                             uintnat dependent_words,
                             uintnat minor_words)
{
  switch (caml_gc_pacing_policy) {
    case GC_PACING_OCAML_53: {
      /* Pacing policy from OCaml 5.3. */

      /* Extra work factor due to dependent allocation. */

      /* The custom major ratio is a percentage relative to the major
         heap size. A complete GC cycle will be done every time 2/3 of
         that much memory is allocated in the major heap. Assuming
         constant allocation and deallocation rates, this means there
         are at most [M/100 * major-heap-size] bytes of floating
         garbage at any time. The reason for a factor of 2/3 is,
         roughly speaking, because the major GC takes 1.5 cycles
         (previous cycle + marking phase) before it starts to
         deallocate dead blocks allocated during the previous
         cycle. */
      double custom_max_major =
        Bsize_wsize(heap_words) * (2.0/3) / 100 * caml_custom_major_ratio;
      double extra_factor = Bsize_wsize(dependent_words) / custom_max_major;
      if (extra_factor > 1.0) extra_factor = 1.0;

      /*
         Free memory at the start of the GC cycle (garbage + free list) (assumed):
                     FM = heap_words * caml_percent_free
                          / (100 + caml_percent_free)

         Assuming steady state and enforcing a constant allocation rate, then
         FM is divided in 2/3 for garbage and 1/3 for free list.
                  G = 2 * FM / 3
         G is also the amount of memory that will be used during this cycle
         (still assuming steady state).

         Proportion of G consumed since the previous slice:
                  PH = dom_st->allocated_words / G
                    = dom_st->allocated_words * 3 * (100 + caml_percent_free)
                      / (2 * heap_words * caml_percent_free)
         Proportion of extra-heap resources consumed since the previous slice:
                  PE = dom_st->extra_heap_resources
         Proportion of total work to do in this slice:
                  P  = max (PH, PE)
         Amount of marking work for the GC cycle:
                  MW = heap_words * 100 / (100 + caml_percent_free)
         Amount of sweeping work for the GC cycle:
         SW = heap_sweep_words
         Amount of total work for the GC cycle:
         TW = MW + SW
         = heap_words * 100 / (100 + caml_percent_free) + heap_sweep_words

         Amount of work for this slice:
         S = P * TW
      */
      uintnat heap_sweep_words = heap_words;

      uintnat total_cycle_work =
        heap_sweep_words + (heap_words * 100 / (100 + caml_percent_free));

      uintnat alloc_work;
      if (heap_words > 0) {
        double alloc_ratio = /* PH */
          allocated_words * 3.0 * (100 + caml_percent_free)
          / (heap_words * caml_percent_free * 2.0);
        alloc_work = (uintnat) (total_cycle_work * alloc_ratio);
      } else {
        alloc_work = 0;
      }

      uintnat offheap_work = (uintnat) (extra_factor * (double) total_cycle_work);
      uintnat clamp = alloc_work * caml_custom_work_max_multiplier;
      if (offheap_work > clamp) {
        CAML_GC_MESSAGE(POLICY, "Work clamped to %"
                        ARCH_INTNAT_PRINTF_FORMAT "d\n",
                        clamp);
        offheap_work = clamp;
      }

      return max2 (alloc_work, offheap_work);
    }
  case GC_PACING_2025: {
    /* Shiny new 2025 Doligez/Dolan pacing policy */
    double sweep_per_mark = (double)caml_percent_sweep_per_mark / 100.0;
    double space_overhead = (double)caml_percent_free / 100.0;
    if (caml_gc_overhead_adjustment != 0) {
      /* In theory, we should adjust according to the allocation rate.

         However, this produces a dependence on real time, making the GC
         nondeterministic even for single-threaded programs and doing weird
         things for programs which pause in I/O for a long time.

         Instead, we use minor heap allocation as a proxy for time, so instead
         of measuring the allocation rate we measure the promotion rate.

         The promotion rate is scaled so that the nominal promotion rate of 10%
         comes out as 1.0, and direct-to-major allocations are deemed to have
         this nominal promotion rate. */
      double denominator = 10.0 * allocated_direct_words + minor_words;
      if (denominator != 0.0) {
        double scaled_prom_rate =
          (10.0 * allocated_words) / denominator;
        /* Clamp to some reasonable range */
        if (scaled_prom_rate > 10.) scaled_prom_rate = 10.;
        if (scaled_prom_rate < 0.1) scaled_prom_rate = 0.1;
        space_overhead *= pow(scaled_prom_rate,
                              caml_gc_overhead_adjustment * 1e-2);
      }
    }
    double sweep_per_dep_alloc = (1 + 2.0 * sweep_per_mark) / space_overhead;
    double sweep_per_alloc = 1 + sweep_per_dep_alloc;

    return (uintnat) (sweep_per_alloc * allocated_words +
                      sweep_per_dep_alloc * dependent_words);

  }
  default:
    caml_fatal_error("Unknown GC pacing policy %"ARCH_INTNAT_PRINTF_FORMAT"u.",
                     caml_gc_pacing_policy);
  }
}

/* The [log_events] parameter is used to disable writing to the ring, to
      avoid logging events when the calling domain is not part of the
      Stop-The-World (STW) participant set. If the domain is not part of
      the STW set, the ring could be torn down concurrently while this domain
      attempts to write to it. */
static void update_major_slice_work(intnat howmuch,
                                    int may_access_gc_phase,
                                    bool log_events)
{
  caml_domain_state *dom_st = Caml_state;

  uintnat work_done_between_slices =
    Sweepwork_markwork(mark_work_done_between_slices()) +
    sweep_work_done_between_slices();
  atomic_fetch_add (&total_work_completed, work_done_between_slices);
  dom_st->stat_major_work_done += work_done_between_slices;

  uintnat my_alloc_count = dom_st->allocated_words;
  uintnat my_alloc_direct_count = dom_st->allocated_words_direct;
  uintnat my_dependent_count = Wsize_bsize (dom_st->allocated_dependent_bytes);
  uintnat last_minor_words = dom_st->minor_words_at_last_slice;
  uintnat curr_minor_words = caml_minor_words_allocated();
  uintnat my_minor_count = curr_minor_words - last_minor_words;
  dom_st->stat_major_words += dom_st->allocated_words;
  dom_st->stat_major_dependent_bytes += dom_st->allocated_dependent_bytes;
  dom_st->allocated_words = 0;
  dom_st->allocated_words_direct = 0;
  dom_st->allocated_dependent_bytes = 0;
  dom_st->minor_words_at_last_slice = curr_minor_words;

  uintnat heap_words = Wsize_bsize(caml_heap_size(dom_st->shared_heap));

  uintnat new_work =
    gc_slice_work(heap_words,
                  my_alloc_count,
                  my_alloc_direct_count,
                  my_dependent_count,
                  my_minor_count);

  atomic_fetch_add (&total_work_incurred, new_work);

  if (howmuch == AUTO_TRIGGERED_MAJOR_SLICE ||
      howmuch == GC_CALCULATE_MAJOR_SLICE) {
    dom_st->slice_target = atomic_load (&total_work_incurred);
    dom_st->slice_budget = 0;
  }else{
    /* forced or opportunistic GC slice with explicit quantity */
    /* already reached slice_target */
    dom_st->slice_target = atomic_load (&total_work_completed);
    dom_st->slice_budget = howmuch;
  }

  CAML_GC_MESSAGE(POLICY, "Major slice [%c] work. Policy="
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u. Allocation: "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u words, "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u direct, "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u dependent, "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u minor. Heap: "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "u words. Work: "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "d work, "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "d new_work, "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "d slice_budget\n",
                  caml_gc_phase_char(may_access_gc_phase),
                  caml_gc_pacing_policy,
                  my_alloc_count,
                  my_alloc_direct_count,
                  my_dependent_count,
                  my_minor_count,
                  heap_words,
                  diffmod(atomic_load(&total_work_incurred),
                          atomic_load(&total_work_completed)),
                  new_work,
                  dom_st->slice_budget);

  if (log_events) {
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_ALLOC_WORDS,
                    my_alloc_count);
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_ALLOC_DEPENDENT_WORDS,
                    my_dependent_count);
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_NEW_WORK,
                    new_work);
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_TOTAL_WORK,
                    (uintnat)diffmod(atomic_load(&total_work_incurred),
                                     atomic_load(&total_work_completed)));
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_BUDGET,
                    dom_st->slice_budget);
  }
}

#define Chunk_size 0x4000

typedef enum {
  Slice_uninterruptible,
  Slice_interruptible,
  Slice_opportunistic
} collection_slice_mode;

static intnat get_major_slice_sweepwork(collection_slice_mode mode){
  caml_domain_state *dom_st = Caml_state;

  if (mode == Slice_interruptible && caml_incoming_interrupts_queued())
    return 0;

  /* calculate how much work remains to do for this slice */
  intnat budget =
    max2 (diffmod (dom_st->slice_target, atomic_load (&total_work_completed)),
          dom_st->slice_budget);
  return min2(budget, Chunk_size);
}

/* Register the work done by a chunk of slice.
   Clear requested_global_major_slice if the work counter has caught up with
   the slice's target counter. */
static void commit_major_slice_sweepwork(intnat words_done) {
  caml_domain_state *dom_st = Caml_state;
  dom_st->slice_budget -= words_done;
  atomic_fetch_add (&total_work_completed, words_done);
  if (diffmod (dom_st->slice_target, atomic_load (&total_work_completed)) <= 0){
    /* We've done enough work by ourselves, no need to interrupt the other
       domains. */
    dom_st->requested_global_major_slice = 0;
  }
}

static intnat get_major_slice_markwork(collection_slice_mode mode)
{
  intnat budget = get_major_slice_sweepwork(mode);
  return Markwork_sweepwork(budget);
}

static void commit_major_slice_markwork(intnat words_done)
{
  commit_major_slice_sweepwork(Sweepwork_markwork(words_done));
}

static void mark_stack_prune(struct mark_stack* stk);

#ifdef DEBUG
#define Is_markable(v) \
    (CAMLassert (v != Debug_free_major), \
     Is_block(v) && !Is_young(v))
#else
#define Is_markable(v) (Is_block(v) && !Is_young(v))
#endif

static void realloc_mark_stack (struct mark_stack* stk)
{
  mark_entry* new;
  uintnat mark_stack_large_bsize = 0;
  uintnat mark_stack_bsize = stk->size * sizeof(mark_entry);
  uintnat local_heap_bsize = caml_heap_size(Caml_state->shared_heap);

  /* When the mark stack might not increase, we count the large mark entries
     to adjust our alloaction. This is needed because large mark stack entries
     will not compress and because we are using a domain local heap bound we
     need to fit large blocks into the local mark stack. See PR#11284 */
  if (mark_stack_bsize >= local_heap_bsize / 32) {
    uintnat i;
    for (i = 0; i < stk->count; ++i) {
      mark_entry* me = &stk->stack[i];
      if (me->end - me->start > BITS_PER_WORD)
        mark_stack_large_bsize += sizeof(mark_entry);
    }
  }

  if (mark_stack_bsize - mark_stack_large_bsize < local_heap_bsize / 32) {
    uintnat target_bsize = (mark_stack_bsize - mark_stack_large_bsize) * 2
                              + mark_stack_large_bsize;
    CAML_GC_MESSAGE(MARK_STACK,
                    "Growing mark stack to %"ARCH_INTNAT_PRINTF_FORMAT"uk bytes"
                    "(large block %"ARCH_INTNAT_PRINTF_FORMAT"uk bytes)\n",
                    target_bsize / 1024, mark_stack_large_bsize / 1024);

    new = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                                target_bsize);
    if (new != NULL) {
      stk->stack = new;
      stk->size = target_bsize / sizeof(mark_entry);
      return;
    }
    CAML_GC_MESSAGE(MARK_STACK,
                    "No room for growing mark stack. Compressing..\n");
  }

  CAML_GC_MESSAGE(MARK_STACK,
                  "Mark stack size is %"ARCH_INTNAT_PRINTF_FORMAT"u "
                  "bytes (> major heap size of this domain %"
                  ARCH_INTNAT_PRINTF_FORMAT"u bytes / 32). Compressing.\n",
                  mark_stack_bsize,
                  local_heap_bsize);
  mark_stack_prune(stk);
}

Caml_inline void mark_stack_push_range(struct mark_stack* stk,
                                       value_ptr start, value_ptr end)
{
  mark_entry* me;

  if (stk->count == stk->size)
    realloc_mark_stack(stk);

  me = &stk->stack[stk->count++];
  me->start = start;
  me->end = end;
}

/* returns the work done by skipping unmarkable objects */
static intnat mark_stack_push_block(struct mark_stack* stk, value block)
{
  int i, end;
  uintnat block_scannable_wsz, offset = 0;

  if (Tag_val(block) == Closure_tag) {
    /* Skip the code pointers and integers at beginning of closure;
       start scanning at the first word of the environment part. */
    offset = Start_env_closinfo(Closinfo_val(block));

    CAMLassert(offset <= Wosize_val(block)
      && offset >= Start_env_closinfo(Closinfo_val(block)));
  }

  CAMLassert(Has_status_val(block, caml_global_heap_state.MARKED));
  CAMLassert(Is_block(block) && !Is_young(block));
  CAMLassert(Tag_val(block) != Infix_tag);
  CAMLassert(Tag_val(block) < No_scan_tag);
  CAMLassert(Tag_val(block) != Cont_tag);

  block_scannable_wsz = Scannable_wosize_val(block);

  /* Optimisation to avoid pushing small, unmarkable objects such as
     [Some 42] into the mark stack. */
  end = (block_scannable_wsz < 8 ? block_scannable_wsz : 8);

  for (i = offset; i < end; i++) {
    value v = Field(block, i);

    if (Is_markable(v))
      break;
  }

  if (i == block_scannable_wsz){
    /* nothing left to mark and credit header */
    return Whsize_wosize(block_scannable_wsz - offset);
  }

  mark_stack_push_range(stk,
                        Op_val(block) + i,
                        Op_val(block) + block_scannable_wsz);

  /* take credit for the work we skipped due to the optimisation.
     we will take credit for the header later as part of marking. */
  return i - offset;
}

/* This function shrinks the mark stack back to the MARK_STACK_INIT_SIZE size
   and is called at domain termination via caml_finish_marking. */
static void shrink_mark_stack (void)
{
  struct mark_stack* stk = Caml_state->mark_stack;
  intnat init_stack_bsize = MARK_STACK_INIT_SIZE * sizeof(mark_entry);
  mark_entry* shrunk_stack;

  CAML_GC_MESSAGE(MARK_STACK,
                  "Shrinking mark stack to %"
                  ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                  init_stack_bsize / 1024);

  shrunk_stack = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                              init_stack_bsize);
  if (shrunk_stack != NULL) {
    stk->stack = shrunk_stack;
    stk->size = MARK_STACK_INIT_SIZE;
  }else{
    CAML_GC_MESSAGE(MARK_STACK, "Mark stack shrinking failed");
  }
}

void caml_darken_cont(value cont);

static void mark_slice_darken(struct mark_stack* stk, value child,
                              intnat* work)
{
  header_t chd;

  if (Is_markable(child)){

  /* This part of the code is duplicated in do_some_marking for performance
   * reasons.
   * Changes here should probably be reflected in do_some_marking. */
  /* Annotating an acquire barrier on the header because TSan does not see the
   * happens-before relationship established by address dependencies with
   * initializing writes in shared_heap.c allocation (#12894) */
    CAML_TSAN_ANNOTATE_HAPPENS_AFTER(Hp_val(child));
    chd = Hd_val(child);
    if (Tag_hd(chd) == Infix_tag) {
      child -= Infix_offset_hd(chd);
      chd = Hd_val(child);
    }
    CAMLassert(!Has_status_hd(chd, caml_global_heap_state.GARBAGE));
    if (Has_status_hd(chd, caml_global_heap_state.UNMARKED)){
      Caml_state->stat_blocks_marked++;
      if (Tag_hd(chd) == Cont_tag){
        caml_darken_cont(child);
        *work -= Wosize_hd(chd);
      } else {
    again:
        if (Tag_hd(chd) == Lazy_tag || Tag_hd(chd) == Forcing_tag){
          if(!atomic_compare_exchange_strong(Hp_atomic_val(child), &chd,
                With_status_hd(chd, caml_global_heap_state.MARKED))){
                  chd = Hd_val(child);
                  goto again;
          }
        } else {
          atomic_store_relaxed(
            Hp_atomic_val(child),
            With_status_hd(chd, caml_global_heap_state.MARKED));
        }
        if(Tag_hd(chd) < No_scan_tag){
          *work -= mark_stack_push_block(stk, child);
        } else {
          *work -= Wosize_hd(chd);
        }
      }
    }
  }
}

static CAMLno_tsan
#if defined(WITH_THREAD_SANITIZER)
Caml_noinline
#endif
value volatile_load_uninstrumented(volatile value* p) {
  return *p;
}

Caml_noinline static intnat do_some_marking(struct mark_stack* stk,
                                            intnat budget)
{
  prefetch_buffer_t pb;
  /* Not a struct initializer, which would force clearing the .buffer */
  pb.enqueued = 0;
  pb.dequeued = 0;
  pb.waterline = PREFETCH_BUFFER_MIN;
  mark_entry me;
  /* These global values are cached in locals,
     so that they can be stored in registers */
  struct global_heap_state heap_state = caml_global_heap_state;
  uintnat blocks_marked = 0;

  while (1) {
    if (pb_above_waterline(&pb)) {
      /* Dequeue from prefetch buffer */
      value block = pb_pop(&pb);
      CAMLassert(Is_markable(block));

      /* This part of the code is a duplicate of mark_slice_darken for
       * performance reasons.
       * Changes here should probably be reflected here in mark_slice_darken.*/
      /* Annotating an acquire barrier on the header because TSan does not see
       * the happens-before relationship established by address dependencies
       * with initializing writes in shared_heap.c allocation (#12894) */
      CAML_TSAN_ANNOTATE_HAPPENS_AFTER(Hp_val(block));
      header_t hd = Hd_val(block);

      if (Tag_hd(hd) == Infix_tag) {
        block -= Infix_offset_hd(hd);
        hd = Hd_val(block);
      }

      CAMLassert(!Has_status_hd(hd, heap_state.GARBAGE));
      if (!Has_status_hd(hd, heap_state.UNMARKED)) {
        /* Already black, nothing to do */
        continue;
      }
      blocks_marked++;

      if (Tag_hd(hd) == Cont_tag) {
        caml_darken_cont(block);
        budget -= Whsize_hd(hd);
        continue;
      }

again:
      if (Tag_hd(hd) == Lazy_tag || Tag_hd(hd) == Forcing_tag) {
        if (!atomic_compare_exchange_strong(Hp_atomic_val(block), &hd,
              With_status_hd(hd, caml_global_heap_state.MARKED))) {
          hd = Hd_val(block);
          goto again;
        }
      } else {
        atomic_store_relaxed(
            Hp_atomic_val(block),
            With_status_hd(hd, caml_global_heap_state.MARKED));
      }

      budget--; /* header word */
      if (Tag_hd(hd) >= No_scan_tag) {
        /* Nothing to scan here */
        budget -= Wosize_hd(hd);
        continue;
      }

      me.start = Op_val(block);

      reserved_t reserved = Reserved_hd(hd);
      if (Is_mixed_block_reserved(reserved)) {
        uintnat scannable_wosize =
          Scannable_wosize_reserved(reserved, Wosize_hd(hd));
        me.end = me.start + scannable_wosize;
        budget -= Wosize_hd(hd) - scannable_wosize; /* unscannable suffix */
      } else {
        me.end = me.start + Wosize_hd(hd);
      }

      if (Tag_hd(hd) == Closure_tag) {
        uintnat env_offset = Start_env_closinfo(Closinfo_val(block));
        budget -= env_offset;
        me.start += env_offset;
      }
    }
    else if (budget <= 0 || stk->count == 0) {
      if (pb.waterline > 0) {
        /* Dequeue from pb even when close to empty, because
           we have nothing else to do */
        pb_drain_mode(&pb);
        continue;
      }
      else {
        /* Couldn't find work with pb in draining mode,
           so there's nothing to do */
        break;
      }
    }
    else {
      me = stk->stack[--stk->count];
    }

    value_ptr scan_end = me.end;
    if (scan_end - me.start > budget) {
      intnat scan_len = budget < 0 ? 0 : budget;
      scan_end = me.start + scan_len;
    }

    for (; me.start < scan_end; me.start++) {
      CAMLassert(budget >= 0);

      /* This load may race with a concurrent caml_modify. It does not
         constitute a data race as this is a volatile load. However, TSan will
         wrongly see a race here (see section 3.2 of comment in tsan.c). We
         therefore make sure it is never TSan-instrumented. */
      value child = volatile_load_uninstrumented(me.start);

      budget--;
      if (Is_markable(child)) {
        if (pb_full(&pb))
          break;
        prefetch_block(child);
        pb_push(&pb, child);
      }
    }

    if (me.start < me.end) {
      /* Didn't finish scanning this object, either because budget <= 0,
         or the prefetch buffer filled up. Leave the rest on the stack. */
      mark_stack_push_range(stk, me.start, me.end);
      caml_prefetch((void*)(me.start + 1));

      if (pb_size(&pb) > PREFETCH_BUFFER_MIN) {
        /* We may have just discovered more work when we were about to run out.
           Reset waterline so that we try to refill the buffer again. */
        pb_fill_mode(&pb);
      }
    }
  }

  Caml_state->stat_blocks_marked += blocks_marked;
  CAMLassert(pb_size(&pb) == 0);
  return budget;
}

/* Compressed mark stack

   We use a bitset, implemented as a hashtable storing word-sized
   integers (uintnat). Each integer represents a "chunk" of addresses
   that may or may not be present in the stack.
 */
static const uintnat chunk_mask = ~(uintnat)(BITS_PER_WORD-1);
static inline uintnat ptr_to_chunk(value_ptr ptr) {
  return ((uintnat)(ptr) / sizeof(value)) & chunk_mask;
}
static inline uintnat ptr_to_chunk_offset(value_ptr ptr) {
  return ((uintnat)(ptr) / sizeof(value)) & ~chunk_mask;
}
static inline value_ptr chunk_and_offset_to_ptr(uintnat chunk, uintnat offset) {
  return (value_ptr)((chunk + offset) * sizeof(value));
}

/* mark until the budget runs out or marking is done */
static intnat mark(intnat budget) {
  caml_domain_state *domain_state = Caml_state;
  CAMLassert(caml_marking_started());
  while (budget > 0 && !domain_state->marking_done) {
    budget = do_some_marking(domain_state->mark_stack, budget);
    if (budget > 0) {
      struct mark_stack* mstk = domain_state->mark_stack;
      addrmap_iterator it = mstk->compressed_stack_iter;
      if (caml_addrmap_iter_ok(&mstk->compressed_stack, it)) {
        uintnat chunk = caml_addrmap_iter_key(&mstk->compressed_stack, it);
        uintnat bitset = caml_addrmap_iter_value(&mstk->compressed_stack, it);

        /* NB: must update the iterator here, as possible that
           mark_slice_darken could lead to the mark stack being pruned
           and invalidation of the iterator */
        mstk->compressed_stack_iter =
                      caml_addrmap_next(&mstk->compressed_stack, it);

        for(int ofs=0; ofs<BITS_PER_WORD; ofs++) {
          if(bitset & ((uintnat)1 << ofs)) {
            value_ptr p = chunk_and_offset_to_ptr(chunk, ofs);
            mark_slice_darken(domain_state->mark_stack, *p, &budget);
          }
        }
      } else {
        ephe_next_cycle ();
        domain_state->marking_done = 1;
        (void)caml_atomic_counter_decr(&num_domains_to_mark);
      }
    }
  }
  return budget;
}

static scanning_action_flags darken_scanning_flags = 0;

void caml_darken_cont(value cont)
{
  CAMLassert(Is_block(cont) && !Is_young(cont) && Tag_val(cont) == Cont_tag);
  {
    SPIN_WAIT {
      header_t hd = atomic_load_relaxed(Hp_atomic_val(cont));
      CAMLassert(!Has_status_hd(hd, caml_global_heap_state.GARBAGE));
      if (Has_status_hd(hd, caml_global_heap_state.MARKED)) {
        /* Perform an acquire load to synchronize with the marking domain */
        hd = atomic_load_acquire(Hp_atomic_val(cont));
        if (Has_status_hd(hd, caml_global_heap_state.MARKED))
          break;
      }
      if (Has_status_hd(hd, caml_global_heap_state.UNMARKED) &&
          atomic_compare_exchange_strong(
              Hp_atomic_val(cont), &hd,
              With_status_hd(hd, NOT_MARKABLE))) {
        value stk = Field(cont, 0);
        if (Ptr_val(stk) != NULL)
          caml_scan_stack(&caml_darken, darken_scanning_flags, Caml_state,
                          Ptr_val(stk), 0);
        atomic_store_release(Hp_atomic_val(cont),
                             With_status_hd(hd, caml_global_heap_state.MARKED));
        Caml_state->mark_work_done_between_slices += Whsize_hd(hd);
      }
    }
  }
}

void caml_darken(void* state, value v, volatile value* ignored) {
  header_t hd;
  if (!Is_markable (v)) return; /* foreign stack, at least */

  CAMLassert(caml_marking_started());
  hd = Hd_val(v);
  if (Tag_hd(hd) == Infix_tag) {
    v -= Infix_offset_hd(hd);
    hd = Hd_val(v);
  }
  if (Has_status_hd(hd, caml_global_heap_state.UNMARKED)) {
    caml_domain_state* domain_state = (caml_domain_state*)state;
    if (domain_state->marking_done) {
      (void)caml_atomic_counter_incr(&num_domains_to_mark);
      domain_state->marking_done = 0;
    }
    if (Tag_hd(hd) == Cont_tag) {
      caml_darken_cont(v);
    } else {
      atomic_store_relaxed(
         Hp_atomic_val(v),
         With_status_hd(hd, caml_global_heap_state.MARKED));
      if (Tag_hd(hd) < No_scan_tag) {
        mark_stack_push_block(domain_state->mark_stack, v);
        Caml_state->mark_work_done_between_slices += 1; /* just the header */
      } else {
        Caml_state->mark_work_done_between_slices += Whsize_hd(hd);
      }
    }
  }
}

static intnat ephe_mark (intnat budget, uintnat for_cycle,
                         /* Forces ephemerons and their data to be alive */
                         int force_alive)
{
  value v, data, key, f, todo;
  value* prev_linkp;
  header_t hd;
  mlsize_t size, i;
  caml_domain_state* domain_state = Caml_state;
  int alive_data;
  uintnat examined = 0, trivial_data = 0, marked_data = 0;

  CAMLassert(caml_marking_started());
  if (domain_state->ephe_info->cursor.cycle == for_cycle &&
      !force_alive) {
    prev_linkp = domain_state->ephe_info->cursor.todop;
    todo = *prev_linkp;
  } else {
    todo = domain_state->ephe_info->todo;
    prev_linkp = &domain_state->ephe_info->todo;
  }
  while (todo != 0 && budget > 0) {
    v = todo;
    todo = Ephe_link(v);
    CAMLassert (Tag_val(v) == Abstract_tag);
    hd = Hd_val(v);
    data = Ephe_data(v);
    alive_data = 1;

    if (force_alive)
      caml_darken (domain_state, v, 0);

    /* If ephemeron is unmarked, data is dead */
    if (is_unmarked(v)) alive_data = 0;

    size = Wosize_hd(hd);
    for (i = CAML_EPHE_FIRST_KEY; alive_data && i < size; i++) {
      key = ephe_key(v, i);
    ephemeron_again:
      if (key != caml_ephe_none && Is_block(key)) {
        if (Tag_val(key) == Forward_tag) {
          f = Forward_val(key);
          if (Is_block(f)) {
            if (Tag_val(f) == Forward_tag || Tag_val(f) == Lazy_tag ||
                Tag_val(f) == Forcing_tag || Tag_val(f) == Double_tag) {
              /* Do not short-circuit the pointer */
            } else {
              Field(v, i) = key = f;
              goto ephemeron_again;
            }
          }
        }
        else {
          if (Tag_val (key) == Infix_tag) key -= Infix_offset_val (key);
          if (is_unmarked (key))
            alive_data = 0;
        }
      }
    }
    budget -= Whsize_wosize(i);

    bool keep;
    if (data == caml_ephe_none || Is_long(data)) {
      /* Not yet known whether this ephemeron's keys/block will be marked,
         but since the data is trivial nothing will happen if they are,
         so remove it from the todo list */
      ++ trivial_data;
      keep = false;
    } else if (force_alive || alive_data) {
      /* This ephemeron's keys & block are marked, so mark the data,
         and remove it from the todo list */
      caml_darken (domain_state, data, 0);
      ++ marked_data;
      keep = false;
    } else {
      /* Leave this ephemeron on the todo list */
      keep = true;
    }

    if (keep) {
      prev_linkp = &Ephe_link(v);
    } else {
      Ephe_link(v) = domain_state->ephe_info->live;
      domain_state->ephe_info->live = v;
      *prev_linkp = todo;
    }
    ++ examined;
  }

#define F_U "%"ARCH_INTNAT_PRINTF_FORMAT"u"
  CAML_GC_MESSAGE(SLICE, "Marked ephemerons: %s. Ephemeron cycle "F_U
                  " examined "F_U" trivial data "F_U" marked data "F_U"\n",
                  domain_state->ephe_info->cursor.cycle == for_cycle ?
                  "Continued from cursor" : "Discarded cursor",
                  for_cycle, examined, trivial_data, marked_data);

  domain_state->ephe_info->cursor.cycle = for_cycle;
  domain_state->ephe_info->cursor.todop = prev_linkp;

  return budget;
}

static intnat ephe_sweep (caml_domain_state* domain_state, intnat budget)
{
  value v;
  CAMLassert (caml_gc_phase == Phase_sweep_ephe);

  while (domain_state->ephe_info->todo != 0 && budget > 0) {
    v = domain_state->ephe_info->todo;
    domain_state->ephe_info->todo = Ephe_link(v);
    CAMLassert (Tag_val(v) == Abstract_tag);

    if (is_unmarked(v)) {
      /* The whole array is dead, drop this ephemeron */
    } else {
      caml_ephe_clean(v);
      Ephe_link(v) = domain_state->ephe_info->live;
      domain_state->ephe_info->live = v;
      budget -= Whsize_val(v);
    }
  }
  return budget;
}

static void request_mark_phase (void)
{
  if (caml_gc_phase == Phase_sweep_main &&
      atomic_load_relaxed(&caml_gc_mark_phase_requested) == 0)
    atomic_store_release(&caml_gc_mark_phase_requested, 1);
}

void caml_mark_roots_stw (int participant_count, caml_domain_state** barrier_participants)
{
  if (caml_gc_phase != Phase_sweep_main)
    return;

  enum global_roots_status {
    WORK_UNSTARTED,
    WORK_STARTED,
    WORK_COMPLETE
  };
  static atomic_uintnat global_roots_scanned;

  Caml_global_barrier_if_final(participant_count) {
    caml_gc_phase = Phase_sweep_and_mark_main;
    atomic_store_relaxed(&global_roots_scanned, WORK_UNSTARTED);
    /* Adopt orphaned work from domains that were spawned and
       terminated in the previous cycle. Do this in the barrier,
       before any domain can terminate on this cycle. */
    adopt_orphaned_work (caml_global_heap_state.UNMARKED);
  }

  caml_domain_state* domain = Caml_state;

  begin_ephe_marking();

  CAML_EV_BEGIN(EV_MAJOR_MARK_ROOTS);
  {
    uintnat work_unstarted = WORK_UNSTARTED;
    if (atomic_load_relaxed(&global_roots_scanned) == WORK_UNSTARTED &&
        atomic_compare_exchange_strong(&global_roots_scanned,
                                       &work_unstarted, WORK_STARTED)) {
      /* This domain did the CAS, so this domain marks the roots */
      caml_scan_global_roots(&caml_darken, domain);
      atomic_store_release(&global_roots_scanned, WORK_COMPLETE);
    }
  }
  /* Locals, C locals, systhreads & finalisers */
  caml_do_roots (&caml_darken, darken_scanning_flags, domain, domain, 0);
  CAML_EV_END(EV_MAJOR_MARK_ROOTS);

  CAML_EV_BEGIN(EV_MAJOR_MEMPROF_ROOTS);
  caml_memprof_scan_roots(caml_darken, darken_scanning_flags, domain,
                          domain, false);
  CAML_EV_END(EV_MAJOR_MEMPROF_ROOTS);

  CAML_GC_MESSAGE(MAJOR,
                  "Marking started, %ld entries on mark stack\n",
                  (long)domain->mark_stack->count);

  if (domain->ephe_info->todo == (value) NULL)
    ephe_todo_list_emptied();

  /* Wait until global roots are marked. It's fine if other domains are still
     marking their local roots, as long as the globals are done */
  if (atomic_load_acquire(&global_roots_scanned) != WORK_COMPLETE) {
    CAML_EV_BEGIN(EV_MAJOR_MARK_OPPORTUNISTIC);
    SPIN_WAIT {
      caml_opportunistic_major_collection_slice(1000);
      if (atomic_load_acquire(&global_roots_scanned) == WORK_COMPLETE)
        break;
    }
    CAML_EV_END(EV_MAJOR_MARK_OPPORTUNISTIC);
  }
}

/* Decide, at the end of a major cycle, whether to compact. */

static bool should_compact_from_stw_single(int compaction_mode)
{
  if (compaction_mode == Compaction_none) {
    return false;
  } else if (compaction_mode == Compaction_forced) {
    CAML_GC_MESSAGE (POLICY, "Forced compaction.\n");
    return true;
  }
  CAMLassert (compaction_mode == Compaction_auto);

  /* runtime 4 algorithm, as close as possible.
   * TODO: revisit this in future. */
  if (caml_max_percent_free >= 1000 * 1000) {
    CAML_GC_MESSAGE (POLICY,
                     "Max percent free %"ARCH_INTNAT_PRINTF_FORMAT"u%%:"
                     "compaction off.\n", caml_max_percent_free);
    return false;
  }
  if (caml_major_cycles_completed < 3) {
    CAML_GC_MESSAGE (POLICY,
                     "Only %"ARCH_INTNAT_PRINTF_FORMAT"u major cycles: "
                     "compaction off.\n", caml_major_cycles_completed);
    return false;
  }

  struct gc_stats s;
  caml_compute_gc_stats(&s);

  uintnat heap_words = s.heap_stats.pool_words + s.heap_stats.large_words;

  if (Bsize_wsize(heap_words) <= 2 * caml_shared_heap_grow_bsize())
    return false;

  uintnat live_words = s.heap_stats.pool_live_words + s.heap_stats.large_words;
  uintnat free_words = heap_words - live_words;
  double current_overhead = 100.0 * free_words / live_words;

  bool compacting = current_overhead >= caml_max_percent_free;
  CAML_GC_MESSAGE (POLICY, "Current overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%% %s %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%: %scompacting.\n",
                   (uintnat) current_overhead,
                   compacting ? ">=" : "<",
                   caml_max_percent_free,
                   compacting ? "" : "not ");
  return compacting;
}

static void cycle_major_heap_from_stw_single(
  caml_domain_state* domain,
  uintnat num_domains_in_stw)
{
  /* Cycle major heap colours */
  /* FIXME: delete caml_cycle_heap_from_stw_single
     and have per-domain copies of the data? */
  caml_cycle_heap_from_stw_single();
  caml_major_cycles_completed++;
  CAML_GC_MESSAGE(MAJOR, "Starting major GC cycle "
                  "%"ARCH_INTNAT_PRINTF_FORMAT"u\n",
                  caml_major_cycles_completed);

  if (atomic_load_relaxed(&caml_verb_gc) & CAML_GC_MSG_STATS) {
    struct gc_stats s;
    intnat heap_words, not_garbage_words, swept_words;

    caml_compute_gc_stats(&s);
    heap_words = s.heap_stats.pool_words + s.heap_stats.large_words;
    not_garbage_words = s.heap_stats.pool_live_words
      + s.heap_stats.large_words;
    swept_words = domain->swept_words;
    CAML_GC_MESSAGE(SLICE,
                    "heap_words: %"ARCH_INTNAT_PRINTF_FORMAT"d "
                    "not_garbage_words %"ARCH_INTNAT_PRINTF_FORMAT"d "
                    "swept_words %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
                    heap_words, not_garbage_words, swept_words);

    static struct {
      intnat heap_words;
      intnat not_garbage_words;
    } last_cycle = {0, 0};

    if (last_cycle.heap_words != 0) {
      /* At the end of a major cycle, no object has colour MARKED.

         [not_garbage_words] counts all objects which are UNMARKED.
         Importantly, this includes both live objects and objects which are
         unreachable in the current cycle (i.e, garbage). But we don't get
         to know which objects are garbage until the end of the next cycle.

         live_words@N = not_garbage_words@N - swept_words@N+1

         space_overhead@N =
         100.0 * (heap_words@N - live_words@N) / live_words@N
      */
      double live_words = last_cycle.not_garbage_words - swept_words;
      double space_overhead = 100.0 * (double)(last_cycle.heap_words
                                               - live_words) / live_words;

      CAML_GC_MESSAGE(SLICE, "Previous cycle's space_overhead: %lf", space_overhead);
    }
    last_cycle.heap_words = heap_words;
    last_cycle.not_garbage_words = not_garbage_words;
  }

  domain->swept_words = 0;

  caml_atomic_counter_init(&num_domains_to_sweep, num_domains_in_stw);
  caml_atomic_counter_init(&num_domains_to_mark, num_domains_in_stw);

  caml_gc_phase = Phase_sweep_main;
  atomic_store(&caml_gc_mark_phase_requested, 0);
  caml_atomic_counter_init(&ephe_cycle_info.num_domains_todo, num_domains_in_stw);
  caml_atomic_counter_init(&ephe_cycle_info.ephe_cycle, 1);
  caml_atomic_counter_init(&ephe_cycle_info.num_domains_done, 0);

  caml_atomic_counter_init(&num_domains_to_ephe_sweep, 0);
  /* Will be set to the correct number when switching to
     [Phase_sweep_ephe] */

  caml_atomic_counter_init(&num_domains_to_final_update_first,
                           num_domains_in_stw);
  caml_atomic_counter_init(&num_domains_to_final_update_last,
                           num_domains_in_stw);

  caml_code_fragment_cleanup_from_stw_single();
}

struct cycle_callback_params {
  int compaction_mode;
};

static void stw_cycle_all_domains(
  caml_domain_state* domain, void* args,
  int participating_count,
  caml_domain_state** participating)
{
  /* We copy params because the stw leader may leave early. No barrier needed
     because there's one in the minor gc and after. */
  struct cycle_callback_params params = *((struct cycle_callback_params*)args);

  /* TODO: Not clear this memprof work is really part of the "cycle"
   * operation. It's more like ephemeron-cleaning really. An earlier
   * version had a separate callback for this, but resulted in
   * failures because using caml_try_run_on_all_domains() on it would
   * mysteriously put all domains back into mark/sweep.
   */
  CAML_EV_BEGIN(EV_MAJOR_MEMPROF_CLEAN);
  caml_memprof_after_major_gc(domain);
  CAML_EV_END(EV_MAJOR_MEMPROF_CLEAN);

  CAML_EV_BEGIN(EV_MAJOR_GC_CYCLE_DOMAINS);

  CAMLassert(domain == Caml_state);
  CAMLassert(atomic_load_acquire(&ephe_cycle_info.num_domains_todo) ==
             atomic_load_acquire(&ephe_cycle_info.num_domains_done));
  CAMLassert(atomic_load(&num_domains_to_mark) == 0);
  CAMLassert(atomic_load(&num_domains_to_sweep) == 0);
  CAMLassert(atomic_load(&num_domains_to_ephe_sweep) == 0);

  caml_empty_minor_heap_no_major_slice_from_stw
                        (domain, (void*)0, participating_count, participating);

  CAML_EV_BEGIN(EV_MAJOR_GC_STW);
  static bool compacting = false;
  Caml_global_barrier_if_final(participating_count) {
    cycle_major_heap_from_stw_single(domain, (uintnat) participating_count);
    /* Do compaction decision for all domains here */
    compacting = should_compact_from_stw_single(params.compaction_mode);
  }

  /* If the heap is to be verified, do it before the domains continue
     running OCaml code. */
  if (caml_params->verify_heap) {
    caml_verify_heap_from_stw(domain);
    CAML_GC_MESSAGE(MAJOR, "Heap verified\n");
    /* This global barrier avoids races between the verify_heap code
       and the rest of the STW critical section, for example the parts
       that mark global roots. */
    caml_global_barrier(participating_count);
  }

  caml_cycle_heap(domain->shared_heap);

  if (compacting) {
    caml_compact_heap(domain, participating_count, participating);
  }

  /* Update GC stats (these could have significantly changed e.g. due
   * to compaction). */
  caml_collect_gc_stats_sample_stw(domain);

  /* Collect domain-local stats to emit to runtime events */
  struct heap_stats local_stats;
  caml_collect_heap_stats_sample(Caml_state->shared_heap, &local_stats);

  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_POOL_WORDS,
                  (uintnat)local_stats.pool_words);
  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_POOL_LIVE_WORDS,
                  (uintnat)local_stats.pool_live_words);
  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_LARGE_WORDS,
                  (uintnat)local_stats.large_words);
  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_POOL_FRAG_WORDS,
                  (uintnat)(local_stats.pool_frag_words));
  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS,
                  (uintnat)local_stats.pool_live_blocks);
  CAML_EV_COUNTER(EV_C_MAJOR_HEAP_LARGE_BLOCKS,
                  (uintnat)local_stats.large_blocks);

  domain->sweeping_done = 0;
  domain->marking_done = 0;

  /* Finalisers */
  domain->final_info->updated_first = 0;
  domain->final_info->updated_last = 0;

  /* To ensure a mutator doesn't resume while global roots are being marked.
     Mutators can alter the set of global roots, to preserve its correctness,
     they should not run while global roots are being marked.*/
  caml_global_barrier(participating_count);

  /* Someone should flush the allocation stats we gathered during the cycle */
  if( participating[0] == domain ) {
    CAML_EV_ALLOC_FLUSH();
  }

  CAML_EV_END(EV_MAJOR_GC_STW);
  CAML_EV_END(EV_MAJOR_GC_CYCLE_DOMAINS);
}

static int is_complete_phase_sweep_and_mark_main (void)
{
  return
    /* Marking is done */
    caml_gc_phase == Phase_sweep_and_mark_main &&
    atomic_load_acquire (&num_domains_to_sweep) == 0 &&
    atomic_load_acquire (&num_domains_to_mark) == 0 &&

    /* No domains are orphaning finalisers. */
    atomic_load_acquire (&num_domains_orphaning_finalisers) == 0 &&

    /* Ephemeron marking is done */
    atomic_load_acquire(&ephe_cycle_info.num_domains_todo) ==
    atomic_load_acquire(&ephe_cycle_info.num_domains_done) &&

    /* All orphaned ephemerons have been adopted */
    no_orphaned_work();
}

static int is_complete_phase_mark_final (void)
{
  return
    /* updated finalise first values */
    caml_gc_phase == Phase_mark_final &&
    atomic_load_acquire (&num_domains_to_final_update_first) == 0 &&

    /* Marking is done */
    atomic_load_acquire (&num_domains_to_mark) == 0 &&

    /* Ephemeron marking is done */
    atomic_load_acquire(&ephe_cycle_info.num_domains_todo) ==
    atomic_load_acquire(&ephe_cycle_info.num_domains_done) &&

    /* All orphaned ephemerons have been adopted */
    no_orphaned_work();
}

static int is_complete_phase_sweep_ephe (void)
{
  return
    /* All domains have swept their ephemerons */
    caml_gc_phase == Phase_sweep_ephe &&
    atomic_load_acquire (&num_domains_to_ephe_sweep) == 0 &&

    /* All domains have updated finalise last values */
    atomic_load_acquire (&num_domains_to_final_update_last) == 0 &&

    /* All orphaned structures have been adopted */
    no_orphaned_work();
}

static void stw_try_complete_gc_phase(
  caml_domain_state* domain, void* unused,
  int participant_count,
  caml_domain_state** participating)
{
  CAML_EV_BEGIN(EV_MAJOR_GC_PHASE_CHANGE);

  Caml_global_barrier_if_final(participant_count) {
    if (is_complete_phase_sweep_and_mark_main()) {
      caml_gc_phase = Phase_mark_final;
    } else if (is_complete_phase_mark_final()) {
      caml_gc_phase = Phase_sweep_ephe;
      caml_atomic_counter_init(&num_domains_to_ephe_sweep, participant_count);
      for (int i = 0; i < participant_count; i++)
        participating[i]->ephe_info->must_sweep_ephe = 1;
    }
  }

  CAML_EV_END(EV_MAJOR_GC_PHASE_CHANGE);
}

intnat caml_opportunistic_major_work_available (caml_domain_state* domain_state)
{
  return
    !domain_state->sweeping_done ||
    (caml_marking_started() && !domain_state->marking_done);
}

static char collection_slice_mode_char(collection_slice_mode mode)
{
  switch(mode) {
    case Slice_uninterruptible:
      return 'u';
    case Slice_interruptible:
      return 'i';
    case Slice_opportunistic:
      return 'o';
    default:
      return ' ';
  }
}

static void major_collection_slice(intnat howmuch,
                                   int participant_count,
                                   caml_domain_state** barrier_participants,
                                   collection_slice_mode mode,
                                   int compaction_mode)
{
  caml_domain_state* domain_state = Caml_state;
  uintnat sweep_work=0, mark_work=0, ephe_sweep_work=0, ephe_mark_work=0;
  uintnat blocks_marked_before = domain_state->stat_blocks_marked;
  uintnat saved_ephe_cycle;
  uintnat saved_major_cycle = caml_major_cycles_completed;
  intnat budget;

  /* Opportunistic slices may run concurrently with gc phase updates. */
  int may_access_gc_phase = (mode != Slice_opportunistic);

  CAML_GC_MESSAGE(SLICE, "Major slice start [%c%c%c]\n",
                  collection_slice_mode_char(mode),
                  !caml_incoming_interrupts_queued() ? '.' : '*',
                  caml_gc_phase_char(may_access_gc_phase));

  bool log_events = mode != Slice_opportunistic ||
                    (atomic_load_relaxed(&caml_verb_gc) &
                     CAML_GC_MSG_SLICE);

  update_major_slice_work(howmuch, may_access_gc_phase, log_events);

  /* When a full slice of major GC work is done,
     or the slice is interrupted (in mode Slice_interruptible),
     get_major_slice_work(mode) will return a budget <= 0 */

  /* shortcut out if there is no opportunistic work to be done
   * NB: needed particularly to avoid caml_ev spam when polling */
  if (mode == Slice_opportunistic &&
      !caml_opportunistic_major_work_available(domain_state)) {
    commit_major_slice_sweepwork (0);
    return;
  }

  if (log_events) CAML_EV_BEGIN(EV_MAJOR_SLICE);
  call_timing_hook(&caml_major_slice_begin_hook);

  if (!domain_state->sweeping_done) {
    if (log_events) CAML_EV_BEGIN(EV_MAJOR_SWEEP);

    while (!domain_state->sweeping_done &&
           (budget = get_major_slice_sweepwork(mode)) > 0) {
      intnat left = caml_sweep(domain_state->shared_heap, budget);
      intnat work_done = budget - left;

      sweep_work += work_done;
      commit_major_slice_sweepwork (work_done);
      if (work_done == 0) {
        domain_state->sweeping_done = 1;
        (void)caml_atomic_counter_decr(&num_domains_to_sweep);
      }
    }

    if (log_events) CAML_EV_END(EV_MAJOR_SWEEP);
  }

  if (domain_state->sweeping_done) {
    /* We do not immediately trigger a minor GC, but instead wait for
       the next one to happen normally, when marking will start. This
       gives some chance that other domains will finish sweeping as
       well. */
    request_mark_phase();
    /* If there was no sweeping to do, but marking hasn't started,
       then minor GC has not occurred naturally between major slices -
       so we should force one now. */
    if (sweep_work == 0 && !caml_marking_started()) {
        caml_request_minor_gc();
    }
  }

mark_again:
  if (caml_marking_started() &&
      !domain_state->marking_done &&
      get_major_slice_markwork(mode) > 0) {
    if (log_events) CAML_EV_BEGIN(EV_MAJOR_MARK);

    while (!domain_state->marking_done &&
           (budget = get_major_slice_markwork(mode)) > 0) {
      intnat left = mark(budget);
      intnat work_done = budget - left;
      /* It is possible to call caml_darken directly during marking,
         if we e.g. discover a continuation and mark its stack.
         This work should count towards this slice */
      work_done += mark_work_done_between_slices();

      mark_work += work_done;
      commit_major_slice_markwork(work_done);
    }

    if (log_events) CAML_EV_END(EV_MAJOR_MARK);
  }

  if (mode != Slice_opportunistic && caml_marking_started()) {
    /* Finalisers */
    if (caml_gc_phase == Phase_mark_final &&
        get_major_slice_markwork(mode) > 0 &&
        caml_final_update_first(domain_state)) {
      /* This domain has updated finalise first values */
      (void)caml_atomic_counter_decr(&num_domains_to_final_update_first);
      if (!domain_state->marking_done &&
          get_major_slice_markwork(mode) > 0)
        goto mark_again;
    }

    /* TODO measure and account for the work of updating finalisers */
    if (caml_gc_phase == Phase_sweep_ephe &&
        get_major_slice_markwork(mode) > 0 &&
        caml_final_update_last(domain_state)) {
      /* This domain has updated finalise last values */
      (void)caml_atomic_counter_decr(&num_domains_to_final_update_last);
      /* Nothing has been marked while updating last */
    }

    adopt_orphaned_work(caml_global_heap_state.MARKED);

    /* Ephemerons */
    if (caml_gc_phase != Phase_sweep_ephe) {
      /* Ephemeron Marking
         This work is accounted as marking work */
      saved_ephe_cycle = atomic_load_acquire(&ephe_cycle_info.ephe_cycle);
      if (domain_state->ephe_info->todo != (value) NULL &&
          saved_ephe_cycle > domain_state->ephe_info->cycle &&
          get_major_slice_markwork(mode) > 0) {
        CAML_EV_BEGIN(EV_MAJOR_EPHE_MARK);

        int ephe_completed_marking = 0;
        while (domain_state->ephe_info->todo != (value) NULL &&
               saved_ephe_cycle > domain_state->ephe_info->cycle &&
               (budget = get_major_slice_markwork(mode)) > 0) {
          intnat left = ephe_mark(budget, saved_ephe_cycle, EPHE_MARK_DEFAULT);
          intnat work_done = budget - left;
          /* caml_darken is called by ephe_mark, so count the work it does */
          work_done += mark_work_done_between_slices();
          ephe_mark_work += work_done;
          commit_major_slice_markwork(work_done);

          // FIXME: Can we delete this?
          if (left > 0) {
            ephe_completed_marking = 1;
            break;
          }
        }

        CAML_EV_END(EV_MAJOR_EPHE_MARK);

        if (domain_state->ephe_info->todo == (value)NULL) {
          ephe_todo_list_emptied ();
        }

        if (ephe_completed_marking) {
          if (!domain_state->marking_done)
            goto mark_again;
          else
            record_ephe_marking_done(saved_ephe_cycle);
        }
      }
    }

    if (caml_gc_phase == Phase_sweep_ephe) {
      /* Ephemeron Sweeping
         This work is accounted as sweeping work */

      if (domain_state->ephe_info->must_sweep_ephe) {
        /* Move the ephemerons on the live list to the todo list. This is
           needed since the live list may contain ephemerons with unmarked
           keys, which need to be cleaned. This code is executed exactly once
           per major cycle per domain. */
        domain_state->ephe_info->must_sweep_ephe = 0;

        value e = ephe_list_tail (domain_state->ephe_info->todo);
        if (e == (value)NULL) {
          domain_state->ephe_info->todo = domain_state->ephe_info->live;
        } else {
          CAMLassert(Ephe_link(e) == (value)NULL);
          Ephe_link(e) = domain_state->ephe_info->live;
        }
        domain_state->ephe_info->live = (value)NULL;

        /* If the todo list is empty, then the ephemeron has no sweeping work
         * to do. */
        if (domain_state->ephe_info->todo == 0) {
          (void)caml_atomic_counter_decr(&num_domains_to_ephe_sweep);
        }
      }

      if (domain_state->ephe_info->todo != 0) {
        CAMLassert (domain_state->ephe_info->must_sweep_ephe == 0);
        /* Sweep the ephemeron todo list */
        CAML_EV_BEGIN(EV_MAJOR_EPHE_SWEEP);

        while (domain_state->ephe_info->todo != 0 &&
               (budget = get_major_slice_sweepwork(mode)) > 0) {
          intnat left = ephe_sweep (domain_state, budget);
          intnat work_done = budget - left;
          ephe_sweep_work += work_done;
          commit_major_slice_sweepwork(work_done);
        }

        CAML_EV_END(EV_MAJOR_EPHE_SWEEP);
        if (domain_state->ephe_info->todo == 0) {
          (void)caml_atomic_counter_decr(&num_domains_to_ephe_sweep);
        }
      }
    }

    /* Complete GC phase */
    if (is_complete_phase_sweep_and_mark_main() ||
        is_complete_phase_mark_final ()) {
      CAMLassert (caml_gc_phase != Phase_sweep_ephe);
      if (barrier_participants) {
        stw_try_complete_gc_phase(
          domain_state,
          (void*)0,
          participant_count,
          barrier_participants);
      } else {
        caml_try_run_on_all_domains (&stw_try_complete_gc_phase, 0, 0);
      }
      if (get_major_slice_sweepwork(mode) > 0) goto mark_again;
    }
  }

  call_timing_hook(&caml_major_slice_end_hook);
  if (log_events) {
    CAML_EV_END(EV_MAJOR_SLICE);
    CAML_EV_COUNTER(EV_C_MAJOR_SLICE_WORK_DONE,
                    sweep_work + mark_work + ephe_mark_work + ephe_sweep_work);
  }

#define F_U "%"ARCH_INTNAT_PRINTF_FORMAT"u"

  CAML_GC_MESSAGE(SLICE,
                  "Major slice completed [%c%c%c]: "F_U " sweep, "F_U" mark ("F_U" blocks), "
                  F_U" ephe mark, "F_U" ephe sweep\n",
                  collection_slice_mode_char(mode),
                  !caml_incoming_interrupts_queued() ? '.' : '*',
                  caml_gc_phase_char(may_access_gc_phase),
                  sweep_work, mark_work,
                  domain_state->stat_blocks_marked - blocks_marked_before,
                  ephe_mark_work, ephe_sweep_work);

  domain_state->stat_major_work_done +=
    sweep_work + mark_work + ephe_mark_work + ephe_sweep_work;

  if (mode != Slice_opportunistic && is_complete_phase_sweep_ephe()) {
    /* To handle the case where multiple domains try to finish the major cycle
       simultaneously, we loop until the current cycle has ended, ignoring
       whether [caml_try_run_on_all_domains] succeeds. */
    saved_major_cycle = caml_major_cycles_completed;

    struct cycle_callback_params params;
    params.compaction_mode = compaction_mode;

    while (saved_major_cycle == caml_major_cycles_completed) {
      if (barrier_participants) {
        stw_cycle_all_domains
              (domain_state, (void*)&params,
                participant_count, barrier_participants);
      } else {
        caml_try_run_on_all_domains
              (&stw_cycle_all_domains, (void*)&params, 0);
      }
    }
  }
}

void caml_opportunistic_major_collection_slice(intnat howmuch)
{
  major_collection_slice(howmuch, 0, 0, Slice_opportunistic, Compaction_none);
}

void caml_major_collection_slice(intnat howmuch)
{
  uintnat major_slice_epoch = atomic_load (&caml_major_slice_epoch);

  /* if this is an auto-triggered GC slice, make it interruptible */
  if (howmuch == AUTO_TRIGGERED_MAJOR_SLICE) {
    major_collection_slice(
        AUTO_TRIGGERED_MAJOR_SLICE,
        0,
        0,
        Slice_interruptible,
        Compaction_auto
        );
    if (caml_incoming_interrupts_queued()) {
      CAML_GC_MESSAGE(SLICE, "Major slice interrupted, rescheduling major slice\n");
      caml_request_major_slice(0);
    }
  } else {
    /* TODO: could make forced API slices interruptible, but would need to do
       accounting or pass up interrupt */
    major_collection_slice(howmuch, 0, 0, Slice_uninterruptible, Compaction_auto);
  }
  /* Record that this domain has completed a major slice for this minor cycle.
   */
  Caml_state->major_slice_epoch = major_slice_epoch;
}

struct finish_major_cycle_params {
  uintnat saved_major_cycles;
  int compaction_mode;
};

static void stw_finish_major_cycle (caml_domain_state* domain, void* arg,
                                         int participating_count,
                                         caml_domain_state** participating)
{
  /* We must copy params because the leader may exit this
    before other domains do. There is at least one barrier somewhere
    in the major cycle ending, so we don't need one immediately
    after this. */
  struct finish_major_cycle_params params =
      *((struct finish_major_cycle_params*)arg);

  CAMLassert (domain == Caml_state);

  /* We are in a STW critical section here. There is no obvious call
     to a barrier at the end of the callback, but the [while] loop
     will only terminate when [caml_major_cycles_completed] is
     incremented, and this happens in [cycle_all_domains] inside
     a barrier. */
  request_mark_phase();
  caml_empty_minor_heap_no_major_slice_from_stw
    (domain, (void*)0, participating_count, participating);
  CAMLassert (caml_marking_started());

  CAML_EV_BEGIN(EV_MAJOR_FINISH_CYCLE);
  while (params.saved_major_cycles == caml_major_cycles_completed) {
    major_collection_slice(10000000, participating_count, participating,
                           Slice_uninterruptible, params.compaction_mode);
  }
  CAML_EV_END(EV_MAJOR_FINISH_CYCLE);
}

void caml_finish_major_cycle (int compaction_mode)
{
  uintnat saved_major_cycles = caml_major_cycles_completed;

  while( saved_major_cycles == caml_major_cycles_completed ) {
    struct finish_major_cycle_params params;
    params.compaction_mode = compaction_mode;
    params.saved_major_cycles = caml_major_cycles_completed;

    caml_try_run_on_all_domains(&stw_finish_major_cycle, (void*)&params, 0);
  }
}

#ifdef DEBUG
int caml_mark_stack_is_empty(void)
{
  return Caml_state->mark_stack->count == 0;
}
#endif

static void empty_mark_stack (void)
{
  while (!Caml_state->marking_done){
    /* while, not if: it is possible for caml_empty_minor_heaps_once
       to actually do a full major GC cycle, and end up returning with
       caml_marking_started false, because the next cycle has started */
    while (!caml_marking_started()) {
      request_mark_phase();
      /* This calls caml_mark_roots_stw with the minor heap empty */
      caml_empty_minor_heaps_once();
    }
    mark(1000);
    caml_handle_incoming_interrupts();
  }

  if (Caml_state->stat_blocks_marked)
    CAML_GC_MESSAGE(MAJOR, "Emptied mark stack. Marked %u blocks\n",
                (unsigned)Caml_state->stat_blocks_marked);
  Caml_state->stat_blocks_marked = 0;
}

void caml_finish_marking (void)
{
  if (!Caml_state->marking_done) {
    CAML_EV_BEGIN(EV_MAJOR_FINISH_MARKING);
    empty_mark_stack();
    shrink_mark_stack();
    Caml_state->stat_major_words += Caml_state->allocated_words;
    Caml_state->stat_major_dependent_bytes +=
      Caml_state->allocated_dependent_bytes;
    Caml_state->allocated_words = 0;
    Caml_state->allocated_words_direct = 0;
    Caml_state->allocated_dependent_bytes = 0;
    CAMLassert(Caml_state->marking_done);
    CAML_EV_END(EV_MAJOR_FINISH_MARKING);
  }
}

void caml_finish_sweeping (void)
{
  if (Caml_state->sweeping_done) return;
  CAML_EV_BEGIN(EV_MAJOR_FINISH_SWEEPING);
  while (!Caml_state->sweeping_done) {
    if (caml_sweep(Caml_state->shared_heap, 10) > 0) {
      /* just finished sweeping */
      CAMLassert(Caml_state->sweeping_done == 0);
      Caml_state->sweeping_done = 1;
      (void)caml_atomic_counter_decr(&num_domains_to_sweep);
      break;
    }
    caml_handle_incoming_interrupts();
  }
  CAML_EV_END(EV_MAJOR_FINISH_SWEEPING);
}

Caml_inline int add_addr(struct addrmap* amap, value_ptr ptr) {
  uintnat chunk = ptr_to_chunk(ptr);
  uintnat offset = ptr_to_chunk_offset(ptr);
  uintnat flag = (uintnat)1 << offset;
  int new_entry = 0;

  value* amap_pos = caml_addrmap_insert_pos(amap, chunk);

  if (*amap_pos == ADDRMAP_NOT_PRESENT) {
    new_entry = 1;
    *amap_pos = 0;
  }

  CAMLassert(ptr == chunk_and_offset_to_ptr(chunk, offset));

  if (!(*amap_pos & flag)) {
    *amap_pos |= flag;
  }

  return new_entry;
}

static void mark_stack_prune(struct mark_stack* stk)
{
  /* Since addrmap is (currently) using open address hashing, we cannot insert
     new compressed stack entries into an existing, partially-processed
     compressed stack. Thus, we create a new compressed stack and insert the
     unprocessed entries of the existing compressed stack into the new one. */
  uintnat old_compressed_entries = 0;
  struct addrmap new_compressed_stack = ADDRMAP_INIT;
  addrmap_iterator it;
  for (it = stk->compressed_stack_iter;
       caml_addrmap_iter_ok(&stk->compressed_stack, it);
       it = caml_addrmap_next(&stk->compressed_stack, it)) {
    value k = caml_addrmap_iter_key(&stk->compressed_stack, it);
    value v = caml_addrmap_iter_value(&stk->compressed_stack, it);
    caml_addrmap_insert(&new_compressed_stack, k, v);
    ++old_compressed_entries;
  }
  if (old_compressed_entries > 0) {
    CAML_GC_MESSAGE(MARK_STACK,
                    "Preserved %"ARCH_INTNAT_PRINTF_FORMAT "d compressed entries\n",
                    old_compressed_entries);
  }
  caml_addrmap_clear(&stk->compressed_stack);
  stk->compressed_stack = new_compressed_stack;

  /* scan mark stack and compress entries */
  uintnat i, new_stk_count = 0, compressed_entries = 0, total_words = 0;
  for (i=0; i < stk->count; i++) {
    mark_entry me = stk->stack[i];
    total_words += me.end - me.start;
    if (me.end - me.start > BITS_PER_WORD) {
      /* keep entry in the stack as more efficient and move to front */
      stk->stack[new_stk_count++] = me;
    } else {
      while(me.start < me.end) {
        compressed_entries += add_addr(&stk->compressed_stack,
                                       me.start);
        me.start++;
      }
    }
  }

  CAML_GC_MESSAGE(MARK_STACK,
                  "Compressed %"ARCH_INTNAT_PRINTF_FORMAT "d mark stack words into "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "d mark stack entries and "
                  "%"ARCH_INTNAT_PRINTF_FORMAT "d compressed entries\n",
                  total_words, new_stk_count,
                  compressed_entries+old_compressed_entries);

  stk->count = new_stk_count;
  CAMLassert(stk->count < stk->size);

  /* setup the compressed stack iterator */
  stk->compressed_stack_iter = caml_addrmap_iterator(&stk->compressed_stack);
}

int caml_init_major_gc(caml_domain_state* d) {
  d->mark_stack = caml_stat_alloc_noexc(sizeof(struct mark_stack));
  if(d->mark_stack == NULL) {
    return -1;
  }
  d->mark_stack->stack =
    caml_stat_alloc_noexc(MARK_STACK_INIT_SIZE * sizeof(mark_entry));
  if(d->mark_stack->stack == NULL) {
    caml_stat_free(d->mark_stack);
    d->mark_stack = NULL;
    return -1;
  }
  d->mark_stack->count = 0;
  d->mark_stack->size = MARK_STACK_INIT_SIZE;
  caml_addrmap_init(&d->mark_stack->compressed_stack);
  d->mark_stack->compressed_stack_iter =
                  caml_addrmap_iterator(&d->mark_stack->compressed_stack);

  /* Fresh domains do not need to performing marking or sweeping. */
  if (caml_gc_phase == Phase_sweep_main) {
    d->sweeping_done = 1;
    d->marking_done = 0;
    (void)caml_atomic_counter_incr(&num_domains_to_mark);
    (void)caml_atomic_counter_incr(&ephe_cycle_info.num_domains_todo);
  } else {
    d->sweeping_done = 1;
    d->marking_done = 1;
  }

  /* Finalisers. Fresh domains participate in updating finalisers. */
  d->final_info = caml_alloc_final_info ();
  if(d->final_info == NULL) {
    caml_stat_free(d->mark_stack->stack);
    caml_stat_free(d->mark_stack);
    return -1;
  }
  d->ephe_info = caml_alloc_ephe_info();
  if(d->ephe_info == NULL) {
    caml_stat_free(d->final_info);
    caml_stat_free(d->mark_stack->stack);
    caml_stat_free(d->mark_stack);
    d->final_info = NULL;
    d->mark_stack = NULL;
    return -1;
  }
  (void)caml_atomic_counter_incr(&num_domains_to_final_update_first);
  (void)caml_atomic_counter_incr(&num_domains_to_final_update_last);

  return 0;
}

void caml_teardown_major_gc(void) {
  caml_domain_state* d = Caml_state;

/* At this point we have been removed from the STW participant set,
   so we may not access the gc phase. */
  int may_access_gc_phase = 0;

  /* Account for latest allocations, but do not write to the event ring since
     we are out of the STW participant set; the ring may be torn down
     concurrently. */
  update_major_slice_work (0, may_access_gc_phase, false);
  CAMLassert(!caml_addrmap_iter_ok(&d->mark_stack->compressed_stack,
                                   d->mark_stack->compressed_stack_iter));
  caml_addrmap_clear(&d->mark_stack->compressed_stack);
  CAMLassert(d->mark_stack->count == 0);
  caml_stat_free(d->mark_stack->stack);
  caml_stat_free(d->mark_stack);
  d->mark_stack = NULL;
}

void caml_finalise_heap (void)
{
  return;
}
