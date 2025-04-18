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

#include <limits.h>
#include <math.h>

#include "caml/compact.h"
#include "caml/custom.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/finalise.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/skiplist.h"
#include "caml/signals.h"
#include "caml/weak.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"

#ifdef _MSC_VER
Caml_inline double fmin(double a, double b) {
  return (a < b) ? a : b;
}
#endif

#define MARK_STACK_INIT_SIZE 2048

struct mark_stack {
  mark_entry* stack;
  uintnat count;
  uintnat size;
};

uintnat caml_percent_free;
static uintnat marked_words, heap_wsz_at_cycle_start;
uintnat caml_major_heap_increment;
CAMLexport char *caml_heap_start;
char *caml_gc_sweep_hp;
int caml_gc_phase;        /* always Phase_mark, Pase_clean,
                             Phase_sweep, or Phase_idle */
uintnat caml_allocated_words;
uintnat caml_dependent_size, caml_dependent_allocated;
double caml_extra_heap_resources;
uintnat caml_fl_wsz_at_phase_change = 0;

extern value caml_fl_merge;  /* Defined in freelist.c. */

/* redarken_first_chunk is the first chunk needing redarkening, if NULL no
  redarkening required */
static char *redarken_first_chunk = NULL;

static char *sweep_chunk;

/* Part of the major slice left for future slices since otherwise a
   single slice would be too big.

   In units of words so that it remains consistent across heap size growth */
static uintnat backlog_words = 0;

int caml_gc_subphase;     /* Subphase_{mark_roots,mark_main,mark_final} */

/**
   Ephemerons:
   During mark phase the list caml_ephe_list_head of ephemerons
   is iterated by different pointers that follow the invariants:
   caml_ephe_list_head ->* ephes_checked_if_pure ->* ephes_to_check ->* null
                        |                         |                  |
                       (1)                       (2)                (3)

    At the start of mark phase, (1) and (2) are empty.

    In mark phase:
      - An ephemeron in (1) have a data alive (grey/black if in the heap)
        or none (nb: new ephemerons are added in this part by weak.c)
      - An ephemeron in (2):
         - is in any state if caml_ephe_list_pure is false
         - otherwise has at least a white key or is white or its data is
           black or none.
           The third case can happen only using a set_* of weak.c
      - the ephemerons in (3) are in an unknown state and must be checked

    At the end of mark phase, (3) is empty and caml_ephe_list_pure is true.
    The ephemeron in (1) and (2) will be cleaned (white keys and data
    replaced by none or the ephemeron is removed from the list if it is white)
    in clean phase.

    In clean phase:
    caml_ephe_list_head ->*                           ephes_to_check ->* null
                         |                                            |
                        (1)                                          (3)

    In clean phase, (2) is not used, ephes_to_check is initialized at
    caml_ephe_list_head:
    - the ephemerons in (1) are clean.
    - the ephemerons in (3) should be cleaned or removed if white.

 */
int caml_ephe_list_pure;
/** The ephemerons is pure if since the start of its iteration
    no value have been darkened. */
static value *ephes_checked_if_pure;
static value *ephes_to_check;

int caml_major_window = 1;
double caml_major_ring[Max_major_window] = { 0. };
int caml_major_ring_index = 0;
double caml_major_work_credit = 0.0;
double caml_gc_clock = 0.0;

#ifdef DEBUG
static unsigned long major_gc_counter = 0;
#endif

#ifdef NAKED_POINTERS_CHECKER
int caml_naked_pointers_detected = 0;
#endif

void (*caml_major_gc_hook)(void) = NULL;

/* This function prunes the mark stack if it's about to overflow. It does so
   by building a skiplist of major heap chunks and then iterating through the
   mark stack and setting redarken_start/redarken_end on each chunk to indicate
   the range that requires redarkening. */
static void mark_stack_prune (struct mark_stack* stk)
{
  int entry;
  uintnat mark_stack_count = stk->count;
  mark_entry* mark_stack = stk->stack;

  char* heap_chunk = caml_heap_start;
  struct skiplist chunk_sklist = SKIPLIST_STATIC_INITIALIZER;

  do {
    caml_skiplist_insert(&chunk_sklist, (uintnat)heap_chunk,
                          (uintnat)(heap_chunk+Chunk_size(heap_chunk)));
    heap_chunk = Chunk_next(heap_chunk);
  } while( heap_chunk != NULL );

  for( entry = 0; entry < mark_stack_count ; entry++ ) {
    mark_entry me = mark_stack[entry];
    uintnat chunk_addr = 0, chunk_addr_below = 0;

    if( caml_skiplist_find_below(&chunk_sklist, (uintnat)me.start,
          &chunk_addr, &chunk_addr_below)
        && (uintnat)me.start < chunk_addr_below ) {
      heap_chunk_head* ch = Chunk_head(chunk_addr);
      if (ch->redarken_first.start > me.start)
        ch->redarken_first = me;

      if (ch->redarken_end < me.object_end)
        ch->redarken_end = me.object_end;

      if( redarken_first_chunk == NULL
          || redarken_first_chunk > (char*)chunk_addr ) {
        redarken_first_chunk = (char*)chunk_addr;
      }
    }
  }

  caml_skiplist_empty(&chunk_sklist);

  caml_gc_message(0x08, "Mark stack overflow.\n");

  stk->count = 0;
}

static void realloc_mark_stack (struct mark_stack* stk)
{
  mark_entry* new;
  uintnat mark_stack_bsize = stk->size * sizeof(mark_entry);

  if ( Wsize_bsize(mark_stack_bsize) < Caml_state->stat_heap_wsz / 64 ) {
    caml_gc_message (0x08, "Growing mark stack to %"
                           ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                     (intnat) mark_stack_bsize * 2 / 1024);

    new = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                                2 * mark_stack_bsize);
    if (new != NULL) {
      stk->stack = new;
      stk->size *= 2;
      return;
    }
  }

  caml_gc_message (0x08, "No room for growing mark stack. Pruning..\n");
  mark_stack_prune(stk);
}

/* This function pushes the provided mark_entry [me] onto the current mark
   stack [stk]. It first checks, if the block is small enough, whether there
   are any fields we would actually do mark work on. If so then it enqueues
   the entry. */
Caml_inline void mark_stack_push(struct mark_stack* stk, value block,
                                  uintnat offset, intnat* work)
{
  value v;
  int i, block_scannable_wsz, block_wsz, end;
  mark_entry* me;

  CAMLassert(Is_block(block) && Is_in_heap (block)
            && Is_black_val(block));
  CAMLassert(Tag_val(block) != Infix_tag);
  CAMLassert(Tag_val(block) < No_scan_tag);

  block_wsz = Wosize_val(block);
  block_scannable_wsz = Scannable_wosize_val(block);

#if defined(NO_NAKED_POINTERS) || defined(NAKED_POINTERS_CHECKER)
  if (Tag_val(block) == Closure_tag) {
    /* Skip the code pointers and integers at beginning of closure;
        start scanning at the first word of the environment part. */
  /* It might be the case that [mark_stack_push] has been called
      while we are traversing a closure block but have not enough
      budget to finish the block. In that specific case, we should not
      update [m.offset] */
    if (offset == 0)
      offset = Start_env_closinfo(Closinfo_val(block));

    CAMLassert(offset <= Wosize_val(block)
      && offset >= Start_env_closinfo(Closinfo_val(block)));
  }
#endif

  end = (block_scannable_wsz < 8 ? block_scannable_wsz : 8);

  /* Optimisation to avoid pushing small, unmarkable objects such as [Some 42]
   * into the mark stack. */
  for (i = offset; i < end; i++) {
    v = Field(block, i);

    if (Is_block(v) && !Is_young(v))
      /* found something to mark */
      break;
  }

  if (i == block_scannable_wsz) {
    /* nothing left to mark */
    if( work != NULL ) {
      /* we should take credit for it though */
      *work -= Whsize_wosize(block_scannable_wsz - offset);
    }
    return;
  }

  if( work != NULL ) {
    /* take credit for the work we skipped due to the optimisation.
       we will take credit for the header later as part of marking. */
    *work -= (i - offset);
  }

  offset = i;

  if (stk->count == stk->size)
    realloc_mark_stack(stk);

  me = &stk->stack[stk->count++];

  me->start = Op_val(block) + offset;
  me->scannable_end = Op_val(block) + block_scannable_wsz;
  me->object_end = Op_val(block) + block_wsz;
}

#if defined(NAKED_POINTERS_CHECKER) && defined(NATIVE_CODE)
static void is_naked_pointer_safe (value v, value *p);
#endif

void caml_darken (value v, value *p)
{
#ifdef NO_NAKED_POINTERS
  if (Is_block(v) && !Is_young (v)) {
#else
  if (Is_block(v) && Is_in_heap (v)) {
#endif
    header_t h = Hd_val (v);
    tag_t t = Tag_hd (h);
    if (t == Infix_tag){
      v -= Infix_offset_val(v);
      h = Hd_val (v);
      t = Tag_hd (h);
    }
    CAMLassert (!Is_blue_hd (h));
    if (Is_white_hd (h)){
      caml_ephe_list_pure = 0;
      Hd_val (v) = Blackhd_hd (h);
      marked_words += Whsize_hd (h);
      if (t < No_scan_tag){
        mark_stack_push(Caml_state->mark_stack, v, 0, NULL);
      }
    }
  }
#if defined(NAKED_POINTERS_CHECKER) && defined(NATIVE_CODE)
  else if (Is_block(v) && !Is_young(v)) {
    is_naked_pointer_safe(v, p);
  }
#endif
}

/* This function shrinks the mark stack back to the MARK_STACK_INIT_SIZE size
   and is called at the end of a GC compaction to avoid a mark stack greater
   than 1/32th of the heap. */
void caml_shrink_mark_stack (void) {
  struct mark_stack* stk = Caml_state->mark_stack;
  intnat init_stack_bsize = MARK_STACK_INIT_SIZE * sizeof(mark_entry);
  mark_entry* shrunk_stack;

  caml_gc_message (0x08, "Shrinking mark stack to %"
                  ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                  init_stack_bsize / 1024);

  shrunk_stack = (mark_entry*) caml_stat_resize_noexc ((char*) stk->stack,
                                              init_stack_bsize);
  if (shrunk_stack != NULL) {
    stk->stack = shrunk_stack;
    stk->size = MARK_STACK_INIT_SIZE;
  }else{
    caml_gc_message (0x08, "Mark stack shrinking failed");
  }
}

/* This function adds blocks in the passed heap chunk [heap_chunk] to
   the mark stack. It returns 1 when the supplied chunk has no more
   range to redarken.  It returns 0 if there are still blocks in the
   chunk that need redarkening because pushing them onto the stack
   would make it grow more than a quarter full. This is to lower the
   chance of triggering another overflow, which would be
   wasteful. Subsequent calls will continue progress.
 */
static int redarken_chunk(char* heap_chunk, struct mark_stack* stk) {
  heap_chunk_head* chunk = Chunk_head(heap_chunk);
  mark_entry me = chunk->redarken_first;
  header_t* end = (header_t*)chunk->redarken_end;
  if (chunk->redarken_end <= me.start) return 1;

  while (1) {
    header_t* hp;
    /* Skip a prefix of fields that need no marking */
    CAMLassert(me.start <= me.scannable_end &&
               (header_t*)me.scannable_end <= end);
    while (me.start < me.scannable_end &&
           (!Is_block(*me.start) || Is_young(*me.start))) {
      me.start++;
    }

    /* Push to the mark stack (if anything's left) */
    if (me.start < me.scannable_end) {
      if (stk->count < stk->size/4) {
        stk->stack[stk->count++] = me;
      } else {
        /* Only fill up a quarter of the mark stack, we can resume later
           for more if we need to */
        chunk->redarken_first = me;
        return 0;
      }
    }

    /* Find the next block that needs to be re-marked */
    hp = (header_t*)me.object_end;
    CAMLassert(hp <= end);
    while (hp < end) {
      value v = Val_hp(hp);
      if (Tag_val(v) < No_scan_tag && Is_black_val(v))
        break;
      hp = (header_t*)(Op_val(v) + Wosize_val(v));
    }
    if (hp == end)
      break;

    /* Found a block */
    me.start = Op_hp(hp);
    me.scannable_end = me.start + Scannable_wosize_hp(hp);
    me.object_end = me.start + Wosize_hp(hp);
    if (Tag_hp(hp) == Closure_tag) {
      me.start += Start_env_closinfo(Closinfo_val(Val_hp(hp)));
    }
  }

  chunk->redarken_first.start =
      (value*)(heap_chunk + Chunk_size(heap_chunk));
  chunk->redarken_first.scannable_end = chunk->redarken_first.start;
  chunk->redarken_first.object_end = chunk->redarken_first.start;
  chunk->redarken_end = (value*)heap_chunk;

  return 1;
}

static void start_cycle (void)
{
  CAMLassert (caml_gc_phase == Phase_idle);
  CAMLassert (Caml_state->mark_stack->count == 0);
  CAMLassert (redarken_first_chunk == NULL);
  caml_gc_message (0x01, "Starting new major GC cycle\n");
  marked_words = 0;
  backlog_words = 0;
  caml_darken_all_roots_start ();
  caml_gc_phase = Phase_mark;
  heap_wsz_at_cycle_start = Caml_state->stat_heap_wsz;
  caml_gc_subphase = Subphase_mark_roots;
  caml_ephe_list_pure = 1;
  ephes_checked_if_pure = &caml_ephe_list_head;
  ephes_to_check = &caml_ephe_list_head;
#ifdef DEBUG
  ++ major_gc_counter;
  caml_heap_check ();
#endif
}

static void init_sweep_phase(void)
{
  /* Phase_clean is done. */
  /* Initialise the sweep phase. */
  caml_gc_sweep_hp = caml_heap_start;
  caml_fl_init_merge ();
  caml_gc_phase = Phase_sweep;
  sweep_chunk = caml_heap_start;
  caml_gc_sweep_hp = sweep_chunk;
  caml_fl_wsz_at_phase_change = caml_fl_cur_wsz;
  if (caml_major_gc_hook) (*caml_major_gc_hook)();
}

/* auxiliary function of mark_ephe_aux */
Caml_inline void mark_ephe_darken(struct mark_stack* stk, value v, mlsize_t i,
                                       int in_ephemeron, int *slice_pointers,
                                       intnat *work)
{
  value child;
  header_t chd;

  child = Field (v, i);

#ifdef NO_NAKED_POINTERS
  if (Is_block (child) && ! Is_young (child)) {
#else
  if (Is_block (child) && Is_in_heap (child)) {
#endif
    CAML_EVENTLOG_DO (++ *slice_pointers);
    chd = Hd_val (child);
    if (Tag_hd (chd) == Forward_tag){
      value f = Forward_val (child);
      if ((in_ephemeron && Is_long(f)) ||
          (Is_block (f)
           && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
               || Tag_val (f) == Lazy_tag || Tag_val (f) == Forcing_tag
#ifdef FLAT_FLOAT_ARRAY
               || Tag_val (f) == Double_tag
#endif
               ))){
        /* Do not short-circuit the pointer. */
      }else{
        /* The variable child is not changed because it must be mark alive */
        Field (v, i) = f;
        if (Is_block (f) && Is_young (f) && !Is_young (child)){
          if(in_ephemeron) {
            add_to_ephe_ref_table (Caml_state->ephe_ref_table, v, i);
          } else {
            add_to_ref_table (Caml_state->ref_table, &Field (v, i));
          }
        }
      }
    }
    else if (Tag_hd(chd) == Infix_tag) {
      child -= Infix_offset_val(child);
      chd = Hd_val(child);
    }
    if (Is_white_hd (chd)){
      caml_ephe_list_pure = 0;
      Hd_val (child) = Blackhd_hd (chd);
      if( Tag_hd(chd) < No_scan_tag ) {
        mark_stack_push(stk, child, 0, work);
      } else {
        *work -= Whsize_hd (chd);
      }
    }
  }
#if defined(NAKED_POINTERS_CHECKER) && defined(NATIVE_CODE)
  else if (Is_block(child) && ! Is_young(child)) {
    is_naked_pointer_safe(child, &Field (v, i));
  }
#endif
}

static void mark_ephe_aux (struct mark_stack *stk, intnat *work,
                             int *slice_pointers)
{
  value v, data, key;
  header_t hd;
  mlsize_t size, i;

  v = *ephes_to_check;
  hd = Hd_val(v);
  CAMLassert(Tag_val (v) == Abstract_tag);
  data = Field(v,CAML_EPHE_DATA_OFFSET);
  if ( data != caml_ephe_none &&
       Is_block (data) &&
#ifdef NO_NAKED_POINTERS
       !Is_young(data) &&
#else
       Is_in_heap (data) &&
#endif
       Is_white_val (data)){

    int alive_data = 1;

    /* The liveness of the ephemeron is one of the condition */
    if (Is_white_hd (hd)) alive_data = 0;

    /* The liveness of the keys not caml_ephe_none is the other condition */
    size = Wosize_hd (hd);
    for (i = CAML_EPHE_FIRST_KEY; alive_data && i < size; i++){
      key = Field (v, i);
    ephemeron_again:
      if (key != caml_ephe_none &&
          Is_block (key) &&
#ifdef NO_NAKED_POINTERS
          !Is_young(key)
#else
          Is_in_heap(key)
#endif
          ){
        if (Tag_val (key) == Forward_tag){
          value f = Forward_val (key);
          if (Is_long (f) ||
              (Is_block (f) &&
               (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
                || Tag_val (f) == Lazy_tag || Tag_val (f) == Forcing_tag
#ifdef FLAT_FLOAT_ARRAY
                || Tag_val (f) == Double_tag
#endif
                ))){
            /* Do not short-circuit the pointer. */
          }else{
            Field (v, i) = key = f;
            goto ephemeron_again;
          }
        }
        if (Is_white_val (key)){
          alive_data = 0;
        }
      }
    }
    *work -= Whsize_wosize(i);

    if (alive_data){
      mark_ephe_darken(stk, v, CAML_EPHE_DATA_OFFSET, /*in_ephemeron=*/1,
                          slice_pointers, work);
    } else { /* not triggered move to the next one */
      ephes_to_check = &Field(v,CAML_EPHE_LINK_OFFSET);
      return;
    }
  } else {  /* a similarly weak pointer or an already alive data */
    *work -= 1;
  }

  /* all keys black or data none or black
     move the ephemerons from (3) to the end of (1) */
  if ( ephes_checked_if_pure == ephes_to_check ) {
    /* corner case and optim */
    ephes_checked_if_pure = &Field(v,CAML_EPHE_LINK_OFFSET);
    ephes_to_check = ephes_checked_if_pure;
  } else {
    /*  - remove v from the list (3) */
    *ephes_to_check = Field(v,CAML_EPHE_LINK_OFFSET);
    /*  - insert it at the end of (1) */
    Field(v,CAML_EPHE_LINK_OFFSET) = *ephes_checked_if_pure;
    *ephes_checked_if_pure = v;
    ephes_checked_if_pure = &Field(v,CAML_EPHE_LINK_OFFSET);
  }
}


#define Pb_size (1 << 8)
#define Pb_min 64
#define Pb_mask (Pb_size - 1)

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
  caml_prefetch(Hp_val(v));
  caml_prefetch(&Field(v, 3));
}

Caml_noinline static intnat do_some_marking
#ifndef CAML_INSTR
  (intnat work)
#else
  (intnat work, int* pslice_fields, int* pslice_pointers)
#endif
{
  uintnat pb_enqueued = 0, pb_dequeued = 0;
  int darkened_anything = 0;
  value pb[Pb_size];
  uintnat min_pb = Pb_min; /* keep pb at least this full */
  /* These global values are cached in locals,
     so that they can be stored in registers */
  struct mark_stack stk = *Caml_state->mark_stack;
#define Is_block_and_not_young(v) Is_block(v) && !Is_young(v)
#ifdef NO_NAKED_POINTERS
  #define Is_major_block(v) Is_block_and_not_young(v)
#else
  #define Is_major_block(v) (Is_block_and_not_young(v) && Is_in_heap(v))
#endif

#ifdef CAML_INSTR
  int slice_fields = 0, slice_pointers = 0;
#endif

  while (1) {
    /* * [scan] is initialized to the point where we should start scanning in
         the object. It is then updated to keep track of the actual scanning
         progress.
       * [obj_scannable_end] is the point up until which it is legal to scan the
         object.
       * [obj_end] is a pointer to one past the end of the object.
       * [scan_end] is where the scanning actually will progress until. It is
         less than [obj_scannable_end] in the event that the work budget is
         lower than what would be required to scan until that point.
     */
    value *scan, *obj_scannable_end, *obj_end, *scan_end;
    intnat scan_len;

    if (pb_enqueued > pb_dequeued + min_pb) {
      /* Dequeue from prefetch buffer */
      value block = pb[(pb_dequeued++) & Pb_mask];
      header_t hd = Hd_val(block);

      if (Tag_hd(hd) == Infix_tag) {
        block -= Infix_offset_val(block);
        hd = Hd_val(block);
      }

      CAMLassert(Is_white_hd(hd) || Is_black_hd(hd));
      if (!Is_white_hd (hd)) {
        /* Already black, nothing to do */
        continue;
      }
      hd = Blackhd_hd (hd);
      Hd_val (block) = hd;
      darkened_anything = 1;
      work--; /* header word */
      if (Tag_hd (hd) >= No_scan_tag) {
        /* Nothing to scan here */
        work -= Wosize_hd (hd);
        continue;
      }
      scan = Op_val(block);
      obj_scannable_end = scan + Scannable_wosize_hd(hd);
      obj_end = scan + Wosize_hd(hd);
      work -= obj_end - obj_scannable_end;

      if (Tag_hd (hd) == Closure_tag) {
        uintnat env_offset = Start_env_closinfo(Closinfo_val(block));
        work -= env_offset;
        scan += env_offset;
      }
    } else if (work <= 0 || stk.count == 0) {
      if (min_pb > 0) {
        /* Dequeue from pb even when close to empty, because
           we have nothing else to do */
        min_pb = 0;
        continue;
      } else {
        /* Couldn't find work with min_pb == 0, so there's nothing to do */
        break;
      }
    } else {
      mark_entry m = stk.stack[--stk.count];
      scan = m.start;
      obj_scannable_end = m.scannable_end;
      obj_end = m.object_end;
    }

    scan_len = obj_scannable_end - scan;
    if (work < scan_len) {
      scan_len = work;
      if (scan_len < 0) scan_len = 0;
    }
    work -= scan_len;
    scan_end = scan + scan_len;

    for (; scan < scan_end; scan++) {
      value v = *scan;
#ifdef CAML_INSTR
      slice_fields ++;
#endif
      if (Is_major_block(v)) {
#ifdef CAML_INSTR
        slice_pointers ++;
#endif
        if (pb_enqueued == pb_dequeued + Pb_size) {
          /* Prefetch buffer is full */
          work += scan_end - scan; /* scanning work not done */
          break;
        }
        prefetch_block(v);
        pb[(pb_enqueued++) & Pb_mask] = v;
      }
#if defined(NAKED_POINTERS_CHECKER) && defined(NATIVE_CODE)
      else if (Is_block_and_not_young (v) && !Is_in_heap (v)){
        is_naked_pointer_safe (v, scan);
      }
#endif
    }

    if (scan < obj_scannable_end) {
      /* Didn't finish scanning this object, either because work <= 0,
         or the prefetch buffer filled up. Leave the rest on the stack. */
      mark_entry m = { scan, obj_scannable_end, obj_end };
      caml_prefetch(scan+1);
      if (stk.count == stk.size) {
        *Caml_state->mark_stack = stk;
        realloc_mark_stack(Caml_state->mark_stack);
        stk = *Caml_state->mark_stack;
      }
      CAML_EVENTLOG_DO({
        if (work <= 0 && pb_enqueued == pb_dequeued) {
          CAML_EV_COUNTER(EV_C_MAJOR_MARK_SLICE_REMAIN,
                          obj_scannable_end - scan);
        }
      });
      stk.stack[stk.count++] = m;
      /* We may have just discovered more work when we were about to run out.
         Reset min_pb so that we try to refill the buffer again. */
      min_pb = Pb_min;
    }
  }
  CAMLassert(pb_enqueued == pb_dequeued);
  *Caml_state->mark_stack = stk;
  if (darkened_anything)
    caml_ephe_list_pure = 0;
#ifdef CAML_INSTR
  *pslice_fields += slice_fields;
  *pslice_pointers += slice_pointers;
#endif
  return work;
}

static void mark_slice (intnat work)
{
#ifdef CAML_INSTR
  int slice_fields = 0; /** eventlog counters */
#endif /*CAML_INSTR*/
  int slice_pointers = 0;
  struct mark_stack* stk = Caml_state->mark_stack;

  caml_gc_message (0x40, "Marking %"ARCH_INTNAT_PRINTF_FORMAT"d words\n", work);
  caml_gc_message (0x40, "Subphase = %d\n", caml_gc_subphase);

  marked_words += work;
  while (1){
#ifndef CAML_INSTR
    work = do_some_marking(work);
#else
    work = do_some_marking(work, &slice_fields, &slice_pointers);
#endif

    if (work <= 0)
      break;

    CAMLassert (stk->count == 0);

    if( redarken_first_chunk != NULL ) {
      /* There are chunks that need to be redarkened because we
         overflowed our mark stack */
      if( redarken_chunk(redarken_first_chunk, stk) ) {
        redarken_first_chunk = Chunk_next(redarken_first_chunk);
      }
    } else if (caml_gc_subphase == Subphase_mark_roots) {
      CAML_EV_BEGIN(EV_MAJOR_MARK_ROOTS);
      marked_words -= work;
      work = caml_darken_all_roots_slice (work);
      marked_words += work;
      CAML_EV_END(EV_MAJOR_MARK_ROOTS);
      if (work > 0){
        caml_gc_subphase = Subphase_mark_main;
      }
    } else if (*ephes_to_check != (value) NULL) {
      /* Continue to scan the list of ephe */
      mark_ephe_aux(stk,&work,&slice_pointers);
    } else if (!caml_ephe_list_pure){
      /* We must scan again the list because some value have been darken */
      caml_ephe_list_pure = 1;
      ephes_to_check = ephes_checked_if_pure;
    }else{
      switch (caml_gc_subphase){
      case Subphase_mark_main: {
          /* Subphase_mark_main is done.
             Mark finalised values. */
          CAML_EV_BEGIN(EV_MAJOR_MARK_MAIN);
          caml_final_update_mark_phase ();
          /* Complete the marking */
          ephes_to_check = ephes_checked_if_pure;
          CAML_EV_END(EV_MAJOR_MARK_MAIN);
          caml_gc_subphase = Subphase_mark_final;
      }
        break;
      case Subphase_mark_final: {
        /** The set of unreachable value will not change anymore for
            this cycle. Start clean phase. */
        CAML_EV_BEGIN(EV_MAJOR_MARK_FINAL);
        caml_gc_phase = Phase_clean;
        caml_final_update_clean_phase ();
        caml_memprof_update_clean_phase ();
        if (caml_ephe_list_head != (value) NULL){
          /* Initialise the clean phase. */
          ephes_to_check = &caml_ephe_list_head;
        } else {
          /* Initialise the sweep phase. */
          init_sweep_phase();
        }
        marked_words -= work;
        work = 0;
        CAML_EV_END(EV_MAJOR_MARK_FINAL);
      }
        break;
      default: CAMLassert (0);
      }
    }
  }
  marked_words -= work;  /* work may be negative */
  CAML_EV_COUNTER(EV_C_MAJOR_MARK_SLICE_FIELDS, slice_fields);
  CAML_EV_COUNTER(EV_C_MAJOR_MARK_SLICE_POINTERS, slice_pointers);
}

/* Clean ephemerons */
static void clean_slice (intnat work)
{
  value v;

  caml_gc_message (0x40, "Cleaning %"
                   ARCH_INTNAT_PRINTF_FORMAT "d words\n", work);
  while (work > 0){
    v = *ephes_to_check;
    if (v != (value) NULL){
      if (Is_white_val (v)){
        /* The whole array is dead, remove it from the list. */
        *ephes_to_check = Field (v, CAML_EPHE_LINK_OFFSET);
        work -= 1;
      }else{
        caml_ephe_clean(v);
        ephes_to_check = &Field (v, CAML_EPHE_LINK_OFFSET);
        work -= Whsize_val (v);
      }
    }else{ /* End of list reached */
      /* Phase_clean is done. */
      /* Initialise the sweep phase. */
      init_sweep_phase();
      work = 0;
    }
  }
}

static void sweep_slice (intnat work)
{
  char *hp, *sweep_hp, *limit;
  header_t hd;

  caml_gc_message (0x40, "Sweeping %"
                   ARCH_INTNAT_PRINTF_FORMAT "d words\n", work);
  sweep_hp = caml_gc_sweep_hp;
  limit = sweep_chunk + Chunk_size(sweep_chunk);
  while (work > 0){
    if (sweep_hp < limit){
      caml_prefetch(sweep_hp + 4000);
      hp = sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case Caml_white:
        caml_gc_sweep_hp = sweep_hp;
        sweep_hp = (char *) caml_fl_merge_block (Val_hp (hp), limit);
        break;
      case Caml_blue:
        /* Only the blocks of the free-list are blue.  See [freelist.c]. */
        caml_fl_merge = (value) Bp_hp (hp);
        break;
      default:          /* gray or black */
        CAMLassert (Color_hd (hd) == Caml_black);
        Hd_hp (hp) = Whitehd_hd (hd);
        break;
      }
      CAMLassert (sweep_hp <= limit);
    }else{
      sweep_chunk = Chunk_next (sweep_chunk);
      if (sweep_chunk == NULL){
        /* Sweeping is done. */
        caml_gc_sweep_hp = sweep_hp;
        ++ Caml_state->stat_major_collections;
        work = 0;
        caml_gc_phase = Phase_idle;
        caml_request_minor_gc ();
      }else{
        sweep_hp = sweep_chunk;
        limit = sweep_chunk + Chunk_size (sweep_chunk);
      }
    }
  }
  caml_gc_sweep_hp = sweep_hp;
}

/* The main entry point for the major GC. Called about once for each
   minor GC. [howmuch] is the amount of work to do:
   -1 if the GC is triggered automatically
   0 to let the GC compute the amount of work
   [n] to make the GC do enough work to (on average) free [n] words
 */
void caml_major_collection_slice (intnat howmuch)
{
  double p, dp, filt_p, spend;
  intnat computed_work;
  int i;
  /*
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = Caml_state->stat_heap_wsz * caml_percent_free
                      / (100 + caml_percent_free)

     Assuming steady state and enforcing a constant allocation rate, then
     FM is divided in 2/3 for garbage and 1/3 for free list.
                 G = 2 * FM / 3
     G is also the amount of memory that will be used during this cycle
     (still assuming steady state).

     Proportion of G consumed since the previous slice:
                 PH = caml_allocated_words / G
                    = caml_allocated_words * 3 * (100 + caml_percent_free)
                      / (2 * Caml_state->stat_heap_wsz * caml_percent_free)
     Proportion of extra-heap resources consumed since the previous slice:
                 PE = caml_extra_heap_resources
     Proportion of total work to do in this slice:
                 P  = max (PH, PE)

     Here, we insert a time-based filter on the P variable to avoid large
     latency spikes in the GC, so the P below is a smoothed-out version of
     the P above.

     Amount of marking work for the GC cycle:
             MW = Caml_state->stat_heap_wsz * 100 / (100 + caml_percent_free)
                  + caml_incremental_roots_count
     Amount of sweeping work for the GC cycle:
             SW = Caml_state->stat_heap_wsz

     In order to finish marking with a non-empty free list, we will
     use 40% of the time for marking, and 60% for sweeping.

     Let MT be the time spent marking, ST the time spent sweeping, and TT
     the total time for this cycle. We have:
                 MT = 40/100 * TT
                 ST = 60/100 * TT

     Amount of time to spend on this slice:
                 T  = P * TT = P * MT / (40/100) = P * ST / (60/100)

     Since we must do MW work in MT time or SW work in ST time, the amount
     of work for this slice is:
                 MS = P * MW / (40/100)  if marking
                 SS = P * SW / (60/100)  if sweeping

     Amount of marking work for a marking slice:
                 MS = P * MW / (40/100)
                 MS = P * (Caml_state->stat_heap_wsz * 250
                           / (100 + caml_percent_free)
                           + 2.5 * caml_incremental_roots_count)
     Amount of sweeping work for a sweeping slice:
                 SS = P * SW / (60/100)
                 SS = P * Caml_state->stat_heap_wsz * 5 / 3

     This slice will either mark MS words or sweep SS words.
  */

  if (caml_major_slice_begin_hook != NULL) (*caml_major_slice_begin_hook) ();

  p = (double) caml_allocated_words * 3.0 * (100 + caml_percent_free)
      / Caml_state->stat_heap_wsz / caml_percent_free / 2.0;
  if (caml_dependent_size > 0){
    dp = (double) caml_dependent_allocated * (100 + caml_percent_free)
         / caml_dependent_size / caml_percent_free;
  }else{
    dp = 0.0;
  }
  if (p < dp) p = dp;
  if (p < caml_extra_heap_resources) p = caml_extra_heap_resources;
  p += (double)backlog_words / (double)Caml_state->stat_heap_wsz;
  backlog_words = 0;
  if (p > 0.3){
    backlog_words = (uintnat)((p - 0.3) * (double)Caml_state->stat_heap_wsz);
    p = 0.3;
  }

  CAML_EV_COUNTER (EV_C_MAJOR_WORK_EXTRA,
                  (uintnat) (caml_extra_heap_resources * 1000000));

  caml_gc_message (0x40, "ordered work = %"
                   ARCH_INTNAT_PRINTF_FORMAT "d words\n", howmuch);
  caml_gc_message (0x40, "allocated_words = %"
                         ARCH_INTNAT_PRINTF_FORMAT "u\n",
                   caml_allocated_words);
  caml_gc_message (0x40, "extra_heap_resources = %"
                         ARCH_INTNAT_PRINTF_FORMAT "uu\n",
                   (uintnat) (caml_extra_heap_resources * 1000000));
  caml_gc_message (0x40, "raw work-to-do = %"
                         ARCH_INTNAT_PRINTF_FORMAT "du\n",
                   (intnat) (p * 1000000));
  caml_gc_message (0x40, "work backlog = %"
                         ARCH_INTNAT_PRINTF_FORMAT "d\n",
                   backlog_words);

  for (i = 0; i < caml_major_window; i++){
    caml_major_ring[i] += p / caml_major_window;
  }

  if (caml_gc_clock >= 1.0){
    caml_gc_clock -= 1.0;
    ++caml_major_ring_index;
    if (caml_major_ring_index >= caml_major_window){
      caml_major_ring_index = 0;
    }
  }
  if (howmuch == -1){
    /* auto-triggered GC slice: spend work credit on the current bucket,
       then do the remaining work, if any */
    /* Note that the minor GC guarantees that the major slice is called in
       automatic mode (with [howmuch] = -1) at least once per clock tick.
       This means we never leave a non-empty bucket behind. */
    spend = fmin (caml_major_work_credit,
                  caml_major_ring[caml_major_ring_index]);
    caml_major_work_credit -= spend;
    filt_p = caml_major_ring[caml_major_ring_index] - spend;
    caml_major_ring[caml_major_ring_index] = 0.0;
  }else{
    /* forced GC slice: do work and add it to the credit */
    if (howmuch == 0){
      /* automatic setting: size of next bucket
         we do not use the current bucket, as it may be empty */
      int i = caml_major_ring_index + 1;
      if (i >= caml_major_window) i = 0;
      filt_p = caml_major_ring[i];
    }else{
      /* manual setting */
      filt_p = (double) howmuch * 3.0 * (100 + caml_percent_free)
               / Caml_state->stat_heap_wsz / caml_percent_free / 2.0;
    }
    caml_major_work_credit += filt_p;
    /* Limit work credit to 1.0 */
    caml_major_work_credit = fmin(caml_major_work_credit, 1.0);
  }

  p = filt_p;

  caml_gc_message (0x40, "filtered work-to-do = %"
                         ARCH_INTNAT_PRINTF_FORMAT "du\n",
                   (intnat) (p * 1000000));

  if (caml_gc_phase == Phase_idle){
    if (Caml_state->young_ptr == Caml_state->young_alloc_end){
      /* We can only start a major GC cycle if the minor allocation arena
         is empty, otherwise we'd have to treat it as a set of roots. */
      CAML_EV_BEGIN(EV_MAJOR_ROOTS);
      start_cycle ();
      CAML_EV_END(EV_MAJOR_ROOTS);
    }
    p = 0;
    goto finished;
  }

  if (p < 0){
    p = 0;
    goto finished;
  }

  if (caml_gc_phase == Phase_mark || caml_gc_phase == Phase_clean){
    computed_work = (intnat) (p * ((double) Caml_state->stat_heap_wsz * 250
                                   / (100 + caml_percent_free)
                                   + caml_incremental_roots_count));
  }else{
    computed_work = (intnat) (p * Caml_state->stat_heap_wsz * 5 / 3);
  }
  caml_gc_message (0x40, "computed work = %"
                   ARCH_INTNAT_PRINTF_FORMAT "d words\n", computed_work);
  if (caml_gc_phase == Phase_mark){
    CAML_EV_COUNTER (EV_C_MAJOR_WORK_MARK, computed_work);
    CAML_EV_BEGIN(EV_MAJOR_MARK);
    mark_slice (computed_work);
    CAML_EV_END(EV_MAJOR_MARK);
    caml_gc_message (0x02, "!");
  }else if (caml_gc_phase == Phase_clean){
    clean_slice (computed_work);
    caml_gc_message (0x02, "%%");
  }else{
    CAMLassert (caml_gc_phase == Phase_sweep);
    CAML_EV_COUNTER (EV_C_MAJOR_WORK_SWEEP, computed_work);
    CAML_EV_BEGIN(EV_MAJOR_SWEEP);
    sweep_slice (computed_work);
    CAML_EV_END(EV_MAJOR_SWEEP);
    caml_gc_message (0x02, "$");
  }

  if (caml_gc_phase == Phase_idle){
    double previous_overhead; // overhead at the end of the previous cycle

    CAML_EV_BEGIN(EV_MAJOR_CHECK_AND_COMPACT);
    caml_gc_message (0x200, "marked words = %"
                     ARCH_INTNAT_PRINTF_FORMAT "u words\n",
                     marked_words);
    caml_gc_message (0x200, "heap size at start of cycle = %"
                     ARCH_INTNAT_PRINTF_FORMAT "u words\n",
                     heap_wsz_at_cycle_start);
    if (marked_words == 0){
      previous_overhead = 1000000.;
      caml_gc_message (0x200, "overhead at start of cycle = +inf\n");
    }else{
      previous_overhead =
        100.0 * (heap_wsz_at_cycle_start - marked_words) / marked_words;
      caml_gc_message (0x200, "overhead at start of cycle = %.0f%%\n",
                       previous_overhead);
    }
    caml_compact_heap_maybe (previous_overhead);
    CAML_EV_END(EV_MAJOR_CHECK_AND_COMPACT);
  }

 finished:
  caml_gc_message (0x40, "work-done = %"
                         ARCH_INTNAT_PRINTF_FORMAT "du\n",
                   (intnat) (p * 1000000));

  /* if some of the work was not done, take it back from the credit
     or spread it over the buckets. */
  p = filt_p - p;
  spend = fmin (p, caml_major_work_credit);
  caml_major_work_credit -= spend;
  if (p > spend){
    p -= spend;
    p /= caml_major_window;
    for (i = 0; i < caml_major_window; i++) caml_major_ring[i] += p;
  }

  Caml_state->stat_major_words += caml_allocated_words;
  caml_allocated_words = 0;
  caml_dependent_allocated = 0;
  caml_extra_heap_resources = 0.0;
  if (caml_major_slice_end_hook != NULL) (*caml_major_slice_end_hook) ();
}

/* This does not call [caml_compact_heap_maybe] because the estimates of
   free and live memory are only valid for a cycle done incrementally.
   Besides, this function itself is called by [caml_compact_heap_maybe].
*/
void caml_finish_major_cycle (void)
{
  if (caml_gc_phase == Phase_idle){
    start_cycle ();
  }
  while (caml_gc_phase == Phase_mark) mark_slice (LONG_MAX);
  while (caml_gc_phase == Phase_clean) clean_slice (LONG_MAX);
  CAMLassert (caml_gc_phase == Phase_sweep);
  CAMLassert (redarken_first_chunk == NULL);
  while (caml_gc_phase == Phase_sweep) sweep_slice (LONG_MAX);
  CAMLassert (caml_gc_phase == Phase_idle);
  Caml_state->stat_major_words += caml_allocated_words;
  caml_allocated_words = 0;
}

/* Call this function to make sure [bsz] is greater than or equal
   to both [Heap_chunk_min] and the current heap increment.
*/
asize_t caml_clip_heap_chunk_wsz (asize_t wsz)
{
  asize_t result = wsz;
  uintnat incr;

  /* Compute the heap increment as a word size. */
  if (caml_major_heap_increment > 1000){
    incr = caml_major_heap_increment;
  }else{
    incr = Caml_state->stat_heap_wsz / 100 * caml_major_heap_increment;
  }

  if (result < incr){
    result = incr;
  }
  if (result < Heap_chunk_min){
    result = Heap_chunk_min;
  }
  return result;
}

/* [heap_size] is a number of bytes */
void caml_init_major_heap (asize_t heap_size)
{
  int i;

  Caml_state->stat_heap_wsz =
    caml_clip_heap_chunk_wsz (Wsize_bsize (heap_size));
  Caml_state->stat_top_heap_wsz = Caml_state->stat_heap_wsz;
  CAMLassert (Bsize_wsize (Caml_state->stat_heap_wsz) % Page_size == 0);
  caml_heap_start =
    (char *) caml_alloc_for_heap (Bsize_wsize (Caml_state->stat_heap_wsz));
  if (caml_heap_start == NULL)
    caml_fatal_error ("cannot allocate initial major heap");
  Chunk_next (caml_heap_start) = NULL;
  Caml_state->stat_heap_wsz = Wsize_bsize (Chunk_size (caml_heap_start));
  Caml_state->stat_heap_chunks = 1;
  Caml_state->stat_top_heap_wsz = Caml_state->stat_heap_wsz;

  if (caml_page_table_add(In_heap, caml_heap_start,
        caml_heap_start + Bsize_wsize (Caml_state->stat_heap_wsz))
      != 0) {
    caml_fatal_error ("cannot allocate initial page table");
  }

  caml_fl_init_merge ();
  caml_make_free_blocks ((value *) caml_heap_start,
                         Caml_state->stat_heap_wsz, 1, Caml_white);
  caml_gc_phase = Phase_idle;

  Caml_state->mark_stack = caml_stat_alloc_noexc(sizeof(struct mark_stack));
  if (Caml_state->mark_stack == NULL)
    caml_fatal_error ("not enough memory for the mark stack");

  Caml_state->mark_stack->stack =
    caml_stat_alloc_noexc(MARK_STACK_INIT_SIZE * sizeof(mark_entry));

  if(Caml_state->mark_stack->stack == NULL)
    caml_fatal_error("not enough memory for the mark stack");

  Caml_state->mark_stack->count = 0;
  Caml_state->mark_stack->size = MARK_STACK_INIT_SIZE;

  caml_allocated_words = 0;
  caml_extra_heap_resources = 0.0;
  for (i = 0; i < Max_major_window; i++) caml_major_ring[i] = 0.0;
}

void caml_set_major_window (int w){
  uintnat total = 0;
  int i;
  if (w == caml_major_window) return;
  CAMLassert (w <= Max_major_window);
  /* Collect the current work-to-do from the buckets. */
  for (i = 0; i < caml_major_window; i++){
    total += caml_major_ring[i];
  }
  /* Redistribute to the new buckets. */
  for (i = 0; i < w; i++){
    caml_major_ring[i] = total / w;
  }
  caml_major_window = w;
}

void caml_finalise_heap (void)
{
  /* Finishing major cycle (all values become white) */
  caml_empty_minor_heap ();
  caml_gc_message (0x1, "Finishing major GC cycle (finalising heap)\n");
  caml_finish_major_cycle ();
  CAMLassert (caml_gc_phase == Phase_idle);

  /* Finalising all values (by means of forced sweeping) */
  caml_fl_init_merge ();
  caml_gc_phase = Phase_sweep;
  sweep_chunk = caml_heap_start;
  caml_gc_sweep_hp = sweep_chunk;
  while (caml_gc_phase == Phase_sweep)
    sweep_slice (LONG_MAX);
}

#if defined(NAKED_POINTERS_CHECKER) && defined(NATIVE_CODE)

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

Caml_inline int safe_load(volatile header_t * p, header_t * result)
{
  header_t v;
  __try {
    v = *p;
  }
  __except(GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION ?
        EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) {
    *result = 0xdeadbeef;
    return 0;
  }
  *result = v;
  return 1;
}

#elif defined(TARGET_amd64)

Caml_inline int safe_load (header_t * addr, /*out*/ header_t * contents)
{
  int ok;
  header_t h;
  intnat tmp;

  asm volatile(
      "leaq 1f(%%rip), %[tmp] \n\t"
      "movq %[tmp], 0(%[handler]) \n\t"
      "xorl %[ok], %[ok] \n\t"
      "movq 0(%[addr]), %[h] \n\t"
      "movl $1, %[ok] \n\t"
  "1: \n\t"
      "xorq %[tmp], %[tmp] \n\t"
      "movq %[tmp], 0(%[handler])"
      : [tmp] "=&r" (tmp), [ok] "=&r" (ok), [h] "=&r" (h)
      : [addr] "r" (addr),
        [handler] "r" (&(Caml_state->checking_pointer_pc)));
  *contents = h;
  return ok;
}

#elif defined(TARGET_arm64)

Caml_inline int safe_load (header_t * addr, /*out*/ header_t * contents)
{
  int ok;
  header_t h;
  intnat tmp;

  asm volatile(
      "adr %[tmp], 1f \n\t"
      "str %[tmp], [%[handler]] \n\t"
      "mov %w[ok], #0 \n\t"
      "ldr %[h], [%[addr]] \n\t"
      "mov %w[ok], #1 \n\t"
  "1: \n\t"
      "mov %[tmp], #0 \n\t"
      "str %[tmp], [%[handler]]"
      : [tmp] "=&r" (tmp), [ok] "=&r" (ok), [h] "=&r" (h)
      : [addr] "r" (addr),
        [handler] "r" (&(Caml_state->checking_pointer_pc)));
  *contents = h;
  return ok;
}

#else
#error "NAKED_POINTERS_CHECKER not supported on this platform"
#endif

static void is_naked_pointer_safe (value v, value *p)
{
  header_t h;
  tag_t t;

  /* The following conditions were checked by the caller */
  CAMLassert(Is_block(v) && !Is_young(v) && !Is_in_heap(v));

  if (! safe_load(&Hd_val(v), &h)) goto on_segfault;

  t = Tag_hd(h);
  if (t == Infix_tag) {
    v -= Infix_offset_hd(h);
    if (! safe_load(&Hd_val(v), &h)) goto on_segfault;
    t = Tag_hd(h);
  }

  /* For the out-of-heap pointer to be considered safe,
   * it should have a black header and its size should be < 2 ** 40
   * words (128 GB). If not, we report a warning. */
  if (Is_black_hd(h) && Wosize_hd(h) < (INT64_LITERAL(1) << 40))
    return;

  caml_naked_pointers_detected = 1;
  if (!Is_black_hd(h)) {
    fprintf (stderr, "Out-of-heap pointer at %p of value %p has "
                     "non-black head (tag=%d)\n", p, (void*)v, t);
  } else {
    fprintf (stderr,
             "Out-of-heap pointer at %p of value %p has "
             "suspiciously large size: %" ARCH_INT64_PRINTF_FORMAT "u words\n",
              p, (void*)v, Wosize_hd(h));
  }
  return;

 on_segfault:
  caml_naked_pointers_detected = 1;
  fprintf (stderr, "Out-of-heap pointer at %p of value %p. "
           "Cannot read head.\n", p, (void*)v);
}

#endif
