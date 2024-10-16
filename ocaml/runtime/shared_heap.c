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
#define CAML_INTERNALS

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "caml/addrmap.h"
#include "caml/custom.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/fiber.h" /* for verification */
#include "caml/gc.h"
#include "caml/globroots.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/roots.h"
#include "caml/shared_heap.h"
#include "caml/sizeclasses.h"
#include "caml/startup_aux.h"
#include "caml/weak.h"

CAMLexport atomic_uintnat caml_compactions_count;

typedef unsigned int sizeclass;

/* Initial MARKED, UNMARKED, and GARBAGE values; any permutation would work */
struct global_heap_state caml_global_heap_state = {
  0 << HEADER_COLOR_SHIFT,
  1 << HEADER_COLOR_SHIFT,
  2 << HEADER_COLOR_SHIFT,
};

static uintnat current_chunk = 0;
static uintnat current_chunk_size = 0;

typedef struct pool {
  struct pool* next;
  value* next_obj;
  caml_domain_state* owner;
  sizeclass sz;
  intnat evacuate;
  uintnat chunk;
  uintnat chunk_size;
} pool;
static_assert(sizeof(pool) == Bsize_wsize(POOL_HEADER_WSIZE), "");
#define POOL_SLAB_WOFFSET(sz) (POOL_HEADER_WSIZE + wastage_sizeclass[sz])
#define POOL_FIRST_BLOCK(p, sz) ((header_t*)(p) + POOL_SLAB_WOFFSET(sz))
#define POOL_END(p) ((header_t*)(p) + POOL_WSIZE)
#define POOL_BLOCKS(sz) ((POOL_WSIZE - POOL_HEADER_WSIZE) / \
                         wsize_sizeclass[sz])

typedef struct large_alloc {
  caml_domain_state* owner;
  struct large_alloc* next;
} large_alloc;
static_assert(sizeof(large_alloc) % sizeof(value) == 0, "");
#define LARGE_ALLOC_HEADER_SZ sizeof(large_alloc)

static struct {
  caml_plat_mutex lock;
  pool* free;

  /* Mapped but not yet active pools */
  uintnat fresh_pools;
  char* next_fresh_pool;

  /* Count of all pools in use across all domains and the global lists below.

     Does not include unused pools ('free' above) or freshly allocated pools
     ('next_fresh_pool' above). */
  uintnat active_pools;

  /* these only contain swept memory of terminated domains*/
  struct heap_stats stats;
  _Atomic(pool*) global_avail_pools[NUM_SIZECLASSES];
  _Atomic(pool*) global_full_pools[NUM_SIZECLASSES];
  large_alloc* global_large;
} pool_freelist = {
  CAML_PLAT_MUTEX_INITIALIZER,
  NULL,
  0,
  NULL,
  0,
  { 0, },
  { NULL, },
  { NULL, },
  NULL
};

/* readable and writable only by the current thread */
struct caml_heap_state {
  pool* avail_pools[NUM_SIZECLASSES];
  pool* full_pools[NUM_SIZECLASSES];
  pool* unswept_avail_pools[NUM_SIZECLASSES];
  pool* unswept_full_pools[NUM_SIZECLASSES];

  large_alloc* swept_large;
  large_alloc* unswept_large;

  sizeclass next_to_sweep;

  caml_domain_state* owner;

  struct heap_stats stats;
};

struct compact_pool_stat {
  int free_blocks;
  int live_blocks;
};

/* You need to hold the [pool_freelist] lock to call these functions. */
static void orphan_heap_stats_with_lock(struct caml_heap_state *);
static void adopt_pool_stats_with_lock(struct caml_heap_state *,
                                       pool *, sizeclass);

struct caml_heap_state* caml_init_shared_heap (void) {
  int i;
  struct caml_heap_state* heap;

  heap = caml_stat_alloc_noexc(sizeof(struct caml_heap_state));
  if(heap != NULL) {
    for (i = 0; i<NUM_SIZECLASSES; i++) {
      heap->avail_pools[i] = heap->full_pools[i] =
        heap->unswept_avail_pools[i] = heap->unswept_full_pools[i] = 0;
    }
    heap->next_to_sweep = 0;
    heap->swept_large = NULL;
    heap->unswept_large = NULL;
    heap->owner = Caml_state;

    memset(&heap->stats, 0, sizeof(heap->stats));
  }
  return heap;
}

static int move_all_pools(pool** src, _Atomic(pool*)* dst,
                          caml_domain_state* new_owner) {
  int count = 0;
  while (*src) {
    pool* p = *src;
    *src = p->next;
    p->owner = new_owner;
    p->next = *dst;
    *dst = p;
    count++;
  }
  return count;
}

void caml_teardown_shared_heap(struct caml_heap_state* heap) {
  int i;
  int released = 0, released_large = 0;
  caml_plat_lock_blocking(&pool_freelist.lock);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    released +=
      move_all_pools(&heap->avail_pools[i],
                     &pool_freelist.global_avail_pools[i], NULL);

    released +=
      move_all_pools(&heap->full_pools[i],
                     &pool_freelist.global_full_pools[i], NULL);

    /* should be swept by now */
    CAMLassert(!heap->unswept_avail_pools[i]);
    CAMLassert(!heap->unswept_full_pools[i]);
  }
  CAMLassert(!heap->unswept_large);
  while (heap->swept_large) {
    large_alloc* a = heap->swept_large;
    heap->swept_large = a->next;
    a->next = pool_freelist.global_large;
    pool_freelist.global_large = a;
    released_large++;
  }
  orphan_heap_stats_with_lock(heap);
  caml_plat_unlock(&pool_freelist.lock);
  caml_stat_free(heap);
  caml_gc_log("Shutdown shared heap. Released %d active pools, %d large",
              released, released_large);
}

/* this _must_ be called either with the pool_freelist.lock held or
   during a stw in only a single domain */
static pool* alloc_pool(struct caml_heap_state* local) {
  pool* r = NULL;

  if (pool_freelist.fresh_pools == 0) {
    uintnat new_pools = pool_freelist.active_pools * 15 / 100;
    if (new_pools < 8) new_pools = 8;

    void* mem = caml_mem_map(Bsize_wsize(POOL_WSIZE) * new_pools, 0);

    if (mem) {
      pool_freelist.fresh_pools = new_pools;
      pool_freelist.next_fresh_pool = mem;
      current_chunk++;
      current_chunk_size = new_pools;
    }
  }

  if (pool_freelist.fresh_pools > 0) {
    r = (pool*)pool_freelist.next_fresh_pool;
    pool_freelist.next_fresh_pool += Bsize_wsize(POOL_WSIZE);
    pool_freelist.fresh_pools --;
    r->next = NULL;
    r->owner = NULL;
    r->evacuate = 0;
    r->chunk = current_chunk;
    r->chunk_size = current_chunk_size;
  }

  return r;
}

/* Allocating and deallocating pools from the global freelist. */

static pool* pool_acquire(struct caml_heap_state* local) {
  pool* r;

  caml_plat_lock_blocking(&pool_freelist.lock);
  r = pool_freelist.free;
  if (r) {
    pool_freelist.free = r->next;
  } else {
    r = alloc_pool(local);
  }
  if (r) {
    pool_freelist.active_pools ++;
    CAMLassert(r->owner == NULL);
  }
  caml_plat_unlock(&pool_freelist.lock);

  return r;
}

/* release [pool] to the current free list of pools */
static void pool_release(struct caml_heap_state* local,
                         pool* pool,
                         sizeclass sz)
{
  pool->owner = NULL;
  CAMLassert(pool->sz == sz);
  local->stats.pool_words -= POOL_WSIZE;
  local->stats.pool_frag_words -= POOL_HEADER_WSIZE + wastage_sizeclass[sz];
  caml_plat_lock_blocking(&pool_freelist.lock);
  pool->next = pool_freelist.free;
  pool_freelist.free = pool;
  pool_freelist.active_pools--;
  caml_plat_unlock(&pool_freelist.lock);
}

static void calc_pool_stats(pool* a, sizeclass sz, struct heap_stats* s)
{
  header_t* p = POOL_FIRST_BLOCK(a, sz);
  header_t* end = POOL_END(a);
  mlsize_t wh = wsize_sizeclass[sz];
  s->pool_frag_words += POOL_SLAB_WOFFSET(sz);

  while (p + wh <= end) {
    header_t hd = (header_t)atomic_load_relaxed((atomic_uintnat*)p);
    if (hd) {
      s->pool_live_words += Whsize_hd(hd);
      s->pool_frag_words += wh - Whsize_hd(hd);
      s->pool_live_blocks++;
    }

    p += wh;
  }
  CAMLassert(end == p);
  s->pool_words += POOL_WSIZE;
}

/* Initialize a pool and its object freelist */
Caml_inline void pool_initialize(pool* r,
                                 sizeclass sz,
                                 caml_domain_state* owner)
{
  mlsize_t wh = wsize_sizeclass[sz];
  header_t* p = POOL_FIRST_BLOCK(r, sz);
  header_t* end = POOL_END(r);

  r->next = 0;
  r->owner = owner;
  r->next_obj = 0;
  r->sz = sz;

  p[0] = 0;
  p[1] = 0;
  p += wh;

  while (p + wh <= end) {
    p[0] = 0; /* zero header indicates free object */
    p[1] = (value)(p - wh);
    #ifdef DEBUG
    for (int w = 2 ; w < wh; w++) {
      p[w] = Debug_free_major;
    }
    #endif
    p += wh;
  }
  CAMLassert(p == end);
  CAMLassert((uintptr_t)end % Cache_line_bsize == 0);
  r->next_obj = (value*)(p - wh);
}

/* Allocating an object from a pool */
CAMLno_tsan_for_perf
static intnat pool_sweep(struct caml_heap_state* local,
                         pool**,
                         sizeclass sz ,
                         int release_to_global_pool);

/* Adopt pool from the pool_freelist avail and full pools
   to satisfy an allocation */
static pool* pool_global_adopt(struct caml_heap_state* local, sizeclass sz)
{
  pool* r = NULL;
  int adopted_pool = 0;

  /* probably no available pools out there to be had */
  if( !atomic_load_relaxed(&pool_freelist.global_avail_pools[sz]) &&
      !atomic_load_relaxed(&pool_freelist.global_full_pools[sz]) )
    return NULL;

  /* Haven't managed to find a pool locally, try the global ones */
  caml_plat_lock_blocking(&pool_freelist.lock);
  if( atomic_load_relaxed(&pool_freelist.global_avail_pools[sz]) ) {
    r = atomic_load_relaxed(&pool_freelist.global_avail_pools[sz]);

    if( r ) {
      atomic_store_relaxed(&pool_freelist.global_avail_pools[sz], r->next);
      r->next = 0;
      local->avail_pools[sz] = r;
      adopt_pool_stats_with_lock(local, r, sz);

      #ifdef DEBUG
      {
        value* next_obj = r->next_obj;
        while( next_obj ) {
          CAMLassert(next_obj[0] == 0);
          next_obj = (value*)next_obj[1];
        }
      }
      #endif

    }
  }

  /* There were no global avail pools, so let's adopt one of the full ones and
     try our luck sweeping it later on */
  if( !r ) {
    r = atomic_load_relaxed(&pool_freelist.global_full_pools[sz]);

    if( r ) {
      atomic_store_relaxed(&pool_freelist.global_full_pools[sz], r->next);
      r->next = local->full_pools[sz];
      local->full_pools[sz] = r;
      adopt_pool_stats_with_lock(local, r, sz);

      adopted_pool = 1;
      r = 0; // this pool is full
    }
  }

  caml_plat_unlock(&pool_freelist.lock);

  if( !r && adopted_pool ) {
    Caml_state->major_work_done_between_slices +=
      pool_sweep(local, &local->full_pools[sz], sz, 0);
    r = local->avail_pools[sz];
  }
  return r;
}

static void update_pool_stats(struct caml_heap_state* local, sizeclass sz) {
  local->stats.pool_words += POOL_WSIZE;
  if (local->stats.pool_words > local->stats.pool_max_words)
    local->stats.pool_max_words = local->stats.pool_words;
  local->stats.pool_frag_words += POOL_HEADER_WSIZE + wastage_sizeclass[sz];
}

/* Allocating an object from a pool */
static pool* pool_find(struct caml_heap_state* local, sizeclass sz) {
  pool* r;

  /* Hopefully we have a pool we can use directly */
  r = local->avail_pools[sz];
  if (r) return r;

  /* Otherwise, try to sweep until we find one */
  while (!local->avail_pools[sz] && local->unswept_avail_pools[sz]) {
    Caml_state->major_work_done_between_slices +=
      pool_sweep(local, &local->unswept_avail_pools[sz], sz, 0);
  }

  r = local->avail_pools[sz];
  if (r) return r;

  /* Haven't managed to find a pool locally, try the global ones */
  r = pool_global_adopt(local, sz);
  if (r) return r;

  /* Failing that, we need to allocate a new pool */
  r = pool_acquire(local);
  if (!r) return 0; /* if we can't allocate, give up */

  update_pool_stats(local, sz);

  /* Having allocated a new pool, set it up for size sz */
  local->avail_pools[sz] = r;
  pool_initialize(r, sz, local->owner);

  return r;
}

static void* pool_allocate(struct caml_heap_state* local, sizeclass sz) {
  value* p;
  value* next;
  pool* r = pool_find(local, sz);

  if (!r) return 0;

  p = r->next_obj;
  next = (value*)p[1];
  r->next_obj = next;
  CAMLassert(p[0] == 0);
  if (!next) {
    local->avail_pools[sz] = r->next;
    r->next = local->full_pools[sz];
    local->full_pools[sz] = r;
  }

  CAMLassert(r->next_obj == 0 || *r->next_obj == 0);
  return p;
}

static void* large_allocate(struct caml_heap_state* local, mlsize_t sz) {
  large_alloc* a = malloc(sz + LARGE_ALLOC_HEADER_SZ);
  if (!a) return NULL;
  local->stats.large_words += Wsize_bsize(sz + LARGE_ALLOC_HEADER_SZ);
  if (local->stats.large_words > local->stats.large_max_words)
    local->stats.large_max_words = local->stats.large_words;
  local->stats.large_blocks++;
  a->owner = local->owner;
  a->next = local->swept_large;
  local->swept_large = a;
  return (char*)a + LARGE_ALLOC_HEADER_SZ;
}

value* caml_shared_try_alloc(struct caml_heap_state* local, mlsize_t wosize,
                             tag_t tag, reserved_t reserved)
{
  mlsize_t whsize = Whsize_wosize(wosize);
  value* p;
  uintnat colour;

  CAMLassert (wosize > 0);
  CAMLassert (tag != Infix_tag);

  CAML_EV_ALLOC(wosize);

  if (whsize <= SIZECLASS_MAX) {
    struct heap_stats* s;
    sizeclass sz = sizeclass_wsize[whsize];
    CAMLassert(wsize_sizeclass[sz] >= whsize);
    p = pool_allocate(local, sz);
    if (!p) return 0;
    s = &local->stats;
    s->pool_live_blocks++;
    s->pool_live_words += whsize;
    s->pool_frag_words += wsize_sizeclass[sz] - whsize;
  } else {
    p = large_allocate(local, Bsize_wsize(whsize));
    if (!p) return 0;
  }
  colour = caml_allocation_status();
  Hd_hp (p) = Make_header_with_reserved(wosize, tag, colour, reserved);
  /* Annotating a release barrier on `p` because TSan does not see the
   * happens-before relationship established by address dependencies
   * between the initializing writes here and the read in major_gc.c
   * marking (#12894) */
  CAML_TSAN_ANNOTATE_HAPPENS_BEFORE(p);
#ifdef DEBUG
  {
    int i;
    for (i = 0; i < wosize; i++) {
      Field(Val_hp(p), i) = Debug_free_major;
    }
  }
#endif
  return p;
}

/* Sweeping */

static intnat pool_sweep(struct caml_heap_state* local, pool** plist,
                         sizeclass sz, int release_to_global_pool) {
  intnat work;
  pool* a = *plist;
  if (!a) return 0;
  *plist = a->next;

  {
    header_t* p = POOL_FIRST_BLOCK(a, sz);
    header_t* end = POOL_END(a);
    mlsize_t wh = wsize_sizeclass[sz];
    int all_used = 1;
    struct heap_stats* s = &local->stats;

    /* conceptually, this is incremented by [wh] for every iteration
       below, however we can hoist these increments knowing that [p ==
       end] on exit from the loop (as asserted) */
    work = end - p;
    do {
      header_t hd = (header_t)atomic_load_relaxed((atomic_uintnat*)p);
      if (hd == 0) {
        /* already on freelist */
        all_used = 0;
      } else if (Has_status_hd(hd, caml_global_heap_state.GARBAGE)) {
        CAMLassert(Whsize_hd(hd) <= wh);
        if (Tag_hd (hd) == Custom_tag) {
          void (*final_fun)(value) = Custom_ops_val(Val_hp(p))->finalize;
          if (final_fun != NULL) final_fun(Val_hp(p));
        }
        /* add to freelist */
        atomic_store_relaxed((atomic_uintnat*)p, 0);
        p[1] = (value)a->next_obj;
        CAMLassert(Is_block((value)p));
#ifdef DEBUG
        {
          int i;
          mlsize_t wo = Wosize_whsize(wh);
          for (i = 1; i < wo; i++) {
            Field(Val_hp(p), i) = Debug_free_major;
          }
        }
#endif
        a->next_obj = (value*)p;
        all_used = 0;
        /* update stats */
        s->pool_live_blocks--;
        s->pool_live_words -= Whsize_hd(hd);
        local->owner->swept_words += Whsize_hd(hd);
        s->pool_frag_words -= (wh - Whsize_hd(hd));
      } else {
        /* still live, the pool can't be released to the global freelist */
        release_to_global_pool = 0;
      }
      p += wh;
    } while (p + wh <= end);
    CAMLassert(p == end);

    if (release_to_global_pool) {
      pool_release(local, a, sz);
    } else {
      pool** list = all_used ? &local->full_pools[sz] : &local->avail_pools[sz];
      a->next = *list;
      *list = a;
    }
  }

  return work;
}

static intnat large_alloc_sweep(struct caml_heap_state* local) {
  value* p;
  header_t hd;
  large_alloc* a = local->unswept_large;
  if (!a) return 0;
  local->unswept_large = a->next;

  p = (value*)((char*)a + LARGE_ALLOC_HEADER_SZ);
  /* The header being read here may be concurrently written by a thread doing
     marking. This is fine because marking can only make UNMARKED objects
     MARKED or NOT_MARKABLE, all of which are treated identically here. */
  hd = Hd_hp(p);
  if (Has_status_hd(hd, caml_global_heap_state.GARBAGE)) {
    if (Tag_hd (hd) == Custom_tag) {
      void (*final_fun)(value) = Custom_ops_val(Val_hp(p))->finalize;
      if (final_fun != NULL) final_fun(Val_hp(p));
    }

    local->stats.large_words -=
      Whsize_hd(hd) + Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    local->owner->swept_words +=
      Whsize_hd(hd) + Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    local->stats.large_blocks--;
    free(a);
  } else {
    a->next = local->swept_large;
    local->swept_large = a;
  }

  return Whsize_hd(hd);
}

static void verify_swept(struct caml_heap_state*);

intnat caml_sweep(struct caml_heap_state* local, intnat work) {
  /* Sweep local pools */
  while (work > 0 && local->next_to_sweep < NUM_SIZECLASSES) {
    sizeclass sz = local->next_to_sweep;
    intnat full_sweep_work = 0;
    intnat avail_sweep_work =
      pool_sweep(local, &local->unswept_avail_pools[sz], sz, 1);
    work -= avail_sweep_work;

    if (work > 0) {
      full_sweep_work = pool_sweep(local,
                                   &local->unswept_full_pools[sz],
                                   sz, 1);

      work -= full_sweep_work;
    }

    if(full_sweep_work+avail_sweep_work == 0) {
      local->next_to_sweep++;
    }
  }

  /* Sweep global pools */
  while (work > 0 && local->unswept_large) {
    work -= large_alloc_sweep(local);
  }

  if (caml_params->verify_heap && work > 0) {
    /* sweeping is complete, check everything worked */
    verify_swept(local);
  }
  return work;
}

uintnat caml_heap_size(struct caml_heap_state* local) {
  return Bsize_wsize(local->stats.pool_words + local->stats.large_words);
}

uintnat caml_top_heap_words(struct caml_heap_state* local) {
  /* FIXME: summing two maximums computed at different points in time
     returns an incorrect result. */
  return local->stats.pool_max_words + local->stats.large_max_words;
}


uintnat caml_heap_blocks(struct caml_heap_state* local) {
  return local->stats.pool_live_blocks + local->stats.large_blocks;
}

void caml_redarken_pool(struct pool* r, scanning_action f, void* fdata) {
  mlsize_t wh = wsize_sizeclass[r->sz];
  header_t* p = POOL_FIRST_BLOCK(r, r->sz);
  header_t* end = POOL_END(r);

  while (p + wh <= end) {
    header_t hd = p[0];
    if (hd != 0 && Has_status_hd(hd, caml_global_heap_state.MARKED)) {
      f(fdata, Val_hp(p), 0);
    }
    p += wh;
  }
}


/* Heap and freelist stats */

/* Move the given heap stats to the orphan pools.
   You need to hold the [pool_freelist] lock. */
static void orphan_heap_stats_with_lock(struct caml_heap_state *heap) {
  caml_accum_heap_stats(&pool_freelist.stats, &heap->stats);
  memset(&heap->stats, 0, sizeof(heap->stats));
}

/* The stats for an adopted pool are moved from the free pool stats to
   the heap stats of the adopting domain.
   You need to hold the [pool_freelist] lock. */
static void adopt_pool_stats_with_lock(
  struct caml_heap_state* adopter, pool *r, sizeclass sz)
{
    struct heap_stats pool_stats = { 0, };

    calc_pool_stats(r, sz, &pool_stats);
    caml_accum_heap_stats(&adopter->stats, &pool_stats);
    caml_remove_heap_stats(&pool_freelist.stats, &pool_stats);
}

/* Move the stats of all orphan pools into the given heap.
   You need to hold the [pool_freelist] lock. */
static void adopt_all_pool_stats_with_lock(struct caml_heap_state *adopter) {
  caml_accum_heap_stats(&adopter->stats, &pool_freelist.stats);
  memset(&pool_freelist.stats, 0, sizeof(pool_freelist.stats));
}

void caml_collect_heap_stats_sample(
  struct caml_heap_state* local,
  struct heap_stats* sample)
{
  *sample = local->stats;
}

/* Add the orphan pool stats to a stats accumulator. */
void caml_accum_orphan_heap_stats(struct heap_stats* acc)
{
  caml_plat_lock_blocking(&pool_freelist.lock);
  caml_accum_heap_stats(acc, &pool_freelist.stats);
  caml_plat_unlock(&pool_freelist.lock);
}


/* Atoms */
static const header_t atoms[256] = {
#define A(i) Make_header(0, i, NOT_MARKABLE)
A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),A(10),
A(11),A(12),A(13),A(14),A(15),A(16),A(17),A(18),A(19),A(20),
A(21),A(22),A(23),A(24),A(25),A(26),A(27),A(28),A(29),A(30),
A(31),A(32),A(33),A(34),A(35),A(36),A(37),A(38),A(39),A(40),
A(41),A(42),A(43),A(44),A(45),A(46),A(47),A(48),A(49),A(50),
A(51),A(52),A(53),A(54),A(55),A(56),A(57),A(58),A(59),A(60),
A(61),A(62),A(63),A(64),A(65),A(66),A(67),A(68),A(69),A(70),
A(71),A(72),A(73),A(74),A(75),A(76),A(77),A(78),A(79),A(80),
A(81),A(82),A(83),A(84),A(85),A(86),A(87),A(88),A(89),A(90),
A(91),A(92),A(93),A(94),A(95),A(96),A(97),A(98),A(99),A(100),
A(101),A(102),A(103),A(104),A(105),A(106),A(107),A(108),A(109),
A(110),A(111),A(112),A(113),A(114),A(115),A(116),A(117),A(118),
A(119),A(120),A(121),A(122),A(123),A(124),A(125),A(126),A(127),
A(128),A(129),A(130),A(131),A(132),A(133),A(134),A(135),A(136),
A(137),A(138),A(139),A(140),A(141),A(142),A(143),A(144),A(145),
A(146),A(147),A(148),A(149),A(150),A(151),A(152),A(153),A(154),
A(155),A(156),A(157),A(158),A(159),A(160),A(161),A(162),A(163),
A(164),A(165),A(166),A(167),A(168),A(169),A(170),A(171),A(172),
A(173),A(174),A(175),A(176),A(177),A(178),A(179),A(180),A(181),
A(182),A(183),A(184),A(185),A(186),A(187),A(188),A(189),A(190),
A(191),A(192),A(193),A(194),A(195),A(196),A(197),A(198),A(199),
A(200),A(201),A(202),A(203),A(204),A(205),A(206),A(207),A(208),
A(209),A(210),A(211),A(212),A(213),A(214),A(215),A(216),A(217),
A(218),A(219),A(220),A(221),A(222),A(223),A(224),A(225),A(226),
A(227),A(228),A(229),A(230),A(231),A(232),A(233),A(234),A(235),
A(236),A(237),A(238),A(239),A(240),A(241),A(242),A(243),A(244),
A(245),A(246),A(247),A(248),A(249),A(250),A(251),A(252),A(253),
A(254),A(255)
#undef A
};

CAMLexport value caml_atom(tag_t tag) {
  return Val_hp(&atoms[tag]);
}

void caml_init_major_heap (asize_t size) {
}


/* Verify heap invariants.

   Verification happens just after the heap is cycled during STW, so
   everything should be unmarked. If something reachable marked after
   cycling the heap, it means that garbage was reachable beforehand.
*/
struct heap_verify_state {
  value* stack;
  int stack_len;
  int sp;
  intnat objs;
  struct addrmap seen;
};

struct heap_verify_state* caml_verify_begin (void)
{
  struct heap_verify_state init = {0, 0, 0, 0, ADDRMAP_INIT};
  struct heap_verify_state* st = caml_stat_alloc(sizeof init);
  *st = init;
  return st;
}

static void verify_push (void* st_v, value v, volatile value* ignored)
{
  struct heap_verify_state* st = st_v;
  if (!Is_block(v)) return;

  if (st->sp == st->stack_len) {
    st->stack_len = st->stack_len * 2 + 100;
    st->stack = caml_stat_resize(st->stack,
         sizeof(value*) * st->stack_len);
  }
  st->stack[st->sp++] = v;
}

void caml_verify_root(void* state, value v, volatile value* p)
{
  verify_push(state, v, p);
}

static scanning_action_flags verify_scanning_flags = 0;

static void verify_object(struct heap_verify_state* st, value v) {
  intnat* entry;
  if (!Is_block(v)) return;

  CAMLassert (!Is_young(v));
  CAMLassert (Hd_val(v));

  if (Tag_val(v) == Infix_tag) {
    v -= Infix_offset_val(v);
    CAMLassert(Tag_val(v) == Closure_tag);
  }

  entry = caml_addrmap_insert_pos(&st->seen, v);
  if (*entry != ADDRMAP_NOT_PRESENT) return;
  *entry = 1;

  if (Has_status_val(v, NOT_MARKABLE)) return;
  st->objs++;

  CAMLassert(Has_status_val(v, caml_global_heap_state.UNMARKED));

  if (Tag_val(v) == Cont_tag) {
    struct stack_info* stk = Ptr_val(Field(v, 0));
    if (stk != NULL)
      caml_scan_stack(verify_push, verify_scanning_flags, st, stk, 0, NULL);
  } else if (Tag_val(v) < No_scan_tag) {
    int i = 0;
    if (Tag_val(v) == Closure_tag) {
      i = Start_env_closinfo(Closinfo_val(v));
    }
    mlsize_t scannable_wosize = Scannable_wosize_val(v);
    for (; i < scannable_wosize; i++) {
      value f = Field(v, i);
      if (Is_block(f)) verify_push(st, f, Op_val(v)+i);
    }
  }
}

void caml_verify_heap_from_stw(caml_domain_state *domain) {
  struct heap_verify_state* st = caml_verify_begin();
  caml_do_roots (&caml_verify_root, verify_scanning_flags, st, domain, 1);
  caml_scan_global_roots(&caml_verify_root, st);

  while (st->sp) verify_object(st, st->stack[--st->sp]);

  caml_addrmap_clear(&st->seen);
  caml_stat_free(st->stack);
  caml_stat_free(st);
}

/* Compaction starts here. See [caml_compact_heap] for entry. */

/* Given a single value `v`, found at `p`, check if it points to an
   evacuated block, and if so update it using the forwarding pointer
   created by the compactor. */
static inline void compact_update_value(void* ignored,
                                        value v,
                                        volatile value* p)
{
  if (Is_block(v)) {
    CAMLassert(!Is_young(v));

    tag_t tag = Tag_val(v);

    int infix_offset = 0;
    if (tag == Infix_tag) {
      infix_offset = Infix_offset_val(v);
      /* v currently points to an Infix_tag inside of a Closure_tag.
        The forwarding pointer we want is in the first field of the
        Closure_tag. */
      v -= infix_offset;
      CAMLassert(Tag_val(v) == Closure_tag);
    }

    /* non-markable blocks can't move */
    if (Has_status_val(v, NOT_MARKABLE))
      return;

    if (Whsize_val(v) <= SIZECLASS_MAX) {
      /* MARKED header status means the location `p` points to a block that
         has been evacuated. Use the forwarding pointer in the first field
         to update to the new location. */
      if (Has_status_val(v, caml_global_heap_state.MARKED)) {
        value fwd = Field(v, 0) + infix_offset;
        CAMLassert(Is_block(fwd));
        CAMLassert(Tag_val(fwd) == tag);
        *p = fwd;
      }
    }
  }
}

#ifdef DEBUG
static void compact_debug_check_pools(pool* p, int full) {
  while( p ) {
    /* go through each block and check the size is sz */
    header_t* block = POOL_FIRST_BLOCK(p, p->sz);
    header_t* end = POOL_END(p);

    while (block < end) {
      CAMLassert( *block || !full );
      if( *block ) {
        sizeclass sz = sizeclass_wsize[Whsize_hd(*block)];
        CAMLassert( sz == p->sz );
      }

      block += wsize_sizeclass[p->sz];
    }

    p = p->next;
  }
}

static void compact_debug_check_heap(struct caml_heap_state* heap) {
  /* check that all pools in the full_pools list are full */
  for (sizeclass sz = 1; sz < NUM_SIZECLASSES; sz++) {
    pool* p = heap->unswept_full_pools[sz];

    compact_debug_check_pools(p, 1);
  }

  /* check that all pools in the avail_pools list are not full */
  for (sizeclass sz = 1; sz < NUM_SIZECLASSES; sz++) {
    pool* p = heap->unswept_avail_pools[sz];

    compact_debug_check_pools(p, 0);
  }
}
#endif

/* Given a value found at `p` check if it points to an evacuated
   block, and if so update it using the forwarding pointer created by
   the compactor. */
static inline void compact_update_value_at(volatile value* p)
{
  compact_update_value(NULL, *p, p);
}

/* For each pointer in the block pointed to by `p`, check if it points
   to an evacuated block and if so update it using the forwarding
   pointer created by the compactor. */
static void compact_update_block(header_t* p)
{
  header_t hd = Hd_hp(p);

  /* We should never be called with a block that has a zero header (this would
    indicate a bug in traversing the shared pools). */
  CAMLassert(hd != 0);

  tag_t tag = Tag_hd(hd);

  /* We should never encounter an Infix tag iterating over the shared pools or
    large allocations. We could find it in roots but those use
    [compact_update_value]. */
  CAMLassert(tag != Infix_tag);

  if (tag == Cont_tag) {
    value stk = Field(Val_hp(p), 0);
    if (Ptr_val(stk)) {
      caml_scan_stack(&compact_update_value, 0, NULL, Ptr_val(stk), 0, NULL);
    }
  } else {
    uintnat offset = 0;

    if (tag == Closure_tag) {
      offset = Start_env_closinfo(Closinfo_val(Val_hp(p)));
    }

    if (tag < No_scan_tag) {
      mlsize_t scannable_wosz = Scannable_wosize_hd(hd);
      for (mlsize_t i = offset; i < scannable_wosz; i++) {
        compact_update_value_at(&Field(Val_hp(p), i));
      }
    }
  }
}

/* Update all the live blocks in a list of pools. */

static void compact_update_pools(pool *cur_pool)
{
  while (cur_pool) {
    header_t* p = POOL_FIRST_BLOCK(cur_pool, cur_pool->sz);
    header_t* end = POOL_END(cur_pool);
    mlsize_t wh = wsize_sizeclass[cur_pool->sz];

    while (p + wh <= end) {
      if (*p &&
          Has_status_val(Val_hp(p), caml_global_heap_state.UNMARKED)) {
        compact_update_block(p);
      }
      p += wh;
    }
    cur_pool = cur_pool->next;
  }
}

/* Update all the fields in the list of ephemerons found at `*ephe_p` */

static void compact_update_ephe_list(volatile value *ephe_p)
{
  while (*ephe_p) {
    compact_update_value_at(ephe_p);

    value ephe = *ephe_p;
    mlsize_t wosize = Wosize_val(ephe);
    compact_update_value_at(&Field(ephe, CAML_EPHE_DATA_OFFSET));

    for (int i = CAML_EPHE_FIRST_KEY ; i < wosize ; i++) {
      compact_update_value_at(&Field(ephe, i));
    }

    ephe_p = &Field(ephe, CAML_EPHE_LINK_OFFSET);
  }
}

int compact_count_pools(pool* start_pool) {
  int count = 0;
  while( start_pool ) {
    count++;
    start_pool = start_pool->next;
  }

  return count;
}

/* sort first by chunk_size and then by chunk */
int compact_compare_pools(const void* v1, const void* v2) {
  pool* p1 = *(pool**)v1;
  pool* p2 = *(pool**)v2;

  if (p1->chunk_size < p2->chunk_size) {
    return 1;
  } else if (p1->chunk_size > p2->chunk_size) {
    return -1;
  } else {
    if (p1->chunk > p2->chunk) {
      return -1;
    } else if (p1->chunk < p2->chunk) {
      return 1;
    } else {
      /* otherwise we want the pools in address order (which helps freeing
         them later) */
      return p2 > p1 ? 1 : -1;
    }
  }
}

/* In this phase each domain goes through its pools and calculates
   the number of live blocks in each pool. We then sort the pools
   (by chunk size) and move all live blocks to the largest chunks
   through marking the other pools as evacuating. */
void compact_phase_one_mark(struct caml_heap_state* heap) {

  for (int sz_class = 1; sz_class < NUM_SIZECLASSES; sz_class++) {
    /* count the number of pools */
    int avail_pools =
      compact_count_pools(heap->unswept_avail_pools[sz_class]);

    int full_pools =
      compact_count_pools(heap->unswept_full_pools[sz_class]);

    int total_pools = avail_pools + full_pools;

    int pool_blocks = POOL_BLOCKS(sz_class);

    pool* cur_pool = heap->unswept_avail_pools[sz_class];

    /* Count the number of free and live blocks in each pool. Note that a live
       block here currently has the header status UNMARKED (because it was
       MARKED in the previous cycle). After compaction the shared pools will
       contain UNMARKED and GARBAGE from the "to" pools and UNMARKED from the
       "from" pools which were evacuated.

       At the cost of some complexity or an additional pass we could compute the
       exact amount of space needed or even sweep all pools in this counting
       pass.
    */

    /* We count the live blocks we have in the full pools */
    int total_live_blocks = full_pools * pool_blocks;

    while (cur_pool) {
      header_t* p = POOL_FIRST_BLOCK(cur_pool, sz_class);
      header_t* end = POOL_END(cur_pool);
      mlsize_t wh = wsize_sizeclass[sz_class];

      while (p + wh <= end) {
        header_t h = (header_t)atomic_load_relaxed((atomic_uintnat*)p);

        /* A zero header in a shared heap pool indicates an empty space */
        if (!h) {
        } else if (Has_status_hd(h, caml_global_heap_state.UNMARKED)) {
          total_live_blocks++;
        }
        p += wh;
      }

      cur_pool = cur_pool->next;
    }

    int i = 0;
    pool** sz_pools =
      (pool**)caml_stat_alloc_noexc(sizeof(pool*) * total_pools);

    /* we failed to allocate sz_pools which means we can't really go any
      further marking pools to evacuate. */
    if( sz_pools == NULL ) {
      caml_gc_log("Unable to allocate sz_pools for phase one mark");

      return;
    }

    /* add all full pools to sz_pools */
    cur_pool = heap->unswept_full_pools[sz_class];
    while(cur_pool) {
      CAMLassert(i < full_pools);

      sz_pools[i] = cur_pool;

      i++;
      cur_pool = cur_pool->next;
    }

    /* add all avail pools to sz_pools */
    cur_pool = heap->unswept_avail_pools[sz_class];
    while(cur_pool) {
      CAMLassert(i < total_pools);

      sz_pools[i] = cur_pool;

      i++;
      cur_pool = cur_pool->next;
    }

    /* Now sort sz_pools so that the largest chunks are first */
    qsort(sz_pools, total_pools, sizeof(pool*), compact_compare_pools);

    /* Index of the first pool to evacuate is the ceiling of the total live
       blocks divided by the number of blocks in a pool. */
    int evac_idx = (total_live_blocks + pool_blocks - 1) / pool_blocks;

    /* mark all pools from 0 to evac_idx as not evacuated */
    for( i = 0 ; i < evac_idx && i < total_pools; i++ ) {
      pool* p = sz_pools[i];

      p->evacuate = 0;
    }

    /* mark all pools from evac_idx to total_pools as evacuating */
    for( i = evac_idx; i < total_pools; i++ ) {
      pool* p = sz_pools[i];

      p->evacuate = 1;
    }

    caml_stat_free(sz_pools);
  }
}

/* This is the second phase of compaction. This function should only
   be called from a single domain (and sandwiched between barriers).
   Returns 1 if it's worth us doing a second phase or 0 if not.

   In this function a single domain will create a sorted array of all
   pools in the process and then mark as evacuating all but the
   highest sorted `live_pools` pools. */
int compact_phase_two_mark(int participating_count,
                            caml_domain_state** participants) {

  /* We need to get the full set of pools across all domains,
     including the free list */
  int pools_count = 0;
  int free_pools = 0;

  for( int i = 0; i < participating_count; i++ ) {
    struct caml_heap_state* heap = participants[i]->shared_heap;

    for( int sz = 1; sz < NUM_SIZECLASSES; sz++ ) {
      /* Just recheck this */
      CAMLassert(heap->full_pools[sz] == NULL);
      CAMLassert(heap->avail_pools[sz] == NULL);

      int full_pools_count = compact_count_pools(heap->unswept_full_pools[sz]);
      int avail_pools_count =
        compact_count_pools(heap->unswept_avail_pools[sz]);

      pools_count += full_pools_count + avail_pools_count;
    }
  }

  /* We need to drain any free_pools that have already been batch allocated. In
    most cases these will be part of the biggest chunk (as the most recently
    allocated). */
  while( pool_freelist.fresh_pools > 0 ) {
    pool* p = alloc_pool(Caml_state->shared_heap);
    p->owner = NULL;
    p->next = pool_freelist.free;
    pool_freelist.free = p;
  }

  int live_pools = pools_count;

  free_pools = compact_count_pools(pool_freelist.free);
  pools_count += free_pools;

  pool** all_pools = (pool**)caml_stat_alloc_noexc(sizeof(pool*) * pools_count);
  int all_pools_idx = 0;

  if( all_pools == NULL ) {
    caml_gc_log("Unable to allocate all_pools for phase two mark");

    return 0;
  }

  /* Now we need to go through everything and add all pools to all_pools */
  for( int i = 0; i < participating_count; i++ ) {
    struct caml_heap_state* heap = participants[i]->shared_heap;

    for( int sz = 1; sz < NUM_SIZECLASSES; sz++ ) {
      /* Add all the full pools to all_pools */
      pool* cur_pool = heap->unswept_full_pools[sz];

      while( cur_pool ) {
        all_pools[all_pools_idx] = cur_pool;
        all_pools_idx++;
        cur_pool = cur_pool->next;
      }

      /* Add all the avail pools to all_pools */
      cur_pool = heap->unswept_avail_pools[sz];

      while( cur_pool ) {
        all_pools[all_pools_idx] = cur_pool;
        all_pools_idx++;
        cur_pool = cur_pool->next;
      }
    }
  }

  /* Add all the free pools to all_pools */
  pool* cur_pool = pool_freelist.free;

  while( cur_pool ) {
    all_pools[all_pools_idx] = cur_pool;
    all_pools_idx++;
    cur_pool = cur_pool->next;
  }

  CAMLassert(all_pools_idx == pools_count);

  /* Now sort all_pools so that the largest chunks are first */
  qsort(all_pools, pools_count, sizeof(pool*), compact_compare_pools);

  CAMLassert(live_pools <= pools_count);

  /* We need to mark the first live_pools as live */
  for(int i = 0; i < live_pools; i++) {
    pool* p = all_pools[i];

    p->evacuate = 0;
  }

  /* We now mark everything else as evacuating */
  for(int i = live_pools; i < pools_count; i++) {
    pool* p = all_pools[i];

    p->evacuate = 1;
  }

  /* The last thing we need to do is recreate pool_freelist.free
     in reverse-sorted order (so we allocate the largest
     chunks first) */
  pool* new_free = NULL;

  for(int i = pools_count - 1; i >= 0; i--) {
    pool* p = all_pools[i];

    if (p->owner == NULL) {
      /* This is a free pool, we need to add it to the free list */
      p->next = new_free;
      new_free = p;
    }
  }

  /* shouldn't need the platform lock here, we are only in one domain
     in a stw */
  pool_freelist.free = new_free;

  #ifdef DEBUG
  /* we check that chunk sizes are increasing in the free list*/
  int current_chunk_size_debug = INT_MAX;
  int free_pools_debug = 0;

  for( pool* p = pool_freelist.free; p; p = p->next ) {
    CAMLassert(p->chunk_size <= current_chunk_size_debug);
    current_chunk_size_debug = p->chunk_size;
    free_pools_debug++;
  }

  CAMLassert(free_pools_debug == free_pools);
  #endif

  caml_stat_free(all_pools);

  /* TODO: We always do a phase two at the moment. There are cases
      where we probably don't want to. */
  return 1;
}


/* This should only be called by one domain. Releases the freelist
   by unmmaping whole chunks of pools. Leaves partial chunks in place. */
void compact_release_freelist(void) {
    pool* cur_pool;

    caml_plat_lock_blocking(&pool_freelist.lock);

    int i = 0;
    int free_pools_count = compact_count_pools(pool_freelist.free);

    pool** free_pools =
      (pool**)caml_stat_alloc_noexc(sizeof(pool*) * free_pools_count);

    if( free_pools == NULL ) {
      /* this is fatal, we don't have enough space to actually free pools */
      caml_fatal_error
        ("Unable to allocate free_pools for compact_release_freelist");
    }

    cur_pool = pool_freelist.free;
    while( cur_pool ) {
      free_pools[i] = cur_pool;
      i++;
      cur_pool = cur_pool->next;
    }

    qsort(free_pools, free_pools_count, sizeof(pool*), compact_compare_pools);

    #ifdef DEBUG
    /* sanity check the free_pools list */
    int current_chunk_size_debug = INT_MAX;
    int current_chunk_debug = 0;

    for( i = 0; i < free_pools_count; i++ ) {
      CAMLassert(free_pools[i]->chunk_size <= current_chunk_size_debug);
      if( free_pools[i]->chunk == current_chunk_debug ) {
        CAMLassert(free_pools[i]->chunk_size == current_chunk_size_debug);

        if( i > 0 ) {
          /* chunks should also be in address order */
          CAMLassert(free_pools[i] < free_pools[i - 1]);
        }
      } else {
        current_chunk_debug = free_pools[i]->chunk;
        current_chunk_size_debug = free_pools[i]->chunk_size;
      }
    }
    #endif

    int chunk_seq = 0;
    int chunk_seq_idx = 0;
    int chunks_remaining = 0;

    if( free_pools_count > 0 ) {
      chunk_seq = free_pools[free_pools_count-1]->chunk;
      chunks_remaining = free_pools[free_pools_count-1]->chunk_size - 1;
      chunk_seq_idx = free_pools_count-1;
    }

    /* We start at the back of the free pools */
    for( i = free_pools_count - 2; i >= 0; i-- ) {
      cur_pool = free_pools[i];

      /* while on the same chunk, decrement chunks_remaining. If it hits zero
          then we can deallocate the whole lot */
      if( cur_pool->chunk == chunk_seq ) {
        chunks_remaining--;

        if( chunks_remaining == 0 ) {
          pool* chunk_seq_ptr = free_pools[chunk_seq_idx];

          caml_mem_unmap(chunk_seq_ptr,
            Bsize_wsize(POOL_WSIZE) * chunk_seq_ptr->chunk_size);

          /* remove the pools for this chunk from free_pools */
          for( int j = i; j <= chunk_seq_idx; j++ ) {
            free_pools[j] = NULL;
          }
        }
      } else {
        chunk_seq = cur_pool->chunk;
        chunks_remaining = cur_pool->chunk_size - 1;
        chunk_seq_idx = i;
      }
    }

    pool* new_free_list = NULL;

    for( i = 0; i < free_pools_count; i++ ) {
      if( free_pools[i] ) {
        free_pools[i]->next = new_free_list;
        new_free_list = free_pools[i];
      }
    }

    pool_freelist.free = new_free_list;

    caml_stat_free(free_pools);

    caml_plat_unlock(&pool_freelist.lock);
}

void compact_run_phase(struct caml_heap_state* heap,
                         int participating_count,
                         caml_domain_state** participants);

/* Compact the heap for the given domain. Run in parallel for all domains. */

static int should_run_phase_two = 0;

void caml_compact_heap(caml_domain_state* domain_state,
                         int participating_count,
                         caml_domain_state** participants)
{
  caml_gc_log("Compacting heap start");
  CAML_EV_BEGIN(EV_COMPACT);
  /* Warning: caml_compact_heap must only be called from
     [cycle_all_domains_callback] in major_gc.c as there are
     very specific conditions the compaction algorithm expects.

    This compaction algorithm operates in two phases. The first phase
    we sort pools in each class size for each domain, prefering larger
    chunks of pools. We then compact each size class in parallel across
    all domains, creating a list of free pools.

    The second phase we globally sort all pools and move those so we end
    up with whole chunks of free pools that can be freed.

    Each phase implements a compaction algorithm that is similar to
    Edward's Two-Finger algorithm from the original 1974 LISP book (The
    Programming Language LISP). At a high level the algorithm works as a series
    of parallel (using all running domains) steps separated by global barriers:

    1. For each size class
      a. Compute the number of live blocks in partially filled pools
      b. Keep enough pools to fully contain the number of live blocks and
         set the rest to be evacuated
      c. For each live block in each pool in the evacuation list,
         allocate and copy into a non-evacuating pool.
    2. Proceed through the roots and the heap, updating pointers to evacuated
       blocks to point to the new location of the block. Update finalisers and
       ephemerons too.
    3. Go through pools evacuated and release them. Finally free all but
        one pool in the freelist.
    4. One domain needs to release the pools in the freelist back to the OS.


  */

  caml_global_barrier(participating_count);
  CAML_EV_BEGIN(EV_COMPACT_EVACUATE);

  struct caml_heap_state* heap = Caml_state->shared_heap;

  #ifdef DEBUG
  /* Check preconditions for the heap: */
  for (int sz_class = 1; sz_class < NUM_SIZECLASSES; sz_class++) {
    /* No sweeping has happened yet */
    CAMLassert(heap->avail_pools[sz_class] == NULL);
    CAMLassert(heap->full_pools[sz_class] == NULL);
    CAMLassert(heap->swept_large == NULL);
    /* No pools waiting for adoption */
    if (participants[0] == Caml_state) {
      CAMLassert(
          atomic_load_relaxed(&pool_freelist.global_avail_pools[sz_class]) ==
            NULL);
      CAMLassert(
          atomic_load_relaxed(&pool_freelist.global_full_pools[sz_class]) ==
            NULL);
    }
    /* The minor heap is empty */
    CAMLassert(Caml_state->young_ptr == Caml_state->young_end);
    /* The mark stack is empty */
    CAMLassert(caml_mark_stack_is_empty());
  }
  #endif

  compact_phase_one_mark(heap);

  caml_global_barrier(participating_count);
  compact_run_phase(heap, participating_count, participants);
  caml_global_barrier(participating_count);

  if( participants[0] == Caml_state ) {
    should_run_phase_two =
      compact_phase_two_mark(participating_count, participants);
  }

  caml_global_barrier(participating_count);
  if( should_run_phase_two ) {
    compact_run_phase(heap, participating_count, participants);
  }
  caml_global_barrier(participating_count);

  if( participants[0] == Caml_state ) {
    compact_release_freelist();
     /* We are done, increment our compaction count */
    atomic_fetch_add(&caml_compactions_count, 1);
  }
}

/* acquires pools from the pool freelist during phase two.
   there should always be an available free list */
static pool* acquire_pool_from_free(caml_domain_state* domain_state,
                                    sizeclass sz) {
  caml_plat_lock_blocking(&pool_freelist.lock);
  pool* p = pool_freelist.free;

  CAMLassert(p);

  pool_freelist.free = p->next;

  pool_freelist.active_pools++;

  caml_plat_unlock(&pool_freelist.lock);

  update_pool_stats(domain_state->shared_heap, sz);

  pool_initialize(p, sz, domain_state);

  return p;
}

  /* Run by each */

void compact_run_phase(struct caml_heap_state* heap,
                         int participating_count,
                         caml_domain_state** participants) {

  pool* domain_evac_pools = NULL;

  for (int sz_class = 1; sz_class < NUM_SIZECLASSES; sz_class++) {
    pool* evac_pool = NULL;
    pool* evac_pool_last = NULL;
    pool* to_pool = NULL;

    #ifdef DEBUG
    compact_debug_check_pools(heap->unswept_avail_pools[sz_class], 0);
    compact_debug_check_pools(heap->unswept_full_pools[sz_class], 1);
    #endif

    /* We start with partial pools */
    pool* cur_pool = heap->unswept_avail_pools[sz_class];

    while( cur_pool != NULL ) {
      pool* next_pool = cur_pool->next;

      if( cur_pool->evacuate ) {
        cur_pool->next = evac_pool;
        evac_pool = cur_pool;
        if( evac_pool_last == NULL ) {
          evac_pool_last = cur_pool;
        }
      } else {
        cur_pool->next = to_pool;
        to_pool = cur_pool;
      }

      cur_pool = next_pool;
    }

    cur_pool = heap->unswept_full_pools[sz_class];

    while( cur_pool != NULL ) {
      pool* next_pool = cur_pool->next;

      if( cur_pool->evacuate ) {
        cur_pool->next = evac_pool;
        evac_pool = cur_pool;
        if( evac_pool_last == NULL ) {
          evac_pool_last = cur_pool;
        }
      } else {
        cur_pool->next = to_pool;
        to_pool = cur_pool;
      }

      cur_pool = next_pool;
    }

    /* There may be pools with an evacuate flag set in the pool_freelist.free
      but it's fine to ignore them here as this is a no-op. */

    #ifdef DEBUG
    compact_debug_check_pools(evac_pool, 0);
    compact_debug_check_pools(to_pool, 0);
    #endif

    /* to_pool can be NULL if there's no pools at all in this size class
       but it should be NULL phase two even if there are */
    if( evac_pool != NULL && to_pool == NULL ) {
      to_pool = acquire_pool_from_free(Caml_state, sz_class);
    }

    /* preserve these as we'll need them later on for re-assembling */
    pool* evac_pool_first = evac_pool;
    pool* to_pool_first = to_pool;

    while(evac_pool != NULL) {
      header_t* p = POOL_FIRST_BLOCK(evac_pool, sz_class);
      header_t* end = POOL_END(evac_pool);
      mlsize_t wh = wsize_sizeclass[sz_class];

      while (p + wh <= end) {
        header_t hd = (header_t)atomic_load_relaxed((atomic_uintnat*)p);

        /* A zero header in a shared heap pool indicates an empty space */
        if (hd) {
          CAMLassert (!Has_status_hd(hd, caml_global_heap_state.MARKED));
          CAMLassert (!Has_status_hd(hd, NOT_MARKABLE));

          /* Reminder: since colours have rotated, UNMARKED indicates a MARKED
          (i.e live) block */
          if (Has_status_hd(hd, caml_global_heap_state.UNMARKED)) {
            /* live block in an evacuating pool, so we evacuate it to
             * the first available block */
            while(to_pool->next_obj == NULL) {
              if( to_pool->next == NULL ) {
                pool* new_pool = acquire_pool_from_free(Caml_state, sz_class);
                to_pool->next = new_pool;
              }

              to_pool = to_pool->next;
            }

            value* new_p = to_pool->next_obj;
            CAMLassert(new_p);
            value *next = (value*)new_p[1];
            to_pool->next_obj = next;

            /* Copy the block to the new location */
            memcpy(new_p, p, Whsize_hd(hd) * sizeof(value));

            /* Set first field of p to a forwarding pointer */
            Field(Val_hp(p), 0) = Val_hp(new_p);

            /* Since there can be no blocks with the MARKED status, we use this
              to indicate that a block has been evacuated and any pointers to
              it should be updated. */
            *p = With_status_hd(hd, caml_global_heap_state.MARKED);
          } else if (Has_status_hd(hd, caml_global_heap_state.GARBAGE)) {
            /* We are implicitly sweeping pools in the evacuation set and thus
               we must remember to call finalisers for Custom blocks that would
               have been swept in a subsequent major cycle. */
            if (Tag_hd (hd) == Custom_tag) {
              void (*final_fun)(value) = Custom_ops_val(Val_hp(p))->finalize;
              if (final_fun) final_fun(Val_hp(p));
            }

            /* In the DEBUG runtime, we should overwrite the fields of swept
               blocks. Note: this pool can't be allocated in to again and so
               we overwrite the header and first fields too. */
            #ifdef DEBUG
            for (int w = 1 ; w < wh ; w++) {
              Field(p, w) = Debug_free_major;
            }
            #endif
          }
        }

        p += wh;
      }

      evac_pool = evac_pool->next;
    }

    /* now we need to fix up the unswept_full_pools and unswept_avail_pools */
    pool* new_full_pools = NULL;
    pool* new_avail_pools = NULL;

    #ifdef DEBUG
    compact_debug_check_pools(to_pool_first, 0);
    #endif

    /* we don't need to check the evac_pools because they're empty */
    cur_pool = to_pool_first;

    while( cur_pool != NULL ) {
      pool* next_pool = cur_pool->next;

      if( cur_pool->next_obj == NULL ) {
        /* This pool is now full */
        cur_pool->next = new_full_pools;
        new_full_pools = cur_pool;
      } else {
        /* This pool is now partially full */
        cur_pool->next = new_avail_pools;
        new_avail_pools = cur_pool;
      }

      cur_pool = next_pool;
    }

    heap->unswept_full_pools[sz_class] = new_full_pools;
    heap->unswept_avail_pools[sz_class] = new_avail_pools;
    if( evac_pool_last != NULL ) {
      CAMLassert(evac_pool_first);
      evac_pool_last->next = domain_evac_pools;
      domain_evac_pools = evac_pool_first;
    }
  }

  CAML_EV_END(EV_COMPACT_EVACUATE);
  caml_global_barrier(participating_count);

  #ifdef DEBUG
  compact_debug_check_heap(heap);
  #endif

  CAML_EV_BEGIN(EV_COMPACT_FORWARD);

  /* Second step: at this point all live blocks in evacuated pools
    have been moved and their old locations' first fields now point to
    their new locations. We now go through all pools again (including
    full ones this time) and for each field we check if the block the
    field points to has the header status MARKED - if it does then the block
    has been evacuated and we need to update the field to point to the new
    location. We do this by using the forwarding pointer that is in the first
    field of the evacuated block. */

  /* First we do roots (locals and finalisers) */
  caml_do_roots(&compact_update_value, 0, NULL, Caml_state, 1);

  /* Memprof roots and "weak" pointers to tracked blocks */
  caml_memprof_scan_roots(&compact_update_value, 0, NULL,
                          Caml_state, true);

  /* Next, one domain does the global roots */
  if (participants[0] == Caml_state) {
    caml_scan_global_roots(&compact_update_value, NULL);
  }

  /* Shared heap pools. */
  for (int sz_class = 1; sz_class < NUM_SIZECLASSES; sz_class++) {
    compact_update_pools(heap->unswept_avail_pools[sz_class]);
    compact_update_pools(heap->unswept_full_pools[sz_class]);
  }

  /* Large allocations */
  for (large_alloc* la = heap->unswept_large; la != NULL; la = la->next) {
    header_t* p = (header_t*)((char*)la + LARGE_ALLOC_HEADER_SZ);
    if (Has_status_val(Val_hp(p), caml_global_heap_state.UNMARKED)) {
      compact_update_block(p);
    }
  }

  /* Ephemerons */
  struct caml_ephe_info* ephe_info = Caml_state->ephe_info;
  compact_update_ephe_list(&ephe_info->todo);
  compact_update_ephe_list(&ephe_info->live);

  CAML_EV_END(EV_COMPACT_FORWARD);
  caml_global_barrier(participating_count);
  CAML_EV_BEGIN(EV_COMPACT_RELEASE);

  /* Third step: move all evacuated pools to the pool freelist */

  pool* cur_pool = domain_evac_pools;
  while (cur_pool) {
    pool* next_pool = cur_pool->next;

    #ifdef DEBUG
    for (header_t *p = POOL_FIRST_BLOCK(cur_pool, cur_pool->sz);
        p < POOL_END(cur_pool); p++) {
      *p = Debug_free_major;
    }
    #endif

    pool_release(heap, cur_pool, cur_pool->sz);
    cur_pool = next_pool;
  }

  CAML_EV_END(EV_COMPACT_RELEASE);
  caml_global_barrier(participating_count);

  caml_gc_log("Compacting heap complete");
  CAML_EV_END(EV_COMPACT);
}

/* Compaction end */

struct mem_stats {
  /* unit is words */
  uintnat alloced;
  uintnat live;
  uintnat free;
  uintnat overhead;

  uintnat live_blocks;
};

static void verify_pool(pool* a, sizeclass sz, struct mem_stats* s) {
  value* v;
  for (v = a->next_obj; v; v = (value*)v[1]) {
    CAMLassert(*v == 0);
  }

  {
    header_t* p = POOL_FIRST_BLOCK(a, sz);
    header_t* end = POOL_END(a);
    mlsize_t wh = wsize_sizeclass[sz];
    s->overhead += POOL_SLAB_WOFFSET(sz);

    while (p + wh <= end) {
      /* This header can be read here and concurrently marked by the GC, but
         this is fine: marking can only turn UNMARKED objects into MARKED or
         NOT_MARKABLE, which is of no consequence for this verification
         (namely, that there is no garbage left). */
      header_t hd = Hd_hp(p);
      CAMLassert(hd == 0 || !Has_status_hd(hd, caml_global_heap_state.GARBAGE));
      if (hd) {
        s->live += Whsize_hd(hd);
        s->overhead += wh - Whsize_hd(hd);
        s->live_blocks++;
      } else {
        s->free += wh;
      }
      p += wh;
    }
    CAMLassert(end == p);
    s->alloced += POOL_WSIZE;
  }
}

static void verify_large(large_alloc* a, struct mem_stats* s) {
  for (; a; a = a->next) {
    header_t hd = *(header_t*)((char*)a + LARGE_ALLOC_HEADER_SZ);
    CAMLassert (!Has_status_hd(hd, caml_global_heap_state.GARBAGE));
    s->alloced += Wsize_bsize(LARGE_ALLOC_HEADER_SZ) + Whsize_hd(hd);
    s->overhead += Wsize_bsize(LARGE_ALLOC_HEADER_SZ);
    s->live_blocks++;
  }
}

static void verify_swept (struct caml_heap_state* local) {
  int i;
  struct mem_stats pool_stats = {0,}, large_stats = {0,};

  /* sweeping should be done by this point */
  CAMLassert(local->next_to_sweep == NUM_SIZECLASSES);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    pool* p;
    CAMLassert(local->unswept_avail_pools[i] == NULL &&
               local->unswept_full_pools[i] == NULL);
    for (p = local->avail_pools[i]; p; p = p->next)
      verify_pool(p, i, &pool_stats);
    for (p = local->full_pools[i]; p; p = p->next) {
      CAMLassert(p->next_obj == NULL);
      verify_pool(p, i, &pool_stats);
    }
  }
  caml_gc_log("Pooled memory: %" ARCH_INTNAT_PRINTF_FORMAT
                "u alloced, %" ARCH_INTNAT_PRINTF_FORMAT
                "u free, %" ARCH_INTNAT_PRINTF_FORMAT
                "u fragmentation",
              pool_stats.alloced, pool_stats.free, pool_stats.overhead);

  verify_large(local->swept_large, &large_stats);
  CAMLassert(local->unswept_large == NULL);
  caml_gc_log("Large memory: %" ARCH_INTNAT_PRINTF_FORMAT
                "u alloced, %" ARCH_INTNAT_PRINTF_FORMAT
                "u free, %" ARCH_INTNAT_PRINTF_FORMAT
                "u fragmentation",
              large_stats.alloced, large_stats.free, large_stats.overhead);

  /* Check stats are being computed correctly */
  CAMLassert(local->stats.pool_words == pool_stats.alloced);
  CAMLassert(local->stats.pool_live_words == pool_stats.live);
  CAMLassert(local->stats.pool_live_blocks == pool_stats.live_blocks);
  CAMLassert(local->stats.pool_frag_words == pool_stats.overhead);
  CAMLassert(local->stats.pool_words -
         (local->stats.pool_live_words + local->stats.pool_frag_words)
         == pool_stats.free);
  CAMLassert(local->stats.large_words == large_stats.alloced);
  CAMLassert(local->stats.large_blocks == large_stats.live_blocks);
}

void caml_cycle_heap_from_stw_single (void) {
  struct global_heap_state oldg = caml_global_heap_state;
  struct global_heap_state newg;
  newg.UNMARKED     = oldg.MARKED;
  newg.GARBAGE      = oldg.UNMARKED;
  newg.MARKED       = oldg.GARBAGE; /* should be empty because
                                        garbage was swept */
  caml_global_heap_state = newg;
}

void caml_cycle_heap(struct caml_heap_state* local) {
  int i, received_p = 0, received_l = 0;

  caml_gc_log("Cycling heap [%02d]", local->owner->id);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    CAMLassert(local->unswept_avail_pools[i] == NULL);
    local->unswept_avail_pools[i] = local->avail_pools[i];
    local->avail_pools[i] = NULL;
    CAMLassert(local->unswept_full_pools[i] == NULL);
    local->unswept_full_pools[i] = local->full_pools[i];
    local->full_pools[i] = NULL;
  }
  CAMLassert(local->unswept_large == NULL);
  local->unswept_large = local->swept_large;
  local->swept_large = NULL;

  caml_plat_lock_blocking(&pool_freelist.lock);
  for (i = 0; i < NUM_SIZECLASSES; i++) {
    received_p += move_all_pools(
        (pool**)&pool_freelist.global_avail_pools[i],
        (_Atomic(pool*)*)&local->unswept_avail_pools[i],
        local->owner);
    received_p += move_all_pools(
        (pool**)&pool_freelist.global_full_pools[i],
        (_Atomic(pool*)*)&local->unswept_full_pools[i],
        local->owner);
  }
  while (pool_freelist.global_large) {
    large_alloc* a = pool_freelist.global_large;
    pool_freelist.global_large = a->next;
    a->owner = local->owner;
    a->next = local->unswept_large;
    local->unswept_large = a;
    received_l++;
  }
  if (received_p || received_l) {
    adopt_all_pool_stats_with_lock(local);
  }
  caml_plat_unlock(&pool_freelist.lock);
  if (received_p || received_l)
    caml_gc_log("Received %d new pools, %d new large allocs",
                received_p, received_l);

  local->next_to_sweep = 0;
}
