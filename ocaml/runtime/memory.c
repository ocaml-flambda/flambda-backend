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
#include <stdarg.h>
#include <stddef.h>
#include "caml/address_class.h"
#include "caml/config.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/shared_heap.h"
#include "caml/signals.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"
#include "caml/alloc.h"

int caml_huge_fallback_count = 0;
/* Number of times that mmapping big pages fails and we fell back to small
   pages. This counter is available to the program through
   [Gc.huge_fallback_count].
*/

uintnat caml_use_huge_pages = 0;
/* True iff the program allocates heap chunks by mmapping huge pages.
   This is set when parsing [OCAMLRUNPARAM] and must stay constant
   after that.
*/

extern uintnat caml_percent_free;                   /* major_gc.c */


CAMLexport color_t caml_allocation_color (void *hp)
{
  return Caml_black;
}

Caml_inline value caml_alloc_shr_aux (mlsize_t wosize, tag_t tag, int track,
                                      uintnat profinfo)
{
  value* hp;

  if (wosize > Max_wosize) return 0;
  CAML_EV_ALLOC(wosize);
  hp = caml_shared_try_alloc(Caml_state->shared_heap, wosize, tag, 0);

  CAMLassert (Is_in_heap (Val_hp (hp)));

  /* Inline expansion of caml_allocation_color. */
  Hd_hp (hp) = Make_header_with_profinfo (wosize, tag, Caml_black, profinfo);

  CAMLassert (Hd_hp (hp)
    == Make_header_with_profinfo (wosize, tag, caml_allocation_color (hp),
                                  profinfo));
  caml_allocated_words += Whsize_wosize (wosize);
  if (caml_allocated_words > Caml_state->minor_heap_wsz){
    CAML_EV_COUNTER (EV_C_REQUEST_MAJOR_ALLOC_SHR, 1);
    caml_request_major_slice ();
  }
#ifdef DEBUG
  {
    uintnat i;
    for (i = 0; i < wosize; i++){
      Field (Val_hp (hp), i) = Debug_uninit_major;
    }
  }
#endif
  if(track)
    caml_memprof_track_alloc_shr(Val_hp (hp));
  return Val_hp (hp);
}

Caml_inline value check_oom(value v)
{
  if (v == 0) {
    caml_fatal_out_of_memory ();
  }
  return v;
}

CAMLexport value caml_alloc_shr_with_profinfo (mlsize_t wosize, tag_t tag,
                                               intnat profinfo)
{
  return check_oom(caml_alloc_shr_aux(wosize, tag, 1, profinfo));
}

CAMLexport value caml_alloc_shr_for_minor_gc (mlsize_t wosize,
                                              tag_t tag, header_t old_hd)
{
  return check_oom(caml_alloc_shr_aux(wosize, tag, 0, Profinfo_hd(old_hd)));
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  return caml_alloc_shr_with_profinfo(wosize, tag, NO_PROFINFO);
}

CAMLexport value caml_alloc_shr_no_track_noexc (mlsize_t wosize, tag_t tag)
{
  return caml_alloc_shr_aux(wosize, tag, 0, NO_PROFINFO);
}

/* Dependent memory is all memory blocks allocated out of the heap
   that depend on the GC (and finalizers) for deallocation.
   For the GC to take dependent memory into account when computing
   its automatic speed setting,
   you must call [caml_alloc_dependent_memory] when you allocate some
   dependent memory, and [caml_free_dependent_memory] when you
   free it.  In both cases, you pass as argument the size (in bytes)
   of the block being allocated or freed.
*/
CAMLexport void caml_alloc_dependent_memory (mlsize_t nbytes)
{
  caml_dependent_size += nbytes / sizeof (value);
  caml_dependent_allocated += nbytes / sizeof (value);
}

CAMLexport void caml_free_dependent_memory (mlsize_t nbytes)
{
  if (caml_dependent_size < nbytes / sizeof (value)){
    caml_dependent_size = 0;
  }else{
    caml_dependent_size -= nbytes / sizeof (value);
  }
}

/* Use this function to tell the major GC to speed up when you use
   finalized blocks to automatically deallocate resources (other
   than memory). The GC will do at least one cycle every [max]
   allocated resources; [res] is the number of resources allocated
   this time.
   Note that only [res/max] is relevant.  The units (and kind of
   resource) can change between calls to [caml_adjust_gc_speed].
*/
CAMLexport void caml_adjust_gc_speed (mlsize_t res, mlsize_t max)
{
  if (max == 0) max = 1;
  if (res > max) res = max;
  caml_extra_heap_resources += (double) res / (double) max;
  if (caml_extra_heap_resources > 1.0){
    CAML_EV_COUNTER (EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED, 1);
    caml_extra_heap_resources = 1.0;
    caml_request_major_slice ();
  }
}

/* You must use [caml_initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [caml_initialize] never calls the GC, so you may call it while a block is
   unfinished (i.e. just after a call to [caml_alloc_shr].) */
/* PR#6084 workaround: define it as a weak symbol */
CAMLexport CAMLweakdef void caml_initialize (value *fp, value val)
{
  CAMLassert(Is_in_heap_or_young(fp));
  *fp = val;
  if (!Is_young((value)fp) && Is_block (val) && Is_young (val)) {
    add_to_ref_table (Caml_state->ref_table, fp);
  }
}

/* You must use [caml_modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [caml_modify] never calls the GC. */
/* [caml_modify] can also be used to do assignment on data structures that are
   in the minor heap instead of in the major heap.  In this case, it
   is a bit slower than simple assignment.
   In particular, you can use [caml_modify] when you don't know whether the
   block being changed is in the minor heap or the major heap. */
/* PR#6084 workaround: define it as a weak symbol */

CAMLexport CAMLweakdef void caml_modify (value *fp, value val)
{
  /* The write barrier implemented by [caml_modify] checks for the
     following two conditions and takes appropriate action:
     1- a pointer from the major heap to the minor heap is created
        --> add [fp] to the remembered set
     2- a pointer from the major heap to the major heap is overwritten,
        while the GC is in the marking phase
        --> call [caml_darken] on the overwritten pointer so that the
            major GC treats it as an additional root.

     The logic implemented below is duplicated in caml_array_fill to
     avoid repeated calls to caml_modify and repeated tests on the
     values.  Don't forget to update caml_array_fill if the logic
     below changes!
  */
  value old;

  if (Is_young((value)fp)) {
    /* The modified object resides in the minor heap.
       Conditions 1 and 2 cannot occur. */
    *fp = val;
  } else {
    /* The modified object resides in the major heap. */
    CAMLassert(Is_in_heap(fp));
    old = *fp;
    *fp = val;
    if (Is_block(old)) {
      /* If [old] is a pointer within the minor heap, we already
         have a major->minor pointer and [fp] is already in the
         remembered set.  Conditions 1 and 2 cannot occur. */
      if (Is_young(old)) return;
      /* Here, [old] can be a pointer within the major heap.
         Check for condition 2. */
      if (caml_gc_phase == Phase_mark) {
        header_t hd = Hd_val(old);
        if (Tag_hd (hd) == Infix_tag) {
          /* Infix_tag is always Caml_white */
          CAMLassert(Is_white_hd(hd));
        }
        /* Inline the white-header check, to save a pagetable lookup */
        if (Is_white_hd(hd)) {
          caml_darken(old, NULL);
        }
      }
    }
    /* Check for condition 1. */
    if (Is_block(val) && Is_young(val)) {
      add_to_ref_table (Caml_state->ref_table, fp);
    }
  }
}

/* This version of [caml_modify] may additionally be used to mutate
   locally-allocated objects. (This version is used by mutations
   generated from OCaml code when the value being modified may be
   locally allocated) */
CAMLexport void caml_modify_local (value obj, intnat i, value val)
{
  if (Color_hd(Hd_val(obj)) == Local_unmarked) {
    Field(obj, i) = val;
  } else {
    caml_modify(&Field(obj, i), val);
  }
}

CAMLexport intnat caml_local_region_begin()
{
  return Caml_state->local_sp;
}

CAMLexport void caml_local_region_end(intnat reg)
{
  Caml_state->local_sp = reg;
}

CAMLexport caml_local_arenas* caml_get_local_arenas()
{
  caml_local_arenas* s = Caml_state->local_arenas;
  if (s != NULL)
    s->saved_sp = Caml_state->local_sp;
  return s;
}

CAMLexport void caml_set_local_arenas(caml_local_arenas* s)
{
  Caml_state->local_arenas = s;
  if (s != NULL) {
    struct caml_local_arena a = s->arenas[s->count - 1];
    Caml_state->local_sp = s->saved_sp;
    Caml_state->local_top = (void*)(a.base + a.length);
    Caml_state->local_limit = - a.length;
  } else {
    Caml_state->local_sp = 0;
    Caml_state->local_top = NULL;
    Caml_state->local_limit = 0;
  }
}

void caml_local_realloc()
{
  caml_local_arenas* s = caml_get_local_arenas();
  intnat i;
  char* arena;
  caml_stat_block block;
  if (s == NULL) {
    s = caml_stat_alloc(sizeof(*s));
    s->count = 0;
    s->next_length = 0;
    s->saved_sp = Caml_state->local_sp;
  }
  if (s->count == Max_local_arenas)
    caml_fatal_error("Local allocation stack overflow - exceeded Max_local_arenas");

  do {
    if (s->next_length == 0) {
      s->next_length = Init_local_arena_bsize;
    } else {
      /* overflow check */
      CAML_STATIC_ASSERT(((intnat)Init_local_arena_bsize << (2*Max_local_arenas)) > 0);
      s->next_length *= 4;
    }
    /* may need to loop, if a very large allocation was requested */
  } while (s->saved_sp + s->next_length < 0);

  arena = caml_stat_alloc_aligned_noexc(s->next_length, 0, &block);
  if (arena == NULL)
    caml_fatal_error("Local allocation stack overflow - out of memory");
#ifdef DEBUG
  for (i = 0; i < s->next_length; i += sizeof(value)) {
    *((header_t*)(arena + i)) = Debug_uninit_local;
  }
#endif
  for (i = s->saved_sp; i < 0; i += sizeof(value)) {
    *((header_t*)(arena + s->next_length + i)) = Local_uninit_hd;
  }
  caml_gc_message(0x08,
                  "Growing local stack to %"ARCH_INTNAT_PRINTF_FORMAT"d kB\n",
                  s->next_length / 1024);
  s->count++;
  s->arenas[s->count-1].length = s->next_length;
  s->arenas[s->count-1].base = arena;
  s->arenas[s->count-1].alloc_block = block;
  caml_set_local_arenas(s);
  CAMLassert(Caml_state->local_limit <= Caml_state->local_sp);
}

CAMLexport value caml_alloc_local(mlsize_t wosize, tag_t tag)
{
#if defined(NATIVE_CODE) && defined(STACK_ALLOCATION)
  intnat sp = Caml_state->local_sp;
  header_t* hp;
  sp -= Bhsize_wosize(wosize);
  Caml_state->local_sp = sp;
  if (sp < Caml_state->local_limit)
    caml_local_realloc();
  hp = (header_t*)((char*)Caml_state->local_top + sp);
  *hp = Make_header(wosize, tag, Local_unmarked);
  return Val_hp(hp);
#else
  if (wosize <= Max_young_wosize) {
    return caml_alloc_small(wosize, tag);
  } else {
    /* The return value is initialised directly using Field.
       This is invalid if it may create major -> minor pointers.
       So, perform a minor GC to prevent this. (See caml_make_vect) */
    caml_minor_collection();
    return caml_alloc_shr(wosize, tag);
  }
#endif
}

CAMLprim value caml_local_stack_offset(value blk)
{
#ifdef NATIVE_CODE
  intnat sp = Caml_state->local_sp;
  return Val_long(-sp);
#else
  return Val_long(0);
#endif
}

/* Global memory pool.

   The pool is structured as a ring of blocks, where each block's header
   contains two links: to the previous and to the next block. The data
   structure allows for insertions and removals of blocks in constant time,
   given that a pointer to the operated block is provided.

   Initially, the pool contains a single block -- a pivot with no data, the
   guaranteed existence of which makes for a more concise implementation.

   The API functions that operate on the pool receive not pointers to the
   block's header, but rather pointers to the block's "data" field. This
   behaviour is required to maintain compatibility with the interfaces of
   [malloc], [realloc], and [free] family of functions, as well as to hide
   the implementation from the user.
*/

/* A type with the most strict alignment requirements */
union max_align {
  char c;
  short s;
  long l;
  int i;
  float f;
  double d;
  void *v;
  void (*q)(void);
};

struct pool_block {
#ifdef DEBUG
  intnat magic;
#endif
  struct pool_block *next;
  struct pool_block *prev;
  /* Use C99's flexible array types if possible */
#if (__STDC_VERSION__ >= 199901L)
  union max_align data[];  /* not allocated, used for alignment purposes */
#else
  union max_align data[1];
#endif
};

#if (__STDC_VERSION__ >= 199901L)
#define SIZEOF_POOL_BLOCK sizeof(struct pool_block)
#else
#define SIZEOF_POOL_BLOCK offsetof(struct pool_block, data)
#endif

static struct pool_block *pool = NULL;


/* Returns a pointer to the block header, given a pointer to "data" */
static struct pool_block* get_pool_block(caml_stat_block b)
{
  if (b == NULL)
    return NULL;

  else {
    struct pool_block *pb =
      (struct pool_block*)(((char*)b) - SIZEOF_POOL_BLOCK);
#ifdef DEBUG
    CAMLassert(pb->magic == Debug_pool_magic);
#endif
    return pb;
  }
}

CAMLexport void caml_stat_create_pool(void)
{
  if (pool == NULL) {
    pool = malloc(SIZEOF_POOL_BLOCK);
    if (pool == NULL)
      caml_fatal_error("out of memory");
#ifdef DEBUG
    pool->magic = Debug_pool_magic;
#endif
    pool->next = pool;
    pool->prev = pool;
  }
}

CAMLexport void caml_stat_destroy_pool(void)
{
  if (pool != NULL) {
    pool->prev->next = NULL;
    while (pool != NULL) {
      struct pool_block *next = pool->next;
      free(pool);
      pool = next;
    }
    pool = NULL;
  }
}

/* [sz] and [modulo] are numbers of bytes */
CAMLexport void* caml_stat_alloc_aligned_noexc(asize_t sz, int modulo,
                                               caml_stat_block *b)
{
  char *raw_mem;
  uintnat aligned_mem;
  CAMLassert (0 <= modulo && modulo < Page_size);
  raw_mem = (char *) caml_stat_alloc_noexc(sz + Page_size);
  if (raw_mem == NULL) return NULL;
  *b = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((uintnat) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    uintnat *p;
    uintnat *p0 = (void *) *b;
    uintnat *p1 = (void *) (aligned_mem - modulo);
    uintnat *p2 = (void *) (aligned_mem - modulo + sz);
    uintnat *p3 = (void *) ((char *) *b + sz + Page_size);
    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}

/* [sz] and [modulo] are numbers of bytes */
CAMLexport void* caml_stat_alloc_aligned(asize_t sz, int modulo,
                                         caml_stat_block *b)
{
  void *result = caml_stat_alloc_aligned_noexc(sz, modulo, b);
  /* malloc() may return NULL if size is 0 */
  if ((result == NULL) && (sz != 0))
    caml_fatal_out_of_memory();
  return result;
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_alloc_noexc(asize_t sz)
{
  /* Backward compatibility mode */
  if (pool == NULL)
    return malloc(sz);
  else {
    struct pool_block *pb = malloc(sz + SIZEOF_POOL_BLOCK);
    if (pb == NULL) return NULL;
#ifdef DEBUG
    memset(&(pb->data), Debug_uninit_stat, sz);
    pb->magic = Debug_pool_magic;
#endif

    /* Linking the block into the ring */
    pb->next = pool->next;
    pb->prev = pool;
    pool->next->prev = pb;
    pool->next = pb;

    return &(pb->data);
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_alloc(asize_t sz)
{
  void *result = caml_stat_alloc_noexc(sz);
  /* malloc() may return NULL if size is 0 */
  if ((result == NULL) && (sz != 0))
    caml_fatal_out_of_memory();
  return result;
}

CAMLexport void caml_stat_free(caml_stat_block b)
{
  /* Backward compatibility mode */
  if (pool == NULL)
    free(b);
  else {
    struct pool_block *pb = get_pool_block(b);
    if (pb == NULL) return;

    /* Unlinking the block from the ring */
    pb->prev->next = pb->next;
    pb->next->prev = pb->prev;

    free(pb);
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_resize_noexc(caml_stat_block b, asize_t sz)
{
  if(b == NULL)
    return caml_stat_alloc_noexc(sz);
  /* Backward compatibility mode */
  if (pool == NULL)
    return realloc(b, sz);
  else {
    struct pool_block *pb = get_pool_block(b);
    struct pool_block *pb_new = realloc(pb, sz + SIZEOF_POOL_BLOCK);
    if (pb_new == NULL) return NULL;

    /* Relinking the new block into the ring in place of the old one */
    pb_new->prev->next = pb_new;
    pb_new->next->prev = pb_new;

    return &(pb_new->data);
  }
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_resize(caml_stat_block b, asize_t sz)
{
  void *result = caml_stat_resize_noexc(b, sz);
  if (result == NULL)
    caml_fatal_out_of_memory();
  return result;
}

/* [sz] is a number of bytes */
CAMLexport caml_stat_block caml_stat_calloc_noexc(asize_t num, asize_t sz)
{
  uintnat total;
  if (caml_umul_overflow(sz, num, &total))
    return NULL;
  else {
    caml_stat_block result = caml_stat_alloc_noexc(total);
    if (result != NULL)
      memset(result, 0, total);
    return result;
  }
}

CAMLexport caml_stat_string caml_stat_strdup_noexc(const char *s)
{
  size_t slen = strlen(s);
  caml_stat_block result = caml_stat_alloc_noexc(slen + 1);
  if (result == NULL)
    return NULL;
  memcpy(result, s, slen + 1);
  return result;
}

CAMLexport caml_stat_string caml_stat_strdup(const char *s)
{
  caml_stat_string result = caml_stat_strdup_noexc(s);
  if (result == NULL)
    caml_fatal_out_of_memory();
  return result;
}

#ifdef _WIN32

CAMLexport wchar_t * caml_stat_wcsdup(const wchar_t *s)
{
  int slen = wcslen(s);
  wchar_t* result = caml_stat_alloc((slen + 1)*sizeof(wchar_t));
  if (result == NULL)
    caml_fatal_out_of_memory();
  memcpy(result, s, (slen + 1)*sizeof(wchar_t));
  return result;
}

#endif

CAMLexport caml_stat_string caml_stat_strconcat(int n, ...)
{
  va_list args;
  char *result, *p;
  size_t len = 0;
  int i;

  va_start(args, n);
  for (i = 0; i < n; i++) {
    const char *s = va_arg(args, const char*);
    len += strlen(s);
  }
  va_end(args);

  result = caml_stat_alloc(len + 1);

  va_start(args, n);
  p = result;
  for (i = 0; i < n; i++) {
    const char *s = va_arg(args, const char*);
    size_t l = strlen(s);
    memcpy(p, s, l);
    p += l;
  }
  va_end(args);

  *p = 0;
  return result;
}

#ifdef _WIN32

CAMLexport wchar_t* caml_stat_wcsconcat(int n, ...)
{
  va_list args;
  wchar_t *result, *p;
  size_t len = 0;
  int i;

  va_start(args, n);
  for (i = 0; i < n; i++) {
    const wchar_t *s = va_arg(args, const wchar_t*);
    len += wcslen(s);
  }
  va_end(args);

  result = caml_stat_alloc((len + 1)*sizeof(wchar_t));

  va_start(args, n);
  p = result;
  for (i = 0; i < n; i++) {
    const wchar_t *s = va_arg(args, const wchar_t*);
    size_t l = wcslen(s);
    memcpy(p, s, l*sizeof(wchar_t));
    p += l;
  }
  va_end(args);

  *p = 0;
  return result;
}

#endif
