/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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
#include "caml/memory.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/stack.h"
#include "caml/roots.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include <string.h>
#include <stdio.h>

/* Roots registered from C functions */

void (*caml_scan_roots_hook) (scanning_action) = NULL;

/* The hashtable of frame descriptors */
frame_descr ** caml_frame_descriptors = NULL;
uintnat caml_frame_descriptors_mask = 0;

/* Linked-list */

typedef struct link {
  void *data;
  struct link *next;
} link;

static link *cons(void *data, link *tl) {
  link *lnk = caml_stat_alloc(sizeof(link));
  lnk->data = data;
  lnk->next = tl;
  return lnk;
}

#define iter_list(list,lnk) \
  for (lnk = list; lnk != NULL; lnk = lnk->next)

/* Linked-list of frametables */

static link *frametables = NULL;
static intnat num_descr = 0;

static intnat count_descriptors(link *list) {
  intnat num_descr = 0;
  link *lnk;
  iter_list(list,lnk) {
    num_descr += *((intnat*) lnk->data);
  }
  return num_descr;
}

static link* frametables_list_tail(link *list) {
  link *lnk, *tail = NULL;
  iter_list(list,lnk) {
    tail = lnk;
  }
  return tail;
}

/* Special marker instead of frame_size for frame_descr in long format */
static uint32_t LONG_FRAME_MARKER = 0x7FFF;

uint32_t caml_get_frame_size(frame_descr *d) {
  CAMLassert(d && d->frame_size != 0xFFFF);
  if (d->frame_size == LONG_FRAME_MARKER) {
    /* Handle long frames */
    frame_descr_long *dl = (frame_descr_long *)d;
    return (dl->frame_size);
  } else {
    return (d->frame_size);
  }
}

/* Skip to end of live_ofs */
unsigned char * caml_get_end_of_live_ofs (frame_descr *d) {
  CAMLassert(d && d->frame_size != 0xFFFF);
  if (d->frame_size == LONG_FRAME_MARKER) {
    /* Handle long frames */
    frame_descr_long *dl = (frame_descr_long *)d;
    return ((unsigned char*)&dl->live_ofs[dl->num_live]);
  } else {
    return ((unsigned char*)&d->live_ofs[d->num_live]);
  }
}

static frame_descr * next_frame_descr(frame_descr * d) {
  unsigned char num_allocs = 0, *p;
  uint32_t frame_size;
  CAMLassert(Retaddr_frame(d) >= 4096);
  if (d->frame_size != 0xFFFF) {
    frame_size = caml_get_frame_size(d);
    p = caml_get_end_of_live_ofs(d);
    /* Skip alloc_lengths if present */
    if (frame_size & 2) {
      num_allocs = *p;
      p += num_allocs + 1;
    }
    /* Skip debug info if present */
    if (frame_size & 1) {
      /* Align to 32 bits */
      p = Align_to(p, uint32_t);
      p += sizeof(uint32_t) * (frame_size & 2 ? num_allocs : 1);
    }
    /* Align to word size */
    p = Align_to(p, void*);
    return ((frame_descr*) p);
  } else {
    /* This marks the top of an ML stack chunk. Skip over empty
     * frame descriptor */
    /* Skip to address of zero-sized live_ofs */
    CAMLassert(d->num_live == 0);
    p = (unsigned char*)&d->live_ofs[0];
    /* Align to word size */
    p = Align_to(p, void*);
    return ((frame_descr*) p);
  }
}

static void fill_hashtable(link *frametables) {
  intnat len, j;
  intnat * tbl;
  frame_descr * d;
  uintnat h;
  link *lnk = NULL;

  iter_list(frametables,lnk) {
    tbl = (intnat*) lnk->data;
    len = *tbl;
    d = (frame_descr *)(tbl + 1);
    for (j = 0; j < len; j++) {
      h = Hash_retaddr(Retaddr_frame(d));
      while (caml_frame_descriptors[h] != NULL) {
        h = (h+1) & caml_frame_descriptors_mask;
      }
      caml_frame_descriptors[h] = d;
      d = next_frame_descr(d);
    }
  }
}

static void init_frame_descriptors(link *new_frametables)
{
  intnat tblsize, increase, i;
  link *tail = NULL;

  CAMLassert(new_frametables);

  tail = frametables_list_tail(new_frametables);
  increase = count_descriptors(new_frametables);
  tblsize = caml_frame_descriptors_mask + 1;

  /* Reallocate the caml_frame_descriptor table if it is too small */
  if(tblsize < (num_descr + increase) * 2) {

    /* Merge both lists */
    tail->next = frametables;
    frametables = NULL;

    /* [num_descr] can be less than [num_descr + increase] if frame
       tables where unregistered */
    num_descr = count_descriptors(new_frametables);

    tblsize = 4;
    while (tblsize < 2 * num_descr) tblsize *= 2;

    caml_frame_descriptors_mask = tblsize - 1;
    if(caml_frame_descriptors) caml_stat_free(caml_frame_descriptors);
    caml_frame_descriptors =
      (frame_descr **) caml_stat_alloc(tblsize * sizeof(frame_descr *));
    for (i = 0; i < tblsize; i++) caml_frame_descriptors[i] = NULL;

    fill_hashtable(new_frametables);
  } else {
    num_descr += increase;
    fill_hashtable(new_frametables);
    tail->next = frametables;
  }

  frametables = new_frametables;
}

void caml_init_frame_descriptors(void) {
  intnat i;
  link *new_frametables = NULL;
  for (i = 0; caml_frametable[i] != 0; i++)
    new_frametables = cons(caml_frametable[i],new_frametables);
  init_frame_descriptors(new_frametables);
}

void caml_register_frametable(intnat *table) {
  link *new_frametables = cons(table,NULL);
  init_frame_descriptors(new_frametables);
}

static void remove_entry(frame_descr * d) {
  uintnat i;
  uintnat r;
  uintnat j;

  i = Hash_retaddr(Retaddr_frame(d));
  while (caml_frame_descriptors[i] != d) {
    i = (i+1) & caml_frame_descriptors_mask;
  }

 r1:
  j = i;
  caml_frame_descriptors[i] = NULL;
 r2:
  i = (i+1) & caml_frame_descriptors_mask;
  // r3
  if(caml_frame_descriptors[i] == NULL) return;
  r = Hash_retaddr(Retaddr_frame(caml_frame_descriptors[i]));
  /* If r is between i and j (cyclically), i.e. if
     caml_frame_descriptors[i]->retaddr don't need to be moved */
  if(( ( j < r )  && ( r <= i ) ) ||
     ( ( i < j )  && ( j < r )  ) ||      /* i cycled, r not */
     ( ( r <= i ) && ( i < j ) )     ) {  /* i and r cycled */
    goto r2;
  }
  // r4
  caml_frame_descriptors[j] = caml_frame_descriptors[i];
  goto r1;
}

void caml_unregister_frametable(intnat *table) {
  intnat len, j;
  link *lnk;
  link *previous = frametables;
  frame_descr * d;

  len = *table;
  d = (frame_descr *)(table + 1);
  for (j = 0; j < len; j++) {
    remove_entry(d);
    d = next_frame_descr(d);
  }

  iter_list(frametables,lnk) {
    if(lnk->data == table) {
      previous->next = lnk->next;
      caml_stat_free(lnk);
      break;
    }
    previous = lnk;
  }
}

/* Communication with [caml_start_program] and [caml_call_gc]. */

intnat caml_globals_inited = 0;
static intnat caml_globals_scanned = 0;
static link * caml_dyn_globals = NULL;

/* Registration of dynamic global GC roots for natdynlink. */
void caml_register_dyn_global(void *v) {
  link *link = caml_dyn_globals;
  while (link) {
    if (link->data == v) {
      const value *exn = caml_named_value("Register_dyn_global_duplicate");
      if (exn == NULL) {
        fprintf(stderr,
          "[ocaml] attempt to add duplicate in caml_dyn_globals: %p\n", v);
        abort();
      }
      caml_raise(*exn);
    }
    link = link->next;
  }
  caml_dyn_globals = cons((void*) v,caml_dyn_globals);
}

/* Logic to determine at which index within a global root to start
   scanning.  [*glob_block] and [*start] may be updated by this function. */
static void compute_index_for_global_root_scan (value* glob_block, int* start)
{
  *start = 0;

  CAMLassert (Is_block (*glob_block));

  if (Tag_val (*glob_block) < No_scan_tag) {
    /* Note: if a [Closure_tag] block is registered as a global root
       (possibly containing one or more [Infix_tag] blocks), then only one
       out of the combined set of the [Closure_tag] and [Infix_tag] blocks
       may be registered as a global root.  Multiple registrations can cause
       the compactor to traverse the same fields of a block twice, which can
       cause a failure. */
    if (Tag_val (*glob_block) == Infix_tag)
      *glob_block -= Infix_offset_val (*glob_block);
    if (Tag_val (*glob_block) == Closure_tag)
      *start = Start_env_closinfo (Closinfo_val (*glob_block));
  }
  else {
    /* Set the index such that none of the block's fields will be scanned. */
    *start = Wosize_val (*glob_block);
  }
}

/* Call [caml_oldify_one] on (at least) all the roots that point to the minor
   heap. */
void caml_oldify_local_roots (void)
{
  intnat i, j;
  value * glob;
  value glob_block;
  int start;
  link *lnk;

  /* The global roots */
  for (i = caml_globals_scanned;
       i <= caml_globals_inited && caml_globals[i] != 0;
       i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      glob_block = *glob;
      compute_index_for_global_root_scan (&glob_block, &start);
      for (j = start; j < Wosize_val (glob_block); j++)
        Oldify (&Field (glob_block, j));
    }
  }
  caml_globals_scanned = caml_globals_inited;

  /* Dynamic global roots */
  iter_list(caml_dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      glob_block = *glob;
      compute_index_for_global_root_scan (&glob_block, &start);
      for (j = start; j < Wosize_val (glob_block); j++) {
        Oldify (&Field (glob_block, j));
      }
    }
  }

  /* Stack & local C roots */
  caml_do_local_roots_nat(NULL, &caml_oldify_one, Caml_state->bottom_of_stack,
                          Caml_state->last_return_address, Caml_state->gc_regs,
                          Caml_state->local_roots,
                          caml_get_local_arenas());

  /* Global C roots */
  caml_scan_global_young_roots(&caml_oldify_one);
  /* Finalised values */
  caml_final_oldify_young_roots ();
  /* Memprof */
  caml_memprof_oldify_young_roots ();
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(&caml_oldify_one);
}

uintnat caml_incremental_roots_count = 0;

/* Call [caml_darken] on all roots, incrementally:
   [caml_darken_all_roots_start] does the non-incremental part and
   sets things up for [caml_darken_all_roots_slice].
*/
void caml_darken_all_roots_start (void)
{
  caml_do_roots (caml_darken, 0);
}

/* Call [caml_darken] on at most [work] global roots. Return the
   amount of work not done, if any. If this is strictly positive,
   the darkening is done.
 */
intnat caml_darken_all_roots_slice (intnat work)
{
  static int i, j;
  static value *glob;
  static int do_resume = 0;
  static value glob_block;
  static int start;
  static mlsize_t roots_count = 0;
  intnat remaining_work = work;
  CAML_EV_BEGIN(EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE);

  /* If the loop was started in a previous call, resume it. */
  if (do_resume) goto resume;

  /* This is the same loop as in [caml_do_roots], but we make it
     suspend itself when [work] reaches 0. */
  for (i = 0; caml_globals[i] != 0; i++) {
    for(glob = caml_globals[i]; *glob != 0; glob++) {
      glob_block = *glob;
      compute_index_for_global_root_scan (&glob_block, &start);
      for (j = start; j < Wosize_val (glob_block); j++) {
        caml_darken (Field (glob_block, j), &Field (glob_block, j));
        -- remaining_work;
        if (remaining_work == 0){
          roots_count += work;
          do_resume = 1;
          goto suspend;
        }
      resume: ;
      }
    }
  }

  /* The loop finished normally, so all roots are now darkened. */
  caml_incremental_roots_count = roots_count + work - remaining_work;
  /* Prepare for the next run. */
  do_resume = 0;
  roots_count = 0;

 suspend:
  /* Do this in both cases. */
  CAML_EV_END(EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE);
  return remaining_work;
}

void caml_do_roots (scanning_action f, int do_globals)
{
  int i, j;
  value * glob;
  link *lnk;
  value glob_block;
  int start;

  CAML_EV_BEGIN(EV_MAJOR_ROOTS_DYNAMIC_GLOBAL);
  if (do_globals){
    /* The global roots */
    for (i = 0; caml_globals[i] != 0; i++) {
      for(glob = caml_globals[i]; *glob != 0; glob++) {
        glob_block = *glob;
        compute_index_for_global_root_scan (&glob_block, &start);
        for (j = start; j < Wosize_val (glob_block); j++)
          f (Field (glob_block, j), &Field (glob_block, j));
      }
    }
  }
  /* Dynamic global roots */
  iter_list(caml_dyn_globals, lnk) {
    for(glob = (value *) lnk->data; *glob != 0; glob++) {
      glob_block = *glob;
      compute_index_for_global_root_scan (&glob_block, &start);
      for (j = start; j < Wosize_val (glob_block); j++) {
        f (Field (glob_block, j), &Field (glob_block, j));
      }
    }
  }
  CAML_EV_END(EV_MAJOR_ROOTS_DYNAMIC_GLOBAL);
  /* The stack and local roots */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_LOCAL);
  caml_do_local_roots_nat(f, f, Caml_state->bottom_of_stack,
                          Caml_state->last_return_address, Caml_state->gc_regs,
                          Caml_state->local_roots,
                          caml_get_local_arenas());
  CAML_EV_END(EV_MAJOR_ROOTS_LOCAL);
  /* Global C roots */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_C);
  caml_scan_global_roots(f);
  CAML_EV_END(EV_MAJOR_ROOTS_C);
  /* Finalised values */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_FINALISED);
  caml_final_do_roots (f);
  CAML_EV_END(EV_MAJOR_ROOTS_FINALISED);
  /* Memprof */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_MEMPROF);
  caml_memprof_do_roots (f);
  CAML_EV_END(EV_MAJOR_ROOTS_MEMPROF);
  /* Hook */
  CAML_EV_BEGIN(EV_MAJOR_ROOTS_HOOK);
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
  CAML_EV_END(EV_MAJOR_ROOTS_HOOK);
}

/* Returns 1 if it visits an unmarked local block */
static int visit(scanning_action maj, scanning_action min,
                 value* p)
{
  value v = *p, vblock = v;
  header_t hd;
  if (!Is_block(v))
    return 0;

  if (Is_young(v)) {
    if (min != NULL) min(v, p);
    return 0;
  }

  if (!Is_in_value_area(v))
    return 0;

  /* Either major or local, distinguish by header color */

  hd = Hd_val(vblock);
  /* Compaction can create things that look like Infix_tag,
     but have color Caml_gray (cf. eptr in compact.c).
     So, check Color_hd(hd) too. */
  if (Color_hd(hd) == 0 && Tag_hd(hd) == Infix_tag) {
    vblock -= Infix_offset_val(v);
    hd = Hd_val(vblock);
  }

  if (Color_hd(hd) == Caml_black)
    return 0;

  if (Color_hd(hd) == Local_unmarked) {
    Hd_val(vblock) = With_color_hd(hd, Local_marked);
    return 1;
  }

  if (maj != NULL) maj(v, p);
  return 0;
}

static int get_local_ix(caml_local_arenas* loc, value v)
{
  int i;
  CAMLassert(Is_block(v));
  for (i = 0; i < loc->count; i++) {
    struct caml_local_arena arena = loc->arenas[i];
    if (arena.base <= (char*)v && (char*)v < arena.base + arena.length)
      return i;
  }
  caml_fatal_error("not a local value");
}

static void do_local_allocations(caml_local_arenas* loc,
                                 scanning_action maj, scanning_action min)
{
  int arena_ix;
  intnat sp;
  struct caml_local_arena arena;

  if (loc == NULL) return;
  CAMLassert(loc->count > 0);
  sp = loc->saved_sp;
  arena_ix = loc->count - 1;
  arena = loc->arenas[arena_ix];

  while (sp < 0) {
    header_t* hp = (header_t*)(arena.base + arena.length + sp), hd = *hp;
    intnat i;

    if (hd == Local_uninit_hd) {
      CAMLassert(arena_ix > 0);
      arena = loc->arenas[--arena_ix];
      continue;
    }
    if (Color_hd(hd) != Local_marked) {
      sp += Bhsize_hd(hd);
      continue;
    }
    *hp = With_color_hd(hd, Local_scanned);
    if (Tag_hd(hd) >= No_scan_tag) {
      sp += Bhsize_hd(hd);
      continue;
    }
    i = 0;
    if (Tag_hd(hd) == Closure_tag)
      i = Start_env_closinfo(Closinfo_val(Val_hp(hp)));
    for (; i < Wosize_hd(hd); i++) {
      value *p = &Field(Val_hp(hp), i);
      int marked_local = visit(maj, min, p);
      if (marked_local) {
        int ix = get_local_ix(loc, *p);
        struct caml_local_arena a = loc->arenas[ix];
        intnat newsp = (char*)*p - (a.base + a.length);
        if (sp <= newsp) {
          /* forwards pointer, common case */
          CAMLassert(ix <= arena_ix);
        } else {
          /* If backwards pointers are ever supported (e.g. local recursive
             values), then this should reset sp and iterate to a fixpoint */
          CAMLassert(ix >= arena_ix);
          caml_fatal_error("backwards local pointer");
        }
      }
    }
    sp += Bhsize_hd(hd);
  }

  /* clear marks */
  sp = loc->saved_sp;
  arena_ix = loc->count - 1;
  arena = loc->arenas[arena_ix];
#ifdef DEBUG
  { header_t* hp;
    for (hp = (header_t*)arena.base;
         hp < (header_t*)(arena.base + arena.length + sp);
         hp++) {
      *hp = Debug_free_local;
    }
  }
#endif

  while (sp < 0) {
    header_t* hp = (header_t*)(arena.base + arena.length + sp);
    if (*hp == Local_uninit_hd) {
      arena = loc->arenas[--arena_ix];
#ifdef DEBUG
      for (hp = (header_t*)arena.base;
           hp < (header_t*)(arena.base + arena.length + sp);
           hp++) {
        *hp = Debug_free_local;
      }
#endif
      continue;
    }
#ifdef DEBUG
    CAMLassert(Color_hd(*hp) != Local_marked);
    if (Color_hd(*hp) == Local_unmarked) {
      intnat i;
      for (i = 0; i < Wosize_hd(*hp); i++) {
        Field(Val_hp(hp), i) = Debug_free_local;
      }
    }
#endif
    *hp = With_color_hd(*hp, Local_unmarked);
    sp += Bhsize_hp(hp);
  }
}

void caml_do_local_roots_nat(scanning_action maj, scanning_action min,
                             char * bottom_of_stack,
                             uintnat last_retaddr, value * gc_regs,
                             struct caml__roots_block * local_roots,
                             caml_local_arenas* arenas)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  uintnat h;
  int i, j, n, ofs;
  unsigned short * p;
  value * root;
  struct caml__roots_block *lr;

  sp = bottom_of_stack;
  retaddr = last_retaddr;
  regs = gc_regs;
  if (sp != NULL) {
    while (1) {
      /* Find the descriptor corresponding to the return address */
      h = Hash_retaddr(retaddr);
      while(1) {
        d = caml_frame_descriptors[h];
        if (Retaddr_frame(d) == retaddr) break;
        h = (h+1) & caml_frame_descriptors_mask;
      }
      if (d->frame_size != 0xFFFF) {
        /* Scan the roots in this frame */
        if (d->frame_size == LONG_FRAME_MARKER) {
          /* Handle long frames */
          frame_descr_long *dl = (frame_descr_long *)d;
          uint32_t * p;
          uint32_t n;
          for (p = dl->live_ofs, n = dl->num_live; n > 0; n--, p++) {
            uint32_t ofs = *p;
            if (ofs & 1) {
              root = regs + (ofs >> 1);
            } else {
              root = (value *)(sp + ofs);
            }
            visit(maj, min, root);
          }
        } else {
          for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
            ofs = *p;
            if (ofs & 1) {
              root = regs + (ofs >> 1);
            } else {
              root = (value *)(sp + ofs);
            }
            visit(maj, min, root);
          }
        }
        /* Move to next frame */
        sp += (caml_get_frame_size(d) & 0xFFFFFFFC);
        retaddr = Saved_return_address(sp);
#ifdef Mask_already_scanned
        retaddr = Mask_already_scanned(retaddr);
#endif
      } else {
        /* This marks the top of a stack chunk for an ML callback.
           Skip C portion of stack and continue with next ML stack chunk. */
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
        /* A null sp means no more ML stack chunks; stop here. */
        if (sp == NULL) break;
      }
    }
  }
  /* Local C roots */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        root = &(lr->tables[i][j]);
        visit(maj, min, root);
      }
    }
  }
  /* Local allocations */
  do_local_allocations(arenas, maj, min);
}

uintnat (*caml_stack_usage_hook)(void) = NULL;

uintnat caml_stack_usage (void)
{
  uintnat sz;
  sz = (value *) Caml_state->top_of_stack -
       (value *) Caml_state->bottom_of_stack;
  if (caml_stack_usage_hook != NULL)
    sz += (*caml_stack_usage_hook)();
  return sz;
}
