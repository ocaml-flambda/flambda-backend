/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2006 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Stack backtrace for uncaught exceptions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/backtrace_prim.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/stack.h"

/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
frame_descr * caml_next_frame_descriptor(uintnat * pc, char ** sp)
{
  frame_descr * d;
  uintnat h;

  while (1) {
    h = Hash_retaddr(*pc);
    while (1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) return NULL; /* happens if some code compiled without -g */
      if (Retaddr_frame(d) == *pc) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    /* Skip to next frame */
    if (d->frame_size != 0xFFFF) {
      /* Regular frame, update sp/pc and return the frame descriptor */
      *sp += (caml_get_frame_size(d) & 0xFFFFFFFC);
      *pc = Saved_return_address(*sp);
#ifdef Mask_already_scanned
      *pc = Mask_already_scanned(*pc);
#endif
      return d;
    } else {
      /* Special frame marking the top of a stack chunk for an ML callback.
         Skip C portion of stack and continue with next ML stack chunk. */
      struct caml_context * next_context = Callback_link(*sp);
      *sp = next_context->bottom_of_stack;
      *pc = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
      if (*sp == NULL) return NULL;
    }
  }
}

int caml_alloc_backtrace_buffer(void){
  CAMLassert(Caml_state->backtrace_pos == 0);
  Caml_state->backtrace_buffer =
    caml_stat_alloc_noexc(BACKTRACE_BUFFER_SIZE * sizeof(backtrace_slot));
  if (Caml_state->backtrace_buffer == NULL) return -1;
  return 0;
}

/* Stores the return addresses contained in the given stack fragment
   into the backtrace array ; this version is performance-sensitive as
   it is called at each [raise] in a program compiled with [-g], so we
   preserved the global, statically bounded buffer of the old
   implementation -- before the more flexible
   [caml_get_current_callstack] was implemented. */
void caml_stash_backtrace(value exn, uintnat pc, char * sp, char * trapsp)
{
  if (exn != Caml_state->backtrace_last_exn) {
    Caml_state->backtrace_pos = 0;
    Caml_state->backtrace_last_exn = exn;
  }

  if (Caml_state->backtrace_buffer == NULL &&
      caml_alloc_backtrace_buffer() == -1)
    return;

  /* iterate on each frame  */
  while (1) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) return;
    /* store its descriptor in the backtrace buffer */
    if (Caml_state->backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
    Caml_state->backtrace_buffer[Caml_state->backtrace_pos++] =
      (backtrace_slot) descr;

    /* Stop when we reach the current exception handler */
    if (sp > trapsp) return;
  }
}

/* A backtrace_slot is either a debuginfo or a frame_descr* */
#define Slot_is_debuginfo(s) ((uintnat)(s) & 2)
#define Debuginfo_slot(s) ((debuginfo)((uintnat)(s) - 2))
#define Slot_debuginfo(d) ((backtrace_slot)((uintnat)(d) + 2))
#define Frame_descr_slot(s) ((frame_descr*)(s))
#define Slot_frame_descr(f) ((backtrace_slot)(f))
static debuginfo debuginfo_extract(frame_descr* d, int alloc_idx);

#define Default_callstack_size 32
intnat caml_collect_current_callstack(value** ptrace, intnat* plen,
                                      intnat max_frames, int alloc_idx)
{
  uintnat pc = Caml_state->last_return_address;
  char * sp = Caml_state->bottom_of_stack;
  intnat trace_pos = 0;

  if (max_frames <= 0) return 0;
  if (*plen == 0) {
    value* trace =
      caml_stat_alloc_noexc(Default_callstack_size * sizeof(value));
    if (trace == NULL) return 0;
    *ptrace = trace;
    *plen = Default_callstack_size;
  }

  if (alloc_idx >= 0) {
    /* First frame has a Comballoc selector */
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    debuginfo info;
    if (descr == NULL) return 0;
    info = debuginfo_extract(descr, alloc_idx);
    if (info != NULL) {
      CAMLassert(((uintnat)info & 3) == 0);
      (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_debuginfo(info));
    } else {
      (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_frame_descr(descr));
    }
  }

  while (trace_pos < max_frames) {
    frame_descr * descr = caml_next_frame_descriptor(&pc, &sp);
    if (descr == NULL) break;
    CAMLassert(((uintnat)descr & 3) == 0);
    if (trace_pos == *plen) {
      intnat new_len = *plen * 2;
      value * trace = caml_stat_resize_noexc(*ptrace, new_len * sizeof(value));
      if (trace == NULL) break;
      *ptrace = trace;
      *plen = new_len;
    }
    (*ptrace)[trace_pos++] = Val_backtrace_slot(Slot_frame_descr(descr));
  }

  return trace_pos;
}

static debuginfo debuginfo_extract(frame_descr* d, int alloc_idx)
{
  unsigned char* infoptr;
  uint32_t debuginfo_offset;
  uint32_t frame_size;

  /* The special frames marking the top of an ML stack chunk are never
     returned by caml_next_frame_descriptor, so should never reach here. */
  CAMLassert(d->frame_size != 0xFFFF);
  frame_size = caml_get_frame_size(d);

  if ((frame_size & 1) == 0) {
    return NULL;
  }
  /* Recover debugging info */
  infoptr = caml_get_end_of_live_ofs(d);
  if (frame_size & 2) {
    CAMLassert(alloc_idx == -1 || (0 <= alloc_idx && alloc_idx < *infoptr));
    /* skip alloc_lengths */
    infoptr += *infoptr + 1;
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    /* select the right debug info for this allocation */
    if (alloc_idx != -1) {
      infoptr += alloc_idx * sizeof(uint32_t);
      if (*(uint32_t*)infoptr == 0) {
        /* No debug info for this particular allocation */
        return NULL;
      }
    } else {
      /* We don't care which alloc_idx we use, so use the first
         that has debug info. (e.g. this is a backtrace through a
         finaliser/signal handler triggered via a Comballoc alloc) */
      while (*(uint32_t*)infoptr == 0) {
        infoptr += sizeof(uint32_t);
      }
    }
  } else {
    /* align to 32 bits */
    infoptr = Align_to(infoptr, uint32_t);
    CAMLassert(alloc_idx == -1);
  }
  debuginfo_offset = *(uint32_t*)infoptr;
  CAMLassert(debuginfo_offset != 0 && (debuginfo_offset & 3) == 0);
  return (debuginfo)(infoptr + debuginfo_offset);
}

debuginfo caml_debuginfo_extract(backtrace_slot slot)
{
  if (Slot_is_debuginfo(slot)) {
    /* already a decoded debuginfo */
    return Debuginfo_slot(slot);
  } else {
    return debuginfo_extract(Frame_descr_slot(slot), -1);
  }
}

debuginfo caml_debuginfo_next(debuginfo dbg)
{
  uint32_t * infoptr;

  if (dbg == NULL)
    return NULL;

  infoptr = dbg;
  if ((infoptr[0] & 1) == 0)
    /* No next debuginfo */
    return NULL;
  else
    /* Next debuginfo is after the two packed info fields */
    return (debuginfo*)(infoptr + 2);
}

/* Multiple names may share the same filename,
   so it is referenced as an offset instead of stored inline */
struct name_info {
  int32_t filename_offs;
  char name[1];
};

/* Extended version of name_info including location fields which didn't fit
   in the main debuginfo word. */
struct name_and_loc_info {
  int32_t filename_offs;
  uint16_t start_chr;
  uint16_t end_chr;
  int32_t end_offset; /* End character position relative to start bol */
  char name[1];
};

/* Extract location information for the given frame descriptor */
void caml_debuginfo_location(debuginfo dbg, /*out*/ struct caml_loc_info * li)
{
  uint32_t info1, info2;

  /* If no debugging information available, print nothing.
     When everything is compiled with -g, this corresponds to
     compiler-inserted re-raise operations. */
  if (dbg == NULL) {
    li->loc_valid = 0;
    li->loc_is_raise = 1;
    li->loc_is_inlined = 0;
    return;
  }
  /* Recover debugging info */
  info1 = ((uint32_t *)dbg)[0];
  info2 = ((uint32_t *)dbg)[1];
  /* Format of the two info words:
     Two possible formats based on value of bit 63:
     Partially packed format
       |------------- info2 ------------||------------- info1 -------------|
       1 lllllllllllllllllll mmmmmmmmmmmmmmmmmm ffffffffffffffffffffffff k n
      63                  44                 26                        2 1 0
     Fully packed format:
       |-------------- info2 --------------||------------- info1 -------------|
       0 llllllllllll mmm aaaaaa bbbbbbb ooooooooo ffffffffffffffffffffffff k n
      63           51  48     42      35        26                        2 1 0
     n (    1 bit ): 0 if this is the final debuginfo
                     1 if there's another following this one
     k (    1 bit ): 0 if it's a call
                     1 if it's a raise
     f (   24 bits): offset (in 4-byte words) of struct relative to dbg. For
                     partially packed format, f is struct name_and_loc_info;
                     for fully packed format, f is struct name_info.
     m ( 17/3 bits): difference between start line and end line
     o (  0/9 bits): difference between start bol and end bol
     a (  0/6 bits): beginning of character range (relative to start bol)
     b (  0/7 bits): end of character range (relative to end bol)
     l (19/12 bits): start line number
   */
  li->loc_valid = 1;
  li->loc_is_raise = (info1 & 2) == 2;
  li->loc_is_inlined = caml_debuginfo_next(dbg) != NULL;
  if (info2 & 0x80000000) {
    struct name_and_loc_info * name_and_loc_info =
      (struct name_and_loc_info*)((char *) dbg + (info1 & 0x3FFFFFC));
    li->loc_defname = name_and_loc_info->name;
    li->loc_filename =
      (char *)name_and_loc_info + name_and_loc_info->filename_offs;
    li->loc_start_lnum = li->loc_end_lnum = (info2 >> 12) & 0x7FFFF;
    li->loc_end_lnum += ((info2 & 0xFFF) << 6) | (info1 >> 26);
    li->loc_start_chr = name_and_loc_info->start_chr;
    li->loc_end_chr = name_and_loc_info->end_chr;
    li->loc_end_offset = name_and_loc_info->end_offset;
  } else {
    struct name_info * name_info =
      (struct name_info*)((char *) dbg + (info1 & 0x3FFFFFC));
    li->loc_defname = name_info->name;
    li->loc_filename =
      (char *)name_info + name_info->filename_offs;
    li->loc_start_lnum = li->loc_end_lnum = info2 >> 19;
    li->loc_end_lnum += (info2 >> 16) & 0x7;
    li->loc_start_chr = (info2 >> 10) & 0x3F;
    li->loc_end_chr = li->loc_end_offset = (info2 >> 3) & 0x7F;
    li->loc_end_offset += (((info2 & 0x7) << 6) | (info1 >> 26));
  }
}

value caml_add_debug_info(backtrace_slot start, value size, value events)
{
  return Val_unit;
}

value caml_remove_debug_info(backtrace_slot start)
{
  return Val_unit;
}

int caml_debug_info_available(void)
{
  return 1;
}

int caml_debug_info_status(void)
{
  return 1;
}
