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

#ifndef CAML_GC_H
#define CAML_GC_H


#include "mlvalues.h"

/* This depends on the layout of the header.  See [mlvalues.h]. */

#define Make_header_with_reserved(wosize, tag, color, reserved)      \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                      \
       ((header_t) (Hd_reserved(reserved))                           \
                    + ((header_t) (wosize) << HEADER_WOSIZE_SHIFT)   \
                    + (color) /* colors are pre-shifted */           \
                    + (tag_t) (tag)))


#define Make_header(wosize, tag, color) \
        Make_header_with_reserved(wosize, tag, color, 0)

#ifdef CAML_INTERNALS


#define Init_local_arena_bsize 4096

/* We allow the local stack to quadruple 19 times, which is virtually infinite.
   Hardware limit will probably hit first (either out of address space on 32bit
   systems, or out of physical memory on 64bit)

   19 is the biggest number without triggering some compiler errors about
   integer overflow during shifting; I don't know if overflow would actually
   happen if I make the number bigger, but 19 corresponds to 1024TB and should
   be sufficient for a very long time. */
#define Max_local_arenas 19

struct caml_local_arena {
  char* base;
  uintnat length;
  void* alloc_block;
};
typedef struct caml_local_arenas {
  int count;
  intnat next_length;
  struct caml_local_arena arenas[Max_local_arenas];
} caml_local_arenas;

/* Neither a valid header nor value */
#define Local_uninit_hd Make_header(0, 0x42, NOT_MARKABLE)

#endif /* CAML_INTERNALS */

#endif /* CAML_GC_H */
