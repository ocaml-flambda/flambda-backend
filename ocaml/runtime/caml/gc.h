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

#define Caml_white (0 << 8)
#define Caml_gray  (1 << 8)
#define Caml_blue  (2 << 8)
#define Caml_black (3 << 8)

#define Color_hd(hd) ((color_t) ((hd) & Caml_black))
#define Color_hp(hp) (Color_hd (Hd_hp (hp)))
#define Color_val(val) (Color_hd (Hd_val (val)))

#define Is_white_hd(hd) (Color_hd (hd) == Caml_white)
#define Is_gray_hd(hd) (Color_hd (hd) == Caml_gray)
#define Is_blue_hd(hd) (Color_hd (hd) == Caml_blue)
#define Is_black_hd(hd) (Color_hd (hd) == Caml_black)

#define Whitehd_hd(hd) (((hd)  & ~Caml_black)/*| Caml_white*/)
#define Grayhd_hd(hd)  (((hd)  & ~Caml_black)  | Caml_gray)
#define Blackhd_hd(hd) (((hd)/*& ~Caml_black*/)| Caml_black)
#define Bluehd_hd(hd)  (((hd)  & ~Caml_black)  | Caml_blue)

/* This depends on the layout of the header.  See [mlvalues.h]. */
#define Make_header(wosize, tag, color)                                       \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                               \
       ((header_t) (((header_t) (wosize) << 10)                               \
                    + (color)                                                 \
                    + (tag_t) (tag)))                                         \
      )

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 0d4056a108c984b74ebed35634ddd3dad4394d30
=======
>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc
#ifdef WITH_PROFINFO
#define Make_header_with_profinfo(wosize, tag, color, profinfo)               \
      (Make_header(wosize, tag, color)                                        \
        | ((((intnat) profinfo) & PROFINFO_MASK) << PROFINFO_SHIFT)           \
      )
#else
#define Make_header_with_profinfo(wosize, tag, color, profinfo) \
  Make_header(wosize, tag, color)
#endif

#define Is_white_val(val) (Color_val(val) == Caml_white)
#define Is_blue_val(val) (Color_val(val) == Caml_blue)
#define Is_black_val(val) (Color_val(val) == Caml_black)

/* For extern.c */
#define Colornum_hd(hd) ((color_t) (((hd) >> 8) & 3))
#define Coloredhd_hd(hd,colnum) (((hd) & ~Caml_black) | ((colnum) << 8))

<<<<<<< HEAD
<<<<<<< HEAD
/* Colors for locally allocated values.
   (Only used during root-scanning, never visible to the rest of the GC) */
#define Local_marked Caml_black
#define Local_unmarked Caml_blue /* allocation color of local objects */
#define Local_scanned Caml_gray

#define Is_stack(blk) (Is_block(blk) && Color_hd(Hd_val(blk)) == Local_unmarked)

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
  intnat saved_sp;
  intnat next_length;
  struct caml_local_arena arenas[Max_local_arenas];
} caml_local_arenas;

#define With_color_hd(hd, color) \
  (((hd) & ~Caml_black) | color)

/* Neither a valid header nor value */
#define Local_uninit_hd Make_header(0, 0x42, Local_unmarked)

#endif /* CAML_INTERNALS */
=======
=======
>>>>>>> c3b2b912cfac7d208d5daafaf044062285c3037a
#define Make_header_with_reserved(wosize, tag, color, reserved)      \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                      \
       ((header_t) (Hd_reserved(reserved))                           \
                    + ((header_t) (wosize) << HEADER_WOSIZE_SHIFT)   \
                    + (color) /* colors are pre-shifted */           \
                    + (tag_t) (tag)))
<<<<<<< HEAD


#define Make_header(wosize, tag, color) \
        Make_header_with_reserved(wosize, tag, color, 0)
>>>>>>> db638e1ef1d923c67cd7142850e6693243f6cbfa
=======
#ifdef CAML_INTERNALS


=======
#ifdef CAML_INTERNALS


>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc
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
  intnat saved_sp;
  intnat next_length;
  struct caml_local_arena arenas[Max_local_arenas];
} caml_local_arenas;

/* Colors for locally allocated values.
   (Only used during root-scanning, never visible to the rest of the GC) */
#define Local_marked Caml_black
#define Local_unmarked Caml_blue /* allocation color of local objects */
#define Local_scanned Caml_gray

#define With_color_hd(hd, color) \
  (((hd) & ~Caml_black) | color)

/* Neither a valid header nor value */
#define Local_uninit_hd Make_header(0, 0x42, Local_unmarked)

#endif /* CAML_INTERNALS */
<<<<<<< HEAD
>>>>>>> 0d4056a108c984b74ebed35634ddd3dad4394d30
=======


#define Make_header(wosize, tag, color) \
        Make_header_with_reserved(wosize, tag, color, 0)
>>>>>>> c3b2b912cfac7d208d5daafaf044062285c3037a
=======
>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc

#endif /* CAML_GC_H */
