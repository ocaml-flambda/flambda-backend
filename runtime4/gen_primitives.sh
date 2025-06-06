#!/bin/sh

#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# duplicated from $(ROOTDIR)/runtime/Makefile

# #8985: the meaning of character range a-z depends on the locale, so force C
#        locale throughout.
export LC_ALL=C
(
  for prim in \
      alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c intern.c \
      interp.c ints.c io.c \
      lexing.c md5.c meta.c memprof.c obj.c parsing.c signals.c str.c sys.c \
      callback.c weak.c \
      finalise.c stacks.c dynlink.c backtrace_byt.c backtrace.c afl.c \
      bigarray.c eventlog.c misc.c domain.c prng.c float32.c simd.c blake2.c \
      systhreads/st_stubs.c systhreads/st_posix.h
  do
      sed -n -e 's/^CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p' "$prim"
  done
  sed -n -e 's/^CAMLprim_int64_[0-9](\([a-z0-9_][a-z0-9_]*\)).*/caml_int64_\1\
caml_int64_\1_native/p' ints.c
) | sort | uniq
