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

/* Operations on objects */

#include <string.h>
#include "caml/camlatomic.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/prims.h"
#include "caml/signals.h"

static int obj_tag (value arg)
{
  header_t hd;

  if (arg == Val_null) {
    return 1010;   /* null_tag */
  } else if (Is_long (arg)) {
    return 1000;   /* int_tag */
  } else if ((long) arg & (sizeof (value) - 1)) {
    return 1002;   /* unaligned_tag */
  } else {
    /* The acquire load ensures that reading the field of a Forward_tag
       block in stdlib/camlinternalLazy.ml:force_gen has the necessary
       synchronization. */
    hd = (header_t)atomic_load_acquire(Hp_atomic_val(arg));
    return Tag_hd(hd);
  }
}

CAMLprim value caml_obj_tag(value arg)
{
  return Val_int (obj_tag(arg));
}

CAMLprim value caml_obj_raw_field(value arg, value pos)
{
  /* Represent field contents as a native integer */
  return caml_copy_nativeint((intnat) Field(arg, Long_val(pos)));
}

CAMLprim value caml_obj_set_raw_field(value arg, value pos, value bits)
{
  Field(arg, Long_val(pos)) = (value) Nativeint_val(bits);
  return Val_unit;
}

CAMLprim value caml_obj_make_forward(value blk, value fwd)
{
  caml_modify(&Field(blk, 0), fwd);
  Tag_val (blk) = Forward_tag;
  return Val_unit;
}

CAMLprim value caml_get_header(value blk)
{
  // undefined behaviour if blk is not a block
  intnat r = Hd_val(blk);
  return caml_copy_nativeint(r);
}

/* [size] is a value encoding a number of blocks */
CAMLprim value caml_obj_block(value tag, value size)
{
  value res;
  mlsize_t sz;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);

  /* When [Scannable_tag(tg)], [caml_alloc] returns an object whose fields are
   * initialised to [Val_unit]. Otherwise, the fields are uninitialised. We aim
   * to avoid inconsistent states in other cases, on a best-effort basis --
   * by default there is no initialization. */
  switch (tg) {
  default: {
      res = caml_alloc(sz, tg);
      break;
  }
  case Abstract_tag:
  case Double_tag:
  case Double_array_tag: {
    /* In these cases, the initial content is irrelevant,
       no specific initialization needed. */
    res = caml_alloc(sz, tg);
    break;
  }
  case Closure_tag: {
    /* [Closure_tag] is below [no_scan_tag], but closures have more
       structure with in particular a "closure information" that
       indicates where the environment starts. We initialize this to
       a sane value, as it may be accessed by runtime functions. */
    /* Closinfo_val is the second field, so we need size at least 2 */
    if (sz < 2) caml_invalid_argument ("Obj.new_block");
    res = caml_alloc(sz, tg);
    Closinfo_val(res) = Make_closinfo(0, 2, 1); /* does not allocate */
    break;
  }
  case String_tag: {
    /* For [String_tag], the initial content does not matter. However,
       the length of the string is encoded using the last byte of the
       block. For this reason, the blocks with [String_tag] cannot be
       of size [0]. We initialise the last byte to [0] such that the
       length returned by [String.length] and [Bytes.length] is
       a non-negative number. */
    if (sz == 0) caml_invalid_argument ("Obj.new_block");
    res = caml_alloc(sz, tg);
    Field (res, sz - 1) = 0;
    break;
  }
  case Custom_tag: {
    /* It is difficult to correctly use custom objects allocated
       through [Obj.new_block], so we disallow it completely. The
       first field of a custom object must contain a valid pointer to
       a block of custom operations. Without initialisation, hashing,
       finalising or serialising this custom object will lead to
       crashes.  See #9513 for more details. */
    caml_invalid_argument ("Obj.new_block");
  }
  }

  return res;
}

CAMLprim value caml_obj_with_tag(value new_tag_v, value arg)
{
  CAMLparam2 (new_tag_v, arg);
  CAMLlocal1 (res);
  mlsize_t sz, i;
  tag_t tag_for_alloc;
  uintnat infix_offset = 0;

  // This function must not raise exceptions (except asynchronous exceptions).
  // It behaves just like an allocation function, which has generative effects,
  // but not arbitrary effects.  (See the [Obj_dup] case in [To_cmm].)

  tag_t new_tag = (tag_t)Long_val(new_tag_v);
#ifdef DEBUG
  tag_t existing_tag = Tag_val(arg);
#endif

  CAMLassert (
    !(
      (existing_tag == Closure_tag || existing_tag == Infix_tag
       || new_tag == Closure_tag || new_tag == Infix_tag)
      && existing_tag != new_tag));
  // Cannot change tags of existing closures or create new closures using
  // [caml_obj_with_tag].

  if (new_tag == Infix_tag) {
    // If we received an infix block, we must return the same; but the whole
    // Closure_tag block has to be copied.
    infix_offset = Infix_offset_val(arg);
    arg -= infix_offset;
    tag_for_alloc = Closure_tag;
    CAMLassert(Tag_val(arg) == tag_for_alloc);
  } else {
    tag_for_alloc = new_tag;
  }

  sz = Wosize_val(arg);
  if (sz == 0) {
    CAMLassert(new_tag != Infix_tag);
    CAMLreturn (Atom(tag_for_alloc));
  }

  if (!Scannable_tag(tag_for_alloc)) {
    res = caml_alloc(sz, tag_for_alloc);
    memcpy(Bp_val(res), Bp_val(arg), sz * sizeof(value));
  } else if (sz <= Max_young_wosize) {
    reserved_t reserved = Reserved_val(arg);
    res = caml_alloc_small_with_reserved(sz, tag_for_alloc, reserved);
    for (i = 0; i < sz; i++) Field(res, i) = Field(arg, i);
  } else {
    mlsize_t scannable_sz = Scannable_wosize_val(arg);
    reserved_t reserved = Reserved_val(arg);

    res = caml_alloc_shr_reserved(sz, tag_for_alloc, reserved);

    CAMLassert(tag_for_alloc != Infix_tag);
    if (tag_for_alloc == Closure_tag) {
      // The portion prior to the scannable environment may contain code
      // pointers, infix tags, infix tagged zero padding and unboxed numbers.
      // The latter in particular must not be copied using [caml_initialize],
      // as they might satisfy [Is_young].

      mlsize_t start_of_scannable_env = Start_env_closinfo(Closinfo_val(arg));

      // There is always at least one function slot in a closure block at
      // the moment.
      CAMLassert(start_of_scannable_env >= 2);

      // These two can be equal when there is no scannable environment.
      CAMLassert(start_of_scannable_env <= scannable_sz);

      for (i = 0; i < start_of_scannable_env; i++) {
        Field(res, i) = Field(arg, i);
      }
    } else {
      i = 0;
    }

    // Copy scannable values (for closures, this is only the scannable
    // environment).
    for (; i < scannable_sz; i++) {
      caml_initialize(&Field(res, i), Field(arg, i));
    }

    // Copy any non-scannable flat suffix of a mixed block.
    for (; i < sz; i++) {
      Field(res, i) = Field(arg, i);
    }

    /* Give gc a chance to run, and run memprof callbacks */
    caml_process_pending_actions();
  }

  res += infix_offset;
  CAMLassert(infix_offset == 0 || Tag_val(res) == Infix_tag);

  CAMLreturn (res);
}

CAMLprim value caml_obj_dup(value arg)
{
  if (!Is_block(arg)) return arg;
  return caml_obj_with_tag(Val_long(Tag_val(arg)), arg);
}

CAMLprim value caml_obj_add_offset (value v, value offset)
{
  return v + (unsigned long) Int32_val (offset);
}

CAMLprim value caml_obj_compare_and_swap (value v, value f,
                                          value oldv, value newv)
{
  int res = caml_atomic_cas_field(v, Int_val(f), oldv, newv);
  caml_check_urgent_gc(Val_unit);
  return Val_int(res);
}

CAMLprim value caml_obj_is_shared (value obj)
{
  return Val_int(Is_long(obj) || !Is_young(obj));
}

/* The following functions are used to support lazy values. They are not
 * written in OCaml in order to ensure atomicity guarantees with respect to the
 * GC. */
CAMLprim value caml_lazy_make_forward (value v)
{
  CAMLparam1 (v);
  CAMLlocal1 (res);

  res = caml_alloc_small (1, Forward_tag);
  Field (res, 0) = v;
  CAMLreturn (res);
}

static int obj_update_tag (value blk, int old_tag, int new_tag)
{
  header_t hd;
  tag_t tag;

  SPIN_WAIT {
    hd = Hd_val(blk);
    tag = Tag_hd(hd);

    if (tag != old_tag) return 0;
    if (caml_domain_alone()) {
      Unsafe_store_tag_val(blk, new_tag);
      return 1;
    }

    if (atomic_compare_exchange_strong(Hp_atomic_val(blk), &hd,
                                       Hd_with_tag(hd, new_tag)))
      return 1;
  }
}

CAMLprim value caml_lazy_reset_to_lazy (value v)
{
  CAMLassert (Tag_val(v) == Forcing_tag);

  obj_update_tag (v, Forcing_tag, Lazy_tag);
  return Val_unit;
}

CAMLprim value caml_lazy_update_to_forward (value v)
{
  CAMLassert (Tag_val(v) == Forcing_tag);

  obj_update_tag (v, Forcing_tag, Forward_tag);
  return Val_unit;
}

CAMLprim value caml_lazy_read_result (value v)
{
  if (obj_tag(v) == Forward_tag)
    return Field(v,0);
  return v;
}

CAMLprim value caml_lazy_update_to_forcing (value v)
{
  if (Is_block(v) && /* Needed to ensure that we don't attempt to update the
                        header of a integer value */
      obj_update_tag (v, Lazy_tag, Forcing_tag)) {
    return Val_int(0);
  } else {
    return Val_int(1);
  }
}

CAMLprim value caml_obj_is_stack (value v)
{
  return Val_int(caml_is_stack(v));
}

/* For mlvalues.h and camlinternalOO.ml
   See also GETPUBMET in interp.c
 */

CAMLprim value caml_get_public_method (value obj, value tag)
{
  value meths = Field (obj, 0);
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) {
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  /* return 0 if tag is not there */
  return (tag == Field(meths,li) ? Field (meths, li-1) : 0);
}

/* Allocate OO ids in chunks, to avoid contention */
#define Id_chunk 1024

static atomic_uintnat oo_next_id;

CAMLprim value caml_fresh_oo_id (value v) {
  if (Caml_state->oo_next_id_local % Id_chunk == 0) {
    Caml_state->oo_next_id_local =
      atomic_fetch_add(&oo_next_id, Id_chunk);
  }
  v = Val_long(Caml_state->oo_next_id_local++);
  return v;
}

CAMLprim value caml_set_oo_id (value obj) {
  value v = Val_unit;
  Field(obj, 1) = caml_fresh_oo_id(v);
  return obj;
}

CAMLprim value caml_int_as_pointer (value n) {
  return n - 1;
}

/* Compute how many words in the heap are occupied by blocks accessible
   from a given value */

#define ENTRIES_PER_QUEUE_CHUNK 4096
struct queue_chunk {
  struct queue_chunk *next;
  value entries[ENTRIES_PER_QUEUE_CHUNK];
};

/* Return 0 for uniform blocks and 1+n for a mixed block with scannable prefix
   len n.
 */
CAMLprim value caml_succ_scannable_prefix_len (value v) {
#ifdef NATIVE_CODE
  return Val_long(Reserved_val(v));
#else
  reserved_t reserved = Reserved_val(v);
  if (reserved == Faux_mixed_block_sentinel) {
    return Val_long(Scannable_wosize_val(v) + 1);
  } else {
    return Val_long(0);
  }
#endif /* NATIVE_CODE */
}

CAMLprim value caml_is_null(value v)
{
  return v == Val_null ? Val_true : Val_false;
}
