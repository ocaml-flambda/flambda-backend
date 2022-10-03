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

/* Callbacks from C to OCaml */

#include <string.h>
#include "caml/callback.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#ifndef NATIVE_CODE

/* Bytecode callbacks */

#include "caml/codefrag.h"
#include "caml/interp.h"
#include "caml/instruct.h"
#include "caml/fix_code.h"
#include "caml/stacks.h"

CAMLexport int caml_callback_depth = 0;

static opcode_t callback_code[] = { ACC, 0, APPLY, 0, POP, 1, STOP };

static int callback_code_inited = 0;

static void init_callback_code(void)
{
  caml_register_code_fragment((char *) callback_code,
                              (char *) callback_code + sizeof(callback_code),
                              DIGEST_IGNORE, NULL);
#ifdef THREADED_CODE
  caml_thread_code(callback_code, sizeof(callback_code));
#endif
  callback_code_inited = 1;
}

/* Functions that return all exceptions, including asynchronous ones */

static value caml_callbackN_exn0(value closure, int narg, value args[],
                                 int *returning_async_exn)
{
  int i;
  value res;

  CAMLassert(narg + 4 <= 256);

  Caml_state->extern_sp -= narg + 4;
  for (i = 0; i < narg; i++) Caml_state->extern_sp[i] = args[i]; /* arguments */
  Caml_state->extern_sp[narg] = (value)(callback_code + 4); /* return address */
  Caml_state->extern_sp[narg + 1] = Val_unit;    /* environment */
  Caml_state->extern_sp[narg + 2] = Val_long(0); /* extra args */
  Caml_state->extern_sp[narg + 3] = closure;
  if (!callback_code_inited) init_callback_code();
  callback_code[1] = narg + 3;
  callback_code[3] = narg;
  res = caml_interprete(callback_code, sizeof(callback_code),
    returning_async_exn);
  if (Is_exception_result(res)) Caml_state->extern_sp += narg + 4; /* PR#3419 */

  return res;
}

CAMLexport value caml_callbackN_exn(value closure, int narg, value args[])
{
  return caml_callbackN_exn0(closure, narg, args, NULL);
}

CAMLexport value caml_callback_exn(value closure, value arg1)
{
  value arg[1];
  arg[0] = arg1;
  return caml_callbackN_exn0(closure, 1, arg, NULL);
}

CAMLexport value caml_callback2_exn(value closure, value arg1, value arg2)
{
  value arg[2];
  arg[0] = arg1;
  arg[1] = arg2;
  return caml_callbackN_exn0(closure, 2, arg, NULL);
}

CAMLexport value caml_callback3_exn(value closure,
                                    value arg1, value arg2, value arg3)
{
  value arg[3];
  arg[0] = arg1;
  arg[1] = arg2;
  arg[2] = arg3;
  return caml_callbackN_exn0(closure, 3, arg, NULL);
}

/* Functions that propagate all exceptions, with any asynchronous exceptions
   also being propagated asynchronously. */

CAMLexport value caml_callbackN(value closure, int narg, value args[])
{
  value res;
  int returning_async_exn;

  res = caml_callbackN_exn0(closure, narg, args, &returning_async_exn);
  if (Is_exception_result(res)) {
    value exn = Extract_exception(res);
    if (returning_async_exn) caml_raise_async(exn);
    else caml_raise(exn);
  }

  return res;
}

CAMLexport value caml_callback(value closure, value arg1)
{
  value arg[1];
  arg[0] = arg1;
  return caml_callbackN(closure, 1, arg);
}

CAMLexport value caml_callback2(value closure, value arg1, value arg2)
{
  value arg[2];
  arg[0] = arg1;
  arg[1] = arg2;
  return caml_callbackN(closure, 2, arg);
}

CAMLexport value caml_callback3(value closure,
                                value arg1, value arg2, value arg3)
{
  value arg[3];
  arg[0] = arg1;
  arg[1] = arg2;
  arg[2] = arg3;
  return caml_callbackN(closure, 3, arg);
}

#else

/* Native-code callbacks. */

typedef value (callback_stub)(caml_domain_state* state, value closure,
                              value* args, int catch_async_exns);

callback_stub caml_callback_asm, caml_callback2_asm, caml_callback3_asm;

/* First we build functions that allow control as to whether an asynchronous
   exception trap frame is installed. */

static value callback(value closure, value arg, int catch_async_exns)
{
  return caml_callback_asm(Caml_state, closure, &arg, catch_async_exns);
}

static value callback2(value closure, value arg1, value arg2,
                       int catch_async_exns)
{
  value args[] = {arg1, arg2};
  return caml_callback2_asm(Caml_state, closure, args, catch_async_exns);
}

static value callback3(value closure, value arg1, value arg2, value arg3,
                       int catch_async_exns)
{
  value args[] = {arg1, arg2, arg3};
  return caml_callback3_asm(Caml_state, closure, args, catch_async_exns);
}

static value callbackN(value closure, int narg, value args[],
                       int catch_async_exns)
{
  CAMLparam1 (closure);
  CAMLxparamN (args, narg);
  CAMLlocal1 (res);
  int i;

  res = closure;
  for (i = 0; i < narg; /*nothing*/) {
    /* Pass as many arguments as possible */
    switch (narg - i) {
    case 1:
      res = callback(res, args[i], catch_async_exns);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 1;
      break;
    case 2:
      res = callback2(res, args[i], args[i + 1], catch_async_exns);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 2;
      break;
    default:
      res = callback3(res, args[i], args[i + 1], args[i + 2], catch_async_exns);
      if (Is_exception_result(res)) CAMLreturn (res);
      i += 3;
      break;
    }
  }
  CAMLreturn (res);
}

/* Functions that return all exceptions, including asynchronous ones */

CAMLexport value caml_callback_exn(value closure, value arg)
{
  return callback(closure, arg, 1);
}

CAMLexport value caml_callback2_exn(value closure, value arg1, value arg2)
{
  return callback2(closure, arg1, arg2, 1);
}

CAMLexport value caml_callback3_exn(value closure,
                                    value arg1, value arg2, value arg3)
{
  return callback3(closure, arg1, arg2, arg3, 1);
}

CAMLexport value caml_callbackN_exn(value closure, int narg, value args[])
{
  return callbackN(closure, narg, args, 1);
}

/* Functions that propagate all exceptions, with any asynchronous exceptions
   also being propagated asynchronously.  In these cases we do not install
   an asynchronous exception trap frame, avoiding any need to perform
   comparisons on exception values to determine if they represent asynchronous
   exceptions (which would be problematic for [Sys.Break], thus likely
   necessitating these being wrapped in another constructor). */

static value raise_if_exception(value res)
{
  if (Is_exception_result(res)) caml_raise(Extract_exception(res));
  return res;
}

CAMLexport value caml_callback(value closure, value arg)
{
  return raise_if_exception(callback(closure, arg, 0));
}

CAMLexport value caml_callback2(value closure, value arg1, value arg2)
{
  return raise_if_exception(callback2(closure, arg1, arg2, 0));
}

CAMLexport value caml_callback3(value closure,
                                value arg1, value arg2, value arg3)
{
  return raise_if_exception(callback3(closure, arg1, arg2, arg3, 0));
}

CAMLexport value caml_callbackN (value closure, int narg, value args[])
{
  return raise_if_exception(callbackN(closure, narg, args, 0));
}

#endif

/* Naming of OCaml values */

struct named_value {
  value val;
  struct named_value * next;
  char name[1];
};

#define Named_value_size 13

static struct named_value * named_value_table[Named_value_size] = { NULL, };

static unsigned int hash_value_name(char const *name)
{
  unsigned int h;
  for (h = 0; *name != 0; name++) h = h * 19 + *name;
  return h % Named_value_size;
}

CAMLprim value caml_register_named_value(value vname, value val)
{
  struct named_value * nv;
  const char * name = String_val(vname);
  size_t namelen = strlen(name);
  unsigned int h = hash_value_name(name);

  for (nv = named_value_table[h]; nv != NULL; nv = nv->next) {
    if (strcmp(name, nv->name) == 0) {
      caml_modify_generational_global_root(&nv->val, val);
      return Val_unit;
    }
  }
  nv = (struct named_value *)
          caml_stat_alloc(sizeof(struct named_value) + namelen);
  memcpy(nv->name, name, namelen + 1);
  nv->val = val;
  nv->next = named_value_table[h];
  named_value_table[h] = nv;
  caml_register_generational_global_root(&nv->val);
  return Val_unit;
}

CAMLexport const value * caml_named_value(char const *name)
{
  struct named_value * nv;
  for (nv = named_value_table[hash_value_name(name)];
       nv != NULL;
       nv = nv->next) {
    if (strcmp(name, nv->name) == 0) return &nv->val;
  }
  return NULL;
}

CAMLexport void caml_iterate_named_values(caml_named_action f)
{
  int i;
  for(i = 0; i < Named_value_size; i++){
    struct named_value * nv;
    for (nv = named_value_table[i]; nv != NULL; nv = nv->next) {
      f( &nv->val, nv->name );
    }
  }
}
