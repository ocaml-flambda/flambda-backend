/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1998 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* CR ocaml 5 domains: This file is the 4.x version together with
   adjustments to the names of exported functions ("unix_" -> "caml_unix").
   (mshinwell/xclerc).
   For multi-domain support we'll need to revisit this.
*/

#define CAML_INTERNALS

#include <errno.h>
#include <signal.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "caml/unixsupport.h"

#ifndef NSIG
#define NSIG 64
#endif

#ifdef POSIX_SIGNALS

static void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

static value encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i) > 0) {
        value newcons = caml_alloc_small(2, 0);
        Field(newcons, 0) = Val_int(caml_rev_convert_signal_number(i));
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigprocmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

CAMLprim value caml_unix_sigprocmask(value vaction, value vset)
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigprocmask_cmd[Int_val(vaction)];
  decode_sigset(vset, &set);
  caml_enter_blocking_section();
<<<<<<< HEAD
#ifdef CAML_RUNTIME_5
  // Differs from upstream at the point we branched, but this PR
  // changes the behaviour to what we have here:
  // https://github.com/ocaml/ocaml/pull/12743
  retcode = pthread_sigmask(how, &set, &oldset);
#else
  retcode = caml_sigmask_hook(how, &set, &oldset);
#endif
||||||| 121bedcfd2
  retcode = sigprocmask(how, &set, &oldset);
=======
  retcode = pthread_sigmask(how, &set, &oldset);
>>>>>>> 5.2.0
  caml_leave_blocking_section();
  /* Run any handlers for just-unmasked pending signals */
  caml_process_pending_actions();
  if (retcode != 0) caml_unix_error(retcode, "sigprocmask", Nothing);
  return encode_sigset(&oldset);
}

CAMLprim value caml_unix_sigpending(value unit)
{
  sigset_t pending;
  int i;
  if (sigpending(&pending) == -1) caml_uerror("sigpending", Nothing);
  for (i = 1; i < NSIG; i++)
    if(caml_pending_signals[i])
      sigaddset(&pending, i);
  return encode_sigset(&pending);
}

CAMLprim value caml_unix_sigsuspend(value vset)
{
  sigset_t set;
  int retcode;
  decode_sigset(vset, &set);
  caml_enter_blocking_section();
  retcode = sigsuspend(&set);
  caml_leave_blocking_section();
  if (retcode == -1 && errno != EINTR) caml_uerror("sigsuspend", Nothing);
  return Val_unit;
}

#else

CAMLprim value caml_unix_sigprocmask(value vaction, value vset)
{ caml_invalid_argument("Unix.sigprocmask not available"); }

CAMLprim value caml_unix_sigpending(value unit)
{ caml_invalid_argument("Unix.sigpending not available"); }

CAMLprim value caml_unix_sigsuspend(value vset)
{ caml_invalid_argument("Unix.sigsuspend not available"); }

#endif
