/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Stephen Dolan, University of Cambridge               */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_DOMAIN_H
#define CAML_DOMAIN_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef CAML_INTERNALS

#include "domain_state.h"

void caml_init_domain(void);

/* OCaml 5 stdlib compatibility hooks */
extern value (*caml_hook_mutex_new)(value unit);
extern value (*caml_hook_mutex_lock)(value wrapper);
extern value (*caml_hook_mutex_unlock)(value wrapper);
extern value (*caml_hook_mutex_try_lock)(value wrapper);
extern value (*caml_hook_condition_new)(value unit);
extern value (*caml_hook_condition_wait)(value wcond, value wmut);
extern value (*caml_hook_condition_signal)(value wrapper);
extern value (*caml_hook_condition_broadcast)(value wrapper);

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_DOMAIN_H */
