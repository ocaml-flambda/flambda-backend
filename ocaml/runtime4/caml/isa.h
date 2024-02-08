/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2024 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_ISA_H
#define CAML_ISA_H

#include "mlvalues.h"

#ifdef CAML_INTERNALS
CAMLextern void caml_assert_arch_extensions(void);
extern uintnat caml_skip_arch_extension_check;
#endif

#endif /* CAML_ISA_H */
