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

#ifndef CAML_FLOAT32_H
#define CAML_FLOAT32_H

#include "mlvalues.h"

#define Float32_val(v) (*((float *) Data_custom_val(v)))

CAMLextern value caml_copy_float32(float);

#endif /* CAML_FLOAT32_H */
