(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

val create :
  prim:Flambda_primitive.t -> comparison_results:t Variable.Map.t -> t option

val print : Format.formatter -> t -> unit

val convert_result_compared_to_tagged_zero :
  t -> _ Flambda_primitive.comparison -> Flambda_primitive.t
