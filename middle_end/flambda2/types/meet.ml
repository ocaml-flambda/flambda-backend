(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let meet env t1 t2 =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet env t1 t2
  else Meet_and_join.meet env t1 t2

let[@inline] meet_type () =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet_type
  else Meet_and_join.meet_type

let meet_shape env t ~shape =
  if Flambda_features.use_n_way_join ()
  then Meet_and_n_way_join.meet_shape env t ~shape
  else Meet_and_join.meet_shape env t ~shape
