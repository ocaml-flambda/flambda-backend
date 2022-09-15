(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

val simple_is_my_closure : DA.t -> Simple.t -> bool

val update_dacc_for_my_closure_use_simple : DA.t -> Simple.t -> DA.t

val update_dacc_for_my_closure_use_list : DA.t -> Simple.t list -> DA.t

val update_dacc_for_my_closure_use_prim : DA.t -> P.t -> DA.t
