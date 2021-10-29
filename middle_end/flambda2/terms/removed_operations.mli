(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val zero : t

val call : t

val branch : t

val prim : Flambda_primitive.t -> t

val alloc : t

val direct_call_of_indirect : t

val specialized_poly_compare : t

val ( + ) : t -> t -> t

val print : Format.formatter -> t -> unit

val evaluate : args:Inlining_arguments.t -> t -> float

val equal : t -> t -> bool
