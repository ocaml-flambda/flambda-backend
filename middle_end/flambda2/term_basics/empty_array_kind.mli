(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                       Guillaume Bury, OCamlPro SAS                     *)
(*                                                                        *)
(*   Copyright 2017--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Values_or_immediates_or_naked_floats  (** Traditional OCaml arrays. *)
  | Naked_int32s
  | Naked_int64s
  | Naked_nativeints
      (** Arrays of unboxed numbers, with a slightly different runtime
          representation. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val of_element_kind : Flambda_kind.t -> t

val of_lambda : Lambda.array_kind -> t
