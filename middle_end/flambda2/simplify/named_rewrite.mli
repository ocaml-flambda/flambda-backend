(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Nathanaëlle Courant, Guillaume Bury and Pierre Chambart, OCamlPro    *)
(*                                                                        *)
(*   Copyright 2022--2022 OCamlPro SAS                                    *)
(*   Copyright 2022--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Prim_rewrite : sig
  (** Rewrite for primitives *)
  type t = private
    | Remove_prim
    | Invalid of Flambda_kind.t
    | Replace_by_binding of
        { var : Variable.t;
          bound_to : Simple.t
        }
  (**)

  val print : Format.formatter -> t -> unit

  (** Replace the primitive by the [Invalid] primitive. *)
  val invalid : Flambda_kind.t -> t

  (** Remove the primitve (and its binding) *)
  val remove_prim : t

  (** Replace the primitive by the given [Simple.t] *)
  val replace_by_binding : var:Variable.t -> bound_to:Simple.t -> t
end

(** Named rewrites. These apply at [let_expr] constructions. *)
type t = private Prim_rewrite of Prim_rewrite.t

val print : Format.formatter -> t -> unit

val prim_rewrite : Prim_rewrite.t -> t
