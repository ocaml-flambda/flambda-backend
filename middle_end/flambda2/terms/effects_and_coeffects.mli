(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Effects, coeffects and placements *)

(** A triple of an effect, a coeffect, and a placement. *)
type t = Effects.t * Coeffects.t * Placement.t

(** Print *)
val print : Format.formatter -> t -> unit

(** Comparison. *)
val compare : t -> t -> int

(** The value stating that no effects or coeffects take place, with a strict
    placement. This is exactly [No_effects, No_coeffects, Strict]. *)
val pure : t

(** The value stating that no effects of coeffects take place, and that the
    expression can be moved and duplicated if needed. This is exactly
    [No_effects, No_coeffects, Delay]. *)
val pure_can_be_duplicated : t

(** The value stating that any effects and/or coeffects may take place (with
    strict placement). This is exactly [Arbitrary_effects, Has_coeffects,
    Strict]. *)
val all : t

(** The value stating that a read (i.e only a coeffect) takes place (with strict
    placement). This is [No_effects, Has_coeffects, Strict]. *)
val read : t

(** Join two effects, coeffects and placements. *)
val join : t -> t -> t
