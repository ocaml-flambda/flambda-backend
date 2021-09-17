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

(* Effects and coeffects *)

(** A pair of an effect and a coeffect. *)
type t = Effects.t * Coeffects.t

(** Print *)
val print : Format.formatter -> t -> unit

(** Comparison. *)
val compare : t -> t -> int

(** The value stating that no effects of coeffects take place. This is exactly
    [No_effects, No_coeffects]. *)
val pure : t

(** The value stating that any effects and/or coeffects may take place. This is
    exactly [Arbitrary_effects, Has_coeffects]. *)
val all : t

(** The value stating that a read (i.e only a coeffect) takes place. This is
    [No_effects, Has_coeffects]. *)
val read : t

(** Join two effects and coeffects. *)
val join : t -> t -> t
