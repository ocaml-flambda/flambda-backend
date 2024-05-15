(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | No_effects
  | Only_generative_effects of Mutability.t
  | Arbitrary_effects

let [@ocamlformat "disable"] print ppf eff =
  match eff with
  | No_effects ->
      Format.fprintf ppf "No_effects"
  | Only_generative_effects mut ->
      Format.fprintf ppf "Only_generative_effects(%a)"
        Mutability.print mut
  | Arbitrary_effects ->
      Format.fprintf ppf "Arbitrary_effects"

let compare eff1 eff2 =
  match eff1, eff2 with
  | No_effects, No_effects -> 0
  | No_effects, (Only_generative_effects _ | Arbitrary_effects) -> -1
  | Only_generative_effects mut1, Only_generative_effects mut2 ->
    Mutability.compare mut1 mut2
  | Only_generative_effects _, No_effects -> 1
  | Only_generative_effects _, Arbitrary_effects -> -1
  | Arbitrary_effects, Arbitrary_effects -> 0
  | Arbitrary_effects, (No_effects | Only_generative_effects _) -> 1

let join eff1 eff2 =
  match eff1, eff2 with
  | No_effects, No_effects
  | No_effects, Only_generative_effects _
  | No_effects, Arbitrary_effects ->
    eff2
  | Only_generative_effects _, No_effects -> eff1
  | Only_generative_effects mut1, Only_generative_effects mut2 ->
    Only_generative_effects (Mutability.join mut1 mut2)
  | Only_generative_effects _, Arbitrary_effects -> eff2
  | Arbitrary_effects, No_effects
  | Arbitrary_effects, Only_generative_effects _
  | Arbitrary_effects, Arbitrary_effects ->
    eff1

let from_lambda (e : Primitive.effects) : t =
  match e with
  | No_effects -> No_effects
  | Only_generative_effects -> Only_generative_effects Mutable
  (* CR-someday gyorsh: propagate mutability from attributes. Currently, it does
     not matter, because this is only used for C calls in the backend, which
     does not distinguish between Only_generative_effects and
     Arbitrary_effects. *)
  | Arbitrary_effects -> Arbitrary_effects
