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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | No_effects
  | Only_generative_effects of Mutability.t
  | Arbitrary_effects

let print ppf eff =
  match eff with
  | No_effects ->
      Format.fprintf ppf "no effects"
  | Only_generative_effects mut ->
      Format.fprintf ppf "only generative effects %a"
        Mutability.print mut
  | Arbitrary_effects ->
      Format.fprintf ppf "Arbitrary effects"

let compare eff1 eff2 =
  match eff1, eff2 with
  | No_effects, No_effects -> 0
  | No_effects, (Only_generative_effects _ | Arbitrary_effects) -> -1
  | Only_generative_effects mut1,
    Only_generative_effects mut2 ->
      Mutability.compare mut1 mut2
  | Only_generative_effects _, No_effects -> 1
  | Only_generative_effects _, Arbitrary_effects -> -1
  | Arbitrary_effects, Arbitrary_effects -> 0
  | Arbitrary_effects, (No_effects | Only_generative_effects _) -> 1

let join eff1 eff2 =
  match eff1, eff2 with
  | No_effects, No_effects
  | No_effects, Only_generative_effects _
  | No_effects, Arbitrary_effects -> eff2
  | Only_generative_effects _, No_effects -> eff1
  | Only_generative_effects mut1,
    Only_generative_effects mut2 ->
      Only_generative_effects (Mutability.join mut1 mut2)
  | Only_generative_effects _, Arbitrary_effects -> eff2
  | Arbitrary_effects, No_effects
  | Arbitrary_effects, Only_generative_effects _
  | Arbitrary_effects, Arbitrary_effects -> eff1

