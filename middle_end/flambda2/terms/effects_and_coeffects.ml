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

type t = Effects.t * Coeffects.t * Placement.t

let [@ocamlformat "disable"] print fmt (eff, coeff, dup) =
  Format.fprintf fmt "%a * %a * %a" Effects.print eff Coeffects.print coeff Placement.print dup

let compare (e1, c1, d1) (e2, c2, d2) =
  match Effects.compare e1 e2 with
  | 0 -> begin
    match Coeffects.compare c1 c2 with
    | 0 -> Placement.compare d1 d2
    | res -> res
  end
  | res -> res

(* Some useful constants *)
let pure : t = No_effects, No_coeffects, Strict

let pure_duplicatable : t = No_effects, No_coeffects, Delay

let all : t = Arbitrary_effects, Has_coeffects, Strict

let read : t = No_effects, Has_coeffects, Strict

(* Joining effects and coeffects *)
let join (eff1, coeff1, dup1) (eff2, coeff2, dup2) =
  Effects.join eff1 eff2, Coeffects.join coeff1 coeff2, Placement.join dup1 dup2
