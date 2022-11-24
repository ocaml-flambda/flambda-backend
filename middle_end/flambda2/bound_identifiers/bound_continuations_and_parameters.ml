(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = {
  conts : Bound_continuations.t ;
  params : Bound_parameters.t ;
}

let params t = t.params

let print ppf { conts ; params } =
  Format.fprintf ppf "%a@ %a"
    Bound_continuations.print conts
    Bound_parameters.print params

let create conts params = { conts ; params }

let free_names { conts ; params } =
  Name_occurrences.union
    (Bound_continuations.free_names conts)
    (Bound_parameters.free_names params)

let apply_renaming { conts ; params } renaming =
  {
    conts = Bound_continuations.apply_renaming conts renaming ;
    params = Bound_parameters.apply_renaming params renaming
  }

let ids_for_export { conts ; params } =
  Ids_for_export.union
    (Bound_continuations.ids_for_export conts)
    (Bound_parameters.ids_for_export params)

let rename { conts ; params } =
  {
    conts = Bound_continuations.rename conts ;
    params = Bound_parameters.rename params
  }

let renaming t ~guaranteed_fresh:t2 =
  Renaming.compose
    ~first:(Bound_continuations.renaming t.conts ~guaranteed_fresh:t2.conts)
    ~second:(Bound_parameters.renaming t.params ~guaranteed_fresh:t2.params)
