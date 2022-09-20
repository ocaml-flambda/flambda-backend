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

let simple_is_my_closure dacc simple =
  Simple.pattern_match' simple
    ~const:(fun _ -> false)
    ~symbol:(fun _ ~coercion:_ -> false)
    ~var:(fun v ~coercion:_ ->
      let closure_info = DE.closure_info (DA.denv dacc) in
      match closure_info with
      | Closure { my_closure; _ } -> Variable.equal v my_closure
      | Not_in_a_closure -> false
      | In_a_set_of_closures_but_not_yet_in_a_specific_closure -> false)
