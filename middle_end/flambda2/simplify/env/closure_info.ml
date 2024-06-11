(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_in_a_closure
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure
  | Closure of
      { code_id : Code_id.t;
        return_continuation : Continuation.t;
        exn_continuation : Continuation.t;
        my_closure : Variable.t;
        stub : bool
      }

let [@ocamlformat "disable"] print ppf = function
  | Not_in_a_closure ->
    Format.fprintf ppf "not_in_a_closure"
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    Format.fprintf ppf "in_a_set_of_closures"
  | Closure { code_id; return_continuation; exn_continuation; my_closure; stub } ->
    Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(code_id@ %a)@]@ \
      @[<hov 1>(return_continuation@ %a)@]@ \
      @[<hov 1>(exn_continuation@ %a)@]@ \
      @[<hov 1>(my_closure@ %a)@]@ \
      @[<hov 1>(stub@ %b)@]\
      )@]"
      Code_id.print code_id
      Continuation.print return_continuation
      Continuation.print exn_continuation
      Variable.print my_closure
      stub

let not_in_a_closure = Not_in_a_closure

let in_a_set_of_closures =
  In_a_set_of_closures_but_not_yet_in_a_specific_closure

let in_a_closure code_id ~return_continuation ~exn_continuation ~my_closure
    ~stub =
  Closure { code_id; return_continuation; exn_continuation; my_closure; stub }

let is_stub = function
  | Not_in_a_closure | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    false
  | Closure { stub; _ } -> stub

type in_or_out_of_closure =
  | In_a_closure
  | Not_in_a_closure

let in_or_out_of_closure (t : t) : in_or_out_of_closure =
  match t with
  | Not_in_a_closure -> Not_in_a_closure
  | Closure _ -> In_a_closure
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    Misc.fatal_errorf
      "Expected to be either inside or outside a closure, but not \
       [In_a_set_of_closures_but_not_yet_in_a_specific_closure]"
