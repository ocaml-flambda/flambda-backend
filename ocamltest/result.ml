(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of test-result related types and functions *)

type status = Pass | Skip | Fail | Predicate of bool
(* CR xclerc/mshinwell: maybe [Predicate] should be split and put under
   [Pass] and [Skip]? *)

type t = {
  status : status;
  reason : string option
}

let result_of_status s = { status = s; reason = None }

let pass = result_of_status Pass

let skip = result_of_status Skip

let fail = result_of_status Fail

let result_with_reason s r = { status = s; reason = Some r }

let pass_with_reason r = result_with_reason Pass r

let skip_with_reason r = result_with_reason Skip r

let fail_with_reason r = result_with_reason Fail r

let predicate_satisfied_with_reason r =
  result_with_reason (Predicate true) r

let predicate_not_satisfied_with_reason r =
  result_with_reason (Predicate false) r

let string_of_status = function
  | Pass -> "=> passed"
  | Skip -> "=> skipped"
  | Fail -> "=> failed"
  | Predicate true -> "=> predicate is true"
  | Predicate false -> "=> predicate is false"

let string_of_reason = function
  | None -> ""
  | Some reason -> (" (" ^ reason ^ ")")

let string_of_result r =
  match r.status with
  | Pass
  | Skip
  | Fail ->
    (string_of_status r.status) ^ (string_of_reason r.reason)
  | Predicate b ->
    Printf.sprintf "=> predicate \"%s\" is %s"
      (Option.value r.reason ~default:"<unknown>")
      (if b then "satisfied" else "not satisfied")

let is_pass r =
  match r.status with
  | Pass | Predicate true -> true
  | Skip
  | Fail
  | Predicate false -> false

let is_skip r =
  match r.status with
  | Skip | Predicate false -> true
  | Pass
  | Fail
  | Predicate true -> false

let is_fail r =
  match r.status with
  | Fail -> true
  | Pass
  | Skip
  | Predicate true
  | Predicate false -> false
