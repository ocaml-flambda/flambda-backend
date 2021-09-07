(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Normal | Phantom | In_types

(* Semilattice:
 *
 *         Normal
 *       /      \
 *      /        \
 *  In_types   Phantom
 *)

let max_in_terms t1 t2 =
  match t1, t2 with
  | Normal, Normal | Phantom, Phantom -> t1
  | Normal, Phantom | Phantom, Normal -> Normal
  | In_types, _ | _, In_types ->
    Misc.fatal_error "Cannot use [max_in_terms] with [In_types] mode"

type kind = t

let normal = Normal

let in_types = In_types

let phantom = Phantom

(* The integer ordering is used by [Name_occurrences] and must agree with the
   partial ordering below. *)
(* CR mshinwell: add unit test for this, and also that the total ordering below
   is a linear extension of the partial ordering. *)

let of_int i =
  match i with
  | 0 -> Phantom
  | 1 -> In_types
  | 2 -> Normal
  | _ -> Misc.fatal_errorf "Name_mode.of_int %d" i

let to_int t = match t with Phantom -> 0 | In_types -> 1 | Normal -> 2

let max_to_int = 2

let is_normal t = match t with Normal -> true | In_types | Phantom -> false

let is_phantom t = match t with Phantom -> true | In_types | Normal -> false

let min_in_types = In_types

let min_in_terms = Phantom

let top = Normal

let can_be_in_terms t =
  match t with Normal | Phantom -> true | In_types -> false

let compare_partial_order t1 t2 =
  match t1, t2 with
  | Normal, Normal | Phantom, Phantom | In_types, In_types -> Some 0
  | Normal, (Phantom | In_types) -> Some 1
  | (Phantom | In_types), Normal -> Some (-1)
  | Phantom, In_types | In_types, Phantom -> None

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Normal -> Format.pp_print_string ppf "Normal"
    | In_types -> Format.pp_print_string ppf "In_types"
    | Phantom -> Format.pp_print_string ppf "Phantom"

  let output _ _ = Misc.fatal_error "Name_mode.output not yet implemented"

  let hash _ = Misc.fatal_error "Name_mode.hash not yet implemented"

  let compare t1 t2 =
    (* This is a linear extension of the ordering used by
       [compare_partial_order], above. *)
    match t1, t2 with
    | Normal, Normal | Phantom, Phantom | In_types, In_types -> 0
    | Normal, (Phantom | In_types) -> 1
    | (Phantom | In_types), Normal -> -1
    | Phantom, In_types -> -1
    | In_types, Phantom -> 1

  let equal t1 t2 = compare t1 t2 = 0
end)

let compare_total_order = compare

let compare _ _ = `Be_explicit_about_total_or_partial_ordering

module Or_absent = struct
  type t = Absent | Present of kind

  let absent = Absent

  let present kind = Present kind

  let is_present = function Absent -> false | Present _ -> true

  let is_present_as_normal = function
    | Absent -> false
    | Present Normal -> true
    | Present (Phantom | In_types) -> false

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Absent -> Format.pp_print_string ppf "Absent"
      | Present kind ->
        Format.fprintf ppf "@[<hov 1>(Present@ %a)@]" print kind

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Absent, Absent -> 0
      | Absent, Present _ -> -1
      | Present _, Absent -> 1
      | Present kind1, Present kind2 -> compare_total_order kind1 kind2

    let equal t1 t2 = compare t1 t2 = 0
  end)

  let compare_total_order = compare

  let compare _ _ = `Be_explicit_about_total_or_partial_ordering

  let compare_partial_order t1 t2 =
    match t1, t2 with
    | Absent, Absent -> Some 0
    | Absent, Present _ -> Some (-1)
    | Present _, Absent -> Some 1
    | Present kind1, Present kind2 -> compare_partial_order kind1 kind2
end

type descr = t = Normal | Phantom | In_types

let descr t = t
