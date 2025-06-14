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

type t =
  | Normal
  | Phantom
  | In_types

type name_mode = t

(* Semilattice:
 *
 *   Normal
 *      |
 *      |
 *   Phantom
 *      |
 *      |
 *   In_types
 *)

let join_in_terms t1 t2 =
  match t1, t2 with
  | Normal, Normal | Phantom, Phantom -> t1
  | Normal, Phantom | Phantom, Normal -> Normal
  | In_types, _ | _, In_types ->
    Misc.fatal_error "Cannot use [join_in_terms] with [In_types] mode"

let normal = Normal

let in_types = In_types

let phantom = Phantom

let is_normal t = match t with Normal -> true | In_types | Phantom -> false

let is_phantom t = match t with Phantom -> true | In_types | Normal -> false

let is_in_types t = match t with In_types -> true | Normal | Phantom -> false

let can_be_in_terms t =
  match t with Normal | Phantom -> true | In_types -> false

let compare_partial_order t1 t2 =
  match t1, t2 with
  | Normal, Normal | Phantom, Phantom | In_types, In_types -> Some 0
  | Normal, (Phantom | In_types) -> Some 1
  | (Phantom | In_types), Normal -> Some (-1)
  | Phantom, In_types -> Some 1
  | In_types, Phantom -> Some (-1)

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Normal -> Format.pp_print_string ppf "Normal"
    | In_types -> Format.pp_print_string ppf "In_types"
    | Phantom -> Format.pp_print_string ppf "Phantom"

  let hash _ = Misc.fatal_error "Name_mode.hash not yet implemented"

  let compare t1 t2 =
    (* This is a linear extension of the ordering used by
       [compare_partial_order], above. *)
    match t1, t2 with
    | Normal, Normal | Phantom, Phantom | In_types, In_types -> 0
    | Normal, (Phantom | In_types) -> 1
    | (Phantom | In_types), Normal -> -1
    | Phantom, In_types -> 1
    | In_types, Phantom -> -1

  let equal t1 t2 = compare t1 t2 = 0
end)

let compare_total_order = compare

let compare _ _ = `Be_explicit_about_total_or_partial_ordering

module Or_absent = struct
  type t =
    | Absent
    | Present of name_mode

  let absent = Absent

  let present name_mode = Present name_mode

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
      | Present name_mode ->
        Format.fprintf ppf "@[<hov 1>(Present@ %a)@]" print name_mode

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Absent, Absent -> 0
      | Absent, Present _ -> -1
      | Present _, Absent -> 1
      | Present name_mode1, Present name_mode2 ->
        compare_total_order name_mode1 name_mode2

    let equal t1 t2 = compare t1 t2 = 0
  end)

  let compare _ _ = `Be_explicit_about_total_or_partial_ordering

  let compare_partial_order t1 t2 =
    match t1, t2 with
    | Absent, Absent -> Some 0
    | Absent, Present _ -> Some (-1)
    | Present _, Absent -> Some 1
    | Present name_mode1, Present name_mode2 ->
      compare_partial_order name_mode1 name_mode2

  let join_in_terms t1 t2 =
    match t1, t2 with
    | Absent, Absent -> Absent
    | Absent, Present _ -> t2
    | Present _, Absent -> t1
    | Present name_mode1, Present name_mode2 ->
      Present (join_in_terms name_mode1 name_mode2)
end
