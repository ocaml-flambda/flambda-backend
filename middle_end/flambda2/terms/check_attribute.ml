(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Property = struct
  type t = Zero_alloc

  let print ppf = function Zero_alloc -> Format.fprintf ppf "zero_alloc"

  let equal x y = match x, y with Zero_alloc, Zero_alloc -> true

  let from_lambda : Lambda.property -> t = function Zero_alloc -> Zero_alloc
end

type t =
  | Default_check
  | Ignore_assert_all of Property.t
  | Check of
      { property : Property.t;
        strict : bool;
        assume : bool;
        loc : Location.t
      }

let print ppf t =
  match t with
  | Default_check -> ()
  | Ignore_assert_all property ->
    Format.fprintf ppf "@[ignore %a@]" Property.print property
  | Check { property; strict; assume; loc = _ } ->
    Format.fprintf ppf "@[%s%s %a@]"
      (if assume then "assume" else "assert")
      (if strict then " strict" else "")
      Property.print property

let from_lambda : Lambda.check_attribute -> t = function
  | Default_check -> Default_check
  | Ignore_assert_all p -> Ignore_assert_all (Property.from_lambda p)
  | Check { property; strict; assume; loc } ->
    Check { property = Property.from_lambda property; strict; assume; loc }

let equal x y =
  match x, y with
  | Default_check, Default_check -> true
  | Ignore_assert_all p1, Ignore_assert_all p2 -> Property.equal p1 p2
  | ( Check { property = p1; strict = s1; assume = a1; loc = loc1 },
      Check { property = p2; strict = s2; assume = a2; loc = loc2 } ) ->
    Property.equal p1 p2 && Bool.equal s1 s2 && Bool.equal a1 a2 && loc1 = loc2
  | (Default_check | Ignore_assert_all _ | Check _), _ -> false

let is_default : t -> bool = function
  | Default_check -> true
  | Ignore_assert_all _ | Check _ -> false

let assume_zero_alloc t =
  match t with
  | Default_check | Ignore_assert_all _ -> false
  | Check { property = Zero_alloc; strict = _; assume; loc = _ } -> assume
