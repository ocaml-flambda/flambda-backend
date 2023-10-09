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
  | Assume of
      { property : Property.t;
        strict : bool;
        never_returns_normally : bool;
        loc : Location.t
      }
  | Check of
      { property : Property.t;
        strict : bool;
        loc : Location.t
      }

let print ppf t =
  match t with
  | Default_check -> ()
  | Ignore_assert_all property ->
    Format.fprintf ppf "@[ignore %a@]" Property.print property
  | Assume { property; strict; never_returns_normally; loc = _ } ->
    Format.fprintf ppf "@[assume_%a%s%s@]" Property.print property
      (if strict then "_strict" else "")
      (if never_returns_normally then "_never_returns_normally" else "")
  | Check { property; strict; loc = _ } ->
    Format.fprintf ppf "@[assert_%a%s@]" Property.print property
      (if strict then "_strict" else "")

let from_lambda : Lambda.check_attribute -> t = function
  | Default_check -> Default_check
  | Ignore_assert_all p -> Ignore_assert_all (Property.from_lambda p)
  | Assume { property; strict; never_returns_normally; loc } ->
    Assume
      { property = Property.from_lambda property;
        strict;
        never_returns_normally;
        loc
      }
  | Check { property; strict; loc } ->
    Check { property = Property.from_lambda property; strict; loc }

let equal x y =
  match x, y with
  | Default_check, Default_check -> true
  | Ignore_assert_all p1, Ignore_assert_all p2 -> Property.equal p1 p2
  | ( Check { property = p1; strict = s1; loc = loc1 },
      Check { property = p2; strict = s2; loc = loc2 } ) ->
    Property.equal p1 p2 && Bool.equal s1 s2 && Location.compare loc1 loc2 = 0
  | ( Assume
        { property = p1; strict = s1; never_returns_normally = n1; loc = loc1 },
      Assume
        { property = p2; strict = s2; never_returns_normally = n2; loc = loc2 }
    ) ->
    Property.equal p1 p2 && Bool.equal s1 s2 && Bool.equal n1 n2
    && Location.compare loc1 loc2 = 0
  | (Default_check | Ignore_assert_all _ | Check _ | Assume _), _ -> false

let is_default : t -> bool = function
  | Default_check -> true
  | Ignore_assert_all _ | Check _ | Assume _ -> false
