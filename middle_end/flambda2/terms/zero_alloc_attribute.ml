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

type t =
  | Default_check
  | Assume of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check of
      { strict : bool;
        loc : Location.t
      }

let print ppf t =
  match t with
  | Default_check -> ()
  | Assume { strict; never_returns_normally; never_raises; loc = _ } ->
    Format.fprintf ppf "@[assume_zero_alloc%s%s%s@]"
      (if strict then "_strict" else "")
      (if never_returns_normally then "_never_returns_normally" else "")
      (if never_raises then "_never_raises" else "")
  | Check { strict; loc = _ } ->
    Format.fprintf ppf "@[assert_zero_alloc%s@]"
      (if strict then "_strict" else "")

let from_lambda : Lambda.zero_alloc_attribute -> t =
 fun a ->
  match a with
  | Default_zero_alloc -> Default_check
  | Assume { strict; never_returns_normally; never_raises; loc; } ->
    Assume { strict; never_returns_normally; never_raises; loc }
  | Check { strict; loc; } ->
    Check { strict; loc }

let equal x y =
  match x, y with
  | Default_check, Default_check -> true
  | Check { strict = s1; loc = loc1 }, Check { strict = s2; loc = loc2 } ->
    Bool.equal s1 s2 && Location.compare loc1 loc2 = 0
  | ( Assume
        { strict = s1;
          never_returns_normally = n1;
          never_raises = r1;
          loc = loc1
        },
      Assume
        { strict = s2;
          never_returns_normally = n2;
          never_raises = r2;
          loc = loc2
        } ) ->
    Bool.equal s1 s2 && Bool.equal n1 n2 && Bool.equal r1 r2
    && Location.compare loc1 loc2 = 0
  | (Default_check | Check _ | Assume _), _ -> false

let is_default : t -> bool = function
  | Default_check -> true
  | Check _ | Assume _ -> false
