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
  type t = Noalloc

  let print ppf = function Noalloc -> Format.fprintf ppf "noalloc"

  let equal x y = match x, y with Noalloc, Noalloc -> true

  let from_lambda : Lambda.property -> t = function Noalloc -> Noalloc
end

type t =
  | Default_check
  | Assert of Property.t * Debuginfo.t
  | Assume of Property.t * Debuginfo.t

let print ppf (t:t) =
  match t with
  | Default_check -> ()
  | Assert (p,_) -> Format.fprintf ppf "@[assert %a@]" Property.print p
  | Assume (p,_) -> Format.fprintf ppf "@[assume %a@]" Property.print p

let from_lambda : Lambda.check_attribute -> t = function
  | Default_check -> Default_check
  | Assert (p,loc) -> Assert ((Property.from_lambda p), Debuginfo.from_location loc)
  | Assume (p,loc) -> Assume ((Property.from_lambda p), Debuginfo.from_location loc)

let equal x y =
  match x, y with
  | Default_check, Default_check -> true
  | Assert (p1,dbg1), Assert (p2,dbg2) | Assume (p1,dbg1), Assume (p2,dbg2) ->
    Property.equal p1 p2 && Debuginfo.compare dbg1 dbg2 = 0
  | (Default_check | Assert _ | Assume _), _ -> false

let is_default : t -> bool = function
  | Default_check -> true
  | Assert _ | Assume _ -> false
