(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Id = Named_rewrite_id

(* Rewriting of primitives. Currently this is only used by the mutable
   unboxing (aka ref-to-var) analysis, and these rewrite are applied when
   going upwards and rebuilding expressions. *)
module Prim_rewrite = struct

  type t =
    | Remove_prim
    | Replace_by_binding of {
        var : Variable.t;
        bound_to : Simple.t
      }

  let print ppf = function
    | Remove_prim -> Format.fprintf ppf "Remove_prim"
    | Replace_by_binding { var; bound_to } ->
      Format.fprintf ppf "Replace_by_binding { %a = %a}"
        Variable.print var Simple.print bound_to

  let remove_prim = Remove_prim

  let replace_by_binding ~var ~bound_to =
    Replace_by_binding { var; bound_to; }

end

(* We currently only rewrite primitives *)
type t =
  | Prim_rewrite of Prim_rewrite.t

let print ppf = function
  | Prim_rewrite prim_rewrite -> Prim_rewrite.print ppf prim_rewrite

let prim_rewrite prim_rewrite = Prim_rewrite prim_rewrite

