(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Guillaume Bury and Nathanaëlle Courant, OCamlPro                    *)
(*                                                                        *)
(*   Copyright 2024--2024 OCamlPro SAS                                    *)
(*   Copyright 2024--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = 'a Apply_cont_rewrite_id.Map.t Continuation.Map.t

let print pp = Continuation.Map.print (Apply_cont_rewrite_id.Map.print pp)

let empty = Continuation.Map.empty

let find cont id t =
  let rewrite_id_map = Continuation.Map.find cont t in
  Apply_cont_rewrite_id.Map.find id rewrite_id_map

let add cont id v t =
  Continuation.Map.update cont
    (fun rewrite_id_map ->
      let rewrite_map =
        Option.value rewrite_id_map ~default:Apply_cont_rewrite_id.Map.empty
      in
      Some
        (Apply_cont_rewrite_id.Map.update id
           (function
             | None -> Some v
             | Some _ ->
               Misc.fatal_errorf "Continuation_callsite_map.add: duplicate add")
           rewrite_map))
    t
