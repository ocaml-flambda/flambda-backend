(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type reason =
  | At_toplevel
  | Contain_static_consts

type cost =
  { size_of_primitives : int;
    sets_of_closures_num : int
  }

type t =
  | Can_specialize of cost
  | Cannot_specialize of { reason : reason }

let [@ocamlformat "disable"] print_reason ppf reason =
  match reason with
  | At_toplevel ->
    Format.fprintf ppf "at_top_level"
  | Contain_static_consts ->
    Format.fprintf ppf "contain_static_consts"

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Can_specialize { size_of_primitives; sets_of_closures_num; } ->
    Format.fprintf ppf "@[<hov>(can_specialize@ \
        @[<hov 1>(size_of_primitives@ %d)@]@ \
        @[<hov 1>(sets_of_closures_num@ %d)@]\
      )@]"
      size_of_primitives
      sets_of_closures_num
  | Cannot_specialize { reason } ->
    Format.fprintf ppf "@[<hov>(cannot_specialize@ \
        @[<hov 1>(reason@ %a)@]\
      )@]"
    print_reason reason

(* Creations *)

let can_specialize =
  Can_specialize { size_of_primitives = 0; sets_of_closures_num = 0 }

let cannot_specialize reason = Cannot_specialize { reason }

(* Updating costs *)

let update_cost ~f = function
  | Can_specialize cost -> Can_specialize (f cost)
  | Cannot_specialize _ as res -> res

let add_prim ~size t =
  update_cost t ~f:(fun ({ size_of_primitives = s; _ } as cost) ->
      { cost with size_of_primitives = size + s })

let add_set_of_closure ~num t =
  update_cost t ~f:(fun ({ sets_of_closures_num = n; _ } as cost) ->
      { cost with sets_of_closures_num = n + num })
