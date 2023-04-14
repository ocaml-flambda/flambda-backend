(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2023 OCamlPro SAS                                    *)
(*   Copyright 2014--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Component = struct
  type t = Singleton of Flambda_kind.With_subkind.t

  let equal_ignoring_subkinds t1 t2 =
    match t1, t2 with
    | Singleton kind1, Singleton kind2 ->
      Flambda_kind.With_subkind.equal_ignoring_subkind kind1 kind2

  let equal_exact t1 t2 =
    match t1, t2 with
    | Singleton kind1, Singleton kind2 ->
      Flambda_kind.With_subkind.equal kind1 kind2

  let print ~product_above:_ ppf t =
    match t with Singleton kind -> Flambda_kind.With_subkind.print ppf kind
end

type t = Component.t list

let nullary = []

let create t = List.map (fun kind -> Component.Singleton kind) t

let to_list t = List.map (fun (Component.Singleton kind) -> kind) t

let print ppf t =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list (Component.print ~product_above:true)
       ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} "))
    t

let equal_ignoring_subkinds t1 t2 =
  List.equal Component.equal_ignoring_subkinds t1 t2

let equal_exact t1 t2 = List.equal Component.equal_exact t1 t2

let is_singleton_value t =
  match t with
  | [Component.Singleton kind]
    when Flambda_kind.equal
           (Flambda_kind.With_subkind.kind kind)
           Flambda_kind.value ->
    true
  | [] | Component.Singleton _ :: _ -> false

let cardinal t = List.length t
