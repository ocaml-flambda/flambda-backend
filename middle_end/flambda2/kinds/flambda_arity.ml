(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Flambda_kind.With_subkind.t list

let create t = t

let to_list t = t

let compare_ignoring_subkinds t1 t2 =
  List.compare Flambda_kind.With_subkind.compare
    (List.map Flambda_kind.With_subkind.erase_subkind t1)
    (List.map Flambda_kind.With_subkind.erase_subkind t2)

let equal_ignoring_subkinds t1 t2 = compare_ignoring_subkinds t1 t2 = 0

let print ppf t =
  match t with
  | [] -> Format.pp_print_string ppf "Nullary"
  | _ ->
    Format.fprintf ppf "@[%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
         Flambda_kind.With_subkind.print)
      t

let is_singleton_value t =
  match t with
  | [kind]
    when Flambda_kind.equal
           (Flambda_kind.With_subkind.kind kind)
           Flambda_kind.value ->
    true
  | _ -> false

let cardinal t = List.length t

let nullary = []
