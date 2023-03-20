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
  type _ t =
    | Singleton : Flambda_kind.With_subkind.t -> [> `Unarized] t
    | Unboxed_product : _ t list -> [> `Complex] t

  let rec equal_ignoring_subkinds : type uc1 uc2. uc1 t -> uc2 t -> bool =
   fun t1 t2 ->
    match t1, t2 with
    | Singleton kind1, Singleton kind2 ->
      Flambda_kind.With_subkind.equal
        (Flambda_kind.With_subkind.erase_subkind kind1)
        (Flambda_kind.With_subkind.erase_subkind kind2)
    | Unboxed_product ts1, Unboxed_product ts2 ->
      Misc.Stdlib.List.equal equal_ignoring_subkinds ts1 ts2
    | Singleton _, Unboxed_product _ | Unboxed_product _, Singleton _ -> false

  let rec equal_exact : type uc1 uc2. uc1 t -> uc2 t -> bool =
   fun t1 t2 ->
    match t1, t2 with
    | Singleton kind1, Singleton kind2 ->
      Flambda_kind.With_subkind.equal kind1 kind2
    | Unboxed_product ts1, Unboxed_product ts2 ->
      Misc.Stdlib.List.equal equal_exact ts1 ts2
    | Singleton _, Unboxed_product _ | Unboxed_product _, Singleton _ -> false

  let rec print : type uc. Format.formatter -> uc t -> unit =
   fun ppf t ->
    match t with
    | Singleton kind -> Flambda_kind.With_subkind.print ppf kind
    | Unboxed_product [] -> Format.pp_print_string ppf "void"
    | Unboxed_product ts ->
      Format.fprintf ppf "@[<hov 1>%t#%t(%a)@]" Flambda_colours.unboxed_product
        Flambda_colours.pop
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
           print)
        ts

  let rec unarize : type uc. uc t -> Flambda_kind.With_subkind.t list =
   fun t ->
    match t with
    | Singleton kind -> [kind]
    | Unboxed_product [] -> []
    | Unboxed_product ts -> List.concat_map unarize ts
end

type 'uc t = 'uc Component.t list

module Component_for_creation = struct
  type 'uc t = 'uc Component.t =
    | Singleton : Flambda_kind.With_subkind.t -> [> `Unarized] t
    | Unboxed_product : _ t list -> [> `Complex] t

  let rec from_lambda (layout : Lambda.layout) =
    match layout with
    | Pvalue _ | Punboxed_float | Punboxed_int _ ->
      Singleton (Flambda_kind.With_subkind.from_lambda layout)
    | Punboxed_product layouts -> Unboxed_product (List.map from_lambda layouts)
    | Ptop ->
      Misc.fatal_error
        "Cannot convert Ptop to Flambda_arity.Component_for_creation"
    | Pbottom ->
      Misc.fatal_error
        "Cannot convert Pbottom to Flambda_arity.Component_for_creation"
end

let nullary = []

let create t = t

let create_singletons t = List.map (fun kind -> Component.Singleton kind) t

let print ppf t =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list Component.print ~pp_sep:(fun ppf () ->
         Format.fprintf ppf " @<1>\u{2a2f} "))
    t

let equal_ignoring_subkinds : type uc1 uc2. uc1 t -> uc2 t -> bool =
 fun t1 t2 -> Misc.Stdlib.List.equal Component.equal_ignoring_subkinds t1 t2

let equal_exact : type uc1 uc2. uc1 t -> uc2 t -> bool =
 fun t1 t2 -> Misc.Stdlib.List.equal Component.equal_exact t1 t2

let is_one_param_of_kind_value : type uc. uc t -> bool =
 fun t ->
  match t with
  | [Component.Singleton kind]
    when Flambda_kind.equal
           (Flambda_kind.With_subkind.kind kind)
           Flambda_kind.value ->
    true
  | [] | Component.Singleton _ :: _ | Component.Unboxed_product _ :: _ -> false

let unarize t = t |> List.map Component.unarize |> List.concat

let unarize_per_parameter t = t |> List.map Component.unarize

let unarize_t t = t |> unarize |> create_singletons

let fresh_idents_unarized t ~id =
  List.mapi
    (fun n kind ->
      let ident =
        Ident.create_local
          (Printf.sprintf "%s_unboxed%d" (Ident.unique_name id) n)
      in
      ident, kind)
    (unarize t)

let cardinal_unarized t = List.length (unarize t)

let num_params t = List.length t

let rec must_be_one_param : type uc. uc t -> Flambda_kind.With_subkind.t option
    =
 fun t ->
  match t with
  | [Component.Singleton kind] -> Some kind
  | [Component.Unboxed_product component] -> must_be_one_param component
  | [] | (Component.Singleton _ | Component.Unboxed_product _) :: _ -> None

let from_lambda_list layouts =
  layouts |> List.map Component_for_creation.from_lambda |> create

let partially_apply t ~num_non_unarized_params_provided =
  if num_non_unarized_params_provided < 0
     || num_non_unarized_params_provided >= List.length t
  then
    Misc.fatal_errorf "Bad num_non_unarized_params_provided (%d): %a"
      num_non_unarized_params_provided print t
  else snd (Misc.Stdlib.List.split_at num_non_unarized_params_provided t)
