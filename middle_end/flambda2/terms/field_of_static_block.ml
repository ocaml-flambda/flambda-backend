(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Symbol of Symbol.t
  | Tagged_immediate of Targetint_31_63.t
  | Dynamically_computed of Variable.t * Debuginfo.t

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
    | Tagged_immediate t1, Tagged_immediate t2 -> Targetint_31_63.compare t1 t2
    | Dynamically_computed (v1, _dbg1), Dynamically_computed (v2, _dbg2) ->
      Variable.compare v1 v2
    | Symbol _, Tagged_immediate _ -> -1
    | Tagged_immediate _, Symbol _ -> 1
    | Symbol _, Dynamically_computed _ -> -1
    | Dynamically_computed _, Symbol _ -> 1
    | Tagged_immediate _, Dynamically_computed _ -> -1
    | Dynamically_computed _, Tagged_immediate _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash t =
    match t with
    | Symbol symbol -> Hashtbl.hash (0, Symbol.hash symbol)
    | Tagged_immediate immediate ->
      Hashtbl.hash (1, Targetint_31_63.hash immediate)
    | Dynamically_computed (var, _dbg) -> Hashtbl.hash (2, Variable.hash var)

  let print ppf t =
    match t with
    | Symbol symbol ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.symbol Symbol.print symbol
        Flambda_colours.pop
    | Tagged_immediate immediate ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.tagged_immediate
        Targetint_31_63.print immediate Flambda_colours.pop
    | Dynamically_computed (var, _dbg) ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.variable Variable.print var
        Flambda_colours.pop
end)

let apply_renaming t renaming =
  match t with
  | Tagged_immediate _ -> t
  | Symbol symbol ->
    let symbol' = Renaming.apply_symbol renaming symbol in
    if symbol == symbol' then t else Symbol symbol'
  | Dynamically_computed (var, dbg) ->
    let var' = Renaming.apply_variable renaming var in
    if var == var' then t else Dynamically_computed (var', dbg)

let free_names t =
  match t with
  | Dynamically_computed (var, _dbg) ->
    Name_occurrences.singleton_variable var Name_mode.normal
  | Symbol sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
  | Tagged_immediate _ -> Name_occurrences.empty

let ids_for_export t =
  match t with
  | Dynamically_computed (var, _dbg) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Symbol sym -> Ids_for_export.add_symbol Ids_for_export.empty sym
  | Tagged_immediate _ -> Ids_for_export.empty

let tagged_immediate i = Tagged_immediate i
