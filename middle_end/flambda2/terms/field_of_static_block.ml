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

module Mixed_field = struct
  module Unboxed_number = struct
  type t =
    | Unboxed_float of Numeric_types.Float_by_bit_pattern.t
    | Unboxed_float32 of Numeric_types.Float32_by_bit_pattern.t
    | Unboxed_int32 of Numeric_types.Int32.t
    | Unboxed_int64 of Numeric_types.Int64.t
    | Unboxed_nativeint of Targetint_32_64.t

  include Container_types.Make (struct
      type nonrec t = t
  let print ppf = function
    | Unboxed_float t ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.naked_number
        Numeric_types.Float_by_bit_pattern.print t Flambda_colours.pop
    | Unboxed_float32 t ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.naked_number
        Numeric_types.Float32_by_bit_pattern.print t Flambda_colours.pop
    | Unboxed_int32 t ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.naked_number
        Numeric_types.Int32.print t Flambda_colours.pop
    | Unboxed_int64 t ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.naked_number
        Numeric_types.Int64.print t Flambda_colours.pop
    | Unboxed_nativeint t ->
      Format.fprintf ppf "%t%a%t" Flambda_colours.naked_number
        Targetint_32_64.print t Flambda_colours.pop

  let compare t1 t2 =
    match t1, t2 with
    | Unboxed_float t1, Unboxed_float t2 ->
      Numeric_types.Float_by_bit_pattern.compare t1 t2
    | Unboxed_float32 t1, Unboxed_float32 t2 ->
      Numeric_types.Float32_by_bit_pattern.compare t1 t2
    | Unboxed_int32 t1, Unboxed_int32 t2 -> Numeric_types.Int32.compare t1 t2
    | Unboxed_int64 t1, Unboxed_int64 t2 -> Numeric_types.Int64.compare t1 t2
    | Unboxed_nativeint t1, Unboxed_nativeint t2 ->
      Targetint_32_64.compare t1 t2
    | Unboxed_float _, _ -> 1
    | _, Unboxed_float _ -> -1
    | Unboxed_float32 _, _ -> 1
    | _, Unboxed_float32 _ -> -1
    | Unboxed_int32 _, _ -> 1
    | _, Unboxed_int32 _ -> -1
    | Unboxed_int64 _, _ -> 1
    | _, Unboxed_int64 _ -> -1

  let equal t1 t2 = compare t1 t2 = 0

  let hash = function
    | Unboxed_float t ->
      Hashtbl.hash (0, Numeric_types.Float_by_bit_pattern.hash t)
    | Unboxed_float32 t ->
      Hashtbl.hash (1, Numeric_types.Float32_by_bit_pattern.hash t)
    | Unboxed_int32 t -> Hashtbl.hash (2, Numeric_types.Int32.hash t)
    | Unboxed_int64 t -> Hashtbl.hash (3, Numeric_types.Int64.hash t)
    | Unboxed_nativeint t -> Hashtbl.hash (4, Targetint_32_64.hash t)
end)
end

  type nonrec t =
    | Value of t
    | Unboxed_number of Unboxed_number.t

  include Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Value t1, Value t2 -> compare t1 t2
      | Unboxed_number t1, Unboxed_number t2 -> Unboxed_number.compare t1 t2
      | Value _, Unboxed_number _ -> 1
      | Unboxed_number _, Value _ -> -1

    let equal t1 t2 = compare t1 t2 = 0

    let hash t =
      match t with
      | Value t -> Hashtbl.hash (0, hash t)
      | Unboxed_number t -> Hashtbl.hash (1, Unboxed_number.hash t)

    let print ppf t =
      match t with
      | Value t ->
        Format.fprintf ppf "%t%a%t" Flambda_colours.symbol print t
          Flambda_colours.pop
      | Unboxed_number t -> Unboxed_number.print ppf t
  end)

  let free_names t =
    match t with
    | Value t -> free_names t
    | Unboxed_number _ -> Name_occurrences.empty

  let apply_renaming t renaming =
    match t with
    | Value value ->
      let value' = apply_renaming value renaming in
      if value == value' then t else Value value'
    | Unboxed_number _ as t -> t

  let ids_for_export t =
    match t with
    | Value t -> ids_for_export t
    | Unboxed_number _ -> Ids_for_export.empty
end
