(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Projection = struct
  type t =
    | Block_load of { index : Targetint_31_63.t }
    | Project_value_slot of
        { project_from : Function_slot.t;
          value_slot : Value_slot.t
        }

  let block_load ~index = Block_load { index }

  let project_value_slot project_from value_slot =
    Project_value_slot { project_from; value_slot }

  let hash t =
    match t with
    | Block_load { index } -> Targetint_31_63.hash index
    | Project_value_slot { project_from; value_slot } ->
      Hashtbl.hash (Function_slot.hash project_from, Value_slot.hash value_slot)

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Block_load { index; } ->
      Format.fprintf ppf "@[<hov 1>(Block_load@ \
          @[<hov 1>(index@ %a)@]\
          )@]"
        Targetint_31_63.print index
    | Project_value_slot { project_from; value_slot; } ->
      Format.fprintf ppf "@[<hov 1>(Project_value_slot@ \
          @[<hov 1>(project_from@ %a)@]@ \
          @[<hov 1>(var@ %a)@]\
          )@]"
        Function_slot.print project_from
        Value_slot.print value_slot

  let compare t1 t2 =
    match t1, t2 with
    | Block_load { index = index1 }, Block_load { index = index2 } ->
      Targetint_31_63.compare index1 index2
    | ( Project_value_slot
          { project_from = project_from1; value_slot = value_slot1 },
        Project_value_slot
          { project_from = project_from2; value_slot = value_slot2 } ) ->
      let c = Function_slot.compare project_from1 project_from2 in
      if c <> 0 then c else Value_slot.compare value_slot1 value_slot2
    | Block_load _, Project_value_slot _ -> -1
    | Project_value_slot _, Block_load _ -> 1
end

type t =
  { symbol : Symbol.t;
    projection : Projection.t;
    kind : Flambda_kind.With_subkind.t
  }

let [@ocamlformat "disable"] print ppf { symbol; projection;kind } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(symbol@ %a)@]@ \
      @[<hov 1>(projection@ %a)@] \
      @[<hov 1>(kind@ %a)@]\
      )@]"
    Symbol.print symbol
    Projection.print projection Flambda_kind.With_subkind.print kind

let create symbol projection kind = { symbol; projection; kind }

let symbol t = t.symbol

let projection t = t.projection

let kind t = t.kind

let compare { symbol = symbol1; projection = projection1; kind = kind1 }
    { symbol = symbol2; projection = projection2; kind = kind2 } =
  let c = Symbol.compare symbol1 symbol2 in
  if c <> 0
  then c
  else
    let c = Projection.compare projection1 projection2 in
    if c <> 0 then c else Flambda_kind.With_subkind.compare kind1 kind2

let equal t1 t2 = compare t1 t2 = 0

let hash { symbol; projection; kind } =
  Hashtbl.hash
    ( Symbol.hash symbol,
      Hashtbl.hash
        (Projection.hash projection, Flambda_kind.With_subkind.hash kind) )

let apply_renaming ({ symbol; projection = _; kind = _ } as t) renaming =
  let symbol' = Renaming.apply_symbol renaming symbol in
  if symbol == symbol' then t else { t with symbol = symbol' }

let free_names { symbol; projection; kind = _ } =
  let free_names = Name_occurrences.singleton_symbol symbol Name_mode.normal in
  match projection with
  | Block_load _ -> free_names
  | Project_value_slot { project_from; value_slot } ->
    Name_occurrences.add_function_slot_in_projection
      (Name_occurrences.add_value_slot_in_projection free_names value_slot
         Name_mode.normal)
      project_from Name_mode.normal

let ids_for_export { symbol; projection = _; kind = _ } =
  Ids_for_export.singleton_symbol symbol
