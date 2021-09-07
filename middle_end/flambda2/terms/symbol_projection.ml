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

[@@@ocaml.warning "+a-30-40-41-42"]

module Projection = struct
  type t =
    | Block_load of { index : Targetint_31_63.Imm.t }
    | Project_var of
        { project_from : Closure_id.t;
          var : Var_within_closure.t
        }

  let block_load ~index = Block_load { index }

  let project_var project_from var = Project_var { project_from; var }

  let hash t =
    match t with
    | Block_load { index } -> Targetint_31_63.Imm.hash index
    | Project_var { project_from; var } ->
      Hashtbl.hash (Closure_id.hash project_from, Var_within_closure.hash var)

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Block_load { index; } ->
      Format.fprintf ppf "@[<hov 1>(Block_load@ \
          @[<hov 1>(index@ %a)@]\
          )@]"
        Targetint_31_63.Imm.print index
    | Project_var { project_from; var; } ->
      Format.fprintf ppf "@[<hov 1>(Project_var@ \
          @[<hov 1>(project_from@ %a)@]@ \
          @[<hov 1>(var@ %a)@]\
          )@]"
        Closure_id.print project_from
        Var_within_closure.print var

  let compare t1 t2 =
    match t1, t2 with
    | Block_load { index = index1 }, Block_load { index = index2 } ->
      Targetint_31_63.Imm.compare index1 index2
    | ( Project_var { project_from = project_from1; var = var1 },
        Project_var { project_from = project_from2; var = var2 } ) ->
      let c = Closure_id.compare project_from1 project_from2 in
      if c <> 0 then c else Var_within_closure.compare var1 var2
    | Block_load _, Project_var _ -> -1
    | Project_var _, Block_load _ -> 1
end

type t =
  { symbol : Symbol.t;
    projection : Projection.t
  }

let [@ocamlformat "disable"] print ppf { symbol; projection; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(symbol@ %a)@]@ \
      @[<hov 1>(projection@ %a)@]\
      )@]"
    Symbol.print symbol
    Projection.print projection

let create symbol projection = { symbol; projection }

let symbol t = t.symbol

let projection t = t.projection

let compare { symbol = symbol1; projection = projection1 }
    { symbol = symbol2; projection = projection2 } =
  let c = Symbol.compare symbol1 symbol2 in
  if c <> 0 then c else Projection.compare projection1 projection2

let equal t1 t2 = compare t1 t2 = 0

let hash { symbol; projection } =
  Hashtbl.hash (Symbol.hash symbol, Projection.hash projection)

let apply_renaming ({ symbol; projection = _ } as t) renaming =
  let symbol' = Renaming.apply_symbol renaming symbol in
  if symbol == symbol' then t else { t with symbol = symbol' }

let free_names { symbol; projection } =
  let free_names = Name_occurrences.singleton_symbol symbol Name_mode.normal in
  match projection with
  | Block_load _ -> free_names
  | Project_var { project_from = _; var } ->
    Name_occurrences.add_closure_var free_names var Name_mode.normal

let all_ids_for_export { symbol; projection = _ } =
  Ids_for_export.singleton_symbol symbol
