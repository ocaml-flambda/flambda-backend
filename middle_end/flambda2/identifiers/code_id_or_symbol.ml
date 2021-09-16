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

type t =
  | Code_id of Code_id.t
  | Symbol of Symbol.t

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Code_id code_id ->
      Format.fprintf ppf "@[<hov 1>(code_id@ %a)@]" Code_id.print code_id
    | Symbol symbol ->
      Format.fprintf ppf "@[<hov 1>(symbol@ %a)@]" Symbol.print symbol

  let compare t1 t2 =
    match t1, t2 with
    | Code_id _, Symbol _ -> -1
    | Symbol _, Code_id _ -> 1
    | Code_id code_id1, Code_id code_id2 -> Code_id.compare code_id1 code_id2
    | Symbol symbol1, Symbol symbol2 -> Symbol.compare symbol1 symbol2

  let equal t1 t2 = compare t1 t2 = 0

  let output _ _ = Misc.fatal_error "Not yet implemented"

  let hash _ = Misc.fatal_error "Not yet implemented"
end)

let compilation_unit t =
  match t with
  | Code_id code_id -> Code_id.get_compilation_unit code_id
  | Symbol sym -> Symbol.compilation_unit sym

let set_of_code_id_set code_ids =
  Code_id.Set.fold
    (fun code_id free_code_ids -> Set.add (Code_id code_id) free_code_ids)
    code_ids Set.empty

let set_of_symbol_set symbols =
  Symbol.Set.fold
    (fun sym free_syms -> Set.add (Symbol sym) free_syms)
    symbols Set.empty

let of_symbol sym = Symbol sym
