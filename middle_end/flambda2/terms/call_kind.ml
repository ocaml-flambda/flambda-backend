(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let fprintf = Format.fprintf

module Function_call = struct
  type t =
    | Direct of Code_id.t
    | Indirect_unknown_arity
    | Indirect_known_arity

  let print ppf call =
    match call with
    | Direct code_id ->
      fprintf ppf "@[<hov 1>(Direct %a)@]" Code_id.print code_id
    | Indirect_unknown_arity -> fprintf ppf "Indirect_unknown_arity"
    | Indirect_known_arity -> fprintf ppf "Indirect_known_arity"
end

type t =
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_types.t
      }
  | C_call of
      { alloc : bool;
        is_c_builtin : bool
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Function_call.print function_call
      Alloc_mode.For_types.print alloc_mode
  | C_call { alloc; is_c_builtin; } ->
    fprintf ppf "@[<hov 1>(C@ \
        @[<hov 1>(alloc %b)@]@ \
        @[<hov 1>(is_c_builtin %b)@]\
        )@]"
      alloc
      is_c_builtin

let direct_function_call code_id alloc_mode =
  Function { function_call = Direct code_id; alloc_mode }

let indirect_function_call_unknown_arity alloc_mode =
  Function { function_call = Indirect_unknown_arity; alloc_mode }

let indirect_function_call_known_arity alloc_mode =
  Function { function_call = Indirect_known_arity; alloc_mode }

let c_call ~alloc ~is_c_builtin = C_call { alloc; is_c_builtin }

let free_names t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode = _ } ->
    Name_occurrences.singleton_code_id code_id Name_mode.normal
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    Name_occurrences.empty

let apply_renaming t renaming =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id'
    then t
    else Function { function_call = Direct code_id'; alloc_mode }
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    t

let ids_for_export t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode = _ } ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    Ids_for_export.empty
