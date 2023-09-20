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
    | Direct of
        { code_id : Code_id.t;
          contains_no_escaping_local_allocs : bool
        }
    | Indirect_unknown_arity
    | Indirect_known_arity

  let print ppf call =
    match call with
    | Direct { code_id; contains_no_escaping_local_allocs } ->
      fprintf ppf
        "@[<hov 1>(Direct@ @[<hov 1>(code_id %a)@]@ @[<hov \
         1>(contains_no_escaping_local_allocs %b)@])@]"
        Code_id.print code_id contains_no_escaping_local_allocs
    | Indirect_unknown_arity -> fprintf ppf "Indirect_unknown_arity"
    | Indirect_known_arity -> fprintf ppf "Indirect_known_arity"
end

module Method_kind = struct
  type t =
    | Self
    | Public
    | Cached

  let print ppf t =
    match t with
    | Self -> fprintf ppf "Self"
    | Public -> fprintf ppf "Public"
    | Cached -> fprintf ppf "Cached"

  let from_lambda (kind : Lambda.meth_kind) =
    match kind with Self -> Self | Public -> Public | Cached -> Cached

  let to_lambda t : Lambda.meth_kind =
    match t with Self -> Self | Public -> Public | Cached -> Cached
end

type t =
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_types.t
      }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t;
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
  | Method { kind; obj; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Simple.print obj
      Method_kind.print kind
      Alloc_mode.For_types.print alloc_mode
  | C_call { alloc; is_c_builtin; } ->
    fprintf ppf "@[<hov 1>(C@ \
        @[<hov 1>(alloc %b)@]@ \
        @[<hov 1>(is_c_builtin %b)@]\
        )@]"
      alloc
      is_c_builtin

let direct_function_call code_id ~contains_no_escaping_local_allocs alloc_mode =
  Function
    { function_call = Direct { code_id; contains_no_escaping_local_allocs };
      alloc_mode
    }

let indirect_function_call_unknown_arity alloc_mode =
  Function { function_call = Indirect_unknown_arity; alloc_mode }

let indirect_function_call_known_arity alloc_mode =
  Function { function_call = Indirect_known_arity; alloc_mode }

let method_call kind ~obj alloc_mode = Method { kind; obj; alloc_mode }

let c_call ~alloc ~is_c_builtin = C_call { alloc; is_c_builtin }

let free_names t =
  match t with
  | Function
      { function_call =
          Direct { code_id; contains_no_escaping_local_allocs = _ };
        alloc_mode = _
      } ->
    Name_occurrences.singleton_code_id code_id Name_mode.normal
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    Name_occurrences.empty
  | Method { kind = _; obj; alloc_mode = _ } -> Simple.free_names obj

let apply_renaming t renaming =
  match t with
  | Function
      { function_call = Direct { code_id; contains_no_escaping_local_allocs };
        alloc_mode
      } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id'
    then t
    else
      Function
        { function_call =
            Direct { code_id = code_id'; contains_no_escaping_local_allocs };
          alloc_mode
        }
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    t
  | Method { kind; obj; alloc_mode } ->
    let obj' = Simple.apply_renaming obj renaming in
    if obj == obj' then t else Method { kind; obj = obj'; alloc_mode }

let ids_for_export t =
  match t with
  | Function
      { function_call =
          Direct { code_id; contains_no_escaping_local_allocs = _ };
        alloc_mode = _
      } ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
  | Function { function_call = Indirect_known_arity; alloc_mode = _ }
  | C_call { alloc = _; is_c_builtin = _ } ->
    Ids_for_export.empty
  | Method { kind = _; obj; alloc_mode = _ } -> Ids_for_export.from_simple obj
