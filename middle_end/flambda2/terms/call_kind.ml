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
        alloc_mode : Alloc_mode.For_allocations.t
      }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t;
        alloc_mode : Alloc_mode.For_allocations.t
      }
  | C_call of
      { needs_caml_c_call : bool;
        is_c_builtin : bool;
        effects : Effects.t;
        coeffects: Coeffects.t;
        alloc_mode : Alloc_mode.For_allocations.t
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Function_call.print function_call
      Alloc_mode.For_allocations.print alloc_mode
  | Method { kind; obj; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Simple.print obj
      Method_kind.print kind
      Alloc_mode.For_allocations.print alloc_mode
  | C_call { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode } ->
    fprintf ppf "@[<hov 1>(C@ \
        @[<hov 1>(needs_caml_c_call@ %b)@]@ \
        @[<hov 1>(is_c_builtin@ %b)@]@ \
        @[<hov 1>(effects@ %a)@]@ \
        @[<hov 1>(coeffects@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      needs_caml_c_call
      is_c_builtin
      Alloc_mode.For_allocations.print alloc_mode
      Effects.print effects
      Coeffects.print coeffects

let direct_function_call code_id alloc_mode =
  Function { function_call = Direct code_id; alloc_mode }

let indirect_function_call_unknown_arity alloc_mode =
  Function { function_call = Indirect_unknown_arity; alloc_mode }

let indirect_function_call_known_arity alloc_mode =
  Function { function_call = Indirect_known_arity; alloc_mode }

let method_call kind ~obj alloc_mode = Method { kind; obj; alloc_mode }

let c_call ~needs_caml_c_call ~is_c_builtin ~effects ~coeffects alloc_mode =
  C_call { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode }

let free_names t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    Name_occurrences.add_code_id
      (Alloc_mode.For_allocations.free_names alloc_mode)
      code_id Name_mode.normal
  | Function { function_call = Indirect_unknown_arity; alloc_mode }
  | Function { function_call = Indirect_known_arity; alloc_mode } ->
    Alloc_mode.For_allocations.free_names alloc_mode
  | C_call { needs_caml_c_call = _; is_c_builtin = _; effects = _; coeffects = _; alloc_mode } ->
    Alloc_mode.For_allocations.free_names alloc_mode
  | Method { kind = _; obj; alloc_mode } ->
    Name_occurrences.union (Simple.free_names obj)
      (Alloc_mode.For_allocations.free_names alloc_mode)

let apply_renaming t renaming =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if code_id == code_id' && alloc_mode == alloc_mode'
    then t
    else Function { function_call = Direct code_id'; alloc_mode = alloc_mode' }
  | Function
      { function_call =
          (Indirect_unknown_arity | Indirect_known_arity) as function_call;
        alloc_mode
      } ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode'
    then t
    else Function { function_call; alloc_mode = alloc_mode' }
  | C_call { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode } ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode'
    then t
    else C_call { needs_caml_c_call; is_c_builtin; effects; coeffects;
                  alloc_mode = alloc_mode' }
  | Method { kind; obj; alloc_mode } ->
    let obj' = Simple.apply_renaming obj renaming in
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if obj == obj' && alloc_mode == alloc_mode'
    then t
    else Method { kind; obj = obj'; alloc_mode = alloc_mode' }

let ids_for_export t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    Ids_for_export.add_code_id
      (Alloc_mode.For_allocations.ids_for_export alloc_mode)
      code_id
  | Function { function_call = Indirect_unknown_arity; alloc_mode }
  | Function { function_call = Indirect_known_arity; alloc_mode } ->
    Alloc_mode.For_allocations.ids_for_export alloc_mode
  | C_call { needs_caml_c_call = _; is_c_builtin = _; effects = _; coeffects = _;
             alloc_mode } ->
    Alloc_mode.For_allocations.ids_for_export alloc_mode
  | Method { kind = _; obj; alloc_mode } ->
    Ids_for_export.union
      (Ids_for_export.from_simple obj)
      (Alloc_mode.For_allocations.ids_for_export alloc_mode)
