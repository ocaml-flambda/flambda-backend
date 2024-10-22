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

module Effect = struct
  type t =
    | Perform of { eff : Simple.t }
    | Reperform of
        { eff : Simple.t;
          cont : Simple.t;
          last_fiber : Simple.t
        }
    | Run_stack of
        { stack : Simple.t;
          f : Simple.t;
          arg : Simple.t
        }
    | Resume of
        { stack : Simple.t;
          f : Simple.t;
          arg : Simple.t;
          last_fiber : Simple.t
        }

  let print ppf t =
    match t with
    | Perform { eff } ->
      fprintf ppf "@[<hov 1>(%tPerform%t@ %a)@]" Flambda_colours.effect
        Flambda_colours.pop Simple.print eff
    | Reperform { eff; cont; last_fiber } ->
      fprintf ppf
        "@[<hov 1>(%tReperform%t@ (eff@ %a)@ (cont@ %a)@ (last_fiber@ %a))@]"
        Flambda_colours.effect Flambda_colours.pop Simple.print eff Simple.print
        cont Simple.print last_fiber
    | Run_stack { stack; f; arg } ->
      fprintf ppf "@[<hov 1>(%tRun_stack%t (stack@ %a)@ (f@ %a)@ (arg@ %a))@]"
        Flambda_colours.effect Flambda_colours.pop Simple.print stack
        Simple.print f Simple.print arg
    | Resume { stack; f; arg; last_fiber } ->
      fprintf ppf
        "@[<hov 1>(%tResume%t (stack@ %a)@ (f@ %a)@ (arg@ %a) (last_fiber@ \
         %a))@]"
        Flambda_colours.effect Flambda_colours.pop Simple.print stack
        Simple.print f Simple.print arg Simple.print last_fiber

  let perform ~eff = Perform { eff }

  let reperform ~eff ~cont ~last_fiber = Reperform { eff; cont; last_fiber }

  let run_stack ~stack ~f ~arg = Run_stack { stack; f; arg }

  let resume ~stack ~f ~arg ~last_fiber = Resume { stack; f; arg; last_fiber }

  let free_names t =
    match t with
    | Perform { eff } -> Simple.free_names eff
    | Reperform { eff; cont; last_fiber } ->
      Name_occurrences.union (Simple.free_names eff)
        (Name_occurrences.union (Simple.free_names cont)
           (Simple.free_names last_fiber))
    | Run_stack { stack; f; arg } ->
      Name_occurrences.union (Simple.free_names stack)
        (Name_occurrences.union (Simple.free_names f) (Simple.free_names arg))
    | Resume { stack; f; arg; last_fiber } ->
      Name_occurrences.union (Simple.free_names stack)
        (Name_occurrences.union (Simple.free_names f)
           (Name_occurrences.union (Simple.free_names arg)
              (Simple.free_names last_fiber)))

  let apply_renaming t renaming =
    match t with
    | Perform { eff } ->
      let eff' = Simple.apply_renaming eff renaming in
      if eff == eff' then t else Perform { eff = eff' }
    | Reperform { eff; cont; last_fiber } ->
      let eff' = Simple.apply_renaming eff renaming in
      let cont' = Simple.apply_renaming cont renaming in
      let last_fiber' = Simple.apply_renaming last_fiber renaming in
      if eff == eff' && cont == cont' && last_fiber == last_fiber'
      then t
      else Reperform { eff = eff'; cont = cont'; last_fiber = last_fiber' }
    | Run_stack { stack; f; arg } ->
      let stack' = Simple.apply_renaming stack renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      if stack == stack' && f == f' && arg == arg'
      then t
      else Run_stack { stack = stack'; f = f'; arg = arg' }
    | Resume { stack; f; arg; last_fiber } ->
      let stack' = Simple.apply_renaming stack renaming in
      let f' = Simple.apply_renaming f renaming in
      let arg' = Simple.apply_renaming arg renaming in
      let last_fiber' = Simple.apply_renaming last_fiber renaming in
      if stack == stack' && f == f' && arg == arg' && last_fiber == last_fiber'
      then t
      else
        Resume { stack = stack'; f = f'; arg = arg'; last_fiber = last_fiber' }

  let ids_for_export t =
    match t with
    | Perform { eff } -> Ids_for_export.from_simple eff
    | Reperform { eff; cont; last_fiber } ->
      Ids_for_export.union
        (Ids_for_export.from_simple eff)
        (Ids_for_export.union
           (Ids_for_export.from_simple cont)
           (Ids_for_export.from_simple last_fiber))
    | Run_stack { stack; f; arg } ->
      Ids_for_export.union
        (Ids_for_export.from_simple stack)
        (Ids_for_export.union
           (Ids_for_export.from_simple f)
           (Ids_for_export.from_simple arg))
    | Resume { stack; f; arg; last_fiber } ->
      Ids_for_export.union
        (Ids_for_export.from_simple stack)
        (Ids_for_export.union
           (Ids_for_export.from_simple f)
           (Ids_for_export.union
              (Ids_for_export.from_simple arg)
              (Ids_for_export.from_simple last_fiber)))
end

type t =
  | Function of
      { function_call : Function_call.t;
        alloc_mode : Alloc_mode.For_applications.t
      }
  | Method of
      { kind : Method_kind.t;
        obj : Simple.t;
        alloc_mode : Alloc_mode.For_applications.t
      }
  | C_call of
      { needs_caml_c_call : bool;
        is_c_builtin : bool;
        effects : Effects.t;
        coeffects : Coeffects.t;
        alloc_mode : Alloc_mode.For_applications.t
      }
  | Effect of Effect.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function { function_call; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Function@ \
        @[<hov 1>(function_call@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Function_call.print function_call
      Alloc_mode.For_applications.print alloc_mode
  | Method { kind; obj; alloc_mode } ->
    fprintf ppf "@[<hov 1>(Method@ \
        @[<hov 1>(obj@ %a)@]@ \
        @[<hov 1>(kind@ %a)@]@ \
        @[<hov 1>(alloc_mode@ %a)@]\
        )@]"
      Simple.print obj
      Method_kind.print kind
      Alloc_mode.For_applications.print alloc_mode
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
      Alloc_mode.For_applications.print alloc_mode
      Effects.print effects
      Coeffects.print coeffects
  | Effect effect_op -> Effect.print ppf effect_op

let direct_function_call code_id alloc_mode =
  Function { function_call = Direct code_id; alloc_mode }

let indirect_function_call_unknown_arity alloc_mode =
  Function { function_call = Indirect_unknown_arity; alloc_mode }

let indirect_function_call_known_arity alloc_mode =
  Function { function_call = Indirect_known_arity; alloc_mode }

let method_call kind ~obj alloc_mode = Method { kind; obj; alloc_mode }

let c_call ~needs_caml_c_call ~is_c_builtin ~effects ~coeffects alloc_mode =
  C_call { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode }

let effect eff = Effect eff

let free_names t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    Name_occurrences.add_code_id
      (Alloc_mode.For_applications.free_names alloc_mode)
      code_id Name_mode.normal
  | Function { function_call = Indirect_unknown_arity; alloc_mode }
  | Function { function_call = Indirect_known_arity; alloc_mode } ->
    Alloc_mode.For_applications.free_names alloc_mode
  | C_call
      { needs_caml_c_call = _;
        is_c_builtin = _;
        effects = _;
        coeffects = _;
        alloc_mode
      } ->
    Alloc_mode.For_applications.free_names alloc_mode
  | Method { kind = _; obj; alloc_mode } ->
    Name_occurrences.union (Simple.free_names obj)
      (Alloc_mode.For_applications.free_names alloc_mode)
  | Effect op -> Effect.free_names op

let apply_renaming t renaming =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    let code_id' = Renaming.apply_code_id renaming code_id in
    let alloc_mode' =
      Alloc_mode.For_applications.apply_renaming alloc_mode renaming
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
      Alloc_mode.For_applications.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode'
    then t
    else Function { function_call; alloc_mode = alloc_mode' }
  | C_call { needs_caml_c_call; is_c_builtin; effects; coeffects; alloc_mode }
    ->
    let alloc_mode' =
      Alloc_mode.For_applications.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode'
    then t
    else
      C_call
        { needs_caml_c_call;
          is_c_builtin;
          effects;
          coeffects;
          alloc_mode = alloc_mode'
        }
  | Method { kind; obj; alloc_mode } ->
    let obj' = Simple.apply_renaming obj renaming in
    let alloc_mode' =
      Alloc_mode.For_applications.apply_renaming alloc_mode renaming
    in
    if obj == obj' && alloc_mode == alloc_mode'
    then t
    else Method { kind; obj = obj'; alloc_mode = alloc_mode' }
  | Effect op ->
    let op' = Effect.apply_renaming op renaming in
    if op == op' then t else Effect op'

let ids_for_export t =
  match t with
  | Function { function_call = Direct code_id; alloc_mode } ->
    Ids_for_export.add_code_id
      (Alloc_mode.For_applications.ids_for_export alloc_mode)
      code_id
  | Function { function_call = Indirect_unknown_arity; alloc_mode }
  | Function { function_call = Indirect_known_arity; alloc_mode } ->
    Alloc_mode.For_applications.ids_for_export alloc_mode
  | C_call
      { needs_caml_c_call = _;
        is_c_builtin = _;
        effects = _;
        coeffects = _;
        alloc_mode
      } ->
    Alloc_mode.For_applications.ids_for_export alloc_mode
  | Method { kind = _; obj; alloc_mode } ->
    Ids_for_export.union
      (Ids_for_export.from_simple obj)
      (Alloc_mode.For_applications.ids_for_export alloc_mode)
  | Effect op -> Effect.ids_for_export op
