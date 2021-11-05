(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Guillaume Bury, OCamlPro                 *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42-55"]

(* CR mshinwell: Disabling warning 55 is required to satisfy Closure, work out
   something better... *)

module Continuations = Permutation.Make [@inlined hint] (Continuation)
module Variables = Permutation.Make [@inlined hint] (Variable)
module Code_ids = Permutation.Make [@inlined hint] (Code_id)
module Symbols = Permutation.Make [@inlined hint] (Symbol)
module Coercion = Reg_width_things.Coercion
module Const = Reg_width_things.Const
module Simple = Reg_width_things.Simple

module Import_map : sig
  type t

  val create :
    symbols:Symbol.t Symbol.Map.t ->
    variables:Variable.t Variable.Map.t ->
    simples:Simple.t Simple.Map.t ->
    consts:Const.t Const.Map.t ->
    code_ids:Code_id.t Code_id.Map.t ->
    continuations:Continuation.t Continuation.Map.t ->
    used_closure_vars:Var_within_closure.Set.t ->
    original_compilation_unit:Compilation_unit.t ->
    t

  val is_empty : t -> bool

  val const : t -> Const.t -> Const.t

  val variable : t -> Variable.t -> Variable.t

  val symbol : t -> Symbol.t -> Symbol.t

  val simple : t -> Simple.t -> Simple.t

  val code_id : t -> Code_id.t -> Code_id.t

  val continuation : t -> Continuation.t -> Continuation.t

  val closure_var_is_used : t -> Var_within_closure.t -> bool
end = struct
  type t =
    { symbols : Symbol.t Symbol.Map.t;
      variables : Variable.t Variable.Map.t;
      simples : Simple.t Simple.Map.t;
      consts : Const.t Const.Map.t;
      code_ids : Code_id.t Code_id.Map.t;
      continuations : Continuation.t Continuation.Map.t;
      used_closure_vars : Var_within_closure.Set.t;
      (* CR vlaviron: [used_closure_vars] is here because we need to rewrite the
         types to remove occurrences of unused closure variables, as otherwise
         the types can contain references to code that is neither exported nor
         present in the actual object file. But this means rewriting types, and
         the only place a rewriting traversal is done at the moment is during
         import. This solution is not ideal because the missing code IDs will
         still be present in the emitted cmx files, and during the traversal in
         [Flambda_cmx.compute_reachable_names_and_code] we have to assume that
         code IDs can be missing (and so we cannot detect code IDs that are
         really missing at this point). *)
      original_compilation_unit : Compilation_unit.t
          (* This complements [used_closure_vars]. Removal of closure variables
             is only allowed for variables that are not used in the compilation
             unit they are defined in. *)
    }

  let is_empty
      { symbols;
        variables;
        simples;
        consts;
        code_ids;
        continuations;
        used_closure_vars;
        original_compilation_unit = _
      } =
    Symbol.Map.is_empty symbols
    && Variable.Map.is_empty variables
    && Simple.Map.is_empty simples
    && Const.Map.is_empty consts
    && Code_id.Map.is_empty code_ids
    && Continuation.Map.is_empty continuations
    && Var_within_closure.Set.is_empty used_closure_vars

  let create ~symbols ~variables ~simples ~consts ~code_ids ~continuations
      ~used_closure_vars ~original_compilation_unit =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
      used_closure_vars;
      original_compilation_unit
    }

  let symbol t orig =
    match Symbol.Map.find orig t.symbols with
    | symbol -> symbol
    | exception Not_found -> orig

  let variable t orig =
    match Variable.Map.find orig t.variables with
    | variable -> variable
    | exception Not_found -> orig

  let const t orig =
    match Const.Map.find orig t.consts with
    | const -> const
    | exception Not_found -> orig

  let code_id t orig =
    match Code_id.Map.find orig t.code_ids with
    | code_id -> code_id
    | exception Not_found -> orig

  let continuation t orig =
    match Continuation.Map.find orig t.continuations with
    | continuation -> continuation
    | exception Not_found -> orig

  let simple t simple =
    (* [t.simples] only holds those [Simple]s with [Coercion] (analogously to
       the grand table of [Simple]s, see reg_width_things.ml). *)
    match Simple.Map.find simple t.simples with
    | simple -> simple
    | exception Not_found -> simple

  let closure_var_is_used t var =
    if Var_within_closure.in_compilation_unit var t.original_compilation_unit
    then Var_within_closure.Set.mem var t.used_closure_vars
    else (* This closure variable might be used in other units *)
      true
end

type t =
  { continuations : Continuations.t;
    variables : Variables.t;
    code_ids : Code_ids.t;
    symbols : Symbols.t;
    import_map : Import_map.t option
  }

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
    code_ids = Code_ids.empty;
    symbols = Symbols.empty;
    import_map = None
  }

let create_import_map ~symbols ~variables ~simples ~consts ~code_ids
    ~continuations ~used_closure_vars ~original_compilation_unit =
  let import_map =
    Import_map.create ~symbols ~variables ~simples ~consts ~code_ids
      ~continuations ~used_closure_vars ~original_compilation_unit
  in
  if Import_map.is_empty import_map
  then empty
  else { empty with import_map = Some import_map }

let [@ocamlformat "disable"] print ppf
      { continuations; variables; code_ids; symbols; import_map = _; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])@ \
      @[<hov 1>(code_ids@ %a)@])@ \
      @[<hov 1>(symbols@ %a)@])@ \
      @]"
    Continuations.print continuations
    Variables.print variables
    Code_ids.print code_ids
    Symbols.print symbols

let is_empty { continuations; variables; code_ids; symbols; import_map } =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Code_ids.is_empty code_ids && Symbols.is_empty symbols
  &&
  match import_map with
  | None -> true
  | Some import_map -> Import_map.is_empty import_map

let compose0
    ~second:
      ({ continuations = continuations2;
         variables = variables2;
         code_ids = code_ids2;
         symbols = symbols2;
         import_map = import_map2
       } as second)
    ~first:
      ({ continuations = continuations1;
         variables = variables1;
         code_ids = code_ids1;
         symbols = symbols1;
         import_map = import_map1
       } as first) =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
    code_ids = Code_ids.compose ~second:code_ids2 ~first:code_ids1;
    symbols = Symbols.compose ~second:symbols2 ~first:symbols1;
    (* The process of simplification of terms together with the collection of
       [Ids_for_export] from types, prior to writing of .cmx files, should
       ensure that only [first] (and not [second]) has an import map. *)
    import_map =
      (match import_map1, import_map2 with
      | None, None -> None
      | Some _, None -> import_map1
      | (None | Some _), Some _ ->
        Misc.fatal_errorf
          "Cannot compose renamings; only the [first] renaming may have an \
           import map.  first:@ %a@ second:@ %a"
          print first print second)
  }

let compose ~second ~first =
  if is_empty second
  then first
  else if is_empty first
  then second
  else compose0 ~second ~first

let add_variable t var1 var2 =
  { t with variables = Variables.compose_one ~first:t.variables var1 var2 }

let add_fresh_variable t var1 ~guaranteed_fresh:var2 =
  { t with
    variables = Variables.compose_one_fresh t.variables var1 ~fresh:var2
  }

let apply_variable t var =
  let var =
    match t.import_map with
    | None -> var
    | Some import_map -> Import_map.variable import_map var
  in
  Variables.apply t.variables var

let apply_variable_set t vars =
  Variable.Set.fold
    (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars Variable.Set.empty

let add_symbol t symbol1 symbol2 =
  { t with symbols = Symbols.compose_one ~first:t.symbols symbol1 symbol2 }

let add_fresh_symbol t symbol1 ~guaranteed_fresh:symbol2 =
  { t with
    symbols = Symbols.compose_one_fresh t.symbols symbol1 ~fresh:symbol2
  }

let apply_symbol t symbol =
  let symbol =
    match t.import_map with
    | None -> symbol
    | Some import_map -> Import_map.symbol import_map symbol
  in
  Symbols.apply t.symbols symbol

let apply_symbol_set t symbols =
  Symbol.Set.fold
    (fun symbol result ->
      let symbol = apply_symbol t symbol in
      Symbol.Set.add symbol result)
    symbols Symbol.Set.empty

let apply_name t name =
  Name.pattern_match name
    ~var:(fun var -> Name.var (apply_variable t var))
    ~symbol:(fun symbol -> Name.symbol (apply_symbol t symbol))

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.compose_one ~first:t.continuations k1 k2
  }

let add_fresh_continuation t k1 ~guaranteed_fresh:k2 =
  { t with
    continuations = Continuations.compose_one_fresh t.continuations k1 ~fresh:k2
  }

let apply_continuation t k =
  let k =
    match t.import_map with
    | None -> k
    | Some import_map -> Import_map.continuation import_map k
  in
  Continuations.apply t.continuations k

let add_code_id t code_id1 code_id2 =
  { t with code_ids = Code_ids.compose_one ~first:t.code_ids code_id1 code_id2 }

let add_fresh_code_id t code_id1 ~guaranteed_fresh:code_id2 =
  { t with
    code_ids = Code_ids.compose_one_fresh t.code_ids code_id1 ~fresh:code_id2
  }

let apply_code_id t code_id =
  let code_id =
    match t.import_map with
    | None -> code_id
    | Some import_map -> Import_map.code_id import_map code_id
  in
  Code_ids.apply t.code_ids code_id

let apply_const t cst =
  match t.import_map with
  | None -> cst
  | Some import_map -> Import_map.const import_map cst

let apply_simple t simple =
  let simple =
    match t.import_map with
    | None -> simple
    | Some import_map -> Import_map.simple import_map simple
  in
  let[@inline always] name old_name ~coercion:old_coercion =
    let new_name = apply_name t old_name in
    let new_coercion =
      Coercion.map_depth_variables old_coercion ~f:(fun dv ->
          apply_variable t dv)
    in
    if old_name == new_name && old_coercion == new_coercion
    then simple
    else Simple.with_coercion (Simple.name new_name) new_coercion
  in
  (* Constants are never permuted, only freshened upon import. *)
  Simple.pattern_match simple ~name ~const:(fun cst ->
      assert (not (Simple.has_coercion simple));
      Simple.const (apply_const t cst))

let closure_var_is_used t closure_var =
  match t.import_map with
  | None -> true (* N.B. not false! *)
  | Some import_map -> Import_map.closure_var_is_used import_map closure_var
