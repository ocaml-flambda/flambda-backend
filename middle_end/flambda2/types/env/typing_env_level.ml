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

module K = Flambda_kind
module TG = Type_grammar

type t =
  { defined_vars : K.t Variable.Map.t;
    binding_times : Variable.Set.t Binding_time.Map.t;
    equations : TG.t Name.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let defined_variables t = Variable.Map.keys t.defined_vars

let defined_variables_with_kinds t = t.defined_vars

let defined_names t = Name.set_of_var_set (Variable.Map.keys t.defined_vars)

let variables_by_binding_time t = t.binding_times

let find_kind t var = Variable.Map.find var t.defined_vars

let variable_is_defined t var = Variable.Map.mem var t.defined_vars

(* CR mshinwell: print symbol projections *)
let print_equations ppf equations =
  let equations = Name.Map.bindings equations in
  match equations with
  | [] -> Format.pp_print_string ppf "()"
  | _ :: _ ->
    Format.fprintf ppf "(%a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (name, ty) ->
           Format.fprintf ppf "@[<hov 1>%a@ :@ %a@]" Name.print name TG.print ty))
      equations

let [@ocamlformat "disable"] print ppf
      { defined_vars; binding_times = _; equations;
        symbol_projections = _; } =
  (* CR mshinwell: Print [defined_vars] when not called from
     [Typing_env.print] *)
  if Variable.Map.is_empty defined_vars then
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(equations@ @[<v 1>%a@])@])\
        @]"
      print_equations equations
  else
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(defined_vars@ @[<hov 1>%a@])@]@ \
        @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
        )@]"
      Variable.Set.print (Variable.Map.keys defined_vars)
      print_equations equations

let fold_on_defined_vars f t init =
  Binding_time.Map.fold
    (fun _bt vars acc ->
      Variable.Set.fold
        (fun var acc ->
          let kind = Variable.Map.find var t.defined_vars in
          f var kind acc)
        vars acc)
    t.binding_times init

let empty =
  { defined_vars = Variable.Map.empty;
    binding_times = Binding_time.Map.empty;
    equations = Name.Map.empty;
    symbol_projections = Variable.Map.empty
  }

let is_empty { defined_vars; binding_times; equations; symbol_projections } =
  Variable.Map.is_empty defined_vars
  && Binding_time.Map.is_empty binding_times
  && Name.Map.is_empty equations
  && Variable.Map.is_empty symbol_projections

let create ~defined_vars ~binding_times ~equations ~symbol_projections =
  (if Flambda_features.check_invariants ()
  then
    let all_defined_vars = Variable.Map.keys defined_vars in
    let all_vars_with_binding_times =
      Binding_time.Map.data binding_times |> Variable.Set.union_list
    in
    if not (Variable.Set.equal all_defined_vars all_vars_with_binding_times)
    then
      Misc.fatal_error
        "[defined_vars] and [binding_times] disagree on the set of variables \
         involved");
  { defined_vars; binding_times; equations; symbol_projections }

let equations t = t.equations

let symbol_projections t = t.symbol_projections

let add_symbol_projection t var proj =
  let symbol_projections = Variable.Map.add var proj t.symbol_projections in
  { t with symbol_projections }

let add_definition t var kind binding_time =
  if Flambda_features.check_invariants () && Variable.Map.mem var t.defined_vars
  then
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var print t;
  let binding_times =
    let vars =
      match Binding_time.Map.find binding_time t.binding_times with
      | exception Not_found -> Variable.Set.singleton var
      | prev_vars -> Variable.Set.add var prev_vars
    in
    Binding_time.Map.add binding_time vars t.binding_times
  in
  { t with
    defined_vars = Variable.Map.add var kind t.defined_vars;
    binding_times
  }

let add_or_replace_equation t name ty =
  More_type_creators.check_equation name ty;
  if TG.is_obviously_unknown ty
  then { t with equations = Name.Map.remove name t.equations }
  else { t with equations = Name.Map.add name ty t.equations }

let concat ~earlier:(t1 : t) ~later:(t2 : t) =
  let defined_vars =
    Variable.Map.union
      (fun var _data1 _data2 ->
        Misc.fatal_errorf
          "Cannot concatenate levels that have overlapping defined variables \
           (e.g. %a):@ %a@ and@ %a"
          Variable.print var print t1 print t2)
      t1.defined_vars t2.defined_vars
  in
  let binding_times =
    Binding_time.Map.union
      (fun _binding_time vars1 vars2 ->
        (* CR vlaviron: Technically this is feasible, as we can allow several
           variables with the same binding time, but it should only come from
           joins; concat arguments should always have disjoint binding time
           domains *)
        Misc.fatal_errorf
          "Cannot concatenate levels that have variables with overlapping \
           binding times (e.g. %a and %a):@ %a@ and@ %a"
          Variable.Set.print vars1 Variable.Set.print vars2 print t1 print t2)
      t1.binding_times t2.binding_times
  in
  let equations =
    (* We rely on the fact that equations in later levels are more precise *)
    Name.Map.union (fun _ _ty1 ty2 -> Some ty2) t1.equations t2.equations
  in
  let symbol_projections =
    Variable.Map.union
      (fun _var _proj1 proj2 -> Some proj2)
      t1.symbol_projections t2.symbol_projections
  in
  { defined_vars; binding_times; equations; symbol_projections }

let ids_for_export t =
  let variables = Variable.Map.keys t.defined_vars in
  let ids = Ids_for_export.create ~variables () in
  let equation name ty ids =
    let ids = Ids_for_export.union ids (TG.ids_for_export ty) in
    Ids_for_export.add_name ids name
  in
  let ids = Name.Map.fold equation t.equations ids in
  let symbol_projection var proj ids =
    let ids =
      Ids_for_export.union ids (Symbol_projection.ids_for_export proj)
    in
    Ids_for_export.add_variable ids var
  in
  Variable.Map.fold symbol_projection t.symbol_projections ids

let as_extension_without_bindings
    ({ defined_vars; binding_times; equations; symbol_projections } as t) =
  if Flambda_features.check_invariants ()
  then
    if Variable.Map.is_empty defined_vars
       && Binding_time.Map.is_empty binding_times
       && Variable.Map.is_empty symbol_projections
    then ()
    else
      Misc.fatal_errorf
        "Typing_env_level.as_extension_without_bindings:@ level %a has bindings"
        print t;
  TG.Env_extension.create ~equations
