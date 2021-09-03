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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  equations : Type_grammar.t Name.Map.t;
}[@@unboxed]

let print_equations ppf equations =
  let equations = Name.Map.bindings equations in
  match equations with
  | [] -> Format.pp_print_string ppf "()"
  | _::_ ->
    Format.pp_print_string ppf "(";
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (name, ty) ->
        Format.fprintf ppf
          "@[<hov 1>%a@ :@ %a@]"
          Name.print name
          Type_grammar.print ty)
      ppf equations;
    Format.pp_print_string ppf ")"

let print ppf t =
  Format.fprintf ppf
    "@[<hov 1>(equations@ @[<v 1>%a@])@]"
    print_equations t.equations

let fold ~equation t acc =
  Name.Map.fold equation t.equations acc

let invariant { equations; } =
  if Flambda_features.check_invariants () then
    Name.Map.iter Type_grammar.check_equation equations

let empty () = { equations = Name.Map.empty; }

let is_empty { equations } = Name.Map.is_empty equations

let from_map equations =
  let t = { equations; } in
  invariant t; t

let one_equation name ty =
  Type_grammar.check_equation name ty;
  { equations = Name.Map.singleton name ty; }

let add_or_replace_equation t name ty =
  Type_grammar.check_equation name ty;
  if Flambda_features.check_invariants ()
  && Name.Map.mem name t.equations
  then begin
    Format.eprintf
      "Warning: Overriding equation for name %a@\n\
       Old equation is@ @[%a@]@\n\
       New equation is@ @[%a@]@."
      Name.print name
      Type_grammar.print (Name.Map.find name t.equations)
      Type_grammar.print ty
  end;
  { equations = Name.Map.add name ty t.equations; }

let all_ids_for_export { equations; } =
  Name.Map.fold (fun name ty acc ->
    let acc = Ids_for_export.union (Type_grammar.all_ids_for_export ty) acc in
    Ids_for_export.add_name acc name
  ) equations Ids_for_export.empty

let free_names { equations; } =
  Name.Map.fold (fun name ty acc ->
    let acc = Name_occurrences.union acc (Type_grammar.free_names ty) in
    Name_occurrences.add_name acc name Name_mode.in_types
  ) equations Name_occurrences.empty

let apply_renaming ({ equations; } as t) perm =
  let changed = ref false in
  let equations' =
    Name.Map.fold (fun name ty acc ->
      let ty' = Type_grammar.apply_renaming ty perm in
      let name' = Renaming.apply_name perm name in
      if not (ty == ty' && name == name') then changed := true;
      Name.Map.add name' ty' acc
    ) equations Name.Map.empty
  in
  if !changed then { equations = equations'; } else t

exception Bottom_meet

let rec meet0 env (t1 : t) (t2 : t) extra_extensions =
  (* A symmetrical meet would be hard to implement, as
     the inner meets can produce extra extensions that need
     to be merged with the result.

     To get around this, we'll suppose that [t2] is smaller than [t1]
     and add equations from [t2] to [t1], along with all extra equations
  *)
  let equations, extra_extensions =
    Name.Map.fold (fun name ty (eqs, extra_extensions) ->
        match Name.Map.find_opt name eqs with
        | None ->
          Type_grammar.check_equation name ty;
          Name.Map.add name ty eqs, extra_extensions
        | Some ty0 ->
          begin match Type_grammar.meet env ty0 ty with
          | Bottom -> raise Bottom_meet
          | Ok (ty, new_ext) ->
            Type_grammar.check_equation name ty;
            Name.Map.add (*replace*) name ty eqs, new_ext :: extra_extensions
          end)
      t2.equations
      (t1.equations, extra_extensions)
  in
  let ext = { equations; } in
  match extra_extensions with
  | [] -> ext
  | new_ext :: extra_extensions ->
    (* CR vlaviron: It's a bad idea to drop the extensions in the general case,
       but since we lack the property that the new extensions are stricter than
       the existing ones we can get into an infinite loop here
       (see flambdatest/unit_test/extension_meet.ml, function
       test_double_recursion for an example).

       This is very uncommon though (it needs recursive types involving at least
       three different names), so for now we still do the meet systematically.
    *)
    meet0 env ext new_ext extra_extensions

let meet env t1 t2 : _ Or_bottom.t =
  try
    Ok (meet0 env t1 t2 [])
  with Bottom_meet -> Bottom

let join env t1 t2 =
  let equations =
    Name.Map.merge (fun name ty1_opt ty2_opt ->
        match ty1_opt, ty2_opt with
        | None, _ | _, None -> None
        | Some ty1, Some ty2 ->
          begin match Type_grammar.join env ty1 ty2 with
          | Known ty ->
            if Type_grammar.is_alias_of_name ty name then
              (* This is rare but not anomalous. It may mean that [ty1] and [ty2]
                 are both alias types which canonicalize to [name], for instance.
                 In any event, if the best type available for [name] is [= name],
                 we effectively know nothing, so we drop [name]. ([name = name]
                 would be rejected by [Typing_env.add_equation] anyway.) *)
              None
            else begin
              (* This should always pass due to the [is_alias_of_name] check. *)
              Type_grammar.check_equation name ty;
              Some ty
            end
          | Unknown -> None
          end)
      t1.equations
      t2.equations
  in
  { equations; }

module With_extra_variables = struct
  type t = {
    existential_vars : Flambda_kind.t Variable.Map.t;
    equations : Type_grammar.t Name.Map.t;
  }

  let print ppf { existential_vars; equations; } =
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(variables@ @[<hov 1>%a@])@]\
       @[<hov 1>(equations@ @[<v 1>%a@])@])@ \
       @]"
      (Variable.Map.print Flambda_kind.print) existential_vars
      print_equations equations

  let fold ~variable ~equation t acc =
    let acc = Variable.Map.fold variable t.existential_vars acc in
    Name.Map.fold equation t.equations acc

  let empty () =
    { existential_vars = Variable.Map.empty;
      equations = Name.Map.empty;
    }

  let add_definition t var kind ty =
    let existential_vars = Variable.Map.add var kind t.existential_vars in
    let equations = Name.Map.add (Name.var var) ty t.equations in
    { existential_vars;
      equations;
    }

  let add_or_replace_equation t name ty =
    Type_grammar.check_equation name ty;
    { existential_vars = t.existential_vars;
      equations = Name.Map.add name ty t.equations;
    }
end
