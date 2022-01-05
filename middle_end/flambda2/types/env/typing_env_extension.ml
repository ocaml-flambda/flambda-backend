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

[@@@ocaml.warning "+a-30-40-41-42"]

module TG = Type_grammar

type t = TG.Env_extension.t

let fold ~equation ({ equations } : t) acc =
  Name.Map.fold equation equations acc

let invariant ({ equations } : t) =
  if Flambda_features.check_invariants ()
  then Name.Map.iter More_type_creators.check_equation equations

let empty = TG.Env_extension.empty

let is_empty ({ equations } : t) = Name.Map.is_empty equations

let from_map equations =
  let t = TG.Env_extension.create ~equations in
  invariant t;
  t

let to_map ({ equations } : t) = equations

let one_equation name ty =
  More_type_creators.check_equation name ty;
  TG.Env_extension.create ~equations:(Name.Map.singleton name ty)

let add_or_replace_equation ({ equations } : t) name ty =
  More_type_creators.check_equation name ty;
  if Flambda_features.check_invariants () && Name.Map.mem name equations
  then
    Format.eprintf
      "Warning: Overriding equation for name %a@\n\
       Old equation is@ @[%a@]@\n\
       New equation is@ @[%a@]@." Name.print name TG.print
      (Name.Map.find name equations)
      TG.print ty;
  TG.Env_extension.create ~equations:(Name.Map.add name ty equations)

let replace_equation ({ equations } : t) name ty =
  TG.Env_extension.create
    ~equations:(Name.Map.add (* replace *) name ty equations)

let all_ids_for_export = TG.Env_extension.all_ids_for_export

let apply_renaming = TG.Env_extension.apply_renaming

let free_names = TG.Env_extension.free_names

let print = TG.Env_extension.print

module With_extra_variables = struct
  type t =
    { existential_vars : Flambda_kind.t Variable.Map.t;
      equations : TG.t Name.Map.t
    }

  let print ppf { existential_vars; equations } =
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(variables@ @[<hov 1>%a@])@]@ @[<hov 1>%a@])@ @]"
      (Variable.Map.print Flambda_kind.print)
      existential_vars TG.Env_extension.print
      (TG.Env_extension.create ~equations)

  let fold ~variable ~equation t acc =
    let acc = Variable.Map.fold variable t.existential_vars acc in
    Name.Map.fold equation t.equations acc

  let empty =
    { existential_vars = Variable.Map.empty; equations = Name.Map.empty }

  let add_definition t var kind ty =
    let existential_vars = Variable.Map.add var kind t.existential_vars in
    let equations = Name.Map.add (Name.var var) ty t.equations in
    { existential_vars; equations }

  let add_or_replace_equation t name ty =
    More_type_creators.check_equation name ty;
    { existential_vars = t.existential_vars;
      equations = Name.Map.add name ty t.equations
    }

  let free_names { existential_vars; equations } =
    let variables = Variable.Map.keys existential_vars in
    let free_names =
      Name_occurrences.create_variables variables Name_mode.in_types
    in
    Name.Map.fold
      (fun name ty free_names ->
        let free_names =
          Name_occurrences.add_name free_names name Name_mode.in_types
        in
        Name_occurrences.union free_names (TG.free_names ty))
      equations free_names

  let apply_renaming { existential_vars; equations } renaming =
    let existential_vars =
      Variable.Map.fold
        (fun var kind result ->
          let var' = Renaming.apply_variable renaming var in
          Variable.Map.add var' kind result)
        existential_vars Variable.Map.empty
    in
    let equations =
      Name.Map.fold
        (fun name ty result ->
          let name' = Renaming.apply_name renaming name in
          let ty' = TG.apply_renaming ty renaming in
          Name.Map.add name' ty' result)
        equations Name.Map.empty
    in
    { existential_vars; equations }

  let all_ids_for_export { existential_vars; equations } =
    let variables = Variable.Map.keys existential_vars in
    let ids = Ids_for_export.create ~variables () in
    Name.Map.fold
      (fun name ty ids ->
        let ids = Ids_for_export.add_name ids name in
        Ids_for_export.union ids (TG.all_ids_for_export ty))
      equations ids

  let existential_vars { existential_vars; _ } =
    Variable.Map.keys existential_vars

  let map_types { existential_vars; equations } ~f =
    let equations = Name.Map.map f equations in
    { existential_vars; equations }
end
