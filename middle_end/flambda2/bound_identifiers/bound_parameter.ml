(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Simple = Reg_width_things.Simple

type t =
  { param : Variable.t;
    kind : Flambda_kind.With_subkind.t
  }

include Container_types.Make (struct
  type nonrec t = t

  let compare { param = param1; kind = kind1 } { param = param2; kind = kind2 }
      =
    let c = Variable.compare param1 param2 in
    if c <> 0 then c else Flambda_kind.With_subkind.compare kind1 kind2

  let equal t1 t2 = compare t1 t2 = 0

  let hash { param; kind } =
    Hashtbl.hash (Variable.hash param, Flambda_kind.With_subkind.hash kind)

  let [@ocamlformat "disable"] print ppf { param; kind; } =
    Format.fprintf ppf "@[(@<0>%s%a@<0>%s @<1>\u{2237} %a)@]"
      (Flambda_colours.parameter ())
      Variable.print param
      (Flambda_colours.normal ())
      Flambda_kind.With_subkind.print kind
end)

let create param kind = { param; kind }

let var t = t.param

let name t = Name.var (var t)

let simple t = Simple.var (var t)

let kind t = t.kind

let with_kind t kind = { t with kind }

let rename t = { t with param = Variable.rename t.param }

let equal_kinds t1 t2 = Flambda_kind.With_subkind.equal t1.kind t2.kind

let free_names ({ param = _; kind = _ } as t) =
  Name_occurrences.singleton_variable (var t) Name_mode.normal

let apply_renaming ({ param = _; kind } as t) perm =
  Name.pattern_match
    (Renaming.apply_name perm (name t))
    ~var:(fun var -> create var kind)
    ~symbol:(fun _ ->
      Misc.fatal_errorf "Illegal name permutation on [Bound_parameter]: %a"
        Renaming.print perm)

let all_ids_for_export { param; kind = _ } =
  Ids_for_export.add_variable Ids_for_export.empty param
