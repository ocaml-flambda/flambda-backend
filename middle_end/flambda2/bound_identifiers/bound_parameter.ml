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

  let output chan t = print (Format.formatter_of_out_channel chan) t
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

module List = struct
  type nonrec t = t list

  let create params_and_kinds =
    List.map (fun (param, kind) -> create param kind) params_and_kinds

  let vars t = List.map var t

  let simples t = List.map simple t

  let equal_vars t1 t2 =
    List.length t1 = List.length t2
    && List.for_all2 (fun param1 var2 -> Variable.equal (var param1) var2) t1 t2

  let var_set t = Variable.Set.of_list (vars t)

  let name_set t = Name.Set.of_list (List.map Name.var (vars t))

  let rename t = List.map (fun t -> rename t) t

  let arity t = List.map (fun t -> Flambda_kind.With_subkind.kind (kind t)) t

  let arity_with_subkinds t = List.map (fun t -> kind t) t

  let equal t1 t2 = List.compare_lengths t1 t2 = 0 && List.for_all2 equal t1 t2

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print) t

  let free_names t =
    List.fold_left
      (fun result param -> Name_occurrences.union result (free_names param))
      Name_occurrences.empty t

  let apply_renaming t perm =
    List.map (fun param -> apply_renaming param perm) t

  let all_ids_for_export t =
    Ids_for_export.union_list (List.map all_ids_for_export t)
end
