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

type t =
  { var : Variable.t;
    name_mode : Name_mode.t
  }

let create var name_mode = { var; name_mode }

let var t = t.var

let name_mode t = t.name_mode

let with_var t var = { t with var }

let with_name_mode t name_mode = { t with name_mode }

let rename t = with_var t (Variable.rename t.var)

let apply_renaming t perm = with_var t (Renaming.apply_variable perm t.var)

let free_names t = Name_occurrences.singleton_variable t.var t.name_mode

let all_ids_for_export { var; name_mode = _ } =
  Ids_for_export.add_variable Ids_for_export.empty var

include Container_types.Make (struct
  type nonrec t = t

  (* let [@ocamlformat "disable"] print ppf { var; name_mode; } = Format.fprintf
     ppf "@[<hov 1>(\ @[<hov 1>(var@ %a)@]@ \ @[<hov 1>(name_mode@ %a)@]\ )@]"
     Variable.print var Name_mode.print name_mode *)

  let [@ocamlformat "disable"] print ppf { var; name_mode; } =
    match Name_mode.descr name_mode with
    | Normal -> Variable.print ppf var
    | In_types -> Format.fprintf ppf "@[%a\u{1d749}@]" Variable.print var
    | Phantom -> Variable.print ppf var
  (* | Phantom -> Format.fprintf ppf "@[%a\u{1f47b}@]" Variable.print var *)

  let compare { var = var1; name_mode = name_mode1 }
      { var = var2; name_mode = name_mode2 } =
    let c = Variable.compare var1 var2 in
    if c <> 0 then c else Name_mode.compare_total_order name_mode1 name_mode2

  let equal t1 t2 = compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let add_to_name_permutation { var; name_mode = _ } ~guaranteed_fresh perm =
  let { var = guaranteed_fresh; name_mode = _ } = guaranteed_fresh in
  Renaming.add_fresh_variable perm var ~guaranteed_fresh

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty
