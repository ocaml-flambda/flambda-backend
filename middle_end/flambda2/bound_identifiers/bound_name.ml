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

type t =
  { name : Name.t;
    name_mode : Name_mode.t
  }

let create name name_mode = { name; name_mode }

let name t = t.name

let name_mode t = t.name_mode

let var v =
  { name = Name.var (Bound_var.var v); name_mode = Bound_var.name_mode v }

let symbol sym = { name = Name.symbol sym; name_mode = Name_mode.normal }

let to_var t =
  Name.pattern_match t.name
    ~var:(fun var -> Some (Bound_var.create var t.name_mode))
    ~symbol:(fun _sym -> None)

let to_name t = t.name

let to_simple t = Simple.name t.name

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf { name; name_mode; } =
    Format.fprintf ppf "@[<hov 1>)\
        @[<hov 1>(name@ %a)@]@ \
        @[<hov 1>(name_mode@ %a)@]\
        )@]"
      Name.print name
      Name_mode.print name_mode

  let compare { name = name1; name_mode = name_mode1 }
      { name = name2; name_mode = name_mode2 } =
    let c = Name.compare name1 name2 in
    if c <> 0 then c else Name_mode.compare_total_order name_mode1 name_mode2

  let equal t1 t2 = compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let is_symbol t = Name.is_symbol t.name

let must_be_symbol t = Name.must_be_symbol t.name

let rename t =
  Name.pattern_match t.name
    ~var:(fun var -> { t with name = Name.var (Variable.rename var) })
    ~symbol:(fun _ -> t)
