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

type t =
  { name : Name.t;
    name_mode : Name_mode.t
  }

let [@ocamlformat "disable"] print ppf { name; name_mode; } =
  Format.fprintf ppf "@[<hov 1>)\
      @[<hov 1>(name@ %a)@]@ \
      @[<hov 1>(name_mode@ %a)@]\
      )@]"
    Name.print name
    Name_mode.print name_mode

let create name name_mode =
  (* See note about name modes in [Bound_var.create]. *)
  { name; name_mode }

let create_var v =
  { name = Name.var (Bound_var.var v); name_mode = Bound_var.name_mode v }

let create_symbol sym = { name = Name.symbol sym; name_mode = Name_mode.normal }

let name t = t.name

let name_mode t = t.name_mode

let is_symbol t = Name.is_symbol t.name
