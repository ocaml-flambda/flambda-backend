(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

let create ~params ~handler ~is_cold = { params; handler; is_cold }

let print ppf { params; handler; is_cold } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hv 1>(params@ %a)@]@ @[<hv 1>(is_cold@ %b)@]@ @[<hv \
     1>(handler@ %a)@])@]"
    Bound_parameters.print params is_cold Flambda.Expr.print handler
