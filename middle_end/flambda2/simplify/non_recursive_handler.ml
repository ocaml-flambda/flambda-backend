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
  { cont : Continuation.t;
    params : Bound_parameters.t;
    lifted_params : Lifted_cont_params.t;
    handler : Flambda.Expr.t;
    is_exn_handler : bool;
    is_cold : bool
  }

let create ~cont ~params ~lifted_params ~handler ~is_exn_handler ~is_cold =
  { cont; params; lifted_params; handler; is_exn_handler; is_cold }

let print ppf { cont; params; lifted_params; handler; is_exn_handler; is_cold }
    =
  Format.fprintf ppf
    "@[<hov 1>(@[<hv 1>(cont@ %a)@]@ @[<hv 1>(params@ %a)@]@ @[<hv \
     1>(lifted_params@ %a)@]@ @[<hv 1>(is_exn_handler@ %b)@]@ @[<hv \
     1>(is_cold@ %b)@]@ @[<hv 1>(handler@ %a)@]@ )@]"
    Continuation.print cont Bound_parameters.print params
    Lifted_cont_params.print lifted_params is_exn_handler is_cold
    Flambda.Expr.print handler
