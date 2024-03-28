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

module DE = Downwards_env
module T = Flambda2_types

type t =
  { id : Apply_cont_rewrite_id.t;
    kind : Continuation_use_kind.t;
    arg_types : T.t list;
    env : DE.t
  }

let create kind ~env_at_use:env id ~arg_types = { id; kind; arg_types; env }

let [@ocamlformat "disable"] print ppf { env = _; id; kind = _; arg_types; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(id %a)@]@ \
      @[<hov 1>(arg_types@ %a)@]@ \
      )@]"
    Apply_cont_rewrite_id.print id
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda2_types.print)
    arg_types

let id t = t.id

let use_kind t = t.kind

let arg_types t = t.arg_types

let env_at_use t = t.env

let mark_non_inlinable t =
  match t.kind with
  | Inlinable -> { t with kind = Non_inlinable { escaping = false } }
  | Non_inlinable _ -> t
