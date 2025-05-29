(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { params : Bound_parameters.t;
    continuation : Continuation.t;
    args : Simple.t list
  }

let[@ocamlformat "disable"] print ppf { params; continuation; args } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(params@ %a)@]@ \
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(args@ %a)@]\
    )@]"
    Bound_parameters.print params
    Continuation.print continuation
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       Simple.print)
    args

let create ~params continuation args = { params; continuation; args }

let continuation { continuation; _ } = continuation

let apply { params; continuation; args } shortcut_args =
  let subst =
    List.fold_left2
      (fun subst param arg -> Variable.Map.add param arg subst)
      Variable.Map.empty
      (Bound_parameters.vars params)
      shortcut_args
  in
  ( continuation,
    List.map
      (fun arg ->
        Simple.pattern_match' arg
          ~var:(fun var ~coercion ->
            match Variable.Map.find var subst with
            | exception Not_found -> arg
            | simple -> Simple.apply_coercion_exn simple coercion)
          ~symbol:(fun _ ~coercion:_ -> arg)
          ~const:(fun _ -> arg))
      args )

let to_alias t =
  let params = Bound_parameters.simples t.params in
  if Misc.Stdlib.List.equal Simple.equal t.args params
  then Some t.continuation
  else None
