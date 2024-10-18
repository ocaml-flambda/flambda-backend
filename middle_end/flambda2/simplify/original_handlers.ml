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
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Lifted_cont_params.t;
        continuation_handlers : One_recursive_handler.t Continuation.Map.t
      }
  | Non_recursive of Non_recursive_handler.t

let print ppf t =
  match t with
  | Non_recursive non_rec_handler ->
    Non_recursive_handler.print ppf non_rec_handler
  | Recursive { invariant_params; lifted_params; continuation_handlers } ->
    Format.fprintf ppf
      "@[<hov 1>(@[<hv 1>(invariant params@ %a)@]@ @[<hv 1>(lifted_params@ \
       %a)@]@ @[<hv 1>(continuation handlers@ %a)@])@]"
      Bound_parameters.print invariant_params Lifted_cont_params.print
      lifted_params
      (Continuation.Map.print One_recursive_handler.print)
      continuation_handlers

let create_recursive ~invariant_params ~lifted_params ~continuation_handlers =
  Recursive { invariant_params; lifted_params; continuation_handlers }

let create_non_recursive non_rec_handler = Non_recursive non_rec_handler

let add_params_to_lift t params_to_lift =
  let lifted_params, renaming = Lifted_cont_params.rename params_to_lift in
  let[@inline] fail () =
    Misc.fatal_errorf
      "Cannot add lifted params to a continuation that already has lifted \
       params:@ params_to_lift=%a@ t=%a"
      Lifted_cont_params.print params_to_lift print t
  in
  match t with
  | Recursive
      ({ lifted_params = old_lifted; continuation_handlers; _ } as recursive) ->
    if Lifted_cont_params.is_empty old_lifted
    then
      let continuation_handlers =
        Continuation.Map.map
          (fun ({ params; handler; is_cold } : One_recursive_handler.t) ->
            let handler = Flambda.Expr.apply_renaming handler renaming in
            One_recursive_handler.create ~params ~handler ~is_cold)
          continuation_handlers
      in
      Recursive { recursive with lifted_params; continuation_handlers }
    else fail ()
  | Non_recursive
      { cont;
        params;
        lifted_params = old_lifted;
        handler;
        is_exn_handler;
        is_cold
      } ->
    if Lifted_cont_params.is_empty old_lifted
    then
      let handler = Flambda.Expr.apply_renaming handler renaming in
      let non_rec =
        Non_recursive_handler.create ~cont ~params ~lifted_params ~handler
          ~is_exn_handler ~is_cold
      in
      Non_recursive non_rec
    else fail ()
