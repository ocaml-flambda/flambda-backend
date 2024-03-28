(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2023 OCamlPro SAS                                    *)
(*   Copyright 2023--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type one_recursive_handler =
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

type non_recursive_handler =
  { cont : Continuation.t;
    params : Bound_parameters.t;
    lifted_params : Lifted_cont_params.t;
    handler : Flambda.Expr.t;
    is_exn_handler : bool;
    is_cold : bool
  }

type original_handlers =
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Lifted_cont_params.t;
        continuation_handlers : one_recursive_handler Continuation.Map.t
      }
  | Non_recursive of non_recursive_handler

let add_params_to_lift t params_to_lift =
  let lifted_params, renaming = Lifted_cont_params.rename params_to_lift in
  match t with
  | Recursive
      ({ lifted_params = old_lifted; continuation_handlers; _ } as recursive) ->
    if Lifted_cont_params.is_empty old_lifted
    then
      let continuation_handlers =
        Continuation.Map.map
          (fun (one_rec_handler : one_recursive_handler) ->
            let handler =
              Flambda.Expr.apply_renaming one_rec_handler.handler renaming
            in
            { one_rec_handler with handler })
          continuation_handlers
      in
      Recursive { recursive with lifted_params; continuation_handlers }
    else
      Misc.fatal_errorf
        "Cannot add lifted params to a continuation that already has lifted \
         params"
  | Non_recursive ({ lifted_params = old_lifted; handler; _ } as non_rec) ->
    if Lifted_cont_params.is_empty old_lifted
    then
      let handler = Flambda.Expr.apply_renaming handler renaming in
      Non_recursive { non_rec with lifted_params; handler }
    else
      Misc.fatal_errorf
        "Cannot add lifted params to a continuation that already has lifted \
         params"

let print_one_recursive_handler ppf
    ({ params; handler; is_cold } : one_recursive_handler) =
  Format.fprintf ppf
    "@[<hov 1>(@[<hv 1>(params@ %a)@]@ @[<hv 1>(is_cold@ %b)@]@ @[<hv \
     1>(handler@ %a)@])@]"
    Bound_parameters.print params is_cold Flambda.Expr.print handler

let print_non_recursive_handler ppf
    { cont; params; lifted_params; handler; is_exn_handler; is_cold } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hv 1>(cont@ %a)@]@ @[<hv 1>(params@ %a)@]@ @[<hv \
     1>(lifted_params@ %a)@]@ @[<hv 1>(is_exn_handler@ %b)@]@ @[<hv \
     1>(is_cold@ %b)@]@ @[<hv 1>(handler@ %a)@]@ )@]"
    Continuation.print cont Bound_parameters.print params
    Lifted_cont_params.print lifted_params is_exn_handler is_cold
    Flambda.Expr.print handler

let print_original_handlers ppf original_handlers =
  match (original_handlers : original_handlers) with
  | Non_recursive non_rec_handler ->
    print_non_recursive_handler ppf non_rec_handler
  | Recursive { invariant_params; lifted_params; continuation_handlers } ->
    Format.fprintf ppf
      "@[<hov 1>(@[<hv 1>(invariant params@ %a)@]@ @[<hv 1>(lifted_params@ \
       %a)@]@ @[<hv 1>(continuation handlers@ %a)@])@]"
      Bound_parameters.print invariant_params Lifted_cont_params.print
      lifted_params
      (Continuation.Map.print print_one_recursive_handler)
      continuation_handlers
