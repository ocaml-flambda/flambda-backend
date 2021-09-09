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

module EA = Continuation_extra_params_and_args.Extra_arg
module KP = Kinded_parameter
module Id = Apply_cont_rewrite_id

type used =
  | Used
  | Unused

type t =
  { original_params : KP.t list;
    used_params : KP.Set.t;
    used_extra_params : KP.t list;
    extra_args : (EA.t * used) list Id.Map.t
  }

let print_used ppf = function
  | Used -> ()
  | Unused -> Format.fprintf ppf "@ unused"

let print_ea_used ppf t =
  Format.fprintf ppf "(%a)"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (ea, used) ->
         Format.fprintf ppf "%a%a" EA.print ea print_used used))
    t

let [@ocamlformat "disable"] print ppf { original_params; used_params; used_extra_params;
                extra_args;
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(original_params@ (%a))@]@ \
      @[<hov 1>(used_params@ %a)@]@ \
      @[<hov 1>(used_extra_params@ (%a))@]@ \
      @[<hov 1>(extra_args@ %a)@]\
      )@]"
    KP.List.print original_params
    KP.Set.print used_params
    KP.List.print used_extra_params
    (Id.Map.print print_ea_used) extra_args

let does_nothing t =
  List.length t.original_params = KP.Set.cardinal t.used_params
  && Id.Map.is_empty t.extra_args

let create ~original_params ~used_params ~extra_params ~extra_args
    ~used_extra_params =
  (* CR mshinwell: check there weren't any duplicates in the param lists too *)
  if List.length original_params < KP.Set.cardinal used_params
  then
    Misc.fatal_errorf
      "Must have at least as many [original_params] (%a)@ as [used_params] (%a)"
      KP.List.print original_params KP.Set.print used_params;
  if List.length extra_params < KP.Set.cardinal used_extra_params
  then
    Misc.fatal_errorf
      "Must have at least as many [extra_params] (%a)@ as [used_extra_params] \
       (%a)"
      KP.List.print extra_params KP.Set.print used_extra_params;
  let extra_args =
    Id.Map.map
      (fun extra_args ->
        if List.compare_lengths extra_params extra_args <> 0
        then
          Misc.fatal_errorf
            "Lengths of [extra_params] (%a)@ and all [extra_args] (e.g. %a) \
             should be equal"
            KP.List.print extra_params
            Continuation_extra_params_and_args.Extra_arg.List.print extra_args;
        let extra_params_and_args = List.combine extra_params extra_args in
        List.map
          (fun (extra_param, extra_arg) ->
            ( extra_arg,
              if KP.Set.mem extra_param used_extra_params then Used else Unused
            ))
          extra_params_and_args)
      extra_args
  in
  let extra_args =
    if Id.Map.for_all (fun _ l -> l = []) extra_args
    then Id.Map.empty
    else extra_args
  in
  let used_extra_params =
    List.filter
      (fun extra_param -> KP.Set.mem extra_param used_extra_params)
      extra_params
  in
  { original_params; used_params; used_extra_params; extra_args }

let original_params t = t.original_params

let used_params t = t.used_params

let used_extra_params t = t.used_extra_params

let extra_args t id =
  match Id.Map.find id t.extra_args with
  | exception Not_found ->
    if List.length (used_extra_params t) <> 0
    then
      Misc.fatal_errorf
        "This [Apply_cont_rewrite] does not have any@ extra arguments for use \
         ID %a, but it has@ >= 1 extra parameter:@ %a"
        Id.print id print t;
    []
  | extra_args -> extra_args

let original_params_arity t = KP.List.arity_with_subkinds t.original_params
