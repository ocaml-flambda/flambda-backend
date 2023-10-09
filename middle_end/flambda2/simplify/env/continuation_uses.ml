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
module U = One_continuation_use

type t =
  { continuation : Continuation.t;
    arity : [`Unarized] Flambda_arity.t;
    uses : U.t list
  }

let create continuation arity = { continuation; arity; uses = [] }

let [@ocamlformat "disable"] print ppf { continuation; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(arity@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Continuation.print continuation
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space U.print) uses

let add_use t kind ~env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal_ignoring_subkinds arity t.arity)
    then
      Misc.fatal_errorf
        "Arity of use (%a) doesn't match continuation's arity (%a)"
        Flambda_arity.print arity Flambda_arity.print t.arity;
    let use = U.create kind ~env_at_use id ~arg_types in
    { t with uses = use :: t.uses }
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf
      "\n\
       %tContext is:%t adding use of %a with arg types@ (%a);@ existing uses:@ \
       %a; environment:@ %a"
      Flambda_colours.error Flambda_colours.pop Continuation.print
      t.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      arg_types print t DE.print env_at_use;
    Printexc.raise_with_backtrace Misc.Fatal_error bt

let union t1 t2 =
  assert (Continuation.equal t1.continuation t2.continuation);
  assert (Flambda_arity.equal_ignoring_subkinds t1.arity t2.arity);
  { continuation = t1.continuation; arity = t1.arity; uses = t1.uses @ t2.uses }

let number_of_uses t = List.length t.uses

let arity t = t.arity

let get_uses t = t.uses

type arg_at_use =
  { arg_type : Flambda2_types.t;
    typing_env : Flambda2_types.Typing_env.t
  }

type arg_types_by_use_id = arg_at_use Apply_cont_rewrite_id.Map.t list

let add_value_to_arg_map arg_map arg_type ~use =
  let env_at_use = U.env_at_use use in
  let typing_env = DE.typing_env env_at_use in
  let arg_at_use : arg_at_use = { arg_type; typing_env } in
  Apply_cont_rewrite_id.Map.add (U.id use) arg_at_use arg_map

let add_uses_to_arg_maps arg_maps uses =
  List.fold_left
    (fun arg_maps use ->
      let arg_types = U.arg_types use in
      fst
        (Misc.Stdlib.List.map2_prefix
           (fun arg_map arg_type -> add_value_to_arg_map arg_map arg_type ~use)
           arg_maps arg_types))
    arg_maps uses

let empty_arg_maps arity : arg_types_by_use_id =
  List.map
    (fun _ -> Apply_cont_rewrite_id.Map.empty)
    (Flambda_arity.unarized_components arity)

let get_arg_types_by_use_id t =
  add_uses_to_arg_maps (empty_arg_maps t.arity) t.uses

(* We want to get the arg_types_by_use_id for the invariant params only of a
   mutually-recursive continuation group. In this case, the arguments we want
   are a prefix of the actual argument lists. *)
let get_arg_types_by_use_id_for_invariant_params arity l =
  List.fold_left
    (fun arg_maps t ->
      if not
           (Misc.Stdlib.List.is_prefix ~equal:Flambda_kind.equal
              (Flambda_arity.unarized_components arity
              |> List.map Flambda_kind.With_subkind.kind)
              ~of_:
                (Flambda_arity.unarized_components t.arity
                |> List.map Flambda_kind.With_subkind.kind))
      then
        Misc.fatal_errorf
          "Arity of invariant params@ (%a) is not a prefix of the arity of the \
           continuation uses@ (%a)"
          Flambda_arity.print arity Flambda_arity.print t.arity;
      add_uses_to_arg_maps arg_maps t.uses)
    (empty_arg_maps arity) l

let get_use_ids t =
  List.fold_left
    (fun uses use -> Apply_cont_rewrite_id.Set.add (U.id use) uses)
    Apply_cont_rewrite_id.Set.empty t.uses

let get_typing_env_no_more_than_one_use t =
  match t.uses with
  | [] -> None
  | [use] -> Some (DE.typing_env (U.env_at_use use))
  | _ :: _ ->
    Misc.fatal_errorf "Only zero or one continuation use(s) expected:@ %a" print
      t

let mark_non_inlinable t =
  { t with uses = List.map U.mark_non_inlinable t.uses }
