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

module Extra_arg = struct
  type t =
    | Already_in_scope of Simple.t
    | New_let_binding of Variable.t * Flambda_primitive.t
    | New_let_binding_with_named_args of
        Variable.t * (Simple.t list -> Flambda_primitive.t)

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Already_in_scope simple ->
      Format.fprintf ppf "@[<hov 1>(Already_in_scope@ %a)@]"
        Simple.print simple
    | New_let_binding (var, prim) ->
      Format.fprintf ppf "@[<hov 1>(New_let_binding@ %a@ %a)@]"
        Variable.print var
        Flambda_primitive.print prim
    | New_let_binding_with_named_args (var, _) ->
      Format.fprintf ppf "@[<hov 1>(New_let_binding_with_named_args@ %a@ <fun>)@]"
        Variable.print var

  module List = struct
    type nonrec t = t list

    let [@ocamlformat "disable"] print ppf t =
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print) t
  end
end

type t =
  | Empty
  | Non_empty of
      { extra_params : Bound_parameters.t;
        extra_args : Extra_arg.t list Or_invalid.t Apply_cont_rewrite_id.Map.t
      }

let [@ocamlformat "disable"] print ppf = function
  | Empty -> Format.fprintf ppf "(empty)"
  | Non_empty { extra_params; extra_args; } ->
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(extra_params@ %a)@]@ \
        @[<hov 1>(extra_args@ %a)@]\
        )@]"
      Bound_parameters.print extra_params
      (Apply_cont_rewrite_id.Map.print (Or_invalid.print Extra_arg.List.print)) extra_args

let empty = Empty

let is_empty = function Empty -> true | Non_empty _ -> false

let add t ~invalids ~extra_param ~extra_args =
  if not
       (Apply_cont_rewrite_id.Set.is_empty
          (Apply_cont_rewrite_id.Set.inter invalids
             (Apply_cont_rewrite_id.Map.keys extra_args)))
  then
    Misc.fatal_errorf
      "Broken invariants: when adding an extra param to a continuation, every \
       Apply_cont_rewrite_id should either have a valid extra arg, or be \
       invalid, but not both:@ %a@ %a"
      Apply_cont_rewrite_id.Set.print invalids
      (Apply_cont_rewrite_id.Map.print Extra_arg.print)
      extra_args;
  match t with
  | Empty ->
    let extra_params = Bound_parameters.create [extra_param] in
    let valid_extra_args =
      Apply_cont_rewrite_id.Map.map
        (fun extra_args -> Or_invalid.Ok [extra_args])
        extra_args
    in
    let extra_args =
      Apply_cont_rewrite_id.Set.fold
        (fun id map -> Apply_cont_rewrite_id.Map.add id Or_invalid.Invalid map)
        invalids valid_extra_args
    in
    Non_empty { extra_params; extra_args }
  | Non_empty { extra_params; extra_args = already_extra_args } ->
    let extra_params = Bound_parameters.cons extra_param extra_params in
    let extra_args =
      Apply_cont_rewrite_id.Map.merge
        (fun id already_extra_args extra_args ->
          match already_extra_args, extra_args with
          | None, None -> None
          | None, Some _ ->
            Misc.fatal_errorf "Cannot change domain: %a"
              Apply_cont_rewrite_id.print id
          | Some _, None ->
            if Apply_cont_rewrite_id.Set.mem id invalids
            then Some Or_invalid.Invalid
            else
              Misc.fatal_errorf "Cannot change domain: %a"
                Apply_cont_rewrite_id.print id
          | Some Or_invalid.Invalid, Some _ -> Some Or_invalid.Invalid
          | Some (Or_invalid.Ok already_extra_args), Some extra_arg ->
            Some (Or_invalid.Ok (extra_arg :: already_extra_args)))
        already_extra_args extra_args
    in
    Non_empty { extra_params; extra_args }

let replace_extra_args t extra_args =
  match t with
  | Empty -> Empty
  | Non_empty { extra_params; _ } -> Non_empty { extra_params; extra_args }

let concat ~outer:t1 ~inner:t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | Non_empty t1, Non_empty t2 ->
    let extra_args =
      Apply_cont_rewrite_id.Map.merge
        (fun id extra_args1 extra_args2 ->
          match extra_args1, extra_args2 with
          | None, None -> None
          | Some _, None | None, Some _ ->
            Misc.fatal_errorf "concat: mismatching domains on id %a"
              Apply_cont_rewrite_id.print id
          | Some Or_invalid.Invalid, Some _ | Some _, Some Or_invalid.Invalid ->
            Some Or_invalid.Invalid
          | Some (Or_invalid.Ok extra_args1), Some (Or_invalid.Ok extra_args2)
            ->
            Some (Or_invalid.Ok (extra_args1 @ extra_args2)))
        t1.extra_args t2.extra_args
    in
    Non_empty
      { extra_params = Bound_parameters.append t1.extra_params t2.extra_params;
        extra_args
      }

let extra_params = function
  | Empty -> Bound_parameters.empty
  | Non_empty { extra_params; _ } -> extra_params

let extra_args = function
  | Empty -> Apply_cont_rewrite_id.Map.empty
  | Non_empty { extra_args; _ } -> extra_args
