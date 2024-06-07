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

module EPA = Continuation_extra_params_and_args
module EA = EPA.Extra_arg
module Id = Apply_cont_rewrite_id

type used =
  | Unused
  | Used
  | Used_as_invariant

type t =
  { original_params_usage : used list;
    extra_params_usage : used list;
    extra_args : EA.t list Or_invalid.t Id.Map.t;
    original_params : Bound_parameters.t;
    extra_params : Bound_parameters.t
  }

let print_used ppf = function
  | Unused -> Format.fprintf ppf "unused"
  | Used -> Format.fprintf ppf "used"
  | Used_as_invariant -> Format.fprintf ppf "invariant"

let print_params_used ppf (params, usage) =
  Format.fprintf ppf "(%a)"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun ppf (param, used) ->
         Format.fprintf ppf "(%a:@ %a)" Bound_parameter.print param print_used
           used))
    (List.combine (Bound_parameters.to_list params) usage)

let [@ocamlformat "disable"] print ppf
  { original_params_usage; extra_params_usage; extra_args;
    original_params; extra_params } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(original_params@ (%a))@]@ \
      @[<hov 1>(extra_params@ %a)@]@ \
      @[<hov 1>(extra_args@ %a)@]\
      )@]"
    print_params_used (original_params, original_params_usage)
    print_params_used (extra_params, extra_params_usage)
    (Id.Map.print (Or_invalid.print
       (Format.pp_print_list ~pp_sep:Format.pp_print_space EA.print)))
    extra_args

let does_nothing t =
  List.for_all
    (function Used -> true | Used_as_invariant | Unused -> false)
    t.original_params_usage
  && List.for_all
       (function Unused -> true | Used | Used_as_invariant -> false)
       t.extra_params_usage

let create ~original_params ~extra_params_and_args ~decide_param_usage =
  let extra_params = EPA.extra_params extra_params_and_args in
  let extra_args = EPA.extra_args extra_params_and_args in
  Bound_parameters.check_no_duplicates original_params;
  Bound_parameters.check_no_duplicates extra_params;
  let original_params_usage =
    List.map decide_param_usage (Bound_parameters.to_list original_params)
  in
  let extra_params_usage =
    List.map decide_param_usage (Bound_parameters.to_list extra_params)
  in
  { original_params_usage;
    extra_params_usage;
    extra_args;
    original_params;
    extra_params
  }

let original_params_arity t = Bound_parameters.arity t.original_params

let rec partition_used l usage =
  match l, usage with
  | [], [] -> [], []
  | [], _ :: _ | _ :: _, [] ->
    Misc.fatal_error
      "Apply_cont_rewrite.partition_used: list and usage must have the same \
       length"
  | _ :: l, Unused :: usage -> partition_used l usage
  | x :: l, Used :: usage ->
    let invariant, normal = partition_used l usage in
    invariant, x :: normal
  | x :: l, Used_as_invariant :: usage ->
    let invariant, normal = partition_used l usage in
    x :: invariant, normal

let rec get_unused l usage =
  match l, usage with
  | [], [] -> []
  | [], _ :: _ | _ :: _, [] ->
    Misc.fatal_errorf
      "Apply_cont_rewrite.get_unused: list and usage must have the same length"
  | x :: l, Unused :: usage -> x :: get_unused l usage
  | _ :: l, (Used | Used_as_invariant) :: usage -> get_unused l usage

let get_used_params rewrite =
  let invariant_params, variant_params =
    partition_used
      (Bound_parameters.to_list rewrite.original_params)
      rewrite.original_params_usage
  in
  let invariant_extra_params, variant_extra_params =
    partition_used
      (Bound_parameters.to_list rewrite.extra_params)
      rewrite.extra_params_usage
  in
  ( Bound_parameters.create (invariant_params @ invariant_extra_params),
    Bound_parameters.create (variant_params @ variant_extra_params) )

let get_unused_params rewrite =
  let unused_params =
    get_unused
      (Bound_parameters.to_list rewrite.original_params)
      rewrite.original_params_usage
  in
  let unused_extra_params =
    get_unused
      (Bound_parameters.to_list rewrite.extra_params)
      rewrite.extra_params_usage
  in
  Bound_parameters.create (unused_params @ unused_extra_params)

type rewrite_apply_cont_ctx =
  | Apply_cont
  | Apply_expr of Simple.t list

let extra_args_list rewrite id =
  try Id.Map.find id rewrite.extra_args
  with Not_found -> (
    match rewrite.extra_params_usage with
    | [] -> Or_invalid.Ok []
    | _ :: _ ->
      Misc.fatal_errorf
        "Apply_cont_rewrite.extra_args_list:@ Could not find extra args but \
         extra params were not empty")

let make_rewrite rewrite ~ctx id args : _ Or_invalid.t =
  let invariant_args, args =
    partition_used args rewrite.original_params_usage
  in
  match extra_args_list rewrite id with
  | Or_invalid.Invalid -> Invalid
  | Or_invalid.Ok extra_args_list ->
    let extra_invariant_args_rev, extra_args_rev, extra_lets, _ =
      List.fold_left2
        (fun ( extra_invariant_args_rev,
               extra_args_rev,
               extra_lets,
               required_by_other_extra_args ) (arg : EA.t) used ->
          (* Some extra_args computation can depend on other extra args. But
             those required extra args might not be needed as argument to the
             continuation. But we want to keep the let bindings.
             [required_by_other_extra_args] tracks that dependency. It is the
             set of free variables of [extra_args_rev] and
             [extra_invariant_args_rev] *)
          let extra_arg, extra_let, free_names, defined_names =
            match arg with
            | Already_in_scope simple ->
              simple, [], Simple.free_names simple, Name_occurrences.empty
            | New_let_binding (temp, prim) ->
              let extra_let =
                ( Bound_var.create temp Name_mode.normal,
                  Code_size.prim prim,
                  Flambda.Named.create_prim prim Debuginfo.none )
              in
              ( Simple.var temp,
                [extra_let],
                Flambda_primitive.free_names prim,
                Name_occurrences.singleton_variable temp Name_mode.normal )
            | New_let_binding_with_named_args (temp, gen_prim) ->
              let prim =
                match (ctx : rewrite_apply_cont_ctx) with
                | Apply_expr function_return_values ->
                  gen_prim function_return_values
                | Apply_cont ->
                  Misc.fatal_errorf
                    "Apply_cont rewrites should not need to name arguments, \
                     since they are already named."
              in
              let extra_let =
                ( Bound_var.create temp Name_mode.normal,
                  Code_size.prim prim,
                  Flambda.Named.create_prim prim Debuginfo.none )
              in
              ( Simple.var temp,
                [extra_let],
                Flambda_primitive.free_names prim,
                Name_occurrences.singleton_variable temp Name_mode.normal )
          in
          let required_let, extra_invariant_args_rev, extra_args_rev =
            match used with
            | Used ->
              true, extra_invariant_args_rev, extra_arg :: extra_args_rev
            | Used_as_invariant ->
              true, extra_arg :: extra_invariant_args_rev, extra_args_rev
            | Unused ->
              ( Name_occurrences.inter_domain_is_non_empty defined_names
                  required_by_other_extra_args,
                extra_invariant_args_rev,
                extra_args_rev )
          in
          if required_let
          then
            ( extra_invariant_args_rev,
              extra_args_rev,
              extra_let @ extra_lets,
              Name_occurrences.union free_names required_by_other_extra_args )
          else
            ( extra_invariant_args_rev,
              extra_args_rev,
              extra_lets,
              required_by_other_extra_args ))
        ([], [], [], Name_occurrences.empty)
        extra_args_list rewrite.extra_params_usage
    in
    Ok
      ( extra_lets,
        invariant_args
        @ List.rev_append extra_invariant_args_rev args
        @ List.rev extra_args_rev )

let rewrite_exn_continuation rewrite id exn_cont =
  let exn_cont_arity = Exn_continuation.arity exn_cont in
  if not
       (Flambda_arity.equal_ignoring_subkinds exn_cont_arity
          (original_params_arity rewrite))
  then
    Misc.fatal_errorf
      "Arity of exception continuation %a does not match@ [original_params] \
       (%a)"
      Exn_continuation.print exn_cont Bound_parameters.print
      rewrite.original_params;
  assert (Flambda_arity.cardinal_unarized exn_cont_arity >= 1);
  (match List.hd rewrite.original_params_usage with
  | Used -> ()
  | Unused | Used_as_invariant ->
    Misc.fatal_errorf
      "The usage of the exn parameter of the continuation rewrite should be \
       [Used]: %a"
      print rewrite);
  if List.exists
       (fun x ->
         match x with Used_as_invariant -> true | Used | Unused -> false)
       (rewrite.original_params_usage @ rewrite.extra_params_usage)
  then
    Misc.fatal_errorf
      "An exception continuation should never have invariant parameters: %a"
      print rewrite;
  let _, extra_args0 =
    partition_used
      (Exn_continuation.extra_args exn_cont)
      (List.tl rewrite.original_params_usage)
  in
  let _, extra_args1 =
    let extra_args_list =
      match extra_args_list rewrite id with
      | Invalid ->
        (* CR gbury: This is not supported for now, but adding support for it
           should be relatively easy and straight-forward *)
        Misc.fatal_error
          "[Invalid] extra args are currently not allowed for exn continuation \
           rewrites"
      | Ok extra_args_list ->
        List.map2
          (fun (arg : EA.t) extra_param ->
            match arg with
            | Already_in_scope simple ->
              simple, Bound_parameter.kind extra_param
            | New_let_binding _ | New_let_binding_with_named_args _ ->
              (* Note: this is unsupported for now. If we choose to support it
                 in the future, we must take care of not introducing a wrapper
                 continuation, which would come with its own
                 pushtrap/poptrap. *)
              Misc.fatal_error
                "[New_let_binding] are currently forbidden for exn \
                 continuation rewrites")
          extra_args_list
          (Bound_parameters.to_list rewrite.extra_params)
    in
    partition_used extra_args_list rewrite.extra_params_usage
  in
  let extra_args = extra_args0 @ extra_args1 in
  Exn_continuation.create
    ~exn_handler:(Exn_continuation.exn_handler exn_cont)
    ~extra_args
