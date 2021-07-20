(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type do_not_unbox_reason =
  | Not_beneficial
  | Max_depth_exceeded
  | Incomplete_parameter_type
  | Not_enough_information_at_use

module Extra_param_and_args = struct
  type t = {
    param : Variable.t;
    args : EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t;
  }

  let create ~name = {
    param = Variable.create name;
    args = Apply_cont_rewrite_id.Map.empty;
  }

  let update_param_args t rewrite_id extra_arg =
    assert (not (Apply_cont_rewrite_id.Map.mem rewrite_id t.args));
    let args = Apply_cont_rewrite_id.Map.add rewrite_id extra_arg t.args in
    { t with args; }

  let print fmt { param; args = _; } =
    Format.fprintf fmt "@[<hv 1>(\
      @[<hov>(param %a)@]@ \
      @[<v 2>(args@ <...>)@]\
      )@]"
      Variable.print param
      (* (Apply_cont_rewrite_id.Map.print EPA.Extra_arg.print) args *)
end

type unboxing_decision =
  | Unique_tag_and_size of {
      tag : Tag.t;
      fields : field_decision list;
    }
  | Variant of {
      tag : Extra_param_and_args.t;
      const_ctors : const_ctors_decision;
      fields_by_tag : field_decision list Tag.Scannable.Map.t;
    }
  | Closure_single_entry of {
      closure_id : Closure_id.t;
      vars_within_closure : field_decision Var_within_closure.Map.t;
    }
  | Number of Flambda_kind.Naked_number_kind.t * Extra_param_and_args.t

and field_decision = {
  epa : Extra_param_and_args.t;
  decision : decision;
}

and const_ctors_decision =
  | Zero
  | At_least_one of {
      is_int : Extra_param_and_args.t;
      ctor : decision;
    }

and decision =
  | Unbox of unboxing_decision
  | Do_not_unbox of do_not_unbox_reason

type decisions = {
  decisions : (KP.t * decision) list;
  rewrite_ids_seen : Apply_cont_rewrite_id.Set.t;
}

type pass =
  | Filter of { recursive : bool; }
  | Compute_all_extra_args

(* Printing *)
(* ******** *)

let print_do_not_unbox_reason ppf = function
  | Not_beneficial ->
    Format.fprintf ppf "not_beneficial"
  | Max_depth_exceeded ->
    Format.fprintf ppf "max_depth_exceeded"
  | Incomplete_parameter_type ->
    Format.fprintf ppf "incomplete_parameter_type"
  | Not_enough_information_at_use ->
    Format.fprintf ppf "not_enough_information_at_use"

let rec print_decision ppf = function
  | Do_not_unbox reason ->
    Format.fprintf ppf "@[<hov 1>(do_not_unbox@ %a)@]"
      print_do_not_unbox_reason reason
  | Unbox Unique_tag_and_size { tag; fields; } ->
    Format.fprintf ppf "@[<v 1>(unique_tag_and_size@ \
      @[<h>(static_tag %a)@]@ \
      @[<hv 2>(fields@ %a)@]\
      )@]"
      Tag.print tag
      print_fields_decisions fields
  | Unbox Variant { tag; const_ctors; fields_by_tag; } ->
    Format.fprintf ppf "@[<v 2>(variant@ \
      @[<hov>(tag %a)@]@ \
      @[<hv 2>(const_ctors@ %a)@]@ \
      @[<v 2>(fields_by_tag@ %a)@]\
      )@]"
      Extra_param_and_args.print tag
      print_const_ctor_num const_ctors
      (Tag.Scannable.Map.print print_fields_decisions) fields_by_tag
  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    Format.fprintf ppf "@[<hov 1>(closure_single_entry@ \
      @[<hov>(closure_id@ %a)@]@ \
      @[<hv 2>(var_within_closures@ %a)@]\
      )@]"
      Closure_id.print closure_id
      (Var_within_closure.Map.print print_field_decision) vars_within_closure
  | Unbox Number (kind, epa) ->
    Format.fprintf ppf "@[<hv 1>(number@ \
      @[<h>(kind %a)@]@ \
      @[<hv 1>(var %a)@]\
    )@]"
    Flambda_kind.Naked_number_kind.print kind
    Extra_param_and_args.print epa

and print_field_decision ppf { epa; decision; } =
  Format.fprintf ppf "@[<hv 1>(@,\
    @[<hov 1>(var %a)@]@ \
    @[<hv 1>(decision@ %a)@]\
    )@]"
    Extra_param_and_args.print epa
    print_decision decision

and print_fields_decisions ppf l =
  let pp_sep = Format.pp_print_space in
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep print_field_decision) l

and print_const_ctor_num ppf = function
  | Zero -> Format.fprintf ppf "no_const_ctors"
  | At_least_one { is_int; ctor; } ->
    Format.fprintf ppf "@[<hov 1>(const_ctors@ \
      @[<hov 1>(is_int@ %a)@]@ \
      @[<hov 1>(ctor@ %a)@]\
      )@]"
      Extra_param_and_args.print is_int
      print_decision ctor

let print ppf { decisions; rewrite_ids_seen; } =
  let pp_sep = Format.pp_print_space in
  let aux ppf (param, decision) =
    Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
      KP.print param print_decision decision
  in
  Format.fprintf ppf "@[<hov 1>(\
    @[<hov 1>(decisions@ %a)@]@ \
    @[<hov 1>(rewrite_ids_seen@ %a)@]\
    )@]"
    (Format.pp_print_list ~pp_sep aux) decisions
    Apply_cont_rewrite_id.Set.print rewrite_ids_seen

module Decisions = struct
  type t = decisions = {
    decisions : (KP.t * decision) list;
    rewrite_ids_seen : Apply_cont_rewrite_id.Set.t;
  }

  let print = print
end
