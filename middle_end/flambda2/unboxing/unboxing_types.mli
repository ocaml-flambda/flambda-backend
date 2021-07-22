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

(* Typedefs for unboxing decisions *)
(* ******************************* *)

type do_not_unbox_reason =
  | Not_beneficial
  | Max_depth_exceeded
  | Incomplete_parameter_type
  | Not_enough_information_at_use

module Extra_param_and_args : sig
  type t = private {
    param : Variable.t;
    args : EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t;
  }

  val create : name:string -> t

  val update_param_args : t -> Apply_cont_rewrite_id.t -> EPA.Extra_arg.t -> t
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
  (* By "single entry" we mean that the corresponding set of closures only
     contains a single closure. *)
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

val print_decision : Format.formatter -> decision -> unit
(** Printing function for individual decisions. *)


module Decisions : sig
  type t = {
    decisions : (KP.t * decision) list;
    rewrite_ids_seen : Apply_cont_rewrite_id.Set.t;
  }

  val print : Format.formatter -> t -> unit
end

(* Decision update pass.

   This notion of pass is used when turning a potential unboxing
   decision into a decision compatible with a given set of uses of the
   continuation. Indeed, depending on whether enough information is
   available at the use site, we can end up in a few different cases:
   - enough information, and the unboxed values are directly available,
     in which case, we can use them directly
   - the unboxed values are not available directly, but can be "reasonably"
     computed by introducing a let-binding (e.g. a block field projection)
   - the unboxed values are not available, and would be too costly to
     compute (see the example about variants a few lines down).

   Thus, the first pass is used to filter out decisions which would end
   up in the third case. *)
type pass =
  | Filter of { recursive : bool; }
  (* First pass when computing unboxing decisions. This is done before
     inspecting the handler of the continuation whose parameters we are
     trying to unbox. For a non-recursive continuation, that means that
     all use sites of the continuation are known, but for recursive
     continuations, there are likely use sites that are not known at
     this point.

     For recursive continuations, we need to prevent unboxing variants
     and closures because we cannot be sure that reasonable extra_args can be
     computed for all use sites. For instance:

     let rec cont k x y =
       switch y with
       | 0 -> k (Some x)
       | 1 -> k (f x) (* for some function f in scope *)

     In this case, even if we know that x is an option, to unbox it we'd
     need to introduce a switch in the `1` branch.  This is:
     1) not implemented (although technically possible)
     2) not efficient or beneficial in most cases.
  *)
  | Compute_all_extra_args
  (* Last pass, after the traversal of the handler of the continuation.
     Thus, at this point, all use-sites are known, and we can compute
     the extra args that were not computed in the first pass (i.e. for
     use-sites that were not known during the first pass). *)
