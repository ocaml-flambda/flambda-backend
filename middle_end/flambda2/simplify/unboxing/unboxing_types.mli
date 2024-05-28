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

open! Simplify_import

(* Typedefs for unboxing decisions *)
(* ******************************* *)

type do_not_unbox_reason =
  | Not_beneficial
  | Max_depth_exceeded
  | Incomplete_parameter_type
  | Not_enough_information_at_use
  | Not_of_kind_value
  | Unboxing_not_requested
  | All_fields_invalid

module Extra_param_and_args : sig
  type t = private
    { param : Variable.t;
      args : EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t
    }

  val create : name:string -> t

  val update_param_args : t -> Apply_cont_rewrite_id.t -> EPA.Extra_arg.t -> t
end

type unboxing_decision =
  | Unique_tag_and_size of
      { tag : Tag.t;
        fields : field_decision list
      }
  | Variant of
      { tag : Extra_param_and_args.t;
        const_ctors : const_ctors_decision;
        fields_by_tag : field_decision list Tag.Scannable.Map.t
      }
  | Closure_single_entry of
      { function_slot : Function_slot.t;
        vars_within_closure : field_decision Value_slot.Map.t
      }
  (* By "single entry" we mean that the corresponding set of closures only
     contains a single closure. *)
  | Number of Flambda_kind.Naked_number_kind.t * Extra_param_and_args.t

and field_decision =
  { epa : Extra_param_and_args.t;
    decision : decision;
    kind : Flambda_kind.With_subkind.t
  }

and const_ctors_decision =
  | Zero
  | At_least_one of
      { is_int : Extra_param_and_args.t;
        ctor : decision
      }

and decision =
  | Unbox of unboxing_decision
  | Do_not_unbox of do_not_unbox_reason

(** Printing function for individual decisions. *)
val print_decision : Format.formatter -> decision -> unit

module Decisions : sig
  type t =
    { decisions : (BP.t * decision) list;
      rewrite_ids_seen : Apply_cont_rewrite_id.Set.t;
      rewrites_ids_known_as_invalid : Apply_cont_rewrite_id.Set.t
    }

  val print : Format.formatter -> t -> unit
end

(* Decision update pass.

   This notion of pass is used when turning a potential unboxing decision into a
   decision compatible with a given set of uses of the continuation. Indeed,
   depending on whether enough information is available at the use site, we can
   end up in a few different cases:

   - enough information, and the unboxed values are directly available, in which
   case, we can use them directly

   - the unboxed values are not available directly, but can be "reasonably"
   computed by introducing a let-binding (e.g. a block field projection)

   - the unboxed values are not available, and would be too costly to compute
   (see the example about variants a few lines down).

   Thus, the first pass is used to filter out decisions which would end up in
   the third case. *)
type pass =
  | Filter
  (* First pass when computing unboxing decisions. This is done before
     inspecting the handler of the continuation whose parameters we are trying
     to unbox. For a non-recursive continuation, that means that all use sites
     of the continuation are known, but for recursive continuations, there are
     likely use sites that are not known at this point, so we only keep the
     original decision and depend on the fact that we do not generate unboxing
     decisions for variants and closures in the recursive case. *)
  | Compute_all_extra_args
(* Last pass, after the traversal of the handler of the continuation. Thus, at
   this point, all use-sites are known, and we can compute the extra args that
   were not computed in the first pass (i.e. for use-sites that were not known
   during the first pass). *)
