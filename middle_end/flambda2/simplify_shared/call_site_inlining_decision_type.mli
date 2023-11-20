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

(* CR-someday mshinwell: Maybe have two types, one giving the reasons why
   something can be inlined, and one giving the reasons why something cannot be
   inlined. *)
type t =
  | Missing_code
  | Definition_says_not_to_inline
  | Environment_says_never_inline
  | Argument_types_not_useful
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inlined_attribute
  | Speculatively_not_inline of
      { cost_metrics : Cost_metrics.t;
        evaluated_to : float;
        threshold : float
      }
  | Attribute_always
  | Begin_unrolling of int
  | Continue_unrolling
  | Definition_says_inline of { was_inline_always : bool }
  | Speculatively_inline of
      { cost_metrics : Cost_metrics.t;
        evaluated_to : float;
        threshold : float
      }
  | X_dir_inlining_forbidden

val print : Format.formatter -> t -> unit

val report : Format.formatter -> t -> unit

type can_inline = private
  | Do_not_inline of { erase_attribute_if_ignored : bool }
  | Inline of
      { unroll_to : int option;
        was_inline_always : bool
      }

val can_inline : t -> can_inline
