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

module Make (Function_params_and_body : sig
  type t

  include Contains_ids.S with type t := t

  val apply_renaming : t -> Renaming.t -> t

  val free_names_of_body : t -> Name_occurrences.t Or_unknown.t

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end) (Cost_metrics : sig
  type t

  val print : Format.formatter -> t -> unit
end) :
  Code_intf.S
    with type function_params_and_body := Function_params_and_body.t
    with type cost_metrics := Cost_metrics.t
