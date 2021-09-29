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

module type S = sig
  type type_grammar

  type typing_env

  type typing_env_extension

  type meet_env

  type join_env

  include Contains_names.S

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  val meet : meet_env -> t -> t -> (t * typing_env_extension) Or_bottom.t

  val join : join_env -> t -> t -> t Or_unknown.t

  val apply_coercion : t -> Coercion.t -> t Or_bottom.t

  val eviscerate : t -> t Or_unknown.t
end
