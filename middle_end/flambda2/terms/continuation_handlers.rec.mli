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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The result of pattern matching on [Recursive_let_cont_handlers]
    (see above). *)
type t

(** Obtain the mapping from continuation to handler. *)
val to_map : t -> Continuation_handler.t Continuation.Map.t

(** The domain of [to_map t]. *)
val domain : t -> Continuation.Set.t

(** Whether any of the continuations are exception handlers. *)
val contains_exn_handler : t -> bool

include Contains_names.S with type t := t
include Contains_ids.S with type t := t
