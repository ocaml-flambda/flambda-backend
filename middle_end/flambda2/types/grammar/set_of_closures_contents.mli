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

(** Descriptions of the entities inside sets of closures: - closures; - closure
    variables. These descriptions do not necessarily describe the entire
    contents of any particular set of closures. *)

type t

val create : Closure_id.Set.t -> Var_within_closure.Set.t -> t

include Container_types.S with type t := t

val subset : t -> t -> bool

val inter : t -> t -> t

val union : t -> t -> t

val closures : t -> Closure_id.Set.t

val closure_vars : t -> Var_within_closure.Set.t

include Contains_names.S with type t := t

val remove_unused_closure_vars :
  t -> used_closure_vars:Var_within_closure.Set.t -> t

module With_closure_id : sig
  type nonrec t = Closure_id.t * t

  include Container_types.S with type t := t
end

module With_closure_id_or_unknown : sig
  type nonrec t = Closure_id.t Or_unknown.t * t

  include Container_types.S with type t := t
end
