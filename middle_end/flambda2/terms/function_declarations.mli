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

(** The part of a [Set_of_closures] that describes the functions therein (as
    opposed to the captured variables). *)
type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

type code_id_in_function_declaration =
  | Deleted
  | Code_id of Code_id.t

val empty : t

val is_empty : t -> bool

(** Create a set of function declarations in the given order. *)
val create : code_id_in_function_declaration Function_slot.Lmap.t -> t

(** The function(s) defined by the set of function declarations, indexed by
    function slot. *)
val funs : t -> code_id_in_function_declaration Function_slot.Map.t

(** The function(s) defined by the set of function declarations, in the order
    originally given. *)
val funs_in_order : t -> code_id_in_function_declaration Function_slot.Lmap.t

(** [find f t] raises [Not_found] if [f] is not in [t]. *)
val find : t -> Function_slot.t -> code_id_in_function_declaration

val binds_function_slot : t -> Function_slot.t -> bool

val compare : t -> t -> int

val filter :
  t -> f:(Function_slot.t -> code_id_in_function_declaration -> bool) -> t
