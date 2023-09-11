(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Things that a [Let]-expression binds.
    If a [Let]-expression binds more than one name, all of those names have
    the same kind. *)

type t = private
  | Singleton of Bound_var.t
      (** The binding of a single variable, which is statically scoped. This
          case is not used for sets of closures. *)
  | Set_of_closures of Bound_var.t list
      (** The binding of one or more variables to the individual closures in a
          set of closures. The variables are statically scoped. *)
  | Static of Bound_static.t
      (** The binding of symbols and code IDs to statically-allocated constants
          and pieces of code. The scoping of the symbols and code IDs follows
          the dominator tree, not syntactic scope. *)

val singleton : Bound_var.t -> t

val set_of_closures : Bound_var.t list -> t

val static : Bound_static.t -> t

val must_be_singleton : t -> Bound_var.t

val must_be_singleton_opt : t -> Bound_var.t option

val must_be_set_of_closures : t -> Bound_var.t list

val must_be_static : t -> Bound_static.t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

val exists_all_bound_vars : t -> f:(Bound_var.t -> bool) -> bool

val fold_all_bound_vars : t -> init:'a -> f:('a -> Bound_var.t -> 'a) -> 'a

val fold_all_bound_names :
  t ->
  init:'a ->
  var:('a -> Bound_var.t -> 'a) ->
  symbol:('a -> Symbol.t -> 'a) ->
  code_id:('a -> Code_id.t -> 'a) ->
  'a

include Bindable.S with type t := t
