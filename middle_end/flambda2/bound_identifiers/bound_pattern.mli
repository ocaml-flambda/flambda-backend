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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Things that a [Let]-expression binds. *)

type symbols = private { bound_symbols : Bound_symbols.t }

type t = private
  | Singleton of Bound_var.t
      (** The binding of a single variable, which is statically scoped. *)
  | Set_of_closures of
      { name_mode : Name_mode.t;
        closure_vars : Bound_var.t list
      }
      (** The binding of one or more variables to the individual closures in a
          set of closures. The variables are statically scoped. *)
  | Symbols of symbols
      (** The binding of one or more symbols to statically-allocated
          constant(s). The scoping of the symbols may either be syntactic, or
          follow the dominator tree. *)

include Bindable.S with type t := t

include Contains_ids.S with type t := t

val singleton : Bound_var.t -> t

val set_of_closures : closure_vars:Bound_var.t list -> t

val symbols : Bound_symbols.t -> t

val must_be_singleton : t -> Bound_var.t

val must_be_singleton_opt : t -> Bound_var.t option

val must_be_set_of_closures : t -> Bound_var.t list

val must_be_symbols : t -> symbols

val may_be_symbols : t -> symbols option

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

val exists_all_bound_vars : t -> f:(Bound_var.t -> bool) -> bool

val fold_all_bound_vars :
  t -> init:'a -> f:('a -> Bound_var.t -> 'a) -> 'a

val all_bound_vars : t -> Bound_var.Set.t

val all_bound_vars' : t -> Variable.Set.t

val fold_all_bound_names :
  t ->
  init:'a ->
  var:('a -> Bound_var.t -> 'a) ->
  symbol:('a -> Symbol.t -> 'a) ->
  code_id:('a -> Code_id.t -> 'a) ->
  'a
