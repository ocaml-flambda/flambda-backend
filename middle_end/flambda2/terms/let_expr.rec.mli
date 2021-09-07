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

(** The alpha-equivalence classes of expressions that bind variables. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create :
  Bindable_let_bound.t ->
  Named.t ->
  body:Expr.t ->
  free_names_of_body:Name_occurrences.t Or_unknown.t ->
  t

(** The defining expression of the [Let]. *)
val defining_expr : t -> Named.t

(** Look inside the [Let] by choosing a member of the alpha-equivalence class. *)
val pattern_match : t -> f:(Bindable_let_bound.t -> body:Expr.t -> 'a) -> 'a

val pattern_match' :
  t ->
  f:
    (Bindable_let_bound.t ->
    num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
    body:Expr.t ->
    'a) ->
  'a

module Pattern_match_pair_error : sig
  type t = Mismatched_let_bindings

  val to_string : t -> string
end

(** Look inside two [Let]s by choosing members of their alpha-equivalence
    classes, using the same bound variables for both. If they are both dynamic
    lets (that is, they both bind variables), this invokes [dynamic] having
    freshened both bodies; if they are both static (that is, they both bind
    symbols), this invokes [static] with the bodies unchanged, since no renaming
    is necessary. *)
val pattern_match_pair :
  t ->
  t ->
  dynamic:(Bindable_let_bound.t -> body1:Expr.t -> body2:Expr.t -> 'a) ->
  static:
    (bound_symbols1:Bindable_let_bound.symbols ->
    bound_symbols2:Bindable_let_bound.symbols ->
    body1:Expr.t ->
    body2:Expr.t ->
    'a) ->
  ('a, Pattern_match_pair_error.t) Result.t
