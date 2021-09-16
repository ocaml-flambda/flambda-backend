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

(** The representation of the alpha-equivalence class of bindings of a list of
    parameters, with associated relations thereon, over the code of a
    continuation handler. *)
type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val print_using_where_with_cache :
  Recursive.t ->
  cache:Printing_cache.t ->
  Format.formatter ->
  Continuation.t ->
  t ->
  Num_occurrences.t Or_unknown.t ->
  first:bool ->
  unit

(** Create a value of type [t] given information about a continuation handler. *)
val create :
  Kinded_parameter.t list ->
  handler:Expr.t ->
  free_names_of_handler:Name_occurrences.t Or_unknown.t ->
  is_exn_handler:bool ->
  t

(** Choose a member of the alpha-equivalence class to enable examination of the
    parameters, relations thereon and the code over which they are scoped. *)
val pattern_match' :
  t ->
  f:
    (Kinded_parameter.t list ->
    num_normal_occurrences_of_params:Num_occurrences.t Variable.Map.t ->
    handler:Expr.t ->
    'a) ->
  'a

val pattern_match :
  t -> f:(Kinded_parameter.t list -> handler:Expr.t -> 'a) -> 'a

module Pattern_match_pair_error : sig
  type t = Parameter_lists_have_different_lengths

  val to_string : t -> string
end

(** Choose members of two bindings' alpha-equivalence classes using the same
    parameters. *)
val pattern_match_pair :
  t ->
  t ->
  f:(Kinded_parameter.t list -> handler1:Expr.t -> handler2:Expr.t -> 'a) ->
  ('a, Pattern_match_pair_error.t) Result.t

(** Whether the continuation is an exception handler.

    Continuations used as exception handlers are always [Non_recursive]. To
    enable identification of them in passes not invoked from [Simplify] (where
    they could be identified by looking at the [Apply_cont]s that reference
    them) they are marked explicitly.

    Continuations used as exception handlers may have more than one parameter
    (see [Exn_continuation]).

    (Relevant piece of background info: the backend cannot compile
    simultaneously-defined continuations when one or more of them is an
    exception handler.) *)
val is_exn_handler : t -> bool
