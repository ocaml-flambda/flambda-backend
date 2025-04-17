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

type t = Type_grammar.Env_extension.t

val print : Format.formatter -> t -> unit

val fold : equation:(Name.t -> Type_grammar.t -> 'a -> 'a) -> t -> 'a -> 'a

val invariant : t -> unit

val empty : t

val is_empty : t -> bool

val from_map : Type_grammar.t Name.Map.t -> t

val to_map : t -> Type_grammar.t Name.Map.t

val has_equation : Name.t -> t -> bool

val one_equation : Name.t -> Type_grammar.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val replace_equation : t -> Name.t -> Type_grammar.t -> t

val disjoint_union : t -> t -> t

include Contains_ids.S with type t := t

include Contains_names.S with type t := t

module With_extra_variables : sig
  type t =
    { existential_vars : Flambda_kind.t Variable.Map.t;
      equations : Type_grammar.t Name.Map.t
    }

  val print : Format.formatter -> t -> unit

  val fold :
    variable:(Variable.t -> Flambda_kind.t -> 'a -> 'a) ->
    equation:(Name.t -> Type_grammar.t -> 'a -> 'a) ->
    t ->
    'a ->
    'a

  val empty : t

  val add_definition : t -> Variable.t -> Flambda_kind.t -> t

  val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

  val existential_vars : t -> Variable.Set.t

  val map_types : t -> f:(Type_grammar.t -> Type_grammar.t) -> t

  (** At present [existential_vars] is not treated as a binding site. *)
  include Contains_ids.S with type t := t

  include Contains_names.S with type t := t
end
