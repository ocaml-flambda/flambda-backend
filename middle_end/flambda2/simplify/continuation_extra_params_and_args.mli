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

module Extra_arg : sig
  type t =
    | Already_in_scope of Simple.t
    | New_let_binding of Variable.t * Flambda_primitive.t
    | New_let_binding_with_named_args of
        Variable.t * (Simple.t list -> Flambda_primitive.t)

  val print : Format.formatter -> t -> unit

  module List : sig
    type nonrec t = t list

    val print : Format.formatter -> t -> unit
  end
end

type t = private
  | Empty
  | Non_empty of
      { extra_params : Bound_parameters.t;
        extra_args : Extra_arg.t list Or_invalid.t Apply_cont_rewrite_id.Map.t
      }

val print : Format.formatter -> t -> unit

val is_empty : t -> bool

(** {2 Creating an EPA} *)

(** {3 First way to create an EPA} *)

val empty : t

val add :
  t ->
  invalids:Apply_cont_rewrite_id.Set.t ->
  extra_param:Bound_parameter.t ->
  extra_args:Extra_arg.t Apply_cont_rewrite_id.Map.t ->
  t

(** {3 Another way to create an EPA} *)

val init_with_params_only : Bound_parameters.t -> t

val add_args_for_all_params :
  t -> Apply_cont_rewrite_id.t -> Extra_arg.t list -> t

(** {2 Other functions} *)

val concat : outer:t -> inner:t -> t

val replace_extra_args :
  t -> Extra_arg.t list Or_invalid.t Apply_cont_rewrite_id.Map.t -> t

val extra_params : t -> Bound_parameters.t

val extra_args : t -> Extra_arg.t list Or_invalid.t Apply_cont_rewrite_id.Map.t
