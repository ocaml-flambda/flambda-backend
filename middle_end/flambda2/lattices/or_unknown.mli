(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t =
  | Known of 'a
  | Unknown

val known : 'a -> 'a t

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_sharing : 'a t -> f:('a -> 'a) -> 'a t

val free_names : ('a -> Name_occurrences.t) -> 'a t -> Name_occurrences.t

val ids_for_export : ('a -> Ids_for_export.t) -> 'a t -> Ids_for_export.t

val apply_renaming : 'a t -> Renaming.t -> ('a -> Renaming.t -> 'a) -> 'a t

module Lift (I : Container_types.S) : sig
  type nonrec t = I.t t

  include Container_types.S with type t := t
end

module Let_syntax : sig
  val ( let>* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let>+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** [let>+$] returns the input ['a t] if the mapping function returns a value
      that is physically-equal to its input. *)
  val ( let>+$ ) : 'a t -> ('a -> 'a) -> 'a t
end
