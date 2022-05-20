(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t

val define :
  generate:(Splittable_random.t -> 'a) ->
  ?print:(Format.formatter -> 'a -> unit) ->
  unit ->
  'a t

val with_print : 'a t -> print:(Format.formatter -> 'a -> unit) -> 'a t

val generate : 'a t -> Splittable_random.t -> 'a

val print : 'a t -> Format.formatter -> 'a -> unit

val bool : bool t

val int : int t

(** Integer between zero (inclusive) and [less_than] (exclusive). *)
val small_nat : less_than:int -> int t

val log_int : int t

val option : 'a t -> 'a option t

val unit : unit t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val const : 'a -> 'a t

val one_of : 'a list -> 'a t

val list : 'a t -> expected_length:int -> 'a list t

val fn : ?hash_arg:('a -> int) -> 'b t -> ('a -> 'b) t

val fn2 : ?hash_args:('a * 'b -> int) -> 'c t -> ('a -> 'b -> 'c) t

val fn3 : ?hash_args:('a * 'b * 'c -> int) -> 'd t -> ('a -> 'b -> 'c -> 'd) t

val map : 'a t -> f:('a -> 'b) -> 'b t

(** Change the generator but keep the printer. *)
val map_generate : 'a t -> f:('a -> 'a) -> 'a t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val choose : (int * 'a t) list -> 'a t

module Let_syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module Tuple : sig
  type 'a type_ := 'a t

  (** A list of types, comprising a tuple type. For example, [[int; bool] : (int
      -> bool -> 'b, 'b) t]. ['b] should be left polymorphic (a la [format3] and
      friends). *)
  type ('a, 'b) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a type_ * ('b, 'c) t -> ('a -> 'b, 'c) t

  module Value : sig
    (** The value of a tuple type, i.e., a tuple. *)
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t
  end
end

val tuple : ('a, 'b) Tuple.t -> ('a, 'b) Tuple.Value.t t
