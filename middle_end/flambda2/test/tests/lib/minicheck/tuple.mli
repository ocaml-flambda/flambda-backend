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

(** A list of values of specified types. For example, [[1; true; "hello"] : (int
    -> bool -> string -> 'r, 'r) t]. ['r] should be left polymorphic (a la
    [format3] and friends). *)
type ('a, 'r) t =
  | [] : ('r, 'r) t
  | ( :: ) : 'a * ('b, 'r) t -> ('a -> 'b, 'r) t

val nil : ('r, 'r) t

val cons : 'a -> ('b, 'r) t -> ('a -> 'b, 'r) t

(** Call a function with this tuple as arguments. *)
val call : ('a, 'r) t -> f:'a -> 'r

val of_pair : 'a * 'b -> ('a -> 'b -> 'c, 'c) t

val of_triple : 'a * 'b * 'c -> ('a -> 'b -> 'c -> 'd, 'd) t

val of_quad : 'a * 'b * 'c * 'd -> ('a -> 'b -> 'c -> 'd -> 'e, 'e) t

val to_pair : ('a -> 'b -> 'c, 'c) t -> 'a * 'b

val to_triple : ('a -> 'b -> 'c -> 'd, 'd) t -> 'a * 'b * 'c

val to_quad : ('a -> 'b -> 'c -> 'd -> 'e, 'e) t -> 'a * 'b * 'c * 'd

module type T1 = sig
  type 'a t
end

module Of (T : T1) : sig
  (** A list of values, each of which has type ['a T.t] for different ['a]. This
      allows us to describe, say, a tuple of lists, without constraining the
      lists to have the same element type. For example, [[[1]; [true]; []] :
      (int -> bool -> string -> 'r, 'r) Of(List).t]. *)
  type ('a, 'r) t =
    | [] : ('r, 'r) t
    | ( :: ) : 'a T.t * ('b, 'r) t -> ('a -> 'b, 'r) t
end

module Map (From : T1) (Into : T1) : sig
  type f = { f : 'a. 'a From.t -> 'a Into.t }

  (** Apply a function [f] to each element of a tuple, producing a new tuple.
      Needing to apply the functor makes using this a bit fiddly; a suggested
      idiom is

      {[let hd_opt = function [] -> None | x::_ -> Some x

      let open Tuple.Map(List)(Option) in map ~f:{f = hd_opt} lists]} *)
  val map : ('a, 'r) Of(From).t -> f:f -> ('a, 'r) Of(Into).t
end

module type T2 = sig
  type ('a, 'b) t
end

module Of2 (T : T2) : sig
  (** Similar to [Of], but where [T.t] has two arguments. ['a] collects the
      first arguments together and ['b] collects the second arguments. For
      example, [[Left 3; Right true] : (int -> unit -> 'r, unit -> bool -> 'r,
      'r) Of2(Either).t]. *)
  type ('a, 'b, 'r) t =
    | [] : ('r, 'r, 'r) t
    | ( :: ) : ('a, 'b) T.t * ('c, 'd, 'r) t -> ('a -> 'c, 'b -> 'd, 'r) t
end
