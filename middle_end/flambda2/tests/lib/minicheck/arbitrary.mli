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

type ('a, 'repr) t

type 'a simple = ('a, 'a) t

val define :
  generator:'repr Generator.t ->
  ?shrinker:'repr Shrinker.t ->
  ?printer:'repr Printer.t ->
  get_value:('repr -> 'a) ->
  unit ->
  ('a, 'repr) t

val define_simple :
  generator:'a Generator.t ->
  ?shrinker:'a Shrinker.t ->
  ?printer:'a Printer.t ->
  unit ->
  'a simple

val with_repr_printer :
  ('a, 'repr) t -> printer:'repr Printer.t -> ('a, 'repr) t

val with_value_printer : ('a, 'repr) t -> printer:'a Printer.t -> ('a, 'repr) t

val generate_repr : (_, 'repr) t -> Splittable_random.t -> 'repr

val generate : ('a, _) t -> Splittable_random.t -> 'a

val shrink : (_, 'repr) t -> 'repr -> 'repr Seq.t

val print : (_, 'repr) t -> Format.formatter -> 'repr -> unit

val value : ('a, 'repr) t -> 'repr -> 'a

val bool : bool simple

val int : int simple

val option : ('a, 'repr) t -> ('a option, 'repr option) t

val list : ('a, 'repr) t -> length:int -> ('a list, 'repr list) t

val unit : unit simple

val pair : ('a, 'r) t -> ('b, 's) t -> ('a * 'b, 'r * 's) t

val triple :
  ('a, 'r) t -> ('b, 's) t -> ('c, 't) t -> ('a * 'b * 'c, 'r * 's * 't) t

val quad :
  ('a, 'r) t ->
  ('b, 's) t ->
  ('c, 't) t ->
  ('d, 'u) t ->
  ('a * 'b * 'c * 'd, 'r * 's * 't * 'u) t

module T : sig
  type nonrec ('a, 'repr) t = ('a, 'repr) t
end

val tuple :
  ('a, 'reprs, 'r) Tuple.Of2(T).t -> (('a, 'r) Tuple.t, ('reprs, 'r) Tuple.t) t

module Function_repr : sig
  type ('a, 'b) t
end

val fn :
  ?hash_arg:('a -> int) ->
  ('b, 'repr) t ->
  ('a -> 'b, ('a, 'repr) Function_repr.t) t

val fn_w_id :
  ?hash_arg:('a -> int) ->
  ('a, 'repr) t ->
  ('a -> 'a, ('a, 'repr) Function_repr.t) t

val fn2 :
  ?hash_args:('a * 'b -> int) ->
  ('c, 'repr) t ->
  ('a -> 'b -> 'c, ('a * 'b, 'repr) Function_repr.t) t

val fn3 :
  ?hash_args:('a * 'b * 'c -> int) ->
  ('d, 'repr) t ->
  ('a -> 'b -> 'c -> 'd, ('a * 'b * 'c, 'repr) Function_repr.t) t

val map : ('a, 'repr) t -> f:('a -> 'b) -> ('b, 'repr) t

val map_repr :
  ('a, 'repr1) t ->
  f:('repr1 -> 'repr2) ->
  f_inv:('repr2 -> 'repr1) ->
  ('a, 'repr2) t

module Bound_repr : sig
  type ('a, 'repr) t
end

val bind :
  ('a, 'a_repr) t ->
  f:('a -> ('b, 'b_repr) t) ->
  ('b, ('b, 'b_repr) Bound_repr.t) t

val bind_generator :
  'a Generator.t ->
  f:('a -> ('b, 'b_repr) t) ->
  ('b, ('b, 'b_repr) Bound_repr.t) t
