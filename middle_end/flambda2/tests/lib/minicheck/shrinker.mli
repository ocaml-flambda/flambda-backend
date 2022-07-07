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

type 'a t = 'a -> 'a Seq.t

val unshrinkable : 'a t

val option : 'a t -> 'a option t

val function_ : ?const:'b -> 'b t -> ('a, 'b) Function.t t

val function_w_id : ?const:'a -> 'a t -> ('a, 'a) Function.t t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val list : 'a t -> 'a list t

module T : sig
  type nonrec 'a t = 'a t
end

val tuple : ('a, 'b) Tuple.Of(T).t -> ('a, 'b) Tuple.t t
