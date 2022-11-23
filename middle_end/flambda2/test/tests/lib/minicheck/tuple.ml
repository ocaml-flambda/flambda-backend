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

type ('a, 'b) t =
  | [] : ('a, 'a) t
  | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

let nil = []

let cons a t = a :: t

let rec call : type a r. (a, r) t -> f:a -> r =
 fun t ~f -> match t with [] -> f | a :: t -> call t ~f:(f a)

let of_pair (a, b) = [a; b]

let of_triple (a, b, c) = [a; b; c]

let of_quad (a, b, c, d) = [a; b; c; d]

let to_pair : type a b r. (a -> b -> r, r) t -> a * b = function
  | [a; b] -> a, b
  | _ -> assert false

let to_triple : type a b c r. (a -> b -> c -> r, r) t -> a * b * c = function
  | [a; b; c] -> a, b, c
  | _ -> assert false

let to_quad : type a b c d r. (a -> b -> c -> d -> r, r) t -> a * b * c * d =
  function
  | [a; b; c; d] -> a, b, c, d
  | _ -> assert false

module type T1 = sig
  type 'a t
end

module Of (T : T1) = struct
  type ('a, 'b) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a T.t * ('b, 'c) t -> ('a -> 'b, 'c) t
end

module Map (From : T1) (Into : T1) = struct
  type f = { f : 'a. 'a From.t -> 'a Into.t }

  let rec map : type a r. (a, r) Of(From).t -> f:f -> (a, r) Of(Into).t =
   fun t ~f -> match t with [] -> [] | a :: t -> f.f a :: map ~f t
end

module type T2 = sig
  type ('a, 'b) t
end

module Of2 (T : T2) = struct
  type ('a, 'b, 'r) t =
    | [] : ('r, 'r, 'r) t
    | ( :: ) : ('a, 'b) T.t * ('c, 'd, 'r) t -> ('a -> 'c, 'b -> 'd, 'r) t
end
