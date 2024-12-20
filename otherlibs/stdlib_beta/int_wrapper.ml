(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

include Stdlib.Int

let int_size = Sys.int_size
let[@inline available] of_int t = t
let[@inline available] to_int t = t
let[@inline available] unsigned_to_int t = t

let[@inline available] unsigned_compare n m =
  compare (sub n min_int) (sub m min_int)

let[@inline] unsigned_lt n m =
  sub n min_int < sub m min_int

(* Unsigned division from signed division of the same bitness.
   See Warren Jr., Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3.
*)
let[@inline available] unsigned_div n d =
  if d < zero then
    if unsigned_lt n d then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline available] unsigned_rem n d =
  sub n (mul ((unsigned_div[@inlined]) n d) d)

let seeded_hash seed x = Stdlib.Hashtbl.seeded_hash seed (x : int)
let hash x = Stdlib.Hashtbl.hash (x : int)

module type S = sig
  (** Signed {n}-bit tagged integer values.

    These integers are {n} bits wide and use two's complement representation.
    All operations are taken modulo 2{^n}. They do not fail on overflow. *)

  (** {1:ints n-bit Integers} *)

  (** The type for n-bit integer values. *)
  type t

  (** The number of bits in an integer of type {!t}. *)
  val int_size : int

  val zero : t

  val one : t

  val minus_one : t

  val neg : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  (** Integer division. This division rounds the real quotient of
      its arguments towards zero, as specified for {!Stdlib.(/)}.
      @raise Division_by_zero if the second argument is zero. *)
  val div : t -> t -> t

  (** Same as {!div}, except that arguments and result are interpreted as {e
      unsigned} integers. *)
  val unsigned_div : t -> t -> t

  (** Integer remainder. If [y] is not zero, [rem x y = sub x (mul (div x y)
      y)]. If [y] is zero, [rem x y] raises [Division_by_zero]. *)
  val rem : t -> t -> t

  (** Same as {!rem}, except that arguments and result are interpreted as {e
      unsigned} integers. *)
  val unsigned_rem : t -> t -> t

  (** [succ x] is [add x 1]. *)
  val succ : t -> t

  (** [pred x] is [sub x 1]. *)
  val pred : t -> t

  (** [abs x] is the absolute value of [x]. That is [x] if [x] is positive and
      [neg x] if [x] is negative. {b Warning.} This may be negative if the
      argument is {!min_int}. *)
  val abs : t -> t

  (** [max_int] is the greatest representable integer,
      [2{^[int_size - 1]} - 1]. *)
  val max_int : t

  (** [min_int] is the smallest representable integer,
      [-2{^[int_size - 1]}]. *)
  val min_int : t

  (** Bitwise logical and. *)
  val logand : t -> t -> t

  (** Bitwise logical or. *)
  val logor : t -> t -> t

  (** Bitwise logical exclusive or. *)
  val logxor : t -> t -> t

  (** Bitwise logical negation. *)
  val lognot : t -> t

  (** [shift_left x n] shifts [x] to the left by [n] bits. The result
      is unspecified if [n < 0] or [n >= ]{!int_size}. *)
  val shift_left : t -> int -> t

  (** [shift_right x n] shifts [x] to the right by [n] bits. This is an
      arithmetic shift: the sign bit of [x] is replicated and inserted
      in the vacated bits. The result is unspecified if [n < 0] or
      [n >=]{!int_size}. *)
  val shift_right : t -> int -> t

  (** [shift_right x n] shifts [x] to the right by [n] bits. This is a
      logical shift: zeroes are inserted in the vacated bits regardless
      of the sign of [x]. The result is unspecified if [n < 0] or
      [n >=]{!int_size}. *)
  val shift_right_logical : t -> int -> t

  (** {1:preds Predicates and comparisons} *)

  (** [equal x y] is [true] if and only if [x = y]. *)
  val equal : t -> t -> bool

  (** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
  val compare : t -> t -> int

  (** Same as {!compare}, except that arguments are interpreted as {e unsigned} integers. *)
  val unsigned_compare : t -> t -> int

  (** Return the lesser of the two arguments. *)
  val min : t -> t -> t

  (** Return the greater of the two arguments. *)
  val max : t -> t -> t

  (** {1:convert Converting} *)

  (** [to_int x] is [x] as an {!int}. If [int_size > Sys.int_size], the topmost
      bits will be lost in the conversion *)
  val to_int : t -> int

  (** [of_int x] truncates the representation of [x] to fit in {!t}. *)
  val of_int : int -> t

  (** Same as {!to_int}, but interprets the argument as an {e unsigned} integer. *)
  val unsigned_to_int : t -> int

  (** [to_float x] is [x] as a floating point number. *)
  val to_float : t -> float

  (** [of_float x] truncates [x] to an integer. The result is
      unspecified if the argument is [nan] or falls outside the range of
      representable integers. *)
  val of_float : float -> t

  (** [to_string x] is the written representation of [x] in decimal. *)
  val to_string : t -> string

  (** A seeded hash function for ints, with the same output value as
      {!Hashtbl.seeded_hash}. This function allows this module to be passed as
      argument to the functor {!Hashtbl.MakeSeeded}. *)
  val seeded_hash : int -> t -> int

  (** An unseeded hash function for ints, with the same output value as
      {!Hashtbl.hash}. This function allows this module to be passed as argument
      to the functor {!Hashtbl.Make}. *)
  val hash : t -> int
end

module Make
    (Container : S) (Spec : sig
      type t

      val int_size : int

      val inject : t -> Container.t

      val unchecked_project : Container.t -> t
    end) : S with type t := Spec.t = struct
  include Spec

  let () = assert (0 < int_size && int_size <= Container.int_size)

  let unused_bits = Container.int_size - int_size

  let[@inline] sign_extend i =
    unchecked_project
      (Container.shift_right (Container.shift_left i unused_bits) unused_bits)

  let[@inline] project i =
    let t = sign_extend i in
    if Container.equal i (inject t) then Some t else None

  let[@inline] zero_extend t =
    Container.shift_right_logical
      (Container.shift_left (inject t) unused_bits)
      unused_bits

  let zero = sign_extend Container.zero

  let one = sign_extend Container.one

  let minus_one = sign_extend Container.minus_one

  let[@inline available] neg x = sign_extend (Container.neg (inject x))

  let[@inline available] add x y =
    sign_extend (Container.add (inject x) (inject y))

  let[@inline available] sub x y =
    sign_extend (Container.sub (inject x) (inject y))

  let[@inline available] mul x y =
    sign_extend (Container.mul (inject x) (inject y))

  let[@inline available] div x y =
    sign_extend (Container.div (inject x) (inject y))

  let[@inline available] rem x y =
    sign_extend (Container.rem (inject x) (inject y))

  let[@inline available] succ x = sign_extend (Container.succ (inject x))

  let[@inline available] pred x = sign_extend (Container.pred (inject x))

  let[@inline available] abs x = sign_extend (Container.abs (inject x))

  let[@inline available] equal x y = Container.equal (inject x) (inject y)

  let[@inline available] compare x y = Container.compare (inject x) (inject y)

  (* since the values are stored sign-extended, we can skip the sign-extension
     for bitwise operations as the sign bits will all be treated the same *)

  let[@inline available] logand x y =
    unchecked_project (Container.logand (inject x) (inject y))

  let[@inline available] logor x y =
    unchecked_project (Container.logor (inject x) (inject y))

  let[@inline available] logxor x y =
    unchecked_project (Container.logxor (inject x) (inject y))

  let[@inline available] lognot x =
    unchecked_project (Container.lognot (inject x))

  let[@inline available] shift_left x y =
    sign_extend (Container.shift_left (inject x) y)

  let[@inline available] shift_right x y =
    sign_extend (Container.shift_right (inject x) y)

  let[@inline available] shift_right_logical x y =
    sign_extend (Container.shift_right_logical (zero_extend x) y)

  let max_int = shift_right_logical minus_one 1

  let min_int = succ max_int

  let[@inline available] unsigned_compare n m =
    Container.unsigned_compare (zero_extend n) (zero_extend m)

  (* Unsigned division from signed division of the same bitness. See Warren Jr.,
     Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3. *)
  let[@inline available] unsigned_div n d =
    sign_extend (Container.unsigned_div (zero_extend n) (zero_extend d))

  let[@inline available] unsigned_rem n d =
    sign_extend (Container.unsigned_rem (zero_extend n) (zero_extend d))

  let[@inline available] min x y =
    unchecked_project (Container.min (inject x) (inject y))

  let[@inline available] max x y =
    unchecked_project (Container.max (inject x) (inject y))

  let[@inline available] of_float f =
    Option.value (project (Container.of_float f)) ~default:zero

  let[@inline available] to_float t = Container.to_float (inject t)

  let[@inline available] to_string t = Container.to_string (inject t)

  let[@inline available] seeded_hash seed t =
    Container.seeded_hash seed (inject t)

  let[@inline available] hash t = Container.hash (inject t)

  let[@inline available] to_int t = Container.to_int (inject t)

  let[@inline available] of_int i = sign_extend (Container.of_int i)

  let[@inline available] unsigned_to_int t =
    Container.unsigned_to_int (zero_extend t)
end
[@@inline available]
