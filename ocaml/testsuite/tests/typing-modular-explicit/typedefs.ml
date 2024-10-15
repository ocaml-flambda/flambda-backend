(* TEST
  expect;
*)

module type T = sig
  type t
end

module type Add = sig
  type t
  val add : t -> t -> t
end

[%%expect{|
module type T = sig type t end
module type Add = sig type t val add : t -> t -> t end
|}]

type t0 = (module M : T) -> M.t -> M.t

type t1 = (module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t

type _ t2 = A : ((module M : T) -> M.t) t2

type t3 = [`A of ((module M : T) -> (module N : T with type t = M.t) -> N.t)]

type t4 = < m : (module M : T) -> M.t >

type 'a t5 = (module M : T with type t = 'a) -> 'a -> 'a

type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a

type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >

type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1

type t9 = C of 'a constraint 'a = (module T : T) -> T.t -> T.t

type t10 = t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)

[%%expect{|
type t0 = (module M : T) -> M.t -> M.t
type t1 = (module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t
type _ t2 = A : ((module M : T) -> M.t) t2
type t3 = [ `A of (module M : T) -> (module N : T with type t = M.t) -> N.t ]
type t4 = < m : (module M : T) -> M.t >
type 'a t5 = (module M : T with type t = 'a) -> 'a -> 'a
type 'a t6 = 'a -> (module M : T with type t = 'a) -> 'a
type t7 = < m : 'a. (module M : T with type t = 'a) -> 'a >
type t8 =
    A of ((module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t)
  | B of t1
type t9 = C of ((module T : T) -> T.t -> T.t)
type t10 =
  t8 =
    A of
      ((module T/1 : T) ->
       (module A/1 : Add with type t = T/1.t) -> A/1.t -> A/1.t)
  | B of
      ((module T/2 : T) ->
       (module A/2 : Add with type t = T/2.t) -> A/2.t -> A/2.t)
|}]

type 'a t6bis_compare = (module T with type t = int) -> (int as 'a)
type 'a t6bis_good = (module M : T with type t = int) -> (M.t as 'a)
(* Does not fail, but shouldn't it ? *)
type 'a t6_fail = (module M : T) -> (M.t as 'a)

[%%expect{|
type 'a t6bis_compare = (module T with type t = int) -> 'a
  constraint 'a = int
type 'a t6bis_good = (module M : T with type t = int) -> M.t
type 'a t6_fail = (module M : T) -> M.t
|}, Principal{|
type 'a t6bis_compare = (module T with type t = int) -> int
  constraint 'a = int
type 'a t6bis_good = (module M : T with type t = int) -> M.t
type 'a t6_fail = (module M : T) -> M.t
|}]

(* Test about invalid types *)

type t_fail1 = (module M : T) -> M.a

[%%expect{|
Line 1, characters 33-36:
1 | type t_fail1 = (module M : T) -> M.a
                                     ^^^
Error: Unbound type constructor "M.a"
|}]

type t_fail2 = (module M : T) -> N.t

[%%expect{|
Line 1, characters 33-36:
1 | type t_fail2 = (module M : T) -> N.t
                                     ^^^
Error: Unbound module "N"
|}]

type t_fail3 = < m : (module M : T) -> M.t; n : M.t >

[%%expect{|
Line 1, characters 48-51:
1 | type t_fail3 = < m : (module M : T) -> M.t; n : M.t >
                                                    ^^^
Error: Unbound module "M"
|}]

(* should this fail ? *)
type +'a t_fail4 = 'a -> (module M : T with type t = 'a) -> unit

[%%expect{|
Line 1, characters 0-64:
1 | type +'a t_fail4 = 'a -> (module M : T with type t = 'a) -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

(* this one should fail but the error message might not be the best *)
type +'a t_fail5 = (module M : T with type t = 'a) -> M.t

[%%expect{|
Line 1, characters 0-57:
1 | type +'a t_fail5 = (module M : T with type t = 'a) -> M.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type t_fail6 = (module M : T) -> 'a constraint 'a = M.t

[%%expect{|
Line 1, characters 52-55:
1 | type t_fail6 = (module M : T) -> 'a constraint 'a = M.t
                                                        ^^^
Error: Unbound module "M"
|}]

(* Is this the expected output ? *)
type 'a t_fail7 = (module M : T) -> (M.t as 'a)

[%%expect{|
type 'a t_fail7 = (module M : T) -> M.t
|}]

(* tests about variance *)

module type V = sig
  type +'a p
  type -'a n
  type !'a i
end

(* this test is here to compare that both behave the same way *)
module type F = functor (X : V) -> sig
  type +'a t_pos = unit -> 'a X.p
  type -'a t_neg = unit -> 'a X.n
  type !'a t_inj = unit -> 'a X.i
  type -'a t_npos = unit -> 'a X.p -> unit
  type +'a t_pneg = unit -> 'a X.n -> unit
end

type +'a t_pos = (module X : V) -> 'a X.p
type -'a t_neg = (module X : V) -> 'a X.n
type !'a t_inj = (module X : V) -> 'a X.i
type -'a t_npos = (module X : V) -> 'a X.p -> unit
type +'a t_pneg = (module X : V) -> 'a X.n -> unit

[%%expect{|
module type V = sig type +'a p type -'a n type !'a i end
module type F =
  functor (X : V) ->
    sig
      type 'a t_pos = unit -> 'a X.p
      type 'a t_neg = unit -> 'a X.n
      type 'a t_inj = unit -> 'a X.i
      type 'a t_npos = unit -> 'a X.p -> unit
      type 'a t_pneg = unit -> 'a X.n -> unit
    end
type 'a t_pos = (module X : V) -> 'a X.p
type 'a t_neg = (module X : V) -> 'a X.n
type 'a t_inj = (module X : V) -> 'a X.i
type 'a t_npos = (module X : V) -> 'a X.p -> unit
type 'a t_pneg = (module X : V) -> 'a X.n -> unit
|}]

type +'a t_pos = (module X : V) -> 'a X.p -> unit

[%%expect{|
Line 1, characters 0-49:
1 | type +'a t_pos = (module X : V) -> 'a X.p -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is contravariant.
|}]

type -'a t_neg = (module X : V) -> 'a X.n -> unit

[%%expect{|
Line 1, characters 0-49:
1 | type -'a t_neg = (module X : V) -> 'a X.n -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is covariant.
|}]

let id_fail1 (x : t0) : _ t5 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail1 (x : t0) : _ t5 = x
                                   ^
Error: This expression has type "t0" = "(module M/1 : T) -> M/1.t -> M/1.t"
       but an expression was expected of type
         "'a t5" = "(module M/2 : T with type t = 'a) -> 'a -> 'a"
       Type "(module T)" is not compatible with type
         "(module T with type t = 'a)"
|}]

let id_fail2 (x : _ t5) : t0 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail2 (x : _ t5) : t0 = x
                                   ^
Error: This expression has type
         "'a t5" = "(module M/1 : T with type t = 'a) -> 'a -> 'a"
       but an expression was expected of type
         "t0" = "(module M/2 : T) -> M/2.t -> M/2.t"
       Type "(module T with type t = 'a)" is not compatible with type
         "(module T)"
       File "_none_", line 1:
         Definition of module "M/2"
|}]

let id_fail2 (x : _ t5) : t1 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail2 (x : _ t5) : t1 = x
                                   ^
Error: This expression has type
         "'a t5" = "(module M : T with type t = 'a) -> 'a -> 'a"
       but an expression was expected of type
         "t1" =
           "(module T : T) -> (module A : Add with type t = T.t) -> A.t -> A.t"
       Type "(module T with type t = 'a)" is not compatible with type
         "(module T)"
|}]

(* This test check that no scope escape happens when trying to replace 'a by
    A.t *)
type 'a wrapper = 'a

type should_succeed2 =
  (module A:T) -> A.t wrapper

[%%expect{|
type 'a wrapper = 'a
type should_succeed2 = (module A : T) -> A.t wrapper
|}]
