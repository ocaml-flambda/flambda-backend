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

type t0 = {M : T} -> M.t -> M.t

type t1 = {T : T} -> {A : Add with type t = T.t} -> A.t -> A.t

type _ t2 = A : ({M : T} -> M.t) t2

type t3 = [`A of ({M : T} -> {N : T with type t = M.t} -> N.t)]

type t4 = < m : {M : T} -> M.t >

type 'a t5 = {M : T with type t = 'a} -> 'a

type 'a t6 = 'a -> {M : T with type t = 'a} -> 'a

type t7 = < m : 'a. {M : T with type t = 'a} -> 'a >

type t8 =
    A of ({T : T} -> {A : Add with type t = T.t} -> A.t -> A.t)
  | B of t1

type t9 = C of 'a constraint 'a = {T : T} -> T.t -> T.t

type t10 = t8 =
    A of ({T : T} -> {A : Add with type t = T.t} -> A.t -> A.t)
  | B of ({T : T} -> {A : Add with type t = T.t} -> A.t -> A.t)

[%%expect{|
type t0 = {M : T} -> M.t -> M.t
type t1 = {T : T} -> {A : Add with type t = T.t} -> A.t -> A.t
type _ t2 = A : ({M : T} -> M.t) t2
type t3 = [ `A of {M : T} -> {N : T with type t = M.t} -> N.t ]
type t4 = < m : {M : T} -> M.t >
type 'a t5 = {M : T with type t = 'a} -> 'a
type 'a t6 = 'a -> ({M : T with type t = 'a} -> 'a)
type t7 = < m : 'a. {M : T with type t = 'a} -> 'a >
type t8 =
    A of ({T : T} -> {A : Add with type t = T.t} -> A.t -> A.t)
  | B of t1
type t9 = C of ({T : T} -> T.t -> T.t)
type t10 =
  t8 =
    A of ({T/1 : T} -> {A/1 : Add with type t = T/1.t} -> A/1.t -> A/1.t)
  | B of ({T/2 : T} -> {A/2 : Add with type t = T/2.t} -> A/2.t -> A/2.t)
|}]

(* Test about invalid types *)

type t_fail1 = {M : T} -> M.a

[%%expect{|
Line 1, characters 26-29:
1 | type t_fail1 = {M : T} -> M.a
                              ^^^
Error: Unbound type constructor M.a
|}]

type t_fail2 = {M : T} -> N.t

[%%expect{|
Line 1, characters 26-29:
1 | type t_fail2 = {M : T} -> N.t
                              ^^^
Error: Unbound module N
|}]

type t_fail3 = < m : {M : T} -> M.t; n : M.t >

[%%expect{|
Line 1, characters 41-44:
1 | type t_fail3 = < m : {M : T} -> M.t; n : M.t >
                                             ^^^
Error: Unbound module M
|}]

(* should this fail ? *)
type +'a t_fail4 = 'a -> {M : T with type t = 'a} -> unit

[%%expect{|
Line 1, characters 0-57:
1 | type +'a t_fail4 = 'a -> {M : T with type t = 'a} -> unit
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

(* this one should fail but the error message might not be the best *)
type +'a t_fail5 = {M : T with type t = 'a} -> M.t

[%%expect{|
Line 1, characters 0-50:
1 | type +'a t_fail5 = {M : T with type t = 'a} -> M.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type t_fail6 = {M : T} -> 'a constraint 'a = M.t

[%%expect{|
Line 1, characters 45-48:
1 | type t_fail6 = {M : T} -> 'a constraint 'a = M.t
                                                 ^^^
Error: Unbound module M
|}]

let id_fail1 (x : t1) : _ t5 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail1 (x : t1) : _ t5 = x
                                   ^
Error: This expression has type
         t1 = {T : T} -> {A : Add with type t = T.t} -> A.t -> A.t
       but an expression was expected of type
         'a t5 = {M : T with type t = 'a} -> 'a
|}]

let id_fail2 (x : _ t5) : t1 = x

[%%expect{|
Line 1, characters 31-32:
1 | let id_fail2 (x : _ t5) : t1 = x
                                   ^
Error: This expression has type 'a t5 = {M : T with type t = 'a} -> 'a
       but an expression was expected of type
         t1 = {T : T} -> {A : Add with type t = T.t} -> A.t -> A.t
|}]
