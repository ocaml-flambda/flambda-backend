(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* We should move these back into [basics.ml] once
   mixed blocks are out of beta. *)

type t_word : word
type ('a : word) t_word_id = 'a

[%%expect{|
type t_word : word
type ('a : word) t_word_id = 'a
|}];;

(****************************************************)
(* Test 5: Allowed in some structures in typedecls. *)

type t5_1 = { x : t_word };;
[%%expect{|
type t5_1 = { x : t_word; }
|}];;

type t5_2 = { y : int; x : t_word };;
[%%expect{|
type t5_2 = { y : int; x : t_word; }
|}];;

type t5_2' = { y : string; x : t_word };;
[%%expect{|
type t5_2' = { y : string; x : t_word; }
|}];;

type t5_4 = A of t_word;;
[%%expect{|
type t5_4 = A of t_word
|}];;

type t5_5 = A of int * t_word;;
[%%expect{|
type t5_5 = A of int * t_word
|}];;

type ('a : word) t5_7 = A of int
type ('a : word) t5_8 = A of 'a;;
[%%expect{|
type ('a : word) t5_7 = A of int
type ('a : word) t5_8 = A of 'a
|}]

type t11_1 = ..

type t11_1 += A of t_word;;
[%%expect{|
type t11_1 = ..
type t11_1 += A of t_word
|}]

(***************************************************)
(* Test 11: Allow word in some extensible variants *)

type t11_1 += B of nativeint#;;
[%%expect{|
type t11_1 += B of nativeint#
|}]

type ('a : word) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : word) t11_2 = ..
type 'a t11_2 += A of int
type 'a t11_2 += B of 'a
|}]

