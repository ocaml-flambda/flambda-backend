(* TEST
 flags = "-dshape";
 expect;
*)

(* We depart slightly from the example in the PLDI'19 paper, which actually
   doesn't type... *)

module type Stringable = sig
  type t
  val to_string : t -> string
end
[%%expect{|
{
 "Stringable"[module type] -> <.4>;
 }
module type Stringable = sig type t val to_string : t -> string end
|}]

module Pair (X : Stringable) (Y : Stringable) = struct
  type t = X.t * Y.t
  let to_string (x, y) =
    X.to_string x ^ " " ^ Y.to_string y
end
[%%expect{|
{
 "Pair"[module] ->
   Abs<.11>(X, Y, {
                   "t"[type] -> <.7>;
                   "to_string"[value] -> <.8>;
                   });
 }
module Pair :
  functor (X : Stringable) (Y : Stringable) ->
    sig type t = X.t * Y.t val to_string : X.t * Y.t -> string end
|}]

module Int = struct
  type t = int
  let to_string i = string_of_int i
end
[%%expect{|
{
 "Int"[module] -> {<.15>
                   "t"[type] -> <.12>;
                   "to_string"[value] -> <.13>;
                   };
 }
module Int : sig type t = int val to_string : int -> string end
|}]

module String = struct
  type t = string
  let to_string s = s
end
[%%expect{|
{
 "String"[module] -> {<.19>
                      "t"[type] -> <.16>;
                      "to_string"[value] -> <.17>;
                      };
 }
module String : sig type t = string val to_string : 'a -> 'a end
|}]

module P = Pair(Int)(Pair(String)(Int))
[%%expect{|
{
 "P"[module] -> {<.20>
                 "t"[type] -> <.7>;
                 "to_string"[value] -> <.8>;
                 };
 }
module P :
  sig
    type t = Int.t * Pair(String)(Int).t
    val to_string : Int.t * Pair(String)(Int).t -> string
  end
|}];;

P.to_string (0, ("!=", 1))
[%%expect{|
{}
- : string = "0 != 1"
|}]
