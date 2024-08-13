(* TEST
 expect;
*)

(* this is a set of tests to test the #show functionality
 * of toplevel *)

class o = object val x = 0 end;;
[%%expect{|
class o : object val x : int end
|}];;
#show o;;
[%%expect{|
Unknown directive "show".
|}];;
class type t = object val x : int end;;
[%%expect{|
class type t = object val x : int end
|}];;
#show t;;
[%%expect{|
Unknown directive "show".
|}];;

#show Foo;;
[%%expect {|
Unknown directive "show".
|}];;

module type S = sig type t val x : t end;;
module M : S = struct type t = int let x = 3 end;;

[%%expect {|
module type S = sig type t val x : t end
module M : S
|}];;

#show M;;
[%%expect {|
Unknown directive "show".
|}];;

#show S;;
[%%expect {|
Unknown directive "show".
|}];;

#show Invalid_argument;;
[%%expect {|
Unknown directive "show".
|}];;

#show Some;;
[%%expect {|
Unknown directive "show".
|}];;

#show option;;
[%%expect {|
Unknown directive "show".
|}];;

#show Open_binary;;
[%%expect {|
Unknown directive "show".
|}];;

#show open_flag;;
[%%expect {|
Unknown directive "show".
|}];;

type extensible = ..;;
type extensible += A | B of int;;
[%%expect {|
type extensible = ..
type extensible += A | B of int
|}];;

#show A;;
[%%expect {|
Unknown directive "show".
|}];;

#show B;;
[%%expect {|
Unknown directive "show".
|}];;

#show extensible;;
[%%expect {|
Unknown directive "show".
|}];;

type 'a t = ..;;
type _ t += A : int t;;
[%%expect{|
type 'a t = ..
type _ t += A : int t
|}];;

#show A;;
[%%expect{|
Unknown directive "show".
|}];;




(* regression tests for #11533 *)
#show Set.OrderedType;;
[%%expect {|
Unknown directive "show".
|}];;

(* extra tests after #11533

   The regression in #11533 would only show up when showing values defined
   outside the current module. Those new tests below test modules and module
   types from the standard library. To minimize test churn / promotion,
   we are looking for some that will change as little as possible
   in the future.

   - For module type it's easy: OrderedType is fixed in stone as
     changing it would break all code using Set.Make.

   - For modules we use Stdlib.Unit, one of the stdlib modules
     that is less likely to change very often (there are only
     so many features you can add to 'unit').
*)
module U = Stdlib.Unit;;
module type OT = Set.OrderedType;;
[%%expect {|
module U = Unit
module type OT = Set.OrderedType
|}];;

#show U;;
[%%expect {|
Unknown directive "show".
|}];;

#show OT;;
[%%expect {|
Unknown directive "show".
|}];;
