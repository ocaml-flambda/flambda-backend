(* TEST
 flags = "-extension unique";
 expect;
*)

module Hidden_string : sig
  type t
  val hide : string -> t
end = struct
  type t = string
  let hide x = x
end

module Hidden_int : sig
  type t : immediate
  val hide : int -> t
end = struct
  type t = int
  let hide x = x
end

module Hidden_float_u : sig
  type t : float64
  val hide : float# -> t
end = struct
  type t = float#
  let hide x = x
end

module Hidden_function : sig
  type (-'a, +'b) t
  val hide : ('a -> 'b) -> ('a, 'b) t
end = struct
  type ('a, 'b) t = 'a -> 'b
  let hide x = x
end

[%%expect{|
module Hidden_string : sig type t val hide : string -> t end
module Hidden_int : sig type t : immediate val hide : int -> t end
module Hidden_float_u : sig type t : float64 val hide : float# -> t end
module Hidden_function :
  sig type (-'a, +'b) t val hide : ('a -> 'b) -> ('a, 'b) t end
|}]

module Float_u : sig
  type ('a : float64, 'b : float64) pair
  val mk_pair : ('a : float64) ('b : float64). 'a -> 'b -> ('a, 'b) pair
end = struct
  type ('a : float64, 'b : float64) pair = { fst : 'a; snd : 'b }
  let mk_pair fst snd = { fst; snd }
end

[%%expect{|
module Float_u :
  sig
    type ('a : float64, 'b : float64) pair
    val mk_pair : ('a : float64) ('b : float64). 'a -> 'b -> ('a, 'b) pair
  end
|}]

type float_u_record = { x : float#; y : float# }

[%%expect{|
type float_u_record = { x : float#; y : float#; }
|}]

let string_escape : local_ _ -> string * string = fun x -> x, x

[%%expect{|
Line 1, characters 59-60:
1 | let string_escape : local_ _ -> string * string = fun x -> x, x
                                                               ^
Error: This value escapes its region.
|}]

let int_escape : local_ _ -> int * int = fun x -> x, x

[%%expect{|
val int_escape : local_ int -> int * int = <fun>
|}]

let string_list_escape : local_ _ -> string list * string list = fun x -> x, x

[%%expect{|
Line 1, characters 74-75:
1 | let string_list_escape : local_ _ -> string list * string list = fun x -> x, x
                                                                              ^
Error: This value escapes its region.
|}]

let int_list_escape : local_ _ -> int list * int list = fun x -> x, x

[%%expect{|
Line 1, characters 65-66:
1 | let int_list_escape : local_ _ -> int list * int list = fun x -> x, x
                                                                     ^
Error: This value escapes its region.
|}]

let hidden_string_escape : local_ _ -> Hidden_string.t * Hidden_string.t =
  fun x -> x, x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x, x
               ^
Error: This value escapes its region.
|}]

let hidden_int_escape : local_ _ -> Hidden_int.t * Hidden_int.t =
  fun x -> x, x

[%%expect{|
val hidden_int_escape : local_ Hidden_int.t -> Hidden_int.t * Hidden_int.t =
  <fun>
|}]

let float_escape : local_ _ -> float * float = fun x -> x, x

[%%expect{|
Line 1, characters 56-57:
1 | let float_escape : local_ _ -> float * float = fun x -> x, x
                                                            ^
Error: This value escapes its region.
|}]

(* CR layouts v2.8: The following should pass, even in principal mode. *)
let float_u_escape : local_ _ -> (float#, float#) Float_u.pair =
  fun x -> Float_u.mk_pair x x [@nontail]

[%%expect{|
val float_u_escape : local_ float# -> (float#, float#) Float_u.pair = <fun>
|}, Principal{|
Line 2, characters 27-28:
2 |   fun x -> Float_u.mk_pair x x [@nontail]
                               ^
Error: This value escapes its region.
|}]

let hidden_float_u_escape :
  local_ _ -> (Hidden_float_u.t, Hidden_float_u.t) Float_u.pair =
  fun x -> Float_u.mk_pair x x [@nontail]

[%%expect{|
val hidden_float_u_escape :
  local_ Hidden_float_u.t ->
  (Hidden_float_u.t, Hidden_float_u.t) Float_u.pair = <fun>
|}, Principal{|
Line 3, characters 27-28:
3 |   fun x -> Float_u.mk_pair x x [@nontail]
                               ^
Error: This value escapes its region.
|}]

let float_u_record_escape : local_ _ -> float_u_record * float_u_record =
  fun x -> x, x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x, x
               ^
Error: This value escapes its region.
|}]

let float_u_record_list_escape :
  local_ _ -> float_u_record list * float_u_record list =
  fun x -> x, x

[%%expect{|
Line 3, characters 11-12:
3 |   fun x -> x, x
               ^
Error: This value escapes its region.
|}]

let function_escape : local_ _ -> (int -> int) * (int -> int) = fun x -> x, x

[%%expect{|
Line 1, characters 73-74:
1 | let function_escape : local_ _ -> (int -> int) * (int -> int) = fun x -> x, x
                                                                             ^
Error: This value escapes its region.
|}]

let function_list_escape : local_ _ -> (int -> int) list * (int -> int) list =
  fun x -> x, x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x, x
               ^
Error: This value escapes its region.
|}]

type t_value
let value_duplicate : once_ _ -> t_value = fun x -> x

[%%expect{|
type t_value
Line 2, characters 52-53:
2 | let value_duplicate : once_ _ -> t_value = fun x -> x
                                                        ^
Error: This value is once but expected to be many.
|}]

let int_duplicate : once_ _ -> int = fun x -> x

[%%expect{|
val int_duplicate : once_ int -> int = <fun>
|}]

let value_list_duplicate : once_ _ -> t_value list = fun x -> x

[%%expect{|
Line 1, characters 62-63:
1 | let value_list_duplicate : once_ _ -> t_value list = fun x -> x
                                                                  ^
Error: This value is once but expected to be many.
|}]

let int_list_duplicate : once_ _ -> int list = fun x -> x

[%%expect{|
Line 1, characters 56-57:
1 | let int_list_duplicate : once_ _ -> int list = fun x -> x
                                                            ^
Error: This value is once but expected to be many.
|}]

let hidden_string_duplicate : once_ _ -> Hidden_string.t =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is once but expected to be many.
|}]

let hidden_int_duplicate : once_ _ -> Hidden_int.t =
  fun x -> x

[%%expect{|
val hidden_int_duplicate : once_ Hidden_int.t -> Hidden_int.t = <fun>
|}]

let float_duplicate : once_ _ -> float = fun x -> x

[%%expect{|
Line 1, characters 50-51:
1 | let float_duplicate : once_ _ -> float = fun x -> x
                                                      ^
Error: This value is once but expected to be many.
|}]

let float_u_duplicate : once_ _ -> float# = fun x -> x

[%%expect{|
val float_u_duplicate : once_ float# -> float# = <fun>
|}]

let hidden_float_u_duplicate : once_ _ -> Hidden_float_u.t = fun x -> x

[%%expect{|
val hidden_float_u_duplicate : once_ Hidden_float_u.t -> Hidden_float_u.t =
  <fun>
|}]

let float_u_record_duplicate : once_ _ -> float_u_record =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is once but expected to be many.
|}]

let float_u_record_list_duplicate :
  once_ _ -> float_u_record list =
  fun x -> x

[%%expect{|
Line 3, characters 11-12:
3 |   fun x -> x
               ^
Error: This value is once but expected to be many.
|}]

let function_duplicate : once_ _ -> (int -> int) = fun x -> x

[%%expect{|
Line 1, characters 60-61:
1 | let function_duplicate : once_ _ -> (int -> int) = fun x -> x
                                                                ^
Error: This value is once but expected to be many.
|}]

let function_list_duplicate : once_ _ -> (int -> int) list =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is once but expected to be many.
|}]

let string_unshare : _ -> unique_ string = fun x -> x

[%%expect{|
Line 1, characters 52-53:
1 | let string_unshare : _ -> unique_ string = fun x -> x
                                                        ^
Error: This value is shared but expected to be unique.
|}]

let int_unshare : _ -> unique_ int = fun x -> x

[%%expect{|
val int_unshare : int -> unique_ int = <fun>
|}]

let string_list_unshare : _ -> unique_ string list = fun x -> x

[%%expect{|
Line 1, characters 62-63:
1 | let string_list_unshare : _ -> unique_ string list = fun x -> x
                                                                  ^
Error: This value is shared but expected to be unique.
|}]

let int_list_unshare : _ -> unique_ int list = fun x -> x

[%%expect{|
Line 1, characters 56-57:
1 | let int_list_unshare : _ -> unique_ int list = fun x -> x
                                                            ^
Error: This value is shared but expected to be unique.
|}]

let function_unshare : _ -> unique_ (int -> int) = fun x -> x

[%%expect{|
val function_unshare : (int -> int) -> unique_ (int -> int) = <fun>
|}]

let hidden_string_unshare : _ -> unique_ Hidden_string.t =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is shared but expected to be unique.
|}]

let hidden_int_unshare : _ -> unique_ Hidden_int.t =
  fun x -> x

[%%expect{|
val hidden_int_unshare : Hidden_int.t -> unique_ Hidden_int.t = <fun>
|}]

let float_unshare : _ -> unique_ float = fun x -> x

[%%expect{|
Line 1, characters 50-51:
1 | let float_unshare : _ -> unique_ float = fun x -> x
                                                      ^
Error: This value is shared but expected to be unique.
|}]

let float_u_unshare : _ -> unique_ float# = fun x -> x

[%%expect{|
val float_u_unshare : float# -> unique_ float# = <fun>
|}]

let hidden_float_u_unshare : _ -> unique_ Hidden_float_u.t = fun x -> x

[%%expect{|
val hidden_float_u_unshare : Hidden_float_u.t -> unique_ Hidden_float_u.t =
  <fun>
|}]

let float_u_record_unshare : _ -> unique_ float_u_record =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is shared but expected to be unique.
|}]

let float_u_record_list_unshare :
  _ -> unique_ float_u_record list =
  fun x -> x

[%%expect{|
Line 3, characters 11-12:
3 |   fun x -> x
               ^
Error: This value is shared but expected to be unique.
|}]

let hidden_function_unshare : _ -> unique_ (int, int) Hidden_function.t = fun x -> x

[%%expect{|
Line 1, characters 83-84:
1 | let hidden_function_unshare : _ -> unique_ (int, int) Hidden_function.t = fun x -> x
                                                                                       ^
Error: This value is shared but expected to be unique.
|}]

let function_list_unshare : _ -> unique_ (int -> int) list =
  fun x -> x

[%%expect{|
Line 2, characters 11-12:
2 |   fun x -> x
               ^
Error: This value is shared but expected to be unique.
|}]
