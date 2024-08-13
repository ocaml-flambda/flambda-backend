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

module Hidden_int64_u : sig
  type t : bits64
  val hide : int64# -> t
end = struct
  type t = int64#
  let hide x = x
end

[%%expect{|
module Hidden_string : sig type t val hide : string -> t end
module Hidden_int : sig type t : immediate val hide : int -> t end
module Hidden_float_u : sig type t : float64 val hide : float# -> t end
module Hidden_int64_u : sig type t : bits64 val hide : int64# -> t end
|}]

module Immediate : sig
  val id : ('a : immediate). 'a -> 'a
  val ignore : ('a : immediate). 'a -> unit
  val unique : ('a : immediate). unique_ 'a -> 'a
end = struct
  let id x = x
  let ignore _ = ()
  let unique (unique_ x) = x
end

[%%expect{|
module Immediate :
  sig
    val id : ('a : immediate). 'a -> 'a
    val ignore : ('a : immediate). 'a -> unit
    val unique : ('a : immediate). unique_ 'a -> 'a
  end
|}]

module Float_u : sig
  val id : ('a : float64). 'a -> 'a
  val ignore : ('a : float64). 'a -> unit
  val unique : ('a : float64). unique_ 'a -> 'a
end = struct
  let id x = x
  let ignore _ = ()
  let unique (unique_ x) = x
end

[%%expect{|
module Float_u :
  sig
    val id : ('a : float64). 'a -> 'a
    val ignore : ('a : float64). 'a -> unit
    val unique : ('a : float64). unique_ 'a -> 'a
  end
|}]

module Int64_u : sig
  val id : ('a : bits64). 'a -> 'a
  val ignore : ('a : bits64). 'a -> unit
  val unique : ('a : bits64). unique_ 'a -> 'a
end = struct
  let id x = x
  let ignore _ = ()
  let unique (unique_ x) = x
end

[%%expect{|
module Int64_u :
  sig
    val id : ('a : bits64). 'a -> 'a
    val ignore : ('a : bits64). 'a -> unit
    val unique : ('a : bits64). unique_ 'a -> 'a
  end
|}]

type float_u_record = { x : float#; y : float# }

[%%expect{|
type float_u_record = { x : float#; y : float#; }
|}]

let string_escape = let local_ x : string = "hello" in x

[%%expect{|
Line 1, characters 55-56:
1 | let string_escape = let local_ x : string = "hello" in x
                                                           ^
Error: This value escapes its region.
|}]

let int_escape = let local_ x : int = 5 in x

[%%expect{|
val int_escape : int = 5
|}]

let string_list_escape = let local_ x : string list = ["hi";"bye"] in x

[%%expect{|
Line 1, characters 70-71:
1 | let string_list_escape = let local_ x : string list = ["hi";"bye"] in x
                                                                          ^
Error: This value escapes its region.
|}]

let int_list_escape = let local_ x : int list = [4;5] in x

[%%expect{|
Line 1, characters 57-58:
1 | let int_list_escape = let local_ x : int list = [4;5] in x
                                                             ^
Error: This value escapes its region.
|}]

let hidden_string_escape =
  let local_ x : Hidden_string.t = Hidden_string.hide "hello" in x

[%%expect{|
Line 2, characters 65-66:
2 |   let local_ x : Hidden_string.t = Hidden_string.hide "hello" in x
                                                                     ^
Error: This value escapes its region.
|}]

let hidden_int_escape =
  let local_ x : Hidden_int.t = Hidden_int.hide 42 in x

[%%expect{|
val hidden_int_escape : Hidden_int.t = <abstr>
|}]

let hidden_string_list_escape =
  let local_ x : Hidden_string.t list =
    [Hidden_string.hide "hi"; Hidden_string.hide "bye"]
  in x

[%%expect{|
Line 4, characters 5-6:
4 |   in x
         ^
Error: This value escapes its region.
|}]

let hidden_int_list_escape =
  let local_ x : Hidden_int.t list =
    [Hidden_int.hide 2; Hidden_int.hide 3]
  in x

[%%expect{|
Line 4, characters 5-6:
4 |   in x
         ^
Error: This value escapes its region.
|}]

let float_escape = let local_ x : float = 3.14 in x

[%%expect{|
Line 1, characters 50-51:
1 | let float_escape = let local_ x : float = 3.14 in x
                                                      ^
Error: This value escapes its region.
|}]

let float_u_escape () = let local_ x : float# = #3.14 in x

[%%expect{|
val float_u_escape : unit -> float# = <fun>
|}]

let int64_u_escape () = let local_ x : int64# = #314L in x

[%%expect{|
val int64_u_escape : unit -> int64# = <fun>
|}]

let hidden_float_u_escape () =
  let local_ x : Hidden_float_u.t = Hidden_float_u.hide #3.14 in x

[%%expect{|
val hidden_float_u_escape : unit -> Hidden_float_u.t = <fun>
|}]

let hidden_int64_u_escape () =
  let local_ x : Hidden_int64_u.t = Hidden_int64_u.hide #314L in x

[%%expect{|
Line 2, characters 65-66:
2 |   let local_ x : Hidden_int64_u.t = Hidden_int64_u.hide #314L in x
                                                                     ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let float_u_record_escape =
  let local_ x : float_u_record = { x = #3.14; y = #2.718 } in x

[%%expect{|
Line 2, characters 63-64:
2 |   let local_ x : float_u_record = { x = #3.14; y = #2.718 } in x
                                                                   ^
Error: This value escapes its region.
|}]

let float_u_record_list_escape =
  let local_ x : float_u_record list = [] in x

[%%expect{|
Line 2, characters 45-46:
2 |   let local_ x : float_u_record list = [] in x
                                                 ^
Error: This value escapes its region.
|}]

type r = {x : float; y : float}

let foo () =
  let local_ r = {x = 3.0; y = 4.0} in
  (* [r.x] is allocated global and can escape. *)
  r.x

[%%expect{|
type r = { x : float; y : float; }
val foo : unit -> float = <fun>
|}]

let function_escape = let local_ x : int -> int = fun y -> y in x

[%%expect{|
Line 1, characters 64-65:
1 | let function_escape = let local_ x : int -> int = fun y -> y in x
                                                                    ^
Error: This value escapes its region.
|}]

let function_list_escape =
  let local_ x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in x

[%%expect{|
Line 2, characters 71-72:
2 |   let local_ x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in x
                                                                           ^
Error: This value escapes its region.
|}]

let string_duplicate = let once_ x : string = "hello" in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 64-65:
1 | let string_duplicate = let once_ x : string = "hello" in Fun.id x
                                                                    ^
Error: This value is "once" but expected to be "many".
|}]

let int_duplicate = let once_ x : int = 5 in Fun.id x

[%%expect{|
val int_duplicate : int = 5
|}]

let string_list_duplicate = let once_ x : string list = ["hi";"bye"] in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 79-80:
1 | let string_list_duplicate = let once_ x : string list = ["hi";"bye"] in Fun.id x
                                                                                   ^
Error: This value is "once" but expected to be "many".
|}]

let int_list_duplicate = let once_ x : int list = [4;5] in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 66-67:
1 | let int_list_duplicate = let once_ x : int list = [4;5] in Fun.id x
                                                                      ^
Error: This value is "once" but expected to be "many".
|}]

let hidden_string_duplicate =
  let once_ x : Hidden_string.t = Hidden_string.hide "hello" in Fun.id x

[%%expect{|
Line 2, characters 71-72:
2 |   let once_ x : Hidden_string.t = Hidden_string.hide "hello" in Fun.id x
                                                                           ^
Error: This value is "once" but expected to be "many".
|}]

let hidden_int_duplicate =
  let once_ x : Hidden_int.t = Hidden_int.hide 42 in Fun.id x

[%%expect{|
val hidden_int_duplicate : Hidden_int.t = <abstr>
|}]

let hidden_string_list_duplicate =
  let once_ x : Hidden_string.t list =
    [Hidden_string.hide "hi"; Hidden_string.hide "bye"]
  in Fun.id x

[%%expect{|
Line 4, characters 12-13:
4 |   in Fun.id x
                ^
Error: This value is "once" but expected to be "many".
|}]

let hidden_int_list_duplicate =
  let once_ x : Hidden_int.t list =
    [Hidden_int.hide 2; Hidden_int.hide 3]
  in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 4, characters 12-13:
4 |   in Fun.id x
                ^
Error: This value is "once" but expected to be "many".
|}]

let float_duplicate = let once_ x : float = 3.14 in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 59-60:
1 | let float_duplicate = let once_ x : float = 3.14 in Fun.id x
                                                               ^
Error: This value is "once" but expected to be "many".
|}]

let float_u_duplicate () = let once_ x : float# = #3.14 in Float_u.id x

[%%expect{|
val float_u_duplicate : unit -> float# = <fun>
|}]

let int64_u_duplicate () = let once_ x : int64# = #314L in Int64_u.id x

[%%expect{|
val int64_u_duplicate : unit -> int64# = <fun>
|}]

let hidden_float_u_duplicate () =
  let once_ x : Hidden_float_u.t = Hidden_float_u.hide #3.14 in Float_u.id x

[%%expect{|
val hidden_float_u_duplicate : unit -> Hidden_float_u.t = <fun>
|}]

let hidden_int64_u_duplicate () =
  let once_ x : Hidden_int64_u.t = Hidden_int64_u.hide #314L in Int64_u.id x

[%%expect{|
Line 2, characters 75-76:
2 |   let once_ x : Hidden_int64_u.t = Hidden_int64_u.hide #314L in Int64_u.id x
                                                                               ^
Error: This value is "once" but expected to be "many".
|}]

let float_u_record_duplicate =
  let once_ x : float_u_record = { x = #3.14; y = #2.718 } in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 2, characters 69-70:
2 |   let once_ x : float_u_record = { x = #3.14; y = #2.718 } in Fun.id x
                                                                         ^
Error: This value is "once" but expected to be "many".
|}]

let float_u_record_list_duplicate =
  let once_ x : float_u_record list = [] in Fun.id x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 2, characters 51-52:
2 |   let once_ x : float_u_record list = [] in Fun.id x
                                                       ^
Error: This value is "once" but expected to be "many".
|}]

let function_duplicate = let once_ x : int -> int = fun y -> y in Fun.id x

[%%expect{|
Line 1, characters 73-74:
1 | let function_duplicate = let once_ x : int -> int = fun y -> y in Fun.id x
                                                                             ^
Error: This value is "once" but expected to be "many".
|}]

let function_list_duplicate =
  let once_ x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in Fun.id x

[%%expect{|
Line 2, characters 77-78:
2 |   let once_ x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in Fun.id x
                                                                                 ^
Error: This value is "once" but expected to be "many".
|}]

let unique (unique_ x) = x

[%%expect{|
val unique : unique_ 'a -> 'a = <fun>
|}]

let string_unshare = let x : string = "hello" in ignore x; unique x

[%%expect{|
Line 1, characters 66-67:
1 | let string_unshare = let x : string = "hello" in ignore x; unique x
                                                                      ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 56-57:
1 | let string_unshare = let x : string = "hello" in ignore x; unique x
                                                            ^

|}]

let int_unshare = let x : int = 5 in ignore x; unique x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 54-55:
1 | let int_unshare = let x : int = 5 in ignore x; unique x
                                                          ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 44-45:
1 | let int_unshare = let x : int = 5 in ignore x; unique x
                                                ^

|}]

let string_list_unshare =
  let x : string list = ["hi";"bye"] in ignore x; unique x

[%%expect{|
Line 2, characters 57-58:
2 |   let x : string list = ["hi";"bye"] in ignore x; unique x
                                                             ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 47-48:
2 |   let x : string list = ["hi";"bye"] in ignore x; unique x
                                                   ^

|}]

let int_list_unshare = let x : int list = [4;5] in ignore x; unique x

[%%expect{|
Line 1, characters 68-69:
1 | let int_list_unshare = let x : int list = [4;5] in ignore x; unique x
                                                                        ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 58-59:
1 | let int_list_unshare = let x : int list = [4;5] in ignore x; unique x
                                                              ^

|}]

let hidden_string_unshare =
  let x : Hidden_string.t = Hidden_string.hide "hello" in ignore x; unique x

(* CR layouts v2.8: Why is this error message different?? *)
[%%expect{|
Line 2, characters 75-76:
2 |   let x : Hidden_string.t = Hidden_string.hide "hello" in ignore x; unique x
                                                                               ^
Error: This value is "shared" but expected to be "unique".
|}]

let hidden_int_unshare =
  let x : Hidden_int.t = Hidden_int.hide 42 in ignore x; unique x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 2, characters 64-65:
2 |   let x : Hidden_int.t = Hidden_int.hide 42 in ignore x; unique x
                                                                    ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 54-55:
2 |   let x : Hidden_int.t = Hidden_int.hide 42 in ignore x; unique x
                                                          ^

|}]

let hidden_string_list_unshare =
  let x : Hidden_string.t list =
    [Hidden_string.hide "hi"; Hidden_string.hide "bye"]
  in ignore x; unique x

[%%expect{|
Line 4, characters 22-23:
4 |   in ignore x; unique x
                          ^
Error: This value is "shared" but expected to be "unique".
|}]

let hidden_int_list_unshare =
  let x : Hidden_int.t list =
    [Hidden_int.hide 2; Hidden_int.hide 3]
  in ignore x; unique x

[%%expect{|
Line 4, characters 22-23:
4 |   in ignore x; unique x
                          ^
Error: This value is used here as unique, but it has already been used:
Line 4, characters 12-13:
4 |   in ignore x; unique x
                ^

|}]

let float_unshare = let x : float = 3.14 in ignore x; unique x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 61-62:
1 | let float_unshare = let x : float = 3.14 in ignore x; unique x
                                                                 ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 51-52:
1 | let float_unshare = let x : float = 3.14 in ignore x; unique x
                                                       ^

|}]

(* CR layouts v2.8: The following should pass, even in principal mode, because the
argument kind is known to cross mode. *)

let float_u_unshare () = let x : float# = #3.14 in Float_u.ignore x; Float_u.unique x

[%%expect{|
val float_u_unshare : unit -> float# = <fun>
|}, Principal{|
Line 1, characters 84-85:
1 | let float_u_unshare () = let x : float# = #3.14 in Float_u.ignore x; Float_u.unique x
                                                                                        ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 66-67:
1 | let float_u_unshare () = let x : float# = #3.14 in Float_u.ignore x; Float_u.unique x
                                                                      ^

|}]

let int64_u_unshare () = let x : int64# = #314L in Int64_u.ignore x; Int64_u.unique x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 84-85:
1 | let int64_u_unshare () = let x : int64# = #314L in Int64_u.ignore x; Int64_u.unique x
                                                                                        ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 66-67:
1 | let int64_u_unshare () = let x : int64# = #314L in Int64_u.ignore x; Int64_u.unique x
                                                                      ^

|}]

let imm_escape () = Immediate.id (local_ 42) [@nontail]

[%%expect{|
val imm_escape : unit -> int = <fun>
|}, Principal{|
Line 1, characters 33-44:
1 | let imm_escape () = Immediate.id (local_ 42) [@nontail]
                                     ^^^^^^^^^^^
Error: This value escapes its region.
|}]

let hidden_float_u_unshare () =
  let x : Hidden_float_u.t = Hidden_float_u.hide #3.14 in
  Float_u.ignore x; Float_u.unique x

[%%expect{|
val hidden_float_u_unshare : unit -> Hidden_float_u.t = <fun>
|}, Principal{|
Line 3, characters 35-36:
3 |   Float_u.ignore x; Float_u.unique x
                                       ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 17-18:
3 |   Float_u.ignore x; Float_u.unique x
                     ^

|}]

let hidden_int64_u_unshare () =
  let x : Hidden_int64_u.t = Hidden_int64_u.hide #314L in
  Int64_u.ignore x; Int64_u.unique x

[%%expect{|
Line 3, characters 35-36:
3 |   Int64_u.ignore x; Int64_u.unique x
                                       ^
Error: This value is "shared" but expected to be "unique".
|}]

let float_u_record_unshare =
  let x : float_u_record = { x = #3.14; y = #2.718 } in ignore x; unique x

[%%expect{|
Line 2, characters 73-74:
2 |   let x : float_u_record = { x = #3.14; y = #2.718 } in ignore x; unique x
                                                                             ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 63-64:
2 |   let x : float_u_record = { x = #3.14; y = #2.718 } in ignore x; unique x
                                                                   ^

|}]

let float_u_record_list_unshare =
  let x : float_u_record list = [] in ignore x; unique x

[%%expect{|
Line 2, characters 55-56:
2 |   let x : float_u_record list = [] in ignore x; unique x
                                                           ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 45-46:
2 |   let x : float_u_record list = [] in ignore x; unique x
                                                 ^

|}]

let function_unshare = let x : int -> int = fun y -> y in ignore x; unique x

(* CR layouts v2.8: this should succeed *)
[%%expect{|
Line 1, characters 75-76:
1 | let function_unshare = let x : int -> int = fun y -> y in ignore x; unique x
                                                                               ^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 65-66:
1 | let function_unshare = let x : int -> int = fun y -> y in ignore x; unique x
                                                                     ^

|}]

let function_list_unshare =
  let x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in ignore x; unique x

[%%expect{|
Line 2, characters 81-82:
2 |   let x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in ignore x; unique x
                                                                                     ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 71-72:
2 |   let x : (int -> int) list = [(fun y -> y); fun z -> z + 1] in ignore x; unique x
                                                                           ^

|}]

(* CR layouts: this should succeed. *)
let foo : (string -> string) -> (string -> string) @ unique
  = fun f -> f
[%%expect{|
Line 2, characters 13-14:
2 |   = fun f -> f
                 ^
Error: This value is "shared" but expected to be "unique".
|}]
