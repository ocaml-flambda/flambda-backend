(* TEST
 expect;
*)

module M : sig
    val f : int @ nonportable -> int @ portable
end = struct
    let f (_ @ portable) = (42 : _ @@ nonportable)
end
[%%expect{|
module M : sig val f : int -> int @ portable end
|}]

module M : sig
    val f : unit -> int
end = struct
    let f () = exclave_ 42
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |     let f () = exclave_ 42
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> local_ int end
       is not included in
         sig val f : unit -> int end
       Values do not match:
         val f : unit -> local_ int
       is not included in
         val f : unit -> int
       The type unit -> local_ int is not compatible with the type
         unit -> int
|}]

module M : sig
    val f : local_ int -> int
end = struct
    let f (_ @ global) = 42
end
[%%expect{|
module M : sig val f : local_ int -> int end
|}]
