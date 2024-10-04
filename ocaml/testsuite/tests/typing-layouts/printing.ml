(* TEST
 flags = "-verbose-types";
 expect;
*)

module M : sig
  val f : int -> bool -> char
end = struct
  let f () _ = ()
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f () _ = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a : '_representable_layout_1). unit -> 'a -> unit end
       is not included in
         sig val f : int -> bool -> char end
       Values do not match:
         val f : ('a : '_representable_layout_1). unit -> 'a -> unit
       is not included in
         val f : int -> bool -> char
       The type "unit -> 'a -> unit" is not compatible with the type
         "int -> bool -> char"
       Type "unit" is not compatible with type "int"
|}]
