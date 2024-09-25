(* TEST
 expect;
*)

(* mode crossing during inclusion check is according to the written type, not
the inferred type. *)

(* In this example, the inferred type does not allow mode crossing, but the
written type does. *)
module M : sig
    val f : int @ nonportable -> int @ portable
end = struct
    let f (x @ portable) = (x : _ @@ nonportable)
end
[%%expect{|
module M : sig val f : int -> int @ portable end
|}]

(* In this example, the inferred type allows crossing to portable, but the
written type does not. *)
module M : sig
    val f : unit -> [`A | `B of 'a -> 'a] @ portable
end = struct
    let f () = (`A : _ @@ nonportable)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |     let f () = (`A : _ @@ nonportable)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> [> `A ] end
       is not included in
         sig val f : unit -> [ `A | `B of 'a -> 'a ] @ portable end
       Values do not match:
         val f : unit -> [> `A ]
       is not included in
         val f : unit -> [ `A | `B of 'a -> 'a ] @ portable
       The type "unit -> [ `A | `B of 'a -> 'a ]"
       is not compatible with the type
         "unit -> [ `A | `B of 'a -> 'a ] @ portable"
|}]

(* In this example, the inferred type does not allow crossing portability, but
the written type does. *)
module M : sig
    val f : [`A] @ nonportable -> unit
end = struct
    let f (x : [< `A | `B of string -> string] @@ portable) =
        match x with
        | `A -> ()
        | `B f -> ()
end
[%%expect{|
module M : sig val f : [ `A ] -> unit end
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
       The type "unit -> local_ int" is not compatible with the type
         "unit -> int"
|}]

module M : sig
    val f : local_ int -> int
end = struct
    let f (_ @ global) = 42
end
[%%expect{|
module M : sig val f : local_ int -> int end
|}]
