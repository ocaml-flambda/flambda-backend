(* TEST
 flags = "-extension unique -extension mode_alpha";
 expect;
*)

type r = {
  mutable x : string;
}

(* In below, printing always gives the strongest modalities possible. Printing
   backtracks the "zapping to strongest" mutation, so the mutation doesn't
   persist. *)

let uncontended_use (_ @ uncontended) = ()
[%%expect{|
type r = { mutable x : string; }
val uncontended_use : 'a -> unit @@ global many = <fun>
|}]

let share_use : 'a -> unit @@ portable = fun _ -> ()
[%%expect{|
val share_use : 'a -> unit @@ global many = <fun>
|}]

let (portable_use @ portable) (_ @ portable) = ()
[%%expect{|
val portable_use : 'a @ portable -> unit @@ global many = <fun>
|}]

(* The compiler building itself is a comprehensive test of legacy modules/values.
   Below we test non-legacy values in modules. *)

module M = struct
  let foo = {x = "hello"}
end
[%%expect{|
module M : sig val foo : r @@ global many end
|}]

module type S = sig
    val x : string @@ global local unique shared once many uncontended contended
      portable nonportable
end
[%%expect{|
module type S =
  sig val x : string @@ global many portable shared contended end
|}]

(* values' comonadic axes must be lower than the module *)
module M = struct
    let local_ x = "hello"
end
[%%expect{|
Line 2, characters 15-16:
2 |     let local_ x = "hello"
                   ^
Error: This value is local, but expected to be global because it is inside a module.
|}]

(* Monadic axes don't have such constraint *)
module M = struct
    let x @ contended = "hello"
end
[%%expect{|
module M : sig val x : string @@ global many portable contended end
|}]

(* Testing the defaulting behaviour.
   "module type of" triggers the defaulting logic.
    Note that the defaulting will mutate the original module type.
*)
module Module_type_of_comonadic = struct
    module M = struct
        let x @ portable = "hello"
    end
    (* for comonadic axes, we default to id = meet_with_max, which is the
    weakest. The original modality is not mutated. *)
    module M' : module type of M = struct
        let x @ portable = "hello"
    end
    let _ = portable_use M.x (* The original modality stays portable *)
    let _ = portable_use M'.x
end
[%%expect{|
Line 11, characters 25-29:
11 |     let _ = portable_use M'.x
                              ^^^^
Error: This value is nonportable but expected to be portable.
|}]

module Module_type_of_monadic = struct
    module M = struct
        let x @ uncontended = "hello"
    end
    module M' : module type of M = M
    (* for monadic axes, we try to push to the id = join_with_min. The original
    modality is pushed to floor. *)
    module M' : module type of M = struct
        let x @ contended = "hello"
    end
end
[%%expect{|
Lines 8-10, characters 35-7:
 8 | ...................................struct
 9 |         let x @ contended = "hello"
10 |     end
Error: Signature mismatch:
       Modules do not match:
         sig val x : string @@ global many portable contended end
       is not included in
         sig val x : string end
       Values do not match:
         val x : string @@ global many portable contended
       is not included in
         val x : string
       The second is empty and the first is contended.
|}]

module Module_type_nested = struct
    module M = struct
        let x @ contended portable = "hello"
        module N = struct
            let y @ uncontended portable = "world"
        end
    end
    module M' : module type of M = struct
        let x = "hello"
        module N = struct
            let y @ contended = "hello"
        end
    end
end
[%%expect{|
Lines 8-13, characters 35-7:
 8 | ...................................struct
 9 |         let x = "hello"
10 |         module N = struct
11 |             let y @ contended = "hello"
12 |         end
13 |     end
Error: Signature mismatch:
       Modules do not match:
         sig
           val x : string @@ global many portable
           module N :
             sig val y : string @@ global many portable contended end
         end
       is not included in
         sig
           val x : string @@ contended
           module N : sig val y : string end
         end
       In module N:
       Modules do not match:
         sig val y : string @@ global many portable contended end
       is not included in
         sig val y : string end
       In module N:
       Values do not match:
         val y : string @@ global many portable contended
       is not included in
         val y : string
       The second is empty and the first is contended.
|}]

(* When defaulting, prioritize modes in arrow types over modalities. *)
(* CR zqian: add tests when this becomes testable. *)

(* When module doesn't have signature, the values' modes/modalities are still
   flexible. *)
module Without_inclusion = struct
    module M = struct
        let x @ portable = "hello"
    end
    let () = portable_use M.x
end
[%%expect{|
module Without_inclusion :
  sig module M : sig val x : string @@ global many portable end end
|}]

module Without_inclusion = struct
    module M = struct
        let x @ nonportable = "hello"
    end
    let () = portable_use M.x
end
[%%expect{|
Line 5, characters 26-29:
5 |     let () = portable_use M.x
                              ^^^
Error: This value is nonportable but expected to be portable.
|}]

module Inclusion_fail = struct
    module M : sig
        val x : string @@ uncontended
    end = struct
        let x @ contended = "hello"
    end
end
[%%expect{|
Lines 4-6, characters 10-7:
4 | ..........struct
5 |         let x @ contended = "hello"
6 |     end
Error: Signature mismatch:
       Modules do not match:
         sig val x : string @@ global many portable contended end
       is not included in
         sig val x : string end
       Values do not match:
         val x : string @@ global many portable contended
       is not included in
         val x : string
       The second is empty and the first is contended.
|}]

module Inclusion_weakens_monadic = struct
    module M : sig
        val x : string @@ contended
    end = struct
        let x @ uncontended = "hello"
    end
    let _ = uncontended_use M.x
end
[%%expect{|
Line 7, characters 28-31:
7 |     let _ = uncontended_use M.x
                                ^^^
Error: This value is contended but expected to be uncontended.
|}]

module Inclusion_weakens_comonadic = struct
  module M : sig
      val x : string @@ nonportable
  end = struct
      let x @ portable = "hello"
  end
  let _ = portable_use M.x
end
[%%expect{|
Line 7, characters 23-26:
7 |   let _ = portable_use M.x
                           ^^^
Error: This value is nonportable but expected to be portable.
|}]

module Inclusion_match = struct
    module M : sig
        val x : string @@ uncontended
    end = struct
        let x @ uncontended = "hello"
    end
    let () = uncontended_use M.x
end
[%%expect{|
module Inclusion_match : sig module M : sig val x : string end end
|}]

(* [foo] closes over [M.x] instead of [M]. This is better ergonomics. *)
module Close_over_value = struct
  module M = struct
    let x @ portable uncontended = "hello"
  end
  let (foo @ portable) () =
    let _ = M.x in
    ()
end
[%%expect{|
module Close_over_value :
  sig
    module M : sig val x : string @@ global many portable end
    val foo : unit -> unit @@ global many portable
  end
|}]

module Close_over_value_monadic = struct
  module M = struct
    let r @ uncontended = "hello"
  end
  let (foo @ portable) () =
    let uncontended_use (_ @ uncontended) = () in
    uncontended_use M.r
end
[%%expect{|
Line 7, characters 20-23:
7 |     uncontended_use M.r
                        ^^^
Error: This value is contended but expected to be uncontended.
|}]

module Close_over_value_comonadic = struct
  module M = struct
    let x @ nonportable = "hello"
  end
  let (foo @ portable) () =
    let _ = M.x in
    ()
end
[%%expect{|
Line 6, characters 12-15:
6 |     let _ = M.x in
                ^^^
Error: The value M.x is nonportable, so cannot be used inside a closure that is portable.
|}]
