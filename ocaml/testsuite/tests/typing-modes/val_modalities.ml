(* TEST
 flags = "-extension mode_alpha";
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
    val x : string @@ global local unique aliased once many uncontended contended
      portable nonportable
end
[%%expect{|
module type S =
  sig val x : string @@ global many portable aliased contended end
|}]

(* values' comonadic axes must be lower than the module *)
module M = struct
    let local_ x = "hello"
end
[%%expect{|
Line 2, characters 15-16:
2 |     let local_ x = "hello"
                   ^
Error: This value is "local", but expected to be "global" because it is inside a module.
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
        let x @ portable = fun x -> x
    end
    (* for comonadic axes, we default to id = meet_with_max, which is the
    weakest. The original modality is not mutated. *)
    module M' : module type of M = struct
        let x @ portable = fun x -> x
    end
    let _ = portable_use M.x (* The original modality stays portable *)
    let _ = portable_use M'.x
end
[%%expect{|
Line 11, characters 25-29:
11 |     let _ = portable_use M'.x
                              ^^^^
Error: This value is "nonportable" but expected to be "portable".
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
       In module "N":
       Modules do not match:
         sig val y : string @@ global many portable contended end
       is not included in
         sig val y : string end
       In module "N":
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
        let x @ portable = fun x -> x
    end
    let () = portable_use M.x
end
[%%expect{|
module Without_inclusion :
  sig module M : sig val x : 'a -> 'a @@ global many portable end end
|}]

module Without_inclusion = struct
    module M = struct
        let x @ nonportable = fun x -> x
    end
    let () = portable_use M.x
end
[%%expect{|
Line 5, characters 26-29:
5 |     let () = portable_use M.x
                              ^^^
Error: This value is "nonportable" but expected to be "portable".
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
        val x : int ref @@ contended
    end = struct
        let x @ uncontended = ref 10
    end
    let _ = uncontended_use M.x
end
[%%expect{|
Line 7, characters 28-31:
7 |     let _ = uncontended_use M.x
                                ^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

module Inclusion_weakens_comonadic = struct
  module M : sig
      val x : 'a -> 'a @@ nonportable
  end = struct
      let x @ portable = fun x -> x
  end
  let _ = portable_use M.x
end
[%%expect{|
Line 7, characters 23-26:
7 |   let _ = portable_use M.x
                           ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

module Inclusion_match = struct
    module M : sig
        val x : int ref @@ uncontended
    end = struct
        let x @ uncontended = ref 10
    end
    let () = uncontended_use M.x
end
[%%expect{|
module Inclusion_match : sig module M : sig val x : int ref end end
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

(* CR mode-crossing: This is used for the below test in place of a mutable record. *)
module M : sig
  type t
  val mk : t @@ portable
end = struct
  type t = unit
  let mk = ()
end
[%%expect {|
module M : sig type t val mk : t @@ portable end
|}]

module Close_over_value_monadic = struct
  module M = struct
    let r @ uncontended = M.mk
  end
  let (foo @ portable) () =
    let uncontended_use (_ @ uncontended) = () in
    uncontended_use M.r
end
[%%expect{|
Line 7, characters 20-23:
7 |     uncontended_use M.r
                        ^^^
Error: This value is "contended" but expected to be "uncontended".
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
Error: The value "M.x" is nonportable, so cannot be used inside a function that is portable.
|}]

(* Modalities on primitives are supported. They are simpler than real values,
   because primitives have the same parsetree in [sig] and [struct]. In
   particular, both contain [val_modalities] already, so we just do a simple
   sub-modality check. *)
module M : sig
  external length : string -> int @@ portable = "%string_length"
end = struct
  external length : string -> int = "%string_length"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external length : string -> int = "%string_length"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external length : string -> int = "%string_length" end
       is not included in
         sig
           external length : string -> int @@ portable = "%string_length"
         end
       Values do not match:
         external length : string -> int = "%string_length"
       is not included in
         external length : string -> int @@ portable = "%string_length"
       The second is portable and the first is not.
|}]

module M : sig
  external length : string -> int @@ portable = "%string_length"
end = struct
  external length : string -> int @@ portable = "%string_length"
end

let _ = portable_use M.length
[%%expect{|
module M :
  sig external length : string -> int @@ portable = "%string_length" end
- : unit = ()
|}]

(* weakening to non-portable *)
module M : sig
  external length : string -> int = "%string_length"
end = struct
  external length : string -> int @@ portable = "%string_length"
end

let _ = portable_use M.length
[%%expect{|
module M : sig external length : string -> int = "%string_length" end
Line 7, characters 21-29:
7 | let _ = portable_use M.length
                         ^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* The example below demonstrates the need to zap modalities from [with module]
   constraints.  A similar example appears in the zero_alloc tests, because
   [zero_alloc] variables must be treated similarly. *)
module type S = sig
  module M : sig
    val f : int -> int
  end
end

module N : sig
  module Plain : sig
    val f : int -> int
  end

  module type S_plain = S with module M = Plain
end = struct
  module Plain = struct
    let f x = x+1
  end

  module type S_plain = S with module M = Plain
end
[%%expect{|
module type S = sig module M : sig val f : int -> int end end
module N :
  sig
    module Plain : sig val f : int -> int end
    module type S_plain = sig module M : sig val f : int -> int end end
  end
|}]

(* This revised version of that example does not typecheck. It would be nice if
   it did, but to make it do so seems hard. In the case of zero_alloc we can fix
   this with a zero_alloc annotation in the structure, but there is currently no
   equivalent for that with modalities. *)
module type S = sig
  module M : sig
    val f : int -> int
  end
end

module N : sig
  module Plain : sig
    val f : int -> int @@ portable
  end

  module type S_plain = S with module M = Plain
end = struct
  module Plain = struct
    let f x = x+1
  end

  module type S_plain = S with module M = Plain
end
[%%expect{|
module type S = sig module M : sig val f : int -> int end end
Lines 13-19, characters 6-3:
13 | ......struct
14 |   module Plain = struct
15 |     let f x = x+1
16 |   end
17 |
18 |   module type S_plain = S with module M = Plain
19 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module Plain : sig val f : int -> int @@ global many end
           module type S_plain =
             sig module M : sig val f : int -> int end end
         end
       is not included in
         sig
           module Plain : sig val f : int -> int @@ portable end
           module type S_plain =
             sig module M : sig val f : int -> int @@ portable end end
         end
       In module "Plain":
       Modules do not match:
         sig val f : int -> int @@ global many end
       is not included in
         sig val f : int -> int @@ portable end
       In module "Plain":
       Values do not match:
         val f : int -> int @@ global many
       is not included in
         val f : int -> int @@ portable
       The second is portable and the first is not.
|}]

(* Including module type with modalities *)
module type S = sig
  val foo : 'a -> 'a

  val bar : 'a -> 'a @@ nonportable

  val baz : 'a -> 'a @@ portable
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a @@ portable
    val bar : 'a -> 'a @@ portable
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include S @@ nonportable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

(* Include functor module types with modalities *)
module type S = functor (_ : sig end) -> sig
  val foo : 'a -> 'a

  val bar : 'a -> 'a @@ nonportable

  val baz : 'a -> 'a @@ portable
end
[%%expect{|
module type S =
  sig end ->
    sig
      val foo : 'a -> 'a
      val bar : 'a -> 'a
      val baz : 'a -> 'a @@ portable
    end
|}]

module type S' = sig
  include functor S @@ portable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a @@ portable
    val bar : 'a -> 'a @@ portable
    val baz : 'a -> 'a @@ portable
  end
|}]

module type S' = sig
  include functor S @@ nonportable
end
[%%expect{|
module type S' =
  sig
    val foo : 'a -> 'a
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

(* CR zqian: add tests of recursive modules & include w/ modalties, once
   modules can have modes. *)

module type S = sig
  val bar : 'a -> 'a
  module M : sig
    val foo : 'a -> 'a
  end
end
[%%expect{|
module type S =
  sig val bar : 'a -> 'a module M : sig val foo : 'a -> 'a end end
|}]

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S' =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a @@ portable end
  end
|}]

module type S' = sig
  include [@no_recursive_modalities] S @@ portable
end
[%%expect{|
module type S' =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a end
  end
|}]

module type T = sig
  val baz : 'a -> 'a
  module M : S
end
[%%expect{|
module type T = sig val baz : 'a -> 'a module M : S end
|}]

module type T' = sig
  include T @@ portable
end
[%%expect{|
module type T' =
  sig
    val baz : 'a -> 'a @@ portable
    module M :
      sig
        val bar : 'a -> 'a @@ portable
        module M : sig val foo : 'a -> 'a @@ portable end
      end
  end
|}]

module type T' = sig
  include [@no_recursive_modalities] T @@ portable
end
[%%expect{|
module type T' = sig val baz : 'a -> 'a @@ portable module M : S end
|}]

(* submodule whose type is in the signature *)
module type S = sig
  module type MT = sig
    val foo : 'a -> 'a
  end
  module M : MT
end

module type S' = sig
  include S @@ portable
end
[%%expect{|
module type S =
  sig module type MT = sig val foo : 'a -> 'a end module M : MT end
module type S' =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : sig val foo : 'a -> 'a @@ portable end
  end
|}]

(* and this works deeply *)
module type S = sig
  module type MT = sig
    val foo : 'a -> 'a
  end
  module M : sig
    module N : MT
  end
end
module type S' = sig
include S @@ portable
end
[%%expect{|
module type S =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : sig module N : MT end
  end
module type S' =
  sig
    module type MT = sig val foo : 'a -> 'a end
    module M : sig module N : sig val foo : 'a -> 'a @@ portable end end
  end
|}]

(* submodule whose type is not in the signature but inside a module *)
module M = struct
  module type Foo = sig
    val foo : 'a -> 'a
  end
  module type Foo' = Foo
  module type S = sig
    module N : Foo'
  end
end
module type S' = sig
  include M.S @@ portable
end
[%%expect{|
module M :
  sig
    module type Foo = sig val foo : 'a -> 'a end
    module type Foo' = Foo
    module type S = sig module N : Foo' end
  end
module type S' = sig module N : sig val foo : 'a -> 'a @@ portable end end
|}]

(* include abstract module type is still not allowed *)
module type S = sig
  module type MT
  include MT @@ portable
end
[%%expect{|
Line 3, characters 10-12:
3 |   include MT @@ portable
              ^^
Error: This module type is not a signature
|}]

(* submodule of abstract type is not affected by modality *)
module type MT
module type S = sig
  module M : MT
end
module type S' = sig
include S @@ portable
end
[%%expect{|
module type MT
module type S = sig module M : MT end
module type S' = sig module M : MT end
|}]

(* strenghtened module type *)
module type S = sig
  module type T = sig
    type a
    val baz : a
    val foo : a -> a
  end
  module MT : T
  module M : T with MT
end
module type S' = sig
include S @@ portable
end
[%%expect{|
module type S =
  sig
    module type T = sig type a val baz : a val foo : a -> a end
    module MT : T
    module M : sig type a = MT.a val baz : a val foo : a -> a end
  end
module type S' =
  sig
    module type T = sig type a val baz : a val foo : a -> a end
    module MT :
      sig type a val baz : a @@ portable val foo : a -> a @@ portable end
    module M :
      sig
        type a = MT.a
        val baz : a @@ portable
        val foo : a -> a @@ portable
      end
  end
|}]
