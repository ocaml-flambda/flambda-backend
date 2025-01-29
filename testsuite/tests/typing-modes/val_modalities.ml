(* TEST
 flags = "-extension mode";
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
val uncontended_use : 'a -> unit = <fun>
|}]

let share_use : 'a -> unit @@ portable = fun _ -> ()
[%%expect{|
val share_use : 'a -> unit = <fun>
|}]

let (portable_use @ portable) (_ @ portable) = ()
[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
|}]

(* The compiler building itself is a comprehensive test of legacy modules/values.
   Below we test non-legacy values in modules. *)

module M = struct
  let foo = {x = "hello"}
end
[%%expect{|
module M : sig val foo : r end
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
module M : sig val x : string @@ contended end
|}]

(* Testing the defaulting behaviour.
   "module type of" triggers the defaulting logic.
    Note that the defaulting will mutate the original module type: it zaps the
    inferred modalities and make them fully fixed. *)
module Module_type_of_comonadic = struct
    module M = struct
        let x @ portable = fun x -> x
    end
    (* for comonadic axes, we default to id = meet_with_max, which is the
    weakest. *)
    module M' : module type of M = struct
        let x @ portable = fun x -> x
    end
    let _ = portable_use M.x (* The original inferred modality is zapped *)
end
[%%expect{|
Line 10, characters 25-28:
10 |     let _ = portable_use M.x (* The original inferred modality is zapped *)
                              ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

module Module_type_of_monadic = struct
    module M = struct
        let x @ uncontended = ref "hello"
    end
    module M' : module type of M = M
    (* for monadic axes, we try to push to the id = join_with_min. The original
    modality is pushed to floor. *)
    module M' : module type of M = struct
        let x @ contended = ref "hello"
    end
end
[%%expect{|
Lines 8-10, characters 35-7:
 8 | ...................................struct
 9 |         let x @ contended = ref "hello"
10 |     end
Error: Signature mismatch:
       Modules do not match:
         sig val x : string ref @@ contended end
       is not included in
         sig val x : string ref end
       Values do not match:
         val x : string ref @@ contended
       is not included in
         val x : string ref
       The second is uncontended and the first is contended.
|}]

module Module_type_nested = struct
    module M = struct
        let x @ portable = fun t -> t
        module N = struct
            let y @ uncontended = ref "hello"
        end
    end
    module M' : module type of M = struct
        let x @ nonportable = fun t -> t
        module N = struct
            let y @ contended = ref "hello"
        end
    end
end
[%%expect{|
Lines 8-13, characters 35-7:
 8 | ...................................struct
 9 |         let x @ nonportable = fun t -> t
10 |         module N = struct
11 |             let y @ contended = ref "hello"
12 |         end
13 |     end
Error: Signature mismatch:
       Modules do not match:
         sig
           val x : 'a -> 'a
           module N : sig val y : string ref @@ contended end
         end
       is not included in
         sig val x : 'a -> 'a module N : sig val y : string ref end end
       In module "N":
       Modules do not match:
         sig val y : string ref @@ contended end
       is not included in
         sig val y : string ref end
       In module "N":
       Values do not match:
         val y : string ref @@ contended
       is not included in
         val y : string ref
       The second is uncontended and the first is contended.
|}]

(* When defaulting, prioritize modes in arrow types over modalities. *)
(* CR zqian: add tests when this becomes testable. *)

(* When module doesn't have signature, the values' modes/modalities are still
   flexible. However, using the values will constrain the modes/modalities. *)
module Without_inclusion = struct
    module M = struct
        let x @ portable = fun x -> x
    end
    let () = portable_use M.x
end
[%%expect{|
module Without_inclusion :
  sig module M : sig val x : 'a -> 'a @@ portable end end
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
        val x : string ref @@ uncontended
    end = struct
        let x @ contended = ref "hello"
    end
end
[%%expect{|
Lines 4-6, characters 10-7:
4 | ..........struct
5 |         let x @ contended = ref "hello"
6 |     end
Error: Signature mismatch:
       Modules do not match:
         sig val x : string ref @@ contended end
       is not included in
         sig val x : string ref end
       Values do not match:
         val x : string ref @@ contended
       is not included in
         val x : string ref
       The second is uncontended and the first is contended.
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
    module M : sig val x : string @@ portable end
    val foo : unit -> unit
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
       The second is portable and the first is nonportable.
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
           module Plain : sig val f : int -> int end
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
         sig val f : int -> int end
       is not included in
         sig val f : int -> int @@ portable end
       In module "Plain":
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int @@ portable
       The second is portable and the first is nonportable.
|}]


(* module inclusion check should look at the modes of the modules, since some
module type inclusion is only true for certain modes. Currently modules are
always global many, which allows more module inclusion. *)

(* value description inclusion check look at the modes of the enclosing
   structure. *)
module M : sig
  val foo : 'a -> 'a @@ global many
end = struct
  include (struct let foo x = x end : sig val foo : 'a -> 'a end)
end
[%%expect{|
module M : sig val foo : 'a -> 'a @@ global many end
|}]

(* CR zqian: with non-legacy modules, we will extend the tests to modalities on
module declarations, instead of relying on modalities on value descriptions to
tell if the extra modes are considered. *)

(* module declaration inclusion check looks at the mode of the enclosing
   structure, which in turn affects value description inclusion check. *)
module M : sig
  module N : sig val foo : 'a -> 'a @@ global many end
end = struct
  module N : sig val foo : 'a -> 'a end = struct let foo x = x end
end
[%%expect{|
module M : sig module N : sig val foo : 'a -> 'a @@ global many end end
|}]

(* inclusion check should cross modes, if we are comparing modes (instead of
  modalities) *)
module M : sig
  val foo : int @@ portable uncontended
end = struct
  let foo @ nonportable contended = 42
end
[%%expect{|
module M : sig val foo : int @@ portable end
|}]

(* The RHS type (expected type) is used for mode crossing. The following still
passes because types are substituted. *)
module M : sig
  type t
  val t : t @@ portable uncontended
end = struct
  type t = int
  let t @ nonportable contended = 42
end
[%%expect{|
module M : sig type t val t : t @@ portable end
|}]

(* LHS type is a subtype of RHS type, which means more type-level information.
That doesn't matter for mode crossing for most cases, except for poly variants.
The following examples seem to suggest that we should use LHS type for mode
crossing, but I couldn't find examples to really demonstrate that. *)
module M : sig
  val t : [`Foo | `Bar] @@ portable uncontended
end = struct
  let t @ nonportable contended = `Foo
end
[%%expect{|
module M : sig val t : [ `Bar | `Foo ] @@ portable end
|}]

module M : sig
  val t : [`Foo | `Bar of 'a -> 'a | `Baz of string ref] @@ portable uncontended
end = struct
  let t @ nonportable contended = `Foo
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let t @ nonportable contended = `Foo
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val t : [> `Foo ] @@ contended end
       is not included in
         sig
           val t : [ `Bar of 'a -> 'a | `Baz of string ref | `Foo ] @@
             portable
         end
       Values do not match:
         val t : [> `Foo ] @@ contended
       is not included in
         val t : [ `Bar of 'a -> 'a | `Baz of string ref | `Foo ] @@ portable
       The second is portable and the first is nonportable.
|}]

(* module constraint inclusion check looks at the modes of modules *)
module F (M : sig val foo : 'a -> 'a end) = struct
  module M' : sig val foo : 'a -> 'a @@ global many end = M
end
[%%expect{|
module F :
  functor (M : sig val foo : 'a -> 'a end) ->
    sig module M' : sig val foo : 'a -> 'a @@ global many end end
|}]

(* Similiar for recursive modules *)
module rec M : sig
  module N : sig val foo : 'a -> 'a @@ global many end
end = struct
  module N : sig val foo : 'a -> 'a end = struct let foo x = x end
end
[%%expect{|
module rec M : sig module N : sig val foo : 'a -> 'a @@ global many end end
|}]


(* functor application inclusion check looks at the modes of parameter and
   argument *)
module F (M : sig val f : 'a -> 'a @@ global many end) = struct
end
[%%expect{|
module F : functor (M : sig val f : 'a -> 'a @@ global many end) -> sig end
|}]

module G (M : sig val f : 'a -> 'a end) = F(M)
[%%expect{|
module G : functor (M : sig val f : 'a -> 'a end) -> sig end
|}]

(* Similiar for [include_functor] *)
module G (M : sig val f : 'a -> 'a end) = struct
  include M
  include functor F
end
[%%expect{|
module G : functor (M : sig val f : 'a -> 'a end) -> sig val f : 'a -> 'a end
|}]

(* functor declaration inclusion check  looks at the modes of parameter and
  return*)
module F : (sig val foo : 'a -> 'a end) -> (sig val bar : 'a -> 'a @@ global many end) =
functor (M : sig val foo : 'a -> 'a @@ global many end) -> struct let bar = M.foo end
[%%expect{|
module F :
  sig val foo : 'a -> 'a end -> sig val bar : 'a -> 'a @@ global many end
|}]

(* CR zqian: package subtyping doesn't look at the package mode for simplicity.
NB: coercion is the only place of subtype checking packages; all other places
are equality check. *)
module type S = sig val foo : 'a -> 'a @@ global many end
module type S' = sig val foo : 'a -> 'a end

let f (x : (module S)) = (x : (module S) :> (module S'))
[%%expect{|
module type S = sig val foo : 'a -> 'a @@ global many end
module type S' = sig val foo : 'a -> 'a end
val f : (module S) -> (module S') = <fun>
|}]

let f (x : (module S')) = (x : (module S') :> (module S))
[%%expect{|
Line 1, characters 26-57:
1 | let f (x : (module S')) = (x : (module S') :> (module S))
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module S')" is not a subtype of "(module S)"
|}]

(* module equality/substitution inclusion check looks at modes of modules, even
   when inside a module type declaration *)
module type S = sig
  module M : sig
    val foo : 'a -> 'a @@ global many
  end
end

module type F = functor (M':sig val foo : 'a -> 'a end) -> sig
  module Subst : sig
    module type S' = S with module M := M'

    module M'' : sig val foo : 'a -> 'a end
    module type S'' = S with module M := M''
  end

  module Eq : sig
    module type S' = S with module M = M'

    module M'' : sig val foo : 'a -> 'a end
    module type S'' = S with module M := M''
  end
end

[%%expect{|
module type S = sig module M : sig val foo : 'a -> 'a @@ global many end end
module type F =
  functor (M' : sig val foo : 'a -> 'a end) ->
    sig
      module Subst :
        sig
          module type S' = sig end
          module M'' : sig val foo : 'a -> 'a end
          module type S'' = sig end
        end
      module Eq :
        sig
          module type S' = sig module M : sig val foo : 'a -> 'a end end
          module M'' : sig val foo : 'a -> 'a end
          module type S'' = sig end
        end
    end
|}]

(* strenghtening inclusion check looks at module modes, even inside a module
  type declaration. *)
module type F = functor (M : sig val foo : 'a -> 'a end) -> sig
  module type S = sig val foo : 'a -> 'a @@ global many end with M
end
[%%expect{|
module type F =
  functor (M : sig val foo : 'a -> 'a end) ->
    sig module type S = sig val foo : 'a -> 'a @@ global many end end
|}]


(* module type declaration inclusion check doesn't look at the enclosing
   structure's mode, because that mode is irrelevant. *)
module M : sig
  module type S = sig val foo : 'a end
end = struct
  module type S = sig val foo : 'a @@ global many end
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type S = sig val foo : 'a @@ global many end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig val foo : 'a @@ global many end end
       is not included in
         sig module type S = sig val foo : 'a end end
       Module type declarations do not match:
         module type S = sig val foo : 'a @@ global many end
       does not match
         module type S = sig val foo : 'a end
       The second module type is not included in the first
       At position "module type S = <here>"
       Module types do not match:
         sig val foo : 'a end
       is not equal to
         sig val foo : 'a @@ global many end
       At position "module type S = <here>"
       Values do not match:
         val foo : 'a
       is not included in
         val foo : 'a @@ global many
       The second is global_ and the first is not.
|}]

(* Module declaration inclusion check inside a module type declaration inclusion
  check. There is no "enclosing module mode" to look at. *)
module M : sig
  module type N = sig
    module M : sig val foo : 'a -> 'a end
  end
end = struct
  module type N = sig
    module M : sig val foo : 'a -> 'a @@ global many end
  end
end
[%%expect{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   module type N = sig
7 |     module M : sig val foo : 'a -> 'a @@ global many end
8 |   end
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type N =
             sig module M : sig val foo : 'a -> 'a @@ global many end end
         end
       is not included in
         sig
           module type N = sig module M : sig val foo : 'a -> 'a end end
         end
       Module type declarations do not match:
         module type N =
           sig module M : sig val foo : 'a -> 'a @@ global many end end
       does not match
         module type N = sig module M : sig val foo : 'a -> 'a end end
       The second module type is not included in the first
       At position "module type N = <here>"
       Module types do not match:
         sig module M : sig val foo : 'a -> 'a end end
       is not equal to
         sig module M : sig val foo : 'a -> 'a @@ global many end end
       At position "module type N = sig module M : <here> end"
       Modules do not match:
         sig val foo : 'a -> 'a end
       is not included in
         sig val foo : 'a -> 'a @@ global many end
       At position "module type N = sig module M : <here> end"
       Values do not match:
         val foo : 'a -> 'a
       is not included in
         val foo : 'a -> 'a @@ global many
       The second is global_ and the first is not.
|}]

(* functor type inclusion: the following two functor types are equivalent,
  because a functor of the first type at any mode, can be zero-runtime casted
  to the second type at the same mode. Essentially, the parameter and return
  mode is in the functor type, and doesn't depend on the mode of the functor. *)
module M : sig
  module type F = (sig val foo : 'a @@ global many end) ->
    (sig end)
end = struct
  module type F = (sig val foo : 'a end) ->
    (sig end)
end
[%%expect{|
module M :
  sig module type F = sig val foo : 'a @@ global many end -> sig end end
|}]

module M : sig
  module type F =
    (sig end) -> (sig val foo : 'a end)
end = struct
  module type F =
    (sig end) -> (sig val foo : 'a @@ global many end)
end
[%%expect{|
module M : sig module type F = sig end -> sig val foo : 'a end end
|}]

module type T = sig @@ portable
  val foo : 'a -> 'a
  val bar : 'a -> 'a @@ nonportable
  val baz : 'a -> 'a @@ portable
end
[%%expect{|
module type T =
  sig
    val foo : 'a -> 'a @@ portable
    val bar : 'a -> 'a
    val baz : 'a -> 'a @@ portable
  end
|}]

(* default modalities does not go deep into module types *)
module type T = sig @@ portable
  module type T = sig
    val foo : 'a -> 'a
  end
end
[%%expect{|
module type T = sig module type T = sig val foo : 'a -> 'a end end
|}]

(* default modalities is overridden as a whole, not per-axis *)
(* CR zqian: make overriding per-axis *)
module type T = sig @@ portable
  val foo : 'a -> 'a @@ contended
end
[%%expect{|
module type T = sig val foo : 'a -> 'a @@ contended end
|}]

(* default modalities is a syntax sugar that doesn't constitute the meaning of
   a module type *)
module type SR = sig @@ portable
  end

module type SL = sig
  end

module F (X : SL) : SR = X
[%%expect{|
module type SR = sig end
module type SL = sig end
module F : functor (X : SL) -> SR
|}]


(* interaction between open and locks *)
module M_nonportable = struct
    let f @ nonportable = fun () -> ()
end

module M_portable = struct
    let f @ portable = fun () -> ()
    end
[%%expect{|
module M_nonportable : sig val f : unit -> unit end
module M_portable : sig val f : unit -> unit end
|}]

let (foo @ portable) () =
    let open M_nonportable in
    let _ = f in
    ()
[%%expect{|
Line 3, characters 12-13:
3 |     let _ = f in
                ^
Error: The value "f" is nonportable, so cannot be used inside a function that is portable.
|}]

let (_foo @ portable) () =
    let open M_portable in
    let _ = f in
    ()

[%%expect{|
val _foo : unit -> unit = <fun>
|}]

let () =
  let open M_nonportable in
  let (foo @ portable) () =
    let _ = f in
    ()
  in
  ()
[%%expect{|
Line 4, characters 12-13:
4 |     let _ = f in
                ^
Error: The value "f" is nonportable, so cannot be used inside a function that is portable.
|}]

let () =
  let open M_portable in
  let (_foo @ portable) () =
    let _ = f in
    ()
  in
  ()
[%%expect{|
|}]

(* CR zqian: finer treatment of packing and unpacking *)
module type Empty = sig end

module type S = sig
  val foo : 'a -> 'a
  val baz : 'a -> 'a @@ portable
end

module M : S = struct
  let foo = fun x -> x
  let baz = fun x -> x
end
[%%expect{|
module type Empty = sig end
module type S = sig val foo : 'a -> 'a val baz : 'a -> 'a @@ portable end
module M : S
|}]

let (bar @ portable) () =
    let m = (module M : Empty) in
    ()
[%%expect{|
Line 2, characters 20-21:
2 |     let m = (module M : Empty) in
                        ^
Error: Modules are nonportable, so cannot be used inside a function that is portable.
|}]

let m = (module M : S)
[%%expect{|
val m : (module S) = <module>
|}]

let (bar @ portable) () =
    let module M' = (val m : Empty) in
    ()
[%%expect{|
Line 2, characters 25-26:
2 |     let module M' = (val m : Empty) in
                             ^
Error: The value "m" is nonportable, so cannot be used inside a function that is portable.
|}]

(* CR zqian: this mode crossing should work *)
module M : sig
  val x : int
end = struct
  let x = 42
end

let (foo @ portable) () =
  let _ = M.x in
  ()
[%%expect{|
module M : sig val x : int end
Line 8, characters 10-13:
8 |   let _ = M.x in
              ^^^
Error: The value "M.x" is nonportable, so cannot be used inside a function that is portable.
|}]
