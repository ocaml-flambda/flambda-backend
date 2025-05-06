(* TEST
 flags = "-extension mode";
 expect;
*)

(* Testing the inferred modalities in modules, including those on sub-modules. *)

type r = {
  mutable x : string;
}

module type Empty = sig end

module type E2E = Empty -> Empty

let uncontended_use (_ @ uncontended) = ()
[%%expect{|
type r = { mutable x : string; }
module type Empty = sig end
module type E2E = Empty -> Empty
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
Error: This is "local", but expected to be "global" because it is inside a module.
|}]

(* CR zqian: currently modules are checked bottom-up, which gives worse error
  messages. The following example should point to [foo] instead. *)
module M @ many = struct
    let (foo @ once) () = ()
end
[%%expect{|
Lines 1-3, characters 18-3:
1 | ..................struct
2 |     let (foo @ once) () = ()
3 | end
Error: This is "once", but expected to be "many".
|}]

(* Monadic axes don't have such constraint *)
module (M @ uncontended) = struct
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
    (* [module type of] zaps the modailty on [x] to identity, which constrains
    submoding [M <= M.x] for comonadic axes *)
    module type S = module type of M
    module M' = (M @ nonportable)
    (* CR zqian: The following should fail but doesn't, due to A limitation
       described in "inferred modalities" in [mode.ml] *)
    let _ = portable_use M'.x (* [M] is nonportable and [x] doesn't have modality *)
end
[%%expect{|
module Module_type_of_comonadic :
  sig
    module M : sig val x : 'a -> 'a end
    module type S = sig val x : 'a -> 'a end
    module M' = M
  end
|}]

module Module_type_of_monadic = struct
    module M = struct
        let x @ contended = ref "hello"
    end
    (* [module type of] zap the modality on [x] to be identity, which constrains
    submoding [M.x <= M] for monadic axes *)
    module type S = module type of M

    module M' = (M @ uncontended)
end
[%%expect{|
Line 9, characters 17-18:
9 |     module M' = (M @ uncontended)
                     ^
Error: This is "contended", but expected to be "uncontended".
|}]

module Module_type_nested = struct
    module M = struct
        let x @ portable = fun t -> t
        module N = struct
            let y @ uncontended = ref "hello"
        end
    end
    (* [module type of] zaps all modalities to id. For this to type check,
    [M'] needs to be at [nonportable] so that [M'.x] is expected at [nonportable].
    [M'] needs to be at [contended] so that [M'.N.y] is expected at [contended].
    *)
    module M' : module type of M = struct
        let x @ nonportable = fun t -> t
        module N = struct
            let y @ contended = ref "hello"
        end
    end
end
[%%expect{|
module Module_type_nested :
  sig
    module M : sig val x : 'a -> 'a module N : sig val y : string ref end end
    module M' :
      sig val x : 'a -> 'a module N : sig val y : string ref end end @@
      contended
  end
|}]

(* When defaulting, prioritize modes in arrow types over modalities. *)
(* CR zqian: add tests when this becomes testable. *)

(* When module doesn't have signature, the values' modes/modalities are still
   flexible. However, using the values will constrain the modes/modalities. *)
module Without_inclusion = struct
    module M = struct
        let x = fun x -> x
        let (y @ nonportable) = fun x -> x (* to avoid the whole module portable *)
    end
    let () = portable_use M.x
end
[%%expect{|
module Without_inclusion :
  sig module M : sig val x : 'a -> 'a @@ portable val y : 'a -> 'a end end
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
        val x : string ref
    end = struct
        let x @ contended = ref "hello"
    end
end
(* For this to type check, M has to be at [contended] *)
[%%expect{|
module Inclusion_fail :
  sig module M : sig val x : string ref end @@ contended end
|}]

module Inclusion_fail = struct
  module M : sig
      val x : string ref
  end @@ uncontended = struct
      let x @ contended = ref "hello"
  end
end
[%%expect{|
Lines 4-6, characters 23-5:
4 | .......................struct
5 |       let x @ contended = ref "hello"
6 |   end
Error: Signature mismatch:
       Modules do not match:
         sig val x : string ref @@ contended end @ uncontended
       is not included in
         sig val x : string ref end @ uncontended
       Values do not match:
         val x : string ref @@ contended @ uncontended
       is not included in
         val x : string ref @ uncontended
       The first is "contended" but the second is "uncontended".
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
      val x : 'a -> 'a
  end = struct
      let x @ portable = fun x -> x
  end
  let _ = portable_use M.x
end
(* [M] is inferred to be [portable] in order to type check *)
[%%expect{|
module Inclusion_weakens_comonadic :
  sig module M : sig val x : 'a -> 'a end end
|}]

module Inclusion_weakens_comonadic = struct
  module M : sig
      val x : 'a -> 'a
  end @@ nonportable = struct
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
    let x @ portable uncontended = fun x -> x
  end
  let (foo @ portable) () =
    let _ = M.x in
    ()
end
[%%expect{|
module Close_over_value :
  sig module M : sig val x : 'a -> 'a end val foo : unit -> unit end
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
    let x @ nonportable = fun x -> x
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
  let (x @ nonportable) = fun x -> x (* to avoid the whole module portable *)
  external length : string -> int = "%string_length"
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let (x @ nonportable) = fun x -> x (* to avoid the whole module portable *)
5 |   external length : string -> int = "%string_length"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val x : 'a -> 'a
           external length : string -> int = "%string_length"
         end @ nonportable
       is not included in
         sig
           external length : string -> int @@ portable = "%string_length"
         end @ nonportable
       Values do not match:
         external length : string -> int = "%string_length" @ nonportable
       is not included in
         external length : string -> int @@ portable = "%string_length" @ nonportable
       The first is "nonportable" but the second is "portable".
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

(* the whole module is portable *)
let () = portable_use M.length
[%%expect{|
module M : sig external length : string -> int = "%string_length" end
|}]

module M' = (M @ nonportable)
let () = portable_use M'.length
[%%expect{|
module M' = M
Line 2, characters 22-31:
2 | let () = portable_use M'.length
                          ^^^^^^^^^
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
    let (f @ nonportable) x = x+1
  end

  module type S_plain = S with module M = Plain
end
[%%expect{|
module type S = sig module M : sig val f : int -> int end end
Lines 13-19, characters 6-3:
13 | ......struct
14 |   module Plain = struct
15 |     let (f @ nonportable) x = x+1
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
         end @ nonportable
       is not included in
         sig
           module Plain : sig val f : int -> int @@ portable end
           module type S_plain =
             sig module M : sig val f : int -> int @@ portable end end
         end @ nonportable
       In module "Plain":
       Modules do not match:
         sig val f : int -> int end @ nonportable
       is not included in
         sig val f : int -> int @@ portable end @ nonportable
       In module "Plain":
       Values do not match:
         val f : int -> int @ nonportable
       is not included in
         val f : int -> int @@ portable @ nonportable
       The first is "nonportable" but the second is "portable".
|}]


(* module inclusion check should look at the modes of the modules to allow more
inclusion. *)

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
         sig val t : [> `Foo ] end @ nonportable
       is not included in
         sig
           val t : [ `Bar of 'a -> 'a | `Baz of string ref | `Foo ] @@
             portable
         end @ nonportable
       Values do not match:
         val t : [> `Foo ] @ nonportable
       is not included in
         val t : [ `Bar of 'a -> 'a | `Baz of string ref | `Foo ] @@ portable @ nonportable
       The first is "nonportable" but the second is "portable".
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

(* module equality/substitution inclusion check doesn't look at modes of modules
since we dont know the mode of the LHS. *)
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
end

[%%expect{|
module type S = sig module M : sig val foo : 'a -> 'a @@ global many end end
Line 9, characters 21-42:
9 |     module type S' = S with module M := M'
                         ^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "M"
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig val foo : 'a -> 'a end
       is not included in
         sig val foo : 'a -> 'a @@ global many end
       Modalities on foo do not match:
       The second is global_ and the first is not.
|}]

module type F = functor (M':sig val foo : 'a -> 'a end) -> sig
  module Eq : sig
    module type S' = S with module M = M'

    module M'' : sig val foo : 'a -> 'a end
    module type S'' = S with module M := M''
  end
end

[%%expect{|
Line 3, characters 21-41:
3 |     module type S' = S with module M = M'
                         ^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "M"
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig val foo : 'a -> 'a end
       is not included in
         sig val foo : 'a -> 'a @@ global many end
       Modalities on foo do not match:
       The second is global_ and the first is not.
|}]


(* strenghtening inclusion check doesn't look at module modes, since we don't
know the mode of the LHS. *)
module type F = functor (M : sig val foo : 'a -> 'a end) -> sig
  module type S = sig val foo : 'a -> 'a @@ global many end with M
end
[%%expect{|
Line 2, characters 18-66:
2 |   module type S = sig val foo : 'a -> 'a @@ global many end with M
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this strengthened module type, the type of "M"
       does not match the underlying type
       Modules do not match:
         sig val foo : 'a -> 'a end
       is not included in
         sig val foo : 'a -> 'a @@ global many end
       Modalities on foo do not match:
       The second is global_ and the first is not.
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
       Modalities on foo do not match:
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
       Modalities on foo do not match:
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

module type Int_nonportable = sig
  val x : int
end

module type Func_portable = sig
  val foo : 'a -> 'a @@ portable
end

module type Func_nonportable = sig
  val baz : 'a -> 'a
end

module type Class = sig
  class cla : object end
end


module type S = sig
  include Int_nonportable
  include Func_portable
  include Func_nonportable
  include Class
end

module type Module = sig
  module M : sig include S end (* to prevent shallow_equal *)
end

module type S' = sig
  include S
  include Module
end

module (M @ nonportable) : S = struct
  let x = 42
  let foo = fun x -> x
  let baz = fun x -> x
  class cla = object end
end
[%%expect{|
module type Int_nonportable = sig val x : int end
module type Func_portable = sig val foo : 'a -> 'a @@ portable end
module type Func_nonportable = sig val baz : 'a -> 'a end
module type Class = sig class cla : object  end end
module type S =
  sig
    val x : int
    val foo : 'a -> 'a @@ portable
    val baz : 'a -> 'a
    class cla : object  end
  end
module type Module =
  sig
    module M :
      sig
        val x : int
        val foo : 'a -> 'a @@ portable
        val baz : 'a -> 'a
        class cla : object  end
      end
  end
module type S' =
  sig
    val x : int
    val foo : 'a -> 'a @@ portable
    val baz : 'a -> 'a
    class cla : object  end
    module M :
      sig
        val x : int
        val foo : 'a -> 'a @@ portable
        val baz : 'a -> 'a
        class cla : object  end
      end
  end
module M : S
|}]

module M' : S' = struct
  include M
  module M = M
end
[%%expect{|
module M' : S'
|}]

(* Pexp_pack only closes over the required things, and if those things are
  portable, the resulted first class module is too. *)
let (bar @ portable) () =
    let k @ portable = (module M : Func_portable) in
    k
[%%expect{|
val bar : unit -> (module Func_portable) = <fun>
|}]

(* Pmod_apply *)
let (bar @ portable) () =
  let module F (X : Func_portable) = struct end in
  let module _ = F(M) in
  ()
[%%expect{|
val bar : unit -> unit = <fun>
|}]

(* Pmod_constraint gives portable, if all required items are portable *)
let (bar @ portable) () =
  let module _ = struct
    module N @ portable = (M : Func_portable)
  end in
  ()
[%%expect{|
val bar : unit -> unit = <fun>
|}]

(* We will now only use Pmod_pack as example; Pmod_apply and Pexp_constraint are
   similiar *)
let (bar @ portable) () =
  let k = (module M : Func_nonportable) in
  k
[%%expect{|
Line 2, characters 18-19:
2 |   let k = (module M : Func_nonportable) in
                      ^
Error: The value "M.baz" is nonportable, so cannot be used inside a function that is portable.
|}]

(* global function can't close over a local module, even though it's coerced
into empty signature. *)
let _ =
  let module M @ local = struct end in
  let (foo @ global) () =
    (module M : Empty)
  in
  foo
[%%expect{|
Line 4, characters 12-13:
4 |     (module M : Empty)
                ^
Error: The module "M" is local, so cannot be used inside a function that might escape.
|}]

(* Empty signature crosses linearity and portability *)
let _ =
  let module M @ nonportable once = struct end in
  let (foo @ portable many) () =
    (module M : Empty)
  in
  foo
[%%expect{|
- : unit -> (module Empty) = <fun>
|}]

(* Functor crosses uniqueness and contention *)
let _ =
  let module (M @ unique uncontended) (X : Empty) = struct end in
  let (foo @ many portable) () =
    let _ @ unique uncontended = (module M : E2E) in
    ()
  in
  foo
[%%expect{|
- : unit -> unit = <fun>
|}]

(* closing over M.x crosses modes *)
let (bar @ portable) () =
  let _ @ portable = (module M : Int_nonportable) in
  ()
[%%expect{|
val bar : unit -> unit = <fun>
|}]

(* If module types are shallow_equal, we still close over the module, even if closing things
  inside would be better *)
module M_Func_portable : Func_portable = M

(* closing over a portable module is fine. The resulting module is contended
because the function is portable *)
let (bar @ portable) () =
  let k = (module M_Func_portable : Func_portable) in
  k
[%%expect{|
module M_Func_portable : Func_portable
val bar : unit -> (module Func_portable) @ contended = <fun>
|}]

module M_Func_portable' @ nonportable = M_Func_portable
let (bar @ portable) () =
  let k = (module M_Func_portable' : Func_portable) in
  k
[%%expect{|
module M_Func_portable' = M_Func_portable
Line 3, characters 18-34:
3 |   let k = (module M_Func_portable' : Func_portable) in
                      ^^^^^^^^^^^^^^^^
Error: The module "M_Func_portable'" is nonportable, so cannot be used inside a function that is portable.
|}]

(* Moreover, note that modules don't cross locality *)
let _ =
  let module M_Func_portable' @ local = M_Func_portable in
  let (bar @ global) () =
    let k = (module M_Func_portable' : Func_portable) in
    k
  in
  bar
[%%expect{|
Line 4, characters 20-36:
4 |     let k = (module M_Func_portable' : Func_portable) in
                        ^^^^^^^^^^^^^^^^
Error: The module "M_Func_portable'" is local, so cannot be used inside a function that might escape.
|}]

(* Closing over a module in a module. *)
let (bar @ portable) () =
  let k = (module M' : Module) in
  k
[%%expect{|
Line 2, characters 18-20:
2 |   let k = (module M' : Module) in
                      ^^
Error: The value "M'.M.baz" is nonportable, so cannot be used inside a function that is portable.
|}]

module type S'_Func_portable = sig module M : Func_portable end

let (bar @ portable) () =
  let k @ portable = (module M' : S'_Func_portable) in
  k
[%%expect{|
module type S'_Func_portable = sig module M : Func_portable end
val bar : unit -> (module S'_Func_portable) = <fun>
|}]

(* closing over a functor is still closing over the functor *)
module type F = sig end -> sig end
module (F @ nonportable) (X : sig end) = struct end
let (bar @ portable) () =
  let k = (module F : F) in
  k
[%%expect{|
module type F = sig end -> sig end
module F : functor (X : sig end) -> sig end
Line 4, characters 18-19:
4 |   let k = (module F : F) in
                      ^
Error: The module "F" is nonportable, so cannot be used inside a function that is portable.
|}]

(* closing over a portable functor is fine *)
module type F = sig end -> sig end
module (F @ portable) (X : sig end) = struct end
let (bar @ portable) () =
  let k = (module F : F) in
  k
[%%expect{|
module type F = sig end -> sig end
module F : functor (X : sig end) -> sig end
val bar : unit -> (module F) = <fun>
|}]

(* closing over class in structure is still prevented *)
let (bar @ portable) () =
  let k = (module M : Class) in
  k
[%%expect{|
Line 2, characters 18-19:
2 |   let k = (module M : Class) in
                      ^
Error: "M.cla" is a class, and classes are always nonportable, so cannot be used inside a function that is portable.
|}]

(* Pmod_unpack requires type equality instead of inclusion, so for a closing-over
to succeed, either the module type can cross modes, or the first class module is
already at good modes. *)
(* CR modes: support the following *)
let m = (module M : Func_portable)
[%%expect{|
val m : (module Func_portable) = <module>
|}]

let (bar @ portable) () =
    let module M' = (val m : Func_portable) in
    ()
[%%expect{|
Line 2, characters 25-26:
2 |     let module M' = (val m : Func_portable) in
                             ^
Error: The value "m" is nonportable, so cannot be used inside a function that is portable.
|}]

(* closing over values from modules crosses modes *)
let (foo @ portable) () =
  let _ @ portable = M.x in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Using F(X).t does not close over F or M *)
module F(X : sig
end) = struct
  type t = string
end

module X = struct end

let (f @ portable) () =
  let _ : F(X).t = "hello" in
  ()
[%%expect{|
module F : functor (X : sig end) -> sig type t = string end
module X : sig end
val f : unit -> unit = <fun>
|}]

(* recursive modules. Modules modes and the modalities are respected *)
module rec M0 : sig
  val f : 'a -> 'a
end @@ portable = struct
  let f = M1.f
end
and M1 : sig
  val f : 'a -> 'a @@ portable
end @@ nonportable = struct
  let f = M0.f
end
[%%expect{|
module rec M0 : sig val f : 'a -> 'a end
and M1 : sig val f : 'a -> 'a @@ portable end
|}]


module rec M0 : sig
  val f : 'a -> 'a
end @@ portable = struct
  let f = M1.f
end
and M1 : sig
  val f : 'a -> 'a @@ portable
end @@ nonportable = struct
  let (f @ nonportable) = M0.f
end
[%%expect{|
Lines 8-10, characters 21-3:
 8 | .....................struct
 9 |   let (f @ nonportable) = M0.f
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end @ nonportable
       is not included in
         sig val f : 'a -> 'a @@ portable end @ nonportable
       Values do not match:
         val f : 'a -> 'a @ nonportable
       is not included in
         val f : 'a -> 'a @@ portable @ nonportable
       The first is "nonportable" but the second is "portable".
|}]

module rec M0 : sig
  val f : 'a -> 'a
end @@ portable = struct
  let (f @ nonportable) = M1.f
end
and M1 : sig
  val f : 'a -> 'a @@ portable
end @@ nonportable = struct
  let f = M0.f
end
[%%expect{|
Lines 3-5, characters 18-3:
3 | ..................struct
4 |   let (f @ nonportable) = M1.f
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end @ nonportable
       is not included in
         sig val f : 'a -> 'a end @ portable
       Values do not match:
         val f : 'a -> 'a @ nonportable
       is not included in
         val f : 'a -> 'a @ portable
       The first is "nonportable" but the second is "portable".
|}]

(* nested signature *)
module M : sig
  module type S = sig module N : sig end @@ portable end
end = struct
  module type S = sig module N : sig end end
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type S = sig module N : sig end end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig module N : sig end end end
       is not included in
         sig module type S = sig module N : sig end @@ portable end end
       Module type declarations do not match:
         module type S = sig module N : sig end end
       does not match
         module type S = sig module N : sig end @@ portable end
       The first module type is not included in the second
       At position "module type S = <here>"
       Module types do not match:
         sig module N : sig end end
       is not equal to
         sig module N : sig end @@ portable end
       At position "module type S = <here>"
       Modalities on N do not match:
       The second is portable and the first is not.
|}]

(* class makes a structure to be nonportable *)
module M @ portable = struct
  class foo = object end
end
[%%expect{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   class foo = object end
3 | end
Error: This is "nonportable", but expected to be "portable".
|}]

module M @ nonportable = struct class foo = object end end
module N = (M : sig class foo : object end end @@ portable)
[%%expect{|
module M : sig class foo : object  end end
Line 2, characters 12-13:
2 | module N = (M : sig class foo : object end end @@ portable)
                ^
Error: Signature mismatch:
       Modules do not match:
         sig class foo : object  end end @ nonportable
       is not included in
         sig class foo : object  end end @ portable
       Class declarations foo do not match:
       First is "nonportable" but second is "portable".
|}]
