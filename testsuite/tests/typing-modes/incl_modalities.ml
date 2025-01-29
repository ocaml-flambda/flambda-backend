(* TEST
    flags += "-extension mode";
    expect;
*)

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

(* default modalities affect include modalities, which is deep. *)
module type T = sig
  module M : sig
    val foo : 'a -> 'a
  end
end
[%%expect{|
module type T = sig module M : sig val foo : 'a -> 'a end end
|}]

module type T = sig @@ portable
  include T
end
[%%expect{|
module type T = sig module M : sig val foo : 'a -> 'a @@ portable end end
|}]
