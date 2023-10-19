(* TEST
  flags = "-extension module_strengthening"
   * expect
*)

module type S1 = sig
  module type T
  module M : T
  module N : T with M
  module F(X:T) : T
  module O : T with F(M)
end
[%%expect{|
module type S1 =
  sig
    module type T
    module M : T
    module N : (T with M)
    module F : functor (X : T) -> T
    module O : (T with F(M))
  end
|}]

module type S2 = sig
  module type T
  module M : sig end
  module N : T with M
end
[%%expect{|
Line 4, characters 13-21:
4 |   module N : T with M
                 ^^^^^^^^
Error: In this strengthened module type, the type of M
       does not match the underlying type
       Modules do not match: sig end is not included in T
|}]

module type S3 = sig
  module type T = sig type t end
  module M : T
  module N : T with M
end
[%%expect{|
module type S3 =
  sig
    module type T = sig type t end
    module M : T
    module N : sig type t = M.t end
  end
|}]

module type S4 = sig
  module type T = sig type t end
  module M : T
  module N : T with M with type t = int
end
[%%expect{|
Line 4, characters 13-39:
4 |   module N : T with M with type t = int
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = int
       is not included in
         type t = M.t
       The type int is not equal to the type M.t
|}]

module Must_be_subtype = struct
  module M = struct type t1 end
  module type S = sig type t1 type t2 end with M
end
[%%expect{|
Line 3, characters 18-48:
3 |   module type S = sig type t1 type t2 end with M
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this strengthened module type, the type of M
       does not match the underlying type
       Modules do not match:
         sig type t1 end
       is not included in
         sig type t1 type t2 end
       The type `t2' is required but not provided
|}]

module Unaliasable = struct
  module A : sig
    module type S
    module F (X:S) : S with X
    module M : S
  end = struct
    module type S = sig end
    module F(X:S) = X
    module M = struct end
  end
  
  module X = A.F(A.M)
end
[%%expect{|
module Unaliasable :
  sig
    module A :
      sig
        module type S
        module F : functor (X : S) -> (S with X)
        module M : S
      end
    module X : (A.S with A.M [@unaliasable])
  end
|}]

module Subtype = struct
  module type S
  module F(G:functor(X:S) -> S)(X:S)(Y:S with X) = G(Y)
end
[%%expect{|
module Subtype :
  sig
    module type S
    module F :
      functor (G : functor (X : S) -> S) (X : S) (Y : (S with X)) ->
        (S with G(Y))
  end
|}]

module Can_use_type = struct
  module type S = sig type t end
  module A : sig
    module M : S
    module N : S with M
    val foo : M.t -> M.t
    val bar : N.t
  end = struct
    module M = struct type t = int end
    module N = M
    let foo x = x
    let bar = 0
  end

  let x = A.foo A.bar
end
[%%expect{|
module Can_use_type :
  sig
    module type S = sig type t end
    module A :
      sig
        module M : S
        module N : sig type t = M.t end
        val foo : M.t -> M.t
        val bar : N.t
      end
    val x : A.M.t
  end
|}]

module Ignore_strengthening = struct
  module type S = sig type t = int end
  module A : sig
    module M : S
    module F(X : S with M) : S
  end = struct
    module M = struct type t = int end
    module F(X : S with M) = X
  end

  module M = A.F(struct type t = int end)
end
[%%expect{|
module Ignore_strengthening :
  sig
    module type S = sig type t = int end
    module A :
      sig module M : S module F : functor (X : sig type t = int end) -> S end
    module M : S
  end
|}]

module Expand_destructive_with = struct
  module type S = sig
    module type T = sig type t val foo : t -> t end
    module M : T
    module N : T with M
  end

  module F(X:S with type M.t := int) = struct
    let bar = X.N.foo 1
  end
end
[%%expect{|
module Expand_destructive_with :
  sig
    module type S =
      sig
        module type T = sig type t val foo : t -> t end
        module M : T
        module N : sig type t = M.t val foo : t -> t end
      end
    module F :
      functor
        (X : sig
               module type T = sig type t val foo : t -> t end
               module M : sig val foo : int -> int end
               module N : sig type t = int val foo : t -> t end
             end)
        -> sig val bar : X.N.t end
  end
|}]

module Destructive_with1 = struct
  module type S = sig type t end

  module type T = sig
    module M : S
    module N : S with M
  end

  module F(X : S) : T with module M := X = struct
    module N = X
  end
end
[%%expect{|
module Destructive_with1 :
  sig
    module type S = sig type t end
    module type T = sig module M : S module N : sig type t = M.t end end
    module F : functor (X : S) -> sig module N : sig type t = X.t end end
  end
|}]

module Destructive_with2 = struct
  module type S = sig module A : sig type t end end

  module type T = sig
    module M : S
    module N : S with M
  end

  module F(X : S) : T with module M := X = struct
    module N = X
  end
end
[%%expect{|
Lines 9-11, characters 43-5:
 9 | ...........................................struct
10 |     module N = X
11 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module N : sig module A : sig type t = X.A.t end end end
       is not included in
         sig module N : sig module A = X.A end end
       In module N:
       Modules do not match:
         sig module A = N.A end
       is not included in
         sig module A = X.A end
       In module N.A:
       Module X.A cannot be aliased
|}]

module Destructive_with3 = struct
  module type S

  module type T = sig
    module M : S
    module N : S with M
  end

  module F(X : S) : T with module M := X = struct
    module N = X
  end
end
[%%expect{|
Lines 9-11, characters 43-5:
 9 | ...........................................struct
10 |     module N = X
11 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module N : (S with X) end
       is not included in
         sig module N : (S with X) end
       In module N:
       Module X cannot be aliased
|}]

module Remove_aliases = struct
  module type T = sig module type S end

  module F(X : T) = struct
    module type U = sig
      module M : X.S
      module N : sig module O = M end
    end

    module G(Y : U) = struct
      module P = Y.N
    end
  end

  module A = struct
    module type S = sig module Q : sig end end
  end

  module B = F(A)
end
[%%expect{|
module Remove_aliases :
  sig
    module type T = sig module type S end
    module F :
      functor (X : T) ->
        sig
          module type U =
            sig module M : X.S module N : sig module O = M end end
          module G :
            functor
              (Y : sig
                     module M : X.S
                     module N :
                       sig module O : (X.S with M [@unaliasable]) end
                   end)
              -> sig module P : sig module O : (X.S with Y.M) end end
        end
    module A : sig module type S = sig module Q : sig end end end
    module B :
      sig
        module type U =
          sig module M : A.S module N : sig module O = M end end
        module G :
          functor
            (Y : sig
                   module M : A.S
                   module N : sig module O : sig module Q : sig end end end
                 end)
            ->
            sig module P : sig module O : sig module Q : sig end end end end
      end
  end
|}]

module Recursive = struct
  module rec M : sig end with N = N
  and N : sig end = struct end
end
[%%expect{|
Line 2, characters 30-31:
2 |   module rec M : sig end with N = N
                                  ^
Error: Illegal recursive module reference
|}]
