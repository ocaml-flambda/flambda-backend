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
Line 1:
Error: Modules do not match: sig end is not included in T
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
Line 1:
Error: Modules do not match:
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
