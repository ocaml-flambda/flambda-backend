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
Error: Modules do not match: T is not included in sig end
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
