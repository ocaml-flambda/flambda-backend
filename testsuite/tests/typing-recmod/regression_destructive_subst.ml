(* TEST
 expect;
*)

module type Destructive_module_subst = sig
  module type S = sig
    module A : sig end
  end

  module rec M : sig
    module A : sig type t end
    include S with module A := A
  end

  and N : sig
    type alias_MAt = M.A.t
  end
end
[%%expect{|
Line 12, characters 21-26:
12 |     type alias_MAt = M.A.t
                          ^^^^^
Error: Unbound type constructor "M.A.t"
|}]

module type Destructive_type_subst = sig
  module type S = sig
    type a : any
  end

  module rec M : sig
    type a
    include S with type a := a
  end

  and N : sig
    type ('a : value) require_value
    type require_Ma_value = M.a require_value
  end
end
[%%expect{|
Line 13, characters 28-31:
13 |     type require_Ma_value = M.a require_value
                                 ^^^
Error: This type "M.a" should be an instance of type "('a : value)"
       The layout of M.a is any
         because of the definition of a at line 3, characters 4-16.
       But the layout of M.a must be a sublayout of value
         because of the definition of require_value at line 12, characters 4-35.
|}]
