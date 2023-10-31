(* TEST
   * expect
*)

module Width = struct
  module type S1 = sig
    val a : int
    val b : int
    val c : int
    val d : int
    val e : int
  end


  module type F = functor
    (X :
      sig
        module M1 : sig include S1 end
        module M2 : sig include S1 end
        module M3 : sig include S1 end
        module M4 : sig include S1 end
        module M5 : sig include S1 end
        module M6 : sig include S1 end
        module M7 : sig include S1 end
      end
    ) -> S1

  module G(F:F)(X:functor() -> S1) = F(X)
end

[%%expect{|
Line 24, characters 37-41:
24 |   module G(F:F)(X:functor() -> S1) = F(X)
                                          ^^^^
Error: Modules do not match: functor () -> S1 is not included in
       sig
         module M1 :
           sig
             val a : int
             val b : int
             val c : int
             val d : int
             val e : int
           end
         module M2 :
           sig val a : int val b : int val c : int val d : int ... end
         module M3 : sig ... end
         module M4 : sig ... end
         module M5 : sig ... end
         module M6 : sig ... end
         module M7 : sig ... end
       end
     Modules do not match:
       functor () -> ...
     is not included in
       functor  -> ...
       An extra argument is provided of module type ()
|}]

module Depth = struct
  module type S = sig
    module M : sig
      module N : sig
        module O : sig
          module P : sig
            module Q : sig
              val x : int
              val y : int
            end
          end
        end
      end
    end
  end

  module type F = functor(X:S) -> S

  module F(F : F)(X : sig module M : sig end end) = F(X)
end

[%%expect{|

Line 19, characters 52-56:
19 |   module F(F : F)(X : sig module M : sig end end) = F(X)
                                                         ^^^^
Error: Modules do not match: sig module M : sig end end is not included in
       S
     In module M:
     Modules do not match:
       sig end
     is not included in
       sig
         module N :
           sig
             module O : sig module P : sig module Q : sig ... end end end
           end
       end
     In module M: The module `N' is required but not provided
|}]
