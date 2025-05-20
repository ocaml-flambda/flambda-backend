(* TEST
    flags += "-extension mode";
    expect;
*)

(* the semantics of pmd_modalities *)
module type S = sig @@ portable
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end
end
[%%expect{|
module type S =
  sig
    module M :
      sig val foo : 'a -> 'a module N : sig val bar : 'a -> 'a end end @@
      portable
  end
|}]

module type S = sig
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end @@ portable
end
[%%expect{|
module type S =
  sig
    module M :
      sig val foo : 'a -> 'a module N : sig val bar : 'a -> 'a end end @@
      portable
  end
|}]


module type S = sig @@ portable
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end @@ nonportable
end
[%%expect{|
module type S =
  sig
    module M :
      sig val foo : 'a -> 'a module N : sig val bar : 'a -> 'a end end
  end
|}]

(* works for recursive modules as well *)
module type S = sig @@ portable
    module rec M : sig
        val foo : 'a -> 'a
        (* illegal to refer to N's type, so the fact that we are using a
        workaround semantics is invisible. *)
    end @@ nonportable
    and N : sig
        val bar : 'a -> 'a
    end
end
[%%expect{|
module type S =
  sig
    module rec M : sig val foo : 'a -> 'a end
    and N : sig val bar : 'a -> 'a end @@ portable
  end
|}]

(* works on Mty_ident as well *)
module type T = sig
  val foo : 'a -> 'a
end

module type S = sig @@ portable
  module M : T
end
[%%expect{|
module type T = sig val foo : 'a -> 'a end
module type S = sig module M : T @@ portable end
|}]

(* works for Mty_functor *)
module type S = sig @@ portable
  module M : (sig val foo : 'a -> 'a end) -> (sig val bar : 'a -> 'a end)
end
[%%expect{|
module type S =
  sig
    module M : sig val foo : 'a -> 'a end -> sig val bar : 'a -> 'a end @@
      portable
  end
|}]

module M : T = struct
  let (foo @ nonportable) x = x
end


(* works for Mty_alias *)
module type S = sig @@ portable
  module M' = M
end
[%%expect{|
module M : T
module type S = sig module M' = M @@ portable end
|}]

(* works for Mty_strenthen, and type check keeps working *)
module type S = sig @@ portable
  module M' : T with M
end
[%%expect{|
module type S = sig module M' : sig val foo : 'a -> 'a end @@ portable end
|}]

module M : S = struct
  module M' = M
end
[%%expect{|
Lines 1-3, characters 15-3:
1 | ...............struct
2 |   module M' = M
3 | end
Error: Signature mismatch:
       Modules do not match: sig module M' = M end is not included in S
       Got "nonportable" but expected "portable".
|}]
