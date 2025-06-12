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
module type Destructive_module_subst =
  sig
    module type S = sig module A : sig end end
    module rec M : sig module A : sig type t end end
    and N : sig type alias_MAt = M.A.t end
  end
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
module type Destructive_type_subst =
  sig
    module type S = sig type a : any end
    module rec M : sig type a end
    and N :
      sig type 'a require_value type require_Ma_value = M.a require_value end
  end
|}]
