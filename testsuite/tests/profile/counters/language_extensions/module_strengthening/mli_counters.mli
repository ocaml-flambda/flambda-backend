(* Module strengthening syntax *)

module type Example_2 = sig
  module type T = sig type t end
  module M : T
  module N : T with M
end

(* Standard syntax (should not be counted) *)

module type Example_1 = sig
  module type T = sig type t end
  module M : T
  module N : sig type t = M.t end
end
