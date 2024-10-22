module type M = sig
  type t = int
end

module type F = functor (M : M) -> sig
  type t_list = M.t list
end

(* Include functor *)

module type Example_1 = sig
  type t = int

  include functor F
end
