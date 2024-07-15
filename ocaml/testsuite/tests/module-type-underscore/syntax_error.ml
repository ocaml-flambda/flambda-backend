(* TEST
 toplevel;
*)

module M : sig
  module type S = sig
    type t
    val foo : t -> t
    val bar : t list -> t
  end

  module type T := _
end = struct
  module type S = _
end ;;
