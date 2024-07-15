module type S = sig
    type t

    val foo : t -> t
    val bar : t -> unit
end
