module rec M0 : sig
  type t
  val foo : t -> t
end = struct
  type t = M.t
  let foo t = M.foo 42 t
end and M : sig
  type t
  val foo : int -> t -> t
end = struct
  include TD.Make (M0)
end and TD : sig
  module Make (X : sig
    type t
    val foo : t -> t
  end) : sig
    type t
    val s : t
    val na : X.t -> t
    val foo : int -> t -> t
  end
end = struct
  module Make (X : sig
    type t
    val foo : t -> t
  end) = struct
    type t =
      | S
      | NA of X.t

    let s = S
    let na x = NA x

    let foo _i t =
      match t with
      | S -> S
      | NA x -> NA (X.foo x)
  end
end
