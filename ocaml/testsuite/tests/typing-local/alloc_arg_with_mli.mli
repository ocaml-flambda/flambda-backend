type t = { a : int; b : int }

module M1 : sig
  val take_unrestricted : t -> int
end

module M2 : sig
  val take_unrestricted : t -> int
end

module M3 : sig
  val take_local : local_ t -> int
end

module M4 : sig
  val take_local : local_ t -> int
end

module M5 : sig
  val take_global : t -> int
end

module M6 : sig
  val take_local__global_in_mli : t -> int
end

module M7 : sig
  val take_local__global_in_mli : t -> int
end
