module Make(Param : sig type t val v : t end) : sig
  type t = P of Param.t

  val v : t
end
