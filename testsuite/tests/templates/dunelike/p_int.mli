type t = int

val create : unit -> t
val frob : t -> t
val to_string : t -> string

module Flunf : sig
  module Biz : sig
    type t
    val thing : int
  end

  module Boz : sig
    type t
    val stuff : int
  end
end

include module type of struct
  module Fleez = (Flunf : sig module Biz : sig type t end end)
end

val frob_biz : Fleez.Biz.t -> Fleez.Biz.t
