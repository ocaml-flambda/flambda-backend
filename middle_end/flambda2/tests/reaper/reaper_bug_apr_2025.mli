module Sexp : sig
  type t
end

type t

val t_of_sexp : Sexp.t -> t
