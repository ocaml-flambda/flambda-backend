module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable : sig
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended : sig
  type 'a t = { contended : 'a @@ contended } [@@unboxed]
end
