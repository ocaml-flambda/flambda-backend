module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable : sig
  type 'a t : value mod portable = { portable : 'a @@ portable } [@@unboxed]
end

module Contended : sig
  type 'a t : value mod uncontended = { contended : 'a @@ contended } [@@unboxed]
end
