module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

(* CR: make unboxed *)
module Portable : sig
  type 'a t : value mod portable = { portable : 'a @@ portable }
end

(* CR: make unboxed *)
module Contended : sig
  type 'a t : value mod uncontended = { contended : 'a @@ contended }
end
