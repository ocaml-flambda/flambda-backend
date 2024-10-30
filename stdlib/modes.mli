(* CR tdelvecchio: Document. *)

module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

(* CR tdelvecchio: make unboxed *)
module Portable : sig
  type 'a t : value mod portable = { portable : 'a @@ portable }
end

(* CR tdelvecchio: make unboxed *)
module Contended : sig
  type 'a t : value mod uncontended = { contended : 'a @@ contended }
end

(* CR tdelvecchio: make unboxed *)
module Sync : sig
  type 'a t : value mod portable uncontended = { sync : 'a @@ portable contended }
end
