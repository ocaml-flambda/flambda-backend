module Global = struct
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type 'a t : value mod portable = { portable : 'a @@ portable }
end

module Contended = struct
  type 'a t : value mod uncontended = { contended : 'a @@ contended }
end
