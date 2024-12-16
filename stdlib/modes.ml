module Global = struct
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type 'a t : value mod portable = { portable : 'a @@ portable } [@@unboxed]
end

module Contended = struct
  type 'a t : value mod uncontended = { contended : 'a @@ contended } [@@unboxed]
end

module Portended = struct
  type 'a t : value mod portable uncontended = { portended : 'a @@ portable contended } [@@unboxed]
end
