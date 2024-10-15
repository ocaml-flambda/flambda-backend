module Global = struct
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended= struct
  type 'a t = { contended : 'a @@ contended } [@@unboxed]
end
