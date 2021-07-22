external opaque : 'a -> 'a = "%opaque"
let f = false
module A = struct
  let init x y = assert false
  let of__unsafe a = assert false
  let of__debug a = assert false
  let of_ = if f then of__debug else of__unsafe
end
