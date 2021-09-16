module M = struct
  let a = List.length

  let b = Sys.os_type

  let c = List.map
end

include M

module F (X : sig
  val x : int
end) =
struct
  let f n = n + X.x

  let g m = m * X.x
end

include F [@inlined never] (struct
  let x = 42
end)
