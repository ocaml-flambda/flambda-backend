module V = struct let v = 10 end

module Res = Make(V)

let _ = Format.printf "Res.v: %d \n%!" (Res.f 2)
