module V = struct
  type t = int

  let v = 10
  let print = Format.pp_print_int
end

module Res = Make(V)

let _ = Format.printf "(Res.create 2): %a \n%!" Res.print (Res.create 2)
