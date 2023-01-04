module Arg = struct
  type t = int
  let v = 58
end

module M = Pack.Make(Arg)

let () =
  let M.P v = M.v in
  Printf.printf "Pack.Make(Arg).v = %d\n%!%!" v
