module Params = struct
 exception E
  type s = { a : int; mutable b : int }
  let[@zero_alloc] test11 s =
    Printf.eprintf "%d\n%!" s.b; raise E
  let[@zero_alloc] test12 ?(s= {a = 4; b = 5}) ~d () =
    test11 s
end
