(* TEST *)

type 'a box = Box of 'a

let[@inline never] f x y z =
  match x, y, z with Box x, Box y, Box z -> x + y + z

type t = int box -> int box -> int
let[@inline always] h x = x
let g = (h f :> int box -> local_ t)

let[@inline never] go a b c =
  let local_ g1 = (g a) in
  let g2 = (g1 b) in
  g2 c

let[@inline never] test () =
  Format.eprintf "%d@." (go (Box 1) (Box 2) (Box 3))

let () = test ()