external nosimp : 'a -> 'a = "%obj_magic"

external ( + ) : int -> int -> int = "%addint"

external opaque : 'a -> 'a = "%opaque"

let test x1 x2 x3 x4 =
  let y = x1, x2 in
  let z = x3, x4 in
  let[@inline never] [@local never] clo1 () =
    let[@inline never] [@local never] rec clo2 () =
      let a, b = y in
      a + b + clo3 ()
    and clo3 () =
      let c, d = z in
      c + d + clo2 ()
    in
    let () = () in
    clo2
  in
  let () = () in
  let clo = clo1 () in
  let () = () in
  clo ()
