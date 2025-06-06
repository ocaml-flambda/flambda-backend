external ( + ) : int -> int -> int = "%addint"

let test x =
  let[@inline never] [@local never] f (y, z) =
    let () = () in
    fun w -> x + y + z + w
  in
  let[@inline never] [@local never] g f = f (x, x) x in
  g f
