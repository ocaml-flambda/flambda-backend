external ( + ) : int -> int -> int = "%addint"

let test x =
  let[@inline never] [@local never] f (y, z) =
    let () = () in
    fun (w, u) -> x + y + z + w + u
  in
  let[@inline never] [@local never] g f = f (x, x) (x, x) in
  g f
