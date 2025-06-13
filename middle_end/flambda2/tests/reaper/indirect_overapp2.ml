external ( + ) : int -> int -> int = "%addint"

let test x h b =
  let[@inline never] [@local never] f y =
    let () = () in
    fun z -> x + y + z
  in
  let[@inline never] [@local never] ind_call p =
    let _ = p x in
    ()
  in
  let[@inline never] [@local never] g f = f x x in
  ind_call (if b then f else h);
  g f
