external word_size : unit -> int = "%word_size"
external (+) : int -> int -> int = "%addint"
external opaque : 'a -> 'a = "%opaque"
let foo =
  match word_size () with
  | 32 -> (fun x -> x + 1)
  | 64 ->
    let y = opaque 2 in
    (fun x -> x + y)
  | _ ->
      assert false
