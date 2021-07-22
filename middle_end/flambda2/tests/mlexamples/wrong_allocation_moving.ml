external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"

external ( *. ) : float -> float -> float = "%mulfloat"
external ( -. ) : float -> float -> float = "%subfloat"

let f x foo =
  let before = minor_words () in
  let y = x *. 2. in
  let after = minor_words () in
  let diff = after -. before in
  foo (Some y) (fun () -> y) diff
