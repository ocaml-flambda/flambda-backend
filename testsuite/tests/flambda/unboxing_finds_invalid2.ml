(* TEST *)

[@@@flambda_o3]

type _ foo =
  | Int : int foo
  | Float : float foo

type _ bar =
  | I : int -> int bar
  | F : float -> float bar

type t = T : 'a foo * 'a -> t

let[@inline never] bar b = b

(* Here, both `b` and `x` are unboxed, and in an early version of
   invalids during unboxing, this results in an overlap of rewrite id
   between extra args computed for `b` and the invalids (which were found
   when computing the extra args for `x`). *)
let test () =
  let[@local] foo (type a) b (x : a bar) =
    match x with
    | I i -> if b then i else 0
    | F f -> if b then int_of_float f else 0
  in
  let aux = Sys.opaque_identity Int in
  let t : t = T (aux, 0) in
  match t with
  | T (Int, i) -> foo true (I i)
  | T (Float, f) -> foo false (F f)


