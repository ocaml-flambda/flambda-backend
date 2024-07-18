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

(* In this test, `z` is not unboxed, but `x` is, and one of the calls to `foo`
   (which are all continuations calls, givne the @local), can be found to be
   invalid because of the unboxing (it is not found earlier because the `Int`
   value is hidden thanks to `Sys.opaque_identity).

   In an early version of invalids during unboxing, there was a bug where in
   such cases, there would be missing cases in the extra arguments computed
   by the unboxing. *)
let test f g =
  let[@local] foo (type a) z (x : a bar) =
    match x with
    | I i -> z i
    | F f -> z (int_of_float f)
  in
  let aux = Sys.opaque_identity Int in
  let t : t = T (aux, 0) in
  match t with
  | T (Int, i) -> foo f (I i)
  | T (Float, f) -> foo g (F f)


