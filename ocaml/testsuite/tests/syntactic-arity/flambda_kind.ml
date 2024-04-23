(* TEST
   flambda2;
   native;
*)

(* This is a regression test, see PR #2471 in ocaml-flambda/flambda-backend *)

[@@@ocaml.flambda_o3]

type _ value =
  | Int : int value
  | Float : float value

let[@inline never] get (type a) : a value -> a = function
  | Int -> 3
  | Float -> 3.

let[@inline] update (type a) (v : a value) (x : a) : a =
  match v with
  | Int -> x + 1
  | Float -> x +. 1.

let run x = update x (get x)

let (_ : float) = run Float
