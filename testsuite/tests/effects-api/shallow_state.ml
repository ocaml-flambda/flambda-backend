(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

open Effect

(*
let handle_state init f x =
  let rec loop state k x =
    continue k x with
    | result -> result, state
    | effect Get, k -> loop state k state
    | effect Set new_state, k -> loop new_state k ()
  in
  loop init (fiber f) x
*)

type 'a op =
  | Get : int op
  | Set : int -> unit op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let handle_state init f x =
  let rec handle (state : int) = function
    | Value result -> result, state
    | Exception e -> raise e
    | Operation (Get, k) -> handle state (continue k state [])
    | Operation (Set new_state, k) -> handle new_state (continue k () [])
  in
  let res = run (fun h -> f h x) in
  handle init res
;;

let comp h () =
  Printf.printf "Initial state: %d\n" (perform h Get);
  perform h (Set 42);
  Printf.printf "Updated state: %d\n" (perform h Get);
  perform h (Set 43)
;;

let main () =
  let (), i = handle_state 0 comp () in
  Printf.printf "Final state: %d\n" i
;;

let _ = main ()
