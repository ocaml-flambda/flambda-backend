(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

(* Tests RESUMETERM with extra_args != 0 in bytecode,
   by calling a handler with a tail-continue that returns a function *)

open Effect

type 'a op = E : int op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let handle comp =
  let rec handle = function
    | Value v -> v
    | Operation(E, k) -> handle (continue k 10 [])
    | Exception e -> raise e
  in
  handle (Eff.run comp)

let () =
  handle (fun h ->
    Printf.printf "%d\n" (perform h E);
    Printf.printf "%d\n") 42
