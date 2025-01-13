(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

module Effect = Stdlib_alpha.Effect

type 'a op = E : unit op
exception X

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let () =
  let handle = function
    | Value v -> v
    | Exception X -> 10
    | Exception e -> raise e
    | Operation(E, k) -> 11
  in
  Printf.printf "%d\n%!" (handle (Eff.run (fun h ->
    Printf.printf "in handler. raising X\n%!";
    raise X)))
