(* TEST
 *)

type 'a op = E : unit op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let rec even n =
  if n = 0 then true
  else handle (Eff.run (fun _ -> odd (n-1)))
and odd n =
  if n = 0 then false
  else even (n-1)
and handle = function
  | Value v -> v
  | Operation(E, _) -> assert false
  | Exception e -> raise e

let _ =
  let n = 100_000 in
  Printf.printf "even %d is %B\n%!" n (even n)
