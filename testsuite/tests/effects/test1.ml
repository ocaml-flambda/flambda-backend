(* TEST
  runtime5;
  { bytecode; }
  { native; }
*)

open Stdlib__Effect
open Stdlib__Effect.Deep

type _ t += E : unit t

let () =
  Printf.printf "%d\n%!" @@
    try_with (fun x -> x) 10
    { effc = (fun (type a) (e : a t) ->
        match e with
        | E -> Some (fun k -> 11)
        | e -> None) }
