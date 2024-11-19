(* TEST
 *)

open Effect

type 'a op = E : unit op

module Eff = Effect.Make(struct
  type 'a t = 'a op
end)

exception Done

let handle_partial f =
  let rec handle = function
    | Eff.Value v -> v
    | Eff.Exception e -> raise e
    | Eff.Operation(E, _) -> assert false
  in
  handle (Eff.run f)

let f h1 =
 (); fun h2 -> Eff.perform h2 E

let () =
  let rec handle = function
    | Eff.Value _ -> assert false
    | Eff.Exception Done -> print_string "ok\n"
    | Eff.Exception e -> raise e
    | Eff.Operation(E, k) -> handle (discontinue k Done [])
  in
  handle (Eff.run (fun h -> handle_partial f h))
