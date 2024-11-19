(* TEST
 *)

type 'a op = E : unit op

module Eff = Effect.Make(struct
  type 'a t = 'a op
end)

let () =
  let rec handle = function
    | Eff.Value x -> x
    | Eff.Exception e -> raise e
    | Eff.Operation(E, k) -> 11
  in
  Printf.printf "%d\n%!" (handle (Eff.run (fun _ -> 10)))
