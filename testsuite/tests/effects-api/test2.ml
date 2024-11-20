(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

open Effect

open Printf

type 'a op = E : int -> int op

module Eff = Effect.Make(struct
  type 'a t = 'a op
end)

let f h =
  printf "perform effect (E 0)\n%!";
  let v = Eff.perform h (E 0) in
  printf "perform returns %d\n%!" v;
  v + 1

let rec handle = function
  | Eff.Value v -> printf "done %d\n%!" v; v + 1
  | Eff.Exception e -> raise e
  | Eff.Operation(E v, k) ->
      printf "caught effect (E %d). continuing..\n%!" v;
      let v = handle (continue k (v + 1) []) in
      printf "continue returns %d\n%!" v;
      v + 1

let v =
  handle (Eff.run f)

let () =
  printf "result=%d\n%!" v
