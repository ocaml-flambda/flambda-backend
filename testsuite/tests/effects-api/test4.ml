(* TEST
 *)

open Effect

type 'a op = Foo : int -> int op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let r =
  let rec handle = function
    | Value v -> v
    | Operation (Foo i, k) ->
        let res = Eff.run (fun _ -> handle (continue k (i + 1) [])) in
        (match res with
         | Value v -> v
         | Exception e -> raise e
         | Operation (Foo _, _) -> failwith "NO")
    | Exception e -> raise e
  in
  handle (Eff.run (fun h -> perform h (Foo 3)))

let () = Printf.printf "%d\n" r
