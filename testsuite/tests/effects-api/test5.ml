(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

open Effect

type 'a op = Foo : int -> int op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)
open Eff

let f h =
  perform h (Foo 3) (* 3 + 1 *)
  + perform h (Foo 3) (* 3 + 1 *)

let r =
  let rec handle = function
    | Value v -> v
    | Exception e -> raise e
    | Operation(Foo i, k) ->
        let res = Eff.run (fun h -> handle (continue k (i + 1) [])) in
        (match res with
         | Value v -> v
         | Exception e -> raise e
         | Operation(Foo _, _) -> failwith "NO")
  in
  handle (Eff.run f)

let () = Printf.printf "%d\n" r
