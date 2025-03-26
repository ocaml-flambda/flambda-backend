(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

[@@@alert "-unsafe_parallelism"]

open Stdlib_alpha

type 'a op = Jump : int op

module Eff = Effect.Make(struct
  type 'a t = 'a op
end)

let rec handle n = function
  | Eff.Portable.Result.Value v -> v
  | Exception e -> raise e
  | Operation (Jump, k) ->
    let f () = handle (n + 1) (Effect.continue k (n + 1) []) in
    Domain.join (Domain.Safe.spawn f)
;;

let () =
  handle 0 (Eff.Portable.run (fun h ->
    print_endline "Starting in domain 0";
    let jump () =
      let n = Eff.perform h Jump in
      Printf.printf "Jump! Now in domain %d\n%!" n
    in
    for i = 1 to 10 do
      jump ()
    done))
;;
