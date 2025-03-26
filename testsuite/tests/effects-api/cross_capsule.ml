(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

[@@@alert "-unsafe_parallelism"]

open Stdlib_alpha

type 'a op =
  | Set : int -> unit op
  | Get : int op

module Eff = Effect.Make(struct
  type 'a t = 'a op
end)

let store = ref 0

let rec handle = function
  | Eff.Contended.Result.Value v -> v
  | Exception e -> raise e
  | Operation (Set n, k) ->
    store := n;
    handle (Effect.continue k {portable = ()} [])
  | Operation (Get, k) ->
    let n = !store in
    handle (Effect.continue k {portable = n} [])
;;

type 'a aliased = Aliased of 'a @@ aliased [@@unboxed]

let () =
  handle (Eff.Contended.run (fun h ->
    let (P k) = Capsule.create () in
    let Aliased x, k =
      Capsule.Key.access k (fun a ->
      Aliased (Capsule.Data.wrap a (ref 4)))
  in
  let (), _k = Capsule.Key.with_password k (fun p ->
    Capsule.Data.iter p (fun a -> Eff.Contended.perform h (Set !a)) x [@nontail]);
  in
  Printf.printf "Get: %d\n" (Eff.Contended.perform h Get)))
;;
