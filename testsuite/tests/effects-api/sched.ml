(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

module Effect = Stdlib_alpha.Effect

type ('a, 'e) op =
  | Yield : (unit, 'e) op
  | Fork : (local_ 'e Effect.Handler.t -> string) -> (unit, 'e) op
  | Ping : (unit, 'e) op

module Eff = Effect.Make_rec (struct
    type ('a, 'e) t = ('a, 'e) op
  end)

open Eff

exception E
exception Pong

let say = print_string

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then
      `Finished
    else
      handle (Effect.continue (Queue.pop run_q) () [])
  and spawn f =
    handle (Eff.run f)
  and handle = function
    | Value "ok" ->
        say ".";
        dequeue ()
    | Value s ->
        failwith ("Unexpected result: " ^ s)
    | Exception E ->
        say "!";
        dequeue ()
    | Exception e ->
        raise e
    | Operation(Yield, k) ->
        say ",";
        enqueue k;
        dequeue ()
    | Operation(Fork f, k) ->
        say "+";
        enqueue k;
        spawn f
    | Operation(Ping, k) ->
        say "[";
        handle (Effect.discontinue k Pong [])
  in
  spawn main
;;

let test h =
  say "A";
  perform h
    (Fork
       (fun h ->
          perform h Yield;
          say "C";
          perform h Yield;
          let handle = function
            | Value v -> v
            | Exception Pong -> say "]"
            | Exception e -> raise e
            | Operation(_, k) -> failwith "what?"
          in
          let res =
            Eff.run_with [h] (fun [_; h2] ->
                perform h2 Ping;
                failwith "no pong?")
          in
          handle res;
          raise E));
  perform h
    (Fork
       (fun h ->
          say "B";
          "ok"));
  say "D";
  perform h Yield;
  say "E";
  "ok"
;;

let () =
  let `Finished = run test in
  say "\n"
;;
