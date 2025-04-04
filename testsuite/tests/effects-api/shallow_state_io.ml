(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

open Effect

type 'a op1 =
  | Get : int op1
  | Set : int -> unit op1

module Eff1 = Effect.Make (struct
  type 'a t = 'a op1
end)

type 'a op2 =
  | Print : string -> unit op2

module Eff2 = Effect.Make (struct
  type 'a t = 'a op2
end)

let handle_state ((*local_*) hs) init f =
  let rec handle (state : int) = function
    | Eff1.Value result -> result, state
    | Eff1.Exception e -> raise e
    | Eff1.Operation(Get, k) ->
        handle state (continue k state hs)
    | Eff1.Operation(Set new_state, k) ->
        handle new_state (continue k () hs)
  in
  handle init (Eff1.run_with hs f) [@nontail]

let handle_print ((*local_*) hs) f =
  let rec handle = function
    | Eff2.Value x -> x
    | Eff2.Exception e -> raise e
    | Eff2.Operation(Print s, k) ->
        print_string s;
        handle (continue k () hs)
  in
  handle (Eff2.run_with hs f) [@nontail]
;;

let main () =
  let (), i =
    handle_print [] (fun hs ->
      handle_state hs 0 (fun [h1; h2] ->
        Eff2.perform h2
          (Print (Printf.sprintf "Initial state: %d\n" (Eff1.perform h1 Get)));
        Eff1.perform h1 (Set 42);
        Eff2.perform h2
          (Print (Printf.sprintf "Updated state: %d\n" (Eff1.perform h1 Get)));
        Eff1.perform h1 (Set 43)))
  in
  Printf.printf "Final state: %d\n" i

let _ = main ()
