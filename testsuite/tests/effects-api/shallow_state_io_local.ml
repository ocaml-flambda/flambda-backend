(* TEST
 include stdlib_alpha;
 skip;
 reason = "Local continuations are not supported yet";
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

let handle_state init f =
  let rec handle (state : int) = function
    | Eff1.Value result -> result, state
    | Eff1.Exception e -> raise e
    | Eff1.Operation(Get, k) ->
        handle state (continue_local k state [])
    | Eff1.Operation(Set new_state, k) ->
        handle new_state (continue_local k () [])
  in
  handle init (Eff1.run_local f) [@nontail]

let handle_print f =
  let rec handle = function
    | Eff2.Value x -> x
    | Eff2.Exception e -> raise e
    | Eff2.Operation(Print s, k) ->
        print_string s;
        handle (continue_local k () [])
  in
  handle (Eff2.run_local f) [@nontail]
;;

let main () =
  let (), i =
    handle_print (fun h1 ->
      handle_state 0 (fun h2 ->
        Eff2.perform h1
          (Print (Printf.sprintf "Initial state: %d\n" (Eff1.perform h2 Get)));
        Eff1.perform h2 (Set 42);
        Eff2.perform h1
          (Print (Printf.sprintf "Updated state: %d\n" (Eff1.perform h2 Get)));
        Eff1.perform h2 (Set 43)))
  in
  Printf.printf "Final state: %d\n" i

let _ = main ()
