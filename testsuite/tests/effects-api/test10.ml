(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

module Effect = Stdlib_alpha.Effect

open Effect

module Ops1 = struct
  type 'a t = Peek : int t
end

module Ops2 = struct
  type 'a t = Poke : unit t
end

module Eff1 = Effect.Make (Ops1)
module Eff2 = Effect.Make (Ops2)

let rec a h i = Eff1.perform h Peek + Random.int i
let rec b h i = a h i + Random.int i
let rec c h i = b h i + Random.int i

let rec d ((*local_*) h) i =
  let rec handle = function
    | Eff2.Value result -> result
    | Eff2.Exception exn -> raise exn
    | Eff2.Operation (Poke, k) -> handle (continue k () [h])
  in
  Random.int i + handle (Eff2.run_with [h] (fun [_; h] -> c h i))
;;

let rec e i =
  let rec handle = function
    | Eff1.Value result -> result
    | Eff1.Exception exn -> raise exn
    | Eff1.Operation (Peek, k) ->
        ignore (Continuation.get_callstack k 100);
        handle (continue k 42 [])
  in
  Random.int i + handle (Eff1.run (fun h -> d h i))
;;

let _ =
  ignore (e 1);
  print_string "ok\n"
;;
