(* TEST
 include stdlib_alpha;
 runtime5;
 { bytecode; }
 { native; }
*)

module Effect = Stdlib_alpha.Effect

open Effect

type 'a op = E : unit op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)

let r = ref (None : (unit, int, unit) Eff.Continuation.t option)
let () =
  let rec handle = function
    | Eff.Value n -> assert (n = 42)
    | Eff.Exception e -> raise e
    | Eff.Operation(E, k) ->
        handle (continue (Obj.magic_unique k) () []);
        r := Some (Obj.magic_unique k);
        Gc.full_major ();
        print_string "ok\n"
  in
  handle (Eff.run (fun h -> Eff.perform h E; 42))
