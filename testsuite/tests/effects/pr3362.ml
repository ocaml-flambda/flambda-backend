(* TEST
   skip;
   reason = "CR ocaml 5 effects: re-enable this test";
   runtime5;
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep

type _ t += F : string t

let handle comp =
  Gc.compact ();
  try_with comp ()
    { effc = fun (type a) (e : a t) ->
      Gc.compact ();
      match e with
      | F -> Some (fun (k : (a,_) continuation) ->
        Gc.compact (); continue k "Hello, world!")
      | _ -> None }

let () = handle (fun () ->
  Gc.compact (); print_endline (perform F ^ (" " ^ perform F)))
