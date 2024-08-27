(* TEST
 reason = "CR ocaml 5 effects: re-enable this test";
 skip;
*)

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  try_with perform E
  { effc = fun (type a) (e : a t) ->
      Some (fun k ->
          match Marshal.to_string k [] with
          | _ -> assert false
          | exception (Invalid_argument _) -> print_endline "ok"
          ) }
