(* TEST
*)

type 'a op = E : unit op

module Eff = Effect.Make (struct
    type 'a t = 'a op
  end)

let () =
  let rec handle = function
    | Eff.Value x -> x
    | Eff.Exception e -> raise e
    | Eff.Operation(E, k) ->
         begin
           match
             (* We have to make sure that neither the match nor the call to caml_equal are
                eliminated, so we call print_string and we print the result of
                caml_equal. *)
             print_string "";
             k = k
           with
           | b ->
               Printf.printf "%b" b;
               assert false
           | exception Invalid_argument _ -> print_endline "ok"
         end;
         begin
           match Hashtbl.hash k with
           | _ -> print_endline "ok"
         end
  in
  handle (Eff.run (fun h -> Eff.perform h E))
;;
