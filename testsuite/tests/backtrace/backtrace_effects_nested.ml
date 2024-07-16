<<<<<<< HEAD
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)









||||||| 121bedcfd2
(* TEST
=======
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)






>>>>>>> 5.2.0


*)

open Effect
open Effect.Deep

type _ t += E : unit t
          | Inc : unit t

let blorp () =
  perform Inc;
  perform E;
  42

let baz () =
    try_with blorp ()
    { effc = fun (type a) (e : a t) ->
        match e with
        | Inc -> Some (fun (k : (a, _) continuation) ->
            1 + continue k ())
        | _ -> None }

let f () =
  match_with baz ()
  { retc = (fun x -> Printf.printf "%d\n" x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
          match e with
          | E -> Some (fun (k : (a, _) continuation) ->
              Deep.get_callstack k 100 |>
              Printexc.raw_backtrace_to_string |>
              print_string;
              continue k ())
          | _ -> None }

let () = f ()
<<<<<<< HEAD

(* TEST
 flags = "-g";
 reason = "CR ocaml 5 effects: re-enable this test";
 skip;
 {
   bytecode;
 }{
   no-flambda;
   native;
 }{
   reference = "${test_source_directory}/backtrace_effects_nested.flambda.reference";
   flambda;
   native;
 }
*)
||||||| 121bedcfd2
=======

(* TEST
 flags = "-g";
 {
   bytecode;
 }{
   no-flambda;
   native;
 }{
   reference = "${test_source_directory}/backtrace_effects_nested.flambda.reference";
   flambda;
   native;
 }
*)
>>>>>>> 5.2.0
