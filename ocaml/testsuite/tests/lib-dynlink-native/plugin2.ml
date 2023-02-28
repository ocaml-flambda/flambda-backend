(*external ex: int -> int = "caml_ex"*)

let foo a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  Printexc.print_raw_backtrace stdout (Printexc.get_callstack 4);
  fun a10 a11 a12 a13 a14 a15 a16 a17 a18 -> ()

(* Ensure that the frametable is set up correctly so that a [caml_curry_18]
   frame can be traversed *)
let[@inline never] test_frametable () =
  (Sys.opaque_identity foo) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18;
  (* force non-tail call *)
  Sys.opaque_identity ()

let () =
  Api.reg_mod "Plugin2";
  Api.add_cb (fun () -> print_endline "Callback from plugin2");
(*  let i = ex 3 in*)
  List.iter (fun i -> Printf.printf "%i\n" i) Plugin.facts;
  test_frametable ();
  Printf.printf "XXX\n"
