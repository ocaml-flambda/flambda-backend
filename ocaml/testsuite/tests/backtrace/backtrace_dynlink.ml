(* TEST

include dynlink

readonly_files = "backtrace_dynlink_plugin.ml"

libraries = ""

* shared-libraries
** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "backtrace_dynlink.ml"
flags = "-g"
**** ocamlopt.byte
program = "backtrace_dynlink_plugin.cmxs"
flags = "-shared -g"
all_modules = "backtrace_dynlink_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/main.exe"
libraries = "dynlink"
all_modules = "backtrace_dynlink.cmx"
***** run
ocamlrunparam += ",b=1"
****** check-program-output
*)

(* test for backtrace and stack unwinding with dynlink. *)
(* https://github.com/ocaml-multicore/ocaml-multicore/issues/440 *)
(* https://github.com/ocaml-multicore/ocaml-multicore/pull/499 *)


[@@@ocaml.warning "-52"]

let () =
  Dynlink.allow_unsafe_modules true;
  try
    (Dynlink.loadfile [@inlined never]) "backtrace_dynlink_plugin.cmxs"
  with
  | Dynlink.Error ((Library's_module_initializers_failed (
      Failure "SUCCESS")) as err) ->
    print_endline (Dynlink.error_message err);
    let bt = Printexc.get_backtrace () in
    let bt_list = String.split_on_char '\n' bt in
    if List.length bt_list > 5 then print_endline "Backtrace sufficiently long"
    else (
      print_endline "Failure: Backtrace too short:";
      print_string bt
    )
  | exn ->
    Printexc.to_string exn |> print_endline;
    print_endline "ERROR"
