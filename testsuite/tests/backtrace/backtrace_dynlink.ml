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
****** no-flambda
******* check-program-output
****** flambda
reference = "${test_source_directory}/backtrace_dynlink.flambda.reference"
******* check-program-output
*)

(* test for backtrace and stack unwinding with dynlink. *)
(* https://github.com/ocaml-multicore/ocaml-multicore/issues/440 *)
(* https://github.com/ocaml-multicore/ocaml-multicore/pull/499 *)

(* Postprocess backtrace to ignore differences between dune and make
   builds (in the former, Dynlink.Native is Dynlink_internal_native.Native) *)
let begins_with ?(from = 0) str ~prefix =
  (* From utils/misc.ml *)
  let rec helper idx =
    if idx < 0 then true
    else
      String.get str (from + idx) = String.get prefix idx && helper (idx-1)
  in
  let n = String.length str in
  let m = String.length prefix in
  if n >= from + m then helper (m-1) else false

let process_backtrace bt =
  let bt = String.split_on_char '\n' bt in
  let bt =
    List.map (fun line ->
        let prefix = "Called from Dynlink.Native" in
        if begins_with line ~prefix
        then
          "Called from Dynlink_internal_native.Native" ^
            (String.sub line (String.length prefix)
              (String.length line - String.length prefix))
        else
          let prefix = "Re-raised at Dynlink.Native" in
          if begins_with line ~prefix
          then
            "Re-raised at Dynlink_internal_native.Native" ^
              (String.sub line (String.length prefix)
                (String.length line - String.length prefix))
          else
            line
      )
      bt
  in
  String.concat "\n" bt

let ()  =
  Dynlink.allow_unsafe_modules true;
  try
    (Dynlink.loadfile [@inlined never]) "backtrace_dynlink_plugin.cmxs"
  with
  | Dynlink.Error err ->
     print_endline @@ Dynlink.error_message err;
     print_string (process_backtrace (Printexc.get_backtrace ()))
  | exn ->
     Printexc.to_string exn |> print_endline;
     print_endline "ERROR"
