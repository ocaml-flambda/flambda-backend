(* TEST
   modules = "backtrace_c_exn_.c"
   flags = "-g"
   ocamlrunparam += ",b=1"
   * bytecode
     reference = "${test_source_directory}/backtrace_c_exn.byte.reference"
   * native
     reference = "${test_source_directory}/backtrace_c_exn.opt.reference"
*)

(* https://github.com/ocaml-multicore/ocaml-multicore/issues/498 *)
external stubbed_raise : unit -> unit = "caml_498_raise"

let raise_exn () = (failwith [@inlined never]) "exn" [@@inline never]

let () = Callback.register "test_raise_exn" raise_exn

let () =
  try
    stubbed_raise ()
  with
  | exn ->
    Printexc.to_string exn |> print_endline;
    Printexc.print_backtrace stdout
