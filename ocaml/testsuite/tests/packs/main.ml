(* TEST

files = "main.ml member.ml"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-for-pack Packed"
module = "member.ml"
*** ocamlc.byte
module = ""
flags = "-pack"
program = "packed.cmo"
all_modules = "member.cmo"
**** ocamlc.byte
flags = ""
module = "main.ml"
***** ocamlc.byte
module = ""
program = "${test_build_directory}/main.byte"
all_modules = "packed.cmo main.cmo"
****** run
exit_status = "0"
******* check-program-output

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-for-pack Packed"
module = "member.ml"
*** ocamlopt.byte
module = ""
flags = "-pack"
program = "packed.cmx"
all_modules = "member.cmx"
**** ocamlopt.byte
flags = ""
module = "main.ml"
***** ocamlopt.byte
module = ""
program = "${test_build_directory}/main.exe"
all_modules = "packed.cmx main.cmx"
****** run
exit_status = "0"
******* check-program-output
*)

let say_hello () =
  Packed.Member.say_hello ()

let () =
  say_hello ();

  let ctor = Obj.Extension_constructor.of_val Packed.Member.A in
  print_endline (Obj.Extension_constructor.name ctor)

