(* TEST

readonly_files = "member.ml member2.ml use_member_directly.ml"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-for-pack Pack"
module = "member.ml"
*** script
script = "mkdir subdir/"
**** script
script = "cp use_member_directly.ml member.ml subdir/"
***** cd
cwd = "subdir"
****** ocamlopt.byte
flags = ""
module = "member.ml"
******* ocamlopt.byte
module = "use_member_directly.ml"
******** cd
cwd = ".."
********* ocamlopt.byte
flags = "-for-pack Pack -I subdir"
module = "main.ml"
ocamlopt_byte_exit_status = "2"
********** check-ocamlopt.byte-output
*)

module _ = Member
module _ = Use_member_directly
