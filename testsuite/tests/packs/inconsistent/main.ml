(* TEST
 readonly_files = "member.ml member2.ml use_member_directly.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Pack";
 module = "member.ml";
 ocamlopt.byte;
 script = "mkdir subdir/";
 script;
 script = "cp use_member_directly.ml member.ml subdir/";
 script;
 cwd = "subdir";
 cd;
 flags = "";
 module = "member.ml";
 ocamlopt.byte;
 module = "use_member_directly.ml";
 ocamlopt.byte;
 cwd = "..";
 cd;
 flags = "-for-pack Pack -I subdir";
 module = "main.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

module _ = Member
module _ = Use_member_directly
