(* TEST
<<<<<<< HEAD
 readonly_files = "foo.mli bar.mli baz.ml";
 setup-ocamlc.byte-build-env;
 module = "foo.mli";
 ocamlc.byte;
 module = "bar.mli";
 ocamlc.byte;
 script = "rm foo.cmi";
 script;
 flags = "-c -i";
 module = "baz.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
||||||| 121bedcfd2
readonly_files = "foo.mli bar.mli baz.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "foo.mli"
*** ocamlc.byte
module = "bar.mli"
**** script
script = "rm foo.cmi"
***** ocamlc.byte
flags = "-c -i"
module = "baz.ml"
ocamlc_byte_exit_status = "0"
****** check-ocamlc.byte-output
=======
 readonly_files = "foo.mli bar.mli baz.ml";
 setup-ocamlc.byte-build-env;
 module = "foo.mli";
 ocamlc.byte;
 module = "bar.mli";
 ocamlc.byte;
 script = "rm foo.cmi";
 script;
 flags = "-c -i";
 module = "baz.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
>>>>>>> 5.2.0
*)
