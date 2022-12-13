(* TEST
readonly_files = "regression_class_dep.ml"
* setup-ocamlc.opt-build-env
** ocamlc.opt
module = "regression_class_dep.ml"
*** ocamlc.opt
module = ""
flags = "-c"
*)

(* https://github.com/ocaml-flambda/ocaml-jst/issues/65 *)

module Dep = Regression_class_dep
class c fname =
  object
    inherit Dep.c
    inherit Dep.cv
  end
