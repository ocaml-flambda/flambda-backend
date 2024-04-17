(* TEST
 flags = "-bin-annot -bin-annot-occurrences";
 compile_only = "true";
 all_modules = "index_functor.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 program = "-quiet -index -decls index_functor.cmt";
 output = "out_objinfo";
 check-ocamlc.byte-output;
 ocamlobjinfo;
 check-program-output;
*)


module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
include O
include N
