(* TEST
 flags = "-bin-annot -bin-annot-occurrences";
 compile_only = "true";
 {
   all_modules = "index_aliases.ml";
   setup-ocamlc.byte-build-env;
 }{
   ocamlc.byte;
   program = "-quiet -index -decls index_aliases.cmt";
   output = "out_objinfo";
   check-ocamlc.byte-output;
   ocamlobjinfo;
   check-program-output;
 }
*)


module A = struct type t end
module B = A

module F (X : sig type t end) = X
module F' = F
module C = F'(A)

module C' = F(B)
module D = C

module G = B
include G
