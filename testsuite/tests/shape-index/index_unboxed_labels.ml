(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
readonly_files = "index_unboxed_labels.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_unboxed_labels.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_unboxed_labels.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

type t = { a: int; b: string }
type tu = #{ a : int }

let x = { a = 42; b = "" }
let _y = x.a

let f = function
  | { a = 42; b } -> ()
  | _ -> ()

let x = #{ a = 42 }
let _y = x.#a

let f = function
  | #{ a = 42 } -> ()
  | _ -> ()

type tb = { b : string }

let x = #{ b = "" }
let _y = x.#b

let f = function
  | #{ b = "" } -> ()
  | _ -> ()
