(* TEST
 flags = "-bin-annot -bin-annot-occurrences";
 compile_only = "true";
 readonly_files = "index_constrs_records.ml";
 all_modules = "index_constrs_records.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 program = "-quiet -index -decls index_constrs_records.cmt";
 output = "out_objinfo";
 check-ocamlc.byte-output;
 ocamlobjinfo;
 check-program-output;
*)
type l = { lbl : int }
module M : sig
  type t = A of { l_c : int }
end = struct
  type t = A of { l_c : int }
  let _ = A { l_c = 42 }
end

let _ = M.A { l_c = 42 }

open M

let _ = A { l_c = 42 }
let f (A { l_c }) ({ lbl } as l) = l_c + lbl + l.lbl

type u = ..
type u += Ext of { l_ext : int }

let f (x : u) = match x with
  | Ext { l_ext } -> l_ext
  | _ -> assert false

exception Exn of {l_exn : int }

let e = Exn { l_exn = 2}
let _ = match e with
  | Exn { l_exn } -> l_exn
  | _ -> assert false
