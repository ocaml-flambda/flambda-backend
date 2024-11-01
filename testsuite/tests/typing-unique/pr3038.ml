(* TEST
 readonly_files = "pr3038.ml";
setup-ocamlc.byte-build-env;
module = "pr3038.ml";
ocamlc.byte;
check-ocamlc.byte-output;
*)

type r = {a : string}
type t =
    | Foo of r
    | Bar of r

let f _ = ()

let foo t =
    match t with
    | Foo x | Bar x ->
    let _ = x.a in f x
