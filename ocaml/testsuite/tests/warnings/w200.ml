(* TEST

* setup-ocamlopt.opt-build-env
** ocamlopt.opt
compile_only = "true"
*** check-ocamlopt.opt-output

*)

type r = { x : int }

let rec count limit (local_ r) = local_
  let {x} = r in
  function
  | 0 -> 42
  | n when n < 50 ->
     (* No warning: explicitly nontail *)
     count limit r (n - x) [@nontail]
  | n when n < 100 ->
     (* No warning: not in syntactic tail position *)
     let r = count limit r (n - x) in
     r
  | n ->
     (* No warning: not a tail call *)
     let _ = count limit r (n - x) in
     (* Warning *)
     count limit r (n - x)
