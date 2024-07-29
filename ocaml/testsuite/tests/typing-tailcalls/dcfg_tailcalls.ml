(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-cfg-analyze-tailcalls -dcfg-tailcalls -c";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let rec foo n =
  if n > 0 then foo (n - 2) else ()

let rec bar n =
  if n > 0 then baz (n - 2) else ()
and baz n = bar (n + 1)

let rec collatz n k =
  let[@inline never] next n k =
    let n = n - 1
    and k = if k mod 2 == 0 then (k / 2) else ((3 * k) + 1) in
    collatz n k
  in
  ignore next;
  if n > 0 then next n k else k
