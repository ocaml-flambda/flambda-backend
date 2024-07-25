(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-dlambda -dflambda -dcmm -dsel -dcfg -c";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* CR less-tco: We should remove this test. It is fragile because it dumps
   cmm/mach output. In CI it is disabled for all configs except for flambda2_dev. *)
external ext : unit -> unit = "ext"

let foo () = ext ()

let tail () =
  foo () [@tail]

let tail_hint () =
  foo () [@tail hint]

let nontail () =
  foo () [@nontail]

let default () =
  foo ()
