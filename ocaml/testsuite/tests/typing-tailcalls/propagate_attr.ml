(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-dlambda -dflambda -dcmm -dsel -dcfg -c -O3";
 ocamlopt.opt;
 {
   arch_amd64; runtime4; flambda2; stack-allocation; frame_pointers;
   check-ocamlopt.opt-output;
 }
*)

(* CR less-tco: We should remove this test.

   This test is fragile because it dumps cmm/mach output. This test does
   not need runtime4, frame_pointers, etc. but only enabling the test when
   those features are present makes the test slighly less fragile.
*)
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
