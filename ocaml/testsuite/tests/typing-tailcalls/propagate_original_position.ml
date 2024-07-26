(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-dlambda -dflambda -dcmm -dsel -dcfg -c";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* CR less-tco: We should remove this test. It is fragile because it dumps
   cmm/mach output. In CI it is disabled for all configs except for flambda2_dev. *)
external ext : unit -> unit = "ext"

let[@inline never] foo () = ext ()

let tail_on_tail_app () =
  foo () [@tail]

let tail_hint_on_tail_app () =
  foo () [@tail hint]

let tail_hint_on_not_tail_app () =
  foo () [@tail hint];
  foo ()

let tail_hint_in_simplif () =
  let x = foo () [@tail hint] in x

let nontail_on_tail_app () =
  foo () [@nontail]

let nontail_on_not_tail_app () =
  foo () [@nontail];
  foo ()
    
let nontail_hint_in_simplif () =
  let x = foo () [@nontail] in x

let default () =
  foo ()

let default_in_simplif () =
  let x = foo () in x
