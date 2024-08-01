(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-no-always-tco -dtypedtree -dlambda -dno-unique-ids -dcmm -c";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let [@inline never] f str = print_endline str

module M = struct
  let f = f
end

(* These calls should not be inferred as tail-calls because their callees
   are not defined in some ancestor let rec. *)
let foo () =
  f "hello";
  f "goodbye"

let bar () =
  f "hello";
  M.f "goodbye"


(* When `foo ()` is inlined into `calls_inlined`, it should not keep its syntactic
   tail position because `inlined` is not in tail position of `calls_inlined`. *)
let [@inline never] baz () =
  f "hello";
  f "goodbye"

let [@inline always] inlined () = baz ()

let calls_inlined () =
  inlined ();
  f "goodbye"

let don't_simplify_calls_inlined () = inlined ()
