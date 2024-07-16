(* TEST
 flags = "-dtypedtree -dlambda -dno-unique-ids";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

let [@inline never] f str = print_endline str

module M = struct
  let f = f
end

let should_be_tail () =
  f "hello";
  f "goodbye"

let shouldn't_be_tail () =
  f "hello";
  M.f "goodbye"

