(* TEST
 native;
*)

(* this failed at one point on flambda2 with -O3:
   https://github.com/ocaml-flambda/flambda-backend/pull/1457 *)
let singleton x = [: x :]
