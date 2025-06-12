(* TEST
 native;
*)

(* this failed at one point on flambda2 with -O3:
   https://github.com/oxcaml/oxcaml/pull/1457 *)
let singleton x = [: x :]
