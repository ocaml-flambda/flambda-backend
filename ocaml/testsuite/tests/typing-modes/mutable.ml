(* TEST
 flags = "-extension unique";
 expect;
*)

(* Since [mutable] implies [global] modality, which in turns implies [shared]
   and [many] modalities, the effect of mutable in isolation is not testable
   yet. *)

(* CR zqian: add test for mutable when mutable is decoupled from modalities. *)

(* CR mode-crossing: Uncomment this example once portability is added. It would
fail because we don't support mutable(m0) crosses modes yet. *)
(* type r = {mutable a : int}
let r @ portable = {a = 42} *)
