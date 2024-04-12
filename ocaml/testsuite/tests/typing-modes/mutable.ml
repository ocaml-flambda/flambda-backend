(* TEST
   * expect
   flags = "-extension unique"
*)

(* Since [mutable] implies [global] modality, which in turns implies [shared]
   and [many] modalities, the effect of mutable in isolation is not testable
   yet. *)

(* CR zqian: add test for mutable when mutable is decoupled from modalities. *)
