(* TEST
   * expect
   flags = "-extension unique"
*)

(* Our current approach is to treat "mutable implying global" only in typecore,
   this simplifies printing, but also means the following duplication cannot be
   detected. But on the other hand, what would be the error message? A specially
   tailored message pointing to `mutable` and says "mutable implies global" -
   that's too much effort for something that will be gone very soon.

   Also note that the legacy syntax [mutable global_ x : string] is not parsable
   anyway. *)
type r = {mutable x : string @@ global}
[%%expect{|
type r = { mutable global_ x : string; }
|}]

(* Since [mutable] implies [global] modality, which in turns implies [shared]
   and [many] modalities, the effect of mutable in isolation is not testable
   yet. *)

(* CR zqian: add test for mutable when mutable is decoupled from modalities. *)
