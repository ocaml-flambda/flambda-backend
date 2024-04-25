(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Since [mutable] implies [global] modality, which in turns implies [shared]
   and [many] modalities, the effect of mutable in isolation is not testable
   yet. *)

(* CR zqian: add test for mutable when mutable is decoupled from modalities. *)

(* CR mode-crossing: Uncomment the following examples once portability is added. *)

(* The [m0] in mutable should cross modes upon construction. *)
(*
type r =
  { f : string -> string;
    mutable a : int }
let r @ portable =
  { f = fun x -> x;
    a = 42 } *)

(* The [m0] in mutable corresponds to the field type wrapped in modality; as a
   result, it enjoys mode crossing enabled by the modality. *)
(*
type r =
  { f : string -> string;
    mutable g : string -> string @@ portable }
let r @ portable =
  { f = fun x -> x;
    g = fun x -> x } *)
