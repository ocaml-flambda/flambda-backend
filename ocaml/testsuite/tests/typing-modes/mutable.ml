(* TEST
 flags = "-extension unique";
 expect;
*)

(* Since [mutable] implies [global] modality, which in turns implies [shared]
   and [many] modalities, the effect of mutable in isolation is not testable
   yet. *)

(* CR zqian: add test for mutable when mutable is decoupled from modalities. *)

type r =
  { f : string -> string;
    mutable a : int }
let r @ portable =
  { f = (fun x -> x);
    a = 42 }
(* CR mode-crossing: The [m0] in mutable should cross modes upon construction. *)
[%%expect{|
type r = { f : string -> string; mutable a : int; }
Lines 5-6, characters 2-12:
5 | ..{ f = (fun x -> x);
6 |     a = 42 }
Error: This value is nonportable but expected to be portable.
|}]

type r =
  { f : string -> string;
    mutable g : string -> string @@ portable }
let r @ portable =
  { f = (fun x -> x);
    g = fun x -> x }
(* CR mode-crossing: The [m0] in mutable corresponds to the field type wrapped
   in modality; as a result, it enjoys mode crossing enabled by the modality. *)
[%%expect{|
type r = { f : string -> string; mutable g : string -> string; }
Lines 5-6, characters 2-20:
5 | ..{ f = (fun x -> x);
6 |     g = fun x -> x }
Error: This value is nonportable but expected to be portable.
|}]
