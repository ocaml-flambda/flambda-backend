(* TEST
   flags += "-extension-universe alpha";
   expect;
   reference = "${test_source_directory}/overwriting_lift_constants_bug.reference";
*)

(* CR uniqueness: To run this test replace 'expect' above by 'native'
   and delete the expect block. *)

type point = { dim : int; x : float; y : float; z : float }

let constant_lift b =
  let unique_ p = { dim = 3; x = 1.0; y = 2.0; z = 3.0 } in
  if b then p else overwrite_ p with { x = 2.0 }

type fpoint = { x : float; y : float; z : float }

let fconstant_lift b =
  let unique_ p = { x = 1.0; y = 2.0; z = 3.0 } in
  if b then p else overwrite_ p with { x = 2.0 }

type mpoint = { dim : int option; x : float#; y : float#; z : float# }

let mconstant_lift b =
  let unique_ p = { dim = Some 3; x = #1.0; y = #2.0; z = #3.0 } in
  if b then p else overwrite_ p with { x = #2.0 }

type ufpoint = { x : float#; y : float#; z : float# }

let ufconstant_lift b =
  let unique_ p = { x = #1.0; y = #2.0; z = #3.0 } in
  if b then p else overwrite_ p with { x = #2.0 }

type utpoint = { xy : #(float * float); z : float }

let ufconstant_lift b =
  let unique_ p = { xy = #(1.0, 2.0); z = 3.0 } in
  if b then p else overwrite_ p with { xy = #(2.0, 2.0) }

let () =
  let x = (constant_lift true).x in
  let y = (constant_lift false).x in
  let z = (constant_lift true).x in
  Printf.printf "%f %f %f\n" x y z;
  let x = (fconstant_lift true).x in
  let y = (fconstant_lift false).x in
  let z = (fconstant_lift true).x in
  Printf.printf "%f %f %f\n" x y z;
  let x = Float_u.to_float (mconstant_lift true).x in
  let y = Float_u.to_float (mconstant_lift false).x in
  let z = Float_u.to_float (mconstant_lift true).x in
  Printf.printf "%f %f %f\n" x y z;
  let x = Float_u.to_float (ufconstant_lift true).x in
  let y = Float_u.to_float (ufconstant_lift false).x in
  let z = Float_u.to_float (ufconstant_lift true).x in
  Printf.printf "%f %f %f\n" x y z;
  let x = Float_u.to_float (fst (ufconstant_lift true).xy) in
  let y = Float_u.to_float (fst (ufconstant_lift false).xy) in
  let z = Float_u.to_float (fst (ufconstant_lift true).xy) in
  Printf.printf "%f %f %f\n" x y z

[%%expect{|
type point = { dim : int; x : float; y : float; z : float; }
val constant_lift : bool -> point @@ global many = <fun>
type fpoint = { x : float; y : float; z : float; }
val fconstant_lift : bool -> fpoint @@ global many = <fun>
type mpoint = { dim : int option; x : float#; y : float#; z : float#; }
val mconstant_lift : bool -> mpoint @@ global many = <fun>
type ufpoint = { x : float#; y : float#; z : float#; }
val ufconstant_lift : bool -> ufpoint @@ global many = <fun>
Line 25, characters 0-51:
25 | type utpoint = { xy : #(float * float); z : float }
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "#(float * float)" has layout "value & value".
       Records may not yet contain types of this layout.
|}]
