(* TEST
 readonly_files = "float_u_array.ml"
 modules = "${readonly_files}"
 * flambda2
 ** native
   flags = "-extension comprehensions -extension layouts_alpha"
 ** bytecode
   flags = "-extension comprehensions -extension layouts_alpha"
 ** native
   flags = "-extension comprehensions -extension layouts_beta"
 ** bytecode
   flags = "-extension comprehensions -extension layouts_beta"
 ** native
   flags = "-extension comprehensions"
 ** bytecode
   flags = "-extension comprehensions"
*)
(* Test for literals, patterns, and comprehension (when that's
   supported) for arrays of unboxed types. In its own file so
   we can test both native and bytecode. *)

module Float_u = Stdlib__Float_u

let (=) = Float_u.equal

(* match statement *)
let () =
  let d = [| #1.; #2. |] in
  match d with
    | [| a; b |] ->
      assert (a = #1.);
      assert (b = #2.)
    | _ -> assert false

(* let statement pattern *)
let () =
  let a = [||] in
  let b = [| #1. |] in
  let c = Float_u_array.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1.)

(* function argument pattern *)
let () =
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1. |] = #1.)
