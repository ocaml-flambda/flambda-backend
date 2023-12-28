(* TEST
 * native
   flags = "-extension comprehensions -extension layouts_alpha"
 * bytecode
   flags = "-extension comprehensions -extension layouts_alpha"
 * native
   flags = "-extension comprehensions -extension layouts_beta"
 * bytecode
   flags = "-extension comprehensions -extension layouts_beta"
 * native
   flags = "-extension comprehensions -extension layouts"
 * bytecode
   flags = "-extension comprehensions -extension layouts"
*)

module Float_u_array = Stdlib__Float_u_array
module Float_u = Stdlib__Float_u

let of_int = Float_u.of_int
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
  let[@inline never][@warning "-8"] f [| b |] = b in
  assert (f [| #1. |] = #1.)


(* array comprehension *)
(* let () =
  let check_i = Float_u_array.iteri (fun i x -> assert (x = of_int i)) in
  (* fixed size *)
  let a = [|Float_u.of_int e for e = 0 to 9|] in
  check_i a;
  (* call to length a *)
  let b = [| x for x in a |] in
  check_i b;
  (* dynamic size *)
  let c = [| Float_u.(add (mul (of_int 10) x) y) for x in a for y in b |] in
  check_i c;
  (* mix types *)
  let d = [| Float_u.(add (mul (of_int 10) x) (of_int y)) for x in a for y = 0 to 9 |] in
  check_i d;
  let e = [| Float_u.(add (of_int (10 * x)) y) for x = 0 to 9 for y in a |] in
  check_i e;
  () *)
