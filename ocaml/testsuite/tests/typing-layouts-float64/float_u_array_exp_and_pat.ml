(* TEST
 * native
   flags = "-extension comprehensions -extension layouts_alpha"
 * bytecode
   flags = "-extension comprehensions -extension layouts_alpha"
 * native
   flags = "-extension comprehensions -extension layouts_beta"
 * bytecode
   flags = "-extension comprehensions -extension layouts_beta"
*)

module Float_u_array = Stdlib__Float_u_array
module Float_u = Stdlib__Float_u

let of_int = Float_u.of_int
let (=) = Float_u.equal

let () =
  let d = [| of_int 1; of_int 2 |] in
  match d with
    | [| a; b |] ->
      assert (a = of_int 1);
      assert (b = of_int 2)
    | _ -> assert false


let () =
  let d = [| of_int 1; of_int 2 |] in
  match d with
  | [| a; _ |] when a = of_int 1 -> ()
  | _ -> assert false

let () =
  let a = [||] in
  let b = [| of_int 1 |] in
  let c = Float_u_array.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = of_int 1)

let check_i = Float_u_array.iteri (fun i x -> assert (x = of_int i))

let () =
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
  ()
