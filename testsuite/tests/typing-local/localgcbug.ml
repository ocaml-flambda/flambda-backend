(* TEST
 native;
*)

type n = Z | S of n

let rec gen_locals (local_ n) depth _ = exclave_
  if depth = 0
  then
    S n
  else
    let s = S n in
    let m = gen_locals s (depth - 1) (ref 42) in
    let _ = gen_locals m (depth - 1) (ref 42) in
    S n

let () =
  match gen_locals Z 21 (ref 42) with
  | S Z -> print_endline "ok"
  | _ -> assert false
