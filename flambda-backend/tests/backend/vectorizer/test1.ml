
let add_pairs (a0, a1 : int64 * int64) (b0, b1 : int64 * int64) =
  (Int64.add a0 b0, Int64.add a1 b1)
;;

let () =
  let sum0, sum1 = add_pairs (0L, 1L) (2L, 3L) in
  Printf.printf "%Lx %Lx\n" sum0 sum1
;;
