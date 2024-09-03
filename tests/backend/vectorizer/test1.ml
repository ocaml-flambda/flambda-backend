type t2 = {  d0 : int ;  d1: int }

let add_pairs_immutable_record (a : t2) (b: t2) : t2 =
  { d0 = a.d0 + b.d0;
    d1 = a.d1 + b.d1 }


type t4 = {  d0 : int ; d1: int ; d2 : int ; d3: int }

let add_fours_immutable_record (a : t4) (b: t4) : t4 =
  { d0 = a.d0 + b.d0;
    d1 = a.d1 + b.d1;
    d2 = a.d2 + b.d2;
    d3 = a.d3 + b.d3 }

let add_int_tuples (a0, a1 : int * int) (b0, b1 : int * int) =
  a0 + b0, a1 + b1

let () =
  let sum_t2 = add_pairs_immutable_record { d0 = 8 ; d1 = 96 } { d0 = 80 ; d1 = 14 } in
  Printf.printf "sum_t2: { d0 = %d ; d1 = %d }\n" sum_t2.d0 sum_t2.d1;
  let sum_t4 = add_fours_immutable_record { d0 = 9 ; d1 = 12 ; d2 = 16 ; d3 = 98 } { d0 = 25 ; d1 = 85 ; d2 = 72 ; d3 = 48 } in
  Printf.printf "sum_t4: { d0 = %d ; d1 = %d ; d2 = %d ; d3 = %d }\n" sum_t4.d0 sum_t4.d1 sum_t4.d2 sum_t4.d3;
  let sum0, sum1 = add_int_tuples (48, 31) (4, 71) in
  Printf.printf "sum0: %d sum1: %d\n" sum0 sum1
;;
