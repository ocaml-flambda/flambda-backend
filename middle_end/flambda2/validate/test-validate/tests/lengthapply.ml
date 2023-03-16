let rec length x =
  match x with
  | [] -> 0
  | a :: l -> 1 + length l

let _ = length [1;2;3]
