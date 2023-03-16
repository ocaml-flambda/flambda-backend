let rec length x =
  match x with
  | [] -> 0
  | a :: l -> 1 + length l

let foo () =
  let y = 3
  in
  let rec g x= f x + y
  and f x = g x
