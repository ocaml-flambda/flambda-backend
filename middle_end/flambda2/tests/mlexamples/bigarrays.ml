
open Bigarray

let () =
  let a = Array2.create int c_layout 3 3 in
  (*
  for i = 0 to 2 do
    for j = 0 to 2 do
      a.{i, j} <- i * 3 + j
    done;
     done;
    *)
  let i = a.{1, 1} in
  print_int i

