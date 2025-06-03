(* TEST
 reference = "${test_source_directory}/let_mutable.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension let_mutable";
   native;
 }{
   flags = "-extension let_mutable";
   bytecode;
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   native;
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   bytecode;
 }{
   flags = "-extension layouts_beta -extension let_mutable";
   native;
 }{
   flags = "-extension layouts_beta -extension let_mutable";
   bytecode;
 }*)

open Stdlib_upstream_compatible

let triangle_f64 n =
  let mutable sum = #0.0 in
  for i = 1 to n do
    sum <- Float_u.add sum (Float_u.of_int i)
  done;
  sum

let () = Printf.printf "%.2f\n" (triangle_f64 10 |> Float_u.to_float)


let triangle_f32 n =
  let mutable sum = #0.0s in
  for i = 1 to n do
    sum <- Float32_u.add sum (Float32_u.of_int i)
  done;
  sum

let () = Printf.printf "%.2f\n" (triangle_f32 10 |> Float32_u.to_float)


let triangle_i64 n =
  let mutable sum = #0L in
  for i = 1 to n do
    sum <- Int64_u.add sum (Int64_u.of_int i)
  done;
  sum

let () = Printf.printf "%d\n" (triangle_i64 10 |> Int64_u.to_int)


let triangle_i32 n =
  let mutable sum = #0l in
  for i = 1 to n do
    sum <- Int32_u.add sum (Int32_u.of_int i)
  done;
  sum

let () = Printf.printf "%d\n" (triangle_i32 10 |> Int32_u.to_int)


(* CR jrayman: how do you create a vec128? *)

let triangle_i64_i32_f64 n =
  let mutable sum = #(#0L, #(#0l, #0.)) in
  for i = 1 to n do
    let #(a, #(b, c)) = sum in
    sum <- #(Int64_u.add a (Int64_u.of_int i),
             #(Int32_u.add b (Int32_u.of_int i),
               Float_u.add c (Float_u.of_int i)))
  done;
  sum

let () =
  let #(a, #(b, c)) = triangle_i64_i32_f64 10 in
  Printf.printf "%d %d %.2f\n" (Int64_u.to_int a)
                               (Int32_u.to_int b)
                               (Float_u.to_float c)
