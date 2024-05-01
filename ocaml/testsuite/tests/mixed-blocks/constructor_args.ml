(* TEST
   flags = "-extension layouts_alpha";
   flambda2;
   {
   native;
   }{
   bytecode;
   }
*)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = Stdlib__Float_u
module Int32_u = Stdlib__Int32_u
module Int64_u = Stdlib__Int64_u
module Nativeint_u = Stdlib__Nativeint_u

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x

(**************************)
(* Mixed constructor args *)

type t =
  | Constant
  | Uniform1 of float
  | Mixed1 of float#
  | Mixed2 of float * float#
  | Mixed3 of float * float# * float#
  | Mixed4 of float * float# * int32#
  | Mixed5 of float * float# * int64# * float#
  | Mixed6 of float * int32# * float#
  | Mixed7 of float * int64# * float# * nativeint#
  | Mixed8 of float * int32# * float# * int64# * float#
  | Uniform2 of float * float

type t_ext = ..

type t_ext +=
  | Ext_mixed1 of float#
  | Ext_mixed2 of float * float#
  | Ext_mixed3 of float * float# * float#
  | Ext_mixed4 of float * float# * int32#
  | Ext_mixed5 of float * float# * int64# * float#
  | Ext_mixed6 of float * int32# * float#
  | Ext_mixed7 of float * int64# * float# * nativeint#
  | Ext_mixed8 of float * int32# * float# * int64# * float#

let sprintf = Printf.sprintf

let to_string = function
  | Constant -> "Constant"
  | Uniform1 x -> sprintf "Uniform1 %f" x
  | Mixed1 x -> sprintf "Mixed1 %f" (Float_u.to_float x)
  | Mixed2 (x1, x2) -> sprintf "Mixed2 (%f, %f)" x1 (Float_u.to_float x2)
  | Mixed3 (x1, x2, x3) ->
      sprintf "Mixed3 (%f, %f, %f)"
        x1 (Float_u.to_float x2) (Float_u.to_float x3)
  | Mixed4 (x1, x2, x3) ->
      sprintf "Mixed4 (%f, %f, %i)"
        x1 (Float_u.to_float x2) (Int32_u.to_int x3)
  | Mixed5 (x1, x2, x3, x4) ->
      sprintf "Mixed5 (%f, %f, %i, %f)"
        x1 (Float_u.to_float x2) (Int64_u.to_int x3) (Float_u.to_float x4)
  | Mixed6 (x1, x2, x3) ->
      sprintf "Mixed6 (%f, %i, %f)"
        x1 (Int32_u.to_int x2) (Float_u.to_float x3)
  | Mixed7 (x1, x2, x3, x4) ->
      sprintf "Mixed7 (%f, %i, %f, %i)"
        x1 (Int64_u.to_int x2) (Float_u.to_float x3) (Nativeint_u.to_int x4)
  | Mixed8 (x1, x2, x3, x4, x5) ->
      sprintf "Mixed8 (%f, %i, %f, %i, %f)"
        x1 (Int32_u.to_int x2) (Float_u.to_float x3) (Int64_u.to_int x4)
        (Float_u.to_float x5)
  | Uniform2 (x1, x2) -> sprintf "Uniform2 (%f, %f)" x1 x2

let ext_to_string = function
  | Ext_mixed1 x -> sprintf "Ext_mixed1 %f" (Float_u.to_float x)
  | Ext_mixed2 (x1, x2) -> sprintf "Ext_mixed2 (%f, %f)" x1 (Float_u.to_float x2)
  | Ext_mixed3 (x1, x2, x3) ->
    sprintf "Ext_mixed3 (%f, %f, %f)"
      x1 (Float_u.to_float x2) (Float_u.to_float x3)
  | Ext_mixed4 (x1, x2, x3) ->
    sprintf "Ext_mixed4 (%f, %f, %i)"
      x1 (Float_u.to_float x2) (Int32_u.to_int x3)
  | Ext_mixed5 (x1, x2, x3, x4) ->
    sprintf "Ext_mixed5 (%f, %f, %i, %f)"
      x1 (Float_u.to_float x2) (Int64_u.to_int x3) (Float_u.to_float x4)
  | Ext_mixed6 (x1, x2, x3) ->
    sprintf "Ext_mixed6 (%f, %i, %f)"
      x1 (Int32_u.to_int x2) (Float_u.to_float x3)
  | Ext_mixed7 (x1, x2, x3, x4) ->
    sprintf "Ext_mixed7 (%f, %i, %f, %i)"
      x1 (Int64_u.to_int x2) (Float_u.to_float x3) (Nativeint_u.to_int x4)
  | Ext_mixed8 (x1, x2, x3, x4, x5) ->
    sprintf "Ext_mixed8 (%f, %i, %f, %i, %f)"
      x1 (Int32_u.to_int x2) (Float_u.to_float x3) (Int64_u.to_int x4)
      (Float_u.to_float x5)
  | _ -> "<ext>"

let print t = print_endline ("  " ^ to_string t)
let print_ext t = print_endline ("  " ^ ext_to_string t)

(**********************)
(* Test: construction *)

let toplevel = Mixed1 #7.0
let toplevel_ext = Ext_mixed1 #8.0

let run f =
  print_endline "Test (construction)";
  let lit = Mixed2 (3.0, #4.5) in
  let half_lit = Mixed3 (6.0, f, #5.0) in
  print toplevel;
  print_ext toplevel_ext;
  print lit;
  print half_lit;
;;

let () = run #17.0

(**********************************************************)
(* Test: construction and destruction of the same value, to
   exercise an optimization code path.
*)

let sum uf uf' f f' i32 i64 i_n =
  Float_u.to_float uf +. Float_u.to_float uf' +. f +. f' +.
  Int32_u.to_float i32 +. Int64_u.to_float i64 +. Nativeint_u.to_float i_n

let construct_and_destruct uf uf' f f' i32 i64 i_n =
  let Constant = Constant in
  let Uniform1 f = Uniform1 f in
  let Mixed1 uf = Mixed1 uf in
  let Mixed2 (f, uf) = Mixed2 (f, uf) in
  let Mixed3 (f, uf, uf') = Mixed3 (f, uf, uf') in
  let Mixed4 (f, uf, i32) = Mixed4 (f, uf, i32) in
  let Mixed5 (f, uf, i64, uf') = Mixed5 (f, uf, i64, uf') in
  let Mixed6 (f, i32, uf) = Mixed6 (f, i32, uf) in
  let Mixed7 (f, i64, uf, i_n) = Mixed7 (f, i64, uf, i_n) in
  let Mixed8 (f, i32, uf, i64, uf') = Mixed8 (f, i32, uf, i64, uf') in
  let Ext_mixed1 uf = Ext_mixed1 uf in
  let Ext_mixed2 (f, uf) = Ext_mixed2 (f, uf) in
  let Ext_mixed3 (f, uf, uf') = Ext_mixed3 (f, uf, uf') in
  let Ext_mixed4 (f, uf, i32) = Ext_mixed4 (f, uf, i32) in
  let Ext_mixed5 (f, uf, i64, uf') = Ext_mixed5 (f, uf, i64, uf') in
  let Ext_mixed6 (f, i32, uf) = Ext_mixed6 (f, i32, uf) in
  let Ext_mixed7 (f, i64, uf, i_n) = Ext_mixed7 (f, i64, uf, i_n) in
  let Ext_mixed8 (f, i32, uf, i64, uf') = Ext_mixed8 (f, i32, uf, i64, uf') in
  let Uniform2 (f, f') = Uniform2 (f, f') in
  sum uf uf' f f' i32 i64 i_n
[@@ocaml.warning "-partial-match"]

let () =
  let uf = #5.0
  and uf' = #5.1
  and f = 14.2
  and f' = 15.4
  and i32 = #12l
  and i64 = #42L
  and i_n = #56n
  in
  let () =
    let sum1 = sum uf uf' f f' i32 i64 i_n in
    let sum2 = construct_and_destruct uf uf' f f' i32 i64 i_n in
    Printf.printf
      "Test (construct and destruct): %f = %f (%s)\n"
      sum1
      sum2
      (if Float.equal sum1 sum2 then "PASS" else "FAIL")
  in
  ()

(**************************)
(* Test: recursive groups *)

let rec f r =
  match r, t_rec1, t_rec2, t_ext_rec1, t_ext_rec2 with
  | Mixed1 a,
    Mixed1 x,
    Mixed2 (y1, y2),
    Ext_mixed1 z,
    Ext_mixed2 (w1, w2) ->
      Float_u.to_float x +.
      y1 +. Float_u.to_float y2 +.
      Float_u.to_float z +.
      w1 +. Float_u.to_float w2
  | _ -> assert false

and t_rec1 = Mixed1 #4.0
and t_rec2 = Mixed2 (5.0, #4.0)
and t_ext_rec1 = Ext_mixed1 #5.0
and t_ext_rec2 = Ext_mixed2 (6.0, #7.0)

let _ =
  Printf.printf "Test (mixed constructors in recursive groups):\n";
  print t_rec1;
  print t_rec2;
  print_ext t_ext_rec2;
  print_ext t_ext_rec2;
  let result = f t_rec1 in
  print_float "  result (31.00)" result;
  print t_rec1;
  print t_rec2;
  print_ext t_ext_rec2;
  print_ext t_ext_rec2;
;;

(**************************)
(* Test: pattern matching *)

let go x y z =
  let f =
    match x with
    | Mixed5 (f1, uf1, i1, uf2) ->
        (* Close over the fields we projected out *)
        (fun () ->
           match y, z with
           | Mixed3 (f2, uf3, uf4),
             Mixed4 (f3, uf5, i2)
           | Mixed4 (f3, uf5, i2),
             Mixed3 (f2, uf3, uf4) ->
               [ f1;
                 Float_u.to_float uf1;
                 Int64_u.to_float i1;
                 Float_u.to_float uf2;
                 f2;
                 Float_u.to_float uf3;
                 Float_u.to_float uf4;
                 f3;
                 Float_u.to_float uf5;
                 Int32_u.to_float i2;
               ]
           | _ -> assert false
        )
    | _ -> assert false
  in
  f ()

let test () =
  let f1 = 4.0
  and f2 = 42.0
  and f3 = 36.0
  and i1 = #3L
  and i2 = -#10l
  and uf1 = #17.0
  and uf2 = #28.0
  and uf3 = #32.0
  and uf4 = #47.5
  and uf5 = #47.8
  in
  let x = Mixed5 (f1, uf1, i1, uf2) in
  let y = Mixed3 (f2, uf3, uf4) in
  let z = Mixed4 (f3, uf5, i2) in
  (* These results should match as [go] is symmetric in
     its 2nd/3rd arguments.
  *)
  let res1 = go x y z
  and res2 = go x z y
  in
  Printf.printf "Test (pattern matching).\n";
  assert (res1 = res2);
  Printf.printf "  Contents of fields:\n";
  List.iter (Printf.printf "  %.3f\n") res1
;;

let () = test ()

(*********************************************)
(* Test: pattern matching, recursive closure *)

let go_recursive x y z =
  let f_even =
    match y, z with
    | Mixed3 (f2, uf3, uf4),
      Mixed4 (f3, uf5, i2)
    | Mixed4 (f3, uf5, i2),
      Mixed3 (f2, uf3, uf4) ->
        (* Close over the fields we projected out
           with recursive functions.
        *)
        let rec f_odd i =
          if i < 7 then f_even (i+1)
          else match x with
          | Mixed5 (f1, uf1, i1, uf2) ->
              [ float_of_int i;
                f1;
                Float_u.to_float uf1;
                Int64_u.to_float i1;
                Float_u.to_float uf2;
                f2;
                Float_u.to_float uf3;
                Float_u.to_float uf4;
                f3;
                Float_u.to_float uf5;
                Int32_u.to_float i2;
              ]
          | _ -> assert false
        and f_even i = f_odd (i+1) in
        f_even
    | _ -> assert false
  in
  f_even 0

let test_recursive () =
  let f1 = 4.0
  and f2 = 42.0
  and f3 = 36.0
  and i1 = #3L
  and i2 = -#10l
  and uf1 = #17.0
  and uf2 = #28.0
  and uf3 = #32.0
  and uf4 = #47.5
  and uf5 = #47.8
  in
  let x = Mixed5 (f1,uf1, i1, uf2) in
  let y = Mixed3 (f2, uf3, uf4) in
  let z = Mixed4 (f3, uf5, i2) in
  (* These results should match as [go_recursive] is symmetric in
     its 2nd/3rd arguments.
  *)
  let res1 = go_recursive x y z
  and res2 = go_recursive x z y
  in
  Printf.printf "Test (pattern matching, recursive closure).\n";
  assert (res1 = res2);
  Printf.printf "  Contents of fields:\n";
  List.iter (Printf.printf "  %.3f\n") res1
;;

let () = test_recursive ()
