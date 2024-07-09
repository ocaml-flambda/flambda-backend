(* TEST
   flags = "-extension layouts_beta -extension small_numbers";
   include stdlib_beta;
   flambda2;
   include stdlib_upstream_compatible;
   {
   native;
   }{
   bytecode;
   }
*)

(*****************************************)
(* Prelude: Functions on unboxed numbers *)

module Float32_u = Stdlib_beta.Float32_u
module Float32 = Stdlib_beta.Float32
module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u

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
  | Mixed4 of string * float# * int32#
  | Mixed5 of float * float# * int * int32# * nativeint# * int64#
  | Mixed6 of float * int32# * float#
  | Mixed7 of float * int64# * float# * nativeint#
  | Mixed8 of float * int32# * float# * int64# * float#
  | Mixed9 of float * float# * float32#
  | Mixed10 of float * float32# * float# * int64# * float#
  | Mixed11 of float * int32# * float32# * float# * int64# * nativeint#
  | Uniform2 of float * float
  (* "I"mixed for "i"nlined records. *)
  | Imixed1 of { x1 : float# }
  | Imixed2 of { mutable x1 : float; mutable x2 : float# }
  | Imixed3 of { x1 : float; x2 : float#; x3 : float# }
  | Imixed4 of { mutable x1: string; mutable x2 : float#; mutable x3 : int32# }
  | Imixed5 of { x1: float; x2 : float#; x3 : int; x4: int32#; x5 : nativeint#;
                 x6 : int64# }
  | Imixed6 of { mutable x1: float; mutable x2 : int32#; mutable x3 : float# }
  | Imixed7 of { x1: float; x2 : int64#; x3 : float#; x4 : nativeint# }
  | Imixed8 of { mutable x1: float; mutable x2 : int32#; mutable x3 : float#;
                 mutable x4 : int64#; mutable x5 : float# }
  | Imixed9 of { x1: float; x2 : float#; x3 : float32# }
  | Imixed10 of { mutable x1 : float; mutable x2 : float32#;
                  mutable x3 : float#; mutable x4 : int64#; mutable x5 : float#
                }
  | Imixed11 of { x1 : float; x2 : int32#; x3 : float32#; x4 : float#;
                  x5 : int64#; x6 : nativeint# }

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
      sprintf "Mixed4 (%s, %f, %i)"
        x1 (Float_u.to_float x2) (Int32_u.to_int x3)
  | Mixed5 (x1, x2, x3, x4, x5, x6) ->
      sprintf "Mixed5 (%f, %f, %i, %i, %i, %i)"
        x1 (Float_u.to_float x2) x3 (Int32_u.to_int x4) (Nativeint_u.to_int x5)
        (Int64_u.to_int x6)
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
  | Mixed9 (x1, x2, x3) ->
      sprintf "Mixed9 (%f, %f, %f)" x1 (Float_u.to_float x2)
      (Float_u.to_float (Float32_u.to_float x3))
  | Mixed10 (x1, x2, x3, x4, x5) ->
      sprintf "Mixed10 (%f, %f, %f, %i, %f)"
        x1 (Float_u.to_float (Float32_u.to_float x2)) (Float_u.to_float x3)
        (Int64_u.to_int x4) (Float_u.to_float x5)
  | Mixed11 (x1, x2, x3, x4, x5, x6) ->
      sprintf "Mixed11 (%f, %i, %f, %f, %i, %i)"
        x1 (Int32_u.to_int x2) (Float_u.to_float (Float32_u.to_float x3))
        (Float_u.to_float x4) (Int64_u.to_int x5) (Nativeint_u.to_int x6)
  | Uniform2 (x1, x2) -> sprintf "Uniform2 (%f, %f)" x1 x2
  | Imixed1 {x1} -> sprintf "Imixed1 { x1=%f }" (Float_u.to_float x1)
  | Imixed2 { x1; x2 } ->
      sprintf "Imixed2 { x1=%f; x2=%f }" x1 (Float_u.to_float x2)
  | Imixed3 { x1; x2; x3 } ->
      sprintf "Imixed3 { x1=%f; x2=%f; x3=%f }"
        x1 (Float_u.to_float x2) (Float_u.to_float x3)
  | Imixed4 { x1; x2; x3 } ->
      sprintf "Imixed4 { x1=%s; s2=%f; x3=%i }"
        x1 (Float_u.to_float x2) (Int32_u.to_int x3)
  | Imixed5 { x1; x2; x3; x4; x5; x6 } ->
      sprintf "Imixed5 { x1=%f; x2=%f; x3=%i; x4=%i; x5=%i; x6=%i }"
        x1 (Float_u.to_float x2) x3 (Int32_u.to_int x4) (Nativeint_u.to_int x5)
        (Int64_u.to_int x6)
  | Imixed6 { x1; x2; x3 } ->
      sprintf "Imixed6 { x1=%f; x2=%i; x3=%f }"
        x1 (Int32_u.to_int x2) (Float_u.to_float x3)
  | Imixed7 { x1; x2; x3; x4 } ->
      sprintf "Imixed7 { x1=%f; x2=%i; x3=%f; x4=%i }"
        x1 (Int64_u.to_int x2) (Float_u.to_float x3) (Nativeint_u.to_int x4)
  | Imixed8 { x1; x2; x3; x4; x5 } ->
      sprintf "Imixed8 { x1=%f; x2=%i; x3=%f; x4=%i; x5%f }"
        x1 (Int32_u.to_int x2) (Float_u.to_float x3) (Int64_u.to_int x4)
        (Float_u.to_float x5)
  | Imixed9 { x1; x2; x3 } ->
      sprintf "Imixed9 { x1=%f; x2=%f; x3=%f }" x1 (Float_u.to_float x2)
      (Float_u.to_float (Float32_u.to_float x3))
  | Imixed10 { x1; x2; x3; x4; x5 } ->
      sprintf "Imixed10 { x1=%f; x2=%f; x3=%f; x4=%i; x5=%f }"
        x1 (Float_u.to_float (Float32_u.to_float x2)) (Float_u.to_float x3)
        (Int64_u.to_int x4) (Float_u.to_float x5)
  | Imixed11 { x1; x2; x3; x4; x5; x6 } ->
      sprintf "Imixed11 { x1=%f; x2=%i; x3=%f; x4=%f; x5=%i; x6=%i }"
        x1 (Int32_u.to_int x2) (Float_u.to_float (Float32_u.to_float x3))
        (Float_u.to_float x4) (Int64_u.to_int x5) (Nativeint_u.to_int x6)

let print t = print_endline ("  " ^ to_string t)

(**********************)
(* Test: construction *)

let toplevel = Mixed1 #7.0

let run f =
  print_endline "Test (construction)";
  let lit = Mixed2 (3.0, #4.5) in
  let half_lit = Mixed3 (6.0, f, #5.0) in
  print toplevel;
  print lit;
  print half_lit;
;;

let () = run #17.0

(**********************************************************)
(* Test: construction and destruction of the same value, to
   exercise an optimization code path.
*)

let sum s uf uf' f f' i i32 i64 i_n f32 =
  float_of_string s +.
  Float_u.to_float uf +. Float_u.to_float uf' +. f +. f' +.
  Int32_u.to_float i32 +. Int64_u.to_float i64 +. Nativeint_u.to_float i_n
  +. float_of_int i +. (Float_u.to_float (Float32_u.to_float f32))

let construct_and_destruct s uf uf' f f' i i32 i64 i_n f32 =
  let Constant = Constant in
  let Uniform1 f = Uniform1 f in
  let Mixed1 uf = Mixed1 uf in
  let Mixed2 (f, uf) = Mixed2 (f, uf) in
  let Mixed3 (f, uf, uf') = Mixed3 (f, uf, uf') in
  let Mixed4 (s, uf, i32) = Mixed4 (s, uf, i32) in
  let Mixed5 (f, uf, i, i32, i_n, i64) = Mixed5 (f, uf, i, i32, i_n, i64) in
  let Mixed6 (f, i32, uf) = Mixed6 (f, i32, uf) in
  let Mixed7 (f, i64, uf, i_n) = Mixed7 (f, i64, uf, i_n) in
  let Mixed8 (f, i32, uf, i64, uf') = Mixed8 (f, i32, uf, i64, uf') in
  let Mixed9 (f, uf, f32) = Mixed9 (f, uf, f32) in
  let Mixed10 (f, f32, uf, i64, uf') = Mixed10 (f, f32, uf, i64, uf') in
  let Mixed11 (f, i32, f32, uf, i64, i_n) = Mixed11 (f, i32, f32, uf, i64, i_n) in
  let Uniform2 (f, f') = Uniform2 (f, f') in
  let Imixed1 { x1 = uf } = Imixed1 { x1 = uf } in
  let Imixed2 { x1 = f; x2 = uf } = Imixed2 { x1 = f; x2 = uf } in
  let Imixed3 { x1 = f; x2 = uf; x3 = uf' } =
      Imixed3 { x1 = f; x2 = uf; x3 = uf' }
  in
  let Imixed4 { x1 = s; x2 = uf; x3 = i32 } =
      Imixed4 { x1 = s; x2 = uf; x3 = i32 }
  in
  let Imixed5 { x1 = f; x2 = uf; x3 = i; x4 = i32; x5 = i_n; x6 = i64 } =
      Imixed5 { x1 = f; x2 = uf; x3 = i; x4 = i32; x5 = i_n; x6 = i64 }
  in
  let Imixed6 { x1 = f; x2 = i32; x3 = uf } =
      Imixed6 { x1 = f; x2 = i32; x3 = uf }
  in
  let Imixed7 { x1 = f; x2 = i64; x3 = uf; x4 = i_n } =
      Imixed7 { x1 = f; x2 = i64; x3 = uf; x4 = i_n }
  in
  let Imixed8 { x1 = f; x2 = i32; x3 = uf; x4 = i64; x5 = uf' } =
      Imixed8 { x1 = f; x2 = i32; x3 = uf; x4 = i64; x5 = uf' }
  in
  let Imixed9 { x1 = f; x2 = uf; x3 = f32 } =
      Imixed9 { x1 = f; x2 = uf; x3 = f32 }
  in
  let Imixed10 { x1 = f; x2 = f32; x3 = uf; x4 = i64; x5 = uf' } =
      Imixed10 { x1 = f; x2 = f32; x3 = uf; x4 = i64; x5 = uf' }
  in
  let Imixed11 { x1 = f; x2 = i32; x3 = f32; x4 = uf; x5 = i64; x6 = i_n } =
      Imixed11 { x1 = f; x2 = i32; x3 = f32; x4 = uf; x5 = i64; x6 = i_n }
  in
  sum s uf uf' f f' i i32 i64 i_n f32
[@@ocaml.warning "-partial-match"]

let () =
  let s = "0.3"
  and uf = #5.0
  and uf' = #5.1
  and f = 14.2
  and f' = 15.1
  and i = 0
  and i32 = #12l
  and i64 = #42L
  and i_n = #56n
  and f32 = #1.2s
  in
  let () =
    let sum1 = sum s uf uf' f f' i i32 i64 i_n f32 in
    let sum2 = construct_and_destruct s uf uf' f f' i i32 i64 i_n f32 in
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
  match r, t_rec1, t_rec2, t_rec3, t_rec4 with
  | Mixed1 a,
    Mixed1 x,
    Mixed2 (y1, y2),
    Mixed1 z,
    Mixed2 (w1, w2) ->
      Float_u.to_float x +.
      y1 +. Float_u.to_float y2 +.
      Float_u.to_float z +.
      w1 +. Float_u.to_float w2
  | _ -> assert false

and t_rec1 = Mixed1 #4.0
and t_rec2 = Mixed2 (5.0, #4.0)
and t_rec3 = Mixed1 #5.0
and t_rec4 = Mixed2 (6.0, #7.0)
and t_rec5 = Imixed1 { x1 = #8.0 }
and t_rec6 = Imixed2 { x1 = 9.0; x2 = #10.0 }

let _ =
  Printf.printf "Test (mixed constructors in recursive groups):\n";
  print t_rec1;
  print t_rec2;
  print t_rec3;
  print t_rec4;
  print t_rec5;
  print t_rec6;
  let result = f t_rec1 in
  print_float "  result (31.00)" result;
  print t_rec1;
  print t_rec2;
  print t_rec3;
  print t_rec4;
  print t_rec5;
  print t_rec6;
;;

(**************************)
(* Test: pattern matching *)

let go_tuple_args x y z =
  let f =
    match x with
    | Mixed11 (f1, i32_1, f32, uf1, i64, i_n) ->
        (* Close over the fields we projected out *)
        (fun () ->
           match y, z with
           | Mixed3 (f2, uf2, uf3),
             Mixed4 (s1, uf4, i32_2)
           | Mixed4 (s1, uf4, i32_2),
             Mixed3 (f2, uf2, uf3) ->
               [ f1;
                 Float_u.to_float uf1;
                 Int32_u.to_float i32_1;
                 Nativeint_u.to_float i_n;
                 Int64_u.to_float i64;
                 f2;
                 Float_u.to_float uf2;
                 Float_u.to_float uf3;
                 float_of_string s1;
                 Float_u.to_float uf4;
                 Int32_u.to_float i32_2;
                 Float32.to_float (Float32_u.to_float32 f32);
               ]
           | _ -> assert false
        )
    | _ -> assert false
  in
  f ()

let go_record_args x y z =
  let f =
    match x with
    | Imixed11 { x1 = f1; x2 = i32_1; x3 = f32; x4 = uf1; x5 = i64; x6 = i_n }
      ->
        (* Close over the fields we projected out *)
        (fun () ->
           match y, z with
           | Imixed3 { x1 = f2; x2 = uf2; x3 = uf3 },
             Imixed4 { x1 = s1; x2 = uf4; x3 = i32_2 }
           | Imixed4 { x1 = s1; x2 = uf4; x3 = i32_2 },
             Imixed3 { x1 = f2; x2 = uf2; x3 = uf3 } ->
               [ f1;
                 Float_u.to_float uf1;
                 Int32_u.to_float i32_1;
                 Nativeint_u.to_float i_n;
                 Int64_u.to_float i64;
                 f2;
                 Float_u.to_float uf2;
                 Float_u.to_float uf3;
                 float_of_string s1;
                 Float_u.to_float uf4;
                 Int32_u.to_float i32_2;
                 Float32.to_float (Float32_u.to_float32 f32);
               ]
           | _ -> assert false
        )
    | _ -> assert false
  in
  f ()


let test ~go ~name ~mixed11 ~mixed3 ~mixed4 =
  let f1 = 4.0
  and f2 = 42.0
  and s1 = "36.0"
  and i32_1 = #3l
  and i32_2 = -#10l
  and i64 = -#20L
  and i_n = #174n
  and uf1 = #17.0
  and uf2 = #32.0
  and uf3 = #47.5
  and uf4 = #47.8
  and f32 = #1.2s
  in
  let x = mixed11 f1 i32_1 f32 uf1 i64 i_n in
  let y = mixed3 f2 uf2 uf3 in
  let z = mixed4 s1 uf4 i32_2 in
  (* These results should match as [go] is symmetric in
     its 2nd/3rd arguments.
  *)
  let res1 = go x y z
  and res2 = go x z y
  in
  Printf.printf "Test (pattern matching, %s).\n" name;
  assert (res1 = res2);
  Printf.printf "  Contents of fields:\n";
  List.iter (Printf.printf "  %.3f\n") res1
;;

let () = test ~go:go_tuple_args ~name:"tuple args"
    ~mixed3:(fun x1 x2 x3 -> Mixed3 (x1, x2, x3))
    ~mixed4:(fun x1 x2 x3 -> Mixed4 (x1, x2, x3))
    ~mixed11:(fun x1 x2 x3 x4 x5 x6 ->
        Mixed11 (x1, x2, x3, x4, x5, x6))

let () = test ~go:go_record_args ~name:"record args"
    ~mixed3:(fun x1 x2 x3 -> Imixed3 {x1; x2; x3})
    ~mixed4:(fun x1 x2 x3 -> Imixed4 {x1; x2; x3})
    ~mixed11:(fun x1 x2 x3 x4 x5 x6 ->
        Imixed11 {x1; x2; x3; x4; x5; x6})

(*********************************************)
(* Test: pattern matching, recursive closure *)

let go_recursive_tuple_args x y z =
  let f_even =
    match y, z with
    | Mixed3 (f2, uf2, uf3),
      Mixed4 (s1, uf4, i32_2)
    | Mixed4 (s1, uf4, i32_2),
      Mixed3 (f2, uf2, uf3) ->
        (* Close over the fields we projected out
           with recursive functions.
        *)
        let rec f_odd n =
          if n < 7 then f_even (n+1)
          else match x with
          | Mixed11 (f1, i32_1, f32, uf1, i64, i_n) ->
              [ float_of_int n;
                f1;
                Float_u.to_float uf1;
                Int32_u.to_float i32_1;
                Nativeint_u.to_float i_n;
                Int64_u.to_float i64;
                f2;
                Float_u.to_float uf2;
                Float_u.to_float uf3;
                float_of_string s1;
                Float_u.to_float uf4;
                Int32_u.to_float i32_2;
                Float32.to_float (Float32_u.to_float32 f32);
              ]
          | _ -> assert false
        and f_even n = f_odd (n+1) in
        f_even
    | _ -> assert false
  in
  f_even 0

let go_recursive_record_args x y z =
  let f_even =
    match y, z with
    | Imixed3 { x1 = f2; x2 = uf2; x3 = uf3 },
      Imixed4 { x1 = s1; x2 = uf4; x3 = i32_2 }
    | Imixed4 { x1 = s1; x2 = uf4; x3 = i32_2 },
      Imixed3 { x1 = f2; x2 = uf2; x3 = uf3 } ->
        (* Close over the fields we projected out
           with recursive functions.
        *)
        let rec f_odd n =
          if n < 7 then f_even (n+1)
          else match x with
          | Imixed11 { x1 = f1; x2 = i32_1; x3 = f32; x4 = uf1;
                       x5 = i64; x6 = i_n } ->
              [ float_of_int n;
                f1;
                Float_u.to_float uf1;
                Int32_u.to_float i32_1;
                Nativeint_u.to_float i_n;
                Int64_u.to_float i64;
                f2;
                Float_u.to_float uf2;
                Float_u.to_float uf3;
                float_of_string s1;
                Float_u.to_float uf4;
                Int32_u.to_float i32_2;
                Float32.to_float (Float32_u.to_float32 f32);
              ]
          | _ -> assert false
        and f_even n = f_odd (n+1) in
        f_even
    | _ -> assert false
  in
  f_even 0

let test_recursive ~go ~name ~mixed3 ~mixed4 ~mixed11 =
  let f1 = 4.0
  and f2 = 42.0
  and s1 = "36.0"
  and i32_1 = #3l
  and i32_2 = -#10l
  and i64 = -#20L
  and i_n = #174n
  and uf1 = #17.0
  and uf2 = #32.0
  and uf3 = #47.5
  and uf4 = #47.8
  and f32 = #1.2s
  in
  let x = mixed11 f1 i32_1 f32 uf1 i64 i_n in
  let y = mixed3 f2 uf2 uf3 in
  let z = mixed4 s1 uf4 i32_2 in
  (* These results should match as [go_recursive] is symmetric in
     its 2nd/3rd arguments.
  *)
  let res1 = go x y z
  and res2 = go x z y
  in
  Printf.printf "Test (pattern matching, recursive closure, %s).\n" name;
  assert (res1 = res2);
  Printf.printf "  Contents of fields:\n";
  List.iter (Printf.printf "  %.3f\n") res1
;;

let () = test_recursive ~go:go_recursive_tuple_args ~name:"tuple args"
    ~mixed3:(fun x1 x2 x3 -> Mixed3 (x1, x2, x3))
    ~mixed4:(fun x1 x2 x3 -> Mixed4 (x1, x2, x3))
    ~mixed11:(fun x1 x2 x3 x4 x5 x6 ->
        Mixed11 (x1, x2, x3, x4, x5, x6))

let () = test_recursive ~go:go_recursive_record_args ~name:"record args"
    ~mixed3:(fun x1 x2 x3 -> Imixed3 {x1; x2; x3})
    ~mixed4:(fun x1 x2 x3 -> Imixed4 {x1; x2; x3})
    ~mixed11:(fun x1 x2 x3 x4 x5 x6 ->
        Imixed11 {x1; x2; x3; x4; x5; x6})
