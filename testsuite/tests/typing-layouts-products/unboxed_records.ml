(* TEST
 reference = "${test_source_directory}/unboxed_records.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe no_extensions";
   compiler_reference = "${test_source_directory}/unboxed_records_disabled.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 } {
   flags = "-extension layouts_alpha";
   native;
 } {
   flags = "-extension layouts_alpha -Oclassic";
   native;
 } {
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }{
   flags = "";
   bytecode;
 }{
   flags = "";
   native;
 }
*)

(* NOTE: When adding tests to this file, consider updating
   [typing-layouts-products/implicit_unboxed_records.ml] *)

open Stdlib_upstream_compatible

type ints = #{ x : int ; y : int }

let print_floatu prefix x =
  Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x
let print_ints prefix #{ x; y } = Printf.printf "%s: [%d %d]\n" prefix x y

(********************************************************)
(* Test 1: Basic functions manipulating unboxed records *)

type unboxed_float_int = #{ f : float# ; i : int }

(* takes an unboxed record *)
let mult_float_by_int #{ f ; i } = Float_u.(mul f (of_int i))

(* returns a unboxed record *)
let div_mod m n = #{ x = m/n; y = m mod n }

type cd = #{ c : int ; d : int }
type abcd = #{ a : int ; b : int ; cd : cd }

type ghi = #{ g : int; h : int ; i : int }
type fghi = #{ f : int ; ghi : ghi }
type efghi = #{ e : int ; fghi : fghi }

type add_some_stuff_res_inner = #{ res2 : int ; res3 : int }
type add_some_stuff_res = #{ res1 : int ; res23 : add_some_stuff_res_inner }

(* take multiple nested unboxed records, returns an unboxed record *)
let add_some_stuff x y =
  let #{ a ; b ; cd = #{ c; d }} = x in
  let #{ e ; fghi = #{ f ; ghi = #{ g; h; i } } } = y in
  #{ res1 = a+b+c; res23 = #{ res2 = d+e+f; res3 = g+h+i } }

let test1 () =
  let pi = #3.14 in
  print_floatu "Test 1, twice pi inlined"
    ((mult_float_by_int [@inlined]) #{ f = pi; i = 2 });
  print_floatu "Test 1, twice pi not inlined"
    ((mult_float_by_int [@inlined never]) #{ f = pi; i = 2 });
  print_ints "Test 1, 14/3 inlined" ((div_mod [@inlined hint]) 14 3);
  print_ints "Test 1, 14/3 not inlined" ((div_mod [@inlined never]) 14 3);
  let #{ res1 ; res23 = #{ res2 ; res3 }} =
    (add_some_stuff [@inlined])
      #{a=1; b=2; cd=#{c=3;d=4}}
      #{e=5; fghi=#{f=6;ghi=#{g=7;h=8;i=9}}}
  in
  Printf.printf "Test 1, [6 15 24] inlined: [%d %d %d]\n" res1 res2 res3;
  let #{ res1 ; res23 = #{ res2 ; res3 }} =
    (add_some_stuff [@inlined never])
      #{a=1; b=2; cd=#{c=3;d=4}}
      #{e=5; fghi=#{f=6;ghi=#{g=7;h=8;i=9}}}
  in
  Printf.printf "Test 1, [6 15 24] not inlined: [%d %d %d]\n" res1 res2 res3

let _ = test1 ()

(**********************************)
(* Test 2: higher-order functions *)

type ff' = #{ f : float# ; f' : float }
type t = #{ i : int ; ff' : ff' }

let[@inline never] add_t
    #{ i = i1; ff' = #{ f = f1; f' = f1'}}
    #{ i = i2; ff' = #{ f = f2; f' = f2'}}
  =
  #{ i = i1 + i2; ff' = #{ f = Float_u.add f1 f2; f' =  f1' +. f2'}}

let[@inline never] sub_t
    #{ i = i1; ff' = #{ f = f1; f' = f1'}}
    #{ i = i2; ff' = #{ f = f2; f' = f2'}}
  =
  #{ i = i1 - i2; ff' = #{ f = Float_u.sub f1 f2; f' =  f1' -. f2'}}

let[@inline never] twice f (x : t) = f (f x)

let[@inline never] compose f g (x : t) = f (g x)

let t_sum #{ i; ff' = #{ f ; f' }} =
  ((Float.of_int i) +. (Float_u.to_float f +. f'))

let print_t_sum prefix t = Printf.printf "%s: %.2f\n" prefix (t_sum t)

let[@inline never] twice_on_pi f =
  let pi = #{ i = 1; ff' = #{ f = #2.0; f' = 0.14 } } in
  twice f pi

let times_four =
  twice (fun x -> add_t x x)

let _ =
  let pi = #{ i = 1; ff' = #{ f = #2.0; f' = 0.14 } } in
  let one = #{ i = 0; ff' = #{ f = #1.0; f' = 0.0 } } in
  let zero = #{ i = 0; ff' = #{ f = #0.0; f' = 0.0 } } in

  print_t_sum "Test 2, add pi twice"
    (twice (fun x -> add_t x pi) zero);
  print_t_sum "Test 2, add pi four times"
    (twice (twice (fun x -> add_t x pi)) zero);
  print_t_sum "Test 2, increment pi twice"
    (twice_on_pi (fun x -> add_t one x));
  print_t_sum "Test 2, increment pi four times"
    (twice_on_pi (twice (fun x -> add_t one x)));
  print_t_sum "Test 2, e times four"
    (times_four #{ i = 1; ff' = #{ f = #1.0 ; f' = 2.72 } });
  print_t_sum "Test 2, pi times sixteen"
    (twice_on_pi times_four);
  print_t_sum "Test 2, pi times sixteen again"
    (compose times_four times_four pi);
  print_t_sum "Test 2, pi minus four"
    (let two = twice (fun x -> add_t x one) zero in
     let add_two = add_t two in
     let add_two_after = compose add_two in
     let minus_four =
       add_two_after
         (twice (fun x -> add_t x #{ i = -1; ff' = #{f = -#1.0; f' = -1.0 } }))
     in
     minus_four pi)

(***************************************)
(* Test 3: unboxed records in closures *)

type int_floatu = #{ i : int ; f : float# }
type floatarray_floatu = #{ fa : float array ; f : float# }

(* [go]'s closure should have an unboxed record with an [int] (immediate), a
   [float#] (float64) and a [float array] (value). *)
let[@inline never] f3 bounds steps_init () =
  let[@inline never] rec go k =
    let #{ i = n; f = m } = bounds in
    let #{ fa = steps; f = init } = steps_init in
    if k = n
    then init
    else begin
      let acc = go (k + 1) in
      steps.(k) <- Float_u.to_float acc;
      Float_u.add m acc
    end
  in
  go 0


(* many args - odd args are floats, even args are unboxed records *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let #{ x = start_k; y = end_k } = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then 0.0
    else begin
      let #{ i = x2_1; ff' = #{ f = x2_2; f' = x2_3 } } = x2 in
      let #{ i = x4_1; ff' = #{ f = x4_2; f' = x4_3 } } = x4 in
      let #{ i = x6_1; ff' = #{ f = x6_2; f' = x6_3 } } = x6 in
      let #{ i = x8_1; ff' = #{ f = x8_2; f' = x8_3 } } = x8 in
      let sum =
        Float.of_int x2_1 +. Float_u.to_float x2_2 +. x2_3 +.
        Float.of_int x4_1 +. Float_u.to_float x4_2 +. x4_3 +.
        Float.of_int x6_1 +. Float_u.to_float x6_2 +. x6_3 +.
        Float.of_int x8_1 +. Float_u.to_float x8_2 +. x8_3
      in
      let acc = go (k + 1) in
      steps.(k) <- acc;
      acc +. ((x1 +. x3 +. x5 +. x7 +. x9) *. sum)
    end
  in
  go start_k

let test3 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let five_pi = f3 #{ i = 5; f = #3.14} #{ fa = steps ; f = #0.0 } in
  print_floatu "Test 3, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 3, step %d: %.2f\n") steps;

  (* Test f3_manyargs

          (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 50.86
      3 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 152.58
      6 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 306.16
      9 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 457.74

    ( but we expect some floating point error )
  *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let x1 = 3.14 in
  let x3 = 2.72 in
  let x5 = 1.62 in
  let x7 = 1.41 in
  let x9 = 42.0 in

  (* these sum to 3 *)
  let x2 = #{ i = 7; ff' = #{ f = #40.0 ; f' = 2.0 } } in
  let x4 = #{ i = -23; ff' = #{ f = #100.0 ; f' = 9.0 } } in
  let x6 = #{ i = -242; ff' = #{ f = #5.5 ; f' = 84.5 } } in
  let x8 = #{ i = -2; ff' = #{ f = #20.0 ; f' = 2.0 } } in

  let f3_manyargs =
    f3_manyargs #{ x = 4; y = 8} x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_float "Test 3, 610.68: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 3, step %d: %.2f\n") steps

let _ = test3 ()

(*********************************************)
(* Test 4: Partial and indirect applications *)

type tt = #{ x1 : t ; x2 : t }

let[@inline never] test4 () =
  let one = #{ i = -1; ff' = #{ f = #1.33 ; f' = 0.67 } } in
  let two = #{ i = -5; ff' = #{ f = #12.7 ; f' = -5.7 } } in

  (* Simple indirect call *)
  let[@inline never] go f = f one two in
  let #{ x1 ; x2 } = #{ x1 = go add_t; x2 = go sub_t } in
  print_t_sum "Test 4, 1 + 2" x1;
  print_t_sum "Test 4, 1 - 2" x2;

  (* partial application to an unboxed record and with one remaining *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let f = Sys.opaque_identity (f3 #{ i = 5 ; f = #3.14 }) in
  let five_pi = f #{ fa = steps ; f = #0.0 } in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 4, step %d: %.2f\n") steps;

  (* That test again, but making f3 also opaque to prevent expansion of the
     partial application. *)
  let f3 = Sys.opaque_identity f3 in

  let steps = Array.init 10 (fun _ -> 0.0) in
  let f = Sys.opaque_identity (f3 #{ i = 5 ; f = #3.14 }) in
  let five_pi = f #{ fa = steps ; f = #0.0 } in
  print_floatu "Test 4, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 4, step %d: %.2f\n") steps

let _ = test4 ()

(****************************)
(* Test 5: Over application *)

let[@inline never] f5 n m =
  let[@inline never] go f =
    f (add_t n m)
  in
  go

let test5 () =
  let one = #{ i = -1; ff' = #{ f = #1.33 ; f' = 0.67 } } in
  let pi = #{ i = 1; ff' = #{ f = #2.0 ; f' = 0.14 } } in
  let e = #{ i = 1; ff' = #{ f = #0.1 ; f' = 1.62 } } in
  let _ : unit =
    f5 pi e
      (fun n s m -> print_t_sum s (add_t n m)) "Test 5, pi+e+1"
      one
  in
  ()

let _ = test5 ()

(*************************************)
(* Test 6: methods on unboxed record *)

(* CR layouts: add tests that unboxed records in objects, once that is
   allowed. *)

(* unboxed record args and returns *)
let f6_1 () = object
  method f6_m1 t1 t2 t3 =
    add_t (sub_t t1 t2) t3
end

(* recursion *)
let f6_2 n = object(self)
  method f6_m2 n3 tup f =
    if n3 = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) then
      tup
    else f (self#f6_m2 (n3+1) tup f)
end

(* overapplication to unboxed record and value args *)
let f6_3 n k = object
  method f6_m3 n3 tup f =
    let n = ((Sys.opaque_identity fst) n) + ((Sys.opaque_identity snd) n) in
    f (n + k + n3) tup
end

let test6 () =
  let one = #{ i = -1; ff' = #{ f = #1.33 ; f' = 0.67 } } in
  let pi = #{ i = 1; ff' = #{ f = #2.0 ; f' = 0.14 } } in
  let e = #{ i = 1; ff' = #{ f = #0.1 ; f' = 1.62 } } in
  let add3 n (m, k) = n + m + k in

  (* (3.14 - 2.72) + 1 = 1.42 *)
  let o = (Sys.opaque_identity f6_1) () in
  print_t_sum "Test 6, 1.42"
    (o#f6_m1 pi e one);

  (* 4.25 * 8 = 34 *)
  let t_4_25 = #{ i = 2; ff' = #{ f = #1.1 ; f' = 1.15 } } in
  let o = (Sys.opaque_identity f6_2) (4,7) in
  let result = o#f6_m2 8 t_4_25 (fun x -> add_t x x) in
  print_t_sum "Test 6, 34.00" result;

  (* (1 + 2 + 3 + (-2) + (-12) + 4) * (2.72 + (-1) + 10) = -46.88 *)
  let o = (Sys.opaque_identity f6_3) (1,2) 3 in
  let negative_one = #{ i = -3; ff' = #{ f = #1.33 ; f' = 0.67 } } in
  let ten = #{ i = -1; ff' = #{ f = #13.2 ; f' = -2.2 } } in
  let result =
    o#f6_m3 (-2) e
      (fun[@inline never] i m1 m2 n m3 ->
         Float.of_int (add3 i n) *. (t_sum m1 +. t_sum m2 +. t_sum m3))
      negative_one (-12,4) ten
  in
  print_float "Test 6, -46.88" result

let _ = test6 ()

(**************************************)
(* Test 7: letop with unboxed records *)

let ( let* ) x f =
  let one = #{ i = 0; ff' = #{ f = #1.0 ; f' = 0.0 } } in
  f Float_u.(add_t x one)

let _ =
  let* pi_plus_one = #{ i = 1; ff' = #{ f = #2.0 ; f' = 0.14 } } in
  print_t_sum "Test 7, 4.14" pi_plus_one

let ( let* ) x (f : _ -> t) =
  let one = #{ i = 0; ff' = #{ f = #1.0 ; f' = 0.0 } } in
  add_t one (f x)
let ( and* ) x y = (x, t_sum y)
let _ =
  let one = #{ i = 0; ff' = #{ f = #1.0 ; f' = 0.0 } } in
  let e = #{ i = 1; ff' = #{ f = #0.1 ; f' = 1.62 } } in
  let result =
    let* x = 42
    and* y = one
    and* z = e in
    #{ i = 42; ff' = #{ f = Float_u.of_float y ; f' = z } }
  in
  print_t_sum "Test 7, 46.72" result

(**************************************************************)
(* Test 8 ommitted, reordering only applies to unboxed tuples *)
(* Partial patterns tested below. *)

(**********************************)
(* Test 9: Continuations / @local *)

type zy = #{ z : float ; y : int }
type zyxw = #{ zy : zy ; x : float# ; w : string }
type yz = #{ y_ : int ; z_ : float }
type wxyz = #{ w : string ; x : float# ; yz : yz }

let print4 prefix #{ zy = #{ z; y }; x; w } =
  Printf.printf "%s: [%.1f %d %.1f %s]\n" prefix z y (Float_u.to_float x) w

let _ =
  let[@local] swap #{ w; x; yz = #{ y_; z_ }} = #{ zy = #{ z=z_; y=y_}; x; w} in
  let[@inline never] g i p1 p2 =
    let z =
      if i < 0 then
        swap p1
      else if i = 0 then
        swap p2
      else
        swap #{ w = "hi"; x = #42.0; yz = #{ y_ = 84; z_ = 3.0} }
    in z
  in
  print4 "Test 9, [3.0 2 1.0 a]"
    (g (-1) #{ w = "a"; x = #1.0; yz = #{ y_ = 2; z_ = 3.0 } }
            #{ w = "a"; x = #4.0; yz = #{ y_ = 5; z_ = 6.0 } });

  let[@local] swap #{ w; x; yz = #{ y_; z_ }} = #{ zy = #{ z=z_; y=y_}; x; w} in
  let[@inline never] g i p1 p2 =
    let z =
      if i < 0 then
        swap p1
      else if i = 0 then
        swap p2
      else
        swap #{ w = "hi"; x = #42.0; yz = #{ y_ = 84; z_ = 3.0} }
    in z
  in
  print4 "Test 9, [6.0 5 4.0 b]"
    (g 0 #{ w = "a"; x = #1.0; yz = #{ y_ = 2; z_ = 3.0 } }
         #{ w = "b"; x = #4.0; yz = #{ y_ = 5; z_ = 6.0 } });

  let[@local] swap #{ w; x; yz = #{ y_; z_ }} = #{ zy = #{ z=z_; y=y_}; x; w} in
  let[@inline never] g i p1 p2 =
    let z =
      if i < 0 then
        swap p1
      else if i = 0 then
        swap p2
      else
        swap #{ w = "hi"; x = #42.0; yz = #{ y_ = 84; z_ = 3.0} }
    in z
  in
  print4 "Test 9, [3.0 84 42.0 hi]"
    (g 1 #{ w = "a"; x = #1.0; yz = #{ y_ = 2; z_ = 3.0 } }
         #{ w = "b"; x = #4.0; yz = #{ y_ = 5; z_ = 6.0 } })

(**************************)
(* Test 10: Loopification *)

type xy = #{ x : float ; y : int }
type wxyz_ = #{ w : float# ; xy : xy ; z : int }

let print4 prefix #{ w; xy = #{x;y}; z } =
  Printf.printf "%s: [%.1f %.1f %d %d]\n" prefix
    (Float_u.to_float w) x y z

let[@loop] rec fib n (#{ w; xy = #{x;y}; z } as p) =
  let w = Float_u.to_float w in
  if Float.compare w (Float.of_int n) > 0 then p else
    let next = Float_u.of_float (w +. x) in
    fib n #{ w = next; xy = #{ x = w; y = Float.to_int x}; z = y}

let _ =
  print4 "Test 10, #(1.0, #(0.0, 0), 0)"
    (fib 0 #{ w = #1.0; xy = #{ x = 0.0; y = 0 }; z = 0 });
  print4 "Test 10, #(5.0, #(3.0, 2), 1))"
    (fib 4 #{ w = #1.0; xy = #{ x = 0.0; y = 0 }; z = 0 });
  print4 "Test 10, #(144.0, #(89.0, 55), 34)"
    (fib 100 #{ w = #1.0; xy = #{ x = 0.0; y = 0}; z = 0});

(****************************************************************************)
(* Test 11: Basic tests of functional updates, projection, partial patterns *)

type t_ = #{ i : int ; j : int }

let add t = t.#i + t.#j
let () =
  let t = #{i = 200; j = 300} in
  let res = add t in
  Printf.printf "Test 11: %d\n" res

let copy_i_to_j #{ i ; j } = #{ i; j = i }
let () =
  let t = #{i = 1000; j = 2} in
  let res = add (copy_i_to_j t) in
  Printf.printf "Test 11: %d\n" res

let copy_i_to_j r = #{ r with j = r.#i }
let () =
  let t = #{i = 1000; j = 2} in
  let res = add (copy_i_to_j t) in
  Printf.printf "Test 11: %d\n" res

type r = #{ is: #(int * int) ; i : int }
let add r =
  let #(x, y) = r.#is in
  let z = r.#i in
  let #{ is = #(x2, y2) ; i = z2 } = r in
  assert (x == x2 && y == y2 && z == z2);
  let #{ is ; _ } = r in
  let #(x3, y3) = is in
  assert (x == x3 && y == y3);
  x + y + z
let () =
  let t = #{ is = #(1, 10); i = 100} in
  let res = add t in
  Printf.printf "Test 11: %d\n" res

let () =
  let t = #{ is = #(1, 10); i = 100} in
  let res = add #{ t with is = #(2, 20) } in
  Printf.printf "Test 11: %d\n" res


(******************************************************************************)
(* Test 12: unboxed records in closures using projection and partial patterns *)

(* This test is adapted from test 3. *)

(* [go]'s closure should have an unboxed record with an [int] (immediate), a
   [float#] (float64) and a [float array] (value). *)
let[@inline never] f3 (bounds : int_floatu) (steps_init :floatarray_floatu) () =
  let[@inline never] rec go k =
    let n = bounds.#i in
    let (#{ f = m; _ } : int_floatu) = bounds in
    let init = steps_init.#f in
    if k = n
    then init
    else begin
      let acc = go (k + 1) in
      let steps = steps_init.#fa in
      steps.(k) <- Float_u.to_float acc;
      Float_u.add m acc
    end
  in
  go 0


(* many args - odd args are floats, even args are unboxed records *)
let[@inline_never] f3_manyargs x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 steps () =
  let (#{ x = start_k; y = end_k } : ints) = x0 in
  let[@inline never] rec go k =
    if k = end_k
    then 0.0
    else begin
      let #{ i = x2_1; ff' = #{ f = x2_2; f' = x2_3 } } = x2 in
      let #{ i = x4_1; ff' = #{ f = x4_2; f' = x4_3 } } = x4 in
      let #{ i = x6_1; ff' = #{ f = x6_2; f' = x6_3 } } = x6 in
      let #{ i = x8_1; ff' = #{ f = x8_2; f' = x8_3 } } = x8 in
      let sum =
        Float.of_int x2_1 +. Float_u.to_float x2_2 +. x2_3 +.
        Float.of_int x4_1 +. Float_u.to_float x4_2 +. x4_3 +.
        Float.of_int x6_1 +. Float_u.to_float x6_2 +. x6_3 +.
        Float.of_int x8_1 +. Float_u.to_float x8_2 +. x8_3
      in
      let acc = go (k + 1) in
      steps.(k) <- acc;
      acc +. ((x1 +. x3 +. x5 +. x7 +. x9) *. sum)
    end
  in
  go start_k

let test12 () =
  (* Test f3 *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let five_pi = f3 #{ i = 5; f = #3.14} #{ fa = steps ; f = #0.0 } in
  print_floatu "Test 12, 5 * pi: " (five_pi ());
  Array.iteri (Printf.printf "  Test 12, step %d: %.2f\n") steps;

  (* Test f3_manyargs

          (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 50.86
      3 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 152.58
      6 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 306.16
      9 * (3.14 + 2.72 + 1.62 + 1.41 + 42.0) = 457.74

    ( but we expect some floating point error )
  *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let x1 = 3.14 in
  let x3 = 2.72 in
  let x5 = 1.62 in
  let x7 = 1.41 in
  let x9 = 42.0 in

  (* these sum to 3 *)
  let x2 = #{ i = 7; ff' = #{ f = #40.0 ; f' = 2.0 } } in
  let x4 = #{ #{ x2 with i = -23 } with ff' = #{ f = #100.0 ; f' = 9.0 } } in
  let x6 = #{
    #{ (Sys.opaque_identity x4) with i = -242 }
    with ff' = #{ f = #5.5 ; f' = 84.5 }
  } in
  let x8 =
    #{ i = -2; ff' = #{ (Sys.opaque_identity x2.#ff') with f = #20.0 } } in
  let f3_manyargs =
    f3_manyargs #{ x = 4; y = 8} x1 x2 x3 x4 x5 x6 x7 x8 x9 steps in
  print_float "Test 3, 610.68: " (f3_manyargs ());
  Array.iteri (Printf.printf "  Test 12, step %d: %.2f\n") steps

let _ = test12 ()
