(* TEST
 reference = "${test_source_directory}/block_indices.reference";
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "";
   compiler_reference = "${test_source_directory}/block_indices_disabled.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 } {
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 } {
   flags = "-extension layouts_alpha -Oclassic";
   native;
 } {
   flags = "-extension layouts_alpha -O3";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

let _fail_when_no_extensions () = (.contents)

external[@layout_poly] read_idx_imm : 'a ('b : any). 'a -> ('a, 'b) idx_imm -> 'b = "%unsafe_read_idx"
external[@layout_poly] read_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%unsafe_read_idx"
external[@layout_poly] write_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%unsafe_write_idx"
external[@layout_poly] makearray_dynamic :
  ('a : any_non_null). len:int -> init:('a [@local_opt]) -> ('a array [@local_opt]) =
  "%makearray_dynamic"
external[@layout_poly] get :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a =
  "%array_safe_get"
external[@layout_poly] set :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit =
  "%array_safe_set"

(*******************************************************)
(* Reads and writes for various record representations *)

type r = { s : string; mutable f : float }

let () =
  print_endline "Boxed record";
  let r = { s = "foo"; f = 1.0 } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" x;
  write_idx_mut r (.f) 2.0;
  Printf.printf "%f\n" r.f;
  print_newline ()

type float_record = { f' : float; mutable f : float }

let () =
  print_endline "Float record";
  let r = { f' = -100.0; f = 1.0 } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" (Float_u.to_float x);
  write_idx_mut r (.f) #2.0;
  Printf.printf "%f\n" r.f;
  print_newline ()

type mixed_record = { i : int; mutable u : float#; s : string }

let () =
  print_endline "Mixed block record";
  let r = { i = -100; u = #1.0; s = "foo" } in
  let x = read_idx_mut r (.u) in
  Printf.printf "%f\n" (Float_u.to_float x);
  write_idx_mut r (.u) #2.0;
  Printf.printf "%f\n" (Float_u.to_float r.u);
  print_newline ()

type mixed_float32_record = { s : string; mutable f : float32# }

let () =
  print_endline "Mixed block record (float32# field)";
  let r = { s = "foo"; f = #1.0s } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float x));
  write_idx_mut r (.f) #2.0s;
  Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float r.f));
  print_newline ()

type nested_record = { f : float#; mutable r : r# }

let () =
  print_endline "Nested mixed block record";
  let r = { f = -#100.0; r = #{ s = "foo"; f = 1.0 } } in
  let x = read_idx_mut r (.r.#f) in
  Printf.printf "%f\n" x;
  write_idx_mut r (.r.#f) 2.0;
  Printf.printf "%f\n" r.r.#f;
  print_newline ()

type mixed_float_record = { mutable f : float; mutable u : float# }

let () =
  print_endline "Mixed float record (float field)";
  let r = { f = 1.0; u = -#100.0 } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" (Float_u.to_float x);
  write_idx_mut r (.f) #2.0;
  Printf.printf "%f\n" r.f;
  print_newline ()

let () =
  print_endline "Mixed float record (float# field)";
  let r = { f = -100.0; u = #1.0 } in
  let x = read_idx_mut r (.u) in
  Printf.printf "%f\n" (Float_u.to_float x);
  write_idx_mut r (.u) #2.0;
  Printf.printf "%f\n" (Float_u.to_float r.u);
  print_newline ()

type mixed_int32_record = { j : int32#; mutable i : int32# }

let () =
  print_endline "Mixed block record (int32# field)";
  let r = { j = -#100l; i = #1l } in
  let x = read_idx_mut r (.i) in
  Printf.printf "%d\n" (Int32_u.to_int x);
  write_idx_mut r (.i) #2l;
  Printf.printf "%d\n" (Int32_u.to_int r.i);
  print_newline ()

type mixed_int64_record = { j : int64#; mutable i : int64# }

let () =
  print_endline "Mixed block record (int64# field)";
  let r = { j = -#100L; i = #1L } in
  let x = read_idx_mut r (.i) in
  Printf.printf "%d\n" (Int64_u.to_int x);
  write_idx_mut r (.i) #2L;
  Printf.printf "%d\n" (Int64_u.to_int r.i);
  print_newline ()

type mixed_nativeint_record = { j : nativeint#; mutable i : nativeint# }

let () =
  print_endline "Mixed block record (nativeint# field)";
  let r = { j = -#100n; i = #1n } in
  let x = read_idx_mut r (.i) in
  Printf.printf "%d\n" (Nativeint_u.to_int x);
  write_idx_mut r (.i) #2n;
  Printf.printf "%d\n" (Nativeint_u.to_int r.i);
  print_newline ()

(***************************************)
(* Nested product update and deepening *)

type a = { s : string; i : int64# }
type b = { i : int64#; a : a#; s : string }
type c = { mutable b : b#; s : string }

let print_t_b t =
  let #{ i = bi; a = #{ s; i }; s = bs } = read_idx_mut t (.b) in
  Printf.printf "{ %s { %s %s } %s }\n"
    (Int.to_string (Int64_u.to_int bi))
    s
    (Int.to_string (Int64_u.to_int i))
    bs

let () =
  print_endline
    "Nested product update and deepen mixed product to mixed product";
  let t = { b = #{ i = #1L; a = #{ s = "a"; i = #2L }; s = "b" }; s = "c" } in
  print_t_b t;
  let idx = (.b) in
  write_idx_mut t idx
    #{ i = #10L; a = #{ s = "aa"; i = #20L }; s = "bb"};
  print_t_b t;
  let deeper_idx = (.idx_mut(idx).#a) in
  write_idx_mut t deeper_idx #{ s = "aaa"; i = #200L };
  print_t_b t;
  print_newline ();
  ()

type is = #{ i : int; j : int }
type fs = #{ f : float#; g : float#; }
type inner = #{ fs : fs; is : is }
type outer = { mutable inner : inner; s : string }

let print_outer prefix { inner = #{ fs = #{ f; g }; is = #{ i; j } }; s } =
  Printf.printf "%s{ { f = %f; g = %f }; { i = %d; j = %d } } %s\n"
    prefix (Float_u.to_float f) (Float_u.to_float g) i j s

let () =
  print_endline "Deepen mixed product to values";
  let r =
    { inner = #{ fs = #{ f = #1.0; g = #11.0 }; is = #{ i = 1; j = 11 } }
    ; s = "foo" }
  in
  print_outer "initial: " r;
  let idx_is = (.idx_mut((.inner)).#is) in
  let #{ i; j } = read_idx_mut r idx_is in
  Printf.printf "will incr: %d %d\n" i j;
  write_idx_mut r (.idx_mut((.inner)).#is) #{ i = 2; j = 22 };
  print_outer "" r;
  print_endline "\nDeepen mixed product to flats (continues above)";
  let idx_fs = (.idx_mut((.inner)).#fs) in
  let #{ f; g } = read_idx_mut r idx_fs in
  Printf.printf "will incr: %f %f\n" (Float_u.to_float f) (Float_u.to_float g);
  write_idx_mut r (.idx_mut((.inner)).#fs) #{ f = #2.0; g = #22.0 };
  print_outer "" r;
  print_endline "\nDeepen values to values (continues above)";
  let idx_j = (.idx_mut(idx_is).#j) in
  let j = read_idx_mut r idx_j in
  Printf.printf "will incr: %d\n" j;
  write_idx_mut r (.idx_mut(idx_is).#j) 33;
  print_outer "" r;
  print_endline "\nDeepen flats to flats (continues above)";
  let idx_g = (.idx_mut(idx_fs).#g) in
  let g = read_idx_mut r idx_g in
  Printf.printf "will incr: %f\n" (Float_u.to_float g);
  write_idx_mut r (.idx_mut(idx_fs).#g) #33.0;
  print_outer "" r;
  print_newline ()

(* let () =
 *   print_endline "string array";
 *   let a = makearray_dynamic ~len:10 ~init:#0.s in
 *   for i = 0 to 9 do
 *     set a i (Float32_u.of_float (Float_u.of_int i))
 *   done;
 *   for i = 0 to 9 do
 *     let idx : (_, float32#) idx_mut = (.(i)) in
 *     let x = read_idx_mut a idx in
 *     Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float x))
 *   done;
 *   print_endline "" *)

let () =
  print_endline "float32# array";
  let a = makearray_dynamic ~len:10 ~init:#0.s in
  for i = 0 to 9 do
    set a i (Float32_u.of_float (Float_u.of_int i))
  done;
  for i = 0 to 9 do
    let idx : (_, float32#) idx_mut = (.(i)) in
    let x = read_idx_mut a idx in
    Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float x))
  done;
  print_endline ""
