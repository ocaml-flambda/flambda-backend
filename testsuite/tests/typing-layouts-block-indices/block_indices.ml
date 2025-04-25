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
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }
*)

(* CR rtjoa: why does including
   after "flambda2;" above fail?
*)

open Stdlib_stable
open Stdlib_upstream_compatible

let _fail_when_no_extensions () = (.contents)

external box_int64 : int64# -> int64 = "%box_int64"
(* external box_float : float# -> float = "%box_float" *)

let box_float = Float_u.to_float

external[@layout_poly] read_idx_imm : 'a ('b : any). 'a -> ('a, 'b) idx_imm -> 'b = "%unsafe_read_idx"
external[@layout_poly] read_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%unsafe_read_idx"
external[@layout_poly] write_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%unsafe_write_idx"

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
  ()

type float_record = { f' : float; mutable f : float }

let () =
  print_endline "Float record";
  let r = { f' = -100.0; f = 1.0 } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" (box_float x);
  write_idx_mut r (.f) #2.0;
  Printf.printf "%f\n" r.f;
  ()

type mixed_record = { i : int; mutable u : float#; s : string }

let () =
  print_endline "Mixed block record";
  let r = { i = -100; u = #1.0; s = "foo" } in
  let x = read_idx_mut r (.u) in
  Printf.printf "%f\n" (box_float x);
  write_idx_mut r (.u) #2.0;
  Printf.printf "%f\n" (box_float r.u);
  ()

type mixed_float32_record = { s : string; mutable f : float32# }

(* let () =
 *   print_endline "Mixed block record (float32# field)";
 *   let r = { s = "foo"; f = #1.0s } in
 *   let x = read_idx_mut r (.f) in
 *   Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float x));
 *   write_idx_mut r (.f) #2.0s;
 *   Printf.printf "%f\n" (Float_u.to_float (Float32_u.to_float r.f));
 *   () *)

type nested_record = { f : float#; mutable r : r# }

let () =
  print_endline "Nested mixed block record";
  let r = { f = -#100.0; r = #{ s = "foo"; f = 1.0 } } in
  let x = read_idx_mut r (.r.#f) in
  Printf.printf "%f\n" x;
  write_idx_mut r (.r.#f) 2.0;
  Printf.printf "%f\n" r.r.#f;
  ()

type mixed_float_record = { mutable f : float; mutable u : float# }

let () =
  print_endline "Mixed float record (float field)";
  let r = { f = 1.0; u = -#100.0 } in
  let x = read_idx_mut r (.f) in
  Printf.printf "%f\n" (box_float x);
  write_idx_mut r (.f) #2.0;
  Printf.printf "%f\n" r.f;
  ()

let () =
  print_endline "Mixed float record (float# field)";
  let r = { f = -100.0; u = #1.0 } in
  let x = read_idx_mut r (.u) in
  Printf.printf "%f\n" (box_float x);
  write_idx_mut r (.u) #2.0;
  Printf.printf "%f\n" (box_float r.u);
  ()

(***************************************)
(* Nested product update and deepening *)

type a = { s : string; i : int64# }
type b = { i : int64#; a : a#; s : string }
type c = { mutable b : b#; s : string }

let print_t_b t =
  let #{ i = bi; a = #{ s; i }; s = bs } = read_idx_mut t (.b) in
  Printf.printf "{ %s { %s %s } %s }\n"
    (Int64.to_string (box_int64 bi))
    s
    (Int64.to_string (box_int64 i))
    bs

let () =
  print_endline "Nested product update and deepening";
  let t = { b = #{ i = #1L; a = #{ s = "a"; i = #2L }; s = "b" }; s = "c" } in
  print_t_b t;
  let idx = (.b) in
  write_idx_mut t idx
    #{ i = #10L; a = #{ s = "aa"; i = #20L }; s = "bb"};
  print_t_b t;
  let deeper_idx = (.idx_mut(idx).#a) in
  write_idx_mut t deeper_idx #{ s = "aaa"; i = #200L };
  print_t_b t;
  ()
