(* TEST
 reference = "${test_source_directory}/block_indices_native.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 modules = "stubs.c";
 flags = "-extension layouts_alpha";
 flambda2;
 arch_amd64;
 native;
*)

external[@layout_poly] get_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%unsafe_get_idx"
external[@layout_poly] set_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%unsafe_set_idx"

external box_int64x2 : int64x2# -> int64x2 = "%box_vec128"
external unbox_int64x2 : int64x2 -> int64x2# = "%unbox_vec128"
external interleave_low_64 : int64x2# -> int64x2# -> int64x2# = "" "caml_simd_vec128_interleave_low_64" [@@unboxed] [@@builtin]
external interleave_high_64 : int64x2# -> int64x2# -> int64x2# = "" "caml_simd_vec128_interleave_high_64" [@@unboxed] [@@builtin]
external int64x2_of_int64 : int64 -> int64x2# = "" "caml_int64x2_low_of_int64" [@@unboxed] [@@builtin]
external int64_of_int64x2 : int64x2# -> int64 = "" "caml_int64x2_low_to_int64" [@@unboxed] [@@builtin]

let int64x2_to_string a =
  let a = unbox_int64x2 a in
  let l = int64_of_int64x2 a in
  let h = int64_of_int64x2 (interleave_high_64 a a) in
  Int64.to_string h ^ ":" ^ Int64.to_string l

type mixed_int64x2_record = { j : int64x2#; mutable i : int64x2# }

let () =
  print_endline "Mixed block record (int64x2# field)";
  let v_1_11 = interleave_low_64 (int64x2_of_int64 1L) (int64x2_of_int64 11L) in
  let v_2_22 = interleave_low_64 (int64x2_of_int64 2L) (int64x2_of_int64 22L) in
  let v_3_33 = interleave_low_64 (int64x2_of_int64 3L) (int64x2_of_int64 33L) in
  let r = { j = v_3_33; i = v_1_11 } in
  let x = get_idx_mut r (.i) in
  Printf.printf "%s\n" (int64x2_to_string (box_int64x2 x));
  set_idx_mut r (.i) v_2_22;
  Printf.printf "%s\n" (int64x2_to_string (box_int64x2 r.i));
  ()
