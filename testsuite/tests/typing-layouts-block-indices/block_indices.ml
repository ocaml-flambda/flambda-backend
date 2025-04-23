(* TEST
 reference = "${test_source_directory}/block_indices.reference";
 include stdlib_upstream_compatible;
 flambda2;
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
let _fail_when_no_extensions () = (.contents)

external box_int64 : int64# -> int64 = "%box_int64"
external box_float : float# -> float = "%box_float"
external[@layout_poly] read_idx_imm : 'a ('b : any). 'a -> ('a, 'b) idx_imm -> 'b = "%unsafe_read_idx"
external[@layout_poly] read_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%unsafe_read_idx"
external[@layout_poly] write_idx_mut : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%unsafe_write_idx"

type a = { s : string; i : int64# }
type b = { i : int64#; a : a#; s : string }
type c = { mutable b : b#; s : string }
(*

As written, this has layout:
  ((b_int64#, (a_string, a_int64#), b_string), c_string)

Then stable two-color sort by values and flats:

   a_string b_string c_string b_i64 a_i64
b  ^^^^^^^^^^^^^^^^^          ^^^^^^^^^^^
a  ^^^^^^^^                         ^^^^^
*)

let print_t_b t =
  let #{ i = bi; a = #{ s; i }; s = bs } = read_idx_mut t (.b) in
  Printf.printf "{ %s { %s %s } %s }\n"
    (Int64.to_string (box_int64 bi))
    s
    (Int64.to_string (box_int64 i))
    bs

let () =
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


type fr = { f : float }

let () =
  let f = read_idx_imm { f = 1.0 } (.f) in
  Printf.printf "f = %f\n" (box_float f)
