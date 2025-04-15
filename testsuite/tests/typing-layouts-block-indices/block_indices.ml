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

external idx_imm_to_int64 : 'base ('a: any) . ('base, 'a) idx_imm -> int64 = "%box_int64"
external idx_mut_to_int64 : _ idx_mut -> int64 = "%box_int64"

type pt = { x : int; y : int }
type line = { p : pt#; q : pt# }

type r = #{ f : float#; s : string }
type rir = { r1 : r; i : int64#; r2 : r }
(* reordered to:
   { s1 : string; s2 : string; f1 : float#; i : int64#; f2 : float# }
      ^^^^^^^^^^^               ^^^^^^^^^^^
         r1.s                      r1.f
                   ^^^^^^^^^^^                           ^^^^^^^^^^^
                      r2.s                                  r2.f
*)

type srirs = { s1 : string; rir : rir#; s2 : string }


type a = { s : string; i : int64# }
type b = { i : int64#; a : a#; s : string }
type c = { b : b#; s : string }
(*

As written, this has layout:
  ((b_int64#, (a_string, a_int64#), b_string), c_string)

Then stable two-color sort by values and flats:

   a_string b_string c_string b_i64 a_i64
b  ^^^^^^^^^^^^^^^^^          ^^^^^^^^^^^
a  ^^^^^^^^                         ^^^^^

(.b): offset 0, gap 8
(.b.#a): offset 0, gap 24
(.s): offset 16, gap 0
(.b.#s): offset 8, gap 0
(.b.#a.#s): offset 0, gap 0

(.b.#i): offset 24, gap 0
(.b.#a.#i): offset 32, gap 0

*)

let deconstruct_idx idx =
  let i = idx_imm_to_int64 idx in
  let offset = Int64.(logand (sub (shift_left one 48) one)) i |> Int64.to_int in
  let gap = Int64.shift_right i 48 |> Int64.to_int in
  ~offset, ~gap

let show_idx_imm idx =
  let ~offset, ~gap = deconstruct_idx idx in
  Printf.sprintf "(offset %d, gap %d)" offset gap

let () =
  Printf.printf "(.p.#x) %s\n" (show_idx_imm (.p.#x));
  Printf.printf "(.r1) %s\n" (show_idx_imm (.r1));
  Printf.printf "(.r2) %s\n" (show_idx_imm (.r2));
  Printf.printf "(.r1.#f) %s\n" (show_idx_imm (.r2.#f));
  let rir = (.rir) in
  Printf.printf "(.idx_imm(rir).#r2.#f) %s\n"
    (show_idx_imm (.idx_imm(rir).#r2.#f));

  let to_b = (.b) in
  let to_a1 = (.idx_imm(to_b).#a) in
  let to_a2 = (.b.#a) in
  let to_c_string1 = (.s) in
  let to_b_string1 = (.b.#s) in
  let to_b_string2 = (.idx_imm(to_b).#s) in
  let to_a_string1 = (.b.#a.#s) in
  let to_a_string2 = (.idx_imm(to_b).#a.#s) in
  let to_a_string3 = (.idx_imm(to_a1).#s) in

  let to_b_int1 = (.b.#i) in
  let to_b_int2 = (.idx_imm(to_b).#i) in

  let to_a_int1 = (.b.#a.#i) in
  let to_a_int2 = (.idx_imm(to_b).#a.#i) in
  let to_a_int3 = (.idx_imm(to_a1).#i) in

  Printf.printf "to_b:  %s\n" (show_idx_imm to_b);
  Printf.printf "to_a (1):  %s\n" (show_idx_imm to_a1);
  Printf.printf "to_a (2):  %s\n" (show_idx_imm to_a2);
  Printf.printf "to_c_string:  %s\n" (show_idx_imm to_c_string1);
  Printf.printf "to_b_string (1):  %s\n" (show_idx_imm to_b_string1);
  Printf.printf "to_b_string (2):  %s\n" (show_idx_imm to_b_string2);
  Printf.printf "to_a_string (1):  %s\n" (show_idx_imm to_a_string1);
  Printf.printf "to_a_string (2):  %s\n" (show_idx_imm to_a_string2);
  Printf.printf "to_a_string (3):  %s\n" (show_idx_imm to_a_string3);
  Printf.printf "to_b_int (1):  %s\n" (show_idx_imm to_b_int1);
  Printf.printf "to_b_int (2):  %s\n" (show_idx_imm to_b_int2);
  Printf.printf "to_a_int (1):  %s\n" (show_idx_imm to_a_int1);
  Printf.printf "to_a_int (2):  %s\n" (show_idx_imm to_a_int2);
  Printf.printf "to_a_int (3):  %s\n" (show_idx_imm to_a_int3);
  ()
