[@@@ocaml.warning "+a-40-41-42"]

module T = struct
  type t =
    | Int64
    | Float128

  let all =
    [Int64; (* general purpose registers *) Float128 (* neon registers *)]

  let first_available_register : t -> int = function
    | Int64 -> 0
    | Float128 -> 100

  let num_available_registers : t -> int = function
    | Int64 -> 23
    | Float128 -> 32

  let num_registers : t -> int = function Int64 -> 28 | Float128 -> 32

  (* See "DWARF for the ARM 64-bit architecture (AArch64)" available from
     developer.arm.com. *)

  let[@ocamlformat "disable"] int_dwarf_reg_numbers =
   [| 0; 1; 2; 3; 4; 5; 6; 7;
      8; 9; 10; 11; 12; 13; 14; 15;
      19; 20; 21; 22; 23; 24;
      25; 26; 27; 28; 16; 17;
   |]

  let[@ocamlformat "disable"] float_dwarf_reg_numbers =
   [| 64; 65; 66; 67; 68; 69; 70; 71;
      72; 73; 74; 75; 76; 77; 78; 79;
      80; 81; 82; 83; 84; 85; 86; 87;
      88; 89; 90; 91; 92; 93; 94; 95;
    |]

  let dwarf_register_numbers reg_class =
    match reg_class with
    | Int64 -> int_dwarf_reg_numbers
    | Float128 -> float_dwarf_reg_numbers

  let[@ocamlformat "disable"] int_reg_name =
   [| "x0";  "x1";  "x2";  "x3";  "x4";  "x5";  "x6";  "x7";  (* 0 - 7 *)
      "x8";  "x9";  "x10"; "x11"; "x12"; "x13"; "x14"; "x15"; (* 8 - 15 *)
      "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25";        (* 16 - 22 *)
      "x26"; "x27"; "x28";                                    (* 23 - 25 *)
      "x16"; "x17" |]
  (* 26 - 27 *)

  let[@ocamlformat "disable"] float_reg_name =
   [| "d0";  "d1";  "d2";  "d3";  "d4";  "d5";  "d6";  "d7";
      "d8";  "d9";  "d10"; "d11"; "d12"; "d13"; "d14"; "d15";
      "d16"; "d17"; "d18"; "d19"; "d20"; "d21"; "d22"; "d23";
      "d24"; "d25"; "d26"; "d27"; "d28"; "d29"; "d30"; "d31" |]

  let[@ocamlformat "disable"] float32_reg_name =
   [| "s0";  "s1";  "s2";  "s3";  "s4";  "s5";  "s6";  "s7";
      "s8";  "s9";  "s10"; "s11"; "s12"; "s13"; "s14"; "s15";
      "s16"; "s17"; "s18"; "s19"; "s20"; "s21"; "s22"; "s23";
      "s24"; "s25"; "s26"; "s27"; "s28"; "s29"; "s30"; "s31" |]

  let[@ocamlformat "disable"] vec128_reg_name =
   [| "q0";  "q1";  "q2";  "q3";  "q4";  "q5";  "q6";  "q7";
      "q8";  "q9";  "q10"; "q11"; "q12"; "q13"; "q14"; "q15";
      "q16"; "q17"; "q18"; "q19"; "q20"; "q21"; "q22"; "q23";
      "q24"; "q25"; "q26"; "q27"; "q28"; "q29"; "q30"; "q31" |]

  let register_name ty r =
    match (ty : Cmm.machtype_component) with
    | Val | Int | Addr -> int_reg_name.(r - first_available_register Int64)
    | Float -> float_reg_name.(r - first_available_register Float128)
    | Float32 -> float32_reg_name.(r - first_available_register Float128)
    | Vec128 | Valx2 -> vec128_reg_name.(r - first_available_register Float128)
    | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"

  let of_machtype typ =
    match (typ : Cmm.machtype_component) with
    | Val | Int | Addr -> Int64
    | Float | Float32 -> Float128
    | Vec128 | Valx2 -> Float128
    | Vec256 | Vec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"

  let equal : t -> t -> bool =
   fun left right ->
    match left, right with
    | Int64, Int64 -> true
    | Float128, Float128 -> true
    | (Int64 | Float128), _ -> false

  let hash : t -> int = function Int64 -> 0 | Float128 -> 1

  let print : Format.formatter -> t -> unit =
   fun ppf reg_class ->
    Format.fprintf ppf "%s"
      (match reg_class with Int64 -> "int64" | Float128 -> "float128")
end

include T
module Tbl = Reg_class_utils.Make_tbl (T)
