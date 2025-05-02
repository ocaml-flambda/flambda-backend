(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Parses [amd64.csv] and outputs [Amd64_simd_defs.instr] definitions.

   [amd64.csv] was retrieved from https://github.com/GregoryComer/x86-csv
   (commit c638bbbaa17f0c81abaa7e84a968335c985542fa) and manually modified.
   Instruction data was originally derived from the Intel SDM Vol 2. *)

open Amd64_simd_defs
open Printf

type id = Dummy

exception Unsupported

let all_mnemonics = Hashtbl.create 1024

let all_instructions = Hashtbl.create 1024

let register instr =
  (match Hashtbl.find_opt all_mnemonics instr.mnemonic with
  | Some i -> Hashtbl.replace all_mnemonics instr.mnemonic (i + 1)
  | None -> Hashtbl.add all_mnemonics instr.mnemonic 1);
  Hashtbl.add all_instructions instr ()

let first_word str =
  match String.split_on_char ' ' str with
  | [fst] -> fst, ""
  | fst :: rest -> fst, String.concat " " rest
  | _ -> failwith str

let fail name part = failwith (name ^ " (" ^ part ^ ")")

let parse in_ =
  let rec one quote acc =
    match In_channel.input_char in_ with
    | None -> `Eof acc
    | Some '\n' -> `Eol acc
    | Some '"' when quote -> (
      match In_channel.input_char in_ with
      | None -> `Eof acc
      | Some ',' -> `One acc
      | Some '\n' -> `Eol acc
      | _ -> assert false)
    | Some ',' when not quote -> `One acc
    | Some '"' -> one true acc
    | Some c -> one quote (acc ^ String.make 1 c)
  in
  let rec line acc =
    match one false "" with
    | `Eof one -> `Eof (List.rev (one :: acc))
    | `Eol one -> `Line (List.rev (one :: acc))
    | `One one -> line (one :: acc)
  in
  let rec csv acc =
    match line [] with
    | `Line line -> csv (line :: acc)
    | `Eof line -> List.rev (line :: acc)
  in
  csv []

let rec parse_args mnemonic acc encs args imm res =
  let set_imm () =
    if !imm then failwith mnemonic;
    imm := true;
    None
  in
  let set_res loc enc =
    (* MULX has two results *)
    if not (!res = First_arg) then raise Unsupported;
    res := Res { loc; enc }
  in
  match args, encs with
  | [], _ -> List.rev acc
  | "" :: args, encs -> parse_args mnemonic acc encs args imm res
  | arg :: args, enc :: encs -> (
    let loc : loc option =
      match String.trim arg with
      | "<RAX>" -> Some (Pin RAX)
      | "<RCX>" -> Some (Pin RCX)
      | "<RDX>" -> Some (Pin RDX)
      | "<XMM0>" -> Some (Pin XMM0)
      | "imm8" -> set_imm ()
      | "r8/m8" | "r/m8" -> Some (Temp [| R8; M8 |])
      | "r16/m16" | "r/m16" -> Some (Temp [| R16; M16 |])
      | "r32/m32" | "r/m32" -> Some (Temp [| R32; M32 |])
      | "r64/m64" | "r/m64" -> Some (Temp [| R64; M64 |])
      | "r32/m8" -> Some (Temp [| R32; M8 |])
      | "r32/m16" -> Some (Temp [| R32; M16 |])
      | "r8" -> Some (Temp [| R8 |])
      | "r16" -> Some (Temp [| R16 |])
      | "r32" | "r32a" | "r32b" -> Some (Temp [| R32 |])
      | "r64" | "r64a" | "r64b" | "reg" -> Some (Temp [| R64 |])
      | "reg/m8" -> Some (Temp [| R64; M8 |])
      | "reg/m16" -> Some (Temp [| R64; M16 |])
      | "reg/m32" -> Some (Temp [| R64; M32 |])
      | "reg/m64" -> Some (Temp [| R64; M64 |])
      | "m8" -> Some (Temp [| M8 |])
      | "m16" -> Some (Temp [| M16 |])
      | "m32" -> Some (Temp [| M32 |])
      | "m64" -> Some (Temp [| M64 |])
      | "m128" -> Some (Temp [| M128 |])
      | "mm" | "mm0" | "mm1" | "mm2" | "mm3" -> Some (Temp [| MM |])
      | "mm0/m8" | "mm1/m8" | "mm2/m8" | "mm3/m8" -> Some (Temp [| MM; M8 |])
      | "mm0/m16" | "mm1/m16" | "mm2/m16" | "mm3/m16" ->
        Some (Temp [| MM; M16 |])
      | "mm0/m32" | "mm1/m32" | "mm2/m32" | "mm3/m32" ->
        Some (Temp [| MM; M32 |])
      | "mm0/m64" | "mm1/m64" | "mm2/m64" | "mm3/m64" ->
        Some (Temp [| MM; M64 |])
      | "xmm" | "xmm0" | "xmm1" | "xmm2" | "xmm3" | "xmm4" ->
        Some (Temp [| XMM |])
      | "xmm0/m8" | "xmm1/m8" | "xmm2/m8" | "xmm3/m8" ->
        Some (Temp [| XMM; M8 |])
      | "xmm0/m16" | "xmm1/m16" | "xmm2/m16" | "xmm3/m16" ->
        Some (Temp [| XMM; M16 |])
      | "xmm0/m32" | "xmm1/m32" | "xmm2/m32" | "xmm3/m32" ->
        Some (Temp [| XMM; M32 |])
      | "xmm0/m64" | "xmm1/m64" | "xmm2/m64" | "xmm3/m64" ->
        Some (Temp [| XMM; M64 |])
      | "xmm0/m128" | "xmm1/m128" | "xmm2/m128" | "xmm3/m128" ->
        Some (Temp [| XMM; M128 |])
      (* Load/store operations are not handled *)
      | "mem" | "vm32x" | "vm64x" | "vm32y" | "vm64y" -> raise Unsupported
      (* CR-soon mslater: AVX / AVX2 *)
      | "ymm" | "ymm0" | "ymm1" | "ymm2" | "ymm3" | "ymm4" | "ymm0/m256"
      | "ymm1/m256" | "ymm2/m256" | "ymm3/m256" | "m256" ->
        raise Unsupported
      | arg -> fail mnemonic arg
    in
    let enc, rw = first_word enc in
    let enc =
      match String.trim enc with
      | "ModRM:reg" -> RM_r
      | "ModRM:r/m" -> RM_rm
      | "VEX.vvvv" -> Vex_v
      | "NA" | "<XMM0>" | "<RAX>" | "<RCX>" | "<RDX>" | "implicit" -> Implicit
      | enc when String.starts_with (String.lowercase_ascii enc) ~prefix:"imm"
        ->
        Implicit
      | enc -> fail mnemonic enc
    in
    match loc with
    | None -> parse_args mnemonic acc encs args imm res
    | Some loc -> (
      match String.trim rw with
      | "(w)" ->
        set_res loc enc;
        parse_args mnemonic acc encs args imm res
      | _ -> parse_args mnemonic ({ loc; enc } :: acc) encs args imm res))
  | _ -> failwith mnemonic

let parse_args mnemonic enc args =
  let imm = ref false in
  let res = ref First_arg in
  let args = parse_args mnemonic [] enc args imm res in
  Array.of_list args, !imm, !res

let parse_enc mnemonic enc =
  let enc = String.uppercase_ascii enc in
  let parse_opcode_rm_reg enc =
    let opcode, rest =
      let opcode, rest = first_word enc in
      match Int64.of_string_opt ("0x" ^ opcode) with
      | Some i -> Int64.to_int i, rest
      | None -> fail mnemonic enc
    in
    let rm_reg =
      let rm_reg, _ = first_word rest in
      match rm_reg with
      | "/0" -> Spec 0
      | "/1" -> Spec 1
      | "/2" -> Spec 2
      | "/3" -> Spec 3
      | "/4" -> Spec 4
      | "/5" -> Spec 5
      | "/6" -> Spec 6
      | "/7" -> Spec 7
      | "/R" | "" -> Reg
      | _ -> fail mnemonic enc
    in
    opcode, rm_reg
  in
  let parse_legacy () =
    let prefix, rest =
      let prefix, rest = first_word enc in
      match prefix with
      | "NP" -> Prx_none, rest
      | "66" -> Prx_66, rest
      | "F2" -> Prx_F2, rest
      | "F3" -> Prx_F3, rest
      | prefix -> fail mnemonic prefix
    in
    let rex, rest =
      let rex, rest' = first_word rest in
      match rex with
      | "REX" -> Rex, rest'
      | "REX.W" -> Rex_w, rest'
      | _ -> Rex_none, rest
    in
    let escape, rest =
      let escape, rest' = first_word rest in
      match escape with
      | "0F" -> (
        let escape, rest'' = first_word rest' in
        match escape with
        | "38" -> Esc_0F38, rest''
        | "3A" -> Esc_0F3A, rest''
        | _ -> Esc_0F, rest')
      | _ -> Esc_none, rest
    in
    let opcode, rm_reg = parse_opcode_rm_reg rest in
    { prefix = Legacy { prefix; escape; rex }; rm_reg; opcode }
  in
  let parse_vex () =
    let prefix, rest = first_word enc in
    let prefix =
      let comps = String.split_on_char '.' prefix |> List.tl in
      let comps =
        match comps with ("NDS" | "NDD") :: comps -> comps | _ -> comps
      in
      let vex_l, comps =
        match comps with
        | ("LZ" | "LIG" | "128") :: comps -> false, comps
        | "256" :: comps -> true, comps
        | _ -> fail mnemonic enc
      in
      let vex_p, comps =
        match comps with
        | "66" :: comps -> Prx_66, comps
        | "F2" :: comps -> Prx_F2, comps
        | "F3" :: comps -> Prx_F3, comps
        | comps -> Prx_none, comps
      in
      let vex_m, comps =
        match comps with
        | "0F" :: comps -> Vexm_0F, comps
        | "0F38" :: comps -> Vexm_0F38, comps
        | "0F3A" :: comps -> Vexm_0F3A, comps
        | _ -> fail mnemonic enc
      in
      let vex_w =
        match comps with
        | [("W0" | "WIG")] | [] -> false
        | ["W1"] -> true
        | _ -> fail mnemonic enc
      in
      Vex { vex_m; vex_w; vex_l; vex_p }
    in
    let opcode, rm_reg = parse_opcode_rm_reg rest in
    { prefix; rm_reg; opcode }
  in
  if String.starts_with enc ~prefix:"VEX" then parse_vex () else parse_legacy ()

let mangle_loc (loc : loc) =
  let width : temp -> int option = function
    | R8 | M8 -> Some 8
    | R16 | M16 -> Some 16
    | R32 | M32 -> Some 32
    | R64 | M64 -> Some 64
    | M128 -> Some 128
    | MM | XMM -> None
  in
  let short : temp -> string = function
    | R8 | R16 | R32 | R64 -> "r"
    | M8 | M16 | M32 | M64 | M128 -> "m"
    | MM -> "M"
    | XMM -> "X"
  in
  match loc with
  | Pin RAX -> "rax"
  | Pin RCX -> "rcx"
  | Pin RDX -> "rdx"
  | Pin XMM0 -> "xmm0"
  | Temp temps ->
    Array.map
      (fun temp ->
        match width temp with
        | Some width -> short temp ^ Int.to_string width
        | None -> short temp)
      temps
    |> Array.to_list |> String.concat ""

let binding instr =
  let variants = Hashtbl.find all_mnemonics instr.mnemonic in
  if variants > 1
  then
    let args =
      Array.map (fun (arg : arg) -> mangle_loc arg.loc) instr.args
      |> Array.to_list |> String.concat "_"
    in
    let res =
      match instr.res with
      | First_arg -> ""
      | Res { loc; _ } -> mangle_loc loc ^ "_"
    in
    instr.mnemonic ^ "_" ^ res ^ args
  else instr.mnemonic

let print_one instr =
  let print_temp : temp -> string = function
    | R8 -> "R8"
    | R16 -> "R16"
    | R32 -> "R32"
    | R64 -> "R64"
    | M8 -> "M8"
    | M16 -> "M16"
    | M32 -> "M32"
    | M64 -> "M64"
    | M128 -> "M128"
    | MM -> "MM"
    | XMM -> "XMM"
  in
  let print_loc : loc -> string = function
    | Pin RAX -> "Pin RAX"
    | Pin RCX -> "Pin RCX"
    | Pin RDX -> "Pin RDX"
    | Pin XMM0 -> "Pin XMM0"
    | Temp temps ->
      let temps =
        Array.map print_temp temps |> Array.to_list |> String.concat ";"
      in
      "Temp [|" ^ temps ^ "|]"
  in
  let print_arg_enc = function
    | RM_r -> "RM_r"
    | RM_rm -> "RM_rm"
    | Vex_v -> "Vex_v"
    | Implicit -> "Implicit"
  in
  let print_res : res -> string = function
    | First_arg -> "First_arg"
    | Res { loc; enc } ->
      sprintf "Res { loc = %s; enc = %s }" (print_loc loc) (print_arg_enc enc)
  in
  let print_legacy_prefix : legacy_prefix -> string = function
    | Prx_none -> "Prx_none"
    | Prx_66 -> "Prx_66"
    | Prx_F2 -> "Prx_F2"
    | Prx_F3 -> "Prx_F3"
  in
  let print_legacy_rex : legacy_rex -> string = function
    | Rex_none -> "Rex_none"
    | Rex -> "Rex"
    | Rex_w -> "Rex_w"
  in
  let print_legacy_escape : legacy_escape -> string = function
    | Esc_none -> "Esc_none"
    | Esc_0F -> "Esc_0F"
    | Esc_0F38 -> "Esc_0F38"
    | Esc_0F3A -> "Esc_0F3A"
  in
  let print_vex_map : vex_map -> string = function
    | Vexm_0F -> "Vexm_0F"
    | Vexm_0F38 -> "Vexm_0F38"
    | Vexm_0F3A -> "Vexm_0F3A"
  in
  let print_prefix : prefix -> string = function
    | Legacy { prefix; rex; escape } ->
      sprintf "Legacy { prefix = %s; rex = %s; escape = %s }"
        (print_legacy_prefix prefix)
        (print_legacy_rex rex)
        (print_legacy_escape escape)
    | Vex { vex_m; vex_w; vex_l; vex_p } ->
      sprintf "Vex { vex_m = %s; vex_w = %b; vex_l = %b; vex_p = %s }"
        (print_vex_map vex_m) vex_w vex_l
        (print_legacy_prefix vex_p)
  in
  let print_rm_reg : rm_reg -> string = function
    | Reg -> "Reg"
    | Spec s -> "Spec " ^ Int.to_string s
  in
  let print_enc { prefix = p; rm_reg = r; opcode } =
    sprintf "{ prefix = %s; rm_reg = %s; opcode = %d }" (print_prefix p)
      (print_rm_reg r) opcode
  in
  let binding = binding instr in
  let constructor = String.capitalize_ascii binding in
  let args =
    Array.map
      (fun (arg : arg) ->
        sprintf "{ loc = %s; enc = %s }" (print_loc arg.loc)
          (print_arg_enc arg.enc))
      instr.args
    |> Array.to_list |> String.concat ";"
  in
  let res = print_res instr.res in
  let enc = print_enc instr.enc in
  printf
    {|
let %s = {
    id = %s
  ; args = [|%s|]
  ; res = %s
  ; imm = %b
  ; mnemonic = "%s"
  ; enc = %s
}|}
    binding constructor args res instr.imm instr.mnemonic enc

let print_all () =
  print_endline "type id = ";
  let constructors = Hashtbl.create 1024 in
  Hashtbl.iter
    (fun instr () ->
      let ctr = String.capitalize_ascii (binding instr) in
      match Hashtbl.find_opt constructors ctr with
      | Some () -> ()
      | None ->
        Hashtbl.add constructors ctr ();
        printf "  | %s\n" ctr)
    all_instructions;
  print_endline "\ntype nonrec instr = id instr";
  Hashtbl.iter (fun instr () -> print_one instr) all_instructions

let relevant_ext = function
  (* CR-soon mslater: AVX / AVX2 *)
  | "SSE" | "SSE2" | "SSE3" | "SSSE3" | "SSE4_1" | "SSE4_2" | "PCLMULQDQ"
  | "BMI2" ->
    true
  | _ -> false

let amd64 () =
  let csv = In_channel.with_open_text "amd64/amd64.csv" parse in
  let lines =
    csv
    |> List.filter_map (function
         | mnemonic :: enc :: ext :: encs -> (
           try
             match relevant_ext ext with
             | true ->
               let mnemonic, args = first_word mnemonic in
               let mnemonic = String.lowercase_ascii mnemonic in
               let args, imm, res =
                 String.split_on_char ',' args |> parse_args mnemonic encs
               in
               let enc = parse_enc mnemonic enc in
               Some { id = Dummy; args; res; imm; mnemonic; enc }
             | false -> None
           with Unsupported -> None)
         | _ -> None)
  in
  print_endline "(* Generated by tools/simdgen/simdgen.ml *)\n";
  print_endline "open Amd64_simd_defs\n";
  List.iter register lines;
  print_all ()

let arm64 () = print_endline "(* Generated by tools/simdgen/simdgen.ml *)\n"

let () =
  match Sys.argv.(1) with
  | "amd64" -> amd64 ()
  | "arm64" -> arm64 ()
  | _ -> assert false
