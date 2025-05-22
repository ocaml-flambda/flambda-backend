[@@@ocaml.warning "+a-40-41-42"]

module T = struct
  type t =
    | GPR
    | SIMD

  let all =
    [GPR; (* general-purpose registers *) SIMD (* xmm/ymm/zmm registers *)]

  let first_available_register : t -> int = function GPR -> 0 | SIMD -> 100

  let num_available_registers : t -> int = function
    | GPR -> if Config.with_frame_pointers then 12 else 13
    | SIMD -> 16

  let num_registers : t -> int = function GPR -> 13 | SIMD -> 16

  (** See "System V Application Binary Interface, AMD64 Architecture Processor
    Supplement" (www.x86-64.org/documentation/abi.pdf) page 57, fig. 3.36. *)
  let gpr_dwarf_reg_numbers = [| 0; 3; 5; 4; 1; 2; 8; 9; 12; 13; 10; 11; 6 |]

  let simd_dwarf_reg_numbers =
    [| 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32 |]

  let dwarf_register_numbers = function
    | GPR -> gpr_dwarf_reg_numbers
    | SIMD -> simd_dwarf_reg_numbers

  let[@ocamlformat "disable"] gpr_name =
    match Config.ccomp_type with
    | "msvc" ->
      [| "rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";
         "r12"; "r13"; "r10"; "r11"; "rbp" |]
    | _ ->
      [| "%rax"; "%rbx"; "%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9";
         "%r12"; "%r13"; "%r10"; "%r11"; "%rbp" |]

  let[@ocamlformat "disable"] xmm_name =
    match Config.ccomp_type with
    | "msvc" ->
      [| "xmm0"; "xmm1"; "xmm2"; "xmm3"; "xmm4"; "xmm5"; "xmm6"; "xmm7";
         "xmm8"; "xmm9"; "xmm10"; "xmm11";
         "xmm12"; "xmm13"; "xmm14"; "xmm15" |]
    | _ ->
      [| "%xmm0"; "%xmm1"; "%xmm2"; "%xmm3"; "%xmm4"; "%xmm5"; "%xmm6"; "%xmm7";
         "%xmm8"; "%xmm9"; "%xmm10"; "%xmm11";
         "%xmm12"; "%xmm13"; "%xmm14"; "%xmm15" |]

  let[@ocamlformat "disable"] ymm_name =
    match Config.ccomp_type with
    | "msvc" ->
      [| "ymm0"; "ymm1"; "ymm2"; "ymm3"; "ymm4"; "ymm5"; "ymm6"; "ymm7";
         "ymm8"; "ymm9"; "ymm10"; "ymm11";
         "ymm12"; "ymm13"; "ymm14"; "ymm15" |]
    | _ ->
      [| "%ymm0"; "%ymm1"; "%ymm2"; "%ymm3"; "%ymm4"; "%ymm5"; "%ymm6"; "%ymm7";
         "%ymm8"; "%ymm9"; "%ymm10"; "%ymm11";
         "%ymm12"; "%ymm13"; "%ymm14"; "%ymm15" |]

  let[@ocamlformat "disable"] zmm_name =
    match Config.ccomp_type with
    | "msvc" ->
      [| "zmm0"; "zmm1"; "zmm2"; "zmm3"; "zmm4"; "zmm5"; "zmm6"; "zmm7";
         "zmm8"; "zmm9"; "zmm10"; "zmm11";
         "zmm12"; "zmm13"; "zmm14"; "zmm15" |]
    | _ ->
      [| "%zmm0"; "%zmm1"; "%zmm2"; "%zmm3"; "%zmm4"; "%zmm5"; "%zmm6"; "%zmm7";
         "%zmm8"; "%zmm9"; "%zmm10"; "%zmm11";
         "%zmm12"; "%zmm13"; "%zmm14"; "%zmm15" |]

  let register_name ty r =
    (* If the ID doesn't match the type, the array access will raise. *)
    match (ty : Cmm.machtype_component) with
    | Int | Addr | Val -> gpr_name.(r - first_available_register GPR)
    | Float | Float32 | Vec128 | Valx2 ->
      xmm_name.(r - first_available_register SIMD)
    | Vec256 -> ymm_name.(r - first_available_register SIMD)
    | Vec512 -> zmm_name.(r - first_available_register SIMD)

  let of_machtype : Cmm.machtype_component -> t = function
    | Val | Int | Addr -> GPR
    | Float | Float32 | Vec128 | Vec256 | Vec512 | Valx2 -> SIMD

  let gc_regs_offset (typ : Cmm.machtype_component) (reg_index : int) =
    (* Given register with type [typ] and index [reg_index], return the offset
       (the number of [value] slots, not their size in bytes) of the register
       from the [gc_regs] pointer during GC at runtime. Keep in sync with
       [amd64.S]. *)
    let reg_class = of_machtype typ in
    let index = reg_index - first_available_register reg_class in
    match reg_class with
    | GPR -> index
    | SIMD ->
      let slot_size_in_vals = 2 in
      assert (Arch.size_vec128 / Arch.size_int = slot_size_in_vals);
      if Config.runtime5
      then
        (* xmm slots are above regular slots based at [gc_regs_bucket] *)
        let num_regular_slots =
          (* rbp is always spilled even without frame pointers *)
          13
        in
        num_regular_slots + (index * slot_size_in_vals)
      else
        (* xmm slots are below [gc_regs] pointer *)
        let num_xmm_slots = 16 in
        let offset = Int.neg (num_xmm_slots * slot_size_in_vals) in
        offset + (index * slot_size_in_vals)

  let equal : t -> t -> bool =
   fun left right ->
    match left, right with
    | GPR, GPR -> true
    | SIMD, SIMD -> true
    | (GPR | SIMD), _ -> false

  let hash : t -> int = function GPR -> 0 | SIMD -> 1

  let print : Format.formatter -> t -> unit =
   fun ppf reg_class ->
    Format.fprintf ppf "%s"
      (match reg_class with GPR -> "GPR" | SIMD -> "SIMD")
end

include T
module Tbl = Reg_class_utils.Make_tbl (T)
