(* CR-someday: see whether the `-4` can be dropped. *)
[@@@ocaml.warning "+a-29-40-41-42-4"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module U = Peephole_utils

(** Logical condition for simplifying the following case:
    {|
    mov x, y
    mov y, x
    |}

    In this case, the second instruction should be removed *)

let remove_useless_mov (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match U.get_cells cell 2 with
  | [fst; snd] -> (
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    match fst_val.desc with
    | Op (Move | Spill | Reload) -> (
      let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
      match snd_val.desc with
      | Op (Move | Spill | Reload) ->
        let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
        if U.are_equal_regs fst_src snd_dst && U.are_equal_regs fst_dst snd_src
        then (
          DLL.delete_curr snd;
          Some (U.prev_at_most U.go_back_const fst))
        else None
      | _ -> None)
    | _ -> None)
  | _ -> None

(** Logical condition for simplifying the following case:
  {|
    <op 1> const1, r
    <op 2> const2, r
  |}

  to:
  {|
    <op 1> (const1 <op 2> const2), r
  |}

   Where <op 1> and <op 2> can be any two binary operators that are associative and commutative
   and const1 and const2 are immediate values. *)

let are_compatible op1 op2 imm1 imm2 =
  match (op1 : Mach.integer_operation), (op2 : Mach.integer_operation) with
  (* Folding two bitwise operations such as (AND, OR, XOR) should never produce
     an overflow so we assert this conditon. *)
  | Mach.Iand, Mach.Iand ->
    assert (U.amd64_imm32_within_bounds imm1 imm2 ( land ));
    Some (Mach.Iand, imm1 land imm2)
  | Ior, Ior ->
    assert (U.amd64_imm32_within_bounds imm1 imm2 ( lor ));
    Some (Mach.Ior, imm1 lor imm2)
  | Ixor, Ixor ->
    assert (U.amd64_imm32_within_bounds imm1 imm2 ( lxor ));
    Some (Mach.Ixor, imm1 lxor imm2)
  (* For the following three cases we have the issue that in some situations,
     one or both immediate values could be out of bounds, but the result might
     be within bounds (e.g. imm1 = -4 and imm2 = 65, their sum being 61). This
     should not happen at all since the immediate values should always be within
     the bounds [0, Sys.int_size]. *)
  | Ilsl, Ilsl ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then (
      U.bitwise_shift_assert imm1 imm2;
      Some (Mach.Ilsl, imm1 + imm2))
    else None
  | Ilsr, Ilsr ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then (
      U.bitwise_shift_assert imm1 imm2;
      Some (Mach.Ilsr, imm1 + imm2))
    else None
  | Iasr, Iasr ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then (
      U.bitwise_shift_assert imm1 imm2;
      Some (Mach.Iasr, imm1 + imm2))
    else None
  (* for the amd64 instruction set the `ADD` `SUB` `MUL` opperations take at
     most an imm32 as the second argument, so we need to check for overflows on
     32-bit signed ints. *)
  (* CR-someday gtulba-lecu: This condition is architecture specific and should
     either live in amd64 specific code or this module should contain
     information about the architecture target. *)
  | Iadd, Iadd ->
    if Misc.no_overflow_add imm1 imm2
       && U.amd64_imm32_within_bounds imm1 imm2 ( + )
    then Some (Mach.Iadd, imm1 + imm2)
    else None
  | Iadd, Isub ->
    if imm1 >= imm2
    then
      if Misc.no_overflow_sub imm1 imm2
         && U.amd64_imm32_within_bounds imm1 imm2 ( - )
      then Some (Mach.Iadd, imm1 - imm2)
      else None
    else if Misc.no_overflow_sub imm2 imm1
            && U.amd64_imm32_within_bounds imm2 imm1 ( - )
    then Some (Mach.Isub, imm2 - imm1)
    else None
  | Isub, Isub ->
    if Misc.no_overflow_add imm1 imm2
       && U.amd64_imm32_within_bounds imm1 imm2 ( + )
    then Some (Mach.Isub, imm1 + imm2)
    else None
  | Isub, Iadd ->
    if imm1 >= imm2
    then
      if Misc.no_overflow_sub imm1 imm2
         && U.amd64_imm32_within_bounds imm1 imm2 ( - )
      then Some (Mach.Isub, imm1 - imm2)
      else None
    else if Misc.no_overflow_sub imm2 imm1
            && U.amd64_imm32_within_bounds imm2 imm1 ( - )
    then Some (Mach.Iadd, imm2 - imm1)
    else None
  | Ilsl, Imul ->
    if imm1 >= 0 && imm1 < 31
       && Misc.no_overflow_mul (1 lsl imm1) imm2
       && U.amd64_imm32_within_bounds (1 lsl imm1) imm2 ( * )
    then Some (Mach.Imul, (1 lsl imm1) * imm2)
    else None
  | Imul, Ilsl ->
    if imm2 >= 0 && imm2 < 31
       && Misc.no_overflow_mul imm1 (1 lsl imm2)
       && U.amd64_imm32_within_bounds imm1 (1 lsl imm2) ( * )
    then Some (Mach.Imul, imm1 * (1 lsl imm2))
    else None
  | Imul, Imul ->
    if Misc.no_overflow_mul imm1 imm2
       && U.amd64_imm32_within_bounds imm1 imm2 ( * )
    then Some (Mach.Imul, imm1 * imm2)
    else None
  (* CR-soon gtulba-lecu: check this last case | Imod, Imod -> if imm1 mod imm2
     = 0 then Some (Mach.Imod, imm2) else None

     The integer modulo imm2 group is a subgroup of the integer modulo imm1 iff
     imm2 divides imm1

     This is because the operations in the groups are addition modulo n and m
     respectively. If n divides m, then every result of the operation (addition)
     in the n group will also be a legal result in the m group, which is
     essentially the definition of a subgroup. If n does not divide m, there
     will be some results in the n group that are not acceptable in the m
     group. *)
  | _ -> None

let fold_intop_imm (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match U.get_cells cell 2 with
  | [fst; snd] ->
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    (* The following check does the following: 1. Ensures that both instructions
       use the same source register; 2. Ensures that both instructions output
       the result to the source register, this is redundant for amd64 since
       there are no instructions that invalidate this condition. *)
    (* CR-someday gtulba-lecu: This condition is architecture specific and
       should either live in amd64 specific code or this module should contain
       information about the architecture target. *)
    if Array.length fst_val.arg = 1
       && Array.length snd_val.arg = 1
       && Array.length fst_val.res = 1
       && Array.length snd_val.res = 1
       && U.are_equal_regs
            (Array.unsafe_get fst_val.arg 0)
            (Array.unsafe_get snd_val.arg 0)
       && U.are_equal_regs
            (Array.unsafe_get fst_val.arg 0)
            (Array.unsafe_get fst_val.res 0)
       && U.are_equal_regs
            (Array.unsafe_get snd_val.arg 0)
            (Array.unsafe_get snd_val.res 0)
    then
      match fst_val.desc, snd_val.desc with
      | Op (Intop_imm (op1, imm1)), Op (Intop_imm (op2, imm2)) -> (
        match are_compatible op1 op2 imm1 imm2 with
        | Some (op, imm) ->
          let new_cell =
            DLL.insert_and_return_before fst
              { fst_val with desc = Cfg.Op (Intop_imm (op, imm)) }
          in
          DLL.delete_curr fst;
          DLL.delete_curr snd;
          Some ((U.prev_at_most U.go_back_const) new_cell)
        | _ -> None)
      | _ -> None
    else None
  | _ -> None

let apply cell =
  match remove_useless_mov cell with
  | None -> ( match fold_intop_imm cell with None -> None | res -> res)
  | res -> res
