[@@@ocaml.warning "+a-30-40-41-42-4"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module U = Peephole_utils

let useless_movs (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match U.get_cells cell 2 with
  | None -> None
  | Some [mov1; mov2] -> (
    let mov1_val = DLL.value mov1 in
    let mov2_val = DLL.value mov2 in
    match mov1_val.desc, mov2_val.desc with
    | Op (Move | Spill | Reload), Op (Move | Spill | Reload) ->
      if not
           (Array.length mov1_val.arg = 1
           && Array.length mov1_val.res = 1
           && Array.length mov2_val.arg = 1
           && Array.length mov2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get mov2_val.arg 0 in
        let reg4 = Array.unsafe_get mov2_val.res 0 in
        let reg1 = Array.unsafe_get mov1_val.arg 0 in
        let reg2 = Array.unsafe_get mov1_val.res 0 in
        if not (U.are_equal_regs reg1 reg4 && U.are_equal_regs reg2 reg3)
        then None
        else (
          DLL.delete_curr mov2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "useless_movs";
          Some ((U.prev_at_most slide_back) mov1))
    | _ -> None)
  | _ -> None

let fold_intop_imm_arith (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match U.get_cells cell 2 with
  | None -> None
  | Some [op1; op2] -> (
    let op1_val = DLL.value op1 in
    let op2_val = DLL.value op2 in
    match op1_val.desc, op2_val.desc with
    | Op (Intop_imm (Mach.Iadd, imm1)), Op (Intop_imm (Mach.Iadd, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && U.amd64_imm32_within_bounds imm1 imm2 ( + ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Iadd, imm1 + imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Isub, imm1)), Op (Intop_imm (Mach.Isub, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && U.amd64_imm32_within_bounds imm1 imm2 ( + ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Isub, imm1 + imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iadd, imm1)), Op (Intop_imm (Mach.Isub, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_sub imm1 imm2
             && U.amd64_imm32_within_bounds imm1 imm2 ( - ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Iadd, imm1 - imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Isub, imm1)), Op (Intop_imm (Mach.Iadd, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_sub imm2 imm1
             && U.amd64_imm32_within_bounds imm2 imm1 ( - ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Iadd, imm2 - imm1))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Imul, imm1)), Op (Intop_imm (Mach.Imul, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_mul imm1 imm2
             && U.amd64_imm32_within_bounds imm1 imm2 ( * ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Imul, imm1 * imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ilsl, imm1)), Op (Intop_imm (Mach.Imul, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3 && imm1 >= 0 && imm1 < 31
             && Misc.no_overflow_mul (1 lsl imm1) imm2
             && U.amd64_imm32_within_bounds (1 lsl imm1) imm2 ( * ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Imul, (1 lsl imm1) * imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Imul, imm1)), Op (Intop_imm (Mach.Ilsl, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3 && imm2 >= 0 && imm2 < 31
             && Misc.no_overflow_mul (1 lsl imm2) imm1
             && U.amd64_imm32_within_bounds (1 lsl imm2) imm1 ( * ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Imul, (1 lsl imm2) * imm1))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_arith";
          Some ((U.prev_at_most slide_back) op3)
    | _ -> None)
  | _ -> None

let fold_intop_imm_bitwise (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match U.get_cells cell 2 with
  | None -> None
  | Some [op1; op2] -> (
    let op1_val = DLL.value op1 in
    let op2_val = DLL.value op2 in
    match op1_val.desc, op2_val.desc with
    | Op (Intop_imm (Mach.Ilsl, imm1)), Op (Intop_imm (Mach.Ilsl, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && imm1 + imm2 <= Sys.int_size)
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Ilsl, imm1 + imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ilsr, imm1)), Op (Intop_imm (Mach.Ilsr, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && imm1 + imm2 <= Sys.int_size)
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Ilsr, imm1 + imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iasr, imm1)), Op (Intop_imm (Mach.Iasr, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && imm1 + imm2 <= Sys.int_size)
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Iasr, imm1 + imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iand, imm1)), Op (Intop_imm (Mach.Iand, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && U.amd64_imm32_within_bounds_assert_if_false imm1 imm2 ( land ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Iand, imm1 land imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ior, imm1)), Op (Intop_imm (Mach.Ior, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && U.amd64_imm32_within_bounds_assert_if_false imm1 imm2 ( lor ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Ior, imm1 lor imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ixor, imm1)), Op (Intop_imm (Mach.Ixor, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = Array.unsafe_get op2_val.arg 0 in
        let reg4 = Array.unsafe_get op2_val.res 0 in
        let reg1 = Array.unsafe_get op1_val.arg 0 in
        let reg2 = Array.unsafe_get op1_val.res 0 in
        if not
             (U.are_equal_regs reg1 reg2 && U.are_equal_regs reg3 reg4
            && U.are_equal_regs reg1 reg3
             && U.amd64_imm32_within_bounds_assert_if_false imm1 imm2 ( lxor ))
        then None
        else
          let op3 =
            DLL.insert_and_return_before op1
              { op1_val with
                desc = Cfg.Op (Intop_imm (Mach.Ixor, imm1 lxor imm2))
              }
          in
          DLL.delete_curr op1;
          DLL.delete_curr op2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then U.update_csv "fold_intop_imm_bitwise";
          Some ((U.prev_at_most slide_back) op3)
    | _ -> None)
  | _ -> None

let apply cell =
  match useless_movs cell with
  | None -> (
    match fold_intop_imm_arith cell with
    | None -> (
      match fold_intop_imm_bitwise cell with None -> None | res -> res)
    | res -> res)
  | res -> res

let generated_rule_names =
  ["useless_movs"; "fold_intop_imm_arith"; "fold_intop_imm_bitwise"]
