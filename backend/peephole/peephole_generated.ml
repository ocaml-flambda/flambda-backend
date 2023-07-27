[@@@ocaml.warning "+a-30-40-41-42-4"]

open! Peephole_utils

let useless_movs (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match get_cells cell 2 with
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
        let reg3 = mov2_val.arg.(0) in
        let reg4 = mov2_val.res.(0) in
        let reg1 = mov1_val.arg.(0) in
        let reg2 = mov1_val.res.(0) in
        if not (are_equal_regs reg1 reg4 && are_equal_regs reg2 reg3)
        then None
        else (
          DLL.delete_curr mov2;
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then update_csv "useless_movs";
          Some ((prev_at_most slide_back) mov1))
    | _ -> None)
  | _ -> None

let fold_intop_imm_arith (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match get_cells cell 2 with
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
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && no_32_bit_overflow imm1 imm2 ( + ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Isub, imm1)), Op (Intop_imm (Mach.Isub, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && Misc.no_overflow_add imm1 imm2
             && no_32_bit_overflow imm1 imm2 ( + ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iadd, imm1)), Op (Intop_imm (Mach.Isub, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && Misc.no_overflow_sub imm1 imm2
             && no_32_bit_overflow imm1 imm2 ( - ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Isub, imm1)), Op (Intop_imm (Mach.Iadd, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && Misc.no_overflow_sub imm2 imm1
             && no_32_bit_overflow imm2 imm1 ( - ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Imul, imm1)), Op (Intop_imm (Mach.Imul, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && Misc.no_overflow_mul imm1 imm2
             && no_32_bit_overflow imm1 imm2 ( * ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ilsl, imm1)), Op (Intop_imm (Mach.Imul, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3 && imm1 >= 0 && imm1 < 31
             && Misc.no_overflow_mul (1 lsl imm1) imm2
             && no_32_bit_overflow (1 lsl imm1) imm2 ( * ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Imul, imm1)), Op (Intop_imm (Mach.Ilsl, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3 && imm2 >= 0 && imm2 < 31
             && Misc.no_overflow_mul (1 lsl imm2) imm1
             && no_32_bit_overflow (1 lsl imm2) imm1 ( * ))
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
          then update_csv "fold_intop_imm_arith";
          Some ((prev_at_most slide_back) op3)
    | _ -> None)
  | _ -> None

let fold_intop_imm_bitwise (cell : Cfg.basic Cfg.instruction DLL.cell) =
  let slide_back = 1 in
  match get_cells cell 2 with
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
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ilsr, imm1)), Op (Intop_imm (Mach.Ilsr, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iasr, imm1)), Op (Intop_imm (Mach.Iasr, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Iand, imm1)), Op (Intop_imm (Mach.Iand, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && bitwise_overflow_assert imm1 imm2 ( land ))
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ior, imm1)), Op (Intop_imm (Mach.Ior, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && bitwise_overflow_assert imm1 imm2 ( lor ))
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | Op (Intop_imm (Mach.Ixor, imm1)), Op (Intop_imm (Mach.Ixor, imm2)) ->
      if not
           (Array.length op1_val.arg = 1
           && Array.length op1_val.res = 1
           && Array.length op2_val.arg = 1
           && Array.length op2_val.res = 1)
      then None
      else
        let reg3 = op2_val.arg.(0) in
        let reg4 = op2_val.res.(0) in
        let reg1 = op1_val.arg.(0) in
        let reg2 = op1_val.res.(0) in
        if not
             (are_equal_regs reg1 reg2 && are_equal_regs reg3 reg4
            && are_equal_regs reg1 reg3
             && bitwise_overflow_assert imm1 imm2 ( lxor ))
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
          then update_csv "fold_intop_imm_bitwise";
          Some ((prev_at_most slide_back) op3)
    | _ -> None)
  | _ -> None

let generated_rule_names =
  ["useless_movs"; "fold_intop_imm_arith"; "fold_intop_imm_bitwise"]

let generated_rules =
  [useless_movs; fold_intop_imm_arith; fold_intop_imm_bitwise]
