(* CR-someday: see whether the `-4` can be dropped. *)
[@@@ocaml.warning "+a-30-40-41-42-4"]

open! Peephole_utils

(** Logical condition for simplifying the following case:
    {|
    mov x, y
    mov y, x
    |}

    In this case, the second instruction should be removed *)

let remove_useless_mov (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match get_cells cell 2 with
  | [fst; snd] -> (
      let fst_val = DLL.value fst in
      let snd_val = DLL.value snd in
      match fst_val.desc with
      | Op (Move | Spill | Reload) -> (
          let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
          match snd_val.desc with
          | Op (Move | Spill | Reload) ->
            let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
            if are_equal_regs fst_src snd_dst && are_equal_regs fst_dst snd_src
            then (
              if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
              then update_csv "remove_useless_mov";
              DLL.delete_curr snd;
              Some (prev_at_most go_back_const fst))
            else None
          | _ -> None)
      | _ -> None)
  | _ -> None
