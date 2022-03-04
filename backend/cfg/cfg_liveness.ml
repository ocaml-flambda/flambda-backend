[@@@ocaml.warning "+a-4-30-40-41-42"]

module Domain = struct
  type t = Reg.Set.t

  let bot = Reg.Set.empty

  let compare = Reg.Set.compare

  let join = Reg.Set.union

  let less_equal = Reg.Set.subset

  let with_formatter ~f x =
    let buff = Buffer.create 64 in
    let fmt = Format.formatter_of_buffer buff in
    f fmt x;
    Format.pp_print_flush fmt ();
    Buffer.contents buff

  let to_string regset =
    regset |> Reg.Set.elements
    |> ListLabels.map ~f:(with_formatter ~f:Printmach.reg)
    |> StringLabels.concat ~sep:", "
end

module Transfer = struct
  type domain = Domain.t

  let basic :
      domain ->
      exn:domain ->
      Cfg.basic Cfg.instruction ->
      domain * Cfg.basic Cfg.instruction =
   fun value ~exn instr ->
    match instr.desc with
    | Op _ | Call _ ->
      if Cfg.is_pure_basic instr.desc
         && Reg.disjoint_set_array value instr.res
         && (not (Proc.regs_are_volatile instr.arg))
         && not (Proc.regs_are_volatile instr.res)
      then value, Cfg.set_live instr value
      else
        let across = Reg.diff_set_array value instr.res in
        let across =
          if Cfg.can_raise_basic instr.desc && instr.trap_depth > 1
          then Reg.Set.union across exn
          else across
        in
        Reg.add_set_array across instr.arg, Cfg.set_live instr across
    | Reloadretaddr ->
      ( Reg.diff_set_array value Proc.destroyed_at_reloadretaddr,
        Cfg.set_live instr Reg.Set.empty )
    | Pushtrap _ -> value, Cfg.set_live instr Reg.Set.empty
    | Poptrap -> value, Cfg.set_live instr Reg.Set.empty
    | Prologue -> value, Cfg.set_live instr Reg.Set.empty

  let terminator :
      domain ->
      exn:domain ->
      Cfg.terminator Cfg.instruction ->
      domain * Cfg.terminator Cfg.instruction =
   fun value ~exn instr ->
    match instr.desc with
    | Never -> assert false
    | Always _ -> Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Parity_test _ ->
      Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Truth_test _ ->
      Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Float_test _ ->
      Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Int_test _ -> Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Switch _ -> Reg.add_set_array value instr.arg, Cfg.set_live instr value
    | Return -> Reg.set_of_array instr.arg, Cfg.set_live instr Reg.Set.empty
    | Tailcall (Self _) ->
      Reg.set_of_array instr.arg, Cfg.set_live instr Reg.Set.empty
    | Raise _ -> Reg.add_set_array exn instr.arg, Cfg.set_live instr exn
    | Tailcall (Func _) ->
      Reg.set_of_array instr.arg, Cfg.set_live instr Reg.Set.empty
    | Call_no_return _ ->
      Reg.add_set_array exn instr.arg, Cfg.set_live instr exn

  let exception_ : domain -> domain =
   fun value -> Reg.Set.remove Proc.loc_exn_bucket value
end

module Liveness = Cfg_dataflow.Backward (Domain) (Transfer)
