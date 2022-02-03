[@@@ocaml.warning "+a-4-30-40-41-42"]

module Domain = struct
  type t = Reg.Set.t

  let bot = Reg.Set.empty

  let compare = Reg.Set.compare

  let join = Reg.Set.union

  let less_equal = Reg.Set.subset

  (* CR xclerc for xclerc: already defined elsewhere? *)
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

  let basic : domain -> exn:domain -> Cfg.basic Cfg.instruction -> domain =
   fun value ~exn instr ->
    match instr.desc with
    | Op _ | Call _ ->
      let across = Reg.diff_set_array value instr.res in
      let across =
        if Cfg.can_raise_basic instr.desc && instr.trap_depth > 1
        then Reg.Set.union across exn
        else across
      in
      instr.live <- across;
      Reg.add_set_array across instr.arg
    | Reloadretaddr ->
      instr.live <- Reg.Set.empty;
      Reg.diff_set_array value Proc.destroyed_at_reloadretaddr
    | Pushtrap _ ->
      instr.live <- Reg.Set.empty;
      value
    | Poptrap ->
      instr.live <- Reg.Set.empty;
      value
    | Prologue ->
      instr.live <- Reg.Set.empty;
      value

  let terminator :
      domain -> exn:domain -> Cfg.terminator Cfg.instruction -> domain =
   fun value ~exn instr ->
    match instr.desc with
    | Never -> assert false
    | Always _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Parity_test _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Truth_test _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Float_test _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Int_test _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Switch _ ->
      instr.live <- value;
      Reg.add_set_array value instr.arg
    | Return ->
      instr.live <- Reg.Set.empty;
      Reg.set_of_array instr.arg
    | Tailcall (Self _) ->
      instr.live <- Reg.Set.empty;
      Reg.set_of_array instr.arg
    | Raise _ ->
      instr.live <- exn;
      Reg.add_set_array exn instr.arg
    | Tailcall (Func _) ->
      instr.live <- Reg.Set.empty;
      Reg.set_of_array instr.arg
    | Call_no_return _ ->
      instr.live <- Reg.Set.empty;
      Reg.Set.union exn (Reg.set_of_array instr.arg)

  let exception_ : domain -> domain =
   fun value -> Reg.Set.remove Proc.loc_exn_bucket value
end

module Liveness = Cfg_dataflow.Backward (Domain) (Transfer)
