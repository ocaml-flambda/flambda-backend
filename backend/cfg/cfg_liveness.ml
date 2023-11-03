[@@@ocaml.warning "+a-4-30-40-41-42"]

type domain =
  { before : Reg.Set.t;
    across : Reg.Set.t
  }

module Domain : Cfg_dataflow.Domain_S with type t = domain = struct
  type t = domain =
    { before : Reg.Set.t;
      across : Reg.Set.t
    }

  let bot = { before = Reg.Set.empty; across = Reg.Set.empty }

  let join { before = left_before; across = _ }
      { before = right_before; across = _ } =
    { before = Reg.Set.union left_before right_before; across = Reg.Set.empty }

  let less_equal { before = left_before; across = _ }
      { before = right_before; across = _ } =
    Reg.Set.subset left_before right_before
end

type error = |

module Transfer :
  Cfg_dataflow.Backward_transfer
    with type domain = domain
     and type error = error = struct
  type nonrec domain = domain =
    { before : Reg.Set.t;
      across : Reg.Set.t
    }

  type nonrec error = error

  let[@inline always] instruction { before; across = _ } ~can_raise ~exn
      (instr : _ Cfg.instruction) =
    let across = Reg.diff_set_array before instr.res in
    let across =
      if can_raise then Reg.Set.union across exn.before else across
    in
    let before = Reg.add_set_array across instr.arg in
    { before; across }

  let basic : domain -> Cfg.basic Cfg.instruction -> (domain, error) result =
   fun ({ before; across = _ } as domain) instr ->
    Result.ok
    @@
    match instr.desc with
    | Op _ | Reloadretaddr | Pushtrap _ | Poptrap | Prologue ->
      if Cfg.is_pure_basic instr.desc && Reg.disjoint_set_array before instr.res
      then
        (* If the operation is without side-effects and the result is unused
           then don't mark the arguments as used because this instruction could
           be removed. *)
        { before; across = before }
      else instruction ~can_raise:false ~exn:Domain.bot domain instr

  let terminator :
      domain ->
      exn:domain ->
      Cfg.terminator Cfg.instruction ->
      (domain, error) result =
   fun domain ~exn instr ->
    Result.ok
    @@
    match instr.desc with
    | Never -> assert false
    | Tailcall_self _ ->
      (* CR-someday azewierzejew: If the stamps for the tail call DomainState
         argument and parameter were the same and Tailcall (Self _) had
         [instr.arg = instr.res] (either by removing the args or adding results
         because currently there is a nonempty array in args but empty in res)
         then it would behave exactly the same as every other instruction. *)
      (* We have to handle this as a special case because the registers for a
         given DomainState argument and parameter pair have different stamps. *)
      instruction
        ~can_raise:(Cfg.can_raise_terminator instr.desc)
        ~exn Domain.bot instr
    | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
    | Switch _ | Return | Raise _ | Tailcall_func _ | Call_no_return _ | Call _
    | Poll_and_jump _ | Prim _ | Specific_can_raise _ ->
      instruction
        ~can_raise:(Cfg.can_raise_terminator instr.desc)
        ~exn domain instr

  let exception_ : domain -> (domain, error) result =
   fun { before; across = _ } ->
    Result.ok
    @@ { before = Reg.Set.remove Proc.loc_exn_bucket before;
         across = Reg.Set.empty
       }
end

module Liveness = Cfg_dataflow.Backward (Domain) (Transfer)
