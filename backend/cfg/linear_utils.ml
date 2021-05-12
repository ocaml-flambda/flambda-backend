[@@@ocaml.warning "+a-30-40-41-42"]

type labelled_insn =
  { label : Label.t;
    insn : Linear.instruction
  }

let labelled_insn_end = { label = -1; insn = Linear.end_instr }

let rec defines_label (i : Linear.instruction) =
  match i.desc with
  | Lend | Llabel _ -> true
  | Ladjust_trap_depth _ -> defines_label i.next
  | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Lbranch _
  | Lcondbranch _ | Lcondbranch3 _ | Lswitch _ | Lentertrap | Lpushtrap _
  | Lpoptrap | Lraise _ ->
      false
