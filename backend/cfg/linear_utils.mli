[@@@ocaml.warning "+a-30-40-41-42"]

(** Miscellaneous utility functions on [Linear] code. *)

type labelled_insn =
  { label : Label.t;
    insn : Linear.instruction
  }

val labelled_insn_end : labelled_insn

val defines_label : Linear.instruction -> bool
