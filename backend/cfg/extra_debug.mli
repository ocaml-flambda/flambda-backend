(** Insertion of extra debugging information used to correlate between
    machine instructions, [Linear] and [Cfg] code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Writes the id of each cfg instruction into [fdo] field.  This information is copied to
    the corresponding field of [Linear.instruction] during [cfg_to_linear]. Having this as
    a separate pass allows us to choose whether to add the extra information to the IR or
    not, without passing extra arguments to [cfg_to_linear] and independently of it.. *)
val add : Cfg_with_layout.t -> unit
