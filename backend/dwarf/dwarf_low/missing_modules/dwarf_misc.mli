(* Linearize.label*)
type label = int
(* Cmm.label *)
type cmm_label = int
(* Arch.size_addr *)
val size_addr : int
(* Arch.size_int *)
val size_int : int
(* Cmm.new_label *)
val new_label : unit -> label