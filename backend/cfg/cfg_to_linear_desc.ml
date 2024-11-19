open Cfg_intf.S

let from_basic (basic : basic) : Linear.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Stack_check { max_frame_size_bytes } -> Lstackcheck { max_frame_size_bytes }
  | Op op -> Lop op
