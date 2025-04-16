[@@@ocaml.warning "+a-40-41-42"]

open Cfg_intf.S
open! Int_replace_polymorphic_compare [@@warning "-66"]

let from_basic (basic : basic) : Linear.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap { lbl_handler = _ } -> Lpoptrap
  | Stack_check { max_frame_size_bytes } -> Lstackcheck { max_frame_size_bytes }
  | Op op -> Lop op
