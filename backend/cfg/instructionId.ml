[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
include Numbers.Int

type t = int

let none = -1

let max = Int.max

let to_string = Int.to_string

let to_string_padded t = Printf.sprintf "#%04d" t

let format fmt t = Format.fprintf fmt "%d" t

type sequence = { mutable next : t }

let make_sequence ?(last_used = -1) () = { next = succ last_used }

let reset seq = seq.next <- 0

let get seq = seq.next

let get_and_incr seq =
  let res = seq.next in
  seq.next <- succ seq.next;
  res

let to_int_unsafe t = t
