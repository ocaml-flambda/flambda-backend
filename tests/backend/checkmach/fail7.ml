exception Exn_string of string
exception Exn_int of int
exception Exn

(* zero_alloc check fails because raise_notrace is not considered as an "error path",
   but as control. *)
let[@zero_alloc][@inline never] test n =
  match n with
  | 1 -> raise_notrace Exn
  | 2 -> raise_notrace (Exn_int n)
  | 3 -> raise_notrace (Exn_string "foo")
  | _ -> 10
