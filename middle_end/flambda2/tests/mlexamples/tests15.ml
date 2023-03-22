(* Exercise empty array *)
let list_to_array = function
  | [] -> [| |]
  | _ :: _ -> assert false

(* Exercise %int_comp Eq (generates %int_comp imm untagged = 2i) *)
type t_imm = A of int | B of int | C of int | D of int
let is_c x = function C _ -> true | _ -> false
