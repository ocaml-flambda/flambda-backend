(* Exercise empty array *)
let list_to_array = function
  | [] -> [| |]
  | _ :: _ -> assert false
