type t = Check_default | Check_all | Check_opt_only | No_check

let all = [ Check_default; Check_all; Check_opt_only; No_check ]

let to_string = function
  | Check_default -> "default"
  | Check_all -> "all"
  | Check_opt_only -> "opt"
  | No_check -> "none"

let equal t1 t2 =
  match t1, t2 with
  | Check_default, Check_default -> true
  | Check_all, Check_all -> true
  | No_check, No_check -> true
  | Check_opt_only, Check_opt_only -> true
  | (Check_default | Check_all | Check_opt_only | No_check), _ -> false

let of_string v =
  let f t =
    if String.equal (to_string t) v then Some t else None
  in
  List.find_map f all

let doc =
  "\n\    The argument specifies which annotations to check: \n\
    \      \"opt\" means attributes with \"opt\" payload and is intended for debugging;\n\
    \      \"default\" means attributes without \"opt\" payload; \n\
    \      \"all\" covers both \"opt\" and \"default\" and is intended for optimized builds."
