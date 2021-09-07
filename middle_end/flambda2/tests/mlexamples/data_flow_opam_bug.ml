(* build with -g *)

let to_string context t =
  let paren ?(cond = false) f = f in
  let cond = Some (context = `Plop) in
  match t with None -> paren ?cond "plop" | Some _ -> paren ?cond "plip"
