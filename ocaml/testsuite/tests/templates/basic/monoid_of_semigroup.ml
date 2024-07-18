type t = Semigroup.t option

let empty = None
let append t1 t2 =
  match t1, t2 with
  | None, None -> None
  | (Some _ as t), None
  | None, (Some _ as t) -> t
  | Some t1, Some t2 -> Some (Semigroup.op t1 t2)
