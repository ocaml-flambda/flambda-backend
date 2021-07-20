type t = Empty | Node of { l: t; v: int; r: t; }

let bal l v r = Node { l; v; r; }
[@@inline never]

let rec min_binding = function
  | Empty -> raise Not_found
  | Node {l=Empty; v} -> v
  | Node {l} -> min_binding l

let rec remove_min_binding = function
  | Empty -> invalid_arg "Map.remove_min_elt"
  | Node {l=Empty; r} -> r
  | Node {l; v; r} -> bal (remove_min_binding l) v r

let merge t1 t2 =
  match t1, t2 with
| (Empty, t) -> t
| (t, Empty) -> t
| (_,_) ->
    let x = min_binding t2 in
    bal t1 x (remove_min_binding t2)

let rec remove x = function
  | Empty -> Empty
  | Node {l; v; r; } ->
      let c = Int.compare x v in
      if c = 0 then merge l r
      else l
