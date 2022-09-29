module Property = struct
  type t = Noalloc

  let to_string = function Noalloc -> "noalloc"

  let equal x y = match x, y with Noalloc, Noalloc -> true

  let from_lambda : Lambda.property -> t = function Noalloc -> Noalloc
end

type t =
  | Default_check
  | Assert of Property.t
  | Assume of Property.t

let print ppf t =
  match t with
  | Default_check -> ()
  | Assert p -> Format.fprintf ppf "assert %s@ " (Property.to_string p)
  | Assume p -> Format.fprintf ppf "assume %s@ " (Property.to_string p)

let from_lambda : Lambda.check_attribute -> t = function
  | Default_check -> Default_check
  | Assert p -> Assert (Property.from_lambda p)
  | Assume p -> Assume (Property.from_lambda p)

let equal x y =
  match x, y with
  | Default_check, Default_check -> true
  | Assert p1, Assert p2 | Assume p1, Assume p2 -> Property.equal p1 p2
  | (Default_check | Assert _ | Assume _), _ -> false

let is_default : t -> bool = function
  | Default_check -> true
  | Assert _ | Assume _ -> false
