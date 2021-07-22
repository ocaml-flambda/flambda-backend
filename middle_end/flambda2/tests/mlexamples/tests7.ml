external ( + ) : int -> int -> int = "%addint"

module String = struct
  type t = string
  external length : t -> int = "%string_length"
end

let cow = "Cow"
let sheep = "Sheep"

let f x str =
  x + String.length cow + String.length sheep
    + String.length str + String.length str

type all_floats = {
  f1 : float;
  f2 : float;
}

external ( +. ) : float -> float -> float = "%addfloat"
external ( *. ) : float -> float -> float = "%mulfloat"

let foo af y =
  let x = af.f1 *. af.f2 in
  x +. y
