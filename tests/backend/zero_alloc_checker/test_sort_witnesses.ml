module Float_u = struct
  type t = float#

  external of_float : (float[@local_opt]) -> float# = "%unbox_float"
  external to_float : float# -> (float[@local_opt]) = "%box_float"

  let[@inline] add t1 t2 : t =
    of_float (Float.add (to_float t1) (to_float t2))
  ;;

end
module UFO = struct
  type t = float#

  let[@inline always] of_option t =
    match t with
    | None -> Float_u.of_float (Float.abs (0. /. 0.))
    | Some f -> Float_u.of_float f

  let[@inline] to_float_none_as_nan (t : t) : float# = t
end

module Int = struct
  let[@inline always] to_float t = (Stdlib.float_of_int :> local_ int -> _) t
end

module S : sig
  type t
  val to_float_option : t -> float option
end = struct
  type t = int option

  let[@inline always] to_float_option t =
    match t with
    | None -> None
    | Some size -> Some (Int.to_float size)
  ;;
end

let[@zero_alloc] compute x y =
  let size (x : S.t) =
    x
    |> S.to_float_option
    |> UFO.of_option
    |> UFO.to_float_none_as_nan
  in
  let x = size x in
  let y = size y in
  Float_u.add x y
