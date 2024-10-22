(* [AMD64] Special case of [caml_csel_value] with a float comparison
   avoid branches for [equal].
*)
type t = float#

module Float_u = struct
  external to_float : float# -> (float[@local_opt]) = "%box_float"
  external of_float : (float[@local_opt]) -> float# = "%unbox_float"

  let[@inline] to_bits x = (Int64.bits_of_float) (to_float x)
  let[@inline] of_bits x = (Int64.float_of_bits) x |> of_float

  let[@inline always] div x y = of_float (Float.div (to_float x) (to_float y))

  external select_int64
    :  bool
    -> (int64[@unboxed])
    -> (int64[@unboxed])
    -> (int64[@unboxed])
    = "caml_csel_value" "caml_csel_int64_unboxed"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

  let[@inline] select b (ifso : t) (ifnot : t) : t =
    select_int64 b (to_bits ifso) (to_bits ifnot) |> of_bits

end

external float_notequal : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%notequal"
external float_equal : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%equal"


let[@inline] divide_unless_denom_zero_else
  ~(numer : float)
  ~(denom : float)
  ~(else_ : float)
  =
  let is_denom_zero = float_equal denom 0. in
  let numer = Float_u.of_float numer in
  let denom = Float_u.of_float denom in
  let else_ = Float_u.of_float else_ in
  Float_u.select
    is_denom_zero
    else_
    (Float_u.div numer denom)
  |> Float_u.to_float
;;

let is_result_larger_than_2
  ~(numer : float)
  ~(denom : float)
  ~(else_ : float)
  =
  let result = divide_unless_denom_zero_else
    ~numer
    ~denom
    ~else_
  in
  Float.compare result 2. > 0
;;
