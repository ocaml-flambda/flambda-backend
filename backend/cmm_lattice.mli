(** A lattice value describing CMM integer values. *)
type t

val top : t

val join : t -> t -> t

(** properties *)

val leading_zeros : t -> int

val leading_ones : t -> int

val sign_bits : t -> int

val sign_bit : t -> bool option

val to_constant : t -> Nativeint.t option

val to_small_int : t -> min:int -> max:int -> int option

val can_weaken_add_to_or : t -> t -> bool

val can_weaken_mul :
  t ->
  t ->
  [ `Constant of nativeint
  | `Shift_LHS_left_by of int
  | `Shift_RHS_left_by of int
  | `Unknown ]

(** constructors *)

val constant : Nativeint.t -> t

val logor : t -> t -> t

val logand : t -> t -> t

val logxor : t -> t -> t

val add : t -> t -> t

val shift_right_logical : t -> t -> t

val shift_right : t -> t -> t

val shift_left : t -> t -> t
