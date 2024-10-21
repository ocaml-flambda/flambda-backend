val[@zero_alloc] f_zero_alloc : 'a -> 'a
val[@zero_alloc] f_alloc : 'a -> 'a * 'a

module M_alloc_var : sig
  val[@zero_alloc] f_alloc2 : 'a -> 'a * 'a
end
val f_call_var : 'a -> 'a * 'a

val[@zero_alloc (arity 1)] f_arity_one : int -> int -> int

val[@zero_alloc] f_shadow : int -> int * int
val[@zero_alloc] f_shadow_alloc : int -> int * int
val[@zero_alloc] f_shadow_alloc_both : int -> int * int

val[@zero_alloc] f_basic_success : int -> unit
val[@zero_alloc] f_basic_fail : 'a -> 'a * 'a

val[@zero_alloc strict] f_strict_success : int -> int
val[@zero_alloc strict] f_strict_fail : int -> unit

val[@zero_alloc opt] f_opt_success : int -> unit
val[@zero_alloc opt] f_opt_fail : int -> int * int

val[@zero_alloc strict opt] f_strict_opt_success : int -> int
val[@zero_alloc strict opt] f_strict_opt_fail : int -> unit
