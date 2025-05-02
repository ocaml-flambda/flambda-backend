val f_zero_alloc : 'a -> 'a [@@zero_alloc]

val f_alloc : 'a -> 'a * 'a [@@zero_alloc]

module M_alloc_var : sig
  val f_alloc2 : 'a -> 'a * 'a [@@zero_alloc]
end

val f_call_var : 'a -> 'a * 'a

val f_arity_one : int -> int -> int [@@zero_alloc arity 1]

val f_shadow : int -> int * int [@@zero_alloc]

val f_shadow_alloc : int -> int * int [@@zero_alloc]

val f_shadow_alloc_both : int -> int * int [@@zero_alloc]

val f_basic_success : int -> unit [@@zero_alloc]

val f_basic_fail : 'a -> 'a * 'a [@@zero_alloc]

val f_strict_success : int -> int [@@zero_alloc strict]

val f_strict_fail : int -> unit [@@zero_alloc strict]

val f_opt_success : int -> unit [@@zero_alloc opt]

val f_opt_fail : int -> int * int [@@zero_alloc opt]

val f_strict_opt_success : int -> int [@@zero_alloc strict opt]

val f_strict_opt_fail : int -> unit [@@zero_alloc strict opt]
