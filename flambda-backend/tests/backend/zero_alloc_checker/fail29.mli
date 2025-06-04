[@@@zero_alloc all]

val foo : 'a -> 'b -> 'a * 'b

val bar : 'a -> 'a -> 'a list

val outer : int -> int * int

val do_not_check_me : int -> int * int [@@zero_alloc ignore]

val only_check_me_in_opt : int -> int -> int * int * int [@@zero_alloc opt]

val check_me_strict : int -> int [@@zero_alloc strict]
