[@@@zero_alloc all_opt]

val foo : 'a -> 'b -> 'a*'b

val bar : 'a -> 'a -> 'a list

val outer : int -> int*int

val[@zero_alloc ignore] do_not_check_me : int -> int*int

val[@zero_alloc opt] only_check_me_in_opt : int -> int -> int*int*int

val[@zero_alloc strict] check_me_strict : int -> int
