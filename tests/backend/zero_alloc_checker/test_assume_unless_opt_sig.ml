module Test_fail_in_opt_only : sig
  val foo : 'a -> 'a * 'a [@@zero_alloc]
end = struct
  let[@inline never][@zero_alloc assume_unless_opt] foo x = (x,x)
end

module Test_pass : sig
  val foo : 'a -> 'a [@@zero_alloc]
end = struct
  let[@inline never][@zero_alloc assume_unless_opt] foo x = x
end

module Test_error : sig
  (* The payload "assume_zero_alloc" is not allowed on signatures, just like "assume".
     We get a warning, then the check of the implementation fails in "opt" mode,
     because the unsupported payload is removed from the attribute,
     and the signature annotation becomes [@@zero_alloc],
     and the check of the caller passes. *)
  val foo : 'a -> 'a * 'a [@@zero_alloc assume_unless_opt]
end = struct
  let[@inline never][@zero_alloc assume_unless_opt] foo x = (x,x)
end

let[@zero_alloc] test1_pass x = Test_fail_in_opt_only.foo x
let[@zero_alloc] test2_pass x = Test_pass.foo x
let[@zero_alloc] test3_pass x = Test_error.foo x
