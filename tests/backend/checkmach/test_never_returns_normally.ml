(* CR gyorsh: Does not work without [@inline]. Fix by propagating never_returns_normally
   to check_mach as an extra annotation on [Debuginfo.t]. *)
let[@zero_alloc assume never_returns_normally] failwithf fmt =
  Printf.ksprintf (fun s () -> failwith s) fmt

let[@zero_alloc assume never_returns_normally] invalid_argf fmt =
  Printf.ksprintf (fun s () -> invalid_arg s) fmt

let[@zero_alloc] foo x = failwithf "%d" x
let[@zero_alloc] bar x y = invalid_argf "%d" (x+y)

