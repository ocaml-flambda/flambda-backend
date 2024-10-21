(* Test error message with anonymous function and module names *)
let[@inline never] foo x f = f x

module Inner = struct
  let bar = foo (Sys.opaque_identity 1) (fun [@zero_alloc] x -> (x,x))
end
