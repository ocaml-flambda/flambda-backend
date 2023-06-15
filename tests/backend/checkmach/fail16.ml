(* still do get a zero_alloc failure if we use a functor in a checked function  *)
[@@@zero_alloc all]

module[@inline never] F (X : sig val x : int end) =
struct
  let f () = X.x
end

module M1 = struct
  let x = 42
end

let f () =
  let module M2 = F(M1) in
  M2.f
