module A1 = struct
  external stub : int -> int = "test_stub"
  let[@inline always] wrap x = (stub[@zero_alloc assume]) x

  let[@zero_alloc] foo x = wrap x
end

module A2 = struct
  external stub : int -> int = "test_stub"
  let[@inline always][@zero_alloc assume] wrap x = stub x

  let[@zero_alloc] foo x = wrap x
end

module A3 = struct
  external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"

  let[@inline never] id x = x

  let[@zero_alloc] overapplied g x = (revapply[@zero_alloc assume]) g id x
end

module A4 = struct
  external opaque_identity : 'a -> 'a = "%opaque"

  let[@zero_alloc] foo _y =
    (opaque_identity[@zero_alloc assume]) (fun x -> x)

  (* overapply, still succeeds *)
  let[@zero_alloc] bar y =
    (opaque_identity[@zero_alloc assume]) (fun x -> x) y

  (* apply, fails as expected *)
  let[@zero_alloc] baz y =
    ((opaque_identity[@zero_alloc assume]) (fun x -> x)) y
end

module A5 = struct
  external get : 'a array -> int -> 'a = "%array_safe_get"

  let[@zero_alloc] foo a i =
    (get [@zero_alloc assume]) a (i+1)

end
