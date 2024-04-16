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
