let[@inline never] test14 n = Float.of_int n

let[@zero_alloc strict] test15 n = Int64.to_int (Int64.of_float (test14 n))
