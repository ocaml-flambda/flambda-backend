let f x len pos =
  let[@zero_alloc][@inline] unsafe_set_int64
                                             ba
                                             pos
    =
    Bigarray.Array1.unsafe_set ba pos (Int64.of_int x)
  in
    let ba = Bigarray.Array1.create Int64 C_layout (Array.length len) in
    unsafe_set_int64 ba pos;
    ba
