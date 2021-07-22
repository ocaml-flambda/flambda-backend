external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
