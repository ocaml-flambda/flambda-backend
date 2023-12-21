(* TEST
   * flambda2
   ** native
*)

module M : sig
  val bits_of_float : float -> int64
end = struct
  external bits_of_float
    :  (float[@local_opt])
    -> (int64[@local_opt])
    = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
end

let go_m f =
  let i = M.bits_of_float f in
  assert (i = 4L);
  ()
