(* This test was extracted from the hdf5 package. It tests the case where the
   kind of the array is refined to Bottom (due to conflicting type annotations),
   and in this case the kind of the returned Bottom type must match the expected
   kind of the primitive, which is Value in this case.

   Note that this function will be simplified to Invalid; the original code
   likely needs some extra calls to Sys.opaque_identity. *)

let f (a : int) v = Array.unsafe_set (Obj.magic a : float array) 0 v
