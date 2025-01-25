(* TEST
 bytecode;
*)

(* The following examples have different output on bytecode and native.
   The order of evaluation of arguments in cmm_helpers needs to be fixed. *)
open Bigarray
let () =
(* CR gyorsh: fix bigarray_get  *)
  let[@inline never] test_bigarray_get
       (a : (Complex.t, complex32_elt, c_layout) Array1.t)
       (i : int) =
    print_float (Array1.unsafe_get (print_endline "A"; a)
                   (print_endline "B"; i)).Complex.re;
    print_newline ()
  in
  test_bigarray_get (Array1.init complex32 c_layout 3 (fun _ -> Complex.one)) 0;

  (* CR gyorsh: fix bigarray_set  *)
  let[@inline never] test_bigarray_set
       (a : (Complex.t, complex32_elt, c_layout) Array1.t)
       (i : int) =
    Array1.unsafe_set (print_endline "A"; a) (print_endline "B"; i)
      (print_endline "C"; Complex.i);
    print_endline "?"
  in
  test_bigarray_set (Array1.create complex32 c_layout 3) 0;

  (* CR gyorsh: fix send *)
  let[@inline never] test_send o x =
    (print_endline "A"; o)#m (print_endline "B"; x)
  in
  test_send (object method m (_ : int) = print_endline "m" end) 1
