(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external caml_bigstring_get_16
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_get16"

external caml_bigstring_get_32
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_get32"

external caml_bigstring_get_f32
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_getf32"

external caml_bigstring_get_64
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_get64"

external caml_bigstring_get_64
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_geta128"

external caml_bigstring_get_64
  :  bigstring
  -> int
  -> int
  = "%caml_bigstring_getu128"

module By_int64_u = struct
  module I = Stdlib_upstream_compatible.Int64_u

  module A = struct
    external get16
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get16_indexed_by_int64#"

    external get16u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get16u_indexed_by_int64#"

    external get32
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get32_indexed_by_int64#"

    external get32u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get32u_indexed_by_int64#"

    external getf32
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_getf32_indexed_by_int64#"

    external getf32u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_getf32u_indexed_by_int64#"

    external get64
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get64_indexed_by_int64#"

    external get64u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_get64u_indexed_by_int64#"

    external geta128
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_geta128_indexed_by_int64#"

    external geta128u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_geta128u_indexed_by_int64#"

    external getu128
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_getu128_indexed_by_int64#"

    external getu128u
      :  bigstring
      -> int64#
      -> int
      = "%caml_bigstring_getu128u_indexed_by_int64#"

    let assert_bound_check_get f bs i =
      try
        ignore (f bs i);
        assert false
      with
      | Invalid_argument _ -> ()
    ;;
  end
end

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a
;;

let reference_str = String.init 300 (fun i -> i * 7 mod 256 |> char_of_int)
let create_bs () = reference_str |> bigstring_of_string

let () =
  let bs = create_bs () in
  (* -1 *)
  By_int64_u.A.assert_bound_check_get By_int64_u.A.get16 bs (-#1L);
  By_int64_u.A.assert_bound_check_get By_int64_u.A.get32 bs (-#1L);
  By_int64_u.A.assert_bound_check_get By_int64_u.A.getf32 bs (-#1L);
  By_int64_u.A.assert_bound_check_get By_int64_u.A.get64 bs (-#1L);
  (* By_int64_u.A.assert_bound_check_get By_int64_u.A.geta128 bs (-#1L); *)
  (* By_int64_u.A.assert_bound_check_get By_int64_u.A.getu128 bs (-#1L); *)
  (* Close to length *)
  By_int64_u.A.assert_bound_check_get By_int64_u.A.get16 bs #300L;
  By_int64_u.A.assert_bound_check_get By_int64_u.A.get16 bs #299L;
  (* This is
     0b1000000000000000000000000000000000000000000000000000000000000001
     in binary and should be out of bound. *)
  By_int64_u.A.assert_bound_check_get
    By_int64_u.A.get16
    bs
    (-#9223372036854775807L)
;;

let () =
  let bs1 = create_bs () in
  let bs2 = create_bs () in
  for i = 0 to 298 do
    assert (
      caml_bigstring_get_16 bs1 i
      = By_int64_u.A.get16 bs2 (i |> By_int64_u.I.of_int));
    assert (
      caml_bigstring_get_16 bs1 i
      = By_int64_u.A.get16u bs2 (i |> By_int64_u.I.of_int))
  done
;;
