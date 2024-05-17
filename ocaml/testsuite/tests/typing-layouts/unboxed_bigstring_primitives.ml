(* TEST
 flambda2;
 include stable;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* CR layouts: add the unboxed simd externals once we support them *)
open Bigarray
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
external caml_bigstring_get_32 : bigstring -> int -> int32# = "%caml_bigstring_get32#"
external caml_bigstring_get_64 : bigstring -> int -> int64# = "%caml_bigstring_get64#"
external caml_bigstring_set_32 : bigstring -> int -> int32# -> unit = "%caml_bigstring_set32#"
external caml_bigstring_set_64 : bigstring -> int -> int64# -> unit = "%caml_bigstring_set64#"

external unsafe_caml_bigstring_get_32 : bigstring -> int -> int32# = "%caml_bigstring_get32u#"
external unsafe_caml_bigstring_get_64 : bigstring -> int -> int64# = "%caml_bigstring_get64u#"
external unsafe_caml_bigstring_set_32 : bigstring -> int -> int32# -> unit = "%caml_bigstring_set32u#"
external unsafe_caml_bigstring_set_64 : bigstring -> int -> int64# -> unit = "%caml_bigstring_set64u#"

external[@layout_poly] ignore : ('a : any). 'a -> unit = "%ignore"

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a

let s = bigstring_of_string (String.make 10 '\x00')
let empty_s = bigstring_of_string ""

let assert_bound_check f =
  try
    ignore(f ());
    assert false
  with
      | Invalid_argument _ -> ()

let () =
  assert_bound_check (fun () -> ignore (caml_bigstring_get_32 s (-1)));
  assert_bound_check (fun () -> ignore (caml_bigstring_get_32 s 7));
  assert_bound_check (fun () -> ignore (caml_bigstring_get_64 s (-1)));
  assert_bound_check (fun () -> ignore (caml_bigstring_get_64 s 3));

  assert_bound_check (fun () -> caml_bigstring_set_32 s (-1) #0l);
  assert_bound_check (fun () -> caml_bigstring_set_32 s 7 #0l);
  assert_bound_check (fun () -> caml_bigstring_set_64 s (-1) #0L);
  assert_bound_check (fun () -> caml_bigstring_set_64 s 3 #0L);

  assert_bound_check (fun () -> ignore (caml_bigstring_get_32 empty_s 0));
  assert_bound_check (fun () -> ignore (caml_bigstring_get_64 empty_s 0));

  assert_bound_check (fun () -> caml_bigstring_set_32 empty_s 0 #0l);
  assert_bound_check (fun () -> caml_bigstring_set_64 empty_s 0 #0L)

external bswap32: int32 -> int32 = "%bswap_int32"
external bswap64: int64 -> int64 = "%bswap_int64"

let swap32 x =
  let open Stable.Int32_u in
  if Sys.big_endian
  then x |> to_int32 |> bswap32 |> of_int32
  else x

let swap64 x =
  let open Stable.Int64_u in
  if Sys.big_endian
  then x |> to_int64 |> bswap64 |> of_int64
  else x

let to_int32 = Stable.Int32_u.to_int32
let to_int64 = Stable.Int64_u.to_int64
let test get_64 set_64 get_32 set_32 =
  set_32 s 0 (swap32 #0x12345678l);
  Printf.printf "%lx %lx %lx\n%!"
                (to_int32 (swap32 (get_32 s 0)))
                (to_int32 (swap32 (get_32 s 1)))
                (to_int32 (swap32 (get_32 s 2)));
  set_32 s 0 (swap32 #0xFEDCBA09l);
  Printf.printf "%lx %lx %lx\n%!"
                (to_int32 (swap32 (get_32 s 0)))
                (to_int32 (swap32 (get_32 s 1)))
                (to_int32 (swap32 (get_32 s 2)));

  set_64 s 0 (swap64 #0x1234567890ABCDEFL);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (to_int64 (swap64 (get_64 s 0)))
                (to_int64 (swap64 (get_64 s 1)))
                (to_int64 (swap64 (get_64 s 2)));
  set_64 s 0 (swap64 #0xFEDCBA0987654321L);
  Printf.printf "%Lx %Lx %Lx\n%!"
                (to_int64 (swap64 (get_64 s 0)))
                (to_int64 (swap64 (get_64 s 1)))
                (to_int64 (swap64 (get_64 s 2)))

let () =
  test
    caml_bigstring_get_64
    caml_bigstring_set_64
    caml_bigstring_get_32
    caml_bigstring_set_32;
  test
    unsafe_caml_bigstring_get_64
    unsafe_caml_bigstring_set_64
    unsafe_caml_bigstring_get_32
    unsafe_caml_bigstring_set_32
