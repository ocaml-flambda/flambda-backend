(* TEST
   native;
*)

(* Preliminaries to minimize deps *)

external int32_u_to_int32 : int32# -> int32
  = "%box_int32" [@@warning "-187"]

external int64_u_to_int64 : int64# -> int64
  = "%box_int64" [@@warning "-187"]

external nativeint_u_to_nativeint : nativeint# -> nativeint
  = "%box_nativeint" [@@warning "-187"]

external nativeint_to_nativeint_u : nativeint -> nativeint#
  = "%unbox_nativeint" [@@warning "-187"]

external float_u_to_float : float# -> float
  = "%box_float" [@@warning "-187"]

(* CR mshinwell for gyorsh: enable these float32 cases *)
  (*
external float32_u_to_float : float32# -> float32
  = "%box_float32" [@@warning "-187"]
  *)

let nativeint_u_add u1 n2 =
  let n1 = nativeint_u_to_nativeint u1 in
  let n = Nativeint.add n1 n2 in
  nativeint_to_nativeint_u n

(* The test itself starts here *)

(* For the real API we don't plan to expose the type equality, but it
   makes it easier to write the test below. *)
type ('a : any) t = nativeint#

external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]

external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]

external calloc :
  count:(int[@untagged]) -> size:(int[@untagged]) -> nativeint# =
  "caml_no_bytecode_impl" "calloc"
  [@@noalloc]

let test_read p1 p2 p3 p4 p5 p6
  : #(int * int32# * int64# * nativeint# * float# * float#) =
  #(read p1, read p2, read p3, read p4, read p5, read p6)

(* CR mshinwell for gyorsh: the last number here should be #0.1234s *)
let values () = #(min_int, #400l, #9999999999L, #123456n, #0.87654, #0.1234)

let test_write p1 p2 p3 p4 p5 p6 =
  let #(n1, n2, n3, n4, n5, n6) = values () in
  write p1 n1;
  write p2 n2;
  write p3 n3;
  write p4 n4;
  write p5 n5;
  write p6 n6

let () =
  let buf = calloc ~count:6 ~size:8 in
  let int_buf = buf in
  let int32_buf = nativeint_u_add buf 8n in
  let int64_buf = nativeint_u_add buf 16n in
  let nativeint_buf = nativeint_u_add buf 24n in
  let float_buf = nativeint_u_add buf 32n in
  let float32_buf = nativeint_u_add buf 40n in
  test_write int_buf int32_buf int64_buf nativeint_buf float_buf float32_buf;
  let #(n1, n2, n3, n4, n5, n6) =
    test_read int_buf int32_buf int64_buf nativeint_buf float_buf float32_buf
  in
  let #(n1', n2', n3', n4', n5', n6') = values () in
  assert (Int.equal n1 n1');
  assert (Int32.equal (int32_u_to_int32 n2) (int32_u_to_int32 n2'));
  assert (Int64.equal (int64_u_to_int64 n3) (int64_u_to_int64 n3'));
  assert (Nativeint.equal (nativeint_u_to_nativeint n4)
    (nativeint_u_to_nativeint n4'));
  assert (Float.equal (float_u_to_float n5) (float_u_to_float n5'));
  assert (Float.equal (float_u_to_float n6) (float_u_to_float n6'))
