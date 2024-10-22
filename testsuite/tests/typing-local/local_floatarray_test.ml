(* TEST *)

type t = Float.Array.t

external create_local : int -> local_ t = "caml_floatarray_create_local"

external get : local_ t -> int -> float = "%floatarray_safe_get"
external set : local_ t -> int -> float -> unit = "%floatarray_safe_set"

external unsafe_get : local_ t -> int -> float = "%floatarray_unsafe_get"
external unsafe_set : local_ t -> int -> float -> unit = "%floatarray_unsafe_set"

let create_local ~len x = exclave_
  match create_local len with
  | uninit ->
    for i = 0 to len - 1 do
      unsafe_set uninit i x
    done;
    uninit
  | exception Invalid_argument _ ->
    failwith "invalid length"

let create_local_length_check () =
  try
    let _ : t = create_local ~len:(-1) 0.0 in
    assert false
  with _ -> ()

let create_local_zero_length_check () =
  let _ : t = create_local ~len:(0) 0.0 in
  ()

let safe_accesses () =
  let rec test ~len =
    if len = 10_000 then ()
    else
      let arr = create_local ~len 1. in
      for i = 0 to len - 1 do
        set arr i (Float.of_int i)
      done;
      test ~len:(len + 1);
      for i = 0 to len - 1 do
        assert(get arr i = Float.of_int i);
      done
 in
 test ~len:0

let unsafe_accesses () =
  let rec test ~len =
    if len = 10_000 then ()
    else
      let arr = create_local ~len 1. in
      for i = 0 to len - 1 do
        unsafe_set arr i (Float.of_int i)
      done;
      test ~len:(len + 1);
      for i = 0 to len - 1 do
        assert (unsafe_get arr i = Float.of_int i);
      done
  in
  test ~len:0

let () =
  create_local_length_check ();
  create_local_zero_length_check ();
  safe_accesses ();
  unsafe_accesses ()
