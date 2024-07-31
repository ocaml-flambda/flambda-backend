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


let length = 300
let reference_str = String.init length (fun i -> i * 7 mod 256 |> char_of_int)
let create_b () = reference_str |> Bytes.of_string

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let create_bs () = reference_str |> bigstring_of_string
end


let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

external bs_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_tested_s : bigstring -> nativeint# -> int
  = "%caml_bigstring_get16_indexed_by_nativeint#"

external bs_tested_u : bigstring -> nativeint# -> int
  = "%caml_bigstring_get16u_indexed_by_nativeint#"

external b_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_tested_s : bytes -> nativeint# -> int
  = "%caml_bytes_get16_indexed_by_nativeint#"

external b_tested_u : bytes -> nativeint# -> int
  = "%caml_bytes_get16u_indexed_by_nativeint#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;


let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> nativeint# -> int32
  = "%caml_bigstring_get32_indexed_by_nativeint#"

external bs_tested_u : bigstring -> nativeint# -> int32
  = "%caml_bigstring_get32u_indexed_by_nativeint#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> nativeint# -> int32
  = "%caml_bytes_get32_indexed_by_nativeint#"

external b_tested_u : bytes -> nativeint# -> int32
  = "%caml_bytes_get32u_indexed_by_nativeint#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;


let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> nativeint# -> int64
  = "%caml_bigstring_get64_indexed_by_nativeint#"

external bs_tested_u : bigstring -> nativeint# -> int64
  = "%caml_bigstring_get64u_indexed_by_nativeint#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> nativeint# -> int64
  = "%caml_bytes_get64_indexed_by_nativeint#"

external b_tested_u : bytes -> nativeint# -> int64
  = "%caml_bytes_get64u_indexed_by_nativeint#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;


let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> nativeint# -> int32#
  = "%caml_bigstring_get32#_indexed_by_nativeint#"

external bs_tested_u : bigstring -> nativeint# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_nativeint#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> nativeint# -> int32#
  = "%caml_bytes_get32#_indexed_by_nativeint#"

external b_tested_u : bytes -> nativeint# -> int32#
  = "%caml_bytes_get32u#_indexed_by_nativeint#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;


let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> nativeint# -> int64#
  = "%caml_bigstring_get64#_indexed_by_nativeint#"

external bs_tested_u : bigstring -> nativeint# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_nativeint#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> nativeint# -> int64#
  = "%caml_bytes_get64#_indexed_by_nativeint#"

external b_tested_u : bytes -> nativeint# -> int64#
  = "%caml_bytes_get64u#_indexed_by_nativeint#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;


let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

external bs_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_tested_s : bigstring -> int32# -> int
  = "%caml_bigstring_get16_indexed_by_int32#"

external bs_tested_u : bigstring -> int32# -> int
  = "%caml_bigstring_get16u_indexed_by_int32#"

external b_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_tested_s : bytes -> int32# -> int
  = "%caml_bytes_get16_indexed_by_int32#"

external b_tested_u : bytes -> int32# -> int
  = "%caml_bytes_get16u_indexed_by_int32#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#2147483648l);;
check_get_bounds (-#2147483647l);;
check_get_bounds (#2147483647l);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> int32# -> int32
  = "%caml_bigstring_get32_indexed_by_int32#"

external bs_tested_u : bigstring -> int32# -> int32
  = "%caml_bigstring_get32u_indexed_by_int32#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> int32# -> int32
  = "%caml_bytes_get32_indexed_by_int32#"

external b_tested_u : bytes -> int32# -> int32
  = "%caml_bytes_get32u_indexed_by_int32#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#2147483648l);;
check_get_bounds (-#2147483647l);;
check_get_bounds (#2147483647l);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> int32# -> int64
  = "%caml_bigstring_get64_indexed_by_int32#"

external bs_tested_u : bigstring -> int32# -> int64
  = "%caml_bigstring_get64u_indexed_by_int32#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> int32# -> int64
  = "%caml_bytes_get64_indexed_by_int32#"

external b_tested_u : bytes -> int32# -> int64
  = "%caml_bytes_get64u_indexed_by_int32#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#2147483648l);;
check_get_bounds (-#2147483647l);;
check_get_bounds (#2147483647l);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> int32# -> int32#
  = "%caml_bigstring_get32#_indexed_by_int32#"

external bs_tested_u : bigstring -> int32# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_int32#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> int32# -> int32#
  = "%caml_bytes_get32#_indexed_by_int32#"

external b_tested_u : bytes -> int32# -> int32#
  = "%caml_bytes_get32u#_indexed_by_int32#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#2147483648l);;
check_get_bounds (-#2147483647l);;
check_get_bounds (#2147483647l);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> int32# -> int64#
  = "%caml_bigstring_get64#_indexed_by_int32#"

external bs_tested_u : bigstring -> int32# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_int32#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> int32# -> int64#
  = "%caml_bytes_get64#_indexed_by_int32#"

external b_tested_u : bytes -> int32# -> int64#
  = "%caml_bytes_get64u#_indexed_by_int32#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#2147483648l);;
check_get_bounds (-#2147483647l);;
check_get_bounds (#2147483647l);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

external bs_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_tested_s : bigstring -> int64# -> int
  = "%caml_bigstring_get16_indexed_by_int64#"

external bs_tested_u : bigstring -> int64# -> int
  = "%caml_bigstring_get16u_indexed_by_int64#"

external b_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_tested_s : bytes -> int64# -> int
  = "%caml_bytes_get16_indexed_by_int64#"

external b_tested_u : bytes -> int64# -> int
  = "%caml_bytes_get16u_indexed_by_int64#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#9223372036854775808L);;
check_get_bounds (-#9223372036854775807L);;
check_get_bounds (#9223372036854775807L);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> int64# -> int32
  = "%caml_bigstring_get32_indexed_by_int64#"

external bs_tested_u : bigstring -> int64# -> int32
  = "%caml_bigstring_get32u_indexed_by_int64#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> int64# -> int32
  = "%caml_bytes_get32_indexed_by_int64#"

external b_tested_u : bytes -> int64# -> int32
  = "%caml_bytes_get32u_indexed_by_int64#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#9223372036854775808L);;
check_get_bounds (-#9223372036854775807L);;
check_get_bounds (#9223372036854775807L);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> int64# -> int64
  = "%caml_bigstring_get64_indexed_by_int64#"

external bs_tested_u : bigstring -> int64# -> int64
  = "%caml_bigstring_get64u_indexed_by_int64#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> int64# -> int64
  = "%caml_bytes_get64_indexed_by_int64#"

external b_tested_u : bytes -> int64# -> int64
  = "%caml_bytes_get64u_indexed_by_int64#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#9223372036854775808L);;
check_get_bounds (-#9223372036854775807L);;
check_get_bounds (#9223372036854775807L);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let eq : int32 -> int32 -> bool = Int32.equal

external bs_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_tested_s : bigstring -> int64# -> int32#
  = "%caml_bigstring_get32#_indexed_by_int64#"

external bs_tested_u : bigstring -> int64# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_int64#"

external b_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_tested_s : bytes -> int64# -> int32#
  = "%caml_bytes_get32#_indexed_by_int64#"

external b_tested_u : bytes -> int64# -> int32#
  = "%caml_bytes_get32u#_indexed_by_int64#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#9223372036854775808L);;
check_get_bounds (-#9223372036854775807L);;
check_get_bounds (#9223372036854775807L);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let eq : int64 -> int64 -> bool = Int64.equal

external bs_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_tested_s : bigstring -> int64# -> int64#
  = "%caml_bigstring_get64#_indexed_by_int64#"

external bs_tested_u : bigstring -> int64# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_int64#"

external b_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_tested_s : bytes -> int64# -> int64#
  = "%caml_bytes_get64#_indexed_by_int64#"

external b_tested_u : bytes -> int64# -> int64#
  = "%caml_bytes_get64u#_indexed_by_int64#"

let check_get_bounds, check_get =
  let create_checkers create reference tested_s tested_u =
    let for_ref = create ()
    and for_s = create ()
    and for_u = create () in
    let check_get_bounds i =
      try let _ = tested_s for_s i in assert false with
      | Invalid_argument _ -> ()
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        try (
          let res = reference for_ref i in
          try (
            assert (eq res (to_boxed_result (tested_s for_s test_i)));
            assert (eq res (to_boxed_result (tested_u for_u test_i))))
          with
          | _ -> assert false)
        with
        | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
        | _ ->
          (try let _ = tested_s for_s test_i in assert false with
           | Invalid_argument _ -> assert false
           | _ -> ())) in
  let cb_for_bs, c_for_bs =
    create_checkers create_bs bs_reference bs_tested_s bs_tested_u in
  let cb_for_b, c_for_b =
    create_checkers create_b b_reference b_tested_s b_tested_u in
  ( (fun i -> cb_for_bs i; cb_for_b i)
  , (fun i -> c_for_bs i; c_for_b i) )
;;

for i = -1 to length + 1 do
  check_get i
done
;;

check_get_bounds (-#9223372036854775808L);;
check_get_bounds (-#9223372036854775807L);;
check_get_bounds (#9223372036854775807L);;
