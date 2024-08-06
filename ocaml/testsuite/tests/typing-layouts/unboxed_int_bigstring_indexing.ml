(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 include stdlib_stable;
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
let create_s () = reference_str

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
let of_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

let rec x = function
  | i when i <= 0 -> Int.zero
  | 1 ->
      (* min int *)
      Int.(shift_left one) (16 - 1)
  | 2 ->
      (* max int *)
      let shift = 16 - 1 in
      Int.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int.(logxor x (shift_left one (i1 mod 16))) in
      let x = Int.(logxor x (shift_left one (i2 mod 16))) in
      let x = Int.(logxor x (shift_left one (i3 mod 16))) in
      x

external bs_get_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_get_tested_s : bigstring -> nativeint# -> int
  = "%caml_bigstring_get16_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> int
  = "%caml_bigstring_get16u_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> int -> unit
  = "%caml_bigstring_set16"

external bs_set_tested_s
  : bigstring -> nativeint# -> int -> unit
  = "%caml_bigstring_set16_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> int -> unit
  = "%caml_bigstring_set16u_indexed_by_nativeint#"

external s_get_reference : string -> int -> int
  = "%caml_string_get16"

external s_get_tested_s : string -> nativeint# -> int
  = "%caml_string_get16_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> int
  = "%caml_string_get16u_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> int -> unit
  = "%caml_string_set16"

external s_set_tested_s
  : string -> nativeint# -> int -> unit
  = "%caml_string_set16_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> int -> unit
  = "%caml_string_set16u_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_get_tested_s : bytes -> nativeint# -> int
  = "%caml_bytes_get16_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> int
  = "%caml_bytes_get16u_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> int -> unit
  = "%caml_bytes_set16"

external b_set_tested_s
  : bytes -> nativeint# -> int -> unit
  = "%caml_bytes_set16_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> int -> unit
  = "%caml_bytes_set16u_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let of_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> nativeint# -> int32
  = "%caml_bigstring_get32_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> int32
  = "%caml_bigstring_get32u_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> nativeint# -> int32 -> unit
  = "%caml_bigstring_set32_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> int32 -> unit
  = "%caml_bigstring_set32u_indexed_by_nativeint#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> nativeint# -> int32
  = "%caml_string_get32_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> int32
  = "%caml_string_get32u_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> nativeint# -> int32
  = "%caml_bytes_get32_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> int32
  = "%caml_bytes_get32u_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> nativeint# -> int32 -> unit
  = "%caml_bytes_set32_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> int32 -> unit
  = "%caml_bytes_set32u_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let of_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> nativeint# -> int64
  = "%caml_bigstring_get64_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> int64
  = "%caml_bigstring_get64u_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> nativeint# -> int64 -> unit
  = "%caml_bigstring_set64_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> int64 -> unit
  = "%caml_bigstring_set64u_indexed_by_nativeint#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> nativeint# -> int64
  = "%caml_string_get64_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> int64
  = "%caml_string_get64u_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> nativeint# -> int64
  = "%caml_bytes_get64_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> int64
  = "%caml_bytes_get64u_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> nativeint# -> int64 -> unit
  = "%caml_bytes_set64_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> int64 -> unit
  = "%caml_bytes_set64u_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let of_boxed_result : int32 -> int32# = Stdlib_upstream_compatible.Int32_u.of_int32
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> nativeint# -> int32#
  = "%caml_bigstring_get32#_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> nativeint# -> int32# -> unit
  = "%caml_bigstring_set32#_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> int32# -> unit
  = "%caml_bigstring_set32u#_indexed_by_nativeint#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> nativeint# -> int32#
  = "%caml_string_get32#_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> int32#
  = "%caml_string_get32u#_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> nativeint# -> int32#
  = "%caml_bytes_get32#_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> int32#
  = "%caml_bytes_get32u#_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> nativeint# -> int32# -> unit
  = "%caml_bytes_set32#_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> int32# -> unit
  = "%caml_bytes_set32u#_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let of_boxed_result : int64 -> int64# = Stdlib_upstream_compatible.Int64_u.of_int64
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> nativeint# -> int64#
  = "%caml_bigstring_get64#_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> nativeint# -> int64# -> unit
  = "%caml_bigstring_set64#_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> int64# -> unit
  = "%caml_bigstring_set64u#_indexed_by_nativeint#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> nativeint# -> int64#
  = "%caml_string_get64#_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> int64#
  = "%caml_string_get64u#_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> nativeint# -> int64#
  = "%caml_bytes_get64#_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> int64#
  = "%caml_bytes_get64u#_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> nativeint# -> int64# -> unit
  = "%caml_bytes_set64#_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> int64# -> unit
  = "%caml_bytes_set64u#_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : float32 -> float32 = fun x -> x
let of_boxed_result : float32 -> float32 = fun x -> x
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> nativeint# -> float32
  = "%caml_bigstring_getf32_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> float32
  = "%caml_bigstring_getf32u_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> nativeint# -> float32 -> unit
  = "%caml_bigstring_setf32_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> float32 -> unit
  = "%caml_bigstring_setf32u_indexed_by_nativeint#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> nativeint# -> float32
  = "%caml_string_getf32_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> float32
  = "%caml_string_getf32u_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> nativeint# -> float32
  = "%caml_bytes_getf32_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> float32
  = "%caml_bytes_getf32u_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> nativeint# -> float32 -> unit
  = "%caml_bytes_setf32_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> float32 -> unit
  = "%caml_bytes_setf32u_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> nativeint# = Stdlib_upstream_compatible.Nativeint_u.of_int
let to_boxed_result : float32# -> float32 = Stdlib_stable.Float32_u.to_float32
let of_boxed_result : float32 -> float32# = Stdlib_stable.Float32_u.of_float32
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> nativeint# -> float32#
  = "%caml_bigstring_getf32#_indexed_by_nativeint#"

external bs_get_tested_u : bigstring -> nativeint# -> float32#
  = "%caml_bigstring_getf32u#_indexed_by_nativeint#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> nativeint# -> float32# -> unit
  = "%caml_bigstring_setf32#_indexed_by_nativeint#"

external bs_set_tested_u
  : bigstring -> nativeint# -> float32# -> unit
  = "%caml_bigstring_setf32u#_indexed_by_nativeint#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> nativeint# -> float32#
  = "%caml_string_getf32#_indexed_by_nativeint#"

external s_get_tested_u : string -> nativeint# -> float32#
  = "%caml_string_getf32u#_indexed_by_nativeint#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_nativeint#"

external s_set_tested_u
  : string -> nativeint# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_nativeint#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> nativeint# -> float32#
  = "%caml_bytes_getf32#_indexed_by_nativeint#"

external b_get_tested_u : bytes -> nativeint# -> float32#
  = "%caml_bytes_getf32u#_indexed_by_nativeint#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> nativeint# -> float32# -> unit
  = "%caml_bytes_setf32#_indexed_by_nativeint#"

external b_set_tested_u
  : bytes -> nativeint# -> float32# -> unit
  = "%caml_bytes_setf32u#_indexed_by_nativeint#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#1n);;
check_set_bounds (-#1n) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int -> int = fun x -> x
let of_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

let rec x = function
  | i when i <= 0 -> Int.zero
  | 1 ->
      (* min int *)
      Int.(shift_left one) (16 - 1)
  | 2 ->
      (* max int *)
      let shift = 16 - 1 in
      Int.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int.(logxor x (shift_left one (i1 mod 16))) in
      let x = Int.(logxor x (shift_left one (i2 mod 16))) in
      let x = Int.(logxor x (shift_left one (i3 mod 16))) in
      x

external bs_get_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_get_tested_s : bigstring -> int32# -> int
  = "%caml_bigstring_get16_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> int
  = "%caml_bigstring_get16u_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> int -> unit
  = "%caml_bigstring_set16"

external bs_set_tested_s
  : bigstring -> int32# -> int -> unit
  = "%caml_bigstring_set16_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> int -> unit
  = "%caml_bigstring_set16u_indexed_by_int32#"

external s_get_reference : string -> int -> int
  = "%caml_string_get16"

external s_get_tested_s : string -> int32# -> int
  = "%caml_string_get16_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> int
  = "%caml_string_get16u_indexed_by_int32#"

external s_set_reference
  : string -> int -> int -> unit
  = "%caml_string_set16"

external s_set_tested_s
  : string -> int32# -> int -> unit
  = "%caml_string_set16_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> int -> unit
  = "%caml_string_set16u_indexed_by_int32#"

external b_get_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_get_tested_s : bytes -> int32# -> int
  = "%caml_bytes_get16_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> int
  = "%caml_bytes_get16u_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> int -> unit
  = "%caml_bytes_set16"

external b_set_tested_s
  : bytes -> int32# -> int -> unit
  = "%caml_bytes_set16_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> int -> unit
  = "%caml_bytes_set16u_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let of_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> int32# -> int32
  = "%caml_bigstring_get32_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> int32
  = "%caml_bigstring_get32u_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> int32# -> int32 -> unit
  = "%caml_bigstring_set32_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> int32 -> unit
  = "%caml_bigstring_set32u_indexed_by_int32#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> int32# -> int32
  = "%caml_string_get32_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> int32
  = "%caml_string_get32u_indexed_by_int32#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> int32# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int32#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> int32# -> int32
  = "%caml_bytes_get32_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> int32
  = "%caml_bytes_get32u_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> int32# -> int32 -> unit
  = "%caml_bytes_set32_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> int32 -> unit
  = "%caml_bytes_set32u_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let of_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> int32# -> int64
  = "%caml_bigstring_get64_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> int64
  = "%caml_bigstring_get64u_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> int32# -> int64 -> unit
  = "%caml_bigstring_set64_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> int64 -> unit
  = "%caml_bigstring_set64u_indexed_by_int32#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> int32# -> int64
  = "%caml_string_get64_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> int64
  = "%caml_string_get64u_indexed_by_int32#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> int32# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int32#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> int32# -> int64
  = "%caml_bytes_get64_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> int64
  = "%caml_bytes_get64u_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> int32# -> int64 -> unit
  = "%caml_bytes_set64_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> int64 -> unit
  = "%caml_bytes_set64u_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let of_boxed_result : int32 -> int32# = Stdlib_upstream_compatible.Int32_u.of_int32
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> int32# -> int32#
  = "%caml_bigstring_get32#_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> int32# -> int32# -> unit
  = "%caml_bigstring_set32#_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> int32# -> unit
  = "%caml_bigstring_set32u#_indexed_by_int32#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> int32# -> int32#
  = "%caml_string_get32#_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> int32#
  = "%caml_string_get32u#_indexed_by_int32#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> int32# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int32#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> int32# -> int32#
  = "%caml_bytes_get32#_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> int32#
  = "%caml_bytes_get32u#_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> int32# -> int32# -> unit
  = "%caml_bytes_set32#_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> int32# -> unit
  = "%caml_bytes_set32u#_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let of_boxed_result : int64 -> int64# = Stdlib_upstream_compatible.Int64_u.of_int64
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> int32# -> int64#
  = "%caml_bigstring_get64#_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> int32# -> int64# -> unit
  = "%caml_bigstring_set64#_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> int64# -> unit
  = "%caml_bigstring_set64u#_indexed_by_int32#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> int32# -> int64#
  = "%caml_string_get64#_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> int64#
  = "%caml_string_get64u#_indexed_by_int32#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> int32# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int32#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> int32# -> int64#
  = "%caml_bytes_get64#_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> int64#
  = "%caml_bytes_get64u#_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> int32# -> int64# -> unit
  = "%caml_bytes_set64#_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> int64# -> unit
  = "%caml_bytes_set64u#_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : float32 -> float32 = fun x -> x
let of_boxed_result : float32 -> float32 = fun x -> x
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> int32# -> float32
  = "%caml_bigstring_getf32_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> float32
  = "%caml_bigstring_getf32u_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> int32# -> float32 -> unit
  = "%caml_bigstring_setf32_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> float32 -> unit
  = "%caml_bigstring_setf32u_indexed_by_int32#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> int32# -> float32
  = "%caml_string_getf32_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> float32
  = "%caml_string_getf32u_indexed_by_int32#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> int32# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int32#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> int32# -> float32
  = "%caml_bytes_getf32_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> float32
  = "%caml_bytes_getf32u_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> int32# -> float32 -> unit
  = "%caml_bytes_setf32_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> float32 -> unit
  = "%caml_bytes_setf32u_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int32# = Stdlib_upstream_compatible.Int32_u.of_int
let to_boxed_result : float32# -> float32 = Stdlib_stable.Float32_u.to_float32
let of_boxed_result : float32 -> float32# = Stdlib_stable.Float32_u.of_float32
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> int32# -> float32#
  = "%caml_bigstring_getf32#_indexed_by_int32#"

external bs_get_tested_u : bigstring -> int32# -> float32#
  = "%caml_bigstring_getf32u#_indexed_by_int32#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> int32# -> float32# -> unit
  = "%caml_bigstring_setf32#_indexed_by_int32#"

external bs_set_tested_u
  : bigstring -> int32# -> float32# -> unit
  = "%caml_bigstring_setf32u#_indexed_by_int32#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> int32# -> float32#
  = "%caml_string_getf32#_indexed_by_int32#"

external s_get_tested_u : string -> int32# -> float32#
  = "%caml_string_getf32u#_indexed_by_int32#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> int32# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int32#"

external s_set_tested_u
  : string -> int32# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int32#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> int32# -> float32#
  = "%caml_bytes_getf32#_indexed_by_int32#"

external b_get_tested_u : bytes -> int32# -> float32#
  = "%caml_bytes_getf32u#_indexed_by_int32#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> int32# -> float32# -> unit
  = "%caml_bytes_setf32#_indexed_by_int32#"

external b_set_tested_u
  : bytes -> int32# -> float32# -> unit
  = "%caml_bytes_setf32u#_indexed_by_int32#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#2147483648l);;
check_set_bounds (-#2147483648l) (x 1);;
check_get_bounds (-#2147483647l);;
check_set_bounds (-#2147483647l) (x 1);;
check_get_bounds (#2147483647l);;
check_set_bounds (#2147483647l) (x 1);;
check_get_bounds (-#1l);;
check_set_bounds (-#1l) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int -> int = fun x -> x
let of_boxed_result : int -> int = fun x -> x
let eq : int -> int -> bool = Int.equal

let rec x = function
  | i when i <= 0 -> Int.zero
  | 1 ->
      (* min int *)
      Int.(shift_left one) (16 - 1)
  | 2 ->
      (* max int *)
      let shift = 16 - 1 in
      Int.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int.(logxor x (shift_left one (i1 mod 16))) in
      let x = Int.(logxor x (shift_left one (i2 mod 16))) in
      let x = Int.(logxor x (shift_left one (i3 mod 16))) in
      x

external bs_get_reference : bigstring -> int -> int
  = "%caml_bigstring_get16"

external bs_get_tested_s : bigstring -> int64# -> int
  = "%caml_bigstring_get16_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> int
  = "%caml_bigstring_get16u_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> int -> unit
  = "%caml_bigstring_set16"

external bs_set_tested_s
  : bigstring -> int64# -> int -> unit
  = "%caml_bigstring_set16_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> int -> unit
  = "%caml_bigstring_set16u_indexed_by_int64#"

external s_get_reference : string -> int -> int
  = "%caml_string_get16"

external s_get_tested_s : string -> int64# -> int
  = "%caml_string_get16_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> int
  = "%caml_string_get16u_indexed_by_int64#"

external s_set_reference
  : string -> int -> int -> unit
  = "%caml_string_set16"

external s_set_tested_s
  : string -> int64# -> int -> unit
  = "%caml_string_set16_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> int -> unit
  = "%caml_string_set16u_indexed_by_int64#"

external b_get_reference : bytes -> int -> int
  = "%caml_bytes_get16"

external b_get_tested_s : bytes -> int64# -> int
  = "%caml_bytes_get16_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> int
  = "%caml_bytes_get16u_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> int -> unit
  = "%caml_bytes_set16"

external b_set_tested_s
  : bytes -> int64# -> int -> unit
  = "%caml_bytes_set16_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> int -> unit
  = "%caml_bytes_set16u_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int32 -> int32 = fun x -> x
let of_boxed_result : int32 -> int32 = fun x -> x
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> int64# -> int32
  = "%caml_bigstring_get32_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> int32
  = "%caml_bigstring_get32u_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> int64# -> int32 -> unit
  = "%caml_bigstring_set32_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> int32 -> unit
  = "%caml_bigstring_set32u_indexed_by_int64#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> int64# -> int32
  = "%caml_string_get32_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> int32
  = "%caml_string_get32u_indexed_by_int64#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> int64# -> int32 -> unit
  = "%caml_string_set32_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> int32 -> unit
  = "%caml_string_set32u_indexed_by_int64#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> int64# -> int32
  = "%caml_bytes_get32_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> int32
  = "%caml_bytes_get32u_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> int64# -> int32 -> unit
  = "%caml_bytes_set32_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> int32 -> unit
  = "%caml_bytes_set32u_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int64 -> int64 = fun x -> x
let of_boxed_result : int64 -> int64 = fun x -> x
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> int64# -> int64
  = "%caml_bigstring_get64_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> int64
  = "%caml_bigstring_get64u_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> int64# -> int64 -> unit
  = "%caml_bigstring_set64_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> int64 -> unit
  = "%caml_bigstring_set64u_indexed_by_int64#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> int64# -> int64
  = "%caml_string_get64_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> int64
  = "%caml_string_get64u_indexed_by_int64#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> int64# -> int64 -> unit
  = "%caml_string_set64_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> int64 -> unit
  = "%caml_string_set64u_indexed_by_int64#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> int64# -> int64
  = "%caml_bytes_get64_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> int64
  = "%caml_bytes_get64u_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> int64# -> int64 -> unit
  = "%caml_bytes_set64_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> int64 -> unit
  = "%caml_bytes_set64u_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int32# -> int32 = Stdlib_upstream_compatible.Int32_u.to_int32
let of_boxed_result : int32 -> int32# = Stdlib_upstream_compatible.Int32_u.of_int32
let eq : int32 -> int32 -> bool = Int32.equal

let rec x = function
  | i when i <= 0 -> Int32.zero
  | 1 ->
      (* min int *)
      Int32.(shift_left one) (32 - 1)
  | 2 ->
      (* max int *)
      let shift = 32 - 1 in
      Int32.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int32.(logxor x (shift_left one (i1 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i2 mod 32))) in
      let x = Int32.(logxor x (shift_left one (i3 mod 32))) in
      x

external bs_get_reference : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bs_get_tested_s : bigstring -> int64# -> int32#
  = "%caml_bigstring_get32#_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> int32#
  = "%caml_bigstring_get32u#_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bs_set_tested_s
  : bigstring -> int64# -> int32# -> unit
  = "%caml_bigstring_set32#_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> int32# -> unit
  = "%caml_bigstring_set32u#_indexed_by_int64#"

external s_get_reference : string -> int -> int32
  = "%caml_string_get32"

external s_get_tested_s : string -> int64# -> int32#
  = "%caml_string_get32#_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> int32#
  = "%caml_string_get32u#_indexed_by_int64#"

external s_set_reference
  : string -> int -> int32 -> unit
  = "%caml_string_set32"

external s_set_tested_s
  : string -> int64# -> int32# -> unit
  = "%caml_string_set32#_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> int32# -> unit
  = "%caml_string_set32u#_indexed_by_int64#"

external b_get_reference : bytes -> int -> int32
  = "%caml_bytes_get32"

external b_get_tested_s : bytes -> int64# -> int32#
  = "%caml_bytes_get32#_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> int32#
  = "%caml_bytes_get32u#_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32"

external b_set_tested_s
  : bytes -> int64# -> int32# -> unit
  = "%caml_bytes_set32#_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> int32# -> unit
  = "%caml_bytes_set32u#_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : int64# -> int64 = Stdlib_upstream_compatible.Int64_u.to_int64
let of_boxed_result : int64 -> int64# = Stdlib_upstream_compatible.Int64_u.of_int64
let eq : int64 -> int64 -> bool = Int64.equal

let rec x = function
  | i when i <= 0 -> Int64.zero
  | 1 ->
      (* min int *)
      Int64.(shift_left one) (64 - 1)
  | 2 ->
      (* max int *)
      let shift = 64 - 1 in
      Int64.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = Int64.(logxor x (shift_left one (i1 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i2 mod 64))) in
      let x = Int64.(logxor x (shift_left one (i3 mod 64))) in
      x

external bs_get_reference : bigstring -> int -> int64
  = "%caml_bigstring_get64"

external bs_get_tested_s : bigstring -> int64# -> int64#
  = "%caml_bigstring_get64#_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> int64#
  = "%caml_bigstring_get64u#_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> int64 -> unit
  = "%caml_bigstring_set64"

external bs_set_tested_s
  : bigstring -> int64# -> int64# -> unit
  = "%caml_bigstring_set64#_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> int64# -> unit
  = "%caml_bigstring_set64u#_indexed_by_int64#"

external s_get_reference : string -> int -> int64
  = "%caml_string_get64"

external s_get_tested_s : string -> int64# -> int64#
  = "%caml_string_get64#_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> int64#
  = "%caml_string_get64u#_indexed_by_int64#"

external s_set_reference
  : string -> int -> int64 -> unit
  = "%caml_string_set64"

external s_set_tested_s
  : string -> int64# -> int64# -> unit
  = "%caml_string_set64#_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> int64# -> unit
  = "%caml_string_set64u#_indexed_by_int64#"

external b_get_reference : bytes -> int -> int64
  = "%caml_bytes_get64"

external b_get_tested_s : bytes -> int64# -> int64#
  = "%caml_bytes_get64#_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> int64#
  = "%caml_bytes_get64u#_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64"

external b_set_tested_s
  : bytes -> int64# -> int64# -> unit
  = "%caml_bytes_set64#_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> int64# -> unit
  = "%caml_bytes_set64u#_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : float32 -> float32 = fun x -> x
let of_boxed_result : float32 -> float32 = fun x -> x
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> int64# -> float32
  = "%caml_bigstring_getf32_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> float32
  = "%caml_bigstring_getf32u_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> int64# -> float32 -> unit
  = "%caml_bigstring_setf32_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> float32 -> unit
  = "%caml_bigstring_setf32u_indexed_by_int64#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> int64# -> float32
  = "%caml_string_getf32_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> float32
  = "%caml_string_getf32u_indexed_by_int64#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> int64# -> float32 -> unit
  = "%caml_string_setf32_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> float32 -> unit
  = "%caml_string_setf32u_indexed_by_int64#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> int64# -> float32
  = "%caml_bytes_getf32_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> float32
  = "%caml_bytes_getf32u_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> int64# -> float32 -> unit
  = "%caml_bytes_setf32_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> float32 -> unit
  = "%caml_bytes_setf32u_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;

let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
let to_boxed_result : float32# -> float32 = Stdlib_stable.Float32_u.to_float32
let of_boxed_result : float32 -> float32# = Stdlib_stable.Float32_u.of_float32
let eq : float32 -> float32 -> bool = Stdlib_stable.Float32.equal
let x _ = Stdlib_stable.Float32.of_float 5.0

external bs_get_reference : bigstring -> int -> float32
  = "%caml_bigstring_getf32"

external bs_get_tested_s : bigstring -> int64# -> float32#
  = "%caml_bigstring_getf32#_indexed_by_int64#"

external bs_get_tested_u : bigstring -> int64# -> float32#
  = "%caml_bigstring_getf32u#_indexed_by_int64#"

external bs_set_reference
  : bigstring -> int -> float32 -> unit
  = "%caml_bigstring_setf32"

external bs_set_tested_s
  : bigstring -> int64# -> float32# -> unit
  = "%caml_bigstring_setf32#_indexed_by_int64#"

external bs_set_tested_u
  : bigstring -> int64# -> float32# -> unit
  = "%caml_bigstring_setf32u#_indexed_by_int64#"

external s_get_reference : string -> int -> float32
  = "%caml_string_getf32"

external s_get_tested_s : string -> int64# -> float32#
  = "%caml_string_getf32#_indexed_by_int64#"

external s_get_tested_u : string -> int64# -> float32#
  = "%caml_string_getf32u#_indexed_by_int64#"

external s_set_reference
  : string -> int -> float32 -> unit
  = "%caml_string_setf32"

external s_set_tested_s
  : string -> int64# -> float32# -> unit
  = "%caml_string_setf32#_indexed_by_int64#"

external s_set_tested_u
  : string -> int64# -> float32# -> unit
  = "%caml_string_setf32u#_indexed_by_int64#"

external b_get_reference : bytes -> int -> float32
  = "%caml_bytes_getf32"

external b_get_tested_s : bytes -> int64# -> float32#
  = "%caml_bytes_getf32#_indexed_by_int64#"

external b_get_tested_u : bytes -> int64# -> float32#
  = "%caml_bytes_getf32u#_indexed_by_int64#"

external b_set_reference
  : bytes -> int -> float32 -> unit
  = "%caml_bytes_setf32"

external b_set_tested_s
  : bytes -> int64# -> float32# -> unit
  = "%caml_bytes_setf32#_indexed_by_int64#"

external b_set_tested_u
  : bytes -> int64# -> float32# -> unit
  = "%caml_bytes_setf32u#_indexed_by_int64#"

let check_get_bounds, check_get, check_set_bounds, check_set =
  let create_checkers create reference_get reference_set get_s get_u set_s
      set_u =
    let for_ref = create () and for_s = create () and for_u = create () in
    let check_get_bounds i =
      try
        let _ = get_s for_s i in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      let test_x = of_boxed_result x in
      try
        let _ = set_s for_s i test_x in
        assert false
      with Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = of_boxed_index i in
      try
        let res = reference_get for_ref i in
        try
          assert (eq res (to_boxed_result (get_s for_s test_i)));
          assert (eq res (to_boxed_result (get_u for_u test_i)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_get_bounds (of_boxed_index i)
      | _ -> (
          try
            let _ = get_s for_s test_i in
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    let check_set i x =
      let test_i = of_boxed_index i in
      let test_x = of_boxed_result x in
      try
        reference_set for_ref i x;
        try
          set_s for_s test_i test_x;
          assert (eq x (reference_get for_s i));
          set_u for_u test_i test_x;
          assert (eq x (reference_get for_u i));
          (* Check that we didn't ruin adjacent indices *)
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_s (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_s (i + 1)));
          assert (
            eq (reference_get for_ref (i - 1)) (reference_get for_u (i - 1)));
          assert (
            eq (reference_get for_ref (i + 1)) (reference_get for_u (i + 1)))
        with _ -> assert false
      with
      | Invalid_argument _ -> check_set_bounds (of_boxed_index i) x
      | _ -> (
          try
            set_s for_s test_i test_x;
            assert false
          with
          | Invalid_argument _ -> assert false
          | _ -> ())
    in
    (check_get_bounds, check_get, check_set_bounds, check_set)
  in
  let gb_for_bs, g_for_bs, sb_for_bs, s_for_bs =
    create_checkers create_bs bs_get_reference bs_set_reference
      bs_get_tested_s bs_get_tested_u bs_set_tested_s bs_set_tested_u in
  let gb_for_s, g_for_s, sb_for_s, s_for_s =
    create_checkers create_s s_get_reference s_set_reference
      s_get_tested_s s_get_tested_u s_set_tested_s s_set_tested_u in
  let gb_for_b, g_for_b, sb_for_b, s_for_b =
    create_checkers create_b b_get_reference b_set_reference
      b_get_tested_s b_get_tested_u b_set_tested_s b_set_tested_u in
  ( (fun i -> gb_for_bs i; gb_for_s i; gb_for_b i)
  , (fun i -> g_for_bs i; g_for_s i; g_for_b i)
  , (fun i x -> sb_for_bs i x; sb_for_s i x; sb_for_b i x)
  , (fun i x -> s_for_bs i x; s_for_s i x; s_for_b i x) )
;;

for i = -1 to length + 1 do
  check_get i;
  check_set i (x i)
done
;;

check_get_bounds (-#9223372036854775808L);;
check_set_bounds (-#9223372036854775808L) (x 1);;
check_get_bounds (-#9223372036854775807L);;
check_set_bounds (-#9223372036854775807L) (x 1);;
check_get_bounds (#9223372036854775807L);;
check_set_bounds (#9223372036854775807L) (x 1);;
check_get_bounds (-#1L);;
check_set_bounds (-#1L) (x 1);;
