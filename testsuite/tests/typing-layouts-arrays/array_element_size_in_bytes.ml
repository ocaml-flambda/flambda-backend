(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 stack-allocation;
 amd64;
 {
   bytecode;
 } {
   native;
 }
*)

(* CR layouts v4: The below definition is just to give this test slightly
   different behavior on native code and bytecode, because some arrays of
   unboxed things are represented as custom blocks on only native code, and
   therefore the size calculations differ slightly. Delete this when we change
   the representation to not use custom blocks. *)
let custom_block_padding =
  match Sys.backend_type with
  | Native -> 1
  | Bytecode -> 0
  | Other _ -> failwith "Don't know what to do"

(* We only compile for 64 bits. *)
let bytes_per_word = 8

external[@layout_poly] size_in_bytes : ('a : any_non_null). 'a array -> int
  = "%array_element_size_in_bytes"

external[@layout_poly] makearray_dynamic :
  ('a : any_non_null). int -> 'a -> 'a array = "%makearray_dynamic"

let array_sizes_to_check = [0; 1; 2; 25]

(* values *)
let check_value ~init ~element_size =
  (* It is unfortunately necessary to duplicate this function many times because
     we don't have layout polymorphism. *)
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let int_array_element_size = size_in_bytes ([||] : int array)
let _ = check_value ~init:42 ~element_size:int_array_element_size

let string_array_element_size = size_in_bytes ([||] : string array)
let _ = check_value ~init:"abc" ~element_size:int_array_element_size

let float_array_element_size = size_in_bytes ([||] : float array)
let _ = check_value ~init:42.0 ~element_size:int_array_element_size

let float32_array_element_size = size_in_bytes ([||] : float32 array)
let _ = check_value ~init:42.0s ~element_size:int_array_element_size

let int32_array_element_size = size_in_bytes ([||] : int32 array)
let _ = check_value ~init:42l ~element_size:int_array_element_size

(* unboxed floats *)
let check_floatu ~init ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let floatu_array_element_size = size_in_bytes ([||] : float# array)

let _ = check_floatu ~init:#42.0 ~element_size:floatu_array_element_size

(* unboxed int64s *)
let check_int64u ~(init : int64#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((custom_block_padding + (element_size * n / bytes_per_word))
            = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let int64u_array_element_size = size_in_bytes ([||] : int64# array)

let _ = check_int64u ~init:#42L ~element_size:int64u_array_element_size

(* unboxed float32s *)
let check_float32u ~(init : float32#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* These arrays are packed in native code *)
    let n =
      match Sys.backend_type with
      | Native -> if n mod 2 = 0 then n else n + 1
      | Bytecode -> n
      | Other _ -> failwith "Don't know what to do"
    in
    assert ((custom_block_padding + (element_size * n / bytes_per_word))
            = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let float32u_array_element_size = size_in_bytes ([||] : float32# array)

let _ = check_float32u ~init:#42.0s ~element_size:float32u_array_element_size

(* unboxed int32s *)
let check_int32u ~(init : int32#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* These arrays are packed in native code *)
    let n =
      match Sys.backend_type with
      | Native -> if n mod 2 = 0 then n else n + 1
      | Bytecode -> n
      | Other _ -> failwith "Don't know what to do"
    in
    assert ((custom_block_padding + (element_size * n / bytes_per_word))
            = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let int32u_array_element_size = size_in_bytes ([||] : int32# array)

let _ = check_int32u ~init:#42l ~element_size:int32u_array_element_size

(* simple scannable products *)
let check_scannable_product1 ~(init : #(int * string * int * float array))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let scannable_product1_array_element_size =
  size_in_bytes ([||] : #(int * string * int * float array) array)

let _ = check_scannable_product1 ~init:#(42, "hi", 0, [| 1.0; 2.0; 3.0 |])
          ~element_size:scannable_product1_array_element_size

(* complex scannable products *)
type t_scan = #{ x : int; y : #(float * string); z: int option }

let check_scannable_product2 ~(init : #(int * t_scan * string * t_scan))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let mk_el () =
  #(42,
    #{ x = 42; y = #(42.0, "hi"); z = Some 42 },
    "hi",
    #{ x = 42; y = #(42.0, "hi"); z = Some 42 })

let scannable_product2_array_element_size =
  size_in_bytes ([||] : #(int * t_scan * string * t_scan) array)

let _ = check_scannable_product2 ~init:(mk_el ())
          ~element_size:scannable_product2_array_element_size

(* simple ignorable products *)
let check_ignorable_product1 ~(init : #(int * float32# * int * int64#))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let ignorable_product1_array_element_size =
  size_in_bytes ([||] : #(int * float32# * int * int64#) array)

let _ = check_ignorable_product1 ~init:#(42, #42.0s, 0, #42L)
          ~element_size:ignorable_product1_array_element_size

(* complex ignorable products *)
type t_ignore = #{ x : int; y : #(float# * int32#); z: int32# }

let check_ignorable_product2 ~(init : #(int * t_ignore * bool * t_ignore))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let mk_el () =
  #(42,
    #{ x = 42; y = #(#41.0, #40l); z = #43l },
    true,
    #{ x = 42; y = #(#41.0, #40l); z = #43l })

let ignorable_product2_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_ignorable_product2 ~init:(mk_el ())
          ~element_size:ignorable_product2_array_element_size

(* check lack of float32# packing in unboxed product arrays *)
let check_float32u_pair ~(init : #(float32# * float32#)) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* 2 because there are two components in the unboxed product *)
    match Sys.backend_type with
    | Native -> assert (n * 2 = (Obj.size (Obj.repr x)))
    | Bytecode | Other _ -> assert (n = Obj.size (Obj.repr x))
  in
  List.iter check_one array_sizes_to_check

let float32u_pair_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_float32u_pair ~init:#(#1.0s, #42.1s)
          ~element_size:float32u_pair_array_element_size

(* check lack of int32# packing in unboxed product arrays *)
let check_int32u_pair ~(init : #(int32# * int32#)) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* 2 because there are two components in the unboxed product *)
    match Sys.backend_type with
    | Native -> assert (n * 2 = (Obj.size (Obj.repr x)))
    | Bytecode | Other _ -> assert (n = Obj.size (Obj.repr x))
  in
  List.iter check_one array_sizes_to_check

let int32u_pair_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_int32u_pair ~init:#(#1l, #42l)
          ~element_size:int32u_pair_array_element_size
