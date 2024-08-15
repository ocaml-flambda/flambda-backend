let indent spaces string =
  let indent_string = String.make spaces ' ' in
  String.split_on_char '\n' string
  |> List.concat_map (fun line ->
    if String.equal line "" then [ "\n" ] else [ indent_string; line; "\n" ])
  |> String.concat ""

let template ~tests = {|(* TEST
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

let lengths = List.init 17 (fun x -> x) @ List.init 17 (fun x -> 300 + x)

type exn += Test_failed

let create_s length =
  String.init length (fun i -> i * 7 mod 256 |> char_of_int)
;;

let create_b length = create_s length |> Bytes.of_string

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let create_bs length = create_s length |> bigstring_of_string
end

module Tester (Primitives : sig
    type boxed_index
    type boxed_data
    type container

    val create : int -> container
    val generate_data : int -> boxed_data
    val to_index : int -> boxed_index
    val data_equal : boxed_data -> boxed_data -> bool

    type 'a getter := container -> 'a -> boxed_data
    type 'a setter := container -> 'a -> boxed_data -> unit

    val get_reference : int getter
    val get_safe : boxed_index getter
    val get_unsafe : boxed_index getter
    val set_reference : int setter
    val set_safe : boxed_index setter
    val set_unsafe : boxed_index setter
    val extra_bounds_checks : boxed_index list
  end) : sig end = struct
  open Primitives

  let make_tester_functions length =
    let for_reference = create length
    and for_safe = create length
    and for_unsafe = create length in
    let check_get_bounds i =
      try
        let _ = get_safe for_safe i in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      try
        let _ = set_safe for_safe i x in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = to_index i in
      try
        let res = get_reference for_reference i in
        try
          assert (data_equal res (get_safe for_safe test_i));
          assert (data_equal res (get_unsafe for_unsafe test_i))
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_get_bounds test_i
      | _ ->
        (try
           let _ = get_safe for_safe test_i in
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    let check_set i x =
      let test_i = to_index i in
      try
        set_reference for_reference i x;
        try
          set_safe for_safe test_i x;
          assert (data_equal x (get_reference for_safe i));
          set_unsafe for_unsafe test_i x;
          assert (data_equal x (get_reference for_unsafe i));
          (* Check that we didn't ruin adjacent indices *)
          check_get (i - 1);
          check_get (i + 1)
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_set_bounds test_i x
      | _ ->
        (try
           set_safe for_safe test_i x;
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    check_get_bounds, check_get, check_set_bounds, check_set
  ;;

  let test length =
    Random.init 1234;
    let check_get_bounds, check_get, check_set_bounds, check_set =
      make_tester_functions length
    in
    for i = -1 to length + 1 do
      check_get i;
      check_set i (generate_data i)
    done;
    List.iter
      (fun bound ->
        check_get_bounds bound;
        check_set_bounds bound (generate_data 1))
      extra_bounds_checks
  ;;

  let () = List.iter test lengths
end

|}^tests

let test_group_template ~setup ~tests = {|
open struct
|}^indent 2 setup^{|
|}^indent 2 (String.concat "" tests)^{|
end
|}

let type_utilities_template
  ~boxed_index
  ~boxed_data
  ~generate_data
  ~to_index
  ~unbox_index
  ~unbox_data
  ~box_data
  ~data_equal
  ~extra_bounds
  = {|
type boxed_index = |}^boxed_index^{|
type boxed_data = |}^boxed_data^{|

let generate_data = |}^generate_data^{|
let to_index = |}^to_index^{|
let data_equal = |}^data_equal^{|
let unbox_index = |}^unbox_index^{|
let unbox_data = |}^unbox_data^{|
let box_data = |}^box_data^{|
let extra_bounds_checks = |}^extra_bounds

let one_test_template
  ~index
  ~boxed_data
  ~tested_data
  ~index_sigil
  ~data_sigil
  ~unboxed_sigil
  ~container
  ~create
  = {|
module _ = Tester (struct
    type nonrec boxed_index = boxed_index
    type nonrec boxed_data = boxed_data
    type container = |}^container^{|

    let create = |}^create^{|
    let generate_data = generate_data
    let to_index = to_index
    let data_equal = data_equal
    let extra_bounds_checks = extra_bounds_checks

    external get_reference
      : |}^container^{|
      -> int
      -> |}^boxed_data^{|
      = "%caml_|}^container^{|_get|}^data_sigil^{|"

    external get_safe
      :  |}^container^{|
      -> |}^index^{|
      -> |}^tested_data^{|
      = "%caml_|}^container^{|_get|}^data_sigil^unboxed_sigil^index_sigil^{|"

    let get_safe b i = box_data (get_safe b (unbox_index i))

    external get_unsafe
      :  |}^container^{|
      -> |}^index^{|
      -> |}^tested_data^{|
      = "%caml_|}^container^{|_get|}^data_sigil^{|u|}^unboxed_sigil^index_sigil^{|"

    let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    external set_reference
      : |}^container^{|
      -> int
      -> |}^boxed_data^{|
      -> unit
      = "%caml_|}^container^{|_set|}^data_sigil^{|"

    external set_safe
      :  |}^container^{|
      -> |}^index^{|
      -> |}^tested_data^{|
      -> unit
      = "%caml_|}^container^{|_set|}^data_sigil^unboxed_sigil^index_sigil^{|"

    let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

    external set_unsafe
      :  |}^container^{|
      -> |}^index^{|
      -> |}^tested_data^{|
      -> unit
      = "%caml_|}^container^{|_set|}^data_sigil^{|u|}^unboxed_sigil^index_sigil^{|"

    let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
  end)
|}

(* We can't always just look up min/max int in the module because of the [int16]
   accessors, which use [Int]. *)
let int_examples_template ~module_ ~min ~max ~min_for_rand ~max_for_rand ~type_ = {|
fun i ->
  match i mod 4 with
  | 0 -> |}^module_^{|.zero
  | 1 -> |}^min^{|
  | 2 -> |}^max^{|
  | _ -> Random.|}^type_^{|_in_range ~min:|}^min_for_rand^{| ~max:|}^max_for_rand^{|
|}

let int_examples_custom_bounds ~module_ ~width ~type_ =
  let min_int = {|(|}^module_^{|.(shift_left one) (|}^width^{| - 1))|} in
  let max_int = {|
    (let shift = |}^width^{| - 1 in
    |}^module_^{|.(lognot (shift_left (shift_right (lognot zero) shift) shift)))|}
  in
  int_examples_template
    ~module_
    ~min:min_int
    ~max:max_int
    ~min_for_rand:(module_^{|.zero|})
    ~max_for_rand:min_int
    ~type_

let int_examples_default_bounds ~module_ ~width ~type_ =
  let min = module_^{|.min_int|} in
  let max = module_^{|.max_int|} in
  int_examples_template ~module_ ~min ~max ~min_for_rand:min ~max_for_rand:max ~type_

let float_examples = {|
fun _ ->
  let f =
    let f = Random.float Float.max_float in
    if Random.bool () then Float.neg f else f
  in
  Stdlib_stable.Float32.of_float f
|}

let int_extra_bounds_template ~module_ =
  module_ ^ {|.[ min_int; max_int; add min_int one; sub zero one ]|}
;;

type index =
  { boxed_type : string
  ; tested_type : string
  ; of_int : string
  ; unbox : string
  ; extra_bounds : string
  }

type result =
  { boxed_type : string
  ; tested_type : string
  ; width : string
  ; unboxed_sigil : string
  ; box : string
  ; unbox : string
  ; eq : string
  ; example : string
  }

type container =
  { container : string
  ; create : string
  }

let int_result ~width ~unboxed ~module_ ~examples =
  let type_ = String.lowercase_ascii module_ in
  let unboxed_sigil = if unboxed then "#" else "" in
  { boxed_type = type_
  ; tested_type = type_ ^ unboxed_sigil
  ; width
  ; unboxed_sigil
  ; box =
      (if unboxed
       then "Stdlib_upstream_compatible." ^ module_ ^ "_u.to_" ^ type_
       else "fun x -> x")
  ; unbox =
      (if unboxed
       then "Stdlib_upstream_compatible." ^ module_ ^ "_u.of_" ^ type_
       else "fun x -> x")
  ; eq = module_ ^ ".equal"
  ; example = examples ~module_ ~width ~type_ |> indent 2
  }
;;

let indices =
  [ { boxed_type = "nativeint"
    ; tested_type = "nativeint#"
    ; of_int = "Nativeint.of_int"
    ; unbox = "Stdlib_upstream_compatible.Nativeint_u.of_nativeint"
    ; extra_bounds = int_extra_bounds_template ~module_:"Nativeint"
    }
  ; { boxed_type = "int32"
    ; tested_type = "int32#"
    ; of_int = "Int32.of_int"
    ; unbox = "Stdlib_upstream_compatible.Int32_u.of_int32"
    ; extra_bounds = int_extra_bounds_template ~module_:"Int32"
    }
  ; { boxed_type = "int64"
    ; tested_type = "int64#"
    ; of_int = "Int64.of_int"
    ; unbox = "Stdlib_upstream_compatible.Int64_u.of_int64"
    ; extra_bounds = int_extra_bounds_template ~module_:"Int64"
    }
  ]
;;

let datas =
  [ int_result ~width:"16" ~unboxed:false ~module_:"Int"
      ~examples:int_examples_custom_bounds
  ; int_result ~width:"32" ~unboxed:false ~module_:"Int32"
      ~examples:int_examples_default_bounds
  ; int_result ~width:"64" ~unboxed:false ~module_:"Int64"
      ~examples:int_examples_default_bounds
  ; int_result ~width:"32" ~unboxed:true ~module_:"Int32"
      ~examples:int_examples_default_bounds
  ; int_result ~width:"64" ~unboxed:true ~module_:"Int64"
      ~examples:int_examples_default_bounds
  ; {
      width = "f32";
      unboxed_sigil = "";
      boxed_type = "float32";
      tested_type = "float32";
      box = "fun x -> x";
      unbox = "fun x -> x";
      eq = "Stdlib_stable.Float32.equal";
      example = float_examples;
    }
  ; {
      width = "f32";
      unboxed_sigil = "#";
      boxed_type = "float32";
      tested_type = "float32#";
      box = "Stdlib_stable.Float32_u.to_float32";
      unbox = "Stdlib_stable.Float32_u.of_float32";
      eq = "Stdlib_stable.Float32.equal";
      example = float_examples;
    };
  ]

let containers =
  [ { container = "string"; create = "create_s" }
  ; { container = "bytes"; create = "create_b" }
  ; { container = "bigstring"; create = "create_bs" }
  ]
;;

let tests =
  let ( let* ) x f = List.concat_map f x in
  let* { boxed_type = boxed_index
       ; tested_type = tested_index
       ; of_int
       ; unbox = unbox_index
       ; extra_bounds
       }
    =
    indices
  in
  let* { boxed_type = boxed_data
       ; tested_type = tested_data
       ; width
       ; unboxed_sigil
       ; box = box_data
       ; unbox = unbox_data
       ; eq
       ; example
       }
    =
    datas
  in
  let setup =
    type_utilities_template
      ~boxed_index
      ~boxed_data
      ~generate_data:example
      ~to_index:of_int
      ~unbox_index
      ~unbox_data
      ~box_data
      ~data_equal:eq
      ~extra_bounds
  in
  let tests =
    List.map
      (fun { container; create } ->
        one_test_template
          ~index:tested_index
          ~boxed_data
          ~tested_data
          ~index_sigil:("_indexed_by_" ^ tested_index)
          ~data_sigil:width
          ~unboxed_sigil
          ~container
          ~create)
      containers
  in
  [ test_group_template ~setup ~tests ]
;;

template ~tests:(String.concat "" tests) |> print_string
