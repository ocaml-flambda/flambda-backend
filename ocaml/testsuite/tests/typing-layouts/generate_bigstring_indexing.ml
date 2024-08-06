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

|}^tests

let bigstring_tests_template ~length ~tests = {|
let length = |}^length^{|
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

|}^tests

let external_bindings_template ~container ~sigil ~width ~test_suffix ~index ~ref_result
      ~test_result = {|
external |}^sigil^{|_get_reference : |}^container^{| -> int -> |}^ref_result^{|
  = "%caml_|}^container^{|_get|}^width^{|"

external |}^sigil^{|_get_tested_s : |}^container^{| -> |}^index^{| -> |}^test_result^{|
  = "%caml_|}^container^{|_get|}^width^test_suffix^{|_indexed_by_|}^index^{|"

external |}^sigil^{|_get_tested_u : |}^container^{| -> |}^index^{| -> |}^test_result^{|
  = "%caml_|}^container^{|_get|}^width^{|u|}^test_suffix^{|_indexed_by_|}^index^{|"

external |}^sigil^{|_set_reference
  : |}^container^{| -> int -> |}^ref_result^{| -> unit
  = "%caml_|}^container^{|_set|}^width^{|"

external |}^sigil^{|_set_tested_s
  : |}^container^{| -> |}^index^{| -> |}^test_result^{| -> unit
  = "%caml_|}^container^{|_set|}^width^test_suffix^{|_indexed_by_|}^index^{|"

external |}^sigil^{|_set_tested_u
  : |}^container^{| -> |}^index^{| -> |}^test_result^{| -> unit
  = "%caml_|}^container^{|_set|}^width^{|u|}^test_suffix^{|_indexed_by_|}^index^{|"
|}

let bigstring_one_test_template ~width ~test_suffix ~index ~ref_result ~test_result
      ~conv_index ~box_result ~unbox_result ~eq ~extra_bounds ~example = {|
let of_boxed_index : int -> |}^index^{| = |}^conv_index^{|
let to_boxed_result : |}^test_result^{| -> |}^ref_result^{| = |}^box_result^{|
let of_boxed_result : |}^ref_result^{| -> |}^test_result^{| = |}^unbox_result^{|
let eq : |}^ref_result^{| -> |}^ref_result^{| -> bool = |}^eq^{|
|}
^example
^external_bindings_template
   ~container:"bigstring" ~sigil:"bs" ~width ~test_suffix ~index ~ref_result ~test_result
^external_bindings_template
   ~container:"string" ~sigil:"s" ~width ~test_suffix ~index ~ref_result ~test_result
^external_bindings_template
   ~container:"bytes" ~sigil:"b" ~width ~test_suffix ~index ~ref_result ~test_result
^{|
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
|}^extra_bounds^{|
|}
;;

let extra_bounds_template ~index = {|
check_get_bounds (|}^index^{|);;
check_set_bounds (|}^index^{|) (x 1);;|}

(* We can't just look up min/max int in the module because of the [int16] accessors, which
   use [Int]. *)
let int_examples_template ~module_ ~width = {|
let rec x = function
  | i when i <= 0 -> |}^module_^{|.zero
  | 1 ->
      (* min int *)
      |}^module_^{|.(shift_left one) (|}^width^{| - 1)
  | 2 ->
      (* max int *)
      let shift = |}^width^{| - 1 in
      |}^module_^{|.(lognot (shift_left (shift_right (lognot zero) shift) shift))
  | i ->
      (* flip 3 "random" bits *)
      let i1 = 3 * i and i2 = 7 * i and i3 = 11 * i in
      let x = x (i - 1) in
      let x = |}^module_^{|.(logxor x (shift_left one (i1 mod |}^width^{|))) in
      let x = |}^module_^{|.(logxor x (shift_left one (i2 mod |}^width^{|))) in
      let x = |}^module_^{|.(logxor x (shift_left one (i3 mod |}^width^{|))) in
      x
|}

(* Copied from [utils/misc.ml] *)
module Hlist = struct
  type _ t =
    | [] : unit t
    | ( :: ) : 'a * 'b t -> ('a -> 'b) t

  type _ lt =
    | [] : unit lt
    | ( :: ) : 'a list * 'b lt -> ('a -> 'b) lt

  let rec cartesian_product : type l. l lt -> l t list = function
    | [] -> [ [] ]
    | hd :: tl ->
      let tl = cartesian_product tl in
      List.concat_map (fun x1 -> List.map (fun x2 : _ t -> x1 :: x2) tl) hd
  ;;
end

type index =
  { type_ : string
  ; conv : string
  ; extra_bounds : string list
  }

type result =
  { width : string
  ; test_suffix : string
  ; ref : string
  ; test : string
  ; box : string
  ; unbox : string
  ; eq : string
  ; example : string
  }

let int_result ~width ~unboxed ~module_ =
  let type_ = String.lowercase_ascii module_ in
  let suffix = if unboxed then "#" else "" in
  { width
  ; test_suffix = suffix
  ; ref = type_
  ; test = type_ ^ suffix
  ; box =
      (if unboxed
       then "Stdlib_upstream_compatible." ^ module_ ^ "_u.to_" ^ type_
       else "fun x -> x")
  ; unbox =
      (if unboxed
       then "Stdlib_upstream_compatible." ^ module_ ^ "_u.of_" ^ type_
       else "fun x -> x")
  ; eq = module_ ^ ".equal"
  ; example = int_examples_template ~module_ ~width
  }
;;

let bigstring_tests =
  Hlist.cartesian_product
    ([ [ { type_ = "nativeint#"
         ; conv = "Stdlib_upstream_compatible.Nativeint_u.of_int"
         ; extra_bounds = [ "-#1n" ]
         }
       ; { type_ = "int32#"
         ; conv = "Stdlib_upstream_compatible.Int32_u.of_int"
         ; extra_bounds =
             [ "-#2147483648l"; "-#2147483647l"; "#2147483647l"; "-#1l" ]
         }
       ; { type_ = "int64#"
         ; conv = "Stdlib_upstream_compatible.Int64_u.of_int"
         ; extra_bounds =
             [ "-#9223372036854775808L"
             ; "-#9223372036854775807L"
             ; "#9223372036854775807L"
             ; "-#1L"
             ]
         }
       ]
     ; [ int_result ~width:"16" ~unboxed:false ~module_:"Int"
       ; int_result ~width:"32" ~unboxed:false ~module_:"Int32"
       ; int_result ~width:"64" ~unboxed:false ~module_:"Int64"
       ; int_result ~width:"32" ~unboxed:true ~module_:"Int32"
       ; int_result ~width:"64" ~unboxed:true ~module_:"Int64"
       ; { width = "f32"
         ; test_suffix = ""
         ; ref = "float32"
         ; test = "float32"
         ; box = "fun x -> x"
         ; unbox = "fun x -> x"
         ; eq = "Stdlib_stable.Float32.equal"
         ; example =
             "let x _ = Stdlib_stable.Float32.of_float 5.0\n"
         }
       ; { width = "f32"
         ; test_suffix = "#"
         ; ref = "float32"
         ; test = "float32#"
         ; box = "Stdlib_stable.Float32_u.to_float32"
         ; unbox = "Stdlib_stable.Float32_u.of_float32"
         ; eq = "Stdlib_stable.Float32.equal"
         ; example =
             "let x _ = Stdlib_stable.Float32.of_float 5.0\n"
         }
       ]
     ]
     : _ Hlist.lt)
  |> List.map
       (fun
           ([ { type_; conv = conv_index; extra_bounds }
            ; { width; test_suffix; ref; test; box; unbox; eq; example }
            ] :
             _ Hlist.t)
         ->
          let extra_bounds =
            String.concat
              ""
              (List.map
                 (fun index -> extra_bounds_template ~index)
                 extra_bounds)
          in
          bigstring_one_test_template
            ~width
            ~test_suffix
            ~index:type_
            ~ref_result:ref
            ~test_result:test
            ~conv_index
            ~box_result:box
            ~unbox_result:unbox
            ~eq
            ~extra_bounds
            ~example)
  |> String.concat ""
;;

template ~tests:(bigstring_tests_template ~length:"300" ~tests:bigstring_tests)
|> print_string
