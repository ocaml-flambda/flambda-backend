let template ~tests = {|(* TEST
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

|}^tests

let bigstring_tests_template ~length ~tests = {|
let length = |}^length^{|
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

|}^tests

let external_bindings_template ~container ~sigil ~width ~test_suffix ~index ~ref_result
      ~test_result = {|
external |}^sigil^{|_reference : |}^container^{| -> int -> |}^ref_result^{|
  = "%caml_|}^container^{|_get|}^width^{|"

external |}^sigil^{|_tested_s : |}^container^{| -> |}^index^{| -> |}^test_result^{|
  = "%caml_|}^container^{|_get|}^width^test_suffix^{|_indexed_by_|}^index^{|"

external |}^sigil^{|_tested_u : |}^container^{| -> |}^index^{| -> |}^test_result^{|
  = "%caml_|}^container^{|_get|}^width^{|u|}^test_suffix^{|_indexed_by_|}^index^{|"
|}

let bigstring_one_test_template ~width ~test_suffix ~index ~ref_result ~test_result
      ~conv_index ~conv_result ~eq ~extra_bounds = {|
let of_boxed_index : int -> |}^index^{| = |}^conv_index^{|
let to_boxed_result : |}^test_result^{| -> |}^ref_result^{| = |}^conv_result^{|
let eq : |}^ref_result^{| -> |}^ref_result^{| -> bool = |}^eq^{|
|}
^external_bindings_template
   ~container:"bigstring" ~sigil:"bs" ~width ~test_suffix ~index ~ref_result ~test_result
^external_bindings_template
   ~container:"bytes" ~sigil:"b" ~width ~test_suffix ~index ~ref_result ~test_result
^{|
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
|}^extra_bounds^{|
|}
;;

let extra_bounds_template ~index = {|
check_get_bounds (|}^index^{|);;|}

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
  ; conv : string
  ; eq : string
  }

let bigstring_tests =
  Hlist.cartesian_product
    ([ [ { type_ = "nativeint#"
         ; conv = "Stdlib_upstream_compatible.Nativeint_u.of_int"
         ; extra_bounds = []
         }
       ; { type_ = "int32#"
         ; conv = "Stdlib_upstream_compatible.Int32_u.of_int"
         ; extra_bounds = [ "-#2147483648l"; "-#2147483647l"; "#2147483647l" ]
         }
       ; { type_ = "int64#"
         ; conv = "Stdlib_upstream_compatible.Int64_u.of_int"
         ; extra_bounds =
             [ "-#9223372036854775808L"
             ; "-#9223372036854775807L"
             ; "#9223372036854775807L"
             ]
         }
       ]
     ; [ { width = "16"
         ; test_suffix = ""
         ; ref = "int"
         ; test = "int"
         ; conv = "fun x -> x"
         ; eq = "Int.equal"
         }
       ; { width = "32"
         ; test_suffix = ""
         ; ref = "int32"
         ; test = "int32"
         ; conv = "fun x -> x"
         ; eq = "Int32.equal"
         }
       ; { width = "64"
         ; test_suffix = ""
         ; ref = "int64"
         ; test = "int64"
         ; conv = "fun x -> x"
         ; eq = "Int64.equal"
         }
       ; { width = "32"
         ; test_suffix = "#"
         ; ref = "int32"
         ; test = "int32#"
         ; conv = "Stdlib_upstream_compatible.Int32_u.to_int32"
         ; eq = "Int32.equal"
         }
       ; { width = "64"
         ; test_suffix = "#"
         ; ref = "int64"
         ; test = "int64#"
         ; conv = "Stdlib_upstream_compatible.Int64_u.to_int64"
         ; eq = "Int64.equal"
         }
       ]
     ]
     : _ Hlist.lt)
  |> List.map
       (fun
           ([ { type_; conv = conv_index; extra_bounds }
            ; { width; test_suffix; ref; test; conv = conv_result; eq }
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
            ~conv_result
            ~eq
            ~extra_bounds)
  |> String.concat ""
;;

template ~tests:(bigstring_tests_template ~length:"300" ~tests:bigstring_tests)
|> print_string
