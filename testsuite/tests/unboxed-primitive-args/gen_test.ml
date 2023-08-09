(* This programs generate stubs with various prototype combinations *)

open StdLabels

type boxed_integer = Pnativeint | Pint32 | Pint64

type boxed_vector = Pint64x2 | Pfloat64x2

type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int
  | Unboxed_vector of boxed_vector

(* Generate primitives with up to this number of arguments *)
let test_all_combination_up_to_n_args = 5

(* Generate primitives using all combination of these argument
   representations. No need to test all combination of other
   representations: regarding the calling convention
   [Same_as_ocaml_repr], [Untagged_int] and
   [Unboxed_integer Pnativeint] are all the same, and are the
   same as [Unboxed_integer Pint<word-size>].

   We have specific tests for the other representations and for the
   result representation in [manual_tests].
*)
let test_all_args_combination_of =
  [ Unboxed_float
  ; Unboxed_integer Pint32
  ; Unboxed_integer Pint64
  ; Unboxed_vector Pint64x2
  ; Unboxed_vector Pfloat64x2
  ]

let code_of_repr = function
  | Same_as_ocaml_repr         -> "v" (* for "value" *)
  | Unboxed_float              -> "f"
  | Unboxed_integer Pint32     -> "l"
  | Unboxed_integer Pint64     -> "L"
  | Unboxed_integer Pnativeint -> "n"
  | Untagged_int               -> "i"
  | Unboxed_vector Pint64x2    -> "I"
  | Unboxed_vector Pfloat64x2  -> "x"

let repr_of_code = function
  | 'v' -> Same_as_ocaml_repr
  | 'f' -> Unboxed_float
  | 'l' -> Unboxed_integer Pint32
  | 'L' -> Unboxed_integer Pint64
  | 'n' -> Unboxed_integer Pnativeint
  | 'i' -> Untagged_int
  | 'x' -> Unboxed_vector Pfloat64x2
  | 'I' -> Unboxed_vector Pint64x2
  | _   -> assert false

let manual_tests =
  [ "v_v"
  ; "f_f"
  ; "l_l"
  ; "L_L"
  ; "n_n"
  ; "i_i"
  ; "x_x"
  ; "f_fffff"
  ; "f_ffffff"
  ; "f_fffffff"
  ; "f_fffffffffffffffff"
  ; "x_xxxxx"
  ; "x_xxxxxx"
  ; "x_xxxxxxx"
  ; "x_xxxxxxxxxxxxxxxxx"
  ; "v_iiiiiiiiiiiiiiiii"
  ; "v_lllllllllllllllll"
  ; "v_LLLLLLLLLLLLLLLLL"
  ; "v_iLiLiLiLiLiLiLiLi"
  ; "v_LiLiLiLiLiLiLiLiL"
  ; "v_flflflflflflflflflflflflflflflflflfl"
  ; "v_fLfLfLfLfLfLfLfLfLfLfLfLfLfLfLfLfLfL"
  ; "v_xfxfxfxfxfxfxfxfx"
  ; "v_fxfxfxfxfxfxfxfxf"
  ; "v_lfxlfxlfxlfxlfxlfx"
  ; "v_lflxlxlflflxlxlflx"
  ; "v_llllllfffffflxxllxx"
  ]

let ocaml_type_of_repr = function
  (* Doesn't really matters what we choose for this case *)
  | Same_as_ocaml_repr         -> "int"
  | Unboxed_float              -> "(float [@unboxed])"
  | Unboxed_integer Pint32     -> "(int32 [@unboxed])"
  | Unboxed_integer Pint64     -> "(int64 [@unboxed])"
  | Unboxed_integer Pnativeint -> "(nativeint [@unboxed])"
  | Untagged_int               -> "(int [@untagged])"
  | Unboxed_vector Pfloat64x2  -> "(float64x2 [@unboxed])"
  | Unboxed_vector Pint64x2    -> "(int64x2 [@unboxed])"

let ocaml_type_gadt_of_repr = function
  (* Doesn't really matters what we choose for this case *)
  | Same_as_ocaml_repr         -> "Int"
  | Unboxed_float              -> "Float"
  | Unboxed_integer Pint32     -> "Int32"
  | Unboxed_integer Pint64     -> "Int64"
  | Unboxed_integer Pnativeint -> "Nativeint"
  | Untagged_int               -> "Int"
  | Unboxed_vector Pfloat64x2  -> "Float64x2"
  | Unboxed_vector Pint64x2    -> "Int64x2"

let c_type_of_repr = function
  | Same_as_ocaml_repr         -> "value"
  | Unboxed_float              -> "double"
  | Unboxed_integer Pint32     -> "int32_t"
  | Unboxed_integer Pint64     -> "int64_t"
  | Unboxed_integer Pnativeint -> "intnat"
  | Untagged_int               -> "intnat"
  | Unboxed_vector Pfloat64x2  -> "__m128d"
  | Unboxed_vector Pint64x2    -> "__m128i"

type proto =
  { params : native_repr list
  ; return : native_repr
  }

let rec explode s =
  let rec loop i acc =
    if i < 0 then
      acc
    else
      loop (i - 1) (s.[i] :: acc)
  in
  loop (String.length s - 1) []

let proto_of_str s =
  Scanf.sscanf s "%c_%s" (fun return params ->
    { params = List.map (explode params) ~f:repr_of_code
    ; return = repr_of_code return
    })

let function_name_of_proto proto =
  Printf.sprintf "test_%s_%s" (code_of_repr proto.return)
    (String.concat ~sep:"" (List.map proto.params ~f:code_of_repr))

let ocaml_type_gadt_of_proto proto =
  Printf.sprintf "%s ** Ret %s"
    (String.concat ~sep:" ** "
       (List.map proto.params ~f:ocaml_type_gadt_of_repr))
    (ocaml_type_gadt_of_repr proto.return)

let ocaml_type_of_proto proto =
  String.concat ~sep:" -> "
    (List.map proto.params ~f:ocaml_type_of_repr
     @ [ocaml_type_of_repr proto.return])

let c_args_of_proto proto =
  String.concat ~sep:", "
    (List.mapi proto.params ~f:(fun i p ->
       Printf.sprintf "%s x%d" (c_type_of_repr p) i))

let manual_protos = List.map manual_tests ~f:proto_of_str

let iter_protos ~f =
  let iter_for_arity arity =
    let rec loop params to_gen =
      List.iter test_all_args_combination_of ~f:(fun repr ->
        let params = repr :: params in
        let to_gen = to_gen - 1 in
        if to_gen = 0 then
          f { params = List.rev params
            ; return = Same_as_ocaml_repr
            }
        else
          loop params to_gen)
    in
    loop [] arity
  in
  let rec iter_arities arity =
    if arity <= test_all_combination_up_to_n_args then begin
      iter_for_arity arity;
      iter_arities (arity + 1)
    end
  in
  List.iter manual_protos ~f;
  iter_arities 1

let pr fmt = Printf.ksprintf (fun s -> print_string s; print_char '\n') fmt

let generate_ml () =
  let close, print_test =
    let n = 2048 in
    let i = ref 0 in
    let file = ref None in
    let close () =
      match !file with
      | Some file ->
        Printf.fprintf file "\nlet run () = run_tests (List.rev tests)\n%!";
        Out_channel.close file
      | None -> ()
    in
    let new_file () =
      close ();
      let next = open_out (Printf.sprintf "test%d.ml" (!i / n)) in
      pr "let () = Test%d.run ()" (!i / n);
      file := Some next;
      Printf.fprintf next "open Common\n";
      Printf.fprintf next "let tests = []\n\n";
    in
    close, fun ext test ->
      if !i mod n = 0 then new_file ();
      Printf.fprintf (Option.get !file) "%s\n%s\n" ext test;
      incr i
  in
  iter_protos ~f:(fun proto ->
    let name = function_name_of_proto proto in
    let ext = Format.sprintf "external %s : %s = \"\" %S [@@@@noalloc]"
              name (ocaml_type_of_proto proto) name in
    let name = function_name_of_proto proto in
    let arity = List.length proto.params in
    let test = if arity <= 6 then
      Format.sprintf "let tests = T%d (%S, %s, %s, %s) :: tests"
      arity name name
      (List.map proto.params ~f:ocaml_type_gadt_of_repr
      |> String.concat ~sep:", ")
      (ocaml_type_gadt_of_repr proto.return)
    else
      Format.sprintf "let tests = T (%S, %s, %s) :: tests"
      name name (ocaml_type_gadt_of_proto proto) in
    print_test ext test);
  close ()

let generate_stubs () =
  pr "#include <stdio.h>";
  pr "#include <caml/mlvalues.h>";
  pr "#include \"test_common.h\"";
  iter_protos ~f:(fun proto ->
      let name = function_name_of_proto proto in
      pr "";
      pr "%s %s(%s)"
        (c_type_of_repr proto.return)
        name
        (c_args_of_proto proto);
      pr "{";
      List.iteri proto.params ~f:(fun i p ->
        pr "  %(%d%d%);"
          (match p with
           | Same_as_ocaml_repr         -> "set_intnat(%d, Long_val(x%d))"
           | Unboxed_float              -> "set_double(%d, x%d)"
           | Unboxed_integer Pint32     -> "set_int32(%d, x%d)"
           | Unboxed_integer Pint64     -> "set_int64(%d, x%d)"
           | Unboxed_integer Pnativeint -> "set_intnat(%d, x%d)"
           | Untagged_int               -> "set_intnat(%d, x%d)"
           | Unboxed_vector Pint64x2    -> "set_int128(%d, x%d)"
           | Unboxed_vector Pfloat64x2  -> "set_float128(%d, x%d)")
          i i);
      pr "  return %(%d%);"
        (match proto.return with
         | Same_as_ocaml_repr         -> "Val_long(get_intnat(%d))"
         | Unboxed_float              -> "get_double(%d)"
         | Unboxed_integer Pint32     -> "get_int32(%d)"
         | Unboxed_integer Pint64     -> "get_int64(%d)"
         | Unboxed_integer Pnativeint -> "get_intnat(%d)"
         | Untagged_int               -> "get_intnat(%d)"
         | Unboxed_vector Pint64x2    -> "get_int128(%d)"
         | Unboxed_vector Pfloat64x2  -> "get_float128(%d)")
        (List.length proto.params);
      pr "}"
  )

let () =
  match Sys.argv with
  | [|_; "ml"|] -> generate_ml ()
  | [|_; "c" |] -> generate_stubs ()
  | _ ->
    prerr_endline "Usage: ocaml gen_test.ml {ml|c}";
    exit 2
