(* This file is used in [run_makearray_dynamic_tests.ml]. *)
module List = ListLabels
module String = StringLabels

let failwithf fmt = Printf.ksprintf failwith fmt
let sprintf = Printf.sprintf

(* See [test_makearray_dynamic] for the main testing steps! *)

module Ty : sig
  (* A type in the generated code *)
  type t = {
    ty_code : string;
    (* Code for this type expression (e.g. "int option * float") *)
    value_code : int -> string;
    (* Given some integer seed, generate code for a value of this type.
       E.g. passing 3 gives "(Some 3, 3.)" for [int option * float]. *)
    mk_value_code : string;
    (* Code that dynamically implements [value_code], creating a value from an
       integer seed bound to "i".
       We should be able to generate this code:
       "let mk_value (i : int) : $ty_code = $mk_value_code" *)
    eq : string;
    (* A function that implements equality in the generated code.
       We should be able generate this code:
       "let eq : $ty_code @ local -> $ty_code @ local -> bool = $eq" *)
    is_gc_ignorable : bool;
    (* Whether type only contains non-values/immediates (this used to gate
       blit tests, but now that blits work for all types, this field is
       unused). *)
  }

  (* Generate typedecls for user-defined nominal types that have been created *)
  val decls_code : unit -> string list

  (* Takes the record name and (label_name, label_type) pairs *)
  val unboxed_record : string -> (string * t) list -> t

  (* [enum 3] represents [type enum3 = A3_0 | A3_1 | A3_2]. *)
  val enum : int -> t

  (* Structural and built-in types *)

  val option : t -> t
  val tuple : t list -> t
  val unboxed_tuple : t list -> t

  val int : t
  val float : t
  val float_u : t
  val float32 : t
  val float32_u : t
  val int32 : t
  val int32_u : t
  val int64 : t
  val int64_u : t
  val nativeint : t
  val nativeint_u : t
end = struct
  type t = {
    ty_code : string;
    value_code : int -> string;
    mk_value_code : string;
    eq : string;
    is_gc_ignorable : bool;
  }

  let ty_code t = t.ty_code
  let value_code t = t.value_code
  let mk_value_code t = t.mk_value_code
  let is_gc_ignorable t = t.is_gc_ignorable

  let map_value_code ts i = List.map ts ~f:(fun t -> t.value_code i)

  (* If (name, decl) is in this list, we'll generate "type $name = $decl" *)
  let decls : (string * string) list ref = ref []

  let decls_code () =
    (* [!decls] is only reversed for aesthetic reasons. *)
    List.mapi (List.rev !decls) ~f:(fun i (name, def) ->
      (if i == 0 then "type " else "and ") ^ name ^ " = " ^ def
    )

  let add_decl ~name ~def =
    match List.assoc_opt name !decls with
    | Some def' ->
      if not (String.equal def def') then
        failwithf
          "%s has conflicting definitions:\n  %s\nand\n  %s" name def' def
    | None -> decls := (name, def) :: !decls

  let unboxed_record name labeled_ts =
    let lbls, ts = List.split labeled_ts in
    let assemble colon_or_eq fields =
      let labeled_fields =
        List.map2 lbls fields ~f:(fun s x -> s ^ " " ^ colon_or_eq ^ " " ^ x)
      in
      "#{ " ^ String.concat ~sep:"; " labeled_fields ^ " }"
    in
    let assemble_expr fields = "(" ^ assemble "=" fields ^ " : " ^ name ^ ")" in
    let value_code i = assemble_expr (map_value_code ts i) in
    let mk_value_code = assemble_expr (List.map ts ~f:mk_value_code) in
    let pat i =
      assemble_expr (List.map lbls ~f:(fun s -> s ^ Int.to_string i))
    in
    let eq =
      let body =
        List.map labeled_ts ~f:(fun (s, t) -> sprintf "%s %s1 %s2" t.eq s s)
        |> String.concat ~sep:" && "
      in
      sprintf "(fun %s %s -> %s)" (pat 1) (pat 2) body
    in
    add_decl ~name ~def:(assemble ":" (List.map ts ~f:ty_code));
    {
      ty_code = name;
      value_code;
      mk_value_code;
      eq;
      is_gc_ignorable = List.for_all ~f:is_gc_ignorable ts;
    }

  let enum size =
    let ith_ctor i = sprintf "A%d_%d" size i in
    let def = List.init ~len:size ~f:ith_ctor |> String.concat ~sep:" | " in
    let eq =
      let eq_pat =
        List.init ~len:size ~f:(fun i -> ith_ctor i ^ ", " ^ ith_ctor i)
        |> String.concat ~sep:" | "
      in
      sprintf "(fun a b -> match a, b with %s -> true | _ -> false)" eq_pat
    in
    let mk_value_code =
      let brs =
        List.init ~len:size ~f:(fun i -> sprintf "%d -> %s" i (ith_ctor i))
        @ ["_ -> assert false"]
      in
      sprintf "(match Int.rem i %d with %s)" size (String.concat ~sep:" | " brs)
    in
    let name = sprintf "enum%d" size in
    add_decl ~name ~def;
    {
      ty_code = name;
      value_code = (fun i -> ith_ctor (Int.rem i size));
      mk_value_code;
      eq;
      is_gc_ignorable = true;
    }

  let option t = {
    ty_code = t.ty_code ^ " option";
    value_code =
      (fun i -> if i == 0 then "None" else "Some " ^ t.value_code i);
    mk_value_code =
      "(if i == 0 then None else Some (" ^ t.mk_value_code ^ "))";
    eq = "(fun a b -> match a, b with None,None -> true | Some a,Some b -> "
         ^ t.eq ^ " a b|_->false)";
    is_gc_ignorable = false;
  }

  let gen_tuple ~unboxed ts =
    let hash = if unboxed then "#" else "" in
    let assemble ~sep xs = sprintf "%s(%s)" hash (String.concat ~sep xs) in
    let value_code i = assemble ~sep:", " (map_value_code ts i) in
    let mk_value_code = assemble ~sep:", " (List.map ts ~f:mk_value_code) in
    let eq =
      let pat s =
        assemble ~sep:", " (List.mapi ts ~f:(fun i _ -> s ^ Int.to_string i))
      in
      let body =
        List.mapi ts ~f:(fun i t -> sprintf "%s a%d b%d" t.eq i i)
        |> String.concat ~sep:" && "
      in
      sprintf "(fun %s %s -> %s)" (pat "a") (pat "b") body
    in
    {
      ty_code = assemble ~sep:" * " (List.map ts ~f:ty_code);
      value_code;
      mk_value_code;
      eq;
      is_gc_ignorable = unboxed && List.for_all ~f:is_gc_ignorable ts;
    }

  let tuple = gen_tuple ~unboxed:false

  let unboxed_tuple = gen_tuple ~unboxed:true

  let int = {
    ty_code = "int";
    value_code = Int.to_string;
    mk_value_code = "i";
    eq = "(fun a b -> Int.equal a b)";
    is_gc_ignorable = true;
  }

  let float = {
    ty_code = "float";
    value_code = (fun i -> Int.to_string i ^ ".");
    mk_value_code = "Float.of_int i";
    eq = "(fun a b -> Float.equal (globalize a) (globalize b))";
    is_gc_ignorable = false;
  }

  let float_u = {
    ty_code = "float#";
    value_code = (fun i -> "#" ^ Int.to_string i ^ ".");
    mk_value_code = "Float_u.of_int i";
    eq = "(fun a b -> Float_u.(equal (add #0. a) (add #0. b)))";
    is_gc_ignorable = true;
  }

  let float32 = {
    ty_code = "float32";
    value_code = (fun i -> Int.to_string i ^ ".s");
    mk_value_code = "Float32.of_int i";
    eq = "(fun a b -> Float.equal (Float32.to_float a) (Float32.to_float b))";
    is_gc_ignorable = false;
  }

  let float32_u = {
    ty_code = "float32#";
    value_code = (fun i -> "#" ^ Int.to_string i ^ ".s");
    mk_value_code = "Float32_u.of_int i";
    eq = "(fun a b -> Float32_u.(equal (add #0.s a) (add #0.s b)))";
    is_gc_ignorable = true;
  }

  let int32 = {
    ty_code = "int32";
    value_code = (fun i -> Int.to_string i ^ "l");
    mk_value_code = "Int32.of_int i";
    eq = "(fun a b -> Int32.equal (globalize a) (globalize b))";
    is_gc_ignorable = false;
  }

  let int32_u = {
    ty_code = "int32#";
    value_code = (fun i -> "#" ^ Int.to_string i ^ "l");
    mk_value_code = "Int32_u.of_int i";
    eq = "(fun a b -> Int32_u.(equal (add #0l a) (add #0l b)))";
    is_gc_ignorable = true;
  }

  let int64 = {
    ty_code = "int64";
    value_code = (fun i -> Int.to_string i ^ "L");
    mk_value_code = "Int64.of_int i";
    eq = "(fun a b -> Int64.equal (globalize a) (globalize b))";
    is_gc_ignorable = false;
  }

  let int64_u = {
    ty_code = "int64#";
    value_code = (fun i -> "#" ^ Int.to_string i ^ "L");
    mk_value_code = "Int64_u.of_int i";
    eq = "(fun a b -> Int64_u.(equal (add #0L a) (add #0L b)))";
    is_gc_ignorable = true;
  }

  let nativeint = {
    ty_code = "nativeint";
    value_code = (fun i -> (Int.to_string i) ^ "n");
    mk_value_code = "Nativeint.of_int i";
    eq = "(fun a b -> Nativeint.equal (globalize a) (globalize b))";
    is_gc_ignorable = false;
  }

  let nativeint_u = {
    ty_code = "nativeint#";
    value_code = (fun i -> "#" ^ Int.to_string i ^ "n");
    mk_value_code = "Nativeint_u.of_int i";
    eq = "(fun a b -> Nativeint_u.(equal (add #0n a) (add #0n b)))";
    is_gc_ignorable = true;
  }
end

let ty_ur1 = Ty.(unboxed_record "ur1" ["a", int64_u; "b", float_u])
let ty_ur2 = Ty.(unboxed_record "ur2" ["a", int64_u; "b", int])
let ty_ur3 = Ty.(unboxed_record "ur3" ["a", int64_u])
let ty_ur4 = Ty.(unboxed_record "ur4" ["a", ty_ur1; "b", ty_ur3])

let every_nth ~offset ~n l =
  List.filteri l ~f:(fun i _ -> Int.equal (i mod n) offset)

(* Types the GC always ignores, which can be used with %makearray_dynamic_uninit *)
let always_ignored_types ~partition ~num_partitions =
  Ty.(
    every_nth ~offset:(partition - 1) ~n:num_partitions
    [
    float32_u; float_u; int32_u; int64_u; nativeint_u; ty_ur1; ty_ur3; ty_ur4;
    unboxed_tuple [float_u; int32_u; int64_u];
    unboxed_tuple [
      float_u;
      unboxed_tuple [int64_u; int64_u];
      float32_u;
      unboxed_tuple [int32_u; unboxed_tuple [float32_u; float_u]];
      int64_u;
    ];
    unboxed_tuple [int64_u; ty_ur1];
  ])

let types ~partition ~num_partitions =
  (always_ignored_types ~partition ~num_partitions)
  @ Ty.(
    every_nth ~offset:(partition - 1) ~n:num_partitions
    [
      float32; float; int32; int64; nativeint; int; enum 3; ty_ur2;
      unboxed_tuple [int; int64];
      unboxed_tuple [
        option int64;
        unboxed_tuple [int; int32; float];
        float;
        unboxed_tuple [float32; option (tuple [nativeint; nativeint])];
        int32
      ];
      unboxed_tuple [float; float; float];
      unboxed_tuple [
        float;
        unboxed_tuple [float; float];
        unboxed_tuple [float; unboxed_tuple [float; float; float]]
      ];
      unboxed_tuple [float_u; int; int64_u];
      unboxed_tuple [
        float_u;
        unboxed_tuple [int; int64_u];
        float32_u;
        unboxed_tuple [int32_u; unboxed_tuple [float32_u; float_u]];
        int;
      ];
      unboxed_tuple [ty_ur2; ty_ur1];
    ])

let preamble = {|
open Stdlib_upstream_compatible
open Stdlib_stable
module List = ListLabels
module String = StringLabels

external[@layout_poly] makearray_dynamic_uninit_local :
  ('a : any_non_null) . int -> 'a array @ local =
  "%makearray_dynamic_uninit"

external[@layout_poly] makearray_dynamic_uninit :
  ('a : any_non_null) . int -> 'a array =
  "%makearray_dynamic_uninit"

external[@layout_poly] makearray_dynamic_local :
  ('a : any_non_null) . int -> 'a -> 'a array @ local =
  "%makearray_dynamic"

external[@layout_poly] makearray_dynamic :
  ('a : any_non_null) . int -> 'a -> 'a array =
  "%makearray_dynamic"

external[@layout_poly] get :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a =
  "%array_safe_get"

external[@layout_poly] set :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit =
  "%array_safe_set"

external[@layout_poly] unsafe_blit :
  ('a : any_non_null) . ('a array[@local_opt]) -> (int[@local_opt]) -> ('a array[@local_opt]) -> (int[@local_opt]) -> (int[@local_opt]) -> unit =
  "%arrayblit"

let failwithf fmt = Printf.ksprintf failwith fmt

external globalize : local_ 'a -> 'a = "%obj_dup";;

(* Redefine iter to infer locality *)
let rec iter ~f = function
    [] -> ()
  | a::l -> f a; iter ~f l

let tests_run = ref []

let mark_test_run test_id =
  if not (List.mem ~set:!tests_run test_id) then
    tests_run := test_id :: !tests_run

(* Various interesting values *)

let sizes = [ 0; 1; 2; 30; 31; 32 ]

let bad_indices size =
  [ -100; -2; -1; size; size + 1; size + 100; Int.min_int; Int.max_int ]

let blit_offsets size =
  let candidates = [ 0; 1; size / 3; size / 2; size - 1; size ] in
  List.filter candidates ~f:(fun ofs -> ofs > 0 && ofs < size)
  |> List.sort_uniq ~cmp:Int.compare

let blit_lens ~ofs1 ~ofs2 ~size1 ~size2 =
  let len_until_end = Int.min (size1 - ofs1) (size2 - ofs2) in
  let candidates = [ 0; 1; size1 / 2; len_until_end - 1; len_until_end ] in
  List.filter candidates ~f:(fun len -> ofs1 + len <= size1 && ofs2 + len <= size2)
  |> List.sort_uniq ~cmp:Int.compare
|}

let indent = ref 0

let with_indent f = incr indent; f (); decr indent

let line fmt =
  Printf.ksprintf
    (fun s ->
        let indent = Seq.init (!indent * 2) (fun _ -> ' ') |> String.of_seq in
        print_endline (indent ^ s);
        flush stdout)
    fmt

let print_in_test s =
  line {|let () = Printf.printf "%s%%!\n";;|} (String.escaped s)

let seq_print_in_test s =
  line {|print_endline "%s%!";|} (String.escaped s)

let makearray_dynamic_fn ~uninit ~local =
  let uninit_s = if uninit then "_uninit" else "" in
  let local_s = if local then "_local" else "" in
  "makearray_dynamic" ^ uninit_s ^ local_s

type debug_expr = { expr : string ; format_s : string }

let concat_with_leading_spaces l =
  List.map l ~f:(fun s -> " " ^ s)
  |> String.concat ~sep:""

let combine_debug_exprs (l : debug_expr list) : debug_expr =
  let debug_expr_to_tuple { expr ; format_s } = expr, format_s in
  let exprs, format_ss = List.split (List.rev_map ~f:debug_expr_to_tuple l) in
  let expr = concat_with_leading_spaces exprs in
  let format_s = concat_with_leading_spaces format_ss in
  { expr; format_s }

let seq_print_debug_exprs ~debug_exprs =
  let { expr ; format_s } = combine_debug_exprs debug_exprs in
  line {|Printf.printf "%s: %s\n%%!"%s;|} expr format_s expr

let test_id = ref 0

let seq_assert ~debug_exprs s =
  incr test_id;
  let { expr ; format_s } = combine_debug_exprs debug_exprs in
  line "mark_test_run %d;" !test_id;
  line "let test = %s in" s;
  line {|if not test then failwithf "test %d failed%s"%s;|}
    !test_id format_s expr

let for_ var ~from ~to_ ~debug_exprs f =
  line "for %s = %s to %s do" var from to_;
  with_indent (fun () ->
    let debug_exprs = { expr = var; format_s = "%d" } :: debug_exprs in
    f ~debug_exprs
  );
  line "done;"

let for_i_below_size = for_ "i" ~from:"0" ~to_:"size - 1"

(* Iterate through a list of ints *)
let iter l var ~debug_exprs f =
  line "iter (%s) ~f:(fun %s ->" l var;
  with_indent (fun () ->
    let debug_exprs =  { expr = var; format_s = "%d" } :: debug_exprs in
    f ~debug_exprs
  );
  line ") [@nontail];"

let section s =
  let s_as_stars = String.init (String.length s) ~f:(fun _ -> '*') in
  line "(**%s**)" s_as_stars;
  line "(* %s *)" s;
  line "(**%s**)" s_as_stars

(* Test steps:
   1. Create an array, possibly local, possibly uninitialized
   2. For initialized arrays, check all elements have the correct value
   3. Fill array with distinct values and read back those values
   4. Check that getting bad indices errors
   5. Check that setting bad indices errors
   6. Check that array contents were unaffected by setting bad indices
   7. Overlapping blits
   8. Blits to heap arrays
   9. Blits to local arrays
*)
let test_makearray_dynamic ~uninit ~local ty =
  let makearray_dynamic = makearray_dynamic_fn ~uninit ~local in
  let debug_exprs = [{ expr = "size"; format_s = "%d"}] in
  let ty_array_s = ty.Ty.ty_code ^ " array" in
  (* seq_print_in_test ty.Ty.ty_code; *)
  section ("  " ^ ty.Ty.ty_code ^ "  ");
  line "let eq = %s in" ty.Ty.eq;
  line "let mk_value i = %s in" ty.Ty.mk_value_code;
  line "(* 1. Create an array of size [size] *)";
  (if uninit then (
     line "let a : %s = %s size in" ty_array_s makearray_dynamic;
     line "(* 2. For uninitialized arrays, element values are unspecified *)"
   ) else
     line "let a : %s = %s size %s in" ty_array_s makearray_dynamic (ty.Ty.value_code 0);
    line "(* 2. For initialized arrays, check all elements have the correct value *)";
     for_i_below_size ~debug_exprs (fun ~debug_exprs ->
       line "let el = get a i in";
       if uninit then
         line "let _ = el in ()"
       else (
         let test = sprintf "eq el %s" (ty.Ty.value_code 0) in
         seq_assert ~debug_exprs test;
       )
     ));
  line "(* 3. Fill [a] with distinct values and read back those values *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
    line "set a i (mk_value i);"
  );
  line "Gc.compact ();";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
    seq_assert ~debug_exprs "eq (get a i) (mk_value i)"
  );
  iter "bad_indices size" "i" ~debug_exprs (fun ~debug_exprs ->
    line "(* 4. Getting bad indices errors *)";
    line "let raises =";
    with_indent (fun () ->
      line "match get a i with";
      line "| exception Invalid_argument _ -> true";
      line "| _ -> false"
    );
    line "in";
    seq_assert ~debug_exprs "raises";
    line "(* 5. Setting bad indices errors *)";
    line "let raises =";
    with_indent (fun () ->
      line "match set a i %s with" (ty.Ty.value_code 0);
      line "| exception Invalid_argument _ -> true";
      line "| _ -> false"
    );
    line "in";
    seq_assert ~debug_exprs "raises"
  );
  line "Gc.compact ();";
  line "(* 6. Array contents were unaffacted by setting bad indices *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
    seq_assert ~debug_exprs "eq (get a i) (mk_value i)"
  );
  (* Blits currently only work for GC ignorable values *)
  line "(* 7. Overlapping blits *)";
  iter "blit_offsets size" "ofs1" ~debug_exprs (fun ~debug_exprs ->
    iter "blit_offsets size" "ofs2" ~debug_exprs (fun ~debug_exprs ->
      let lens = "blit_lens ~ofs1 ~ofs2 ~size1:size ~size2:size" in
      iter lens "len" ~debug_exprs (fun ~debug_exprs ->
        line "unsafe_blit a ofs1 a ofs2 len;";
        for_i_below_size ~debug_exprs (fun ~debug_exprs ->
          line "let expected_src_i =";
          with_indent (fun () ->
            line "if i >= ofs2 && i < ofs2 + len then i - ofs2 + ofs1 else i"
          );
          line "in";
          seq_assert ~debug_exprs "eq (get a i) (mk_value expected_src_i)"
        );
        line "(* Reset array *)";
        for_i_below_size ~debug_exprs (fun ~debug_exprs ->
          line "set a i (mk_value i);"
        )
      );
    );
  );
  line "Gc.compact ();";
  let test_blit_to ~to_local =
    iter "sizes" "size2" ~debug_exprs (fun ~debug_exprs ->
      iter "blit_offsets size" "ofs1" ~debug_exprs (fun ~debug_exprs ->
        iter "blit_offsets size2" "ofs2" ~debug_exprs (fun ~debug_exprs ->
          let lens = "blit_lens ~ofs1 ~ofs2 ~size1:size ~size2" in
          iter lens "len" ~debug_exprs (fun ~debug_exprs ->
            (if to_local then
              line "let local_ a2 = makearray_dynamic_local size2 %s in" (ty.Ty.value_code 0)
            else
              line "let a2 = makearray_dynamic size2 %s in" (ty.Ty.value_code 0));
            line "unsafe_blit a ofs1 a2 ofs2 len;";
            for_ "i" ~from:"0" ~to_:"size2 - 1" ~debug_exprs (fun ~debug_exprs ->
              line "let expected_src_i =";
              with_indent (fun () ->
                line "if i >= ofs2 && i < ofs2 + len then i - ofs2 + ofs1 else 0"
              );
              line "in";
              seq_assert ~debug_exprs "eq (get a2 i) (mk_value expected_src_i)"
            )
          )
        )
      )
    );
    line "Gc.compact ();"
  in
  line "(* 8. Blits to heap arrays *)";
  test_blit_to ~to_local:false;
  line "(* 9. Blits to local arrays *)";
  test_blit_to ~to_local:true;
  print_endline ""

let toplevel_unit_block f =
  assert (Int.equal !indent 0);
  line "let () =";
  with_indent (fun () ->
    f (); line "()"
  );
  line ";;";
  line ""

let main ~bytecode ~partition ~num_partitions =
  let types = types ~partition ~num_partitions in
  let always_ignored_types = always_ignored_types ~partition ~num_partitions in
  line {|(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;|};
  if bytecode then (
    line {| flags = "-extension layouts_beta";|};
    (* CR layouts: enable for arm64 once float32 is available *)
    line {| arch_amd64;|};
    line {| bytecode;|};
  ) else (
    line {| modules = "stubs.c";|};
    line {| flags = "-extension layouts_beta -extension simd_beta";|};
    line {| flambda2;|};
    line {| stack-allocation;|};
    line {| arch_amd64;|};
    line {| native;|};
  );
  line {|*)|};
  line "(** This is code generated by [generate_makearray_dynamic_tests.ml]. *)";
  line "";
  line "%s" preamble;
  List.iter (Ty.decls_code ()) ~f:(fun s -> line "%s" s);
  line "";
  (* line "(* Catch metaprogramming errors early *)";
   * toplevel_unit_block (fun () ->
   *   let open Ty in
   *   let debug_exprs = [] in
   *   line "(* Check types and constants *)";
   *   List.iter types ~f:(fun ty ->
   *     line "let _ : %s = %s in" ty.ty_code (ty.value_code 0)
   *   );
   *   line "(* Check equality and mk_value functions *)";
   *   List.iter types ~f:(fun ty ->
   *     line "let eq : %s @ local -> %s @ local -> bool = %s in"
   *       ty.ty_code ty.ty_code ty.eq;
   *     line "let mk_value i = %s in" ty.mk_value_code;
   *     seq_assert ~debug_exprs
   *       (sprintf "eq (mk_value 1) %s"  (ty.value_code 1));
   *     seq_assert ~debug_exprs
   *       (sprintf "eq %s %s" (ty.value_code 1) (ty.value_code 1));
   *     seq_assert ~debug_exprs
   *       (sprintf "not (eq %s %s)" (ty.value_code 1) (ty.value_code 2))
   *   );
   *   line "(* Check always-GC-ignored types *)";
   *   List.iter always_ignored_types ~f:(fun ty ->
   *     line "let _ = (makearray_dynamic_uninit 1 : %s array) in" (ty.ty_code)
   *   );
   * ); *)
  List.iter [false; true] ~f:(fun uninit ->
    List.iter [false; true] ~f:(fun local ->
      line "let test_%s size =" (makearray_dynamic_fn ~uninit ~local);
      with_indent (fun () ->
        let tys = if uninit then always_ignored_types else types in
        List.iter tys ~f:(test_makearray_dynamic ~uninit ~local);
        line "()";
      );
      line "";
    )
  );
  line "(* Main tests *)";
  toplevel_unit_block (fun () ->
    List.iter [false; true] ~f:(fun uninit ->
      List.iter [false; true] ~f:(fun local ->
        let test_fn = "test_" ^ makearray_dynamic_fn ~uninit ~local in
        seq_print_in_test test_fn;
        line "iter sizes ~f:%s;" test_fn
      )
    )
  );
  line "for i = 1 to %d do" !test_id;
  with_indent (fun () ->
    line
    {|if not (List.mem ~set:!tests_run i) then failwithf "test %%d not run" i|}
  );
  line "done;;";
  print_in_test "All tests passed."

(* We partition the list of interesting types into N partitions, to cut down on test
   runtime *)
let () =
  let bytecode, n, i =
    match Sys.argv with
    | [| _; "native"; n; i |] ->
      false, n, i
    | [| _; "byte"; n; i |] ->
      true, n, i
    | _ ->
      failwith
        (sprintf
          "Usage: %s <byte|native> PARTITION NUM_PARTITIONS\n\
            1 <= PARTITION <= NUM_PARTITIONS"
          Sys.argv.(0))
  in
  let num_partitions = int_of_string n in
  let partition = int_of_string i in
  assert (partition >= 1);
  assert (partition <= num_partitions);
  main ~bytecode ~partition ~num_partitions
