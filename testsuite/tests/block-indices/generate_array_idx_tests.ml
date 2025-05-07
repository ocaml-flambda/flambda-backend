(* This file is used in [run_array_idx_tests.ml]. *)

(* See [test_array_idx_get_and_set] for the main testing steps! *)

open Stdlib_upstream_compatible
open Stdlib_stable
module String = StringLabels
module List = ListLabels
open Metaprogramming

let sprintf = Printf.sprintf

let failwithf fmt = Printf.ksprintf failwith fmt

let interesting_type_trees : Type_structure.t Tree.t list =
  (* There are possible type trees, exponential in the size of the tree and the
     number of types we consider.

     Here, we strike a balance by combining e.g. a collection of trees with
     many/complex shapes but few types and a collection of trees with a few
     shapes but many types. And so on. *)
  let open Gen_type in
  let deep_trees : unit Tree.t list =
    [ Branch [Leaf (); Branch [Leaf (); Leaf ()]];
      Branch [Branch [Leaf (); Leaf ()]; Leaf ()]
    ]
  in
  List.concat_map deep_trees ~f:(fun shape ->
      Tree.enumerate ~shape ~leaves:[Int64_u; String]
  )
  @ List.concat_map (Tree.enumerate_shapes ~max_num_nodes:4) ~f:(fun shape ->
        Tree.enumerate ~shape ~leaves:[Int; Int64; Int32_u; Float; Int64x2_u]
    )
  @ List.concat_map (Tree.enumerate_shapes ~max_num_nodes:3) ~f:(fun shape ->
        Tree.enumerate ~shape
          ~leaves:[Int; Int64; Int32_u; Float; Int64_u; Nativeint_u]
    )
  @ [ Branch
        [Branch [Leaf Int; Leaf Int64_u]; Branch [Leaf Int64_u; Leaf Float_u]];
      (* An int64x2 that would be reordered to the gap of a "sibling" record *)
      Branch
        [Branch [Leaf Int64x2_u; Leaf String]; Branch [Leaf Int64; Leaf Float_u]];
      (* An int64x2 that would be reordered to the gap of an inner record *)
      Branch [Leaf Int64x2_u; Branch [Leaf String; Leaf Float_u]]
    ]

let array_element_types : Type_structure.t list =
  List.filter_map interesting_type_trees ~f:Type_structure.array_element

let nested_record_types : Type_structure.t list =
  List.filter_map interesting_type_trees
    ~f:Type_structure.boxed_record_containing_unboxed_records

(* let () =
 *   List.iter array_element_types ~f:(fun ty ->
 *       Printf.printf "%s\n" (Type_structure.to_string ty)
 *   );
 *   List.iter nested_record_types ~f:(fun ty ->
 *       Printf.printf "%s\n" (Type_structure.to_string ty)
 *   );
 *   Printf.printf "%d\n"
 *     (Type_structure.compare
 *        (List.hd array_element_types)
 *        (List.hd nested_record_types)
 *     ) *)

let naming = Type_naming.empty

let (naming : Type_naming.t), (array_element_types : Type.t list) =
  List.fold_left_map array_element_types ~init:naming ~f:(fun naming ty ->
      Type_naming.add_names naming ty
  )

let (naming : Type_naming.t), (nested_record_types : Type.t list) =
  List.fold_left_map nested_record_types ~init:naming ~f:(fun naming ty ->
      Type_naming.add_names naming ty
  )

let preamble bytecode =
  {|

[@@@ocaml.warning "-23"]

open Stdlib_upstream_compatible
open Stdlib_stable

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

external[@layout_poly] get_idx_imm :
  'a ('b : any). ('a [@local_opt]) -> ('a, 'b) idx_imm -> ('b [@local_opt]) =
  "%unsafe_get_idx_imm"

external[@layout_poly] get_idx_mut :
  'a ('b : any). ('a [@local_opt]) -> ('a, 'b) idx_mut -> ('b [@local_opt]) =
  "%unsafe_get_idx"

external[@layout_poly] set_idx_mut :
  'a ('b : any).
    ('a [@local_opt]) -> ('a, 'b) idx_mut -> ('b [@local_opt]) -> unit =
  "%unsafe_set_idx"

    |}
  ^ ( if bytecode
    then ""
    else
      {|
external box_int64x2 : int64x2# -> int64x2 = "%box_vec128"
external unbox_int64x2 : int64x2 -> int64x2# = "%unbox_vec128"
external interleave_low_64 : int64x2# -> int64x2# -> int64x2# = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64" [@@unboxed] [@@builtin]
external interleave_high_64 : int64x2# -> int64x2# -> int64x2# = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64" [@@unboxed] [@@builtin]
external int64x2_of_int64 : int64 -> int64x2# = "caml_vec128_unreachable" "caml_int64x2_low_of_int64" [@@unboxed] [@@builtin]
external int64_of_int64x2 : int64x2# -> int64 = "caml_vec128_unreachable" "caml_int64x2_low_to_int64" [@@unboxed] [@@builtin]

let int64x2_u_equal i1 i2 =
    let a1 = int64_of_int64x2 i1 in
    let b1 = int64_of_int64x2 (interleave_high_64 i1 i1) in
    let a2 = int64_of_int64x2 i2 in
    let b2 = int64_of_int64x2 (interleave_high_64 i2 i2) in
    Int64.equal a1 a2 && Int64.equal b1 b2
|}
    )
  ^ {|

module Idx_repr : sig
  type t
  val of_idx_imm : 'a ('b : any). ('a, 'b) idx_imm -> t
  val of_idx_mut : 'a ('b : any). ('a, 'b) idx_mut -> t
  val equal : t -> t -> bool
  val debug_string : t -> string
end = struct
  (* See Note [Representation of block indices] in [lambda/translcore.ml] *)
  type t =
    | Bytecode of { path : int list }
    | Native of { offset : int; gap : int }

  external magic_box_bits64 : ('a : bits64) 'b . 'a -> 'b =
    "%box_int64"
  external lessthan_if_bytecode : int -> int -> bool =
    "caml_lessthan" "caml_greaterthan"

  let of_idx idx =
    let is_bytecode = lessthan_if_bytecode 0 1 in
    if is_bytecode then
      let r = Obj.repr (magic_box_bits64 idx) in
      let nth_idx n : int = Obj.magic (Obj.field r n) in
      let path = List.init (Obj.size r) nth_idx in
      Bytecode { path }
    else
      let i : int64 = magic_box_bits64 idx in
      let offset =
        Int64.(logand (sub (shift_left one 48) one)) i
        |> Int64.to_int
      in
      let gap =
        Int64.shift_right i 48
        |> Int64.to_int
      in
      Native { offset; gap }

  let of_idx_imm = of_idx
  let of_idx_mut = of_idx

  let equal t1 t2 =
    match t1, t2 with
    | Bytecode { path = path1 }, Bytecode { path = path2 } ->
      List.equal Int.equal path1 path2
    | Native { gap = gap1; offset = offset1 },
      Native { gap = gap2; offset = offset2 } ->
      Int.equal gap1 gap2 && Int.equal offset1 offset2
    | Bytecode _, Native _ | Native _, Bytecode _ -> assert false

  let debug_string = function
    | Bytecode { path } ->
      Printf.sprintf "{ %s }"
        (String.concat "; " (List.map Int.to_string path))
    | Native { offset; gap } ->
      Printf.sprintf "offset %d; gap %d" offset gap
end

let failwithf fmt = Printf.ksprintf failwith fmt

external globalize : local_ 'a -> 'a = "%obj_dup";;

(* Redefine iter to infer locality *)
let rec iter ~f = function
    [] -> ()
  | a::l -> f a; iter ~f l

module Int_set = Set.Make(Int)

let tests_run = ref Int_set.empty

let mark_test_run test_id =
  tests_run := Int_set.add test_id !tests_run

(* Various interesting values *)

let sizes = [ 0; 1; 2; 30 ]

let indices_in_deepening_tests = [0; 100_000]

|}

let indent = ref 0

let with_indent f =
  incr indent;
  f ();
  decr indent

let line fmt =
  Printf.ksprintf
    (fun s ->
      let indent = Seq.init (!indent * 2) (fun _ -> ' ') |> String.of_seq in
      print_endline (indent ^ s);
      flush stdout
    )
    fmt

let print_in_test s =
  line {|let () = Printf.printf "%s%%!\n";;|} (String.escaped s)

let seq_print_in_test s = line {|print_endline "%s%!";|} (String.escaped s)

let makearray_dynamic_fn ~local =
  let local_s = if local then "_local" else "" in
  "makearray_dynamic" ^ local_s

type debug_expr =
  { expr : string;
    format_s : string
  }

let concat_with_leading_spaces l =
  List.map l ~f:(fun s -> " " ^ s) |> String.concat ~sep:""

let combine_debug_exprs (l : debug_expr list) : debug_expr =
  let debug_expr_to_tuple { expr; format_s } = expr, format_s in
  let exprs, format_ss = List.split (List.rev_map ~f:debug_expr_to_tuple l) in
  let expr = concat_with_leading_spaces exprs in
  let format_s = concat_with_leading_spaces format_ss in
  { expr; format_s }

let seq_print_debug_exprs ~debug_exprs =
  let { expr; format_s } = combine_debug_exprs debug_exprs in
  line {|Printf.printf "%s: %s\n%%!"%s;|} expr format_s expr

let test_id = ref 0

let seq_assert ~debug_exprs s =
  incr test_id;
  let { expr; format_s } = combine_debug_exprs debug_exprs in
  line "mark_test_run %d;" !test_id;
  line "let test = %s in" s;
  line {|if not test then failwithf "test %d failed%s"%s;|} !test_id format_s
    expr

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
      let debug_exprs = { expr = var; format_s = "%d" } :: debug_exprs in
      f ~debug_exprs
  );
  line ") [@nontail];"

let section s =
  let s_as_stars = String.init (String.length s) ~f:(fun _ -> '*') in
  line "(**%s**)" s_as_stars;
  line "(* %s *)" s;
  line "(**%s**)" s_as_stars

let type_section (ty : Type.t) =
  let header =
    match ty with
    | Record _ ->
      (* show the structure of nominal types to reduce definition-chasing *)
      sprintf "%s = %s" (Type.code ty)
        (Type_structure.to_string (Type.structure ty))
    | _ -> Type.code ty
  in
  section ("  " ^ header ^ "  ")

let test_array_idx_get_and_set ~local ty =
  let makearray_dynamic = makearray_dynamic_fn ~local in
  let debug_exprs = [{ expr = "size"; format_s = "%d" }] in
  let ty_array_s = Type.code ty ^ " array" in
  type_section ty;
  line "let eq = %s in" (Type.eq_code ty);
  line "let mk_value i = %s in" (Type.mk_value_body_code ty);
  line "(* 1. Create an array of size [size] *)";
  line "let a : %s = %s size %s in" ty_array_s makearray_dynamic
    (Type.value_code ty 0);
  line "(* 3. Fill [a] with distinct values using block indices *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      line "set_idx_mut a (.(i)) (mk_value i);"
  );
  line "Gc.compact ();";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      seq_assert ~debug_exprs "eq (get a i) (mk_value i)"
  );
  line "(* Also read back those values with block indices *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      seq_assert ~debug_exprs "eq (get_idx_mut a (.(i))) (mk_value i)"
  );
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      List.iter (Type.unboxed_paths_by_depth ty)
        ~f:(fun (depth, unboxed_paths) ->
          line "(* Paths of depth %d *)" depth;
          line "let el = get a i in";
          line "let next_el = mk_value (i + 100 * %d) in" depth;
          let up_concat l =
            String.concat (List.map l ~f:(fun s -> ".#" ^ s)) ~sep:""
          in
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              line "(* %s *)" (up_concat unboxed_path);
              let reference_update =
                (* To perform our reference update (without block indices) to
                   [el.x.y.z], we generate [#{ el with x = #{ el.#x with y = #{
                   el.#x.#y with z = next_el.#x.#y.#z } } }] *)
                let rec f rev_path new_val =
                  match rev_path with
                  | [] -> new_val
                  | s :: rest ->
                    let new_val =
                      sprintf "#{ el%s with %s = %s }"
                        (up_concat (List.rev rest))
                        s new_val
                    in
                    f rest new_val
                in
                f (List.rev unboxed_path)
                  (sprintf "next_el%s" (up_concat unboxed_path))
              in
              line "let el = %s in" reference_update;
              line "set_idx_mut a ((.(i)%s) : (%s array, _) idx_mut) next_el%s;"
                (up_concat unboxed_path) (Type.code ty) (up_concat unboxed_path);
              seq_assert ~debug_exprs "eq (get_idx_mut a (.(i))) el"
          )
      );
      line "()"
  );
  line "Gc.compact ();";
  print_newline ()

(* Splits a list into the first N and the remaining elements *)
let take_n l n =
  List.mapi l ~f:(fun i x -> i, x)
  |> List.partition_map ~f:(fun (i, x) -> if i < n then Left x else Right x)

let test_array_idx_deepening ty =
  (* Include the empty unboxed path to test the "identity" deepening *)
  let unboxed_paths_by_depth = (0, [[]]) :: Type.unboxed_paths_by_depth ty in
  let debug_exprs = [] in
  let ty_array_s = Type.code ty ^ " array" in
  type_section ty;
  let up_concat l = String.concat (List.map l ~f:(fun s -> ".#" ^ s)) ~sep:"" in
  List.iter unboxed_paths_by_depth ~f:(fun (depth, unboxed_paths) ->
      List.iter unboxed_paths ~f:(fun unboxed_path ->
          line "iter indices_in_deepening_tests ~f:(fun i ->";
          with_indent (fun () ->
              line "let unboxed_path : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                (up_concat unboxed_path);
              for prefix_len = 0 to depth do
                let prefix, suffix = take_n unboxed_path prefix_len in
                line "let shallow : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                  (up_concat prefix);
                line "let deepened = (.idx_mut(shallow)%s) in" (up_concat suffix);
                seq_assert ~debug_exprs
                  "Idx_repr.equal (Idx_repr.of_idx_mut unboxed_path) \
                   (Idx_repr.of_idx_mut deepened)"
              done
          );
          line ");"
      )
  );
  print_newline ()

let toplevel_unit_block f =
  assert (Int.equal !indent 0);
  line "let () =";
  with_indent (fun () ->
      f ();
      line "()"
  );
  line ";;";
  line ""

let main ~bytecode =
  line {|(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;|};
  if bytecode
  then (
    line {| flags = "-extension layouts_alpha";|};
    line {| bytecode;|}
  )
  else (
    line {| modules = "stubs.c";|};
    line {| flags = "-extension simd_beta -extension layouts_alpha";|};
    line {| flambda2;|};
    line {| stack-allocation;|};
    line {| native;|}
  );
  line {|*)|};
  line "(** This is code generated by [generate_array_idx_tests.ml]. *)";
  line "";
  line "%s" (preamble bytecode);
  List.iter (Type_naming.decls_code naming) ~f:(fun s -> line "%s" s);
  line "";
  List.iter [false; true] ~f:(fun local ->
      line "let test_array_idx_with_%s size =" (makearray_dynamic_fn ~local);
      with_indent (fun () ->
          List.iter array_element_types ~f:(test_array_idx_get_and_set ~local);
          line "()"
      );
      line ""
  );
  line "(* Test array idx deepening *)";
  line "let () =";
  with_indent (fun () ->
      List.iter array_element_types ~f:test_array_idx_deepening;
      line "()"
  );
  line "(* *)";
  toplevel_unit_block (fun () ->
      List.iter [false; true] ~f:(fun local ->
          let test_fn = "test_array_idx_with_" ^ makearray_dynamic_fn ~local in
          seq_print_in_test test_fn;
          line "iter sizes ~f:%s;" test_fn
      )
  );
  line "for i = 1 to %d do" !test_id;
  with_indent (fun () ->
      line
        {|if not (Int_set.mem i !tests_run) then failwithf "test %%d not run" i|}
  );
  line "done;;";
  print_in_test "All tests passed."

let () =
  let bytecode =
    match Sys.argv with
    | [| _; "native" |] -> false
    | [| _; "bytecode" |] -> true
    | _ -> failwith (sprintf "Usage %s <bytecode|native>" Sys.argv.(0))
  in
  main ~bytecode
