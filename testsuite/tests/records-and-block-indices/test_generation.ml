(* This file is used in [run_test_generation.ml]. *)

open Stdlib_upstream_compatible
open Stdlib_stable
module String = StringLabels
module List = ListLabels
open Metaprogramming

let sprintf = Printf.sprintf

let failwithf fmt = Printf.ksprintf failwith fmt

let interesting_type_trees : Type_structure.t Tree.t list =
  (* There are many possible type trees, exponential in the size of the tree and
     the number of types we consider.

     We balance these factors by combining e.g. a collection of trees with
     many/complex shapes but few types and a collection of trees with a few
     shapes but many types. And so on. *)
  let open Type_structure in
  List.concat_map
    [[Int; Float]; [Int; Int32_u]; [Int; Int64x2_u] (* [Float; Float_u] *)]
    ~f:(fun leaves ->
      List.concat_map
        (Tree.enumerate_shapes' ~max_leaves_and_singleton_branches:3)
        ~f:(fun shape -> Tree.enumerate ~shape ~leaves
      )
    )
  @ List.concat_map
      (Tree.enumerate_shapes' ~max_leaves_and_singleton_branches:2)
      ~f:(fun shape ->
        Tree.enumerate ~shape
          ~leaves:[Int; Int64; Int32_u; Float; Int64_u; Nativeint_u]
    )
  @ (* Some particular interesting trees *)
  [ Branch
      (* Mixed then all flat *)
      [Branch [Leaf Int64; Leaf Int64_u]; Branch [Leaf Int64_u; Leaf Float_u]];
    Branch
      (* Mixed then all values *)
      [Branch [Leaf Int64_u; Leaf Int64]; Branch [Leaf Int64; Leaf Int64]];
    Branch
      (* All values then mixed *)
      [Branch [Leaf Int64; Leaf String]; Branch [Leaf Int64_u; Leaf String]];
    Branch
      (* All flats then mixed *)
      [Branch [Leaf Float32_u; Leaf Int64_u]; Branch [Leaf String; Leaf Int64_u]];
    Branch
      (* Mixed then mixed *)
      [Branch [Leaf Int64_u; Leaf Int64]; Branch [Leaf Float32_u; Leaf Float]];
    Branch
      (* An int64x2 that would be reordered to the gap of a "sibling" record *)
      [Branch [Leaf Int64x2_u; Leaf String]; Branch [Leaf Int64; Leaf Float_u]];
    Branch
      (* An int64x2 that would be reordered to the gap of an inner record *)
      [Leaf Int64x2_u; Branch [Leaf String; Leaf Float_u]]
  ]
  |> List.sort_uniq ~cmp:(Tree.compare Type_structure.compare)

let preamble =
  {|open Stdlib_upstream_compatible
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

let test_array_idx_access ~local ty =
  let makearray_dynamic = makearray_dynamic_fn ~local in
  let debug_exprs = [{ expr = "size"; format_s = "%d" }] in
  let ty_array_s = Type.code ty ^ " array" in
  type_section ty;
  line "leqet eq = %s in" (Type.eq_code ty);
  line "let mk_value i = %s in" (Type.mk_value_body_code ty);
  line "(* Create an array of size [size] *)";
  line "let a : %s = %s size %s in" ty_array_s makearray_dynamic
    (Type.value_code ty 0);
  line "(* Fill [a] with distinct values using block indices *)";
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
          if depth = 0
          then ()
          else (
            line "(* Paths of depth %d *)" depth;
            line "let el = get a i in";
            line "let next_el = mk_value (i + 100 * %d) in" depth;
            List.iter unboxed_paths ~f:(fun unboxed_path ->
                line "(* Path: [%s] *)" (Path.to_string unboxed_path);
                let reference_update =
                  (* To perform our reference update (without block indices) to
                     [el.#x.#y.#z], we generate [#{ el with x = #{ el.#x with y
                     = #{ el.#x.#y with z = next_el.#x.#y.#z } } }] *)
                  let rec f (rev_path : Path.t) new_val =
                    match rev_path with
                    | [] -> new_val
                    | Field _ :: _ -> assert false
                    | Unboxed_field s :: rest ->
                      let new_val =
                        sprintf "#{ el%s with %s = %s }"
                          (Path.to_string (List.rev rest))
                          s new_val
                      in
                      f rest new_val
                  in
                  f (List.rev unboxed_path)
                    (sprintf "next_el%s" (Path.to_string unboxed_path))
                in
                line "let el = %s in" reference_update;
                line
                  "set_idx_mut a ((.(i)%s) : (%s array, _) idx_mut) next_el%s;"
                  (Path.to_string unboxed_path)
                  (Type.code ty)
                  (Path.to_string unboxed_path);
                (* CR rtjoa: need anothe assert here *)
                seq_assert ~debug_exprs "eq (get_idx_mut a (.(i))) el"
            )
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
  let unboxed_paths_by_depth = Type.unboxed_paths_by_depth ty in
  let debug_exprs = [] in
  let ty_array_s = Type.code ty ^ " array" in
  type_section ty;
  List.iter unboxed_paths_by_depth ~f:(fun (depth, unboxed_paths) ->
      List.iter unboxed_paths ~f:(fun unboxed_path ->
          line "iter indices_in_deepening_tests ~f:(fun i ->";
          with_indent (fun () ->
              line "let unboxed_path : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                (Path.to_string unboxed_path);
              for prefix_len = 0 to depth do
                let prefix, suffix = take_n unboxed_path prefix_len in
                line "let shallow : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                  (Path.to_string prefix);
                line "let deepened = (.idx_mut(shallow)%s) in"
                  (Path.to_string suffix);
                seq_assert ~debug_exprs
                  "Idx_repr.equal (Idx_repr.of_idx_mut unboxed_path) \
                   (Idx_repr.of_idx_mut deepened)"
              done
          );
          line ");"
      )
  );
  print_newline ()

let test_record_idx_access ty ~local =
  type_section ty;
  line "let eq = %s in" (Type.eq_code ty);
  let fields =
    match ty with
    | Record { name = _; fields; boxing = Boxed } -> fields
    | _ -> invalid_arg "expected boxed record"
  in
  let debug_exprs = [] in
  List.iter fields ~f:(fun (lbl, fld_t) ->
      line "let r = %s in" (Type.value_code ty 0);
      List.iter (Type.unboxed_paths_by_depth fld_t)
        ~f:(fun (depth, unboxed_paths) ->
          line "(* Paths of depth %d *)" (depth + 1);
          line "let next_r = %s in" (Type.value_code ty (100 * (depth + 1)));
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              line "(* .%s%s *)" lbl (Path.to_string unboxed_path);
              let full_path = Path.Field lbl :: unboxed_path in
              let flattened_float =
                Type_structure.is_flat_float_record (Type.structure ty)
                && Type_structure.layout
                     (Type.structure (Type.follow_path ty full_path))
                   = Value Float
              in
              let sub_ty =
                if flattened_float
                then (
                  line "(* ff *)";
                  Type.Float_u
                )
                else Type.follow_path ty full_path
              in
              line "let sub_eq = %s in" (Type.eq_code sub_ty);
              let reference_update =
                (* To perform our reference update (without block indices) to
                   [el.x.#y.#z], we generate [{ el with x = #{ el.#x with y = #{
                   el.x.#y with z = next_el.x.#y.#z } } }] *)
                let rec f (rev_path : Path.t) new_val =
                  match rev_path with
                  | [] -> new_val
                  | Field _ :: _ -> assert false
                  | Unboxed_field s :: rest ->
                    let new_val =
                      sprintf "#{ r.%s%s with %s = %s }" lbl
                        (Path.to_string (List.rev rest))
                        s new_val
                    in
                    f rest new_val
                in
                let new_field =
                  f (List.rev unboxed_path)
                    (sprintf "next_r.%s%s" lbl (Path.to_string unboxed_path))
                in
                sprintf "{ r with %s = %s }" lbl new_field
              in
              line "let expected = %s in" reference_update;
              let idx =
                sprintf "((.%s%s) : (%s, _) idx_mut)" lbl
                  (Path.to_string unboxed_path)
                  (Type.code ty)
              in
              let next_r_sub_element_flat =
                if flattened_float
                then
                  (* next_r at some path must be a float(#) or nested singleton
                     unboxed records to one *)
                  let ty' = ty in
                  let rec path_to_float (ty : Type.t) =
                    match ty with
                    | Float | Float_u -> []
                    | Record
                        { fields = [(lbl, ty)]; boxing = Unboxed; name = _ } ->
                      Path.Unboxed_field lbl :: path_to_float ty
                    | Tuple (_, Unboxed) -> failwith "unimplemented"
                    | _ ->
                      failwith
                        (sprintf "ty %s; %s; subty %s; stuck at %s"
                           (Type.code ty') (Path.to_string full_path)
                           (Type.code (Type.follow_path ty' full_path))
                           (Type.code ty)
                        )
                  in
                  sprintf "(Float_u.of_float next_r%s%s)"
                    (Path.to_string full_path)
                    (Path.to_string
                       (path_to_float (Type.follow_path ty full_path))
                    )
                else sprintf "next_r%s" (Path.to_string full_path)
              in
              line "set_idx_mut r %s %s;" idx next_r_sub_element_flat;
              seq_assert ~debug_exprs "eq r expected";
              seq_assert ~debug_exprs
                (sprintf "sub_eq (get_idx_mut r %s) %s" idx
                   next_r_sub_element_flat
                )
          )
      )
  )

let test_record_idx_deepening ty =
  (* CR rtjoa: implement *)
  type_section ty

let test_record_size ty ~bytecode =
  type_section ty;
  line "let r = %s in" (Type.value_code ty 0);
  let expected_size = Type_structure.size (Type.structure ty) ~bytecode in
  seq_assert ~debug_exprs:[]
    (sprintf "Int.equal (Obj.size (Obj.repr r)) %d" expected_size)

let test_record_access ty ~local =
  type_section ty;
  let stack_if_local = if local then "stack_ " else "" in
  line "let r = %s%s in" stack_if_local (Type.value_code ty 0);
  line "(* 1. Test field get *)";
  let fields =
    match ty with
    | Record { name = _; fields; boxing = Boxed } -> fields
    | _ -> invalid_arg "expected boxed record"
  in
  List.iter fields ~f:(fun (lbl, fld_t) ->
      List.iter (Type.unboxed_paths_by_depth fld_t)
        ~f:(fun (depth, unboxed_paths) ->
          line "(* Paths of depth %d *)" (depth + 1);
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              line "(* .%s%s *)" lbl (Path.to_string unboxed_path);
              let full_path = Path.Field lbl :: unboxed_path in
              let sub_ty = Type.follow_path ty full_path in
              line "let actual = r%s in" (Path.to_string full_path);
              line "let expected = %s in"
                (Type.value_code sub_ty
                   (Type.num_subvals_left_of_path ty full_path)
                );
              seq_assert ~debug_exprs:[]
                (sprintf "%s actual expected" (Type.eq_code sub_ty))
          )
      )
  );
  line "Gc.compact ();";
  line "(* 2. Test field set *)";
  line "(* Change [r] to [next_r] one field at a time *)";
  line "let eq = %s in" (Type.eq_code ty);
  (* CR layouts v7.1: test "fine-grained" record sets once we support those *)
  line "let next_r = %s%s in" stack_if_local (Type.value_code ty 100);
  line "let r_expected = %s%s in" stack_if_local (Type.value_code ty 0);
  List.iter fields ~f:(fun (lbl, fld_t) ->
      line "(* .%s *)" lbl;
      line "r.%s <- next_r.%s;" lbl lbl;
      line "let r_expected = { r_expected with %s = next_r.%s } in" lbl lbl;
      seq_assert ~debug_exprs:[] "eq r r_expected";
      line "Gc.compact ();"
  );
  line "(* 3. Test deep matching *)";
  let rec mk_deep_record_el_pat (ty : Type.t) : string option =
    match ty with
    | Record { fields; boxing = Unboxed } ->
      let field_pats =
        List.map fields ~f:(fun (s, fld_ty) ->
            match mk_deep_record_el_pat fld_ty with
            | Some pat -> pat
            | None -> s
        )
      in
      Some (sprintf "#{ %s }" (String.concat ~sep:"; " field_pats))
    | _ -> None
  in
  (* Make a pattern for a nested record that binds a variable for each field
     name (which are guaranteed to be distinct by [Metaprogramming.Type_naming].
     If deep, then also binds fields of contained unboxed records. *)
  let rec mk_record_pat (ty : Type.t) ~deep =
    match ty with
    | Record { fields; boxing = Boxed } ->
      let field_pats =
        List.map fields ~f:(fun (s, fld_ty) ->
            if deep
            then
              match mk_deep_record_el_pat fld_ty with
              | Some pat -> sprintf "%s = %s" s pat
              | None -> s
            else s
        )
      in
      sprintf "{ %s }" (String.concat ~sep:"; " field_pats)
    | _ -> invalid_arg "expected boxed record"
  in
  let pat = mk_record_pat ty ~deep:true in
  line "let %s = r in" pat;
  List.iter fields ~f:(fun (lbl, fld_ty) ->
      List.iter (Type.unboxed_paths_by_depth fld_ty)
        ~f:(fun (depth, unboxed_paths) ->
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              let full_path = Path.Field lbl :: unboxed_path in
              let sub_ty = Type.follow_path ty full_path in
              (* [sub_ty] must be terminal for it to be bound*)
              if Option.is_none (mk_deep_record_el_pat sub_ty)
              then (
                let last_lbl =
                  match List.hd (List.rev full_path) with
                  | Path.Field lbl | Path.Unboxed_field lbl -> lbl
                in
                let sub_ty = Type.follow_path ty full_path in
                line "let expected_%s = %s in" last_lbl
                  (Type.value_code sub_ty
                     (Type.num_subvals_left_of_path ty full_path + 100)
                  );
                seq_assert ~debug_exprs:[]
                  (sprintf "%s expected_%s %s" (Type.eq_code sub_ty) last_lbl
                     last_lbl
                  )
              )
          )
      )
  );
  line "Gc.compact ();";
  line "(* 4. Test shallow matching *)";
  let pat = mk_record_pat ty ~deep:false in
  line "let %s = r in" pat;
  List.iter fields ~f:(fun (lbl, fld_ty) ->
      line "let expected_%s = %s in" lbl
        (Type.value_code fld_ty
           (Type.num_subvals_left_of_path ty [Path.Field lbl] + 100)
        );
      seq_assert ~debug_exprs:[]
        (sprintf "%s expected_%s %s" (Type.eq_code fld_ty) lbl lbl)
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

type test =
  | Array_idx_access of { local : bool }
  | Record_idx_access of { local : bool }
  | Array_idx_deepening
  | Record_idx_deepening
  | Record_size
  | Record_access of { local : bool }

let main test ~bytecode =
  let types =
    match test with
    | Array_idx_access _ | Array_idx_deepening ->
      List.filter_map interesting_type_trees ~f:Type_structure.array_element
    | Record_idx_access _ | Record_idx_deepening | Record_size _
    | Record_access _ ->
      List.filter_map interesting_type_trees
        ~f:Type_structure.boxed_record_containing_unboxed_records
  in
  let types =
    if bytecode
    then List.filter types ~f:(fun ty -> not (Type_structure.contains_vec128 ty))
    else types
  in
  let naming = Type_naming.empty in
  let (naming : Type_naming.t), (types : Type.t list) =
    List.fold_left_map types ~init:naming ~f:(fun naming ty ->
        Type_naming.add_names naming ty
    )
  in
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
  line "(** This is code generated by [test_generation.ml]. *)";
  line "";
  line {|[@@@warning "-23"]|};
  line "%s" (Metaprogramming.preamble ~bytecode);
  line "%s" preamble;
  List.iter (Type_naming.decls_code naming) ~f:(fun s -> line "%s" s);
  line "";
  begin
    match test with
    | Array_idx_access { local } ->
      line "let test size =";
      with_indent (fun () ->
          List.iter types ~f:(test_array_idx_access ~local);
          line "()"
      );
      print_endline ";;";
      print_newline ();
      toplevel_unit_block (fun () -> line "iter sizes ~f:test;")
    | Array_idx_deepening ->
      toplevel_unit_block (fun () -> List.iter types ~f:test_array_idx_deepening)
    | Record_idx_access { local } ->
      toplevel_unit_block (fun () ->
          List.iter types ~f:(test_record_idx_access ~local)
      )
    | Record_idx_deepening ->
      toplevel_unit_block (fun () ->
          List.iter types ~f:test_record_idx_deepening
      )
    | Record_size ->
      toplevel_unit_block (fun () ->
          List.iter types ~f:(test_record_size ~bytecode)
      )
    | Record_access { local } ->
      toplevel_unit_block (fun () ->
          List.iter types ~f:(test_record_access ~local)
      )
  end;
  line "for i = 1 to %d do" !test_id;
  with_indent (fun () ->
      line
        {|if not (Int_set.mem i !tests_run) then failwithf "test %%d not run" i|}
  );
  line "done;;";
  print_in_test "All tests passed."

let tests =
  [ "array_idx_access", Array_idx_access { local = false };
    "array_idx_access_local", Array_idx_access { local = true };
    "record_idx_access", Record_idx_access { local = false };
    "record_idx_access_local", Record_idx_access { local = true };
    "array_idx_deepening", Array_idx_deepening;
    "record_idx_deepening", Record_idx_deepening;
    "record_size", Record_size;
    "record_access", Record_access { local = false };
    "record_access_local", Record_access { local = true }
  ]

let () =
  let fail () =
    failwith
      (sprintf "Usage %s <bytecode|native> <%s>" Sys.argv.(0)
         (String.concat ~sep:"|" (List.map ~f:fst tests))
      )
  in
  let bytecode, test_name =
    match Sys.argv with
    | [| _; "native"; test_name |] -> false, test_name
    | [| _; "bytecode"; test_name |] -> true, test_name
    | _ -> fail ()
  in
  let test =
    match List.assoc_opt test_name tests with
    | Some test -> test
    | None -> fail ()
  in
  main test ~bytecode
