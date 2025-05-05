(* This file is used in [run_array_idx_tests.ml]. *)
open Stdlib_upstream_compatible
open Stdlib_stable
module List = ListLabels
module String = StringLabels

let failwithf fmt = Printf.ksprintf failwith fmt

let sprintf = Printf.sprintf

(* See [test_array_idx_get_and_set] for the main testing steps! *)

module IntMap = Map.Make (Int)

module Ty : sig
  (** A type in the generated code *)
  type t

  (** Code for this type expression (e.g. "int option * float") *)
  val ty_code : t -> string

  (** Given some integer seed, generate code for a value of this type.
       E.g. passing 3 gives "(Some 3, 4.)" for [int option * float]. *)
  val value_code : t -> int -> string

  (** The number of subvalues of this type, e.g. [int option * #(float * float)]
      has three. *)
  val num_subvals : t -> int

  (** Code that dynamically implements [value_code], creating a value from an
       integer seed bound to "i".
       We should be able to generate this code:
       "let mk_value (i : int) : $ty_code = $mk_value_code" *)
  val mk_value_code : t -> string

  (** A function that implements equality in the generated code.
       We should be able generate this code:
       "let eq : $ty_code @ local -> $ty_code @ local -> bool = $eq" *)
  val eq : t -> string

  val unboxed_paths_by_depth : t -> string list list IntMap.t

  (** Generate typedecls for user-defined nominal types that have been created *)
  val decls_code : unit -> string list

  (** Takes the record name and (label_name, label_type) pairs *)
  val unboxed_record : string -> (string * t) list -> t

  (** [enum 3] represents [type enum3 = A3_0 | A3_1 | A3_2]. *)
  val enum : int -> t

  (* Structural and built-in types *)

  val option : t -> t

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
  type kind =
    | Addr
    | Immediate
    | Non_value

  type t =
    | Unboxed_record of
        { name : string;
          fields : (string * t) list
        }
    | Unboxed_tuple of t list
    | Non_product of
        { ty_code : string;
          value_code : int -> string;
          mk_value_body_code : int -> string;
          eq : string;
          kind : kind
        }

  let assemble_unboxed_record colon_or_eq fields vals =
    let labeled_fields =
      List.map2 fields vals ~f:(fun (s, _) x -> s ^ " " ^ colon_or_eq ^ " " ^ x)
    in
    "#{ " ^ String.concat ~sep:"; " labeled_fields ^ " }"

  let assemble_unboxed_record_expr name fields vals =
    "(" ^ assemble_unboxed_record "=" fields vals ^ " : " ^ name ^ ")"

  let assemble_unboxed_tuple ~sep xs = sprintf "#(%s)" (String.concat ~sep xs)

  let rec ty_code = function
    | Unboxed_record { name; fields } -> name
    | Unboxed_tuple ts ->
      assemble_unboxed_tuple ~sep:" * " (List.map ts ~f:ty_code)
    | Non_product { ty_code; _ } -> ty_code

  let rec num_subvals t =
    match t with
    | Unboxed_record { name = _; fields } ->
      List.fold_left fields ~f:(fun acc (_, t) -> acc + num_subvals t) ~init:0
    | Unboxed_tuple ts ->
      List.fold_left ts ~f:(fun acc t -> acc + num_subvals t) ~init:0
    | Non_product { eq = _; _ } -> 1

  let rec reversed_unboxed_paths t acc cur_path =
    match t with
    | Unboxed_record { name = _; fields } ->
      List.fold_left ~init:acc fields ~f:(fun acc (s, t) ->
          let cur_path = s :: cur_path in
          let acc = cur_path :: acc in
          reversed_unboxed_paths t acc cur_path
      )
    | Unboxed_tuple _ -> acc
    | Non_product _ -> acc

  let unboxed_paths_by_depth t =
    List.fold_left
      (reversed_unboxed_paths t [] [])
      ~f:(fun acc rev_path ->
        let depth = List.length rev_path in
        let path = List.rev rev_path in
        let paths =
          match IntMap.find_opt depth acc with
          | Some paths -> path :: paths
          | None -> [path]
        in
        IntMap.add depth paths acc
      )
      ~init:IntMap.empty

  let rec value_code t i =
    match t with
    | Unboxed_record { name; fields } ->
      let _, xs =
        List.fold_left_map fields
          ~f:(fun acc (_, t) ->
            let x = value_code t acc in
            acc + num_subvals t, x
          )
          ~init:i
      in
      assemble_unboxed_record_expr name fields xs
    | Unboxed_tuple ts ->
      let _, xs =
        List.fold_left_map ts
          ~f:(fun acc t ->
            let x = value_code t acc in
            acc + num_subvals t, x
          )
          ~init:i
      in
      assemble_unboxed_tuple ~sep:", " xs
    | Non_product { value_code; _ } -> value_code i

  let rec mk_value_body_code t i =
    match t with
    | Unboxed_record { name; fields } ->
      let _, xs =
        List.fold_left_map fields
          ~f:(fun acc (_, t) ->
            let x = mk_value_body_code t acc in
            acc + num_subvals t, x
          )
          ~init:i
      in
      assemble_unboxed_record_expr name fields xs
    | Unboxed_tuple ts ->
      let _, xs =
        List.fold_left_map ts
          ~f:(fun acc t ->
            let x = mk_value_body_code t acc in
            acc + num_subvals t, x
          )
          ~init:i
      in
      assemble_unboxed_tuple ~sep:", " xs
    | Non_product { mk_value_body_code; _ } -> mk_value_body_code i

  let mk_value_code t = mk_value_body_code t 0

  let rec eq = function
    | Unboxed_record { name; fields } ->
      let body =
        List.map fields ~f:(fun (s, t) -> sprintf "%s %s1 %s2" (eq t) s s)
        |> String.concat ~sep:" && "
      in
      let pat i =
        assemble_unboxed_record_expr name fields
          (List.map fields ~f:(fun (s, _) -> s ^ Int.to_string i))
      in
      sprintf "(fun %s %s -> %s)" (pat 1) (pat 2) body
    | Unboxed_tuple ts ->
      let pat s =
        assemble_unboxed_tuple ~sep:", "
          (List.mapi ts ~f:(fun i _ -> s ^ Int.to_string i))
      in
      let body =
        List.mapi ts ~f:(fun i t -> sprintf "%s a%d b%d" (eq t) i i)
        |> String.concat ~sep:" && "
      in
      sprintf "(fun %s %s -> %s)" (pat "a") (pat "b") body
    | Non_product { eq; _ } -> eq

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
      if not (String.equal def def')
      then
        failwithf "%s has conflicting definitions:\n  %s\nand\n  %s" name def'
          def
    | None -> decls := (name, def) :: !decls

  let unboxed_record name fields =
    let xs = List.map ~f:(fun (_, t) -> ty_code t) fields in
    add_decl ~name ~def:(assemble_unboxed_record ":" fields xs);
    Unboxed_record { name; fields }

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
    let mk_value_body_code i =
      let brs =
        List.init ~len:size ~f:(fun i -> sprintf "%d -> %s" i (ith_ctor i))
        @ ["_ -> assert false"]
      in
      sprintf "(match Int.rem (i + %d) %d with %s)" i size
        (String.concat ~sep:" | " brs)
    in
    let name = sprintf "enum%d" size in
    add_decl ~name ~def;
    Non_product
      { ty_code = name;
        value_code = (fun i -> ith_ctor (Int.rem i size));
        mk_value_body_code;
        eq;
        kind = Immediate
      }

  let option t =
    Non_product
      { ty_code = ty_code t ^ " option";
        value_code =
          (fun i -> if Int.equal i 0 then "None" else "Some " ^ value_code t i);
        mk_value_body_code =
          (fun i ->
            sprintf "(if (i + %d) == 0 then None else Some (%s))" i
              (mk_value_body_code t i)
          );
        eq =
          "(fun a b -> match a, b with None,None -> true | Some a,Some b -> "
          ^ eq t ^ " a b|_->false)";
        kind = Addr
      }

  let unboxed_tuple ts = Unboxed_tuple ts

  let int =
    Non_product
      { ty_code = "int";
        value_code = (fun i -> Int.to_string i);
        mk_value_body_code = (fun i -> sprintf "i + %d" i);
        eq = "(fun a b -> Int.equal a b)";
        kind = Immediate
      }

  let float =
    Non_product
      { ty_code = "float";
        value_code = (fun i -> Int.to_string i ^ ".");
        mk_value_body_code = (fun i -> sprintf "Float.of_int (i + %d)" i);
        eq = "(fun a b -> Float.equal (globalize a) (globalize b))";
        kind = Addr
      }

  let float_u =
    Non_product
      { ty_code = "float#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ ".");
        mk_value_body_code = (fun i -> sprintf "Float_u.of_int (i + %d)" i);
        eq = "(fun a b -> Float_u.(equal (add #0. a) (add #0. b)))";
        kind = Non_value
      }

  let float32 =
    Non_product
      { ty_code = "float32";
        value_code = (fun i -> Int.to_string i ^ ".s");
        mk_value_body_code = (fun i -> sprintf "Float32.of_int (i + %d)" i);
        eq =
          "(fun a b -> Float.equal (Float32.to_float a) (Float32.to_float b))";
        kind = Addr
      }

  let float32_u =
    Non_product
      { ty_code = "float32#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ ".s");
        mk_value_body_code = (fun i -> sprintf "Float32_u.of_int (i + %d)" i);
        eq = "(fun a b -> Float32_u.(equal (add #0.s a) (add #0.s b)))";
        kind = Non_value
      }

  let int32 =
    Non_product
      { ty_code = "int32";
        value_code = (fun i -> Int.to_string i ^ "l");
        mk_value_body_code = (fun i -> sprintf "Int32.of_int (i + %d)" i);
        eq = "(fun a b -> Int32.equal (globalize a) (globalize b))";
        kind = Addr
      }

  let int32_u =
    Non_product
      { ty_code = "int32#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "l");
        mk_value_body_code = (fun i -> sprintf "Int32_u.of_int (i + %d)" i);
        eq = "(fun a b -> Int32_u.(equal (add #0l a) (add #0l b)))";
        kind = Non_value
      }

  let int64 =
    Non_product
      { ty_code = "int64";
        value_code = (fun i -> Int.to_string i ^ "L");
        mk_value_body_code = (fun i -> sprintf "Int64.of_int (i + %d)" i);
        eq = "(fun a b -> Int64.equal (globalize a) (globalize b))";
        kind = Addr
      }

  let int64_u =
    Non_product
      { ty_code = "int64#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "L");
        mk_value_body_code = (fun i -> sprintf "Int64_u.of_int (i + %d)" i);
        eq = "(fun a b -> Int64_u.(equal (add #0L a) (add #0L b)))";
        kind = Non_value
      }

  let nativeint =
    Non_product
      { ty_code = "nativeint";
        value_code = (fun i -> Int.to_string i ^ "n");
        mk_value_body_code = (fun i -> sprintf "Nativeint.of_int (i + %d)" i);
        eq = "(fun a b -> Nativeint.equal (globalize a) (globalize b))";
        kind = Addr
      }

  let nativeint_u =
    Non_product
      { ty_code = "nativeint#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "n");
        mk_value_body_code = (fun i -> sprintf "Nativeint_u.of_int (i + %d)" i);
        eq = "(fun a b -> Nativeint_u.(equal (add #0n a) (add #0n b)))";
        kind = Non_value
      }
end

let ty_ur1 = Ty.(unboxed_record "ur1" ["a", int64_u; "b", float_u])

let ty_ur2 = Ty.(unboxed_record "ur2" ["a", int; "b", int64_u])

let ty_ur3 = Ty.(unboxed_record "ur3" ["a", int64_u])

let ty_ur4 = Ty.(unboxed_record "ur4" ["a", ty_ur2; "b", ty_ur3])

let array_element_types =
  Ty.
    [ float32_u;
      float_u;
      int32_u;
      int64_u;
      nativeint_u;
      ty_ur1;
      ty_ur3;
      ty_ur4;
      float32;
      int32;
      int64;
      nativeint;
      int;
      enum 3;
      (* ty_ur2; *)
      unboxed_tuple [float_u; int32_u; int64_u];
      unboxed_tuple
        [ float_u;
          unboxed_tuple [int64_u; int64_u];
          float32_u;
          unboxed_tuple [int32_u; unboxed_tuple [float32_u; float_u]];
          int64_u
        ];
      unboxed_tuple [int64_u; ty_ur1];
      unboxed_tuple [int; int64];
      unboxed_tuple [option int64; int32; unboxed_tuple [int32; float]; float];
      unboxed_tuple [float; float; float];
      unboxed_tuple
        [ float;
          unboxed_tuple [float; float];
          unboxed_tuple [float; unboxed_tuple [float; float; float]]
        ]
    ]

type tree =
  | Branch of tree list
  | Leaf

let rec enumerate_forests num_nodes : tree list list =
  assert (num_nodes > 0);
  let possible_first_tree_num_nodes =
    List.init ~len:num_nodes ~f:(fun x -> x + 1)
  in
  List.concat_map possible_first_tree_num_nodes ~f:(fun first_tree_num_nodes ->
      List.concat_map (enumerate_trees first_tree_num_nodes)
        ~f:(fun first_tree ->
          let rest_num_nodes = num_nodes - first_tree_num_nodes in
          assert (rest_num_nodes >= 0);
          if Int.equal rest_num_nodes 0
          then [[first_tree]]
          else
            List.map (enumerate_forests rest_num_nodes) ~f:(fun rest_trees ->
                first_tree :: rest_trees
            )
      )
  )

and enumerate_trees num_nodes : tree list =
  assert (num_nodes >= 1);
  if Int.equal num_nodes 1
  then [Leaf]
  else
    List.map (enumerate_forests (num_nodes - 1)) ~f:(fun forest -> Branch forest)

let preamble =
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

let tests_run = ref []

let mark_test_run test_id =
  if not (List.mem test_id !tests_run) then
    tests_run := test_id :: !tests_run

(* Various interesting values *)

let sizes = [ 0; 1; 2; 30; 31; 32 ]

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

let test_array_idx_get_and_set ~local ty =
  let makearray_dynamic = makearray_dynamic_fn ~local in
  let debug_exprs = [{ expr = "size"; format_s = "%d" }] in
  let ty_array_s = Ty.ty_code ty ^ " array" in
  section ("  " ^ Ty.ty_code ty ^ "  ");
  line "let eq = %s in" (Ty.eq ty);
  line "let mk_value i = %s in" (Ty.mk_value_code ty);
  line "(* 1. Create an array of size [size] *)";
  line "let a : %s = %s size %s in" ty_array_s makearray_dynamic
    (Ty.value_code ty 0);
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
  let unboxed_paths_by_depth = Ty.unboxed_paths_by_depth ty in
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      IntMap.iter
        (fun depth unboxed_paths ->
          (* If r has structure #{ x = #{ y = #{ z } } } then our update is #{ r
             with x = #{ r.#x with y = #{ r.#x.#y with z = next_el.#x.#y.#z } }
             } *)
          line "(* Paths of depth %d *)" depth;
          line "let el = get a i in";
          line "let next_el = mk_value (i + 100 * %d) in" depth;
          let up_concat l =
            String.concat (List.map l ~f:(fun s -> ".#" ^ s)) ~sep:""
          in
          let rev_up_concat l = up_concat (List.rev l) in
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              line "(* %s *)" (up_concat unboxed_path);
              let rec f rev_path new_val =
                match rev_path with
                | [] -> new_val
                | s :: rest ->
                  let new_val =
                    sprintf "#{ el%s with %s = %s }" (rev_up_concat rest) s
                      new_val
                  in
                  f rest new_val
              in
              let new_val = sprintf "next_el%s" (up_concat unboxed_path) in
              line "let el = %s in" (f (List.rev unboxed_path) new_val);
              line "set_idx_mut a ((.(i)%s) : (%s array, _) idx_mut) next_el%s;"
                (up_concat unboxed_path) (Ty.ty_code ty) (up_concat unboxed_path);
              seq_assert ~debug_exprs "eq (get_idx_mut a (.(i))) el";
              ()
          )
        )
        unboxed_paths_by_depth;
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
  let unboxed_paths_by_depth =
    Ty.unboxed_paths_by_depth ty |> IntMap.add 0 [[]]
  in
  let debug_exprs = [] in
  let ty_array_s = Ty.ty_code ty ^ " array" in
  section ("  " ^ Ty.ty_code ty ^ "  ");
  let up_concat l = String.concat (List.map l ~f:(fun s -> ".#" ^ s)) ~sep:"" in
  IntMap.iter
    (fun depth unboxed_paths ->
      List.iter unboxed_paths ~f:(fun unboxed_path ->
          line "iter [0; 1; 2; 100_000] ~f:(fun i ->";
          with_indent (fun () ->
              line "let unboxed_path : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                (up_concat unboxed_path);
              for prefix_len = 0 to depth do
                let prefix, suffix = take_n unboxed_path prefix_len in
                line "let shallow  : (%s, _) idx_mut = (.(i)%s) in" ty_array_s
                  (up_concat prefix);
                line "let deepened = (.idx_mut(shallow)%s) in" (up_concat suffix);
                seq_assert ~debug_exprs
                  "Idx_repr.equal (Idx_repr.of_idx_mut unboxed_path) \
                   (Idx_repr.of_idx_mut deepened)"
              done
          );
          line ");"
      )
    )
    unboxed_paths_by_depth;
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
  let debug_exprs = [] in
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
  line "%s" preamble;
  List.iter (Ty.decls_code ()) ~f:(fun s -> line "%s" s);
  line "";
  line "(* Catch metaprogramming errors early *)";
  toplevel_unit_block (fun () ->
      let open Ty in
      line "(* Check types and constants *)";
      List.iter array_element_types ~f:(fun ty ->
          line "let _ : %s = %s in" (Ty.ty_code ty) (Ty.value_code ty 0)
      );
      line "(* Check equality and mk_value functions *)";
      List.iter array_element_types ~f:(fun ty ->
          line "let eq : %s @ local -> %s @ local -> bool = %s in"
            (Ty.ty_code ty) (Ty.ty_code ty) (Ty.eq ty);
          line "let mk_value i = %s in" (Ty.mk_value_code ty);
          seq_assert ~debug_exprs
            (sprintf "eq (mk_value 1) %s" (Ty.value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "eq %s %s" (Ty.value_code ty 1) (Ty.value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "not (eq %s %s)" (Ty.value_code ty 1) (Ty.value_code ty 2))
      )
  );
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
        {|if not (List.mem i !tests_run) then failwithf "test %%d not run" i|}
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
