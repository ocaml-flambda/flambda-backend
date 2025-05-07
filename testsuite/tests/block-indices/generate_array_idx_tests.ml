(* This file is used in [run_array_idx_tests.ml]. *)

(* See [test_array_idx_get_and_set] for the main testing steps! *)

open Stdlib_upstream_compatible
open Stdlib_stable

module List : sig
  include module type of ListLabels

  val fold_left_mapi :
    f:('a -> int -> 'b -> 'a * 'c) -> init:'a -> 'b list -> 'a * 'c list

  val fold_lefti : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b list -> 'a
end = struct
  include ListLabels

  let enumerate l = List.mapi (fun i x -> i, x) l

  let fold_left_mapi ~f ~init l =
    enumerate l |> List.fold_left_map (fun acc (i, x) -> f acc i x) init

  let fold_lefti ~f ~init l =
    enumerate l |> List.fold_left (fun acc (i, x) -> f acc i x) init
end

module String = StringLabels

let sprintf = Printf.sprintf

module Int_map = Map.Make (Int)

module Type_kind = struct
  type value_kind =
    | Addr
    | Immediate
    | Float

  type t =
    | Product of t list
    | Value of value_kind
    | Non_value
end

module Boxing = struct
  type t =
    | Boxed
    | Unboxed
end

module Gen_type = struct
  type 'a t =
    | Record of
        { name : 'a;
          fields : ('a * 'a t) list;
          boxing : Boxing.t
        }
    | Tuple of 'a t list * Boxing.t
    | Option of 'a t
    | Int
    | Int64
    | Int64_u
    | Int32
    | Int32_u
    | Nativeint
    | Nativeint_u
    | Float
    | Float_u
    | Float32
    | Float32_u
    | String
end

module Type_structure = struct
  type t = unit Gen_type.t

  let compare : t -> t -> int = Stdlib.compare
end

module Type = struct
  type t = string Gen_type.t

  let compare : t -> t -> int = Stdlib.compare
end

module Type_structure_map = Map.Make (Type_structure)

let assemble_record colon_or_eq (boxing : Boxing.t) fields vals =
  let hash = match boxing with Boxed -> "" | Unboxed -> "#" in
  let labeled_fields =
    List.map2 fields vals ~f:(fun (s, _) x -> s ^ " " ^ colon_or_eq ^ " " ^ x)
  in
  hash ^ "{ " ^ String.concat ~sep:"; " labeled_fields ^ " }"

let assemble_tuple ~sep (boxing : Boxing.t) xs =
  let hash = match boxing with Boxed -> "" | Unboxed -> "#" in
  sprintf "%s(%s)" hash (String.concat ~sep xs)

(* Code for this type expression (e.g. "int option * float") *)
let rec ty_code (t : Type.t) : string =
  match t with
  | Record { name; _ } -> name
  | Tuple (tys, boxing) ->
    assemble_tuple ~sep:" * " boxing (List.map tys ~f:ty_code)
  | Option t -> ty_code t ^ " option"
  | Int -> "int"
  | Int64 -> "int64"
  | Int64_u -> "int64#"
  | Int32 -> "int32"
  | Int32_u -> "int32#"
  | Nativeint -> "nativeint"
  | Nativeint_u -> "nativeint#"
  | Float -> "float"
  | Float_u -> "float#"
  | Float32 -> "float32"
  | Float32_u -> "float32#"
  | String -> "string"

module Type_naming : sig
  type t

  val empty : t

  val add_names : t -> Type_structure.t -> t * Type.t

  val decls_code : t -> string list
end = struct
  type t =
    { next_id : int;
      cache : Type.t Type_structure_map.t
    }

  let empty = { next_id = 0; cache = Type_structure_map.empty }

  let rec add_names t (ty_structure : Type_structure.t) : t * Type.t =
    match Type_structure_map.find_opt ty_structure t.cache with
    | Some ty -> t, ty
    | None -> (
      match ty_structure with
      | Record { name = (); fields; boxing } ->
        let tys = List.map ~f:snd fields in
        let t, tys =
          List.fold_left_mapi ~init:t tys ~f:(fun t i ty ->
              let t, ty = add_names t ty in
              t, ty
          )
        in
        let id = t.next_id in
        let t = { t with next_id = id + 1 } in
        let fields =
          List.mapi tys ~f:(fun i ty ->
              let field_name =
                sprintf "%c%d" (Char.unsafe_chr (Char.code 'a' + i)) id
              in
              field_name, ty
          )
        in
        let name = sprintf "t%d" id in
        let ty : Type.t = Record { name; fields; boxing } in
        { t with cache = Type_structure_map.add ty_structure ty t.cache }, ty
      | Tuple (tys, boxing) ->
        let t, tys =
          List.fold_left_mapi ~init:t tys ~f:(fun t i ty ->
              let t, ty = add_names t ty in
              t, ty
          )
        in
        t, Tuple (tys, boxing)
      | Option ty ->
        let t, ty = add_names t ty in
        t, Option ty
      | Int64 -> t, Int64
      | Int64_u -> t, Int64_u
      | Int32 -> t, Int32
      | Int32_u -> t, Int32_u
      | Nativeint -> t, Nativeint
      | Nativeint_u -> t, Nativeint_u
      | Float -> t, Float
      | Float_u -> t, Float_u
      | Float32 -> t, Float32
      | Float32_u -> t, Float32_u
      | Int -> t, Int
      | String -> t, String
    )

  let decls_code t =
    let decls =
      Type_structure_map.fold
        (fun (ty_structure : Type_structure.t) (ty : Type.t) acc ->
          let type_definition =
            match ty with
            | Record { name; fields; boxing } ->
              let tys = List.map ~f:snd fields in
              assemble_record ":" boxing fields (List.map tys ~f:ty_code)
            | _ -> assert false
          in
          let type_name = ty_code ty in
          let type_number =
            (* remove t *)
            assert (String.get type_name 0 = 't');
            String.sub type_name ~pos:1 ~len:(String.length type_name - 1)
            |> int_of_string
          in
          (type_number, sprintf "type %s = %s" type_name type_definition) :: acc
        )
        t.cache []
    in
    (* Sort by type number *)
    List.sort decls ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
    |> List.map ~f:snd
end

(* The number of subvalues of this type, e.g. [int option * #(float * float)]
   has three. *)
let rec num_subvals (t : _ Gen_type.t) : int =
  match t with
  | Record { fields; _ } ->
    List.fold_left fields ~f:(fun acc (_, t) -> acc + num_subvals t) ~init:0
  | Tuple (ts, _) ->
    List.fold_left ts ~f:(fun acc t -> acc + num_subvals t) ~init:0
  | Option t -> num_subvals t
  | Int | Int64 | Int64_u | Int32 | Int32_u | Nativeint | Nativeint_u | Float
  | Float_u | Float32 | Float32_u | String ->
    1

let assemble_record_expr boxing name fields vals =
  "(" ^ assemble_record "=" boxing fields vals ^ " : " ^ name ^ ")"

(* Given some integer seed, generate code for a value of this type. E.g. passing
   3 gives "(Some 3, 4.)" for [int option * float]. *)
let rec value_code (t : Type.t) (i : int) : string =
  match t with
  | Record { name; fields; boxing } ->
    let _, xs =
      List.fold_left_map fields ~init:i ~f:(fun acc (_, t) ->
          let x = value_code t acc in
          acc + num_subvals t, x
      )
    in
    assemble_record_expr boxing name fields xs
  | Tuple (tys, boxing) ->
    let _, xs =
      List.fold_left_map tys ~init:i ~f:(fun acc t ->
          let x = value_code t acc in
          acc + num_subvals t, x
      )
    in
    assemble_tuple ~sep:", " boxing xs
  | Option t -> if Int.equal i 0 then "None" else "Some " ^ value_code t i
  | Int -> Int.to_string i
  | Int64 -> Int.to_string i ^ "L"
  | Int64_u -> "#" ^ Int.to_string i ^ "L"
  | Int32 -> Int.to_string i ^ "l"
  | Int32_u -> "#" ^ Int.to_string i ^ "l"
  | Nativeint -> Int.to_string i ^ "n"
  | Nativeint_u -> "#" ^ Int.to_string i ^ "n"
  | Float -> Int.to_string i ^ "."
  | Float_u -> "#" ^ Int.to_string i ^ "."
  | Float32 -> Int.to_string i ^ ".s"
  | Float32_u -> "#" ^ Int.to_string i ^ ".s"
  | String -> "\"" ^ Int.to_string i ^ "\""

let rec mk_value_body_code (t : Type.t) i =
  match t with
  | Record { name; fields; boxing } ->
    let _, xs =
      List.fold_left_map fields ~init:i ~f:(fun acc (_, t) ->
          let x = mk_value_body_code t acc in
          acc + num_subvals t, x
      )
    in
    assemble_record_expr boxing name fields xs
  | Tuple (tys, boxing) ->
    let _, xs =
      List.fold_left_map tys ~init:i ~f:(fun acc t ->
          let x = mk_value_body_code t acc in
          acc + num_subvals t, x
      )
    in
    assemble_tuple ~sep:", " boxing xs
  | Option t ->
    sprintf "(if (i + %d) == 0 then None else Some (%s))" i
      (mk_value_body_code t i)
  | Int -> sprintf "(i + %d)" i
  | Int64 -> sprintf "Int64.of_int (i + %d)" i
  | Int64_u -> sprintf "Int64_u.of_int (i + %d)" i
  | Int32 -> sprintf "Int32.of_int (i + %d)" i
  | Int32_u -> sprintf "Int32_u.of_int (i + %d)" i
  | Nativeint -> sprintf "Nativeint.of_int (i + %d)" i
  | Nativeint_u -> sprintf "Nativeint_u.of_int (i + %d)" i
  | Float -> sprintf "Float.of_int (i + %d)" i
  | Float_u -> sprintf "Float_u.of_int (i + %d)" i
  | Float32 -> sprintf "Float32.of_int (i + %d)" i
  | Float32_u -> sprintf "Float32_u.of_int (i + %d)" i
  | String -> sprintf "String.of_int (i + %d)" i

(* Code that dynamically implements [value_code], creating a value from an
   integer seed bound to "i". We should be able to generate this code: "let
   mk_value (i : int) : $ty_code = $mk_value_code" *)
let mk_value_code (t : Type.t) : string = mk_value_body_code t 0

(* A function that implements equality in the generated code. We should be able
   generate this code: "let eq : $ty_code @ local -> $ty_code @ local -> bool =
   $eq_code" *)
let rec eq_code (t : Type.t) : string =
  match t with
  | Record { name; fields; boxing } ->
    let body =
      List.map fields ~f:(fun (s, t) -> sprintf "%s %s1 %s2" (eq_code t) s s)
      |> String.concat ~sep:" && "
    in
    let pat i =
      assemble_record_expr boxing name fields
        (List.map fields ~f:(fun (s, _) -> s ^ Int.to_string i))
    in
    sprintf "(fun %s %s -> %s)" (pat 1) (pat 2) body
  | Tuple (tys, boxing) ->
    let pat s =
      assemble_tuple ~sep:", " boxing
        (List.mapi tys ~f:(fun i _ -> s ^ Int.to_string i))
    in
    let body =
      List.mapi tys ~f:(fun i t -> sprintf "%s a%d b%d" (eq_code t) i i)
      |> String.concat ~sep:" && "
    in
    sprintf "(fun %s %s -> %s)" (pat "a") (pat "b") body
  | Option t ->
    sprintf
      "(fun a b -> match a, b with None,None -> true | Some a,Some b -> %s a \
       b|_->false)"
      (eq_code t)
  | Int -> sprintf "(fun a b -> Int.equal a b)"
  | Int64 -> sprintf "(fun a b -> Int64.equal (globalize a) (globalize b))"
  | Int64_u -> sprintf "(fun a b -> Int64_u.(equal (add #0L a) (add #0L b)))"
  | Int32 -> sprintf "(fun a b -> Int32.equal (globalize a) (globalize b))"
  | Int32_u -> sprintf "(fun a b -> Int32_u.(equal (add #0l a) (add #0l b)))"
  | Nativeint ->
    sprintf "(fun a b -> Nativeint.equal (globalize a) (globalize b))"
  | Nativeint_u ->
    sprintf "(fun a b -> Nativeint_u.(equal (add #0n a) (add #0n b)))"
  | Float -> sprintf "(fun a b -> Float.equal (globalize a) (globalize b))"
  | Float_u -> sprintf "(fun a b -> Float_u.(equal (add #0. a) (add #0. b)))"
  | Float32_u ->
    sprintf "(fun a b -> Float32_u.(equal (add #0.s a) (add #0.s b)))"
  | Float32 ->
    sprintf "(fun a b -> Float.equal (Float32.to_float a) (Float32.to_float b))"
  | String -> sprintf "(fun a b -> String.equal (globalize a) (globalize b))"

let rec kind_of (t : _ Gen_type.t) : Type_kind.t =
  match t with
  | Record { boxing = Unboxed; fields; _ } ->
    if List.length fields = 1
    then kind_of (List.hd fields |> snd)
    else Product (List.map fields ~f:(fun (_, t) -> kind_of t))
  | Record { boxing = Boxed; _ } -> Value Addr
  | Tuple (_, Boxed) -> Value Addr
  | Tuple (tys, Unboxed) -> Product (List.map tys ~f:kind_of)
  | Option t -> Value Addr
  | Int -> Value Immediate
  | Float -> Value Float
  | String | Int64 | Nativeint | Float32 | Int32 -> Value Addr
  | Int64_u | Int32_u | Nativeint_u | Float_u | Float32_u ->
    Non_value

let rec reverse_unboxed_paths (ty : Type.t) acc cur_path =
  match ty with
  | Record { name = _; fields; boxing = Unboxed } ->
    List.fold_left fields ~init:acc ~f:(fun acc (s, t) ->
        let cur_path = s :: cur_path in
        let acc = cur_path :: acc in
        reverse_unboxed_paths t acc cur_path
    )
  | _ -> acc

let unboxed_paths_by_depth ty : string list list Int_map.t =
  List.fold_left
    (reverse_unboxed_paths ty [] [])
    ~f:(fun acc rev_path ->
      let depth = List.length rev_path in
      let path = List.rev rev_path in
      let paths =
        match Int_map.find_opt depth acc with
        | Some paths -> path :: paths
        | None -> [path]
      in
      Int_map.add depth paths acc
    )
    ~init:Int_map.empty

type 'a tree =
  | Branch of 'a tree list
  | Leaf of 'a

let rec enumerate_forests num_nodes : unit tree list list =
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

and enumerate_trees num_nodes : unit tree list =
  assert (num_nodes >= 1);
  if Int.equal num_nodes 1
  then [Leaf ()]
  else
    List.map (enumerate_forests (num_nodes - 1)) ~f:(fun forest -> Branch forest)

let rec enumerate_tys_for_tree (tree : unit tree)
    (leaf_tys : Type_structure.t list) : Type_structure.t tree list =
  match tree with
  | Leaf () -> List.map leaf_tys ~f:(fun ty -> Leaf ty)
  | Branch forest ->
    List.map (enumerate_tys_for_forest forest leaf_tys) ~f:(fun forest ->
        Branch forest
    )

and enumerate_tys_for_forest (forest : unit tree list)
    (leaf_tys : Type_structure.t list) : Type_structure.t tree list list =
  match forest with
  | [] -> [[]]
  | tree :: forest ->
    let trees = enumerate_tys_for_tree tree leaf_tys in
    let forests = enumerate_tys_for_forest forest leaf_tys in
    List.concat_map trees ~f:(fun tree ->
        List.map forests ~f:(fun forest -> tree :: forest)
    )

  let rec tree_to_string tree =
    match tree with
    | Leaf () -> "()"
    | Branch forest ->
      sprintf "(%s)" (String.concat ~sep:"" (List.map forest ~f:tree_to_string))

(* let () =
  let trees = enumerate_trees 4 in
  print_endline (String.concat ~sep:"\n" (List.map trees ~f:tree_to_string));
  Printf.printf "%d\n" (List.length (enumerate_trees 4)) *)

let enumerate_ty_trees_up_to_size leaf_tys size : Type_structure.t tree list =
  let sizes = List.init ~f:(fun i -> i + 1) ~len:size in
  let trees = List.concat_map sizes ~f:(fun size -> enumerate_trees size) in
  List.concat_map trees ~f:(fun tree ->
    enumerate_tys_for_tree tree leaf_tys)

let rec ty_tree_to_unboxed_record (tree : Type_structure.t tree) :
    Type_structure.t =
  match tree with
  | Leaf ty -> ty
  | Branch trees ->
    let fields = List.map trees ~f:(fun tree -> (), ty_tree_to_unboxed_record tree) in
    Record { name = (); fields; boxing = Unboxed }

let ty_tree_to_nested_record (tree : Type_structure.t tree) : Type_structure.t option =
  match tree with
  | Leaf _ -> None
  | Branch trees ->
    let fields = List.map trees ~f:(fun tree -> (), ty_tree_to_unboxed_record tree) in
    Some (Record { name = (); fields; boxing = Boxed })

let rec all_scannable (kind : Type_kind.t) =
  match kind with
  | Value _ -> true
  | Non_value -> false
  | Product kinds -> List.for_all kinds ~f:all_scannable

let rec all_ignorable (kind : Type_kind.t) =
  match kind with
  | Value Immediate | Non_value -> true
  | Value Addr | Value Float -> false
  | Product kinds -> List.for_all kinds ~f:all_ignorable

type will_be_reordered_acc = { seen_flat : bool; last_value_after_flat : bool }
let will_be_reordered kind =
  let rec aux (kind : Type_kind.t) acc =
    match kind with
    | Value _ -> { acc with last_value_after_flat = acc.seen_flat }
    | Non_value -> { acc with seen_flat = true }
    | Product kinds -> List.fold_left kinds ~init:acc ~f:(fun acc kind -> aux kind acc)
  in
  (aux kind { seen_flat = false; last_value_after_flat = false }).last_value_after_flat

let ty_tree_to_array_element (tree : Type_structure.t tree) : Type_structure.t option =
  let ty = ty_tree_to_unboxed_record tree in
  let kind = kind_of ty in
  if kind = Value Float then None else
  (* CR layouts v8: once striped arrays are supported, remove the below filter *)
  if not (will_be_reordered kind) && (all_scannable kind || all_ignorable kind) then
    Some ty
  else
    None

let failwithf fmt = Printf.ksprintf failwith fmt

(* Type structures *)
let ty_ur1 : Type_structure.t =
  Record { name = (); fields = [(), Int64_u; (), Float_u]; boxing = Unboxed }

let ty_ur2 : Type_structure.t =
  Record { name = (); fields = [(), Int; (), Int64_u]; boxing = Unboxed }

let ty_ur3 : Type_structure.t =
  Record { name = (); fields = [(), Int64_u]; boxing = Unboxed }

let ty_ur4 : Type_structure.t =
  Record { name = (); fields = [(), ty_ur2; (), ty_ur3]; boxing = Unboxed }

let favorite_non_product_types : _ Gen_type.t list =
  [ Int; Int64; Float32_u; Float ]

let non_product_types : _ Gen_type.t list =
  favorite_non_product_types
  @ [Float_u; Int32_u; Int64_u; Nativeint_u; Float32; Int32; Nativeint; Int]

let interesting_type_trees : Type_structure.t tree list =
  enumerate_ty_trees_up_to_size favorite_non_product_types 4 @
  enumerate_ty_trees_up_to_size non_product_types 3

(* print interesting_type_trees *)
(* let () =
  List.iter interesting_type_trees ~f:(fun tree ->
    print_endline (Type_structure.to_string tree)
  ); *)

let interesting_array_element_types =
  List.filter_map interesting_type_trees ~f:ty_tree_to_array_element

let interesting_nested_records =
  List.filter_map interesting_type_trees ~f:ty_tree_to_nested_record

let array_element_types =
  interesting_array_element_types
  (* non_product_types
  @ [ ty_ur1;
      ty_ur3;
      ty_ur4;
      ty_ur2;
      Tuple ([Float_u; Int32_u; Int64_u], Unboxed);
      Tuple
        ( [ Float_u;
            Tuple ([Int64_u; Int64_u], Unboxed);
            Float32_u;
            Tuple ([Int32_u; Tuple ([Float32_u; Float_u], Unboxed)], Unboxed);
            Int64_u
          ],
          Unboxed
        );
      Tuple ([Int64_u; ty_ur1], Unboxed);
      Tuple ([Int; Int64], Unboxed);
      Tuple
        ([Option Int64; Int32; Tuple ([Int32; Float], Unboxed); Float], Unboxed);
      Tuple ([Float; Float; Float], Unboxed);
      Tuple
        ( [ Float;
            Tuple ([Float; Float], Unboxed);
            Tuple ([Float; Tuple ([Float; Float; Float], Unboxed)], Unboxed)
          ],
          Unboxed
        )
    ] *)

let (naming : Type_naming.t), (array_element_types : Type.t list) =
  List.fold_left_map array_element_types ~init:Type_naming.empty
    ~f:(fun naming ty -> Type_naming.add_names naming ty
  )

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

let indices_in_deepening_tests = [0; 1; 2; 100_000]

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
  let ty_array_s = ty_code ty ^ " array" in
  section ("  " ^ ty_code ty ^ "  ");
  line "let eq = %s in" (eq_code ty);
  line "let mk_value i = %s in" (mk_value_code ty);
  line "(* 1. Create an array of size [size] *)";
  line "let a : %s = %s size %s in" ty_array_s makearray_dynamic
    (value_code ty 0);
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
  let unboxed_paths_by_depth = unboxed_paths_by_depth ty in
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      Int_map.iter
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
                (up_concat unboxed_path) (ty_code ty) (up_concat unboxed_path);
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
    unboxed_paths_by_depth ty |> Int_map.add 0 [[]]
  in
  let debug_exprs = [] in
  let ty_array_s = ty_code ty ^ " array" in
  section ("  " ^ ty_code ty ^ "  ");
  let up_concat l = String.concat (List.map l ~f:(fun s -> ".#" ^ s)) ~sep:"" in
  Int_map.iter
    (fun depth unboxed_paths ->
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
  List.iter (Type_naming.decls_code naming) ~f:(fun s -> line "%s" s);
  line "";
  line "(* Catch metaprogramming errors early *)";
  toplevel_unit_block (fun () ->
      line "(* Check types and constants *)";
      List.iter array_element_types ~f:(fun ty ->
          line "let _ : %s = %s in" (ty_code ty) (value_code ty 0)
      );
      line "(* Check equality and mk_value functions *)";
      List.iter array_element_types ~f:(fun ty ->
          line "let eq : %s @ local -> %s @ local -> bool = %s in" (ty_code ty)
            (ty_code ty) (eq_code ty);
          line "let mk_value i = %s in" (mk_value_code ty);
          seq_assert ~debug_exprs
            (sprintf "eq (mk_value 1) %s" (value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "eq %s %s" (value_code ty 1) (value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "not (eq %s %s)" (value_code ty 1) (value_code ty 2))
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
