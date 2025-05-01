(* This file is used in [run_array_idx_tests.ml]. *)
open Stdlib_upstream_compatible
open Stdlib_stable
module List = ListLabels
module String = StringLabels

let failwithf fmt = Printf.ksprintf failwith fmt

let sprintf = Printf.sprintf

(* See [test_makearray_dynamic] for the main testing steps! *)

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
  val num_subvalues : t -> int

  (** Code that dynamically implements [value_code], creating a value from an
       integer seed bound to "i".
       We should be able to generate this code:
       "let mk_value (i : int) : $ty_code = $mk_value_code" *)
  val mk_value_code : t -> string

  (** A function that implements equality in the generated code.
       We should be able generate this code:
       "let eq : $ty_code @ local -> $ty_code @ local -> bool = $eq" *)
  val eq : t -> string

  (* Generate typedecls for user-defined nominal types that have been created *)
  val decls_code : unit -> string list

  (* Takes the record name and (label_name, label_type) pairs *)
  val unboxed_record : string -> (string * t) list -> t

  (* [enum 3] represents [type enum3 = A3_0 | A3_1 | A3_2]. *)
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
  type t =
    | Unboxed_record of
        { name : string;
          fields : (string * t) list
        }
    | Unboxed_tuple of t list
    | Non_product of
        { ty_code : string;
          value_code : int -> string;
          mk_value_code : string;
          eq : string
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

  let rec value_code t i =
    match t with
    | Unboxed_record { name; fields } ->
      assemble_unboxed_record_expr name fields
        (List.map fields ~f:(fun (_, t) -> value_code t i))
    | Unboxed_tuple ts ->
      assemble_unboxed_tuple ~sep:", "
        (List.map ts ~f:(fun t -> value_code t i))
    | Non_product { value_code; _ } -> value_code i

  let rec mk_value_code t : string =
    match t with
    | Unboxed_record { name; fields } ->
      assemble_unboxed_record_expr name fields
        (List.map fields ~f:(fun (_, t) -> mk_value_code t))
    | Unboxed_tuple ts ->
      assemble_unboxed_tuple ~sep:", " (List.map ts ~f:mk_value_code)
    | Non_product { mk_value_code; _ } -> mk_value_code

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

  let rec num_subvalues t =
    match t with
    | Unboxed_record { name = _; fields } ->
      List.fold_left fields ~f:(fun acc (_, t) -> acc + num_subvalues t) ~init:0
    | Unboxed_tuple ts ->
      List.fold_left ts ~f:(fun acc t -> acc + num_subvalues t) ~init:0
    | Non_product { eq = _; _ } -> 1

  (* If (name, decl) is in this list, we'll generate "type $name = $decl" *)
  let decls : (string * string) list ref = ref []

  let decls_code () =
    (* [!decls] is only reversed for aesthetic reasons. *)
    List.mapi (List.rev !decls) ~f:(fun i (name, def) ->
        (if i == 0 then "type " else "and ") ^ name ^ " = " ^ def)

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
    let mk_value_code =
      let brs =
        List.init ~len:size ~f:(fun i -> sprintf "%d -> %s" i (ith_ctor i))
        @ ["_ -> assert false"]
      in
      sprintf "(match Int.rem i %d with %s)" size (String.concat ~sep:" | " brs)
    in
    let name = sprintf "enum%d" size in
    add_decl ~name ~def;
    Non_product
      { ty_code = name;
        value_code = (fun i -> ith_ctor (Int.rem i size));
        mk_value_code;
        eq
      }

  let option t =
    Non_product
      { ty_code = ty_code t ^ " option";
        value_code =
          (fun i -> if i == 0 then "None" else "Some " ^ value_code t i);
        mk_value_code =
          "(if i == 0 then None else Some (" ^ mk_value_code t ^ "))";
        eq =
          "(fun a b -> match a, b with None,None -> true | Some a,Some b -> "
          ^ eq t ^ " a b|_->false)"
      }

  let unboxed_tuple ts = Unboxed_tuple ts

  let int =
    Non_product
      { ty_code = "int";
        value_code = Int.to_string;
        mk_value_code = "i";
        eq = "(fun a b -> Int.equal a b)"
      }

  let float =
    Non_product
      { ty_code = "float";
        value_code = (fun i -> Int.to_string i ^ ".");
        mk_value_code = "Float.of_int i";
        eq = "(fun a b -> Float.equal (globalize a) (globalize b))"
      }

  let float_u =
    Non_product
      { ty_code = "float#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ ".");
        mk_value_code = "Float_u.of_int i";
        eq = "(fun a b -> Float_u.(equal (add #0. a) (add #0. b)))"
      }

  let float32 =
    Non_product
      { ty_code = "float32";
        value_code = (fun i -> Int.to_string i ^ ".s");
        mk_value_code = "Float32.of_int i";
        eq =
          "(fun a b -> Float.equal (Float32.to_float a) (Float32.to_float b))"
      }

  let float32_u =
    Non_product
      { ty_code = "float32#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ ".s");
        mk_value_code = "Float32_u.of_int i";
        eq = "(fun a b -> Float32_u.(equal (add #0.s a) (add #0.s b)))"
      }

  let int32 =
    Non_product
      { ty_code = "int32";
        value_code = (fun i -> Int.to_string i ^ "l");
        mk_value_code = "Int32.of_int i";
        eq = "(fun a b -> Int32.equal (globalize a) (globalize b))"
      }

  let int32_u =
    Non_product
      { ty_code = "int32#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "l");
        mk_value_code = "Int32_u.of_int i";
        eq = "(fun a b -> Int32_u.(equal (add #0l a) (add #0l b)))"
      }

  let int64 =
    Non_product
      { ty_code = "int64";
        value_code = (fun i -> Int.to_string i ^ "L");
        mk_value_code = "Int64.of_int i";
        eq = "(fun a b -> Int64.equal (globalize a) (globalize b))"
      }

  let int64_u =
    Non_product
      { ty_code = "int64#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "L");
        mk_value_code = "Int64_u.of_int i";
        eq = "(fun a b -> Int64_u.(equal (add #0L a) (add #0L b)))"
      }

  let nativeint =
    Non_product
      { ty_code = "nativeint";
        value_code = (fun i -> Int.to_string i ^ "n");
        mk_value_code = "Nativeint.of_int i";
        eq = "(fun a b -> Nativeint.equal (globalize a) (globalize b))"
      }

  let nativeint_u =
    Non_product
      { ty_code = "nativeint#";
        value_code = (fun i -> "#" ^ Int.to_string i ^ "n");
        mk_value_code = "Nativeint_u.of_int i";
        eq = "(fun a b -> Nativeint_u.(equal (add #0n a) (add #0n b)))"
      }
end

let ty_ur1 = Ty.(unboxed_record "ur1" ["a", int64_u; "b", float_u])

let ty_ur2 = Ty.(unboxed_record "ur2" ["a", int; "b", int64_u])

let ty_ur3 = Ty.(unboxed_record "ur3" ["a", int64_u])

let ty_ur4 = Ty.(unboxed_record "ur4" ["a", ty_ur2; "b", ty_ur3])

let types =
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
          int64_u ];
      unboxed_tuple [int64_u; ty_ur1];
      unboxed_tuple [int; int64];
      unboxed_tuple [option int64; int32; unboxed_tuple [int32; float]; float];
      unboxed_tuple [float; float; float];
      unboxed_tuple
        [ float;
          unboxed_tuple [float; float];
          unboxed_tuple [float; unboxed_tuple [float; float; float]] ] ]

let preamble =
  {|
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
      flush stdout)
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
      f ~debug_exprs);
  line "done;"

let for_i_below_size = for_ "i" ~from:"0" ~to_:"size - 1"

(* Iterate through a list of ints *)
let iter l var ~debug_exprs f =
  line "iter (%s) ~f:(fun %s ->" l var;
  with_indent (fun () ->
      let debug_exprs = { expr = var; format_s = "%d" } :: debug_exprs in
      f ~debug_exprs);
  line ") [@nontail];"

let section s =
  let s_as_stars = String.init (String.length s) ~f:(fun _ -> '*') in
  line "(**%s**)" s_as_stars;
  line "(* %s *)" s;
  line "(**%s**)" s_as_stars

(* Test steps *)
let test_makearray_dynamic ~local ty =
  let makearray_dynamic = makearray_dynamic_fn ~local in
  let debug_exprs = [{ expr = "size"; format_s = "%d" }] in
  let ty_array_s = Ty.ty_code ty ^ " array" in
  (* seq_print_in_test ty.Ty.ty_code; *)
  section ("  " ^ Ty.ty_code ty ^ "  ");
  line "let eq = %s in" (Ty.eq ty);
  line "let mk_value i = %s in" (Ty.mk_value_code ty);
  line "(* 1. Create an array of size [size] *)";
  line "let a : %s = %s size %s in" ty_array_s makearray_dynamic
    (Ty.value_code ty 0);
  line "(* 3. Fill [a] with distinct values using block indices *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      line "set_idx_mut a (.(i)) (mk_value i);");
  line "Gc.compact ();";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      seq_assert ~debug_exprs "eq (get a i) (mk_value i)");
  line "(* Also read back those values with block indices *)";
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      seq_assert ~debug_exprs "eq (get_idx_mut a (.(i))) (mk_value i)");
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      line "set_idx_mut a (.(i)) (mk_value i);");
  for_i_below_size ~debug_exprs (fun ~debug_exprs ->
      seq_assert ~debug_exprs "eq (get a i) (mk_value i)");
  line "Gc.compact ();";
  print_endline ""

let toplevel_unit_block f =
  assert (Int.equal !indent 0);
  line "let () =";
  with_indent (fun () ->
      f ();
      line "()");
  line ";;";
  line ""

let main ~bytecode =
  let debug_exprs = [] in
  line {|(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;|};
  if bytecode
  then (
    line {| bytecode;|};
    line {| flags = "-extension layouts_alpha";|})
  else (
    line {| modules = "stubs.c";|};
    line {| flags = "-extension simd_beta -extension layouts_alpha";|};
    line {| flambda2;|};
    line {| stack-allocation;|};
    line {| native;|});
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
      List.iter types ~f:(fun ty ->
          line "let _ : %s = %s in" (Ty.ty_code ty) (Ty.value_code ty 0));
      line "(* Check equality and mk_value functions *)";
      List.iter types ~f:(fun ty ->
          line "let eq : %s @ local -> %s @ local -> bool = %s in"
            (Ty.ty_code ty) (Ty.ty_code ty) (Ty.eq ty);
          line "let mk_value i = %s in" (Ty.mk_value_code ty);
          seq_assert ~debug_exprs
            (sprintf "eq (mk_value 1) %s" (Ty.value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "eq %s %s" (Ty.value_code ty 1) (Ty.value_code ty 1));
          seq_assert ~debug_exprs
            (sprintf "not (eq %s %s)" (Ty.value_code ty 1) (Ty.value_code ty 2))));
  List.iter [false; true] ~f:(fun local ->
      line "let test_%s size =" (makearray_dynamic_fn ~local);
      with_indent (fun () ->
          List.iter types ~f:(test_makearray_dynamic ~local);
          line "()");
      line "");
  line "(* Main tests *)";
  toplevel_unit_block (fun () ->
      List.iter [false; true] ~f:(fun local ->
          let test_fn = "test_" ^ makearray_dynamic_fn ~local in
          seq_print_in_test test_fn;
          line "iter sizes ~f:%s;" test_fn));
  line "for i = 1 to %d do" !test_id;
  with_indent (fun () ->
      line
        {|if not (List.mem i !tests_run) then failwithf "test %%d not run" i|});
  line "done;;";
  print_in_test "All tests passed."

let () =
  let bytecode =
    match Sys.argv with
    | [| _; "native" |] -> false
    | [| _; "byte" |] -> true
    | _ -> failwith (sprintf "Usage %s <byte|native>" Sys.argv.(0))
  in
  main ~bytecode
