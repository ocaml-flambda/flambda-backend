(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Path
open Types
open Asttypes
open Typedtree
open Lambda

(* Expand a type, looking through ordinary synonyms, private synonyms,
   links, and [@@unboxed] types. The returned type will be therefore be none
   of these cases. *)
let scrape_ty env ty =
  let ty =
    match get_desc ty with
    | Tpoly(ty, _) -> ty
    | _ -> ty
  in
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.expand_head_opt env (Ctype.correct_levels ty) in
      begin match get_desc ty with
      | Tconstr (p, _, _) ->
          begin match find_unboxed_type (Env.find_type p env) with
          | Some _ -> Ctype.get_unboxed_type_approximation env ty
          | None -> ty
          | exception Not_found -> ty
          end
      | _ ->
          ty
      end
  | _ -> ty

(* See [scrape_ty]; this returns the [type_desc] of a scraped [type_expr]. *)
let scrape env ty =
  get_desc (scrape_ty env ty)

let scrape_poly env ty =
  let ty = scrape_ty env ty in
  match get_desc ty with
  | Tpoly (ty, _) -> get_desc ty
  | d -> d

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (_, lhs, rhs, _) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr(p, _, _) -> Path.same p base_ty_path
  | _ -> false

let is_always_gc_ignorable env ty =
  let imm : Type_immediacy.t =
    (* We check that we're compiling to (64-bit) native code before counting
       immediate64 types as gc_ignorable, because bytecode is intended to be
       platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64
    then Always_on_64bits
    else Always
  in
  Result.is_ok (Ctype.check_type_immediate env ty imm)

let maybe_pointer_type env ty =
  let ty = scrape_ty env ty in
  if is_always_gc_ignorable env ty then Immediate else Pointer

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

type classification =
  | Int   (* any immediate type *)
  | Float
  | Lazy
  | Addr  (* anything except a float or a lazy *)
  | Any

(* Classify a ty into a [classification]. Looks through synonyms, using [scrape_ty].
   Returning [Any] is safe, though may skip some optimizations. *)
let classify env ty : classification =
  let ty = scrape_ty env ty in
  if is_always_gc_ignorable env ty then Int
  else match get_desc ty with
  | Tvar _ | Tunivar _ ->
      Any
  | Tconstr (p, _args, _abbrev) ->
      if Path.same p Predef.path_float then Float
      else if Path.same p Predef.path_lazy_t then Lazy
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_bytes
           || Path.same p Predef.path_array
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64 then Addr
      else begin
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract _ ->
              Any
          | Type_record _ | Type_variant _ | Type_open ->
              Addr
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any
      end
  | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ ->
      Addr
  | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ ->
      assert false

let array_type_kind env ty =
  match scrape_poly env ty with
  | Tconstr(p, [elt_ty], _)
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
      begin match classify env elt_ty with
      | Any -> if Config.flat_float_array then Pgenarray else Paddrarray
      | Float -> if Config.flat_float_array then Pfloatarray else Paddrarray
      | Addr | Lazy -> Paddrarray
      | Int -> Pintarray
      end
  | Tconstr(p, [], _) when Path.same p Predef.path_floatarray ->
      Pfloatarray
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_type_kind exp.exp_env exp.exp_type

let array_pattern_kind pat = array_type_kind pat.pat_env pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name), [], _)
    when Ident.name mod_id = "Stdlib__Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_type_kind_and_layout env typ =
  match scrape env typ with
  | Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev) ->
      (bigarray_decode_type env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type env layout_type layout_table
                            Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)

let rec layout env ty =
  let rec loop env ~visited ~depth ~num_nodes_visited ty
      : int * Lambda.layout =
    let[@inline] cannot_proceed () =
      Numbers.Int.Set.mem (get_id ty) visited
        || depth >= 2
        || num_nodes_visited >= 30
    in
    let scty = scrape_ty env ty in
    match get_desc scty with
    (* | _ when is_immediate (Ctype.immediacy env scty) -> *)
    | _ when is_always_gc_ignorable env scty ->
      num_nodes_visited, Pvalue Pintval
    | Tconstr(p, _, _) when Path.same p Predef.path_int ->
      num_nodes_visited, Pvalue Pintval
    | Tconstr(p, _, _) when Path.same p Predef.path_char ->
      num_nodes_visited, Pvalue Pintval
    | Tconstr(p, _, _) when Path.same p Predef.path_float ->
      num_nodes_visited, Pvalue Pfloatval
    | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
      num_nodes_visited, Pvalue (Pboxedintval Pint32)
    | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
      num_nodes_visited, Pvalue (Pboxedintval Pint64)
    | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
      num_nodes_visited, Pvalue (Pboxedintval Pnativeint)
    | Tconstr(p, args, _) when Path.same p Predef.path_unboxed_pair ->
      let layouts = List.map (layout env) args in
      num_nodes_visited, Punboxed_product layouts
    | Tconstr(p, _, _)
        when (Path.same p Predef.path_array
              || Path.same p Predef.path_floatarray) ->
      num_nodes_visited, Pvalue (Parrayval (array_type_kind env ty))
    | Tconstr(p, _, _) ->
      if cannot_proceed () then
        num_nodes_visited, Pvalue Pgenval
      else begin
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        match (Env.find_type p env).type_kind with
        | exception Not_found ->
          num_nodes_visited, Pvalue Pgenval
        | Type_variant (constructors, _rep) ->
          let is_constant (constructor : Types.constructor_declaration) =
            match constructor.cd_args with
            | Cstr_tuple [] -> true
            | _ -> false
          in
          if List.for_all is_constant constructors then
            num_nodes_visited, Pvalue Pintval
          else
            let depth = depth + 1 in
            let for_one_constructor
                  (constructor : Types.constructor_declaration)
                  ~depth ~num_nodes_visited =
              let num_nodes_visited = num_nodes_visited + 1 in
              match constructor.cd_args with
              | Cstr_tuple fields ->
                let num_nodes_visited, fields =
                  List.fold_left_map
                    (fun num_nodes_visited (ty, _) ->
                      let num_nodes_visited = num_nodes_visited + 1 in
                      loop env ~visited ~depth ~num_nodes_visited ty)
                    num_nodes_visited fields
                in
                (false, num_nodes_visited), fields
              | Cstr_record labels ->
                List.fold_left_map
                  (fun (is_mutable, num_nodes_visited)
                       (label:Types.label_declaration) ->
                      let is_mutable =
                        match label.ld_mutable with
                        | Mutable -> true
                        | Immutable -> is_mutable
                      in
                      let num_nodes_visited = num_nodes_visited + 1 in
                      let num_nodes_visited, field =
                        loop env ~visited ~depth ~num_nodes_visited
                          label.ld_type
                      in
                      (is_mutable, num_nodes_visited), field)
                  (false, num_nodes_visited) labels
            in
            let result =
              List.fold_left (fun result constructor ->
                  match result with
                  | None -> None
                  | Some (num_nodes_visited,
                          next_const, consts, next_tag, non_consts) ->
                    let (is_mutable, num_nodes_visited), fields =
                      for_one_constructor constructor ~depth ~num_nodes_visited
                    in
                    if is_mutable then None
                    else if List.compare_length_with fields 0 = 0 then
                      let consts = next_const :: consts in
                      Some (num_nodes_visited,
                        next_const + 1, consts, next_tag, non_consts)
                    else
                      let non_consts = (next_tag, fields) :: non_consts in
                      Some (num_nodes_visited,
                        next_const, consts, next_tag + 1, non_consts))
                (Some (num_nodes_visited, 0, [], 0, []))
                constructors
            in
            begin match result with
            | None -> num_nodes_visited, Pvalue Pgenval
            | Some (num_nodes_visited, _, consts, _, non_consts) ->
              match non_consts with
              | [] -> assert false  (* See [List.for_all is_constant], above *)
              | _::_ ->
                let non_consts =
                  List.map (fun (l, layouts) ->
                    let value_kinds =
                      List.map (function (Pvalue vk) -> vk | _ -> assert false) layouts
                    in l, value_kinds) non_consts
                in
                num_nodes_visited, Pvalue (Pvariant { consts; non_consts })
            end
        | Type_record (labels, record_representation) ->
          let depth = depth + 1 in
          let (is_mutable, num_nodes_visited), fields =
            List.fold_left_map
              (fun (is_mutable, num_nodes_visited)
                   (label:Types.label_declaration) ->
                 let is_mutable =
                   match label.ld_mutable with
                   | Mutable -> true
                   | Immutable -> is_mutable
                 in
                 let num_nodes_visited = num_nodes_visited + 1 in
                 let num_nodes_visited, field =
                   loop env ~visited ~depth ~num_nodes_visited label.ld_type
                  in
                 (is_mutable, num_nodes_visited), field)
              (false, num_nodes_visited) labels
          in
          if is_mutable then
            num_nodes_visited, Pvalue Pgenval
          else begin match record_representation with
            | Record_regular ->
              let fields =
                List.map (function (Pvalue vk) -> vk | _ -> assert false) fields
              in
              num_nodes_visited,
                Pvalue (Pvariant { consts = []; non_consts = [0, fields] })
            | Record_float ->
              num_nodes_visited,
                Pvalue (Pvariant {
                  consts = [];
                  non_consts = [
                    Obj.double_array_tag,
                    List.map (fun _ -> Pfloatval) fields
                  ] })
            | Record_inlined tag ->
              let fields =
                List.map (function (Pvalue vk) -> vk | _ -> assert false) fields
              in
              num_nodes_visited,
                Pvalue (Pvariant { consts = []; non_consts = [tag, fields] })
            | Record_unboxed _ ->
              begin match fields with
              | [field] -> num_nodes_visited, field
              | [] | _::_ ->
                Misc.fatal_error "Records that are [Record_unboxed] should \
                  have exactly one field"
              end
            | Record_extension _ ->
              num_nodes_visited, Pvalue Pgenval
          end
        | Type_abstract _ | Type_open -> num_nodes_visited, Pvalue Pgenval
      end
    | Ttuple fields ->
      if cannot_proceed () then
        num_nodes_visited, Pvalue Pgenval
      else begin
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        let depth = depth + 1 in
        let num_nodes_visited, fields =
          List.fold_left_map (fun num_nodes_visited field ->
              let num_nodes_visited = num_nodes_visited + 1 in
              loop env ~visited ~depth ~num_nodes_visited field)
            num_nodes_visited
            fields
        in
        let fields =
          List.map (function (Pvalue vk) -> vk | _ -> assert false) fields
        in
        num_nodes_visited,
          Pvalue (Pvariant { consts = []; non_consts = [0, fields] })
      end
    | _ ->
      num_nodes_visited, Pvalue Pgenval
  in
  let _num_nodes_visited, value_kind =
    loop env ~visited:Numbers.Int.Set.empty ~depth:0
      ~num_nodes_visited:0 ty
  in
  value_kind

let function_return_layout env ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> layout env rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function2_return_layout env ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> function_return_layout env rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"


(** Whether a forward block is needed for a lazy thunk on a value, i.e.
    if the value can be represented as a float/forward/lazy *)
let lazy_val_requires_forward env ty =
  match classify env ty with
  | Any | Lazy -> true
  | Float -> Config.flat_float_array
  | Addr | Int -> false

(** The compilation of the expression [lazy e] depends on the form of e:
    constants, floats and identifiers are optimized.  The optimization must be
    taken into account when determining whether a recursive binding is safe. *)
let classify_lazy_argument : Typedtree.expression ->
                             [`Constant_or_function
                             |`Float_that_cannot_be_shortcut
                             |`Identifier of [`Forward_value|`Other]
                             |`Other] =
  fun e -> match e.exp_desc with
    | Texp_constant
        ( Const_int _ | Const_char _ | Const_string _
        | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
    | Texp_function _
    | Texp_construct (_, {cstr_arity = 0}, _, _) ->
       `Constant_or_function
    | Texp_constant(Const_float _) ->
       if Config.flat_float_array
       then `Float_that_cannot_be_shortcut
       else `Constant_or_function
    | Texp_ident _ when lazy_val_requires_forward e.exp_env e.exp_type ->
       `Identifier `Forward_value
    | Texp_ident _ ->
       `Identifier `Other
    | _ ->
       `Other

let value_kind_union (k1 : Lambda.value_kind) (k2 : Lambda.value_kind) =
  if Lambda.equal_value_kind k1 k2 then k1
  else Pgenval

let rec layout_union l1 l2 =
  match l1, l2 with
  | Pbottom, l
  | l, Pbottom -> l
  | Pvalue layout1, Pvalue layout2 ->
      Pvalue (value_kind_union layout1 layout2)
  | Punboxed_float, Punboxed_float -> Punboxed_float
  | Punboxed_int bi1, Punboxed_int bi2 ->
      if equal_boxed_integer bi1 bi2 then l1 else Ptop
  | Punboxed_product layouts1, Punboxed_product layouts2 ->
      if List.compare_lengths layouts1 layouts2 <> 0 then Ptop
      else Punboxed_product (List.map2 layout_union layouts1 layouts2)
  | (Ptop | Pvalue _ | Punboxed_float | Punboxed_int _ | Punboxed_product _),
    _ ->
      Ptop
