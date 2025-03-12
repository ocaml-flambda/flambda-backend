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


open Types
open Shapes
open Typedtree


(* Helper utility for debugging. *)
let _pp_type fmt ty =
  match get_desc ty with
  | Tconstr (p, _, _) -> Format.fprintf fmt "constr(%a)" Path.print p
  | Tvariant _ -> Format.fprintf fmt "variant"
  | Tvar { name = None; _ } -> Format.fprintf fmt "var(_)"
  | Tvar { name = Some v; _ } -> Format.fprintf fmt "var(%s)" v
  | _ -> Format.fprintf fmt "other type"
;;

module Subst = Map.Make (struct
    type t = Int.t

    let compare = Int.compare
  end)

let identity_subst = Subst.empty
let add_subst x a s = Subst.add x a s
let lookup_subst x s = Subst.find_opt x s

(* Truncating update, if one list is longer than the other, it throws it away.
   The update assumes the keys are not overlapping (since it adds from left to right). *)
let rec batch_add_subst args vals subst =
  match args, vals with
  | a :: args', v :: vals' -> batch_add_subst args' vals' (add_subst a v subst)
  | _, _ -> subst
;;

(* NOTE: The following is adapted from the `typeopt.ml` in the compiler. *)

(* Expand a type, looking through ordinary synonyms, private synonyms,
   links, and [@@unboxed] types. The returned type will therefore be none
   of these cases (except in case of missing cmis). *)
let scrape_ty env ty =
  let ty =
    match get_desc ty with
    | Tpoly (ty, _) -> ty
    | _ -> ty
  in
  match get_desc ty with
  | Tconstr _ ->
    let ty = Ctype.expand_head_opt env (Ctype.correct_levels ty) in
    (match get_desc ty with
     | Tconstr (p, _, _) ->
       (match find_unboxed_type (Env.find_type p env) with
        | Some _ -> Ctype.get_unboxed_type_approximation env ty
        | None -> ty
        | exception Not_found -> ty)
     | _ -> ty)
  | _ -> ty
;;

let scrape_poly env ty =
  let ty = scrape_ty env ty in
  match get_desc ty with
  | Tpoly (ty, _) -> get_desc ty
  | d -> d
;;

(* See [scrape_ty]; this returns the [type_desc] of a scraped [type_expr]. *)
let is_always_gc_ignorable env ty =
  let ext : Jkind_axis.Externality.t =
    (* We check that we're compiling to (64-bit) native code before counting
       External64 types as gc_ignorable, because bytecode is intended to be
       platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64 then External64 else External
  in
  Ctype.check_type_externality env ty ext
;;

type classification =
  | Int (* any immediate type *)
  | Float
  | Lazy
  | Addr (* anything except a float or a lazy *)
  | Any

(* Classify a ty into a [classification]. Looks through synonyms, using [scrape_ty].
   Returning [Any] is safe, though may skip some optimizations. *)
let classify env ty : classification =
  (* NOTE: this call is redundant, but also does not hurt.
     It is inherited from the original definition. *)
  let ty = scrape_ty env ty in
  if is_always_gc_ignorable env ty
  then Int
  else (
    match get_desc ty with
    | Tvar _ | Tunivar _ -> Any
    | Tconstr (p, _args, _abbrev) ->
      if Path.same p Predef.path_float
      then Float
      else if Path.same p Predef.path_lazy_t
      then Lazy
      else if Path.same p Predef.path_string
              || Path.same p Predef.path_bytes
              || Path.same p Predef.path_array
              || Path.same p Predef.path_nativeint
              || Path.same p Predef.path_int32
              || Path.same p Predef.path_int64
      then Addr
      else (
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract _ -> Any
          | Type_record _ | Type_variant _ | Type_open -> Addr
          | Type_record_unboxed_product _ -> Any
        with
        | Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any)
    | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ -> Addr
    | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ | Tunboxed_tuple _ -> assert false)
;;

type can_be_float_array =
  | YesFloatArray
  | NoFloatArray
  | MaybeFloatArray

let array_type_kind env ty =
  match scrape_poly env ty with
  | Tconstr (p, [ elt_ty ], _)
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
    (match classify env elt_ty with
     | Any -> if Config.flat_float_array then MaybeFloatArray else NoFloatArray
     | Float -> if Config.flat_float_array then YesFloatArray else NoFloatArray
     | Addr | Lazy -> NoFloatArray
     | Int -> NoFloatArray)
  | Tconstr (p, [], _) when Path.same p Predef.path_floatarray -> YesFloatArray
  | _ ->
    (* This can happen with e.g. Obj.field *)
    MaybeFloatArray
;;

(* Invariant:
   [value_kind] functions may only be called on types with layout  value. *)
let rec value_kind env (subst : type_shape Subst.t) ~visited ~depth ty : type_shape =
  let[@inline] cannot_proceed () =
    Numbers.Int.Set.mem (get_id ty) visited || depth >= 2
  in
  let scty = scrape_ty env ty in
  match get_desc scty with
  | Tconstr (p, _, _) when Path.same p Predef.path_int -> Imm
  | Tconstr (p, _, _) when Path.same p Predef.path_char -> Imm
  | Tconstr (p, _, _) when Path.same p Predef.path_unit -> Imm
  | Tconstr (p, _, _) when Path.same p Predef.path_bool -> Imm
  | Tconstr (p, _, _) when Path.same p Predef.path_float -> Double
  (* NOTE: we can add a primitive for lists *)
  | Tconstr (p, _, _) when Path.same p Predef.path_int32 -> Int32
  | Tconstr (p, _, _) when Path.same p Predef.path_int64 -> Int64
  | Tconstr (p, _, _) when Path.same p Predef.path_nativeint -> Nativeint
  | Tconstr (p, _, _) when Path.same p Predef.path_string -> String
  | Tconstr (p, _, _) when Path.same p Predef.path_bytes -> String
  | Tconstr (p, _, _) when Path.same p Predef.path_exn -> Or (Imm, Block None)
  | Tconstr (p, _, _) when Path.same p Predef.path_floatarray -> FloatArray
  | Tconstr (p, [ ty' ], _)
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
    if cannot_proceed ()
    then Array Any
    else (
      let visited = Numbers.Int.Set.add (get_id ty) visited in
      let depth = depth + 1 in
      let sh = value_kind env subst ~visited ~depth ty' in
      match array_type_kind env ty with
      | YesFloatArray -> FloatArray
      | MaybeFloatArray -> Or (FloatArray, Array sh)
      | NoFloatArray -> Array sh)
  | Tconstr (p, args, _) ->
    if cannot_proceed ()
    then Any
    else (
      match Env.find_type p env with
      | exception Not_found -> Any
      | decl ->
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        let arg_shapes =
          List.map (fun a -> value_kind env subst ~visited ~depth:(depth + 1) a) args
        in
        let subst = batch_add_subst (List.map get_id decl.type_params) arg_shapes subst in
        (match decl.type_kind with
         | Type_variant (cstrs, rep, _) ->
           value_kind_variant env subst ~visited ~depth cstrs rep
         | Type_record (labels, rep, _) ->
           let depth = depth + 1 in
           value_kind_record env subst ~visited ~depth labels rep
         | Type_record_unboxed_product ([ { ld_type; _ } ], Record_unboxed_product, _) ->
           let depth = depth + 1 in
           value_kind env subst ~visited ~depth ld_type
         | Type_record_unboxed_product (([] | _ :: _ :: _), Record_unboxed_product, _) ->
           Misc.fatal_error
             "Traverse_typed_tree.value_kind: non-unary unboxed record can't have kind \
              value"
         | Type_abstract _ -> Any
         | Type_open ->
           (* open types are variants so should always
              be either a block or an immediate.
           *)
           Or (Imm, Block None)))
  | Ttuple fields ->
    if cannot_proceed ()
    then Block None
    else (
      let visited = Numbers.Int.Set.add (get_id ty) visited in
      let depth = depth + 1 in
      let fields =
        List.map (fun (_, field) -> value_kind env subst ~visited ~depth field) fields
      in
      Block (Some (0, fields)))
  | Tvariant row ->
    (* FIXME: we could do something better here *)
    if Ctype.tvariant_not_immediate row then Or (Imm, Block None) else Imm
  | Tarrow _ -> Closure
  | Tobject _ -> Obj
  | Tvar _ | Tunivar _ ->
    if cannot_proceed ()
    then Any
    else (
      match lookup_subst (get_id ty) subst with
      | None -> Any
      | Some sh -> sh)
  | Tpoly _ -> assert false (* handled by [scrape_ty] currently *)
  | Tfield _ | Tnil | Tlink _ | Tsubst _ -> assert false
  (* NOTE: we should never encounter those in an external declaration *)
  | Tunboxed_tuple _ -> assert false (* not of layout value *)
  | Tpackage _ -> Block None

and value_kind_variant
  env
  subst
  ~visited
  ~depth
  (cstrs : Types.constructor_declaration list)
  rep
  =
  match rep with
  | Variant_extensible -> assert false
  | Variant_with_null -> assert false
  | Variant_unboxed ->
    (match cstrs with
     | [ { cd_args = Cstr_tuple [ { ca_type = ty; _ } ]; _ } ]
     | [ { cd_args = Cstr_record [ { ld_type = ty; _ } ]; _ } ] ->
       value_kind env subst ~visited ~depth ty
     | _ -> assert false)
  | Variant_boxed _layouts ->
    let depth = depth + 1 in
    let for_one_constructor (constructor : Types.constructor_declaration) ~depth =
      match constructor.cd_args with
      | Cstr_tuple fields ->
        let fields =
          List.map
            (fun (arg : Types.constructor_argument) ->
              value_kind env subst ~visited ~depth arg.ca_type)
            fields
        in
        fields
      | Cstr_record labels ->
        List.map
          (fun (label : Types.label_declaration) ->
            value_kind env subst ~visited ~depth label.ld_type)
          labels
    in
    let is_constant (cstr : Types.constructor_declaration) =
      match cstr.cd_args with
      | Cstr_tuple [] -> true
      | Cstr_tuple (_ :: _) | Cstr_record _ -> false
    in
    if List.for_all is_constant cstrs
    then Imm
    else (
      let _, consts, _, non_consts =
        List.fold_left
          (fun result constructor ->
            let next_const, consts, next_tag, non_consts = result in
            let fields = for_one_constructor constructor ~depth in
            if List.compare_length_with fields 0 = 0
            then (
              let consts = next_const :: consts in
              next_const + 1, consts, next_tag, non_consts)
            else (
              let non_consts = Block (Some (next_tag, fields)) :: non_consts in
              next_const, consts, next_tag + 1, non_consts))
          (0, [], 0, [])
          cstrs
      in
      let sh =
        match non_consts with
        | [] -> assert false (* See [List.for_all is_constant], above *)
        | sh :: shs ->
          let shapes = List.fold_left (fun a b -> Or (a, b)) sh shs in
          shapes
      in
      if List.length consts > 0 then Or (Imm, sh) else sh)

and value_kind_record
  env
  subst
  ~visited
  ~depth
  (labels : Types.label_declaration list)
  rep
  =
  match rep with
  | Record_mixed _ ->
    (* TODO: To support these, we'll need to stop calling
       [value_kind] on all fields.
    *)
    failwith "No support for mixed records"
  | Record_unboxed | Record_inlined (_, _, Variant_unboxed) ->
    (match labels with
     | [ { ld_type; _ } ] -> value_kind env subst ~visited ~depth ld_type
     | [] | _ :: _ :: _ -> assert false)
  | _ ->
    let fields =
      List.map
        (fun (label : Types.label_declaration) ->
          value_kind env subst ~visited ~depth label.ld_type)
        labels
    in
    let non_consts =
      match rep with
      | Record_inlined (Ordinary { runtime_tag; _ }, _, _) ->
        Block (Some (runtime_tag, fields))
      | Record_float -> FloatArray
      | Record_boxed _ -> Block (Some (0, fields))
      | Record_inlined (Extension _, _, _) -> Block (Some (0, fields))
      | Record_inlined (Null, _, _) -> assert false
      | Record_unboxed -> assert false
      | Record_mixed _ -> assert false
      | Record_ufloat -> FloatArray
    in
    non_consts
;;

let value_kind env ty =
  (* TODO: experiment with different depths here *)
  value_kind env identity_subst ~visited:Numbers.Int.Set.empty ~depth:(-1) ty
;;

(* functionality on top of [value_kind] to extract shapes from typed trees *)
let shape_from_core_type (ct : Typedtree.core_type) : type_shape =
  let env' = Environments.env_of_only_summary ct.ctyp_env in
  value_kind env' ct.ctyp_type
;;

let rec split_external_type (ct : core_type) : (core_type * bool) list * core_type =
  match ct.ctyp_desc with
  | Ttyp_poly (_, ct) -> split_external_type ct
  | Ttyp_arrow (lab, arg, cont) ->
    let args, ret = split_external_type cont in
    (match lab with
     | Nolabel | Labelled _ -> (arg, false) :: args, ret
     | Optional _ -> (arg, true) :: args, ret
     | Position _ ->
       (* This is being used only to extract a shape, so being a little wrong
          in some fields is OK. *)
       let cty =
         { ctyp_desc = Ttyp_call_pos
         ; ctyp_type = Predef.type_lexing_position
         ; ctyp_env = ct.ctyp_env
         ; ctyp_loc = Location.none
         ; ctyp_attributes = []
         }
       in
       (cty, false) :: args, ret)
  | _ -> [], ct
;;

let argument_shape (ty, opt) =
  let sh = shape_from_core_type ty in
  if opt then Or (Imm, Block (Some (0, [ sh ]))) else sh
;;

let extract_external_declaration outp (v : value_description) =
  (* Based on Primitive.parse_declaration in the compiler. *)
  let cname, _native_cname, tail =
    match v.val_prim with
    | [] ->
      (* The compiler does not allow it. *)
      let open Core in
      raise_s
        [%sexp
          "Missing name"
          , (v.val_loc.loc_start.pos_fname : string)
          , (v.val_loc.loc_start.pos_lnum : int)
          , (v.val_name.txt : string)]
    | name :: "noalloc" :: name2 :: "float" :: tail -> name, name2, tail
    | name :: "noalloc" :: name2 :: tail -> name, name2, tail
    | name :: name2 :: "float" :: tail -> name, name2, tail
    | name :: "noalloc" :: tail -> name, name, tail
    | name :: name2 :: tail -> name, name2, tail
    | name :: tail -> name, name, tail
  in
  (* compiler intrinsics should not show up in the external declarations *)
  if String.starts_with ~prefix:"%" cname
  then ()
  else (
    (match tail with
     | [] -> ()
     | tail ->
       (* The compiler currently accepts and silently ignores this case but the checker
        should reject it. *)
       let open Core in
       raise_s
         [%sexp
           "Unexpected names"
           , (v.val_loc.loc_start.pos_fname : string)
           , (v.val_loc.loc_start.pos_lnum : int)
           , (String.concat ~sep:" " tail : string)]);
    (* TODO: Add support for extracting/checking the native code name. *)
    let args, ret = split_external_type v.val_desc in
    let arg_shapes = List.map argument_shape args in
    let ret_shape = shape_from_core_type ret in
    outp
      { name = cname
      ; desc = { shape = Some { arguments = arg_shapes; return = ret_shape } }
      })
;;

let extract_from_typed_tree tt =
  let open Tast_iterator in
  let externals = ref [] in
  let outp f = externals := f :: !externals in
  let iter_structure_item (it : iterator) (si : structure_item) =
    match si.str_desc with
    | Tstr_primitive prim ->
      extract_external_declaration outp prim;
      it.value_description it prim
    | _ -> default_iterator.structure_item it si
  in
  let iter =
    { Tast_iterator.default_iterator with structure_item = iter_structure_item }
  in
  iter.structure iter tt;
  !externals
;;
