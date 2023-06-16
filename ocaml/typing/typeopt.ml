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
open Layouts
open Types
open Asttypes
open Typedtree
open Lambda

(* CR layout v2: Error infrastructure was added to this module just to support
   the void sanity check.  When we're ready to take that out, remove the errors
   stuff. *)
type error =
    Non_value_layout of type_expr * Layout.Violation.t

exception Error of Location.t * error

(* Expand a type, looking through ordinary synonyms, private synonyms,
   links, and [@@unboxed] types. The returned type will be therefore be none
   of these cases (except in case of missing cmis). *)
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
  let layout =
    (* We check that we're compiling to (64-bit) native code before counting
       immediate64 types as gc_ignorable, because bytecode is intended to be
       platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64
    then Layout.immediate64 ~why:Gc_ignorable_check
    else Layout.immediate ~why:Gc_ignorable_check
  in
  Result.is_ok (Ctype.check_type_layout env ty layout)

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
          | Type_abstract ->
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

let value_kind_of_value_layout layout =
  match Layout.get_default_value layout with
  | Value -> Pgenval
  | Immediate -> Pintval
  | Immediate64 ->
    if !Clflags.native_code && Sys.word_size = 64 then Pintval else Pgenval
  | Any | Void -> assert false

(* Invariant: [value_kind] functions may only be called on types with layout
   value. *)
(* CR layouts v2: However, the current version allows for it to be called on
   types whose layouts we can not determine due to missing cmis.  This is
   unsound once we have non-value layouts.

   For the particularly annoying case where we're missing the cmis for
   subcomponent of a composite type, we'll need to fall back and return Pgenval
   for the whole type.  See this commit, which implements that:

   https://github.com/ccasin/ocaml-jst/tree/unboxed-types-v1-layouts-fallback
*)
(* CR layouts v5 (and maybe other versions): As we relax the requirement that
   various things have to be values, many recursive calls in [value_kind]
   need to be updated to check layouts. *)
(* CR layouts v2: At the moment, sort variables may be defaulted to value by
   value_kind.  For example, here:

   let () =
     match assert false  with
     | _ -> assert false

   There is a sort variable for the scrutinee of the match in typedtree that is
   still a sort variable after checking this.  And this is fine - we default
   sorts that appear in interfaces, but other sorts may still appear in the
   typedtree.  These represent places where the compiler can pick a sort, and
   should pick whatever is most efficient (eventually void).

   When we eliminate the safety check here, we should think again about where
   and how sort variables are defaulted.
*)
let rec value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
  : int * value_kind =
  let[@inline] cannot_proceed () =
    Numbers.Int.Set.mem (get_id ty) visited
    || depth >= 2
    || num_nodes_visited >= 30
  in
  let scty = scrape_ty env ty in
  begin
    (* CR layouts: We want to avoid correcting levels twice, and scrape_ty will
       correct levels for us.  But it may be the case that we could do the
       sanity check on the original type but not the scraped type, because of
       missing cmis.  So we try the scraped type, and fall back to correcting
       levels a second time if that doesn't work.

       It would be nice to correct levels once at the beginning and pass that
       type to both scrape_ty and the safety check, but I found this causes an
       infinite loop in the typechecker.  Whichever you do second, the layout
       check or scrape_ty, that thing will loop.  This is the test case that
       triggers it:

       (* Check for a potential infinite loop in the typing algorithm. *)
       type 'a t12 = M of 'a t12 [@@ocaml.unboxed] [@@value];;

       This should be understood, but for now I'm doing the simple fall back
       thing so I can test the performance difference.
    *)
    match Ctype.check_type_layout env scty (Layout.value ~why:V1_safety_check)
    with
    | Ok _ -> ()
    | Error _ ->
      match
        Ctype.(check_type_layout env
                 (correct_levels ty) (Layout.value ~why:V1_safety_check))
      with
      | Ok _ -> ()
      | Error violation ->
        if not (Layout.Violation.is_missing_cmi violation)
        then raise (Error (loc, Non_value_layout (ty, violation)))
        (* CR layouts v1.5: stop allowing missing cmis *)

  end;
  match get_desc scty with
  | Tconstr(p, _, _) when Path.same p Predef.path_int ->
    num_nodes_visited, Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_char ->
    num_nodes_visited, Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_float ->
    num_nodes_visited, Pfloatval
  | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
    num_nodes_visited, (Pboxedintval Pint32)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
    num_nodes_visited, (Pboxedintval Pint64)
  | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
    num_nodes_visited, (Pboxedintval Pnativeint)
  | Tconstr(p, _, _)
    when (Path.same p Predef.path_array
          || Path.same p Predef.path_floatarray) ->
    num_nodes_visited, Parrayval (array_type_kind env ty)
  | Tconstr(p, _, _) -> begin
    try
      let decl = Env.find_type p env in
      if cannot_proceed () then
        num_nodes_visited,
        value_kind_of_value_layout decl.type_layout
      else
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        match decl.type_kind with
        | Type_variant (cstrs, rep) ->
          value_kind_variant env ~loc ~visited ~depth ~num_nodes_visited cstrs rep
        | Type_record (labels, rep) ->
          let depth = depth + 1 in
          value_kind_record env ~loc ~visited ~depth ~num_nodes_visited labels rep
        | Type_abstract ->
          num_nodes_visited,
          value_kind_of_value_layout decl.type_layout
        | Type_open -> num_nodes_visited, Pgenval
    with Not_found -> num_nodes_visited, Pgenval
    (* CR layouts v1.5: stop allowing missing cmis. *)
    end
  | Ttuple fields ->
    if cannot_proceed () then
      num_nodes_visited, Pgenval
    else begin
      let visited = Numbers.Int.Set.add (get_id ty) visited in
      let depth = depth + 1 in
      let num_nodes_visited, fields =
        List.fold_left_map (fun num_nodes_visited field ->
          let num_nodes_visited = num_nodes_visited + 1 in
          (* CR layouts v5 - this is fine because voids are not allowed in
             tuples.  When they are, we probably need to add a list of layouts
             to Ttuple, as in variant_representation and record_representation
             *)
          value_kind env ~loc ~visited ~depth ~num_nodes_visited field)
          num_nodes_visited fields
      in
      num_nodes_visited,
      Pvariant { consts = []; non_consts = [0, fields] }
    end
  | Tvariant row ->
    num_nodes_visited,
    if Ctype.tvariant_not_immediate row then Pgenval else Pintval
  | _ ->
    num_nodes_visited, Pgenval

and value_kind_variant env ~loc ~visited ~depth ~num_nodes_visited
      (cstrs : Types.constructor_declaration list) rep =
  match rep with
  | Variant_extensible -> assert false
  | Variant_unboxed -> begin
      (* CR layouts v1.5: This should only be reachable in the case of a missing
         cmi, according to the comment on scrape_ty.  Reevaluate whether it's
         needed when we deal with missing cmis. *)
      match cstrs with
      | [{cd_args=Cstr_tuple [ty,_]}]
      | [{cd_args=Cstr_record [{ld_type=ty}]}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
      | _ -> assert false
    end
  | Variant_boxed _layouts ->
    let depth = depth + 1 in
    let for_one_constructor (constructor : Types.constructor_declaration)
          ~depth ~num_nodes_visited =
      let num_nodes_visited = num_nodes_visited + 1 in
      match constructor.cd_args with
      | Cstr_tuple fields ->
        let num_nodes_visited, fields =
          List.fold_left_map
            (fun num_nodes_visited (ty, _) ->
               let num_nodes_visited = num_nodes_visited + 1 in
               (* CR layouts v2: when we add other layouts, we'll need to check
                  here that we aren't about to call value_kind on a different
                  sort (we can get this info from the variant representation).
                  For now we rely on the sanity check at the top of value_kind
                  to rule out void. *)
               value_kind env ~loc ~visited ~depth ~num_nodes_visited ty)
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
                value_kind env ~loc ~visited ~depth ~num_nodes_visited
                  label.ld_type
              in
              (is_mutable, num_nodes_visited), field)
          (false, num_nodes_visited) labels
    in
    let is_constant (cstr: Types.constructor_declaration) =
      (* CR layouts v5: This won't count constructors with void args as
         constant. *)
      match cstr.cd_args with
      | Cstr_tuple [] -> true
      | (Cstr_tuple (_::_) | Cstr_record _) -> false
    in
    if List.for_all is_constant cstrs then
      (num_nodes_visited, Pintval)
    else
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
          cstrs
      in
      begin match result with
      | None -> (num_nodes_visited, Pgenval)
      | Some (num_nodes_visited, _, consts, _, non_consts) ->
        match non_consts with
        | [] -> assert false  (* See [List.for_all is_constant], above *)
        | _::_ ->
          (num_nodes_visited, Pvariant { consts; non_consts })
      end

and value_kind_record env ~loc ~visited ~depth ~num_nodes_visited
      (labels : Types.label_declaration list) rep =
  match rep with
  | (Record_unboxed | (Record_inlined (_,Variant_unboxed))) -> begin
      (* CR layouts v1.5: This should only be reachable in the case of a missing
         cmi, according to the comment on scrape_ty.  Reevaluate whether it's
         needed when we deal with missing cmis. *)
      match labels with
      | [{ld_type}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ld_type
      | [] | _ :: _ :: _ -> assert false
    end
  | _ -> begin
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
              (* CR layouts v2: when we add other layouts, we'll need to check
                 here that we aren't about to call value_kind on a different
                 sort (we can get this info from the label.ld_layout).  For now
                 we rely on the sanity check at the top of value_kind to rule
                 out void. *)
              value_kind env ~loc ~visited ~depth ~num_nodes_visited
                label.ld_type
            in
            (is_mutable, num_nodes_visited), field)
          (false, num_nodes_visited) labels
      in
      if is_mutable then
        num_nodes_visited, Pgenval
      else
        let non_consts =
          match rep with
          | Record_inlined (Ordinary {runtime_tag}, _) ->
            [runtime_tag, fields]
          | Record_float ->
            [ Obj.double_array_tag,
              List.map (fun _ -> Pfloatval) fields ]
          | Record_boxed _ ->
            [0, fields]
          | Record_inlined (Extension _, _) ->
            [0, fields]
          | Record_unboxed -> assert false
        in
        (num_nodes_visited, Pvariant { consts = []; non_consts })
    end

let value_kind env loc ty =
  let (_num_nodes_visited, value_kind) =
    value_kind env ~loc ~visited:Numbers.Int.Set.empty ~depth:0
      ~num_nodes_visited:0 ty
  in
  value_kind

(* CR layouts v2: We'll have other layouts. Think about what to do with the
   sanity check in value_kind. *)
let layout env loc ty = Lambda.Pvalue (value_kind env loc ty)

let function_return_layout env loc ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> layout env loc rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function2_return_layout env loc ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> function_return_layout env loc rhs
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

let layout_union l1 l2 =
  match l1, l2 with
  | Pbottom, l
  | l, Pbottom -> l
  | Pvalue layout1, Pvalue layout2 ->
      Pvalue (value_kind_union layout1 layout2)
  | Punboxed_float, Punboxed_float -> Punboxed_float
  | Punboxed_int bi1, Punboxed_int bi2 ->
      if equal_boxed_integer bi1 bi2 then l1 else Ptop
  | (Ptop | Pvalue _ | Punboxed_float | Punboxed_int _), _ ->
      Ptop

(* Error report *)
open Format

let report_error ppf = function
  | Non_value_layout (ty, err) ->
      fprintf ppf
        "Non-value detected in [value_kind].@ Please report this error to \
         the Jane Street compilers team.@ %a"
        (Layout.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
