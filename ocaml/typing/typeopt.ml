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
open Typedtree
open Lambda

type error =
    Non_value_layout of type_expr * Jkind.Violation.t option
  | Non_value_sort of Jkind.Sort.t * type_expr
  | Sort_without_extension of
      Jkind.Sort.t * Language_extension.maturity * type_expr option
  | Non_value_sort_unknown_ty of Jkind.Sort.t
  | Small_number_sort_without_extension of Jkind.Sort.t * type_expr option
  | Not_a_sort of type_expr * Jkind.Violation.t
  | Unsupported_sort of Jkind.Sort.Const.t
  | Unsupported_product_in_lazy
  | Mixed_product_array of Jkind.Sort.Const.t

exception Error of Location.t * error

(* Expand a type, looking through ordinary synonyms, private synonyms, links,
   and [@@unboxed] types. The returned type will be therefore be none of these
   cases (except in case of missing cmis).

   If we fail to fully scrape the type due to missing a missing cmi file, we
   return the original, rather than a partially expanded one.  The original may
   have cached jkind information that is more accurate than can be computed
   from its expanded form. *)
let scrape_ty env ty =
  let ty =
    match get_desc ty with
    | Tpoly(ty, _) -> ty
    | _ -> ty
  in
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.correct_levels ty in
      let ty' = Ctype.expand_head_opt env ty in
      begin match get_desc ty' with
      | Tconstr (p, _, _) ->
          begin match find_unboxed_type (Env.find_type p env) with
          | Some _ -> Ctype.get_unboxed_type_approximation env ty'
          | None -> ty'
          | exception Not_found -> ty (* missing cmi file *)
          end
      | _ ->
          ty'
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
  let ext : Jkind.Externality.t =
    (* We check that we're compiling to (64-bit) native code before counting
       External64 types as gc_ignorable, because bytecode is intended to be
       platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64
    then External64
    else External
  in
  Ctype.check_type_externality env ty ext

let maybe_pointer_type env ty =
  let ty = scrape_ty env ty in
  if is_always_gc_ignorable env ty then Immediate else Pointer

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

(* CR layouts v2.8: Calling [type_legacy_sort] in [typeopt] is not ideal
   and this function should be removed at some point. To do that, there
   needs to be a way to store sort vars on [Tconstr]s. That means
   either introducing a [Tpoly_constr], allow type parameters with
   sort info, or do something else. *)
(* CR layouts v3.0: have a better error message
   for nullable jkinds.*)
let type_legacy_sort ~why env loc ty =
  match Ctype.type_legacy_sort ~why env ty with
  | Ok sort -> sort
  | Error err -> raise (Error (loc, Not_a_sort (ty, err)))

type classification =
  | Int   (* any immediate type *)
  | Float
  | Unboxed_float of unboxed_float
  | Unboxed_int of unboxed_integer
  | Lazy
  | Addr  (* any value except a float or a lazy *)
  | Product of Jkind.Sort.Const.t list
  | Any

(* Classify a ty into a [classification]. Looks through synonyms, using [scrape_ty].
   Returning [Any] is safe, though may skip some optimizations. *)
let classify env loc ty sort : classification =
  let ty = scrape_ty env ty in
  match Jkind.(Sort.default_to_value_and_get sort) with
  | Base Value -> begin
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
           || Path.same p Predef.path_iarray
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_float32
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
  | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ | Tunboxed_tuple _ ->
      assert false
  end
  | Base Float64 -> Unboxed_float Pfloat64
  | Base Float32 -> Unboxed_float Pfloat32
  | Base Bits32 -> Unboxed_int Pint32
  | Base Bits64 -> Unboxed_int Pint64
  | Base Word -> Unboxed_int Pnativeint
  | Base Void as c ->
    raise (Error (loc, Unsupported_sort c))
  | Product sorts -> Product sorts

let rec scannable_product_array_kind loc sorts =
  List.map (sort_to_scannable_product_element_kind loc) sorts

and sort_to_scannable_product_element_kind loc (s : Jkind.Sort.Const.t) =
  (* Unfortunate: this never returns `Pint_scannable`.  Doing so would require
     this to traverse the type, rather than just the kind, or to add product
     kinds. *)
  match s with
  | Base Value -> Paddr_scannable
  | Base (Float64 | Float32 | Bits32 | Bits64 | Word) as c ->
    raise (Error (loc, Mixed_product_array c))
  | Base Void as c ->
    raise (Error (loc, Unsupported_sort c))
  | Product sorts -> Pproduct_scannable (scannable_product_array_kind loc sorts)

let rec ignorable_product_array_kind loc sorts =
  List.map (sort_to_ignorable_product_element_kind loc) sorts

and sort_to_ignorable_product_element_kind loc (s : Jkind.Sort.Const.t) =
  match s with
  | Base Value -> Pint_ignorable
  | Base Float64 -> Punboxedfloat_ignorable Pfloat64
  | Base Float32 -> Punboxedfloat_ignorable Pfloat32
  | Base Bits32 -> Punboxedint_ignorable Pint32
  | Base Bits64 -> Punboxedint_ignorable Pint64
  | Base Word -> Punboxedint_ignorable Pnativeint
  | Base Void as c ->
    raise (Error (loc, Unsupported_sort c))
  | Product sorts -> Pproduct_ignorable (ignorable_product_array_kind loc sorts)

let array_type_kind ~elt_sort env loc ty =
  match scrape_poly env ty with
  | Tconstr(p, [elt_ty], _)
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
      let elt_sort =
        match elt_sort with
        | Some s -> s
        | None ->
          type_legacy_sort ~why:Array_element env loc elt_ty
      in
      begin match classify env loc elt_ty elt_sort with
      | Any -> if Config.flat_float_array then Pgenarray else Paddrarray
      | Float -> if Config.flat_float_array then Pfloatarray else Paddrarray
      | Addr | Lazy -> Paddrarray
      | Int -> Pintarray
      | Unboxed_float f -> Punboxedfloatarray f
      | Unboxed_int i -> Punboxedintarray i
      | Product sorts ->
        (* XXX need scrape_ty elt_ty? *)
        if is_always_gc_ignorable env elt_ty then
          Pgcignorableproductarray (ignorable_product_array_kind loc sorts)
        else
          Pgcscannableproductarray (scannable_product_array_kind loc sorts)
      end
  | Tconstr(p, [], _) when Path.same p Predef.path_floatarray ->
      Pfloatarray
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp elt_sort =
  array_type_kind
    ~elt_sort:(Some elt_sort)
    exp.exp_env exp.exp_loc exp.exp_type

let array_pattern_kind pat elt_sort =
  array_type_kind
    ~elt_sort:(Some elt_sort)
    pat.pat_env pat.pat_loc pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name), [], _)
    when Ident.name mod_id = "Stdlib__Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float16_elt", Pbigarray_float16;
   "float32_elt", Pbigarray_float32;
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

let bigarray_specialize_kind_and_layout env ~kind ~layout typ =
  match scrape env typ with
  | Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev) ->
      let kind =
        match kind with
        | Pbigarray_unknown ->
          bigarray_decode_type env elt_type kind_table Pbigarray_unknown
        | _ -> kind
      in
      let layout =
        match layout with
        | Pbigarray_unknown_layout ->
          bigarray_decode_type env layout_type layout_table Pbigarray_unknown_layout
        | _ -> layout
      in
      (kind, layout)
  | _ ->
      (kind, layout)

let value_kind_of_value_jkind jkind =
  let const_jkind = Jkind.default_to_value_and_get jkind in
  let layout = Jkind.Const.get_layout const_jkind in
  let externality_upper_bound =
    Jkind.Const.get_externality_upper_bound const_jkind
  in
  match layout, externality_upper_bound with
  | Base Value, External -> Pintval
  | Base Value, External64 ->
    if !Clflags.native_code && Sys.word_size = 64 then Pintval else Pgenval
  | Base Value, Internal -> Pgenval
  | Any, _
  | Product _, _
  | Base (Void | Float64 | Float32 | Word | Bits32 | Bits64) , _ ->
    Misc.fatal_error "expected a layout of value"

(* [value_kind] has a pre-condition that it is only called on values.  With the
   current set of sort restrictions, there are two reasons this invariant may
   be violated:

   1) A bug in the type checker or the translation to lambda.
   2) A missing cmi file, so that we can't accurately compute the sort of
      some type.

   In case 1, we have a bug and should fail loudly.

   In case 2, we could issue an error and make the user add the dependency
   explicitly.  But because [value_kind] looks at the subcomponents of your type,
   this can lead to some surprising and unnecessary errors.  Suppose we're
   computing the value kind for some type:

     type t = int * M.t

   If we're missing the cmi for [M], we can't verify the invariant that
   [value_kind] is only called on values.  However, we still know the pair
   itself is a value, so a sound thing to do is fall back and return [Pgenval]
   for [t].

   On the other hand, if we're asked to compute the value kind for [M.t]
   directly and are missing the cmi for [M], we really do need to issue an error.
   This is a bug in the typechecker, which should have checked that the type
   in question has layout value.

   To account for these possibilities, [value_kind] can not simply assume its
   precondition holds, and must check.  This is implemented as calls to
   [check_type_jkind] at the start of its implementation.  If this check
   encounters layout [any] and it arises from a missing cmi, it raises
   [Missing_cmi_fallback].  If it encounters [any] that didn't arise from a
   missing cmi, or any other non-value layout, it fails loudly.

   In places where we're computing value_kinds for a bunch of subcomponents of a
   type, we catch [Missing_cmi_fallback] and just return [Pgenval] for the outer
   type.  If it escapes unhandled from value-kind, we catch it and issue the
   loud error.

   We used to believe we would eventually drop the layout check from
   [value_kind], because we thought it was just a sanity check.  This is wrong.
   We'll always need it to make sure we're sound in the event of a missing cmi
   (at least, as long as [value_kind] continues to inspect types more deeply
   than is otherwise needed for typechecking).  Even if the build system always
   passed cmis for all transitive dependencies, we shouldn't be unsound in the
   event the compiler is invoked manually without them.

   (But, if we ever do find a way to get rid of the safety check: Note that the
   it is currently doing some defaulting of sort variables, as in cases like:

     let () =
       match assert false  with
       | _ -> assert false

   There is a sort variable for the scrutinee of the match in typedtree that is
   still a sort variable after checking this.  It's fine to default this to
   anything - void would be ideal, but for now it gets value.  If the safety check
   goes away, think about whether we should add defaulting elsewhere.)
*)
exception Missing_cmi_fallback

let fallback_if_missing_cmi ~default f =
  try f () with Missing_cmi_fallback -> default

(* CR layouts v2.5: It will be possible for subcomponents of types to be
   non-values for non-error reasons (e.g., [type t = { x : float# }
   [@@unboxed]).  And in later releases, this will also happen in normal
   records, variants, tuples...

   The current layout checks are overly conservative in those cases, because
   they are currently errors.  Instead, recursive calls to value kind should
   check the sorts of the relevant types.  Ideally this wouldn't involve
   expensive layout computation, because the sorts are stored somewhere (e.g.,
   [record_representation]).  But that's not currently the case for tuples. *)
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
       layout check on the original type but not the scraped type, because of
       missing cmis.  So we try the scraped type, and fall back to correcting
       levels a second time if that doesn't work.

       It would be nice to correct levels once at the beginning and pass that
       type to both scrape_ty and the safety check, but I found this causes an
       infinite loop in the typechecker.  Whichever you do second, the layout
       check or scrape_ty, that thing will loop.  This is the test case that
       triggers it:

       (* Check for a potential infinite loop in the typing algorithm. *)
       type 'a t12 = M of 'a t12 [@@ocaml.unboxed] [@@value];;

       This should be understood, but for now the simple fall back thing is
       sufficient.  *)
    match Ctype.check_type_jkind env scty (Jkind.Builtin.value_or_null ~why:V1_safety_check)
    with
    | Ok _ -> ()
    | Error _ ->
      match
        Ctype.(check_type_jkind env
                 (correct_levels ty) (Jkind.Builtin.value_or_null ~why:V1_safety_check))
      with
      | Ok _ -> ()
      | Error violation ->
        if (Jkind.Violation.is_missing_cmi violation)
        then raise Missing_cmi_fallback
        else raise (Error (loc, Non_value_layout (ty, Some violation)))
  end;
  match get_desc scty with
  | Tconstr(p, _, _) when Path.same p Predef.path_int ->
    num_nodes_visited, Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_char ->
    num_nodes_visited, Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_float ->
    num_nodes_visited, (Pboxedfloatval Pfloat64)
  | Tconstr(p, _, _) when Path.same p Predef.path_float32 ->
    num_nodes_visited, (Pboxedfloatval Pfloat32)
  | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
    num_nodes_visited, (Pboxedintval Pint32)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
    num_nodes_visited, (Pboxedintval Pint64)
  | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
    num_nodes_visited, (Pboxedintval Pnativeint)
  | Tconstr(p, _, _) when Path.same p Predef.path_int8x16 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Int8x16))
  | Tconstr(p, _, _) when Path.same p Predef.path_int16x8 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Int16x8))
  | Tconstr(p, _, _) when Path.same p Predef.path_int32x4 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Int32x4))
  | Tconstr(p, _, _) when Path.same p Predef.path_int64x2 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Int64x2))
  | Tconstr(p, _, _) when Path.same p Predef.path_float32x4 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Float32x4))
  | Tconstr(p, _, _) when Path.same p Predef.path_float64x2 ->
    num_nodes_visited, (Pboxedvectorval (Pvec128 Float64x2))
  | Tconstr(p, _, _)
    when (Path.same p Predef.path_array
          || Path.same p Predef.path_floatarray) ->
    (* CR layouts: [~elt_sort:None] here is bad for performance. To
       fix it, we need a place to store the sort on a [Tconstr]. *)
    num_nodes_visited, Parrayval (array_type_kind ~elt_sort:None env loc ty)
  | Tconstr(p, _, _) -> begin
      let decl =
        try Env.find_type p env with Not_found -> raise Missing_cmi_fallback
      in
      if cannot_proceed () then
        num_nodes_visited,
        value_kind_of_value_jkind decl.type_jkind
      else
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        (* Default of [Pgenval] is currently safe for the missing cmi fallback
           in the case of @@unboxed variant and records, due to the precondition
           of [value_kind]. *)
        match decl.type_kind with
        | Type_variant (cstrs, rep) ->
          fallback_if_missing_cmi ~default:(num_nodes_visited, Pgenval)
            (fun () -> value_kind_variant env ~loc ~visited ~depth
                         ~num_nodes_visited cstrs rep)
        | Type_record (labels, rep) ->
          let depth = depth + 1 in
          fallback_if_missing_cmi ~default:(num_nodes_visited, Pgenval)
            (fun () -> value_kind_record env ~loc ~visited ~depth
                         ~num_nodes_visited labels rep)
        | Type_abstract _ ->
          num_nodes_visited,
          value_kind_of_value_jkind decl.type_jkind
        | Type_open -> num_nodes_visited, Pgenval
    end
  | Ttuple labeled_fields ->
    if cannot_proceed () then
      num_nodes_visited, Pgenval
    else
      fallback_if_missing_cmi ~default:(num_nodes_visited, Pgenval) (fun () ->
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        let depth = depth + 1 in
        let num_nodes_visited, fields =
          List.fold_left_map (fun num_nodes_visited (_, field) ->
            let num_nodes_visited = num_nodes_visited + 1 in
            (* CR layouts v5 - this is fine because voids are not allowed in
               tuples.  When they are, we'll need to make sure that elements
               are values before recurring.
            *)
            value_kind env ~loc ~visited ~depth ~num_nodes_visited field)
            num_nodes_visited labeled_fields
        in
        num_nodes_visited,
        Pvariant { consts = []; non_consts = [0, Constructor_uniform fields] })
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
      | [{cd_args=Cstr_tuple [{ca_type=ty}]}]
      | [{cd_args=Cstr_record [{ld_type=ty}]}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
      | _ -> assert false
    end
  | Variant_boxed cstrs_and_jkinds ->
    let depth = depth + 1 in
    let for_constructor_fields fields ~depth ~num_nodes_visited ~field_to_type =
      List.fold_left_map
        (fun num_nodes_visited field ->
           let ty = field_to_type field in
           let num_nodes_visited = num_nodes_visited + 1 in
           value_kind env ~loc ~visited ~depth ~num_nodes_visited ty)
        num_nodes_visited
        fields
    in
    let for_one_uniform_value_constructor
        fields ~field_to_type ~depth ~num_nodes_visited =
      let num_nodes_visited, fields =
        for_constructor_fields fields ~depth ~num_nodes_visited ~field_to_type
      in
      num_nodes_visited, Lambda.Constructor_uniform fields
    in
    let for_one_mixed_constructor fields ~value_prefix_len ~flat_suffix
        ~field_to_type ~depth ~num_nodes_visited =
      let value_prefix, _ =
        Misc.Stdlib.List.split_at value_prefix_len fields
      in
      assert (List.length value_prefix = value_prefix_len);
      let num_nodes_visited, value_prefix =
        for_constructor_fields value_prefix ~depth ~num_nodes_visited
          ~field_to_type
      in
      num_nodes_visited + Array.length flat_suffix,
      Lambda.Constructor_mixed
        { value_prefix; flat_suffix = Array.to_list flat_suffix }
    in
    let for_one_constructor (constructor : Types.constructor_declaration)
          ~depth ~num_nodes_visited
          ~(cstr_shape : Types.constructor_representation) =
      let num_nodes_visited = num_nodes_visited + 1 in
      match constructor.cd_args with
      | Cstr_tuple fields ->
        let field_to_type { Types.ca_type } = ca_type in
        let num_nodes_visited, fields =
          match cstr_shape with
          | Constructor_uniform_value ->
              for_one_uniform_value_constructor fields ~field_to_type
                ~depth ~num_nodes_visited
          | Constructor_mixed { value_prefix_len; flat_suffix } ->
              for_one_mixed_constructor fields
                ~value_prefix_len ~flat_suffix ~field_to_type
                ~depth ~num_nodes_visited
        in
        (false, num_nodes_visited), fields
      | Cstr_record labels ->
        let field_to_type (lbl:Types.label_declaration) = lbl.ld_type in
        let is_mutable =
          List.exists
            (fun (lbl:Types.label_declaration) ->
               Types.is_mutable lbl.ld_mutable)
            labels
        in
        let num_nodes_visited, fields =
          match cstr_shape with
          | Constructor_uniform_value ->
              for_one_uniform_value_constructor labels ~field_to_type
                ~depth ~num_nodes_visited
          | Constructor_mixed { value_prefix_len; flat_suffix } ->
              for_one_mixed_constructor labels
                ~value_prefix_len ~flat_suffix ~field_to_type
                ~depth ~num_nodes_visited
        in
        (is_mutable, num_nodes_visited), fields
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
      let _idx, result =
        List.fold_left (fun (idx, result) constructor ->
          idx+1,
          match result with
          | None -> None
          | Some (num_nodes_visited,
                  next_const, consts, next_tag, non_consts) ->
            let cstr_shape, _ = cstrs_and_jkinds.(idx) in
            let (is_mutable, num_nodes_visited), fields =
              for_one_constructor constructor ~depth ~num_nodes_visited
                ~cstr_shape
            in
            if is_mutable then None
            else match fields with
            | Constructor_uniform xs when List.compare_length_with xs 0 = 0 ->
              let consts = next_const :: consts in
              Some (num_nodes_visited,
                    next_const + 1, consts, next_tag, non_consts)
            | Constructor_mixed _ | Constructor_uniform _ ->
              let non_consts =
                (next_tag, fields) :: non_consts
              in
              Some (num_nodes_visited,
                    next_const, consts, next_tag + 1, non_consts))
          (0, Some (num_nodes_visited, 0, [], 0, []))
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
  | (Record_unboxed | (Record_inlined (_, _, Variant_unboxed))) -> begin
      (* CR layouts v1.5: This should only be reachable in the case of a missing
         cmi, according to the comment on scrape_ty.  Reevaluate whether it's
         needed when we deal with missing cmis. *)
      match labels with
      | [{ld_type}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ld_type
      | [] | _ :: _ :: _ -> assert false
    end
  | Record_inlined (_, _, (Variant_boxed _ | Variant_extensible))
  | Record_boxed _ | Record_float | Record_ufloat | Record_mixed _ -> begin
      let is_mutable =
        List.exists (fun label -> Types.is_mutable label.Types.ld_mutable)
          labels
      in
      if is_mutable then
        num_nodes_visited, Pgenval
      else
        let num_nodes_visited, fields =
          match rep with
          | Record_unboxed ->
              (* The outer match guards against this *)
              assert false
          | Record_inlined (_, Constructor_uniform_value, _)
          | Record_boxed _ | Record_float | Record_ufloat ->
              let num_nodes_visited, fields =
                List.fold_left_map
                  (fun num_nodes_visited (label:Types.label_declaration) ->
                    let num_nodes_visited = num_nodes_visited + 1 in
                    let num_nodes_visited, field =
                      (* CR layouts v5: when we add other layouts, we'll need to
                        check here that we aren't about to call value_kind on a
                        different sort (we can get this info from the
                        label.ld_jkind). For now we rely on the layout check at
                        the top of value_kind to rule out void. *)
                      (* We're using the `Pboxedfloatval` value kind for unboxed
                        floats inside of records. This is kind of a lie, but
                         that was already happening here due to the float record
                        optimization. *)
                      match rep with
                      | Record_float | Record_ufloat ->
                        num_nodes_visited, Pboxedfloatval Pfloat64
                      | Record_inlined _ | Record_boxed _ ->
                          value_kind env ~loc ~visited ~depth ~num_nodes_visited
                            label.ld_type
                      | Record_mixed _ | Record_unboxed ->
                          (* The outer match guards against this *)
                          assert false
                    in
                    num_nodes_visited, field)
                  num_nodes_visited labels
              in
              num_nodes_visited, Constructor_uniform fields
          | Record_inlined (_, Constructor_mixed shape, _)
          | Record_mixed shape ->
              let { value_prefix_len; flat_suffix } : mixed_product_shape =
                shape
              in
              let labels_value_prefix, _ =
                Misc.Stdlib.List.split_at value_prefix_len labels
              in
              assert (List.length labels_value_prefix = value_prefix_len);
              let num_nodes_visited, value_prefix =
                List.fold_left_map
                  (fun num_nodes_visited
                    (label:Types.label_declaration) ->
                    let num_nodes_visited = num_nodes_visited + 1 in
                    value_kind env ~loc ~visited ~depth ~num_nodes_visited
                      label.ld_type)
                  num_nodes_visited labels_value_prefix
              in
              let flat_suffix = Array.to_list flat_suffix in
              num_nodes_visited,
              Constructor_mixed { value_prefix; flat_suffix }
        in
        let non_consts =
          match rep with
          | Record_inlined (Ordinary {runtime_tag}, _, _) ->
            [runtime_tag, fields]
          | Record_float | Record_ufloat ->
            [ Obj.double_array_tag, fields ]
          | Record_boxed _ ->
            [0, fields]
          | Record_inlined (Extension _, _, _) ->
            [0, fields]
          | Record_mixed _ ->
            [0, fields]
          | Record_unboxed -> assert false
        in
        (num_nodes_visited, Pvariant { consts = []; non_consts })
    end

let value_kind env loc ty =
  try
    let (_num_nodes_visited, value_kind) =
      value_kind env ~loc ~visited:Numbers.Int.Set.empty ~depth:0
        ~num_nodes_visited:0 ty
    in
    value_kind
  with
  | Missing_cmi_fallback -> raise (Error (loc, Non_value_layout (ty, None)))

let[@inline always] rec layout_of_const_sort_generic ~value_kind ~error
  : Jkind.Sort.Const.t -> _ = function
  | Base Value -> Lambda.Pvalue (Lazy.force value_kind)
  | Base Float64 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_float Pfloat64
  | Base Word when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Pnativeint
  | Base Bits32 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Pint32
  | Base Bits64 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Pint64
  | Base Float32 when Language_extension.(is_at_least Layouts Stable) &&
                            Language_extension.(is_enabled Small_numbers) ->
    Lambda.Punboxed_float Pfloat32
  | Product consts when Language_extension.(is_at_least Layouts Beta) ->
    (* CR layouts v7.1: assess whether it is important for performance to support
       deep value_kinds here *)
    Lambda.Punboxed_product
      (List.map (layout_of_const_sort_generic ~value_kind:(lazy Pgenval) ~error)
         consts)
  | ((  Base (Void | Float32 | Float64 | Word | Bits32 | Bits64)
      | Product _) as const) ->
    error const

let layout env loc sort ty =
  layout_of_const_sort_generic
    (Jkind.Sort.default_to_value_and_get sort)
    ~value_kind:(lazy (value_kind env loc ty))
    ~error:(function
      | Base Value -> assert false
      | Base Void ->
        raise (Error (loc, Non_value_sort (Jkind.Sort.void,ty)))
      | Base Float32 as const ->
        raise (Error (loc, Small_number_sort_without_extension
                             (Jkind.Sort.of_const const, Some ty)))
      | Base (Float64 | Word | Bits32 | Bits64) as const ->
        raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                   Stable,
                                                   Some ty)))
      | Product _ as const ->
        raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                   Beta,
                                                   Some ty)))
    )

let layout_of_sort loc sort =
  layout_of_const_sort_generic
    (Jkind.Sort.default_to_value_and_get sort)
    ~value_kind:(lazy Pgenval)
    ~error:(function
    | Base Value -> assert false
    | Base Void ->
      raise (Error (loc, Non_value_sort_unknown_ty Jkind.Sort.void))
    | Base Float32 as const ->
      raise (Error (loc, Small_number_sort_without_extension
                           (Jkind.Sort.of_const const, None)))
    | Base (Float64 | Word | Bits32 | Bits64) as const ->
      raise (Error (loc, Sort_without_extension
                           (Jkind.Sort.of_const const, Stable, None)))
    | Product _ as const ->
      raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                 Beta,
                                                 None))))

let layout_of_const_sort c =
  layout_of_const_sort_generic
    c
    ~value_kind:(lazy Pgenval)
    ~error:(fun const ->
      Misc.fatal_errorf "layout_of_const_sort: %a encountered"
        Jkind.Sort.Const.format const)

let function_return_layout env loc sort ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> layout env loc sort rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function2_return_layout env loc sort ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> function_return_layout env loc sort rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function_arg_layout env loc sort ty =
  match is_function_type env ty with
  | Some (arg_type, _) -> layout env loc sort arg_type
  | None -> Misc.fatal_error "function_arg_layout called on non-function type"

(** Whether a forward block is needed for a lazy thunk on a value, i.e.
    if the value can be represented as a float/forward/lazy *)
let lazy_val_requires_forward env loc ty =
  let sort = Jkind.Sort.for_lazy_body in
  match classify env loc ty sort with
  | Any | Lazy -> true
  (* CR layouts: Fix this when supporting lazy unboxed values.
     Blocks with forward_tag can get scanned by the gc thus can't
     store unboxed values. Not boxing is also incorrect since the lazy
     type has layout [value] which is different from these unboxed layouts. *)
  | Unboxed_float _ | Unboxed_int _ ->
    Misc.fatal_error "Unboxed value encountered inside lazy expression"
  | Float -> Config.flat_float_array
  | Addr | Int -> false
  | Product _ -> raise (Error (loc, Unsupported_product_in_lazy))


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
        | Const_float32 _ (* There is no float32 array optimization *)
        | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
    | Texp_function _
    | Texp_construct (_, {cstr_arity = 0}, _, _) ->
       `Constant_or_function
    | Texp_constant(Const_float _) ->
       if Config.flat_float_array
       then `Float_that_cannot_be_shortcut
       else `Constant_or_function
    | Texp_ident _ when lazy_val_requires_forward e.exp_env e.exp_loc e.exp_type ->
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
  | Punboxed_float f1, Punboxed_float f2 ->
      if equal_boxed_float f1 f2 then l1 else Ptop
  | Punboxed_int bi1, Punboxed_int bi2 ->
      if equal_boxed_integer bi1 bi2 then l1 else Ptop
  | Punboxed_vector vi1, Punboxed_vector vi2 ->
      Lambda.join_boxed_vector_layout vi1 vi2
  | Punboxed_product layouts1, Punboxed_product layouts2 ->
      if List.compare_lengths layouts1 layouts2 <> 0 then Ptop
      else Punboxed_product (List.map2 layout_union layouts1 layouts2)
  | (Ptop | Pvalue _ | Punboxed_float _ | Punboxed_int _ |
     Punboxed_vector _ | Punboxed_product _),
    _ ->
      Ptop

(* Error report *)
open Format

let report_error ppf = function
  | Non_value_layout (ty, err) ->
      fprintf ppf
        "Non-value detected in [value_kind].@ Please report this error to \
         the Jane Street compilers team.";
      begin match err with
      | None ->
        fprintf ppf "@ Could not find cmi for: %a" Printtyp.type_expr ty
      | Some err ->
        fprintf ppf "@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err
      end
  | Non_value_sort (sort, ty) ->
      fprintf ppf
        "Non-value layout %a detected in [Typeopt.layout] as sort for type@ %a.@ \
         Please report this error to the Jane Street compilers team."
        Jkind.Sort.format sort Printtyp.type_expr ty
  | Non_value_sort_unknown_ty sort ->
      fprintf ppf
        "Non-value layout %a detected in [layout_of_sort]@ Please report this \
         error to the Jane Street compilers team."
        Jkind.Sort.format sort
  | Sort_without_extension (sort, maturity, ty) ->
      fprintf ppf "Non-value layout %a detected" Jkind.Sort.format sort;
      begin match ty with
      | None -> ()
      | Some ty -> fprintf ppf " as sort for type@ %a" Printtyp.type_expr ty
      end;
      fprintf ppf
        ",@ but this requires extension %s, which is not enabled.@ \
         If you intended to use this layout, please add this flag to your \
         build file.@ \
         Otherwise, please report this error to the Jane Street compilers team."
        (Language_extension.to_command_line_string Layouts maturity)
  | Small_number_sort_without_extension (sort, ty) ->
      fprintf ppf "Non-value layout %a detected" Jkind.Sort.format sort;
      begin match ty with
      | None -> ()
      | Some ty -> fprintf ppf " as sort for type@ %a" Printtyp.type_expr ty
      end;
      let extension, verb, flags =
        match Language_extension.(is_at_least Layouts Stable),
              Language_extension.(is_enabled Small_numbers) with
        | false, true -> " layouts", "is", "this flag"
        | true, false -> " small_numbers", "is", "this flag"
        | false, false -> "s layouts and small numbers", "are", "these flags"
        | true, true -> assert false
      in
      fprintf ppf
        ",@ but this requires the extension%s, which %s not enabled.@ \
         If you intended to use this layout, please add %s to your \
         build file.@ \
         Otherwise, please report this error to the Jane Street compilers team."
        extension verb flags
  | Not_a_sort (ty, err) ->
      fprintf ppf "A representable layout is required here.@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err
  | Unsupported_sort const ->
      fprintf ppf "Layout %a is not supported yet."
        Jkind.Sort.Const.format const
  | Unsupported_product_in_lazy ->
      fprintf ppf
        "Product layout detected in lazy in [Typeopt.Layout].@ \
         Please report this error to the Jane Street compilers team."
  | Mixed_product_array const ->
      fprintf ppf
        "Unboxed product array elements must be external or contain all gc \
         scannable types.@ This product is not external but contains an \
         element of sort %a."
        Jkind.Sort.Const.format const

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
