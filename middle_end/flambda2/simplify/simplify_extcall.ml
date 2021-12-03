(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type t =
  | Unchanged of { return_types : Flambda2_types.t list Or_unknown.t }
  | Poly_compare_specialized of DA.t * Expr.t
  | Invalid

(* Helpers *)
(* ******* *)

let fun_symbol simple =
  let fail simple =
    Misc.fatal_errorf "Expected a function symbol, instead of@ %a" Simple.print
      simple
  in
  Simple.pattern_match simple
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun _ ~coercion:_ -> fail simple)
        ~symbol:(fun sym ~coercion:_ -> sym))
    ~const:(fun _ -> fail simple)

let apply_cont cont v ~dbg =
  let args = [Simple.name (Name.var v)] in
  let apply_cont = Apply_cont.create cont ~args ~dbg in
  let free_names = Apply_cont.free_names apply_cont in
  let expr = Expr.create_apply_cont apply_cont in
  free_names, expr

let let_prim ~dbg v prim (free_names, body) =
  let v' = Bound_var.create v Name_mode.normal in
  let bindable = Bound_pattern.singleton v' in
  let named = Named.create_prim prim dbg in
  let free_names_of_body = Or_unknown.Known free_names in
  let let_expr = Let.create bindable named ~body ~free_names_of_body in
  let free_names = Name_occurrences.remove_var free_names v in
  let expr = Expr.create_let let_expr in
  free_names, expr

(* Exported simplification function *)
(* ******************************** *)

let simplify_comparison_of_tagged_immediates ~dbg dacc ~cmp_prim cont a b =
  let v_comp = Variable.create "comp" in
  let tagged = Variable.create "tagged" in
  let _free_names, res =
    let_prim ~dbg v_comp (P.Binary (cmp_prim, a, b))
    @@ let_prim ~dbg tagged
         (P.Unary (Box_number Untagged_immediate, Simple.var v_comp))
    @@ apply_cont ~dbg cont tagged
  in
  Poly_compare_specialized (dacc, res)

let simplify_comparison_of_boxed_numbers ~dbg dacc ~kind ~cmp_prim cont a b =
  let a_naked = Variable.create "unboxed" in
  let b_naked = Variable.create "unboxed" in
  let v_comp = Variable.create "comp" in
  let tagged = Variable.create "tagged" in
  let _free_names, res =
    let_prim ~dbg a_naked (P.Unary (Unbox_number kind, a))
    @@ let_prim ~dbg b_naked (P.Unary (Unbox_number kind, b))
    @@ let_prim ~dbg v_comp
         (P.Binary (cmp_prim, Simple.var a_naked, Simple.var b_naked))
    @@ let_prim ~dbg tagged
         (P.Unary (Box_number Untagged_immediate, Simple.var v_comp))
    @@ apply_cont ~dbg cont tagged
  in
  Poly_compare_specialized (dacc, res)

let simplify_comparison ~dbg ~dacc ~cont ~tagged_prim ~float_prim
    ~boxed_int_prim a b a_ty b_ty =
  let tenv = DA.typing_env dacc in
  match
    T.prove_is_a_boxed_number tenv a_ty, T.prove_is_a_boxed_number tenv b_ty
  with
  | Proved Untagged_immediate, Proved Untagged_immediate ->
    simplify_comparison_of_tagged_immediates ~dbg dacc cont a b
      ~cmp_prim:tagged_prim
  | Proved Naked_float, Proved Naked_float ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_float
      ~cmp_prim:float_prim
  | Proved Naked_int32, Proved Naked_int32 ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_int32
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_int32)
  | Proved Naked_int64, Proved Naked_int64 ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_int64
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_int64)
  | Proved Naked_nativeint, Proved Naked_nativeint ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b
      ~kind:Naked_nativeint
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_nativeint)
  (* Kind differences *)
  | ( Proved Untagged_immediate,
      Proved (Naked_float | Naked_nativeint | Naked_int32 | Naked_int64) )
  | ( Proved Naked_float,
      Proved (Untagged_immediate | Naked_nativeint | Naked_int32 | Naked_int64)
    )
  | ( Proved Naked_int32,
      Proved (Untagged_immediate | Naked_float | Naked_nativeint | Naked_int64)
    )
  | ( Proved Naked_int64,
      Proved (Untagged_immediate | Naked_float | Naked_nativeint | Naked_int32)
    )
  | ( Proved Naked_nativeint,
      Proved (Untagged_immediate | Naked_float | Naked_int32 | Naked_int64) )
  (* One or two of the arguments is not known *)
  | (Unknown | Invalid | Wrong_kind), (Unknown | Invalid | Wrong_kind)
  | ( (Unknown | Invalid | Wrong_kind),
      Proved
        ( Untagged_immediate | Naked_float | Naked_nativeint | Naked_int32
        | Naked_int64 ) )
  | ( Proved
        ( Untagged_immediate | Naked_float | Naked_nativeint | Naked_int32
        | Naked_int64 ),
      (Unknown | Invalid | Wrong_kind) ) ->
    Unchanged { return_types = Unknown }

let simplify_caml_make_vect dacc ~len_ty ~init_value_ty : t =
  let typing_env = DA.typing_env dacc in
  let element_kind : _ Or_unknown.t Or_bottom.t =
    (* We can't deduce subkind information, e.g. an array is all-immediates
       rather than arbitrary values, but we can deduce kind information. *)
    if not (Flambda_features.flat_float_array ())
    then
      Ok
        (Known
           (Flambda_kind.With_subkind.create (T.kind init_value_ty) Anything))
    else
      match T.prove_is_or_is_not_a_boxed_float typing_env init_value_ty with
      | Proved true ->
        (* A boxed float provided to [caml_make_vect] with the float array
           optimisation on will always yield a flat array of naked floats. *)
        Ok (Known Flambda_kind.With_subkind.naked_float)
      | Proved false | Unknown -> Ok Unknown
      | Invalid | Wrong_kind -> Bottom
  in
  let length : _ Or_bottom.t =
    match T.prove_is_a_tagged_immediate typing_env len_ty with
    | Proved () | Unknown -> Ok len_ty
    | Invalid | Wrong_kind -> Bottom
  in
  match element_kind, length with
  | Bottom, Bottom | Ok _, Bottom | Bottom, Ok _ -> Invalid
  | Ok element_kind, Ok length ->
    (* CR-someday mshinwell: We should really adjust the kind of the parameter
       of the return continuation, e.g. to go from "any value" to "float array"
       -- but that will need some more infrastructure, since the actual
       continuation definition needs to be changed on the upwards traversal.
       Also we would need to think about what would happen if there were other
       uses of the return continuation possibly with different kinds. *)
    let type_of_returned_array = T.array_of_length ~element_kind ~length in
    Unchanged { return_types = Known [type_of_returned_array] }

let simplify_returning_extcall ~dbg ~cont ~exn_cont:_ dacc fun_name args
    ~arg_types =
  match fun_name, args, arg_types with
  (* Polymorphic comparisons *)
  | ".extern__caml_compare", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp Yielding_int_like_compare_functions)
      ~tagged_prim:
        (Int_comp (Tagged_immediate, Signed, Yielding_int_like_compare_functions))
      ~boxed_int_prim:(fun kind ->
        Int_comp (kind, Signed, Yielding_int_like_compare_functions))
  | ".extern__caml_equal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~tagged_prim:(Phys_equal (K.value, Eq))
      ~float_prim:(Float_comp (Yielding_bool Eq))
      ~boxed_int_prim:(fun kind -> Phys_equal (K.Standard_int.to_kind kind, Eq))
  | ".extern__caml_notequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~tagged_prim:(Phys_equal (K.value, Neq))
      ~float_prim:(Float_comp (Yielding_bool Neq))
      ~boxed_int_prim:(fun kind ->
        Phys_equal (K.Standard_int.to_kind kind, Neq))
  | ".extern__caml_lessequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Le))
      ~tagged_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Le))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Signed, Yielding_bool Le))
  | ".extern__caml_lessthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Lt))
      ~tagged_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Lt))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Signed, Yielding_bool Lt))
  | ".extern__caml_greaterequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Ge))
      ~tagged_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Ge))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Signed, Yielding_bool Ge))
  | ".extern__caml_greaterthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool Gt))
      ~tagged_prim:(Int_comp (Tagged_immediate, Signed, Yielding_bool Gt))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Signed, Yielding_bool Gt))
  | ".extern__caml_make_vect", [_; _], [len_ty; init_value_ty] ->
    simplify_caml_make_vect dacc ~len_ty ~init_value_ty
  | _ -> Unchanged { return_types = Unknown }

(* Exported simplification function *)
(* ******************************** *)

let simplify_extcall dacc apply ~callee_ty:_ ~param_arity:_ ~return_arity:_
    ~arg_types =
  let dbg = Apply.dbg apply in
  let args = Apply.args apply in
  let exn_cont = Apply.exn_continuation apply in
  let fun_name =
    Apply.callee apply |> fun_symbol |> Symbol.linkage_name
    |> Linkage_name.to_string
  in
  match Apply.continuation apply with
  | Never_returns -> Unchanged { return_types = Unknown }
  | Return cont ->
    simplify_returning_extcall ~dbg ~cont ~exn_cont dacc fun_name args
      ~arg_types
