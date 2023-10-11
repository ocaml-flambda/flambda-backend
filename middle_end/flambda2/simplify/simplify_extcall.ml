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

open! Simplify_import
module RO = Removed_operations

type t =
  | Unchanged of { return_types : Flambda2_types.t list Or_unknown.t }
  | Specialised of DA.t * Expr.t * Removed_operations.t
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
  let free_names = NO.remove_var free_names ~var:v in
  let expr = Expr.create_let let_expr in
  free_names, expr

(* Exported simplification function *)
(* ******************************** *)

let simplify_comparison_of_tagged_immediates ~dbg dacc ~cmp_prim cont a b =
  let v_comp = Variable.create "comp" in
  let tagged = Variable.create "tagged" in
  let _free_names, res =
    let_prim ~dbg v_comp (P.Binary (cmp_prim, a, b))
    @@ let_prim ~dbg tagged (P.Unary (Tag_immediate, Simple.var v_comp))
    @@ apply_cont ~dbg cont tagged
  in
  Specialised (dacc, res, RO.specialized_poly_compare)

let simplify_comparison_of_boxed_numbers ~dbg dacc ~kind ~cmp_prim cont a b =
  let a_naked = Variable.create "unboxed" in
  let b_naked = Variable.create "unboxed" in
  let v_comp = Variable.create "comp" in
  let tagged = Variable.create "tagged" in
  let _free_names, res =
    (* XXX try to remove @@ *)
    let_prim ~dbg a_naked (P.Unary (Unbox_number kind, a))
    @@ let_prim ~dbg b_naked (P.Unary (Unbox_number kind, b))
    @@ let_prim ~dbg v_comp
         (P.Binary (cmp_prim, Simple.var a_naked, Simple.var b_naked))
    @@ let_prim ~dbg tagged (P.Unary (Tag_immediate, Simple.var v_comp))
    @@ apply_cont ~dbg cont tagged
  in
  Specialised (dacc, res, RO.specialized_poly_compare)

let simplify_comparison ~dbg ~dacc ~cont ~tagged_prim ~float_prim
    ~boxed_int_prim a b a_ty b_ty =
  let tenv = DA.typing_env dacc in
  match
    ( T.prove_is_a_boxed_or_tagged_number tenv a_ty,
      T.prove_is_a_boxed_or_tagged_number tenv b_ty )
  with
  | Proved Tagged_immediate, Proved Tagged_immediate ->
    simplify_comparison_of_tagged_immediates ~dbg dacc cont a b
      ~cmp_prim:tagged_prim
  | Proved (Boxed (_, Naked_float, _)), Proved (Boxed (_, Naked_float, _)) ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_float
      ~cmp_prim:float_prim
  | Proved (Boxed (_, Naked_int32, _)), Proved (Boxed (_, Naked_int32, _)) ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_int32
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_int32)
  | Proved (Boxed (_, Naked_int64, _)), Proved (Boxed (_, Naked_int64, _)) ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b ~kind:Naked_int64
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_int64)
  | ( Proved (Boxed (_, Naked_nativeint, _)),
      Proved (Boxed (_, Naked_nativeint, _)) ) ->
    simplify_comparison_of_boxed_numbers ~dbg dacc cont a b
      ~kind:Naked_nativeint
      ~cmp_prim:(boxed_int_prim K.Standard_int.Naked_nativeint)
  (* Mismatches between varieties of numbers *)
  | Proved Tagged_immediate, Proved (Boxed _)
  | Proved (Boxed _), Proved Tagged_immediate
  | ( Proved
        (Boxed
          ( _,
            ( Naked_float | Naked_int32 | Naked_int64 | Naked_nativeint
            | Naked_vec128 ),
            _ )),
      Proved (Boxed _) )
  (* One or two of the arguments is not known *)
  | Unknown, Unknown
  | Unknown, Proved (Tagged_immediate | Boxed _)
  | Proved (Tagged_immediate | Boxed _), Unknown ->
    Unchanged { return_types = Unknown }

let simplify_caml_make_vect dacc ~len_ty ~init_value_ty : t =
  let typing_env = DA.typing_env dacc in
  let element_kind : _ Or_unknown_or_bottom.t =
    (* We can't deduce subkind information, e.g. an array is all-immediates
       rather than arbitrary values, but we can deduce kind information. *)
    if not (Flambda_features.flat_float_array ())
    then Ok (Flambda_kind.With_subkind.create (T.kind init_value_ty) Anything)
    else
      match T.prove_is_or_is_not_a_boxed_float typing_env init_value_ty with
      | Proved true ->
        (* A boxed float provided to [caml_make_vect] with the float array
           optimisation on will always yield a flat array of naked floats. *)
        Ok Flambda_kind.With_subkind.naked_float
      | Proved false | Unknown -> Unknown
  in
  (* CR-someday mshinwell: We should really adjust the kind of the parameter of
     the return continuation, e.g. to go from "any value" to "float array" --
     but that will need some more infrastructure, since the actual continuation
     definition needs to be changed on the upwards traversal. Also we would need
     to think about what would happen if there were other uses of the return
     continuation possibly with different kinds.

     Also maybe we should allow static allocation of these arrays for reasonable
     sizes. *)
  let type_of_returned_array =
    T.mutable_array ~element_kind ~length:len_ty Alloc_mode.For_types.heap
  in
  Unchanged { return_types = Known [type_of_returned_array] }

let simplify_returning_extcall ~dbg ~cont ~exn_cont:_ dacc fun_name args
    ~arg_types =
  match fun_name, args, arg_types with
  (* Polymorphic comparisons *)
  | "caml_compare", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_int_like_compare_functions ()))
      ~tagged_prim:
        (Int_comp (Tagged_immediate, Yielding_int_like_compare_functions Signed))
      ~boxed_int_prim:(fun kind ->
        Int_comp (kind, Yielding_int_like_compare_functions Signed))
  | "caml_equal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~tagged_prim:(Phys_equal Eq) ~float_prim:(Float_comp (Yielding_bool Eq))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool Eq))
  | "caml_notequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~tagged_prim:(Phys_equal Neq) ~float_prim:(Float_comp (Yielding_bool Neq))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool Neq))
  | "caml_lessequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool (Le ())))
      ~tagged_prim:(Int_comp (Tagged_immediate, Yielding_bool (Le Signed)))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool (Le Signed)))
  | "caml_lessthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool (Lt ())))
      ~tagged_prim:(Int_comp (Tagged_immediate, Yielding_bool (Lt Signed)))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool (Lt Signed)))
  | "caml_greaterequal", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool (Ge ())))
      ~tagged_prim:(Int_comp (Tagged_immediate, Yielding_bool (Ge Signed)))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool (Ge Signed)))
  | "caml_greaterthan", [a; b], [a_ty; b_ty] ->
    simplify_comparison ~dbg ~dacc ~cont a b a_ty b_ty
      ~float_prim:(Float_comp (Yielding_bool (Gt ())))
      ~tagged_prim:(Int_comp (Tagged_immediate, Yielding_bool (Gt Signed)))
      ~boxed_int_prim:(fun kind -> Int_comp (kind, Yielding_bool (Gt Signed)))
  | "caml_make_vect", [_; _], [len_ty; init_value_ty] ->
    simplify_caml_make_vect dacc ~len_ty ~init_value_ty
  | _ -> Unchanged { return_types = Unknown }

(* Exported simplification function *)
(* ******************************** *)

let simplify_extcall dacc apply ~callee_ty:_ ~arg_types =
  let dbg = Apply.dbg apply in
  let args = Apply.args apply in
  let exn_cont = Apply.exn_continuation apply in
  let fun_name =
    let callee =
      match Apply.callee apply with
      | Some callee -> callee
      | None ->
        Misc.fatal_errorf
          "Application expression did not provide callee for C call:@ %a"
          Apply.print apply
    in
    callee |> fun_symbol |> Symbol.linkage_name |> Linkage_name.to_string
  in
  match Apply.continuation apply with
  | Never_returns -> Unchanged { return_types = Unknown }
  | Return cont ->
    simplify_returning_extcall ~dbg ~cont ~exn_cont dacc fun_name args
      ~arg_types
