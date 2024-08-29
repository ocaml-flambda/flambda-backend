(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jakub Bachurski and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Types.type_expr Jkind_types.higher_jkind

let wrap = Jkind_types.wrap_higher_jkind

let unwrap = Jkind_types.unwrap_type_jkind

module History = struct
  include Jkind_intf.History

  let is_imported t = Jkind.History.is_imported (unwrap t)

  let update_reason t reason =
    Jkind.History.update_reason (unwrap t) reason |> wrap
end

(******************************)
(* constants *)

module Const = struct
  type t = Types.type_expr Jkind_types.Higher_const.t

  let wrap (t : Jkind.Const.t) : t = Type t

  let rec equal (t : t) (t' : t) =
    match t, t' with
    | Type s, Type s' -> Jkind.Const.equal s s'
    | Arrow (args1, result1), Arrow (args2, result2) ->
      List.for_all2 equal args1 args2 && equal result1 result2
    | Top, Top -> true
    | _, (Arrow _ | Type _ | Top) -> false

  let format = Jkind.Const.format_higher
end

let rec to_const (t : t) : Const.t option =
  match t.hdesc with
  | Type ty -> (
    match Jkind.get ty with Const c -> Some (Type c) | Var _ -> None)
  | Arrow (args, result) ->
    let args = List.map to_const args in
    let result = to_const result in
    if List.for_all Option.is_some args && Option.is_some result
    then Some (Arrow (List.map Option.get args, Option.get result))
    else None
  | Top -> Some Top

module Builtin = struct
  let top ~why : t = { hdesc = Top; hhistory = Creation (Top_creation why) }

  let any ~why = Jkind.Builtin.any ~why |> wrap

  let void ~why = Jkind.Builtin.void ~why |> wrap

  let value_or_null ~why = Jkind.Builtin.value_or_null ~why |> wrap

  let value ~why = Jkind.Builtin.value ~why |> wrap

  let immediate ~why = Jkind.Builtin.immediate ~why |> wrap
end

(******************************)
(* construction *)

let of_const = Jkind.higher_of_const

let of_annotation = Jkind.higher_of_annotation

let of_annotation_option_default = Jkind.higher_of_annotation_option_default

let of_type_decl = Jkind.higher_of_type_decl

let of_type_decl_default = Jkind.higher_of_type_decl_default

(******************************)
(* elimination and defaulting *)

let rec default_to_value (t : t) =
  match t.hdesc with
  | Type ty -> Jkind.default_to_value ty
  | Arrow (args, result) ->
    List.iter default_to_value args;
    default_to_value result
  | Top -> ()

(*********************************)
(* pretty printing *)

let format = Jkind.format_higher

let format_history ~intro ppf t = Jkind.format_history ~intro ppf (unwrap t)

(******************************)
(* errors *)

module Violation = Jkind.Violation

(******************************)
(* relations *)

let rec equate_or_equal ~allow_mutation (t1 : t) (t2 : t) =
  match t1.hdesc, t2.hdesc with
  | Type ty, Type ty' -> Jkind.equate_or_equal ~allow_mutation ty ty'
  | Arrow (args1, result1), Arrow (args2, result2) ->
    equate_or_equal ~allow_mutation result1 result2
    && List.for_all2 (equate_or_equal ~allow_mutation) args1 args2
  | Top, Top -> true
  | _, (Arrow _ | Type _ | Top) -> false

let equate = equate_or_equal ~allow_mutation:true

(* This mirrors the choice for Jkind.Type, following the note:
   (layouts v2.8) allow_mutation is to be set to false *)
let equal = equate_or_equal ~allow_mutation:true

let rec combine_list_or_error ~violation combine ts1 ts2 =
  let ( let* ) = Result.bind in
  match ts1, ts2 with
  | [], [] -> Ok []
  | t1 :: ts1, t2 :: ts2 ->
    let* t = combine t1 t2 in
    let* ts = combine_list_or_error ~violation combine ts1 ts2 in
    Ok (t :: ts)
  | [], _ :: _ | _ :: _, [] -> Error violation

let combine_histories (_reason : History.interact_reason) t1 _t2 =
  (t1 : t).hhistory

let rec intersection_or_error ~reason (t1 : t) (t2 : t) :
    (t, Jkind.Violation.t) result =
  let ( let* ) = Result.bind in
  let violation = Violation.of_ (No_intersection (t1, t2)) in
  match t1.hdesc, t2.hdesc with
  | _, Top -> Ok t1
  | Top, _ -> Ok t2
  | Type ty1, Type ty2 ->
    Result.map wrap (Jkind.intersection_or_error ~reason ty1 ty2)
  | Arrow (args1, result1), Arrow (args2, result2) ->
    let* args = union_list_or_error ~reason ~violation args1 args2 in
    let* result = intersection_or_error ~reason result1 result2 in
    Ok
      Jkind_types.
        { hdesc = Arrow (args, result);
          hhistory = combine_histories reason t1 t2
        }
  | _, (Arrow _ | Type _) -> Error violation

and union_or_error ~reason (t1 : t) (t2 : t) =
  let ( let* ) = Result.bind in
  (* This violation doesn't actually matter, as union always exists *)
  let violation = Violation.of_ (No_intersection (t1, t2)) in
  match t1.hdesc, t2.hdesc with
  | Type ty1, Type ty2 ->
    (* Union is infallible at type jkinds *)
    Ok (wrap (Jkind.union ~reason ty1 ty2))
  | Arrow (args1, result1), Arrow (args2, result2) ->
    Ok
      (Result.value
         ~default:(Builtin.top ~why:Union_incompatible)
         (let* args =
            intersection_list_or_error ~reason ~violation args1 args2
          in
          let* result = union_or_error ~reason result1 result2 in
          Ok
            Jkind_types.
              { hdesc = Arrow (args, result);
                hhistory = combine_histories reason t1 t2
              }))
  | Top, _ -> Ok t1
  | _, Top -> Ok t2
  | _, (Arrow _ | Type _) -> Ok (Builtin.top ~why:Union_incompatible)

and intersection_list_or_error ~reason ~violation =
  combine_list_or_error ~violation (intersection_or_error ~reason)

and union_list_or_error ~reason ~violation =
  combine_list_or_error ~violation (union_or_error ~reason)

let has_intersection t t' =
  Result.is_ok
    (intersection_or_error
     (* This reason is just used as a dummy for the test *)
       ~reason:Subjkind t t')

let rec check_sub (t1 : t) (t2 : t) : Misc.Le_result.t =
  match t1.hdesc, t2.hdesc with
  | Top, Top -> Misc.Le_result.Equal
  | _, Top -> Misc.Le_result.Less
  | Type ty, Type ty' -> Jkind.check_sub ty ty'
  | Arrow (args1, result1), Arrow (args2, result2) ->
    Misc.Le_result.combine
      (check_sub result1 result2)
      (check_sub_list args2 args1)
  | _, (Type _ | Arrow _) -> Misc.Le_result.Not_le

and check_sub_list ts ts' =
  if List.length ts <> List.length ts'
  then Misc.Le_result.Not_le
  else Misc.Le_result.combine_list (List.map2 check_sub ts ts')

let sub t t' = Misc.Le_result.is_le (check_sub t t')

let sub_or_error t t' =
  if sub t t' then Ok () else Error (Violation.of_ (Not_a_subjkind (t, t')))

let sub_with_history (t : t) (t' : t) =
  if sub t t'
  then Ok { t with hhistory = combine_histories Subjkind t t' }
  else Error (Violation.of_ (Not_a_subjkind (t, t')))

(* This doesn't do any mutation because mutating a sort variable can't make it
   any, and modal upper bounds are constant. *)
let is_max t = sub (Builtin.top ~why:Dummy_jkind) t

let has_layout_any (t : t) =
  match t.hdesc with Type ty -> Jkind.has_layout_any ty | _ -> false

module Debug_printers = struct
  let t = Jkind.Debug_printers.higher

  module Const = struct
    let t = Jkind.Debug_printers.Const.higher
  end
end
