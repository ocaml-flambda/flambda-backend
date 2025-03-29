(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mode
open Jkind_types
open Jkind_axis
open Types

[@@@warning "+9"]

let print_type_expr : (Format.formatter -> type_expr -> unit) ref =
  ref (fun _ _ -> assert false)

let set_print_type_expr p = print_type_expr := p

let raw_type_expr : (Format.formatter -> type_expr -> unit) ref =
  ref (fun _ _ -> assert false)

let set_raw_type_expr p = raw_type_expr := p

module Nonempty_list = Misc.Nonempty_list

(* A *sort* is the information the middle/back ends need to be able to
   compile a manipulation (storing, passing, etc) of a runtime value. *)
module Sort = struct
  include Jkind_types.Sort

  module Flat = struct
    type t =
      | Var of Var.id
      | Base of base
  end
end

type sort = Sort.t

module Sub_failure_reason = struct
  type t =
    | Axis_disagreement of Axis.packed
    | Layout_disagreement
    | Constrain_ran_out_of_fuel
end

module Sub_result = struct
  type t =
    | Equal
    | Less
    | Not_le of Sub_failure_reason.t Nonempty_list.t

  let[@inline] of_le_result ~failure_reason (le_result : Misc.Le_result.t) =
    match le_result with
    | Less -> Less
    | Equal -> Equal
    | Not_le -> Not_le (failure_reason ())

  let[@inline] combine sr1 sr2 =
    match sr1, sr2 with
    | Equal, Equal -> Equal
    | Equal, Less | Less, Equal | Less, Less -> Less
    | Not_le reasons1, Not_le reasons2 ->
      Not_le Nonempty_list.(reasons1 @ reasons2)
    | Not_le reasons, _ | _, Not_le reasons -> Not_le reasons

  let require_le = function
    | Less | Equal -> Ok ()
    | Not_le reason -> Error reason

  let is_le t = require_le t |> Result.is_ok
end

(* A *layout* of a type describes the way values of that type are stored at
   runtime, including details like width, register convention, calling
   convention, etc. A layout may be *representable* or *unrepresentable*.  The
   middle/back ends are unable to cope with values of types with an
   unrepresentable layout. The only unrepresentable layout is `any`, which is
   the top of the layout lattice. *)
module Layout = struct
  open Jkind_types.Layout

  type nonrec 'sort t = 'sort t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  module Const = struct
    type t = Const.t =
      | Any
      | Base of Sort.base
      | Product of t list

    let max = Any

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> Sort.equal_base b1 b2
      | Any, Any -> true
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | (Base _ | Any | Product _), _ -> false

    module Static = struct
      let value = Base Sort.Value

      let void = Base Sort.Void

      let float64 = Base Sort.Float64

      let float32 = Base Sort.Float32

      let word = Base Sort.Word

      let bits32 = Base Sort.Bits32

      let bits64 = Base Sort.Bits64

      let vec128 = Base Sort.Vec128

      let of_base : Sort.base -> t = function
        | Value -> value
        | Void -> void
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
    end

    include Static

    let rec get_sort : t -> Sort.Const.t option = function
      | Any -> None
      | Base b -> Some (Base b)
      | Product ts ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)

    let of_sort s =
      let rec of_sort : Sort.t -> _ = function
        | Var _ -> None
        | Base b -> Some (Static.of_base b)
        | Product sorts ->
          Option.map
            (fun x -> Product x)
            (* [Sort.get] is deep, so no need to repeat it here *)
            (Misc.Stdlib.List.map_option of_sort sorts)
      in
      of_sort (Sort.get s)

    let of_flat_sort : Sort.Flat.t -> _ = function
      | Var _ -> None
      | Base b -> Some (Static.of_base b)

    let rec of_sort_const : Sort.Const.t -> t = function
      | Base b -> Base b
      | Product consts -> Product (List.map of_sort_const consts)

    let to_string t =
      let rec to_string nested (t : t) =
        match t with
        | Any -> "any"
        | Base b -> Sort.to_string_base b
        | Product ts ->
          String.concat ""
            [ (if nested then "(" else "");
              String.concat " & " (List.map (to_string true) ts);
              (if nested then ")" else "") ]
      in
      to_string false t

    module Debug_printers = struct
      open Format

      let t ppf t = fprintf ppf "%s" (to_string t)
    end
  end

  module Debug_printers = struct
    open Format

    let rec t format_sort ppf = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" format_sort s
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             (t format_sort))
          ts
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any -> Any
    | Base b -> Sort (Sort.of_base b)
    | Product cs -> Product (List.map of_const cs)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> Product lays

  let rec to_sort = function
    | Any -> None
    | Sort s -> Some s
    | Product ts -> to_product_sort ts

  and to_product_sort ts =
    Option.map
      (fun x -> Sort.Product x)
      (Misc.Stdlib.List.map_option to_sort ts)

  let rec get : Sort.t t -> Sort.Flat.t t =
    let rec flatten_sort : Sort.t -> Sort.Flat.t t = function
      | Var v -> Sort (Var (Sort.Var.get_id v))
      | Base b ->
        Sort (Base b)
        (* No need to call [Sort.get] here, because one [get] is deep. *)
      | Product sorts -> Product (List.map flatten_sort sorts)
    in
    function
    | Any -> Any
    | Sort s -> flatten_sort (Sort.get s)
    | Product ts -> Product (List.map get ts)

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any -> Some Any
    | Sort s -> of_sort s
    | Product layouts ->
      Option.map
        (fun x -> Layout.Const.Product x)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let sort_equal_result ~allow_mutation result =
    match (result : Sort.equate_result) with
    | (Equal_mutated_first | Equal_mutated_second | Equal_mutated_both)
      when not allow_mutation ->
      Misc.fatal_errorf "Jkind.equal: Performed unexpected mutation"
    | Unequal -> false
    | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second
    | Equal_mutated_both ->
      true

  let rec equate_or_equal ~allow_mutation t1 t2 =
    match t1, t2 with
    | Sort s1, Sort s2 ->
      sort_equal_result ~allow_mutation (Sort.equate_tracking_mutation s1 s2)
    | Product ts, Sort sort | Sort sort, Product ts -> (
      (* If [ts] can't be turned into a product sort -- because it has [any]
         -- then equality will surely fail. No need to create new sort
         variables here. *)
      match to_product_sort ts with
      | None -> false
      | Some sort' ->
        sort_equal_result ~allow_mutation
          (Sort.equate_tracking_mutation sort sort'))
    | Product ts1, Product ts2 ->
      List.equal (equate_or_equal ~allow_mutation) ts1 ts2
    | Any, Any -> true
    | (Any | Sort _ | Product _), _ -> false

  let sub t1 t2 =
    let rec sub t1 t2 : Misc.Le_result.t =
      match t1, t2 with
      | Any, Any -> Equal
      | _, Any -> Less
      | Any, _ -> Not_le
      | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Equal else Not_le
      | Product ts1, Product ts2 ->
        if List.compare_lengths ts1 ts2 = 0
        then Misc.Le_result.combine_list (List.map2 sub ts1 ts2)
        else Not_le
      | Product ts1, Sort s2 -> (
        (* This case could use [to_product_sort] because every component will need
           to end up less than a sort (so, no [any]), but it seems easier to keep
           this case lined up with the inverse case, which definitely cannot use
           [to_product_sort]. *)
        match Sort.decompose_into_product s2 (List.length ts1) with
        | None -> Not_le
        | Some ss2 ->
          Misc.Le_result.combine_list
            (List.map2 (fun t1 s2 -> sub t1 (Sort s2)) ts1 ss2))
      | Sort s1, Product ts2 -> (
        match Sort.decompose_into_product s1 (List.length ts2) with
        | None -> Not_le
        | Some ss1 ->
          Misc.Le_result.combine_list
            (List.map2 (fun s1 t2 -> sub (Sort s1) t2) ss1 ts2))
    in
    Sub_result.of_le_result (sub t1 t2) ~failure_reason:(fun () ->
        [Layout_disagreement])

  let rec intersection t1 t2 =
    (* pre-condition to [products]: [ts1] and [ts2] have the same length *)
    let products ts1 ts2 =
      let components = List.map2 intersection ts1 ts2 in
      Option.map
        (fun x -> Product x)
        (Misc.Stdlib.List.some_if_all_elements_are_some components)
    in
    match t1, t2 with
    | _, Any -> Some t1
    | Any, _ -> Some t2
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Some t1 else None
    | Product ts1, Product ts2 ->
      if List.compare_lengths ts1 ts2 = 0 then products ts1 ts2 else None
    | Product ts, Sort sort | Sort sort, Product ts -> (
      match Sort.decompose_into_product sort (List.length ts) with
      | None -> None
      | Some sorts -> products ts (List.map (fun x -> Sort x) sorts))

  let of_new_sort_var () =
    let sort = Sort.new_var () in
    Sort sort, sort

  let rec default_to_value_and_get : _ Layout.t -> Const.t = function
    | Any -> Any
    | Sort s -> Const.of_sort_const (Sort.default_to_value_and_get s)
    | Product p -> Product (List.map default_to_value_and_get p)

  let format ppf layout =
    let open Format in
    let rec pp_element ~nested ppf : _ Layout.t -> unit = function
      | Any -> fprintf ppf "any"
      | Sort s -> Sort.format ppf s
      | Product ts ->
        let pp_sep ppf () = Format.fprintf ppf " & " in
        Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
    in
    pp_element ~nested:false ppf layout
end

module Externality = Externality
module Nullability = Nullability

module History = struct
  include Jkind_intf.History

  let is_imported t =
    match t.history with Creation Imported -> true | _ -> false

  (* CR layouts: Anything that returns false here could probably just be removed,
     but let's keep the info around at least during development. *)
  let is_informative t =
    match t.history with Creation Imported -> false | _ -> true

  let update_reason t reason = { t with history = Creation reason }

  let with_warning t = { t with has_warned = true }

  let has_warned t = t.has_warned
end

(******************************)
(*** user errors ***)

module Error = struct
  type t =
    | Insufficient_level :
        { jkind : Parsetree.jkind_annotation;
          required_layouts_level : Language_extension.maturity
        }
        -> t
    | Unknown_jkind of Parsetree.jkind_annotation
    | Multiple_jkinds of
        { from_annotation : Parsetree.jkind_annotation;
          from_attribute : Builtin_attributes.jkind_attribute Location.loc
        }
    | Unimplemented_syntax
    | With_on_right

  exception User_error of Location.t * t
end

let raise ~loc err = raise (Error.User_error (loc, err))

(******************************)

(* Returns the set of axes that is relevant under a given modality. For example,
   under the [global] modality, the locality axis is *not* relevant. *)
let relevant_axes_of_modality ~relevant_for_nullability ~modality =
  Axis_set.create ~f:(fun ~axis:(Pack axis) ->
      match axis with
      | Modal axis ->
        let (P axis) = Mode.Const.Axis.alloc_as_value (P axis) in
        let modality = Mode.Modality.Value.Const.proj axis modality in
        not (Mode.Modality.is_constant modality)
      (* The kind-inference.md document (in the repo) discusses both constant
         modalities and identity modalities. Of course, reality has modalities
         (such as [shared]) that are neither constants nor identities. Here, we
         treat all non-constant modalities the way that the design treats identity
         modalities. This is safe, because it leads to a minimum of
         mode-crossing. In the future, we may want to complexify the modal-kinds
         setup to allow for more mode-crossing in the presence of non-constant
         non-identity modalities. *)
      | Nonmodal Externality -> true
      | Nonmodal Nullability -> (
        match relevant_for_nullability with
        | `Relevant -> true
        | `Irrelevant -> false))

module Mod_bounds = struct
  include Types.Jkind_mod_bounds

  let min =
    create ~locality:Locality.min ~linearity:Linearity.min
      ~uniqueness:Uniqueness.min ~portability:Portability.min
      ~contention:Contention.min ~yielding:Yielding.min
      ~externality:Externality.min ~nullability:Nullability.min

  let max =
    create ~locality:Locality.max ~linearity:Linearity.max
      ~uniqueness:Uniqueness.max ~portability:Portability.max
      ~contention:Contention.max ~yielding:Yielding.max
      ~externality:Externality.max ~nullability:Nullability.max

  let join t1 t2 =
    let locality = Locality.join (locality t1) (locality t2) in
    let linearity = Linearity.join (linearity t1) (linearity t2) in
    let uniqueness = Uniqueness.join (uniqueness t1) (uniqueness t2) in
    let portability = Portability.join (portability t1) (portability t2) in
    let contention = Contention.join (contention t1) (contention t2) in
    let yielding = Yielding.join (yielding t1) (yielding t2) in
    let externality = Externality.join (externality t1) (externality t2) in
    let nullability = Nullability.join (nullability t1) (nullability t2) in
    create ~locality ~linearity ~uniqueness ~portability ~contention ~yielding
      ~externality ~nullability

  let meet t1 t2 =
    let locality = Locality.meet (locality t1) (locality t2) in
    let linearity = Linearity.meet (linearity t1) (linearity t2) in
    let uniqueness = Uniqueness.meet (uniqueness t1) (uniqueness t2) in
    let portability = Portability.meet (portability t1) (portability t2) in
    let contention = Contention.meet (contention t1) (contention t2) in
    let yielding = Yielding.meet (yielding t1) (yielding t2) in
    let externality = Externality.meet (externality t1) (externality t2) in
    let nullability = Nullability.meet (nullability t1) (nullability t2) in
    create ~locality ~linearity ~uniqueness ~portability ~contention ~yielding
      ~externality ~nullability

  let less_or_equal t1 t2 =
    let[@inline] axis_less_or_equal ~le ~axis a b : Sub_result.t =
      match le a b, le b a with
      | true, true -> Equal
      | true, false -> Less
      | false, _ -> Not_le [Axis_disagreement axis]
    in
    Sub_result.combine
      (axis_less_or_equal ~le:Locality.le
         ~axis:(Pack (Modal (Comonadic Areality))) (locality t1) (locality t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Uniqueness.le
            ~axis:(Pack (Modal (Monadic Uniqueness))) (uniqueness t1)
            (uniqueness t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Linearity.le
            ~axis:(Pack (Modal (Comonadic Linearity))) (linearity t1)
            (linearity t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Contention.le
            ~axis:(Pack (Modal (Monadic Contention))) (contention t1)
            (contention t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Portability.le
            ~axis:(Pack (Modal (Comonadic Portability))) (portability t1)
            (portability t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Yielding.le
            ~axis:(Pack (Modal (Comonadic Yielding))) (yielding t1)
            (yielding t2))
    @@ Sub_result.combine
         (axis_less_or_equal ~le:Externality.le
            ~axis:(Pack (Nonmodal Externality)) (externality t1)
            (externality t2))
    @@ axis_less_or_equal ~le:Nullability.le ~axis:(Pack (Nonmodal Nullability))
         (nullability t1) (nullability t2)

  let equal t1 t2 =
    Locality.equal (locality t1) (locality t2)
    && Linearity.equal (linearity t1) (linearity t2)
    && Uniqueness.equal (uniqueness t1) (uniqueness t2)
    && Portability.equal (portability t1) (portability t2)
    && Contention.equal (contention t1) (contention t2)
    && Yielding.equal (yielding t1) (yielding t2)
    && Externality.equal (externality t1) (externality t2)
    && Nullability.equal (nullability t1) (nullability t2)

  let[@inline] get (type a) ~(axis : a Axis.t) t : a =
    match axis with
    | Modal (Monadic Uniqueness) -> uniqueness t
    | Modal (Comonadic Areality) -> locality t
    | Modal (Monadic Contention) -> contention t
    | Modal (Comonadic Linearity) -> linearity t
    | Modal (Comonadic Portability) -> portability t
    | Modal (Comonadic Yielding) -> yielding t
    | Nonmodal Externality -> externality t
    | Nonmodal Nullability -> nullability t

  (** Get all axes that are set to max *)
  let get_max_axes t =
    let[@inline] add_if b ax axis_set =
      if b then Axis_set.add axis_set ax else axis_set
    in
    Axis_set.empty
    |> add_if
         (Locality.le Locality.max (locality t))
         (Modal (Comonadic Areality))
    |> add_if
         (Linearity.le Linearity.max (linearity t))
         (Modal (Comonadic Linearity))
    |> add_if
         (Uniqueness.le Uniqueness.max (uniqueness t))
         (Modal (Monadic Uniqueness))
    |> add_if
         (Portability.le Portability.max (portability t))
         (Modal (Comonadic Portability))
    |> add_if
         (Contention.le Contention.max (contention t))
         (Modal (Monadic Contention))
    |> add_if
         (Yielding.le Yielding.max (yielding t))
         (Modal (Comonadic Yielding))
    |> add_if
         (Externality.le Externality.max (externality t))
         (Nonmodal Externality)
    |> add_if
         (Nullability.le Nullability.max (nullability t))
         (Nonmodal Nullability)

  let for_arrow =
    create ~linearity:Linearity.max ~locality:Locality.max
      ~uniqueness:Uniqueness.min ~portability:Portability.max
      ~contention:Contention.min ~yielding:Yielding.max
      ~externality:Externality.max ~nullability:Nullability.Non_null

  let to_mode_crossing t =
    Mode.Crossing.of_bounds
      Types.Jkind_mod_bounds.
        { comonadic =
            { areality = locality t;
              linearity = linearity t;
              portability = portability t;
              yielding = yielding t
            };
          monadic = { uniqueness = uniqueness t; contention = contention t }
        }
end

module With_bounds = struct
  type 'd t = 'd Types.with_bounds constraint 'd = 'l * 'r

  module Type_info = struct
    include With_bounds_type_info

    let print ppf { relevant_axes } =
      let open Format in
      fprintf ppf "@[{ relevant_axes = %a }@]" Axis_set.print relevant_axes

    let join { relevant_axes = axes1 } { relevant_axes = axes2 } =
      { relevant_axes = Axis_set.union axes1 axes2 }

    let axes_ignored_by_modalities ~mod_bounds
        ~type_info:{ relevant_axes = explicit_relevant_axes } =
      (* Axes that are max are implicitly relevant. ie, including or excluding an
         axis from the set of relevant axes is semantically equivalent if the mod-
         bound on that axis is max.

         Note that this mostly matters because we mark axes as /not/ explicitly relevant
         on types when the axis is max, for performance reasons - but we don't want to
         print constant modalities for those axes!
      *)
      let implicit_relevant_axes = Mod_bounds.get_max_axes mod_bounds in
      let relevant_axes =
        Axis_set.union explicit_relevant_axes implicit_relevant_axes
      in
      let irrelevant_axes = Axis_set.complement relevant_axes in
      (* nullability is always implicitly irrelevant since it isn't deep *)
      Axis_set.remove irrelevant_axes (Nonmodal Nullability)
  end

  let to_best_eff_map = function
    | No_with_bounds -> With_bounds_types.empty
    | With_bounds bounds -> bounds

  let for_all (type l r) f (t : (l * r) t) =
    match t with
    | No_with_bounds -> true
    | With_bounds tys -> With_bounds_types.for_all f tys

  let to_list : type d. d with_bounds -> _ = function
    | No_with_bounds -> []
    | With_bounds tys -> tys |> With_bounds_types.to_seq |> List.of_seq

  open Allowance

  include Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

    let disallow_left : type l r. (l * r) t -> (disallowed * r) t = function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let disallow_right : type l r. (l * r) t -> (l * disallowed) t = function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let allow_left : type l r. (allowed * r) t -> (l * r) t = function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let allow_right : type l r. (l * allowed) t -> (l * r) t = function
      | No_with_bounds -> No_with_bounds
  end)

  let try_allow_l : type l r. (l * r) t -> (allowed * r) t option = function
    | No_with_bounds -> Some No_with_bounds
    | With_bounds _ as b -> Some b

  let try_allow_r : type l r. (l * r) t -> (l * allowed) t option = function
    | No_with_bounds -> Some No_with_bounds
    | With_bounds _ -> None

  let map_type_expr (type l r) f : (l * r) t -> (l * r) t = function
    | No_with_bounds -> No_with_bounds
    | With_bounds tys ->
      With_bounds (With_bounds_types.map_with_key (fun ty ti -> f ty, ti) tys)

  let map (type l r) f : (l * r) t -> (l * r) t = function
    | No_with_bounds -> No_with_bounds
    | With_bounds tys -> With_bounds (With_bounds_types.map f tys)

  let debug_print_types ppf tys =
    let open Format in
    pp_print_seq
      ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
      (fun ppf (ty, ti) ->
        fprintf ppf "@[(%a, %a)@]" !raw_type_expr ty Type_info.print ti)
      ppf
      (With_bounds_types.to_seq tys)

  let debug_print (type l r) ppf : (l * r) t -> _ =
    let open Format in
    function
    | No_with_bounds -> fprintf ppf "No_with_bounds"
    | With_bounds tys ->
      fprintf ppf "With_bounds @[[%a]@]" debug_print_types tys

  let join_bounds =
    With_bounds_types.merge (fun _ ti1 ti2 ->
        match ti1, ti2 with
        | None, None -> None
        | Some ti, None -> Some ti
        | None, Some ti -> Some ti
        | Some ti1, Some ti2 -> Some (Type_info.join ti1 ti2))

  (* You might think that we can only do joins on the left. But that's not true!
     We can join constants. The important thing is that the allowances of both
     arguments are the same and that they match the result: this will mean that
     if we have any with_bounds in either argument, the result is an l-With_bounds, as
     required. This might change once we have arrow kinds, but we'll deal with
     that when we get there. *)
  let join (type l r) (bag1 : (l * r) t) (bag2 : (l * r) t) : (l * r) t =
    match bag1, bag2 with
    | No_with_bounds, No_with_bounds -> No_with_bounds
    | No_with_bounds, b -> b
    | b, No_with_bounds -> b
    | With_bounds tys1, With_bounds tys2 -> With_bounds (join_bounds tys1 tys2)

  let meet (type l1 l2) (bag1 : (l1 * allowed) t) (bag2 : (l2 * allowed) t) :
      (l1 * allowed) t =
    match bag1, bag2 with No_with_bounds, No_with_bounds -> No_with_bounds

  let add_bound type_expr type_info tys =
    With_bounds_types.update type_expr
      (function
        | None -> Some type_info | Some ti -> Some (Type_info.join ti type_info))
      tys

  let add type_expr type_info bounds =
    match bounds with
    | No_with_bounds ->
      With_bounds (With_bounds_types.singleton type_expr type_info)
    | With_bounds bounds -> With_bounds (add_bound type_expr type_info bounds)

  let add_modality ~relevant_for_nullability ~modality ~type_expr
      (t : (allowed * 'r) t) : (allowed * 'r) t =
    let relevant_axes =
      relevant_axes_of_modality ~relevant_for_nullability ~modality
    in
    match t with
    | No_with_bounds ->
      With_bounds
        (With_bounds_types.singleton type_expr
           ({ relevant_axes } : With_bounds_type_info.t))
    | With_bounds tys -> With_bounds (add_bound type_expr { relevant_axes } tys)

  let format (type l r) ppf (t : (l * r) t) =
    match t with
    | No_with_bounds -> ()
    | With_bounds wbs ->
      let type_exprs =
        wbs |> With_bounds_types.to_seq
        |> Seq.map (fun (ty, _) -> Format.asprintf "%a" !print_type_expr ty)
        |> List.of_seq
        (* HACK: we pre-format the types as strings so we so we can sort them
           lexicographically, because otherwise the order of printed [with]s is
           nondeterministic. This is sad, but we'd need deterministic sorting of types to
           work around it.

           CR aspsmith: remove this (and the same HACK in Oprint) if we ever add
           deterministic, semantic type comparison *)
        |> List.sort String.compare
      in
      Format.(
        fprintf ppf "%a"
          (pp_print_list (fun ppf -> fprintf ppf "with@ %s"))
          type_exprs)
end

module Layout_and_axes = struct
  module Allow_disallow = Allowance.Magic_allow_disallow (struct
    type (_, 'layout, 'd) sided = ('layout, 'd) layout_and_axes

    let disallow_left t =
      { t with with_bounds = With_bounds.disallow_left t.with_bounds }

    let disallow_right t =
      { t with with_bounds = With_bounds.disallow_right t.with_bounds }

    let allow_left t =
      { t with with_bounds = With_bounds.allow_left t.with_bounds }

    let allow_right t =
      { t with with_bounds = With_bounds.allow_right t.with_bounds }
  end)

  include Allow_disallow

  let map f t = { t with layout = f t.layout }

  let map_option f t =
    match f t.layout with None -> None | Some layout -> Some { t with layout }

  let map_type_expr f t =
    { t with with_bounds = With_bounds.map_type_expr f t.with_bounds }

  let equal eq_layout
      { layout = lay1;
        mod_bounds = mod_bounds1;
        with_bounds = (No_with_bounds : (allowed * allowed) with_bounds)
      }
      { layout = lay2;
        mod_bounds = mod_bounds2;
        with_bounds = (No_with_bounds : (allowed * allowed) with_bounds)
      } =
    eq_layout lay1 lay2 && Mod_bounds.equal mod_bounds1 mod_bounds2

  let try_allow_l :
      type l r.
      ('layout, l * r) layout_and_axes ->
      ('layout, Allowance.allowed * r) layout_and_axes option =
   fun { layout; mod_bounds; with_bounds } ->
    match With_bounds.try_allow_l with_bounds with
    | None -> None
    | Some with_bounds ->
      Some { layout; mod_bounds = Obj.magic mod_bounds; with_bounds }

  let try_allow_r { layout; mod_bounds; with_bounds } =
    match With_bounds.try_allow_r with_bounds with
    | Some with_bounds ->
      Some { layout; mod_bounds = Obj.magic mod_bounds; with_bounds }
    | None -> None

  let debug_print format_layout ppf { layout; mod_bounds; with_bounds } =
    Format.fprintf ppf "{ layout = %a;@ mod_bounds = %a;@ with_bounds = %a }"
      format_layout layout Mod_bounds.debug_print mod_bounds
      With_bounds.debug_print with_bounds

  type 'r normalize_mode =
    | Require_best : disallowed normalize_mode
    | Ignore_best : 'r normalize_mode

  module Fuel_status = struct
    type t =
      | Ran_out_of_fuel
      | Sufficient_fuel

    let both a b =
      match a, b with
      | Ran_out_of_fuel, _ | _, Ran_out_of_fuel -> Ran_out_of_fuel
      | Sufficient_fuel, Sufficient_fuel -> Sufficient_fuel
  end

  (* Normalize the jkind. If mode is [Require_best], only jkinds that have quality [Best]
     will be used. If mode is [Ignore_best], then jkinds that have quality [Not_best] will
     also be used. Since [Ignore_best] can use [Not_best] jkinds, the result is guaranteed
     to have no with-bounds.

     At each step during normalization, before expanding a type, [map_type_info]
     is used to map the type-info for the type being expanded. The type can be
     prevented from being expanded by mapping the relevant axes to an empty
     set. [map_type_info] is used by sub_jkind_l to remove irrelevant axes.

     The [skip_axes] argument says which axes we can skip normalizing along. The behavior
     of this function for these axes is undefined; do *not* look at the results for these
     axes.
  *)
  let normalize (type layout l r1 r2) ~jkind_of_type ~(mode : r2 normalize_mode)
      ~skip_axes
      ?(map_type_info :
         (type_expr -> With_bounds_type_info.t -> With_bounds_type_info.t)
         option) (t : (layout, l * r1) layout_and_axes) :
      (layout, l * r2) layout_and_axes * Fuel_status.t =
    (* handle a few common cases first, before doing anything else *)
    (* DEBUGGING
       Format.printf "@[normalize: %a@;  relevant_axes: %a@]@;"
         With_bounds.debug_print t.with_bounds Jkind_axis.Axis_set.print
         relevant_axes;
    *)
    match t with
    | { with_bounds = No_with_bounds; _ } as t -> t, Sufficient_fuel
    | { with_bounds = With_bounds tys; _ } as t
      when Axis_set.equal skip_axes Axis_set.all
           || With_bounds_types.is_empty tys ->
      { t with with_bounds = No_with_bounds }, Sufficient_fuel
    | _
      when Mod_bounds.is_max_within_set t.mod_bounds
             (Axis_set.complement skip_axes) ->
      { t with with_bounds = No_with_bounds }, Sufficient_fuel
    | _ ->
      (* Sadly, it seems hard (impossible?) to be sure to expand all types
         here without using a fuel parameter to stop infinite regress. Here
         is a nasty case:

         {[
           type zero
           type 'n succ

           type 'n loopy = Mk of 'n succ loopy list [@@unboxed]
         ]}

         First off: this type *is* inhabited, because of the [list] intervening
         type (which can be empty). It's also inhabited by various circular
         structures.

         But what's the jkind of ['n loopy]? It must be the jkind of
         ['n succ loopy list], which is [immutable_data with 'n succ loopy].
         In order to see if we shouldn't mode-cross, we have to expand the
         ['n succ loopy] in the jkind, but expanding that just yields the need
         to expand ['n succ succ loopy], and around we go.

         It seems hard to avoid this problem. And so we use fuel. Yet we want
         both a small amount of fuel (a type like [type t = K of (t * t) list]
         gets big very quickly) and a lot of fuel (we can imagine using a unit
         of fuel for each level of a deeply nested record structure). The
         compromise is to track fuel per type head, where a type head is either
         the path to a type constructor (like [t] or [loopy]) or a tuple.
         (We need to include tuples because of the possibility of recursive
         types and the fact that tuples track their element types in their
         jkind's with_bounds.)

         The initial fuel per type head is 10, as it seems hard to imagine that
         we're going to make meaningful progress if we've seen the same type
         head 10 times in one line of recursive descent. (This "one line of
         recursive descent" bit is why we recur separately down one type before
         iterating down the list.)
      *)
      (* CR reisenberg: document seen_args *)
      let module Loop_control = struct
        type t =
          { tuple_fuel : int;
            constr : (int * type_expr list) Path.Map.t;
            fuel_status : Fuel_status.t
          }

        type result =
          | Stop of t (* give up, returning [max] *)
          | Skip (* skip reducing this type, but otherwise continue *)
          | Continue of t (* continue, with a new [t] *)

        let initial_fuel_per_ty = 2

        let starting =
          { tuple_fuel = initial_fuel_per_ty;
            constr = Path.Map.empty;
            fuel_status = Sufficient_fuel
          }

        let rec check ({ tuple_fuel; constr; fuel_status = _ } as t) ty =
          match Types.get_desc ty with
          | Tpoly (ty, _) -> check t ty
          | Ttuple _ ->
            if tuple_fuel > 0
            then Continue { t with tuple_fuel = tuple_fuel - 1 }
            else Stop { t with fuel_status = Ran_out_of_fuel }
          | Tconstr (p, args, _) -> (
            match Path.Map.find_opt p constr with
            | None ->
              Continue
                { t with
                  constr = Path.Map.add p (initial_fuel_per_ty, args) constr
                }
            | Some (fuel, seen_args) ->
              if List.for_all2
                   (fun ty1 ty2 ->
                     TransientTypeOps.equal (Transient_expr.repr ty1)
                       (Transient_expr.repr ty2))
                   seen_args args
              then Skip
              else if fuel > 0
              then
                Continue
                  { t with constr = Path.Map.add p (fuel - 1, args) constr }
              else Stop { t with fuel_status = Ran_out_of_fuel })
          | Tvar _ | Tarrow _ | Tunboxed_tuple _ | Tobject _ | Tfield _ | Tnil
          | Tvariant _ | Tunivar _ | Tpackage _ ->
            (* these cases either cannot be infinitely recursive or their jkinds
               do not have with_bounds *)
            (* CR layouts v2.8: Some of these might get with-bounds someday. We
               should double-check before we're done that they haven't. *)
            Continue t
          | Tlink _ | Tsubst _ ->
            Misc.fatal_error "Tlink or Tsubst in normalize"
      end in
      let rec loop (ctl : Loop_control.t) bounds_so_far relevant_axes :
          (type_expr * With_bounds_type_info.t) list ->
          Mod_bounds.t * (l * r2) with_bounds * Fuel_status.t = function
        (* early cutoff *)
        | [] -> bounds_so_far, No_with_bounds, ctl.fuel_status
        | _ when Mod_bounds.equal Mod_bounds.max bounds_so_far ->
          (* CR layouts v2.8: we can do better by early-terminating on a per-axis
             basis *)
          bounds_so_far, No_with_bounds, Sufficient_fuel
        | (ty, ti) :: bs -> (
          (* Map the type's info before expanding the type *)
          let ti =
            match map_type_info with
            | None -> ti
            | Some map_type_info -> map_type_info ty ti
          in
          (* We don't care about axes that are already max because they can't get
             any better or worse. By ignoring them, we may be able to terminate
             early *)
          let ti : With_bounds_type_info.t =
            { relevant_axes =
                Axis_set.diff ti.relevant_axes
                  (Mod_bounds.get_max_axes bounds_so_far)
            }
          in
          match Axis_set.is_empty ti.relevant_axes with
          | true ->
            (* If [ty] is not relevant to any axes, then we can safely drop it and
               thereby avoid doing the work of expanding it. *)
            loop ctl bounds_so_far relevant_axes bs
          | false -> (
            let join_bounds b1 b2 ~relevant_axes =
              let value_for_axis (type a) ~(axis : a Axis.t) : a =
                if Axis_set.mem relevant_axes axis
                then
                  let (module Bound_ops) = Axis.get axis in
                  Bound_ops.join (Mod_bounds.get ~axis b1)
                    (Mod_bounds.get ~axis b2)
                else Mod_bounds.get ~axis b1
              in
              Mod_bounds.create
                ~locality:(value_for_axis ~axis:(Modal (Comonadic Areality)))
                ~linearity:(value_for_axis ~axis:(Modal (Comonadic Linearity)))
                ~uniqueness:(value_for_axis ~axis:(Modal (Monadic Uniqueness)))
                ~portability:
                  (value_for_axis ~axis:(Modal (Comonadic Portability)))
                ~contention:(value_for_axis ~axis:(Modal (Monadic Contention)))
                ~yielding:(value_for_axis ~axis:(Modal (Comonadic Yielding)))
                ~externality:(value_for_axis ~axis:(Nonmodal Externality))
                ~nullability:(value_for_axis ~axis:(Nonmodal Nullability))
            in
            let found_jkind_for_ty new_ctl b_upper_bounds b_with_bounds quality
                : Mod_bounds.t * (l * r2) with_bounds * Fuel_status.t =
              match quality, mode with
              | Best, _ | Not_best, Ignore_best ->
                (* The relevant axes are the intersection of the relevant axes within our
                   branch of the with-bounds tree, and the relevant axes on this
                   particular with-bound *)
                let next_relevant_axes =
                  Axis_set.intersection relevant_axes ti.relevant_axes
                in
                let bounds_so_far =
                  join_bounds bounds_so_far b_upper_bounds
                    ~relevant_axes:next_relevant_axes
                in
                (* Descend into the with-bounds of each of our with-bounds types'
                    with-bounds *)
                let bounds_so_far, nested_with_bounds, fuel_result1 =
                  loop new_ctl bounds_so_far next_relevant_axes
                    (With_bounds.to_list b_with_bounds)
                in
                (* CR layouts v2.8: we use [new_ctl] here, not [ctl], to avoid big
                   quadratic stack growth for very widely recursive types. This is
                   sad, since it prevents us from mode crossing a record with 20
                   lists with different payloads, but less sad than a stack
                   overflow of the compiler during type declaration checking.

                   Ideally, this whole problem goes away once we rethink fuel.
                *)
                let bounds, bs', fuel_result2 =
                  loop new_ctl bounds_so_far relevant_axes bs
                in
                ( bounds,
                  With_bounds.join nested_with_bounds bs',
                  Fuel_status.both fuel_result1 fuel_result2 )
              | Not_best, Require_best ->
                (* CR layouts v2.8: The type annotation on the next line is
                   necessary only because [loop] is
                   local. Bizarre. Investigate. *)
                let bounds_so_far, (bs' : (l * r2) With_bounds.t), fuel_result =
                  loop new_ctl bounds_so_far relevant_axes bs
                in
                bounds_so_far, With_bounds.add ty ti bs', fuel_result
            in
            match Loop_control.check ctl ty with
            | Stop ctl_after_stop ->
              (* out of fuel, so assume [ty] has the worst possible bounds. *)
              found_jkind_for_ty ctl_after_stop Mod_bounds.max No_with_bounds
                Not_best [@nontail]
            | Skip -> loop ctl bounds_so_far relevant_axes bs (* skip [b] *)
            | Continue ctl_after_unpacking_b -> (
              match jkind_of_type ty with
              | Some b_jkind ->
                found_jkind_for_ty ctl_after_unpacking_b
                  b_jkind.jkind.mod_bounds b_jkind.jkind.with_bounds
                  b_jkind.quality [@nontail]
              | None ->
                (* kind of b is not principally known, so we treat it as having
                   the max bound (only along the axes we care about for this
                   type!) *)
                found_jkind_for_ty ctl_after_unpacking_b Mod_bounds.max
                  No_with_bounds Not_best [@nontail])))
      in
      let mod_bounds = Mod_bounds.set_max_in_set t.mod_bounds skip_axes in
      let mod_bounds, with_bounds, fuel_status =
        loop Loop_control.starting mod_bounds
          (Axis_set.complement skip_axes)
          (With_bounds.to_list t.with_bounds)
      in
      { t with mod_bounds; with_bounds }, fuel_status
end

(*********************************)

module Quality = struct
  include Allowance.Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd jkind_quality constraint 'd = 'l * 'r

    let disallow_left :
        type l r. (l * r) jkind_quality -> (disallowed * r) jkind_quality =
      function
      | Not_best -> Not_best
      | Best -> Best

    let disallow_right :
        type l r. (l * r) jkind_quality -> (l * disallowed) jkind_quality =
      function
      | Not_best -> Not_best
      | Best -> Best

    let allow_left :
        type l r. (allowed * r) jkind_quality -> (l * r) jkind_quality =
      function
      | Not_best -> Not_best
      | Best -> Best

    let allow_right :
        type l r. (l * allowed) jkind_quality -> (l * r) jkind_quality =
      function
      | Not_best -> Not_best
  end)

  let try_allow_r :
      type l r. (l * r) jkind_quality -> (l * allowed) jkind_quality option =
    function
    | Not_best -> Some Not_best
    | Best -> None
end

include Allowance.Magic_allow_disallow (struct
  type (_, _, 'd) sided = 'd jkind

  let disallow_right t =
    { t with
      jkind = Layout_and_axes.disallow_right t.jkind;
      quality = Quality.disallow_right t.quality
    }

  let disallow_left t =
    { t with
      jkind = Layout_and_axes.disallow_left t.jkind;
      quality = Quality.disallow_left t.quality
    }

  let allow_right t =
    { t with
      jkind = Layout_and_axes.allow_right t.jkind;
      quality = Quality.allow_right t.quality
    }

  let allow_left t =
    { t with
      jkind = Layout_and_axes.allow_left t.jkind;
      quality = Quality.allow_left t.quality
    }
end)

let try_allow_r t =
  let open Misc.Stdlib.Monad.Option.Syntax in
  let* jkind = Layout_and_axes.try_allow_r t.jkind in
  let* quality = Quality.try_allow_r t.quality in
  Some { t with jkind; quality }

let fresh_jkind jkind ~annotation ~why =
  { jkind;
    annotation;
    history = Creation why;
    has_warned = false;
    ran_out_of_fuel_during_normalize = false;
    quality = Not_best
  }
  |> allow_left |> allow_right

(* This version propagates the allowances from the [jkind] to the output. *)
let fresh_jkind_poly jkind ~annotation ~why =
  { jkind;
    annotation;
    history = Creation why;
    has_warned = false;
    ran_out_of_fuel_during_normalize = false;
    quality = Not_best
  }

(***********************)
(*** constant jkinds ***)

module Context_with_transl = struct
  type 'd t =
    | Right_jkind :
        ('l * allowed) History.annotation_context
        -> ('l * allowed) t
    | Left_jkind :
        (Parsetree.core_type -> Types.type_expr)
        * (allowed * disallowed) History.annotation_context
        -> (allowed * disallowed) t

  let get_context : type l r. (l * r) t -> (l * r) History.annotation_context =
    function
    | Right_jkind ctx -> ctx
    | Left_jkind (_, ctx) -> ctx
end

(* CR layouts v2.8: This should sometimes be for type schemes, not types
   (which print weak variables like ['_a] correctly), but this works better
   for the common case. When we re-do printing, fix. *)
let outcometree_of_type = ref (fun _ -> assert false)

let set_outcometree_of_type p = outcometree_of_type := p

let outcometree_of_modalities_new = ref (fun _ _ _ -> assert false)

let set_outcometree_of_modalities_new p = outcometree_of_modalities_new := p

module Const = struct
  type 'd t = (Layout.Const.t, 'd) Types.layout_and_axes

  include Allowance.Magic_allow_disallow (struct
    include Layout_and_axes.Allow_disallow

    type (_, _, 'd) sided = 'd t
  end)

  let max =
    Types.
      { layout = Layout.Const.max;
        mod_bounds = Mod_bounds.max;
        with_bounds = No_with_bounds
      }

  let no_with_bounds_and_equal t1 t2 =
    let open Misc.Stdlib.Monad.Option.Syntax in
    let t1_t2 =
      let* t1 = Layout_and_axes.try_allow_l t1 in
      let* t1 = Layout_and_axes.try_allow_r t1 in
      let* t2 = Layout_and_axes.try_allow_l t2 in
      let* t2 = Layout_and_axes.try_allow_r t2 in
      Some (t1, t2)
    in
    match t1_t2 with
    | Some (t1, t2) ->
      Layout.Const.equal t1.layout t2.layout
      && Mod_bounds.equal t1.mod_bounds t2.mod_bounds
    | None -> false

  module Builtin = struct
    type nonrec t =
      { jkind : (allowed * allowed) t;
        name : string
      }

    let mk_jkind ~mode_crossing ~nullability (layout : Layout.Const.t) =
      let mod_bounds =
        (match mode_crossing with
        | true -> Mod_bounds.min
        | false -> Mod_bounds.max)
        |> Mod_bounds.set_nullability nullability
      in
      { layout; mod_bounds; with_bounds = No_with_bounds }

    let any =
      { jkind = mk_jkind Any ~mode_crossing:false ~nullability:Maybe_null;
        name = "any"
      }

    let any_non_null =
      { jkind = mk_jkind Any ~mode_crossing:false ~nullability:Non_null;
        name = "any_non_null"
      }

    let value_or_null =
      { jkind =
          mk_jkind (Base Value) ~mode_crossing:false ~nullability:Maybe_null;
        name = "value_or_null"
      }

    let value =
      { jkind = mk_jkind (Base Value) ~mode_crossing:false ~nullability:Non_null;
        name = "value"
      }

    let immutable_data =
      { jkind =
          { layout = Base Value;
            mod_bounds =
              Mod_bounds.create ~locality:Locality.Const.max
                ~linearity:Linearity.Const.min
                ~portability:Portability.Const.min ~yielding:Yielding.Const.min
                ~uniqueness:Uniqueness.Const_op.max
                ~contention:Contention.Const_op.min ~externality:Externality.max
                ~nullability:Nullability.Non_null;
            with_bounds = No_with_bounds
          };
        name = "immutable_data"
      }

    let mutable_data =
      { jkind =
          { layout = Base Value;
            mod_bounds =
              Mod_bounds.create ~locality:Locality.Const.max
                ~linearity:Linearity.Const.min
                ~portability:Portability.Const.min ~yielding:Yielding.Const.min
                ~contention:Contention.Const_op.max
                ~uniqueness:Uniqueness.Const_op.max ~externality:Externality.max
                ~nullability:Nullability.Non_null;
            with_bounds = No_with_bounds
          };
        name = "mutable_data"
      }

    (* CR layouts v3: change to [or_null] when separability is implemented. *)
    let void =
      { jkind = mk_jkind (Base Void) ~mode_crossing:false ~nullability:Non_null;
        name = "void"
      }

    let immediate =
      { jkind = mk_jkind (Base Value) ~mode_crossing:true ~nullability:Non_null;
        name = "immediate"
      }

    let immediate_or_null =
      { jkind =
          mk_jkind (Base Value) ~mode_crossing:true ~nullability:Maybe_null;
        name = "immediate_or_null"
      }

    (* [immediate64] describes types that are stored directly (no indirection)
       on 64-bit platforms but indirectly on 32-bit platforms. The key question:
       along which modes should a [immediate64] cross? As of today, all of them,
       but the reasoning for each is independent and somewhat subtle:

       * Locality: This is fine, because we do not have stack-allocation on
       32-bit platforms. Thus mode-crossing is sound at any type on 32-bit,
       including immediate64 types.

       * Linearity: This is fine, because linearity matters only for function
       types, and an immediate64 cannot be a function type and cannot store
       one either.

       * Uniqueness: This is fine, because uniqueness matters only for
       in-place update, and no record supporting in-place update is an
       immediate64. ([@@unboxed] records do not support in-place update.)

       * Syncness: This is fine, because syncness matters only for function
       types, and an immediate64 cannot be a function type and cannot store
       one either.

       * Contention: This is fine, because contention matters only for
       types with mutable fields, and an immediate64 does not have immutable
       fields.

       In practice, the functor that creates immediate64s,
       [Stdlib.Sys.Immediate64.Make], will require these conditions on its
       argument. But the arguments that we expect here will have no trouble
       meeting the conditions.
    *)
    let immediate64 =
      { jkind =
          { immediate.jkind with
            mod_bounds =
              Mod_bounds.set_externality Externality.External64
                immediate.jkind.mod_bounds
          };
        name = "immediate64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float64 =
      { jkind =
          mk_jkind (Base Float64) ~mode_crossing:true ~nullability:Non_null;
        name = "float64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float32 =
      { jkind =
          mk_jkind (Base Float32) ~mode_crossing:true ~nullability:Non_null;
        name = "float32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let word =
      { jkind = mk_jkind (Base Word) ~mode_crossing:true ~nullability:Non_null;
        name = "word"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits32 =
      { jkind = mk_jkind (Base Bits32) ~mode_crossing:true ~nullability:Non_null;
        name = "bits32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits64 =
      { jkind = mk_jkind (Base Bits64) ~mode_crossing:true ~nullability:Non_null;
        name = "bits64"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let vec128 =
      { jkind = mk_jkind (Base Vec128) ~mode_crossing:true ~nullability:Non_null;
        name = "vec128"
      }

    let all =
      [ any;
        any_non_null;
        value_or_null;
        value;
        immutable_data;
        mutable_data;
        void;
        immediate;
        immediate_or_null;
        immediate64;
        float64;
        float32;
        word;
        bits32;
        bits64;
        vec128 ]

    let of_attribute : Builtin_attributes.jkind_attribute -> t = function
      | Immediate -> immediate
      | Immediate64 -> immediate64
  end

  module To_out_jkind_const : sig
    (** Convert a [t] into a [Outcometree.out_jkind_const].
        The jkind is written in terms of the built-in jkind that requires the
        least amount of modes after the mod. For example, [value mod global many
        unique portable uncontended external_ non_null] could be written in
        terms of [value] (as it appears above), or in terms of [immediate]
        (which would just be [immediate]). Since the latter requires less modes
        to be printed, it is chosen.
    *)
    val convert : 'd t -> Outcometree.out_jkind_const
  end = struct
    type printable_jkind =
      { base : string;
        modal_bounds : string list;
        printable_with_bounds :
          (Outcometree.out_type * Outcometree.out_modality_new list) list
      }

    let get_modal_bound (type a) ~(axis : a Axis.t) ~(base : a) (actual : a) =
      let (module A) = Axis.get axis in
      (* CR layouts v2.8: Fix printing! *)
      let less_or_equal a b =
        let (module Axis_ops) = Axis.get axis in
        Axis_ops.less_or_equal a b
      in
      match less_or_equal actual base with
      | Less | Equal -> (
        match less_or_equal base actual with
        | Less | Equal -> `Valid None
        | Not_le -> `Valid (Some (Format.asprintf "%a" A.print actual)))
      | Not_le -> `Invalid

    let get_modal_bounds ~(base : Mod_bounds.t) (actual : Mod_bounds.t) =
      Axis.all
      |> List.map (fun (Axis.Pack axis) ->
             let base = Mod_bounds.get ~axis base in
             let actual = Mod_bounds.get ~axis actual in
             get_modal_bound ~axis ~base actual)
      |> List.rev
      |> List.fold_left
           (fun acc mode ->
             match acc, mode with
             | _, `Invalid | None, _ -> None
             | acc, `Valid None -> acc
             | Some acc, `Valid (Some mode) -> Some (mode :: acc))
           (Some [])

    let modality_to_ignore_axes axes_to_ignore =
      (* The modality is constant along axes to ignore and id along others *)
      List.fold_left
        (fun acc (Axis.Pack axis) ->
          match axis with
          | Modal axis ->
            let then_ : Modality.t =
              let (P axis) = Mode.Const.Axis.alloc_as_value (P axis) in
              match axis with
              | Monadic monadic ->
                Atom
                  (axis, Join_with (Mode.Value.Monadic.Const.max_axis monadic))
              | Comonadic comonadic ->
                Atom
                  ( axis,
                    Meet_with (Mode.Value.Comonadic.Const.min_axis comonadic) )
            in
            Modality.Value.Const.compose acc ~then_
          | Nonmodal _ ->
            (* TODO: don't know how to print *)
            acc)
        Modality.Value.Const.id
        (Axis_set.to_list axes_to_ignore)

    (** Write [actual] in terms of [base] *)
    let convert_with_base ~(base : Builtin.t) (actual : _ t) =
      let matching_layouts =
        Layout.Const.equal base.jkind.layout actual.layout
      in
      let modal_bounds =
        get_modal_bounds ~base:base.jkind.mod_bounds actual.mod_bounds
      in
      let printable_with_bounds =
        List.map
          (fun (type_expr, type_info) ->
            let axes_ignored_by_modalities =
              With_bounds.Type_info.axes_ignored_by_modalities
                ~mod_bounds:actual.mod_bounds ~type_info
            in
            ( !outcometree_of_type type_expr,
              !outcometree_of_modalities_new
                Types.Immutable []
                (modality_to_ignore_axes axes_ignored_by_modalities) ))
          (With_bounds.to_list actual.with_bounds)
      in
      match matching_layouts, modal_bounds with
      | true, Some modal_bounds ->
        Some { base = base.name; modal_bounds; printable_with_bounds }
      | false, _ | _, None -> None

    (** Select the out_jkind_const with the least number of modal bounds to print *)
    let rec select_simplest = function
      | a :: b :: tl ->
        let simpler =
          if List.length a.modal_bounds < List.length b.modal_bounds
          then a
          else b
        in
        select_simplest (simpler :: tl)
      | [out] -> Some out
      | [] -> None

    let convert jkind =
      (* For each primitive jkind, we try to print the jkind in terms of it
         (this is possible if the primitive is a subjkind of it). We then choose
         the "simplest". The "simplest" is taken to mean the one with the least
         number of modes that need to follow the [mod]. *)
      let simplest =
        Builtin.all
        |> List.filter_map (fun base -> convert_with_base ~base jkind)
        |> select_simplest
      in
      let printable_jkind =
        match simplest with
        | Some simplest -> simplest
        | None -> (
          (* CR layouts v2.8: sometimes there is no valid way to build a jkind
             from a built-in abbreviation. For now, we just pretend that the
             layout name is a valid jkind abbreviation whose modal bounds are
             all max, even though this is a lie. *)
          let out_jkind_verbose =
            convert_with_base
              ~base:
                { jkind =
                    { layout = jkind.layout;
                      mod_bounds =
                        Mod_bounds.set_nullability Nullability.Non_null
                          Mod_bounds.max;
                      with_bounds = No_with_bounds
                    };
                  name = Layout.Const.to_string jkind.layout
                }
              jkind
          in
          match out_jkind_verbose with
          | Some out_jkind -> out_jkind
          | None ->
            (* If we fail, try again with nullable jkinds. *)
            let out_jkind_verbose =
              convert_with_base
                ~base:
                  { jkind =
                      { layout = jkind.layout;
                        mod_bounds = Mod_bounds.max;
                        with_bounds = No_with_bounds
                      };
                    name = Layout.Const.to_string jkind.layout
                  }
                jkind
            in
            (* convert_with_base is guaranteed to succeed since the layout
               matches and the modal bounds are all max *)
            Option.get out_jkind_verbose)
      in
      let base, with_tys =
        match printable_jkind with
        | { base; modal_bounds = _ :: _ as modal_bounds; printable_with_bounds }
          ->
          ( Outcometree.Ojkind_const_mod
              (Some (Ojkind_const_abbreviation base), modal_bounds),
            printable_with_bounds )
        | { base; modal_bounds = []; printable_with_bounds } ->
          Outcometree.Ojkind_const_abbreviation base, printable_with_bounds
      in
      (* Finally, add on the [with]-types and their modalities *)
      List.fold_left
        (fun jkind (ty, modalities) ->
          Outcometree.Ojkind_const_with (jkind, ty, modalities))
        base with_tys
  end

  let to_out_jkind_const jkind = To_out_jkind_const.convert jkind

  let format ppf jkind =
    To_out_jkind_const.convert jkind |> !Oprint.out_jkind_const ppf

  (*******************************)
  (* converting user annotations *)

  let jkind_of_product_annotations (type l r) (jkinds : (l * r) t list) =
    let folder (type l r) (layouts_acc, mod_bounds_acc, with_bounds_acc)
        ({ layout; mod_bounds; with_bounds } : (l * r) t) =
      ( layout :: layouts_acc,
        Mod_bounds.join mod_bounds mod_bounds_acc,
        With_bounds.join with_bounds with_bounds_acc )
    in
    let layouts, mod_bounds, with_bounds =
      List.fold_left folder ([], Mod_bounds.min, No_with_bounds) jkinds
    in
    { layout = Layout.Const.Product (List.rev layouts);
      mod_bounds;
      with_bounds
    }

  let rec of_user_written_annotation_unchecked_level :
      type l r.
      (l * r) Context_with_transl.t -> Parsetree.jkind_annotation -> (l * r) t =
   fun context jkind ->
    match jkind.pjkind_desc with
    | Abbreviation name ->
      (* CR layouts v2.8: move this to predef *)
      (match name with
      | "any" -> Builtin.any.jkind
      | "any_non_null" -> Builtin.any_non_null.jkind
      | "value_or_null" -> Builtin.value_or_null.jkind
      | "value" -> Builtin.value.jkind
      | "void" -> Builtin.void.jkind
      | "immediate64" -> Builtin.immediate64.jkind
      | "immediate" -> Builtin.immediate.jkind
      | "immediate_or_null" -> Builtin.immediate_or_null.jkind
      | "float64" -> Builtin.float64.jkind
      | "float32" -> Builtin.float32.jkind
      | "word" -> Builtin.word.jkind
      | "bits32" -> Builtin.bits32.jkind
      | "bits64" -> Builtin.bits64.jkind
      | "vec128" -> Builtin.vec128.jkind
      | "immutable_data" -> Builtin.immutable_data.jkind
      | "mutable_data" -> Builtin.mutable_data.jkind
      | _ -> raise ~loc:jkind.pjkind_loc (Unknown_jkind jkind))
      |> allow_left |> allow_right
    | Mod (base, modifiers) ->
      let base = of_user_written_annotation_unchecked_level context base in
      (* for each mode, lower the corresponding modal bound to be that mode *)
      let parsed_modifiers = Typemode.transl_modifier_annots modifiers in
      let mod_bounds =
        let value_for_axis (type a) ~(axis : a Axis.t) : a =
          let (module A) = Axis.get axis in
          let parsed_modifier =
            Typemode.Transled_modifiers.get ~axis parsed_modifiers
          in
          let base_bound = Mod_bounds.get ~axis base.mod_bounds in
          match parsed_modifier, base_bound with
          | None, base_modifier -> base_modifier
          | Some parsed_modifier, base_modifier ->
            A.meet base_modifier parsed_modifier.txt
        in
        Mod_bounds.create
          ~locality:(value_for_axis ~axis:(Modal (Comonadic Areality)))
          ~linearity:(value_for_axis ~axis:(Modal (Comonadic Linearity)))
          ~uniqueness:(value_for_axis ~axis:(Modal (Monadic Uniqueness)))
          ~portability:(value_for_axis ~axis:(Modal (Comonadic Portability)))
          ~contention:(value_for_axis ~axis:(Modal (Monadic Contention)))
          ~yielding:(value_for_axis ~axis:(Modal (Comonadic Yielding)))
          ~externality:(value_for_axis ~axis:(Nonmodal Externality))
          ~nullability:(value_for_axis ~axis:(Nonmodal Nullability))
      in
      { layout = base.layout; mod_bounds; with_bounds = No_with_bounds }
    | Product ts ->
      let jkinds =
        List.map (of_user_written_annotation_unchecked_level context) ts
      in
      jkind_of_product_annotations jkinds
    | With (base, type_, modalities) -> (
      let base = of_user_written_annotation_unchecked_level context base in
      match context with
      | Right_jkind _ -> raise ~loc:type_.ptyp_loc With_on_right
      | Left_jkind (transl_type, _) ->
        let type_ = transl_type type_ in
        let modality =
          Typemode.transl_modalities ~maturity:Stable Immutable [] modalities
        in
        { layout = base.layout;
          mod_bounds = base.mod_bounds;
          with_bounds =
            With_bounds.add_modality ~modality
              ~relevant_for_nullability:`Irrelevant ~type_expr:type_
              base.with_bounds
        })
    | Default | Kind_of _ -> raise ~loc:jkind.pjkind_loc Unimplemented_syntax

  (* The [annotation_context] parameter can be used to allow annotations / kinds
     in different contexts to be enabled with different extension settings.
     At some points in time, we will not care about the context, and so this
     parameter might effectively be unused.
  *)
  (* CR layouts: When everything is stable, remove this function. *)
  let get_required_layouts_level (_context : 'd Context_with_transl.t)
      (jkind : 'd t) =
    let rec scan_layout (l : Layout.Const.t) : Language_extension.maturity =
      match l, Mod_bounds.nullability jkind.mod_bounds with
      | (Base (Float64 | Float32 | Word | Bits32 | Bits64 | Vec128) | Any), _
      | Base Value, Non_null
      | Base Value, Maybe_null ->
        Stable
      | Product layouts, _ ->
        List.fold_left
          (fun m l -> Language_extension.Maturity.max m (scan_layout l))
          Language_extension.Stable layouts
      | Base Void, _ -> Alpha
    in
    scan_layout jkind.layout

  let of_user_written_annotation ~context (annot : Parsetree.jkind_annotation) =
    let const = of_user_written_annotation_unchecked_level context annot in
    let required_layouts_level = get_required_layouts_level context const in
    if not (Language_extension.is_at_least Layouts required_layouts_level)
    then
      raise ~loc:annot.pjkind_loc
        (Insufficient_level { jkind = annot; required_layouts_level });
    const
end

module Desc = struct
  type 'd t = (Sort.Flat.t Layout.t, 'd) layout_and_axes

  let get_const t = Layout_and_axes.map_option Layout.get_flat_const t

  (* CR layouts v2.8: This will probably need to be overhauled with
     [with]-types. See also [Printtyp.out_jkind_of_desc], which uses the same
     algorithm. *)
  let format ppf t =
    let open Format in
    let rec format_desc ~nested ppf (desc : _ t) =
      match desc.layout with
      | Sort (Var n) -> fprintf ppf "'s%d" (Sort.Var.get_print_number n)
      (* Analyze a product before calling [get_const]: the machinery in
         [Const.format] works better for atomic layouts, not products. *)
      | Product lays ->
        let pp_sep ppf () = fprintf ppf "@ & " in
        Misc.pp_nested_list ~nested ~pp_element:format_desc ~pp_sep ppf
          (List.map (fun layout -> { desc with layout }) lays)
      | _ -> (
        match get_const desc with
        | Some c -> Const.format ppf c
        | None -> assert false (* handled above *))
    in
    format_desc ~nested:false ppf t
end

module Jkind_desc = struct
  let of_const t = Layout_and_axes.map Layout.of_const t

  let add_nullability_crossing t =
    { t with
      mod_bounds = Mod_bounds.set_nullability Nullability.min t.mod_bounds
    }

  let unsafely_set_bounds t ~from =
    { t with mod_bounds = from.mod_bounds; with_bounds = from.with_bounds }

  let add_with_bounds ~relevant_for_nullability ~type_expr ~modality t =
    match Types.get_desc type_expr with
    | Tarrow (_, _, _, _) ->
      (* Optimization: all arrow types have the same (with-bound-free) jkind, so
         we can just eagerly do a join on the mod-bounds here rather than having
         to add them to our with bounds only to be normalized away later. *)
      { t with
        mod_bounds =
          Mod_bounds.join t.mod_bounds
            (Mod_bounds.set_min_in_set Mod_bounds.for_arrow
               (Axis_set.complement
                  (relevant_axes_of_modality ~modality ~relevant_for_nullability)))
      }
    | _ ->
      { t with
        with_bounds =
          With_bounds.add_modality ~relevant_for_nullability ~type_expr
            ~modality t.with_bounds
      }

  let max = of_const Const.max

  let equate_or_equal ~allow_mutation t1 t2 =
    Layout_and_axes.equal (Layout.equate_or_equal ~allow_mutation) t1 t2

  let sub (type l r) ~type_equal:_ ~jkind_of_type
      (sub : (allowed * r) jkind_desc)
      ({ layout = lay2; mod_bounds = bounds2; with_bounds = No_with_bounds } :
        (l * allowed) jkind_desc) =
    let axes_max_on_right =
      (* Optimization: if the upper_bound is max on the right, then that axis is
         irrelevant - the left will always satisfy the right along that axis. *)
      Mod_bounds.get_max_axes bounds2
    in
    let ( ({ layout = lay1; mod_bounds = bounds1; with_bounds = No_with_bounds } :
            (_ * allowed) jkind_desc),
          _ ) =
      Layout_and_axes.normalize ~skip_axes:axes_max_on_right ~mode:Ignore_best
        ~jkind_of_type sub
    in
    let layout = Layout.sub lay1 lay2 in
    let bounds = Mod_bounds.less_or_equal bounds1 bounds2 in
    Sub_result.combine layout bounds

  let intersection
      { layout = lay1; mod_bounds = mod_bounds1; with_bounds = with_bounds1 }
      { layout = lay2; mod_bounds = mod_bounds2; with_bounds = with_bounds2 } =
    match Layout.intersection lay1 lay2 with
    | None -> None
    | Some layout ->
      Some
        { layout;
          mod_bounds = Mod_bounds.meet mod_bounds1 mod_bounds2;
          with_bounds = With_bounds.meet with_bounds1 with_bounds2
        }

  let map_type_expr f t = Layout_and_axes.map_type_expr f t

  let of_new_sort_var nullability_upper_bound =
    let layout, sort = Layout.of_new_sort_var () in
    ( { layout;
        mod_bounds =
          Mod_bounds.set_nullability nullability_upper_bound Mod_bounds.max;
        with_bounds = No_with_bounds
      },
      sort )

  module Builtin = struct
    let any = max

    let value_or_null = of_const Const.Builtin.value_or_null.jkind

    let value = of_const Const.Builtin.value.jkind

    let immutable_data = of_const Const.Builtin.immutable_data.jkind

    let mutable_data = of_const Const.Builtin.mutable_data.jkind

    let void = of_const Const.Builtin.void.jkind

    let immediate = of_const Const.Builtin.immediate.jkind

    let immediate_or_null = of_const Const.Builtin.immediate_or_null.jkind
  end

  let product tys_modalities layouts =
    let layout = Layout.product layouts in
    let mod_bounds = Mod_bounds.min in
    let with_bounds =
      List.fold_right
        (fun (type_expr, modality) bounds ->
          With_bounds.add_modality ~relevant_for_nullability:`Relevant
            ~type_expr ~modality bounds)
        tys_modalities No_with_bounds
    in
    { layout; mod_bounds; with_bounds }

  let get t = Layout_and_axes.map Layout.get t

  let get_const t = Layout_and_axes.map_option Layout.get_const t

  module Debug_printers = struct
    let t ppf t =
      Layout_and_axes.debug_print
        (Layout.Debug_printers.t Sort.Debug_printers.t)
        ppf t
  end
end

(******************************)
(* constants *)

(* every context where this is used actually wants an [option] *)
let mk_annot name =
  Some Parsetree.{ pjkind_loc = Location.none; pjkind_desc = Abbreviation name }

let mark_best (type l r) (t : (l * r) Types.jkind) =
  { (disallow_right t) with quality = Best }

let is_best t = match t.quality with Best -> true | Not_best -> false

module Builtin = struct
  let any_dummy_jkind =
    { jkind = Jkind_desc.max;
      annotation = None;
      (* this should never get printed: it's a dummy *)
      history = Creation (Any_creation Dummy_jkind);
      has_warned = false;
      ran_out_of_fuel_during_normalize = false;
      quality = Not_best
    }

  (* CR layouts: Should we be doing more memoization here? *)
  let any ~(why : History.any_creation_reason) =
    match why with
    | Dummy_jkind ->
      any_dummy_jkind (* share this one common case *) |> allow_left
      |> allow_right
    | _ ->
      fresh_jkind Jkind_desc.Builtin.any ~annotation:(mk_annot "any")
        ~why:(Any_creation why)

  let value_v1_safety_check =
    { jkind = Jkind_desc.Builtin.value_or_null;
      annotation = mk_annot "value";
      history = Creation (Value_or_null_creation V1_safety_check);
      has_warned = false;
      ran_out_of_fuel_during_normalize = false;
      quality = Not_best
    }

  let void ~why =
    fresh_jkind Jkind_desc.Builtin.void ~annotation:(mk_annot "void")
      ~why:(Void_creation why)
    |> mark_best

  let value_or_null ~why =
    match (why : History.value_or_null_creation_reason) with
    | V1_safety_check -> value_v1_safety_check |> allow_left |> allow_right
    | _ ->
      fresh_jkind Jkind_desc.Builtin.value_or_null
        ~annotation:(mk_annot "value_or_null") ~why:(Value_or_null_creation why)

  let value ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Builtin.value ~annotation:(mk_annot "value")
      ~why:(Value_creation why)

  let immutable_data ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Builtin.immutable_data
      ~annotation:(mk_annot "immutable_data")
      ~why:(Value_creation why)

  let mutable_data ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Builtin.mutable_data
      ~annotation:(mk_annot "mutable_data") ~why:(Value_creation why)

  let immediate ~why =
    fresh_jkind Jkind_desc.Builtin.immediate ~annotation:(mk_annot "immediate")
      ~why:(Immediate_creation why)
    |> mark_best

  let immediate_or_null ~why =
    fresh_jkind Jkind_desc.Builtin.immediate_or_null
      ~annotation:(mk_annot "immediate_or_null")
      ~why:(Immediate_or_null_creation why)

  let product ~why tys_modalities layouts =
    let desc = Jkind_desc.product tys_modalities layouts in
    fresh_jkind_poly desc ~annotation:None ~why:(Product_creation why)
    (* [mark_best] is correct here because the with-bounds of a product jkind
       include all the components of the product. Accordingly, looking through
       the product, by one step, never loses any information. *)
    |> mark_best

  let product_of_sorts ~why arity =
    let layout =
      Layout.product
        (List.init arity (fun _ -> fst (Layout.of_new_sort_var ())))
    in
    let desc : _ jkind_desc =
      { layout; mod_bounds = Mod_bounds.max; with_bounds = No_with_bounds }
    in
    fresh_jkind_poly desc ~annotation:None ~why:(Product_creation why)
  (* We do not [mark_best] here because the resulting jkind is used (only) in
     the middle of type-checking mutually recursive type declarations. See Note
     [Default jkind in transl_declaration] for more commentary on why we don't
     want [Best] jkinds there. *)
end

let add_nullability_crossing t =
  { t with jkind = Jkind_desc.add_nullability_crossing t.jkind }

let unsafely_set_bounds (type l r) ~(from : (l * r) jkind) t =
  { t with jkind = Jkind_desc.unsafely_set_bounds t.jkind ~from:from.jkind }

let add_with_bounds ~modality ~type_expr t =
  { t with
    jkind =
      Jkind_desc.add_with_bounds
      (* We only care about types in fields of unboxed products for the
         nullability of the overall kind *)
        ~relevant_for_nullability:`Irrelevant ~type_expr ~modality t.jkind
  }

let has_with_bounds (type r) (t : (_ * r) jkind) =
  match t.jkind.with_bounds with
  | No_with_bounds -> false
  | With_bounds tys -> not (With_bounds_types.is_empty tys)

(******************************)
(* construction *)

let of_new_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Maybe_null in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_creation why), sort

let of_new_sort ~why = fst (of_new_sort_var ~why)

let of_new_legacy_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Non_null in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_legacy_creation why), sort

let of_new_legacy_sort ~why = fst (of_new_legacy_sort_var ~why)

let of_const (type l r) ~annotation ~why ~(quality : (l * r) jkind_quality)
    (c : (l * r) Const.t) =
  { jkind = Layout_and_axes.map Layout.of_const c;
    annotation;
    history = Creation why;
    has_warned = false;
    ran_out_of_fuel_during_normalize = false;
    quality
  }

let of_builtin ~why Const.Builtin.{ jkind; name } =
  jkind |> Layout_and_axes.allow_left |> Layout_and_axes.disallow_right
  |> of_const ~annotation:(mk_annot name)
       ~why
         (* The [Best] is OK here because this function is used only in
            Predef. *)
       ~quality:Best

let of_annotated_const ~context ~annotation ~const ~const_loc =
  let context = Context_with_transl.get_context context in
  of_const ~annotation
    ~why:(Annotated (context, const_loc))
    const ~quality:Not_best

let of_annotation_lr ~context (annot : Parsetree.jkind_annotation) =
  let const = Const.of_user_written_annotation ~context annot in
  of_annotated_const ~annotation:(Some annot) ~const ~const_loc:annot.pjkind_loc
    ~context

let of_annotation ~context annot =
  of_annotation_lr ~context:(Right_jkind context) annot

let of_annotation_option_default ~default ~context = function
  | None -> default
  | Some annot -> of_annotation ~context annot

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let ({ jkind = const; name } : Const.Builtin.t) =
    Const.Builtin.of_attribute attribute.txt
  in
  of_annotated_const ~context ~annotation:(mk_annot name) ~const
    ~const_loc:attribute.loc

let of_type_decl ~context ~transl_type (decl : Parsetree.type_declaration) =
  let context = Context_with_transl.Left_jkind (transl_type, context) in
  let jkind_of_annotation =
    decl.ptype_jkind_annotation
    |> Option.map (fun annot -> of_annotation_lr ~context annot, annot)
  in
  let jkind_of_attribute =
    Builtin_attributes.jkind decl.ptype_attributes
    |> Option.map (fun attr ->
           (of_attribute ~context attr |> disallow_right, None), attr)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | Some (jkind, annot), None -> Some (jkind, Some annot)
  | None, Some (jkind_with_annot, _) -> Some jkind_with_annot
  | Some (_, from_annotation), Some (_, from_attribute) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let of_type_decl_default ~context ~transl_type ~default
    (decl : Parsetree.type_declaration) =
  match of_type_decl ~context ~transl_type decl with
  | Some (t, _) -> t
  | None -> default

let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls

let all_void_labels lbls =
  List.for_all
    (fun (lbl : Types.label_declaration) -> Sort.Const.(equal void lbl.ld_sort))
    lbls

let add_labels_as_with_bounds lbls jkind =
  List.fold_right
    (fun (lbl : Types.label_declaration) ->
      add_with_bounds ~type_expr:lbl.ld_type ~modality:lbl.ld_modalities)
    lbls jkind

let for_boxed_record lbls =
  if all_void_labels lbls
  then Builtin.immediate ~why:Empty_record
  else
    let is_mutable = has_mutable_label lbls in
    let base =
      (if is_mutable then Builtin.mutable_data else Builtin.immutable_data)
        ~why:Boxed_record
      |> mark_best
    in
    add_labels_as_with_bounds lbls base

let for_unboxed_record lbls =
  let open Types in
  let tys_modalities =
    List.map (fun lbl -> lbl.ld_type, lbl.ld_modalities) lbls
  in
  let layouts =
    List.map
      (fun lbl -> lbl.ld_sort |> Layout.Const.of_sort_const |> Layout.of_const)
      lbls
  in
  Builtin.product ~why:Unboxed_record tys_modalities layouts

let for_boxed_variant cstrs =
  let open Types in
  if List.for_all
       (fun cstr ->
         match cstr.cd_args with
         | Cstr_tuple args ->
           List.for_all (fun arg -> Sort.Const.(equal void arg.ca_sort)) args
         | Cstr_record lbls -> all_void_labels lbls)
       cstrs
  then Builtin.immediate ~why:Enumeration
  else
    let is_mutable =
      List.exists
        (fun cstr ->
          match cstr.cd_args with
          | Cstr_tuple _ -> false
          | Cstr_record lbls -> has_mutable_label lbls)
        cstrs
    in
    let has_gadt_constructor =
      List.exists
        (fun cstr -> match cstr.cd_res with None -> false | Some _ -> true)
        cstrs
    in
    if has_gadt_constructor
       (* CR layouts v2.8: This is sad, but I don't know how to account for
          existentials in the with_bounds. See doc named "Existential
          with_bounds". *)
    then Builtin.value ~why:Boxed_variant
    else
      let base =
        (if is_mutable then Builtin.mutable_data else Builtin.immutable_data)
          ~why:Boxed_variant
        |> mark_best
      in
      let add_cstr_args cstr jkind =
        match cstr.cd_args with
        | Cstr_tuple args ->
          List.fold_right
            (fun arg ->
              add_with_bounds ~modality:arg.ca_modalities ~type_expr:arg.ca_type)
            args jkind
        | Cstr_record lbls -> add_labels_as_with_bounds lbls jkind
      in
      List.fold_right add_cstr_args cstrs base

let for_boxed_tuple elts =
  List.fold_right
    (fun (_, type_expr) ->
      add_with_bounds ~modality:Mode.Modality.Value.Const.id ~type_expr)
    elts
    (Builtin.immutable_data ~why:Tuple |> mark_best)

let for_arrow =
  fresh_jkind
    { layout = Sort (Base Value);
      mod_bounds = Mod_bounds.for_arrow;
      with_bounds = No_with_bounds
    }
    ~annotation:None ~why:(Value_creation Arrow)
  |> mark_best

let for_object =
  (* The crossing of objects are based on the fact that they are
     produced/defined/allocated at legacy, which applies to only the
     comonadic axes. *)
  let ({ linearity; areality = locality; portability; yielding }
        : Mode.Alloc.Comonadic.Const.t) =
    Alloc.Comonadic.Const.legacy
  in
  let ({ contention; uniqueness } : Mode.Alloc.Monadic.Const_op.t) =
    Alloc.Monadic.Const_op.max
  in
  fresh_jkind
    { layout = Sort (Base Value);
      mod_bounds =
        Mod_bounds.create ~linearity ~locality ~uniqueness ~portability
          ~contention ~yielding ~externality:Externality.max
          ~nullability:Non_null;
      with_bounds = No_with_bounds
    }
    ~annotation:None ~why:(Value_creation Object)

(******************************)
(* elimination and defaulting *)

type normalize_mode =
  | Require_best
  | Ignore_best

let[@inline] normalize ~mode ~jkind_of_type t =
  let mode : _ Layout_and_axes.normalize_mode =
    match mode with Require_best -> Require_best | Ignore_best -> Ignore_best
  in
  let jkind, fuel_result =
    Layout_and_axes.normalize ~jkind_of_type ~skip_axes:Axis_set.empty ~mode
      t.jkind
  in
  { t with
    jkind;
    quality =
      (match t.quality, fuel_result with
      | Not_best, _ | _, Ran_out_of_fuel -> Not_best
      | Best, Sufficient_fuel -> Best);
    ran_out_of_fuel_during_normalize =
      (match fuel_result with
      | Ran_out_of_fuel -> true
      | _ -> t.ran_out_of_fuel_during_normalize)
  }

let get_layout_defaulting_to_value { jkind = { layout; _ }; _ } =
  Layout.default_to_value_and_get layout

let default_to_value t = ignore (get_layout_defaulting_to_value t)

let get t = Jkind_desc.get t.jkind

let get_const t = Jkind_desc.get_const t.jkind

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let sort_of_jkind (t : jkind_l) : sort =
  let rec sort_of_layout (t : _ Layout.t) =
    match t with
    | Any -> Misc.fatal_error "Jkind.sort_of_jkind"
    | Sort s -> s
    | Product ls -> Sort.Product (List.map sort_of_layout ls)
  in
  sort_of_layout t.jkind.layout

let get_layout jk : Layout.Const.t option = Layout.get_const jk.jkind.layout

let extract_layout jk = jk.jkind.layout

let get_modal_bounds (type l r) ~jkind_of_type (jk : (l * r) jkind) =
  let ( ({ layout = _; mod_bounds; with_bounds = No_with_bounds } :
          (_ * allowed) jkind_desc),
        _ ) =
    Layout_and_axes.normalize ~mode:Ignore_best
      ~skip_axes:Axis_set.all_nonmodal_axes ~jkind_of_type jk.jkind
  in
  Mod_bounds.
    { comonadic =
        { areality = locality mod_bounds;
          linearity = linearity mod_bounds;
          portability = portability mod_bounds;
          yielding = yielding mod_bounds
        };
      monadic =
        { uniqueness = uniqueness mod_bounds;
          contention = contention mod_bounds
        }
    }

let get_mode_crossing (type l r) ~jkind_of_type (jk : (l * r) jkind) =
  let bounds = get_modal_bounds ~jkind_of_type jk in
  Mode.Crossing.of_bounds bounds

let to_unsafe_mode_crossing jkind =
  { unsafe_mod_bounds = Mod_bounds.to_mode_crossing jkind.jkind.mod_bounds;
    unsafe_with_bounds = jkind.jkind.with_bounds
  }

let all_except_externality =
  Axis_set.singleton (Nonmodal Externality) |> Axis_set.complement

let get_externality_upper_bound ~jkind_of_type jk =
  let ( ({ layout = _; mod_bounds; with_bounds = No_with_bounds } :
          (_ * allowed) jkind_desc),
        _ ) =
    Layout_and_axes.normalize ~mode:Ignore_best
      ~skip_axes:all_except_externality ~jkind_of_type jk.jkind
  in
  Mod_bounds.get mod_bounds ~axis:(Nonmodal Externality)

let set_externality_upper_bound jk externality_upper_bound =
  { jk with
    jkind =
      { jk.jkind with
        mod_bounds =
          Mod_bounds.set_externality externality_upper_bound jk.jkind.mod_bounds
      }
  }

let all_except_nullability =
  Axis_set.singleton (Nonmodal Nullability) |> Axis_set.complement

let get_nullability ~jkind_of_type jk =
  (* Optimization: Usually, no with-bounds are relevant to nullability. If we check for
     this case, we can avoid calling normalize. *)
  let all_with_bounds_are_irrelevant =
    jk.jkind.with_bounds
    |> With_bounds.for_all
         (fun _ ({ relevant_axes } : With_bounds_type_info.t) ->
           not (Axis_set.mem relevant_axes (Nonmodal Nullability)))
  in
  if all_with_bounds_are_irrelevant
  then Mod_bounds.nullability jk.jkind.mod_bounds
  else
    let ( ({ layout = _; mod_bounds; with_bounds = No_with_bounds } :
            (_ * allowed) jkind_desc),
          _ ) =
      Layout_and_axes.normalize ~mode:Ignore_best ~jkind_of_type
        ~skip_axes:all_except_nullability jk.jkind
    in
    Mod_bounds.get mod_bounds ~axis:(Nonmodal Nullability)

let set_nullability_upper_bound jk nullability_upper_bound =
  let new_bounds =
    Jkind_mod_bounds.set_nullability nullability_upper_bound jk.jkind.mod_bounds
  in
  { jk with jkind = { jk.jkind with mod_bounds = new_bounds } }

let set_layout jk layout = { jk with jkind = { jk.jkind with layout } }

let apply_modality_l modality jk =
  let relevant_axes =
    relevant_axes_of_modality ~modality ~relevant_for_nullability:`Relevant
  in
  let mod_bounds =
    Mod_bounds.set_min_in_set jk.jkind.mod_bounds
      (Axis_set.complement relevant_axes)
  in
  let with_bounds =
    With_bounds.map
      (fun ti ->
        { relevant_axes = Axis_set.intersection ti.relevant_axes relevant_axes })
      jk.jkind.with_bounds
  in
  { jk with jkind = { jk.jkind with mod_bounds; with_bounds } }
  |> disallow_right

let apply_modality_r modality jk =
  let relevant_axes =
    relevant_axes_of_modality ~modality ~relevant_for_nullability:`Relevant
  in
  let mod_bounds =
    Mod_bounds.set_max_in_set jk.jkind.mod_bounds
      (Axis_set.complement relevant_axes)
  in
  { jk with jkind = { jk.jkind with mod_bounds } } |> disallow_left

let get_annotation jk = jk.annotation

let decompose_product ({ jkind; _ } as jk) =
  let mk_jkind layout = { jk with jkind = { jkind with layout } } in
  let deal_with_sort : Sort.t -> _ = function
    | Var _ -> None (* we've called [get] and there's *still* a variable *)
    | Base _ -> None
    | Product sorts -> Some (List.map (fun sort -> mk_jkind (Sort sort)) sorts)
  in
  match jkind.layout with
  | Any -> None
  | Product layouts ->
    (* CR layouts v7.1: The histories here are wrong (we are giving each
       component the history of the whole product).  They don't show up in
       errors, so it's fine for now, but we'll probably need to fix this as
       part of improving errors around products. A couple options: re-work the
       relevant bits of [Ctype.type_jkind_sub] to just work on layouts, or
       introduce product histories. *)
    Some (List.map mk_jkind layouts)
  | Sort s -> deal_with_sort (Sort.get s)

(*********************************)
(* pretty printing *)

(* CR layouts v2.8: This is the spot where we could print the annotation in
   the jkind, if there is one. But actually the output seems better without
   doing so, because it teaches the user that e.g. [value mod local] is better
   off spelled [value]. Possibly remove [jkind.annotation], but only after
   we have a proper printing story. *)
let format ppf jkind = Desc.format ppf (Jkind_desc.get jkind.jkind)

let printtyp_path = ref (fun _ _ -> assert false)

let set_printtyp_path f = printtyp_path := f

module Report_missing_cmi : sig
  (* used both in format_history and in Violation.report_general *)
  val report_missing_cmi : Format.formatter -> Path.t option -> unit
end = struct
  open Format

  (* CR layouts: Remove this horrible (but useful) heuristic once we have
     transitive dependencies in jenga. *)
  let missing_cmi_hint ppf type_path =
    let root_module_name p = p |> Path.head |> Ident.name in
    let delete_trailing_double_underscore s =
      if Misc.Stdlib.String.ends_with ~suffix:"__" s
      then String.sub s 0 (String.length s - 2)
      else s
    in
    (* A heuristic for guessing at a plausible library name for an identifier
       with a missing .cmi file; definitely less likely to be right outside of
       Jane Street. *)
    let guess_library_name : Path.t -> string option = function
      | Pdot _ as p ->
        Some
          (match root_module_name p with
          | "Location" | "Longident" -> "ocamlcommon"
          | mn ->
            mn |> String.lowercase_ascii |> delete_trailing_double_underscore)
      | Pident _ | Papply _ | Pextra_ty _ -> None
    in
    Option.iter
      (fprintf ppf "@,Hint: Adding \"%s\" to your dependencies might help.")
      (guess_library_name type_path)

  let report_missing_cmi ppf = function
    | Some p ->
      fprintf ppf "@,@[No .cmi file found containing %a.%a@]" !printtyp_path p
        missing_cmi_hint p
    | None -> ()
end

include Report_missing_cmi

(* CR layouts: should this be configurable? In the meantime, you
   may want to change these to experiment / debug. *)

(* should we print histories at all? *)
let display_histories = true

(* should we print histories in a way users can understand?
   The alternative is to print out all the data, which may be useful
   during debugging. *)
let flattened_histories = true

(* This module is just to keep all the helper functions more locally
   scoped. *)
module Format_history = struct
  (* CR layouts: all the output in this section is subject to change;
     actually look closely at error messages once this is activated *)

  open Format

  let format_with_notify_js ppf str =
    fprintf ppf
      "@[%s.@ Please notify the Jane Street compilers group if you see this \
       output@]"
      str

  let format_position ~arity position =
    let to_ordinal num = Int.to_string num ^ Misc.ordinal_suffix num in
    match arity with 1 -> "" | _ -> to_ordinal position ^ " "

  let format_concrete_creation_reason ppf :
      History.concrete_creation_reason -> unit = function
    | Match -> fprintf ppf "a value of this type is matched against a pattern"
    | Constructor_declaration _ ->
      fprintf ppf "it's the type of a constructor field"
    | Label_declaration lbl ->
      fprintf ppf "it is the type of record field %s" (Ident.name lbl)
    | Record_projection ->
      fprintf ppf "it's the record type used in a projection"
    | Record_assignment ->
      fprintf ppf "it's the record type used in an assignment"
    | Record_functional_update ->
      fprintf ppf "it's the record type used in a functional update"
    | Let_binding -> fprintf ppf "it's the type of a variable bound by a `let`"
    | Function_argument ->
      fprintf ppf "we must know concretely how to pass a function argument"
    | Function_result ->
      fprintf ppf "we must know concretely how to return a function result"
    | Structure_item_expression ->
      fprintf ppf "it's the type of an expression in a structure"
    | External_argument ->
      fprintf ppf "it's the type of an argument in an external declaration"
    | External_result ->
      fprintf ppf "it's the type of the result of an external declaration"
    | Statement -> fprintf ppf "it's the type of a statement"
    | Optional_arg_default ->
      fprintf ppf "it's the type of an optional argument default"
    | Unboxed_tuple_element ->
      fprintf ppf "it's the type of unboxed tuple element"
    | Layout_poly_in_external ->
      fprintf ppf
        "it's the layout polymorphic type in an external declaration@ \
         ([@@layout_poly] forces all variables of layout 'any' to be@ \
         representable at call sites)"
    | Peek_or_poke ->
      fprintf ppf "it's the type being used for a peek or poke primitive"

  let format_concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "it instantiates an unannotated type parameter of %a"
        !printtyp_path path
    | Wildcard -> fprintf ppf "it's a _ in the type"
    | Unification_var -> fprintf ppf "it's a fresh unification variable"
    | Array_element -> fprintf ppf "it's the type of an array element"
    | Old_style_unboxed_type -> fprintf ppf "it's an [@@@@unboxed] type"

  let rec format_annotation_context :
      type l r. _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p ->
      fprintf ppf "the declaration of the type %a" !printtyp_path p
    | Type_parameter (path, var) ->
      let var_string = match var with None -> "_" | Some v -> "'" ^ v in
      fprintf ppf "@[%s@ in the declaration of the type@ %a@]" var_string
        !printtyp_path path
    | Newtype_declaration name ->
      fprintf ppf "the abstract type declaration for %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "@[%s@ in the declaration of constructor@ %a@]" name
        !printtyp_path cstr
    | Existential_unpack name -> fprintf ppf "the existential variable %s" name
    | Univar name -> fprintf ppf "the universal variable %s" name
    | Type_variable name -> fprintf ppf "the type variable %s" name
    | Type_wildcard loc ->
      fprintf ppf "the wildcard _ at %a" Location.print_loc_in_lowercase loc
    | With_error_message (_message, context) ->
      (* message gets printed in [format_flattened_history] so we ignore it here *)
      format_annotation_context ppf context

  let format_any_creation_reason ppf : History.any_creation_reason -> unit =
    function
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Initial_typedecl_env ->
      format_with_notify_js ppf
        "a dummy kind of any is used to check mutually recursive datatypes"
    | Wildcard -> format_with_notify_js ppf "there's a _ in the type"
    | Unification_var ->
      format_with_notify_js ppf "it's a fresh unification variable"
    | Dummy_jkind ->
      format_with_notify_js ppf
        "it's assigned a dummy kind that should have been overwritten"
    (* CR layouts: Improve output or remove this constructor ^^ *)
    | Type_expression_call ->
      format_with_notify_js ppf
        "there's a call to [type_expression] via the ocaml API"
    | Inside_of_Tarrow -> fprintf ppf "argument or result of a function type"
    | Array_type_argument ->
      fprintf ppf "it's the type argument to the array type"

  let format_immediate_creation_reason ppf :
      History.immediate_creation_reason -> _ = function
    | Empty_record ->
      fprintf ppf "it's a record type containing all void elements"
    | Enumeration ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"
    | Primitive id ->
      fprintf ppf "it is the primitive immediate type %s" (Ident.name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"

  let format_immediate_or_null_creation_reason ppf :
      History.immediate_or_null_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive immediate_or_null type %s"
        (Ident.name id)

  let format_value_or_null_creation_reason ppf ~layout_or_kind :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive value_or_null type %s" (Ident.name id)
    | Tuple_element -> fprintf ppf "it's the type of a tuple element"
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"
    | Polymorphic_variant_field ->
      fprintf ppf "it's the type of the field of a polymorphic variant"
    | Structure_element ->
      fprintf ppf "it's the type of something stored in a module structure"
    | V1_safety_check ->
      fprintf ppf "it has to be value for the V1 safety check"
    | Probe -> format_with_notify_js ppf "it's a probe"
    | Captured_in_object ->
      fprintf ppf "it's the type of a variable captured in an object"
    | Let_rec_variable v ->
      fprintf ppf "it's the type of the recursive variable %s" (Ident.name v)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s value_or_null"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind

  let format_value_creation_reason ppf ~layout_or_kind :
      History.value_creation_reason -> _ = function
    | Class_let_binding ->
      fprintf ppf "it's the type of a let-bound variable in a class expression"
    | Object -> fprintf ppf "it's the type of an object"
    | Instance_variable -> fprintf ppf "it's the type of an instance variable"
    | Object_field -> fprintf ppf "it's the type of an object field"
    | Class_field -> fprintf ppf "it's the type of a class field"
    | Boxed_record -> fprintf ppf "it's a boxed record type"
    | Boxed_variant -> fprintf ppf "it's a boxed variant type"
    | Extensible_variant -> fprintf ppf "it's an extensible variant type"
    | Primitive id ->
      fprintf ppf "it is the primitive value type %s" (Ident.name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s value"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Tuple -> fprintf ppf "it's a tuple type"
    | Row_variable -> format_with_notify_js ppf "it's a row variable"
    | Polymorphic_variant -> fprintf ppf "it's a polymorphic variant type"
    | Arrow -> fprintf ppf "it's a function type"
    | Tfield ->
      format_with_notify_js ppf
        "it's an internal Tfield type (you shouldn't see this)"
    | Tnil ->
      format_with_notify_js ppf
        "it's an internal Tnil type (you shouldn't see this)"
    | First_class_module -> fprintf ppf "it's a first-class module type"
    | Univar ->
      fprintf ppf "it is or unifies with an unannotated universal variable"
    | Default_type_jkind ->
      fprintf ppf "an abstract type has the value %s by default" layout_or_kind
    | Existential_type_variable ->
      fprintf ppf "it's an unannotated existential type variable"
    | Array_comprehension_element ->
      fprintf ppf "it's the element type of array comprehension"
    | Lazy_expression -> fprintf ppf "it's the type of a lazy expression"
    | Class_type_argument ->
      fprintf ppf "it's a type argument to a class constructor"
    | Class_term_argument ->
      fprintf ppf
        "it's the type of a term-level argument to a class constructor"
    | Debug_printer_argument ->
      format_with_notify_js ppf
        "it's the type of an argument to a debugger printer function"
    | Recmod_fun_arg ->
      fprintf ppf
        "it's the type of the first argument to a function in a recursive \
         module"
    | Unknown s ->
      fprintf ppf
        "unknown @[(please alert the Jane Street@;\
         compilers team with this message: %s)@]" s

  let format_product_creation_reason ppf : History.product_creation_reason -> _
      = function
    | Unboxed_tuple -> fprintf ppf "it is an unboxed tuple"
    | Unboxed_record -> fprintf ppf "it is an unboxed record"

  let format_creation_reason ppf ~layout_or_kind :
      History.creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Any_creation any -> format_any_creation_reason ppf any
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Immediate_or_null_creation immediate ->
      format_immediate_or_null_creation_reason ppf immediate
    | Void_creation _ -> .
    | Value_or_null_creation value ->
      format_value_or_null_creation_reason ppf value ~layout_or_kind
    | Value_creation value ->
      format_value_creation_reason ppf ~layout_or_kind value
    | Product_creation product -> format_product_creation_reason ppf product
    | Concrete_creation concrete -> format_concrete_creation_reason ppf concrete
    | Concrete_legacy_creation concrete ->
      format_concrete_legacy_creation_reason ppf concrete
    | Primitive id -> fprintf ppf "it is the primitive type %s" (Ident.name id)
    | Unboxed_primitive id ->
      fprintf ppf "it is the unboxed version of the primitive type %s"
        (Ident.name id)
    | Imported ->
      fprintf ppf "of %s requirements from an imported definition"
        layout_or_kind
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has this %s"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Generalized (id, loc) ->
      let format_id ppf = function
        | Some id -> fprintf ppf " of %s" (Ident.name id)
        | None -> ()
      in
      fprintf ppf "of the definition%a at %a" format_id id
        Location.print_loc_in_lowercase loc

  let format_interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation name ->
      fprintf ppf "a GADT match refining the type %a" !printtyp_path name
    | Tyvar_refinement_intersection -> fprintf ppf "updating a type variable"
    | Subjkind -> fprintf ppf "subkind check"

  (* CR layouts: An older implementation of format_flattened_history existed
      which displays more information not limited to one layout and one creation_reason
      around commit 66a832d70bf61d9af3b0ec6f781dcf0a188b324d in main.

      Consider revisiting that if the current implementation becomes insufficient. *)

  let format_flattened_history ~intro ~layout_or_kind ppf t =
    let jkind_desc = Jkind_desc.get t.jkind in
    fprintf ppf "@[<v 2>%t" intro;
    (match t.history with
    | Creation reason ->
      if History.is_informative t
      then (
        fprintf ppf "@ because %a"
          (format_creation_reason ~layout_or_kind)
          reason;
        match reason, Desc.get_const jkind_desc with
        | Concrete_legacy_creation _, Some _ ->
          fprintf ppf ",@ chosen to have %s %a" layout_or_kind format t
        | _ -> ())
    | Interact _ ->
      Misc.fatal_error "Non-flat history in format_flattened_history");
    fprintf ppf ".";
    (match t.history with
    | Creation (Annotated (With_error_message (message, _), _)) ->
      fprintf ppf "@ @[%s@]" message
    | _ -> ());
    fprintf ppf "@]"

  (* this isn't really formatted for user consumption *)
  let format_history_tree ~intro ~layout_or_kind ppf t =
    let rec in_order ppf = function
      | Interact { reason; history1; history2; jkind1 = _; jkind2 = _ } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]" in_order history1
          format_interact_reason reason in_order history2
      | Creation c -> format_creation_reason ppf ~layout_or_kind c
    in
    fprintf ppf "@;%t has this %s history:@;@[<v 2>  %a@]" intro layout_or_kind
      in_order t.history

  let format_history ~intro ~layout_or_kind ppf t =
    if display_histories
    then
      if flattened_histories
      then format_flattened_history ~intro ~layout_or_kind ppf t
      else format_history_tree ~intro ~layout_or_kind ppf t
end

let format_history ~intro ppf t =
  Format_history.format_history ~intro ~layout_or_kind:"kind" ppf t

(******************************)
(* errors *)

module Violation = struct
  open Format
  module Sub_failure_reason = Sub_failure_reason

  type violation =
    | Not_a_subjkind :
        (allowed * 'r1) jkind * ('l * 'r2) jkind * Sub_failure_reason.t list
        -> violation
    | No_intersection : 'd jkind * ('l * allowed) jkind -> violation

  type nonrec t =
    { violation : violation;
      missing_cmi : Path.t option
    }
  (* [missing_cmi]: is this error a result of a missing cmi file?
     This is stored separately from the [violation] because it's
     used to change the behavior of [value_kind], and we don't
     want that function to inspect something that is purely about
     the choice of error message. (Though the [Path.t] payload *is*
     indeed just about the payload.) *)

  let of_ ~jkind_of_type ?missing_cmi violation =
    (* Normalize for better printing *)
    let violation =
      match violation with
      | Not_a_subjkind (jkind1, jkind2, reasons) ->
        let jkind1 =
          normalize ~mode:Require_best ~jkind_of_type (disallow_right jkind1)
        in
        let jkind2 =
          normalize ~mode:Require_best ~jkind_of_type (disallow_right jkind2)
        in
        Not_a_subjkind (jkind1, jkind2, reasons)
      | No_intersection (jkind1, jkind2) ->
        let jkind1 =
          normalize ~mode:Require_best ~jkind_of_type (disallow_right jkind1)
        in
        (* jkind2 can't have with-bounds, by its type *)
        No_intersection (jkind1, jkind2)
    in
    { violation; missing_cmi }

  let is_missing_cmi viol = Option.is_some viol.missing_cmi

  type locale =
    | Mode
    | Layout

  let report_reason ppf violation =
    (* Print out per-axis information about why the error occurred. This only
       happens when modalities are printed because the errors are simple enough
       when there are no modalities that it makes the error unnecessarily noisy.
    *)
    match violation with
    | Not_a_subjkind (sub, super, reasons) -> (
      let disagreeing_axes =
        (* Collect all the axes that disagree into a set. If none disagree,
           then it is [None] *)
        List.fold_left
          (fun disagreeing_axes_so_far reason ->
            match (reason : Sub_failure_reason.t), disagreeing_axes_so_far with
            | Axis_disagreement (Pack axis), Some disagreeing_axes_so_far ->
              Some (Axis_set.add disagreeing_axes_so_far axis)
            | Axis_disagreement (Pack axis), None ->
              Some (Axis_set.singleton axis)
            | (Layout_disagreement | Constrain_ran_out_of_fuel), _ ->
              disagreeing_axes_so_far)
          None reasons
      in
      let has_modalities =
        let jkind_has_modalities jkind =
          List.exists
            (fun (_, type_info) ->
              let axes_ignored_by_modalities =
                With_bounds.Type_info.axes_ignored_by_modalities
                  ~mod_bounds:jkind.jkind.mod_bounds ~type_info
              in
              not (Axis_set.is_empty axes_ignored_by_modalities))
            (With_bounds.to_list jkind.jkind.with_bounds)
        in
        jkind_has_modalities sub || jkind_has_modalities super
      in
      match disagreeing_axes, has_modalities with
      | None, _ | _, false -> ()
      | Some disagreeing_axes, true ->
        (* CR: @\n is discouraged by the documentation, but @;@; seems to emit
           one newline and then one space rather than two newlines *)
        fprintf ppf "@\n@\nThe first mode-crosses less than the second along:";
        Axis_set.to_list disagreeing_axes
        |> List.iter (fun (Pack axis : Axis.packed) ->
               let pp_bound ppf jkind =
                 let mod_bound = Mod_bounds.get ~axis jkind.mod_bounds in
                 let (module Axis_ops) = Axis.get axis in
                 let with_bounds =
                   match Axis_ops.(le max mod_bound) with
                   | true ->
                     (* If the mod_bound is max, then no with-bounds are
                        relevant *)
                     []
                   | false ->
                     With_bounds.to_list jkind.with_bounds
                     |> List.filter_map
                          (fun
                            (ty, ({ relevant_axes } : With_bounds_type_info.t))
                          ->
                            match Axis_set.mem relevant_axes axis with
                            | true -> Some (!outcometree_of_type ty)
                            | false -> None)
                 in
                 let ojkind =
                   List.fold_left
                     (fun acc with_bound ->
                       Outcometree.Ojkind_const_with (acc, with_bound, []))
                     (Outcometree.Ojkind_const_mod
                        (None, [Format.asprintf "%a" Axis_ops.print mod_bound]))
                     with_bounds
                 in
                 !Oprint.out_jkind_const ppf ojkind
               in
               fprintf ppf "@;  @[<hov 2>%s:@ %a ≰@ %a@]" (Axis.name axis)
                 pp_bound sub.jkind pp_bound super.jkind))
    | No_intersection _ -> ()

  let report_fuel ppf violation =
    let report_fuel_for_type which =
      fprintf ppf
        "@;\
         @[Note: I gave up trying to find the simplest kind for the %s,@,\
         as it is very large or deeply recursive.@]" which
    in
    let first_ran_out, second_ran_out =
      match violation with
      | Not_a_subjkind (k1, k2, _) ->
        k1.ran_out_of_fuel_during_normalize, k2.ran_out_of_fuel_during_normalize
      | No_intersection (k1, k2) ->
        k1.ran_out_of_fuel_during_normalize, k2.ran_out_of_fuel_during_normalize
    in
    if first_ran_out then report_fuel_for_type "first";
    if second_ran_out then report_fuel_for_type "second"

  let report_general preamble pp_former former ppf t =
    let mismatch_type =
      match t.violation with
      | Not_a_subjkind (k1, k2, _) ->
        if Sub_result.is_le (Layout.sub k1.jkind.layout k2.jkind.layout)
        then Mode
        else Layout
      | No_intersection _ -> Layout
    in
    let layout_or_kind =
      match mismatch_type with Mode -> "kind" | Layout -> "layout"
    in
    let rec has_sort_var : Sort.Flat.t Layout.t -> bool = function
      | Sort (Var _) -> true
      | Product layouts -> List.exists has_sort_var layouts
      | Sort (Base _) | Any -> false
    in
    let format_layout_or_kind ppf jkind =
      match mismatch_type with
      | Mode -> Format.fprintf ppf "@,%a" format jkind
      | Layout -> Layout.format ppf jkind.jkind.layout
    in
    let subjkind_format verb k2 =
      if has_sort_var (get k2).layout
      then dprintf "%s representable" verb
      else
        dprintf "%s a sub%s of %a" verb layout_or_kind format_layout_or_kind k2
    in
    let Pack_jkind k1, Pack_jkind k2, fmt_k1, fmt_k2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (k1, k2, _); missing_cmi } -> (
        let missing_cmi =
          match missing_cmi with
          | None -> (
            match k1.history with
            | Creation (Missing_cmi p) -> Some p
            | Creation (Any_creation (Missing_cmi p)) -> Some p
            | _ -> None)
          | Some _ -> missing_cmi
        in
        match missing_cmi with
        | None ->
          ( Pack_jkind k1,
            Pack_jkind k2,
            dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
            subjkind_format "is not" k2,
            None )
        | Some p ->
          ( Pack_jkind k1,
            Pack_jkind k2,
            dprintf "an unknown %s" layout_or_kind,
            subjkind_format "might not be" k2,
            Some p ))
      | { violation = No_intersection (k1, k2); missing_cmi } ->
        assert (Option.is_none missing_cmi);
        ( Pack_jkind k1,
          Pack_jkind k2,
          dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
          dprintf "does not overlap with %a" format_layout_or_kind k2,
          None )
    in
    if display_histories
    then
      let connective =
        match t.violation, has_sort_var (get k2).layout with
        | Not_a_subjkind _, false ->
          dprintf "be a sub%s of %a" layout_or_kind format_layout_or_kind k2
        | No_intersection _, false ->
          dprintf "overlap with %a" format_layout_or_kind k2
        | _, true -> dprintf "be representable"
      in
      fprintf ppf "@[<v>%a@;%a@]"
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>The %s of %a is %a@]" layout_or_kind pp_former
                former format_layout_or_kind k1)
           ~layout_or_kind)
        k1
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>But the %s of %a must %t@]" layout_or_kind
                pp_former former connective)
           ~layout_or_kind)
        k2
    else
      fprintf ppf "@[<hov 2>%s%a has %t,@ which %t.@]" preamble pp_former former
        fmt_k1 fmt_k2;
    report_missing_cmi ppf missing_cmi_option;
    report_reason ppf t.violation;
    report_fuel ppf t.violation

  let pp_t ppf x = fprintf ppf "%t" x

  let report_with_offender ~offender = report_general "" pp_t offender

  let report_with_offender_sort ~offender =
    report_general "A representable layout was expected, but " pp_t offender

  let report_with_name ~name = report_general "" pp_print_string name
end

(******************************)
(* relations *)

let equate_or_equal ~allow_mutation
    { jkind = jkind1;
      annotation = _;
      history = _;
      has_warned = _;
      ran_out_of_fuel_during_normalize = _;
      quality = _
    }
    { jkind = jkind2;
      annotation = _;
      history = _;
      has_warned = _;
      ran_out_of_fuel_during_normalize = _;
      quality = _
    } =
  Jkind_desc.equate_or_equal ~allow_mutation jkind1 jkind2

(* CR layouts v2.8: Switch this back to ~allow_mutation:false *)
let equal t1 t2 = equate_or_equal ~allow_mutation:true t1 t2

let equate t1 t2 = equate_or_equal ~allow_mutation:true t1 t2

(* Not all jkind history reasons are created equal. Some are more helpful than
   others.  This function encodes that information.

    The reason with higher score should get preserved when combined with one of
    lower score. *)
let score_reason = function
  (* error_message annotated by the user should always take priority *)
  | Creation (Annotated (With_error_message _, _)) -> 1
  (* Concrete creation is quite vague, prefer more specific reasons *)
  | Creation (Concrete_creation _ | Concrete_legacy_creation _) -> -1
  | _ -> 0

let combine_histories ~type_equal ~jkind_of_type reason (Pack_jkind k1)
    (Pack_jkind k2) =
  if flattened_histories
  then
    let choose_higher_scored_history history_a history_b =
      if score_reason history_a >= score_reason history_b
      then history_a
      else history_b
    in
    let choose_subjkind_history k_a history_a k_b history_b =
      match Jkind_desc.sub ~type_equal ~jkind_of_type k_a k_b with
      | Less -> history_a
      | Not_le _ ->
        (* CR layouts: this will be wrong if we ever have a non-trivial meet in
           the kind lattice -- which is now! So this is actually wrong. *)
        history_b
      | Equal -> choose_higher_scored_history history_a history_b
    in
    match Layout_and_axes.(try_allow_l k1.jkind, try_allow_r k2.jkind) with
    | Some k1_l, Some k2_r ->
      choose_subjkind_history k1_l k1.history k2_r k2.history
    | _ -> (
      match Layout_and_axes.(try_allow_r k1.jkind, try_allow_l k2.jkind) with
      | Some k1_r, Some k2_l ->
        choose_subjkind_history k2_l k2.history k1_r k1.history
      | _ -> choose_higher_scored_history k1.history k2.history)
  else
    Interact
      { reason;
        jkind1 = Pack_jkind_desc k1.jkind;
        history1 = k1.history;
        jkind2 = Pack_jkind_desc k2.jkind;
        history2 = k2.history
      }

let has_intersection t1 t2 =
  (* Need to check only the layouts: all the axes have bottom elements. *)
  Option.is_some (Layout.intersection t1.jkind.layout t2.jkind.layout)

let intersection_or_error ~type_equal ~jkind_of_type ~reason t1 t2 =
  match Jkind_desc.intersection t1.jkind t2.jkind with
  | None -> Error (Violation.of_ ~jkind_of_type (No_intersection (t1, t2)))
  | Some jkind ->
    Ok
      { jkind;
        annotation = None;
        history =
          combine_histories ~type_equal ~jkind_of_type reason (Pack_jkind t1)
            (Pack_jkind t2);
        has_warned = t1.has_warned || t2.has_warned;
        ran_out_of_fuel_during_normalize =
          t1.ran_out_of_fuel_during_normalize
          || t2.ran_out_of_fuel_during_normalize;
        quality =
          Not_best (* As required by the fact that this is a [jkind_r] *)
      }

let round_up (type l r) ~jkind_of_type (t : (allowed * r) jkind) :
    (l * allowed) jkind =
  let normalized =
    normalize ~mode:Ignore_best ~jkind_of_type (t |> disallow_right)
  in
  { t with
    jkind = { normalized.jkind with with_bounds = No_with_bounds };
    quality = Not_best (* As required by the fact that this is a [jkind_r] *)
  }

let map_type_expr f t =
  if has_with_bounds t
  then { t with jkind = Jkind_desc.map_type_expr f t.jkind }
  else t (* short circuit this common case *)

(* this is hammered on; it must be fast! *)
let check_sub ~jkind_of_type sub super =
  Jkind_desc.sub ~jkind_of_type sub.jkind super.jkind

let sub_with_reason ~type_equal ~jkind_of_type sub super =
  Sub_result.require_le (check_sub ~type_equal ~jkind_of_type sub super)

let sub ~type_equal ~jkind_of_type sub super =
  Result.is_ok (sub_with_reason ~type_equal ~jkind_of_type sub super)

type sub_or_intersect =
  | Sub
  | Disjoint of Violation.Sub_failure_reason.t Nonempty_list.t
  | Has_intersection of Violation.Sub_failure_reason.t Nonempty_list.t

let sub_or_intersect ~type_equal ~jkind_of_type t1 t2 =
  match sub_with_reason ~type_equal ~jkind_of_type t1 t2 with
  | Ok () -> Sub
  | Error reason ->
    if has_intersection t1 t2 then Has_intersection reason else Disjoint reason

let sub_or_error ~type_equal ~jkind_of_type t1 t2 =
  match sub_or_intersect ~type_equal ~jkind_of_type t1 t2 with
  | Sub -> Ok ()
  | Disjoint reason | Has_intersection reason ->
    Error
      (Violation.of_ ~jkind_of_type
         (Not_a_subjkind (t1, t2, Nonempty_list.to_list reason)))

let sub_jkind_l ~type_equal ~jkind_of_type ?(allow_any_crossing = false) sub
    super =
  (* This function implements the "SUB" judgement from kind-inference.md. *)
  let open Misc.Stdlib.Monad.Result.Syntax in
  let require_le sub_result =
    Sub_result.require_le sub_result
    |> Result.map_error (fun reasons ->
           (* When we report an error, we want to show the best-normalized
              version of sub, but the original super. When this check fails, it
              is usually the case that the super was written by the user and the
              sub was inferred. Thus, we should display the user-written jkind,
              but simplify the inferred one, since the inferred one is probably
              overly complex. *)
           (* CR layouts v2.8: It would be useful report to the user why this
              violation occurred, specifically which axes the violation is
              along. *)
           let best_sub = normalize ~mode:Require_best ~jkind_of_type sub in
           Violation.of_ ~jkind_of_type
             (Not_a_subjkind (best_sub, super, Nonempty_list.to_list reasons)))
  in
  let* () =
    (* Validate layouts *)
    require_le (Layout.sub sub.jkind.layout super.jkind.layout)
  in
  match allow_any_crossing with
  | true -> Ok ()
  | false ->
    let best_super =
      (* MB_EXPAND_R *)
      normalize ~mode:Require_best ~jkind_of_type super
    in
    let right_bounds =
      With_bounds.to_best_eff_map best_super.jkind.with_bounds
    in
    let axes_max_on_right =
      (* If the upper_bound is max on the right, then that axis is irrelevant -
         the left will always satisfy the right along that axis. This is an
         optimization, not necessary for correctness *)
      Mod_bounds.get_max_axes best_super.jkind.mod_bounds
    in
    let right_bounds_seq = right_bounds |> With_bounds_types.to_seq in
    let ( ({ layout = _;
             mod_bounds = sub_upper_bounds;
             with_bounds = No_with_bounds
           } :
            (_ * allowed) jkind_desc),
          _ ) =
      (* MB_EXPAND_L *)
      (* Here we progressively expand types on the left.

         Every time we see a type [ty] on the left, we first look to see if [ty]
         occurs on the right. If it does, then we can skip* [ty]. There is an *
         on skip because we can actually only skip on a per-axis basis - if [ty]
         is relevant only along the portability axis on the right, then [ty] is
         no longer relevant to portability on the left, but it is still relevant
         to all other axes. So really, we subtract the axes that are relevant to
         the right from the axes that are relevant to the left.  We can also
         skip [ty] on any axes that are max on the right since anything is <=
         max. Hence, we can also subtract [axes_max_on_right].

         After finding which axes [ty] is relevant along, we lookup [ty]'s jkind
         and join it with the [mod_bounds] along the relevant axes. *)
      (* [Jkind_desc.map_normalize] handles the stepping, jkind lookups, and
         joining.  [map_type_info] handles looking for [ty] on the right and
         removing irrelevant axes. *)
      Layout_and_axes.normalize sub.jkind ~skip_axes:axes_max_on_right
        ~jkind_of_type ~mode:Ignore_best
        ~map_type_info:(fun ty { relevant_axes = left_relevant_axes } ->
          let right_relevant_axes =
            (* Look for [ty] on the right. There may be multiple occurrences of
               it on the right; if so, we union together the relevant axes. *)
            right_bounds_seq
            (* CR layouts v2.8: maybe it's worth memoizing using a best-effort
               type map? *)
            |> Seq.fold_left
                 (fun acc (ty2, ti) ->
                   match type_equal ty ty2 with
                   | true ->
                     Axis_set.union acc ti.With_bounds_type_info.relevant_axes
                   | false -> acc)
                 Axis_set.empty
          in
          (* MB_WITH : drop types from the left that appear on the right *)
          { relevant_axes = Axis_set.diff left_relevant_axes right_relevant_axes
          })
    in
    let* () =
      (* MB_MODE : verify that the remaining upper_bounds from sub are <=
         super's bounds *)
      let super_lower_bounds = best_super.jkind.mod_bounds in
      require_le (Mod_bounds.less_or_equal sub_upper_bounds super_lower_bounds)
    in
    Ok ()

let is_void_defaulting = function
  | { jkind = { layout = Sort s; _ }; _ } -> Sort.is_void_defaulting s
  | _ -> false

let is_obviously_max = function
  (* This doesn't do any mutation because mutating a sort variable can't make it
     any, and modal upper bounds are constant. *)
  | { jkind = { layout = Any; mod_bounds; with_bounds = _ }; _ } ->
    Mod_bounds.is_max mod_bounds
  | _ -> false

let has_layout_any jkind =
  match jkind.jkind.layout with Any -> true | _ -> false

let is_value_for_printing ~ignore_null { jkind; _ } =
  match Desc.get_const (Jkind_desc.get jkind) with
  | None -> false
  | Some const ->
    let value = Const.Builtin.value.jkind in
    let values = [value] in
    let values =
      if ignore_null
      then
        { value with
          mod_bounds =
            Mod_bounds.set_nullability Nullability.Maybe_null value.mod_bounds
        }
        :: values
      else values
    in
    List.exists (fun v -> Const.no_with_bounds_and_equal const v) values

(*********************************)
(* debugging *)

module Debug_printers = struct
  open Format

  let concrete_creation_reason ppf : History.concrete_creation_reason -> unit =
    function
    | Match -> fprintf ppf "Match"
    | Constructor_declaration idx ->
      fprintf ppf "Constructor_declaration %d" idx
    | Label_declaration lbl ->
      fprintf ppf "Label_declaration %a" Ident.print lbl
    | Record_projection -> fprintf ppf "Record_projection"
    | Record_assignment -> fprintf ppf "Record_assignment"
    | Record_functional_update -> fprintf ppf "Record_functional_update"
    | Let_binding -> fprintf ppf "Let_binding"
    | Function_argument -> fprintf ppf "Function_argument"
    | Function_result -> fprintf ppf "Function_result"
    | Structure_item_expression -> fprintf ppf "Structure_item_expression"
    | External_argument -> fprintf ppf "External_argument"
    | External_result -> fprintf ppf "External_result"
    | Statement -> fprintf ppf "Statement"
    | Optional_arg_default -> fprintf ppf "Optional_arg_default"
    | Layout_poly_in_external -> fprintf ppf "Layout_poly_in_external"
    | Unboxed_tuple_element -> fprintf ppf "Unboxed_tuple_element"
    | Peek_or_poke -> fprintf ppf "Peek_or_poke"

  let concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "Unannotated_type_parameter %a" !printtyp_path path
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Array_element -> fprintf ppf "Array_element"
    | Old_style_unboxed_type -> fprintf ppf "Old_style_unboxed_type"

  let rec annotation_context :
      type l r. _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p -> fprintf ppf "Type_declaration %a" Path.print p
    | Type_parameter (p, var) ->
      fprintf ppf "Type_parameter (%a, %a)" Path.print p
        (Misc.Stdlib.Option.print Misc.Stdlib.String.print)
        var
    | Newtype_declaration name -> fprintf ppf "Newtype_declaration %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "Constructor_type_parameter (%a, %S)" Path.print cstr name
    | Existential_unpack name -> fprintf ppf "Existential_unpack %s" name
    | Univar name -> fprintf ppf "Univar %S" name
    | Type_variable name -> fprintf ppf "Type_variable %S" name
    | Type_wildcard loc ->
      fprintf ppf "Type_wildcard (%a)" Location.print_loc loc
    | With_error_message (message, context) ->
      fprintf ppf "With_error_message (%s, %a)" message annotation_context
        context

  let any_creation_reason ppf : History.any_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Type_expression_call -> fprintf ppf "Type_expression_call"
    | Inside_of_Tarrow -> fprintf ppf "Inside_of_Tarrow"
    | Array_type_argument -> fprintf ppf "Array_type_argument"

  let immediate_creation_reason ppf : History.immediate_creation_reason -> _ =
    function
    | Empty_record -> fprintf ppf "Empty_record"
    | Enumeration -> fprintf ppf "Enumeration"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf "Immediate_polymorphic_variant"

  let immediate_or_null_creation_reason ppf :
      History.immediate_or_null_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Tuple_element -> fprintf ppf "Tuple_element"
    | Separability_check -> fprintf ppf "Separability_check"
    | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
    | Structure_element -> fprintf ppf "Structure_element"
    | V1_safety_check -> fprintf ppf "V1_safety_check"
    | Probe -> fprintf ppf "Probe"
    | Captured_in_object -> fprintf ppf "Captured_in_object"
    | Let_rec_variable v -> fprintf ppf "Let_rec_variable %a" Ident.print v
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path

  let value_creation_reason ppf : History.value_creation_reason -> _ = function
    | Class_let_binding -> fprintf ppf "Class_let_binding"
    | Object -> fprintf ppf "Object"
    | Instance_variable -> fprintf ppf "Instance_variable"
    | Object_field -> fprintf ppf "Object_field"
    | Class_field -> fprintf ppf "Class_field"
    | Boxed_record -> fprintf ppf "Boxed_record"
    | Boxed_variant -> fprintf ppf "Boxed_variant"
    | Extensible_variant -> fprintf ppf "Extensible_variant"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path
    | Tuple -> fprintf ppf "Tuple"
    | Row_variable -> fprintf ppf "Row_variable"
    | Polymorphic_variant -> fprintf ppf "Polymorphic_variant"
    | Arrow -> fprintf ppf "Arrow"
    | Tfield -> fprintf ppf "Tfield"
    | Tnil -> fprintf ppf "Tnil"
    | First_class_module -> fprintf ppf "First_class_module"
    | Univar -> fprintf ppf "Univar"
    | Default_type_jkind -> fprintf ppf "Default_type_jkind"
    | Existential_type_variable -> fprintf ppf "Existential_type_variable"
    | Array_comprehension_element -> fprintf ppf "Array_comprehension_element"
    | Lazy_expression -> fprintf ppf "Lazy_expression"
    | Class_type_argument -> fprintf ppf "Class_type_argument"
    | Class_term_argument -> fprintf ppf "Class_term_argument"
    | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
    | Recmod_fun_arg -> fprintf ppf "Recmod_fun_arg"
    | Unknown s -> fprintf ppf "Unknown %s" s

  let product_creation_reason ppf : History.product_creation_reason -> _ =
    function
    | Unboxed_tuple -> fprintf ppf "Unboxed_tuple"
    | Unboxed_record -> fprintf ppf "Unboxed_record"

  let creation_reason ppf : History.creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Immediate_or_null_creation immediate ->
      fprintf ppf "Immediate_or_null_creation %a"
        immediate_or_null_creation_reason immediate
    | Value_or_null_creation value ->
      fprintf ppf "Value_or_null_creation %a" value_or_null_creation_reason
        value
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Product_creation product ->
      fprintf ppf "Product_creation %a" product_creation_reason product
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_creation_reason concrete
    | Concrete_legacy_creation concrete ->
      fprintf ppf "Concrete_legacy_creation %a" concrete_legacy_creation_reason
        concrete
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.name id)
    | Unboxed_primitive id -> fprintf ppf "Unboxed_primitive %s" (Ident.name id)
    | Imported -> fprintf ppf "Imported"
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "Imported_type_argument (pos %d, arity %d) of %a" position
        arity !printtyp_path parent_path
    | Generalized (id, loc) ->
      fprintf ppf "Generalized (%s, %a)"
        (match id with Some id -> Ident.unique_name id | None -> "")
        Location.print_loc loc

  let interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation p -> fprintf ppf "Gadt_equation %a" Path.print p
    | Tyvar_refinement_intersection ->
      fprintf ppf "Tyvar_refinement_intersection"
    | Subjkind -> fprintf ppf "Subjkind"

  let rec history ppf =
    let jkind_desc = Jkind_desc.Debug_printers.t in
    function
    | Interact
        { reason;
          jkind1 = Pack_jkind_desc jkind1;
          history1;
          jkind2 = Pack_jkind_desc jkind2;
          history2
        } ->
      fprintf ppf
        "Interact {@[reason = %a;@ jkind1 = %a;@ history1 = %a;@ jkind2 = %a;@ \
         history2 = %a}@]"
        interact_reason reason jkind_desc jkind1 history history1 jkind_desc
        jkind2 history history2
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  let t (type l r) ppf
      ({ jkind;
         annotation = a;
         history = h;
         has_warned = _;
         ran_out_of_fuel_during_normalize = roofdn;
         quality = q
       } :
        (l * r) jkind) : unit =
    fprintf ppf
      "@[<v 2>{ jkind = %a@,\
       ; annotation = %a@,\
       ; history = %a@,\
       ; ran_out_of_fuel_during_normalize = %a@,\
       ; quality = %s@,\
      \ }@]" Jkind_desc.Debug_printers.t jkind
      (pp_print_option Pprintast.jkind_annotation)
      a history h pp_print_bool roofdn
      (match q with Best -> "Best" | Not_best -> "Not_best")

  module Const = struct
    let t ppf ({ layout; mod_bounds; with_bounds } : _ Const.t) =
      fprintf ppf
        "@[<v 2>{ layout = %a@,; mod_bounds = %a@,; with_bounds = %a@, }@]"
        Layout.Const.Debug_printers.t layout Mod_bounds.debug_print mod_bounds
        With_bounds.debug_print with_bounds
  end
end

(*** formatting user errors ***)
let report_error ~loc : Error.t -> _ = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]" Pprintast.jkind_annotation jkind
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%a) and a layout attribute \
       ([@@@@%s]).@]"
      Pprintast.jkind_annotation from_annotation
      (Builtin_attributes.jkind_attribute_to_string from_attribute.txt)
  | Insufficient_level { jkind; required_layouts_level } -> (
    let hint ppf =
      Format.fprintf ppf "You must enable -extension %s to use this feature."
        (Language_extension.to_command_line_string Layouts
           required_layouts_level)
    in
    match Language_extension.is_enabled Layouts with
    | false ->
      Location.errorf ~loc
        "@[<v>The appropriate layouts extension is not enabled.@;%t@]" hint
    | true ->
      Location.errorf ~loc
        (* CR layouts errors: use the context to produce a better error message.
           When RAE tried this, some types got printed like [t/2], but the
           [/2] shouldn't be there. Investigate and fix. *)
        "@[<v>Layout %a is more experimental than allowed by the enabled \
         layouts extension.@;\
         %t@]"
        Pprintast.jkind_annotation jkind hint)
  | Unimplemented_syntax ->
    Location.errorf ~loc "@[<v>Unimplemented kind syntax@]"
  | With_on_right ->
    Location.errorf ~loc "'with' syntax is not allowed on a right mode."

let () =
  Location.register_error_of_exn (function
    | Error.User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)
