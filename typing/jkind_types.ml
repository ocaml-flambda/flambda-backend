(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Sort = struct
  type base =
    | Void
    | Value
    | Float64
    | Float32
    | Word
    | Bits32
    | Bits64
    | Vec128

  type t =
    | Var of var
    | Base of base
    | Product of t list

  and var =
    { mutable contents : t option;
      uid : int (* For debugging / printing only *)
    }

  let equal_base b1 b2 =
    match b1, b2 with
    | Void, Void
    | Value, Value
    | Float64, Float64
    | Float32, Float32
    | Word, Word
    | Bits32, Bits32
    | Bits64, Bits64
    | Vec128, Vec128 ->
      true
    | (Void | Value | Float64 | Float32 | Word | Bits32 | Bits64 | Vec128), _ ->
      false

  let to_string_base = function
    | Value -> "value"
    | Void -> "void"
    | Float64 -> "float64"
    | Float32 -> "float32"
    | Word -> "word"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"
    | Vec128 -> "vec128"

  module Const = struct
    type t =
      | Base of base
      | Product of t list

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | (Base _ | Product _), _ -> false

    let format ppf c =
      let rec pp_element ~nested ppf = function
        | Base b -> Format.fprintf ppf "%s" (to_string_base b)
        | Product cs ->
          let pp_sep ppf () = Format.fprintf ppf "@ & " in
          Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf cs
      in
      pp_element ~nested:false ppf c

    let value = Base Value

    let void = Base Void

    let float64 = Base Float64

    let float32 = Base Float32

    let word = Base Word

    let bits32 = Base Bits32

    let bits64 = Base Bits64

    let vec128 = Base Vec128

    module Debug_printers = struct
      let t ppf c =
        let rec pp_element ~nested ppf = function
          | Base b ->
            Format.fprintf ppf "%s"
              (match b with
              | Void -> "Void"
              | Value -> "Value"
              | Float64 -> "Float64"
              | Float32 -> "Float32"
              | Word -> "Word"
              | Bits32 -> "Bits32"
              | Bits64 -> "Bits64"
              | Vec128 -> "Vec128")
          | Product cs ->
            let pp_sep ppf () = Format.fprintf ppf "@ , " in
            Format.fprintf ppf "Product [%a]"
              (Misc.pp_nested_list ~nested ~pp_element ~pp_sep)
              cs
        in
        pp_element ~nested:false ppf c
    end

    let for_function = value

    let for_predef_value = value

    let for_block_element = value

    let for_probe_body = value

    let for_poly_variant = value

    let for_record = value

    let for_object = value

    let for_lazy_body = value

    let for_tuple_element = value

    let for_variant_arg = value

    let for_instance_var = value

    let for_class_arg = value

    let for_method = value

    let for_initializer = value

    let for_module = value

    let for_tuple = value

    let for_array_get_result = value

    let for_array_comprehension_element = value

    let for_list_element = value
  end

  module Var = struct
    type id = int

    let get_id { uid; _ } = uid

    (* Map var uids to smaller numbers for more consistent printing. *)
    let next_id = ref 1

    let names : (int, int) Hashtbl.t = Hashtbl.create 16

    let get_print_number uid =
      match Hashtbl.find_opt names uid with
      | Some n -> n
      | None ->
        let id = !next_id in
        incr next_id;
        Hashtbl.add names uid id;
        id

    let name { uid; _ } =
      "'_representable_layout_" ^ Int.to_string (get_print_number uid)
  end

  (*** debug printing **)
  module Debug_printers = struct
    open Format

    let base ppf b =
      fprintf ppf "%s"
        (match b with
        | Void -> "Void"
        | Value -> "Value"
        | Float64 -> "Float64"
        | Float32 -> "Float32"
        | Word -> "Word"
        | Bits32 -> "Bits32"
        | Bits64 -> "Bits64"
        | Vec128 -> "Vec128")

    let rec t ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Base b -> base ppf b
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list ~pp_sep:(fun ppf () -> pp_print_text ppf "; ") t)
          ts

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ uid = %d@ @]}" opt_t v.contents v.uid
  end

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change = var * t option

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let set_change_log cl = change_log := cl

  let log_change change = !change_log change

  let undo_change (v, t_op) = v.contents <- t_op

  let set : var -> t option -> unit =
   fun v t_op ->
    log_change (v, v.contents);
    v.contents <- t_op

  module Static = struct
    (* Statically allocated values of various consts and sorts to save
       allocations in in the core hot path functions. [T] is also included in
       the outer module to provide the core sorts. *)

    module T = struct
      let void = Base Void

      let value = Base Value

      let float64 = Base Float64

      let float32 = Base Float32

      let word = Base Word

      let bits32 = Base Bits32

      let bits64 = Base Bits64

      let vec128 = Base Vec128

      let of_base = function
        | Void -> void
        | Value -> value
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128

      let rec of_const : Const.t -> t = function
        | Base b -> of_base b
        | Product cs -> Product (List.map of_const cs)
    end

    module T_option = struct
      let value = Some T.value

      let void = Some T.void

      let float64 = Some T.float64

      let float32 = Some T.float32

      let word = Some T.word

      let bits32 = Some T.bits32

      let bits64 = Some T.bits64

      let vec128 = Some T.vec128

      let of_base = function
        | Void -> void
        | Value -> value
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128

      let rec of_const : Const.t -> t option = function
        | Base b -> of_base b
        | Product cs ->
          Option.map
            (fun x -> Product x)
            (Misc.Stdlib.List.map_option of_const cs)
    end

    module Const = struct
      open Const

      let value = Base Value

      let void = Base Void

      let float64 = Base Float64

      let float32 = Base Float32

      let word = Base Word

      let bits32 = Base Bits32

      let bits64 = Base Bits64

      let vec128 = Base Vec128

      let of_base : base -> Const.t = function
        | Value -> value
        | Void -> void
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
    end
  end

  let of_var v = Var v

  let last_var_uid = ref 0

  let new_var () =
    incr last_var_uid;
    Var { contents = None; uid = !last_var_uid }

  let rec get : t -> t = function
    | Base _ as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Var r as t -> (
      match r.contents with
      | None -> t
      | Some s ->
        let result = get s in
        if result != s then set r (Some result);
        (* path compression *)
        result)

  let rec default_to_value_and_get : t -> Const.t = function
    | Base b -> Static.Const.of_base b
    | Product ts -> Product (List.map default_to_value_and_get ts)
    | Var r -> (
      match r.contents with
      | None ->
        set r Static.T_option.value;
        Static.Const.value
      | Some s ->
        let result = default_to_value_and_get s in
        set r (Static.T_option.of_const result);
        (* path compression *)
        result)

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_value_and_get s

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  let swap_equate_result = function
    | Equal_mutated_first -> Equal_mutated_second
    | Equal_mutated_second -> Equal_mutated_first
    | (Unequal | Equal_no_mutation | Equal_mutated_both) as r -> r

  let[@inline] sorts_of_product s =
    (* In the equate functions, it's useful to pass around lists of sorts inside
       the product constructor they came from to avoid re-allocating it if we
       end up wanting to store it in a variable. We could probably eliminate the
       use of this by collapsing a bunch of the functions below into each other,
       but that would be much less readable. *)
    match s with
    | Product sorts -> sorts
    | Var _ | Base _ -> Misc.fatal_error "Jkind_types.sorts_of_product"

  let rec equate_sort_sort s1 s2 =
    match s1 with
    | Base b1 -> swap_equate_result (equate_sort_base s2 b1)
    | Var v1 -> equate_var_sort v1 s2
    | Product _ -> swap_equate_result (equate_sort_product s2 s1)

  and equate_sort_base s1 b2 =
    match s1 with
    | Base b1 -> if equal_base b1 b2 then Equal_no_mutation else Unequal
    | Var v1 -> equate_var_base v1 b2
    | Product _ -> Unequal

  and equate_var_base v1 b2 =
    match v1.contents with
    | Some s1 -> equate_sort_base s1 b2
    | None ->
      set v1 (Static.T_option.of_base b2);
      Equal_mutated_first

  and equate_var_sort v1 s2 =
    match s2 with
    | Base b2 -> equate_var_base v1 b2
    | Var v2 -> equate_var_var v1 v2
    | Product _ -> equate_var_product v1 s2

  and equate_var_var v1 v2 =
    if v1 == v2
    then Equal_no_mutation
    else
      match v1.contents, v2.contents with
      | Some s1, _ -> swap_equate_result (equate_var_sort v2 s1)
      | _, Some s2 -> equate_var_sort v1 s2
      | None, None ->
        set v1 (Some (of_var v2));
        Equal_mutated_first

  and equate_var_product v1 s2 =
    match v1.contents with
    | Some s1 -> equate_sort_product s1 s2
    | None ->
      set v1 (Some s2);
      Equal_mutated_first

  and equate_sort_product s1 s2 =
    match s1 with
    | Base _ -> Unequal
    | Product sorts1 ->
      let sorts2 = sorts_of_product s2 in
      equate_sorts sorts1 sorts2
    | Var v1 -> equate_var_product v1 s2

  and equate_sorts sorts1 sorts2 =
    let rec go sorts1 sorts2 acc =
      match sorts1, sorts2 with
      | [], [] -> acc
      | sort1 :: sorts1, sort2 :: sorts2 -> (
        match equate_sort_sort sort1 sort2, acc with
        | Unequal, _ -> Unequal
        | _, Unequal -> assert false
        | Equal_no_mutation, acc | acc, Equal_no_mutation ->
          go sorts1 sorts2 acc
        | Equal_mutated_both, _ | _, Equal_mutated_both ->
          go sorts1 sorts2 Equal_mutated_both
        | Equal_mutated_first, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_first
        | Equal_mutated_second, Equal_mutated_second ->
          go sorts1 sorts2 Equal_mutated_second
        | Equal_mutated_first, Equal_mutated_second
        | Equal_mutated_second, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_both)
      | _, _ -> assert false
    in
    if List.compare_lengths sorts1 sorts2 = 0
    then go sorts1 sorts2 Equal_no_mutation
    else Unequal

  let equate_tracking_mutation = equate_sort_sort

  (* Don't expose whether or not mutation happened; we just need that for
     [Jkind] *)
  let equate s1 s2 =
    match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation
    | Equal_mutated_both ->
      true

  let is_void_defaulting t =
    (* CR layouts v5: this should probably default to void now *)
    match default_to_value_and_get t with
    | Base Void -> true
    | Base (Value | Float64 | Float32 | Word | Bits32 | Bits64 | Vec128) ->
      false
    | Product _ -> false

  let decompose_into_product t n =
    let ts = List.init n (fun _ -> new_var ()) in
    if equate t (Product ts) then Some ts else None

  (*** pretty printing ***)

  let format ppf t =
    let rec pp_element ~nested ppf t =
      match get t with
      | Base b -> Format.fprintf ppf "%s" (to_string_base b)
      | Var v -> Format.fprintf ppf "%s" (Var.name v)
      | Product ts ->
        let pp_sep ppf () = Format.fprintf ppf " & " in
        Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
    in
    pp_element ~nested:false ppf t

  include Static.T
end

module Layout = struct
  type 'sort t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  module Const = struct
    type t =
      | Any
      | Base of Sort.base
      | Product of t list
  end
end

module With_bounds = struct
  module Type_info = struct
    type relevant_for_nullability =
      | Relevant_for_nullability
      | Irrelevant_for_nullability

    type +'type_expr t =
      { type_expr : 'type_expr;
        modality : Mode.Modality.Value.Const.t;
        relevant_for_nullability: relevant_for_nullability
      }

    let print ~print_type_expr ppf { type_expr; modality } =
      let open Format in
      if Mode.Modality.Value.Const.is_id modality
      then print_type_expr ppf type_expr
      else
        fprintf ppf "(@[%a@ @@@@ %a])" print_type_expr type_expr
          Mode.Modality.Value.Const.print modality

    let map_type_expr f ({ type_expr; _ } as t) =
      { t with type_expr = f type_expr }

    let is_relevant_for_nullability = function
      | { relevant_for_nullability = Relevant_for_nullability; _ } -> true
      | { relevant_for_nullability = Irrelevant_for_nullability; _ } -> false

    let is_on_axis (type a) ~(axis : a Jkind_axis.Axis.t) t =
      match axis with
      | Nonmodal Externality -> true (* All fields matter for externality *)
      | Nonmodal Nullability -> is_relevant_for_nullability t
      | Modal axis ->
        let (P axis) = Mode.Const.Axis.alloc_as_value (P axis) in
        not
          (Mode.Modality.Value.Const.proj axis t.modality
          |> Mode.Modality.is_constant)

    let compose_modality t ~then_ =
      let modality = Mode.Modality.Value.Const.compose t.modality ~then_ in
      assert (not (Mode.Modality.Value.Const.is_id modality));
      { t with modality }

    let create ~type_expr ~modality ~relevant_for_nullability =
      { type_expr; modality; relevant_for_nullability }
  end

  type (+'type_expr, 'd) t =
    | No_with_bounds : ('type_expr, 'l * 'r) t
    (* There must always be at least one type. *)
    | With_bounds :
        'type_expr Type_info.t * 'type_expr Type_info.t list
        -> ('type_expr, 'l * Allowance.disallowed) t

  let as_list : type l r. (_, l * r) t -> _ = function
    | No_with_bounds -> []
    | With_bounds (ty, tys) -> ty :: tys

  let has_with_bounds : type l r. (_, l * r) t -> _ = function
    | No_with_bounds -> false
    | With_bounds _ -> true

  open Allowance

  include Magic_allow_disallow (struct
    type ('type_expr, _, 'd) sided = ('type_expr, 'd) t constraint 'd = 'l * 'r

    let disallow_left :
        type l r. ('type_expr, l * r) t -> ('type_expr, disallowed * r) t =
      function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let disallow_right :
        type l r. ('type_expr, l * r) t -> ('type_expr, l * disallowed) t =
      function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let allow_left :
        type l r. ('type_expr, allowed * r) t -> ('type_expr, l * r) t =
      function
      | No_with_bounds -> No_with_bounds
      | With_bounds _ as b -> b

    let allow_right :
        type l r. ('type_expr, l * allowed) t -> ('type_expr, l * r) t =
      function
      | No_with_bounds -> No_with_bounds
  end)

  let try_allow_l :
      type l r. ('type_expr, l * r) t -> ('type_expr, allowed * r) t option =
    function
    | No_with_bounds -> Some No_with_bounds
    | With_bounds _ as b -> Some b

  let try_allow_r :
      type l r. ('type_expr, l * r) t -> ('type_expr, l * allowed) t option =
    function
    | No_with_bounds -> Some No_with_bounds
    | With_bounds _ -> None

  let map_type_expr (type l r) f :
      ('type_expr, l * r) t -> ('type_expr, l * r) t = function
    | No_with_bounds -> No_with_bounds
    | With_bounds (ty, tys) ->
      let f' = Type_info.map_type_expr f in
      With_bounds (f' ty, List.map f' tys)

  let types_on_axis (type l r a) ~(axis : a Jkind_axis.Axis.t) :
      (_, l * r) t -> _ = function
    | No_with_bounds -> []
    | With_bounds (ti, tis) ->
      List.filter_map
        (fun (type_info : _ Type_info.t) ->
          if Type_info.is_on_axis ~axis type_info
          then Some type_info.type_expr
          else None)
        (ti :: tis)

  let compose_modality (type l r) ~then_ : (_, l * r) t -> (_, l * r) t =
    function
    | No_with_bounds -> No_with_bounds
    | With_bounds (t, ts) ->
      With_bounds
        ( Type_info.compose_modality ~then_ t,
          List.map (Type_info.compose_modality ~then_) ts )

  let debug_print (type l r) ~print_type_expr ppf : (_, l * r) t -> _ =
    let open Format in
    function
    | No_with_bounds -> fprintf ppf "No_with_bounds"
    | With_bounds (ty, tys) ->
      fprintf ppf "With_bounds @[[%a]@]"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
           (Type_info.print ~print_type_expr))
        (ty :: tys)
end

module Bounds = struct
  open Jkind_axis
  include Axis_collection.Indexed (Misc.Stdlib.Monad.Identity)

  let equal bounds1 bounds2 =
    Fold2.f
      { f =
          (fun (type axis) ~(axis : axis Axis.t) bound1 bound2 ->
            let (module Bound_ops) = Axis.get axis in
            Bound_ops.equal bound1 bound2)
      }
      ~combine:( && ) bounds1 bounds2

  let debug_print ppf
      { locality;
        linearity;
        uniqueness;
        portability;
        contention;
        externality;
        nullability
      } =
    Format.fprintf ppf
      "@[{ locality = %a;@ linearity = %a;@ uniqueness = %a;@ portability = \
       %a;@ contention = %a;@ externality = %a;@ nullability = %a }@]"
      Mode.Locality.Const.print locality Mode.Linearity.Const.print linearity
      Mode.Uniqueness.Const.print uniqueness Mode.Portability.Const.print
      portability Mode.Contention.Const.print contention Externality.print
      externality Nullability.print nullability
end

module Layout_and_axes = struct
  type ('type_expr, 'layout, 'd) t =
    { layout : 'layout;
      upper_bounds : Bounds.t;
      with_bounds : ('type_expr, 'd) With_bounds.t
    }
    constraint 'd = 'l * 'r

  module Allow_disallow = Allowance.Magic_allow_disallow (struct
    type ('type_expr, 'layout, 'd) sided = ('type_expr, 'layout, 'd) t

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

  let equal eq_layout { layout = lay1; upper_bounds = bounds1 }
      { layout = lay2; upper_bounds = bounds2 } =
    eq_layout lay1 lay2 && Bounds.equal bounds1 bounds2

  let try_allow_l :
      type l r.
      ('type_expr, 'layout, l * r) t ->
      ('type_expr, 'layout, Allowance.allowed * r) t option =
   fun { layout; upper_bounds; with_bounds } ->
    match With_bounds.try_allow_l with_bounds with
    | None -> None
    | Some with_bounds ->
      Some { layout; upper_bounds; with_bounds }

  let try_allow_r { layout; upper_bounds; with_bounds } =
    match With_bounds.try_allow_r with_bounds with
    | Some with_bounds ->
      Some { layout; upper_bounds; with_bounds }
    | None -> None

  let debug_print ~print_type_expr format_layout ppf
      { layout; upper_bounds; with_bounds } =
    Format.fprintf ppf "{ layout = %a;@ upper_bounds = %a;@ with_bounds = %a }"
      format_layout layout Bounds.debug_print upper_bounds
      (With_bounds.debug_print ~print_type_expr)
      with_bounds
end

module Jkind_desc = struct
  type ('type_expr, 'd) t = ('type_expr, Sort.t Layout.t, 'd) Layout_and_axes.t

  type 'type_expr packed = Pack : ('type_expr, 'd) t -> 'type_expr packed
  [@@unboxed]
end

(* A history of conditions placed on a jkind.

   INVARIANT: at most one sort variable appears in this history.
   This is a natural consequence of producing this history by comparing
   jkinds.
*)
type 'type_expr history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        jkind1 : 'type_expr Jkind_desc.packed;
        history1 : 'type_expr history;
        jkind2 : 'type_expr Jkind_desc.packed;
        history2 : 'type_expr history
      }
  | Creation of Jkind_intf.History.creation_reason

type ('type_expr, 'd) t =
  { jkind : ('type_expr, 'd) Jkind_desc.t;
    annotation : Parsetree.jkind_annotation option;
    history : 'type_expr history;
    has_warned : bool
  }

module Const = struct
  type ('type_expr, 'd) t = ('type_expr, Layout.Const.t, 'd) Layout_and_axes.t
end
