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
  type const =
    | Void
    | Value
    | Float64
    | Float32
    | Word
    | Bits32
    | Bits64

  type t =
    | Var of var
    | Const of const

  and var = t option ref

  module Const = struct
    type t = const

    let equal c1 c2 =
      match c1, c2 with
      | Void, Void
      | Value, Value
      | Float64, Float64
      | Float32, Float32
      | Word, Word
      | Bits32, Bits32
      | Bits64, Bits64 ->
        true
      | (Void | Value | Float64 | Float32 | Word | Bits32 | Bits64), _ -> false

    let to_string = function
      | Value -> "value"
      | Void -> "void"
      | Float64 -> "float64"
      | Float32 -> "float32"
      | Word -> "word"
      | Bits32 -> "bits32"
      | Bits64 -> "bits64"

    let format ppf const = Format.fprintf ppf "%s" (to_string const)
  end

  module Var = struct
    type t = var

    let name : var -> string =
      let next_id = ref 1 in
      let named = ref [] in
      fun v ->
        match List.assq_opt v !named with
        | Some name -> name
        | None ->
          let id = !next_id in
          let name = "'_representable_layout_" ^ Int.to_string id in
          next_id := id + 1;
          named := (v, name) :: !named;
          name
  end

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change = var * t option

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let set_change_log cl = change_log := cl

  let log_change change = !change_log change

  let undo_change (v, t_op) = v := t_op

  let set : var -> t option -> unit =
   fun v t_op ->
    log_change (v, !v);
    v := t_op

  (* Memoize these values for of_const *)

  let void = Const Void

  let value = Const Value

  let float64 = Const Float64

  let float32 = Const Float32

  let word = Const Word

  let bits32 = Const Bits32

  let bits64 = Const Bits64

  let some_value = Some (Const Value)

  let of_const = function
    | Void -> void
    | Value -> value
    | Float64 -> float64
    | Float32 -> float32
    | Word -> word
    | Bits32 -> bits32
    | Bits64 -> bits64

  let of_var v = Var v

  let new_var () = Var (ref None)

  (* Post-condition: If the result is a [Var v], then [!v] is [None]. *)
  let rec get : t -> t = function
    | Const _ as t -> t
    | Var r as t -> (
      match !r with
      | None -> t
      | Some s ->
        let result = get s in
        if result != s then set r (Some result);
        (* path compression *)
        result)

  let memoized_value : t option = Some (Const Value)

  let memoized_void : t option = Some (Const Void)

  let memoized_float64 : t option = Some (Const Float64)

  let memoized_float32 : t option = Some (Const Float32)

  let memoized_word : t option = Some (Const Word)

  let memoized_bits32 : t option = Some (Const Bits32)

  let memoized_bits64 : t option = Some (Const Bits64)

  let[@inline] get_memoized = function
    | Value -> memoized_value
    | Void -> memoized_void
    | Float64 -> memoized_float64
    | Float32 -> memoized_float32
    | Word -> memoized_word
    | Bits32 -> memoized_bits32
    | Bits64 -> memoized_bits64

  let rec default_to_value_and_get : t -> const = function
    | Const c -> c
    | Var r -> (
      match !r with
      | None ->
        set r memoized_value;
        Value
      | Some s ->
        let result = default_to_value_and_get s in
        set r (get_memoized result);
        (* path compression *)
        result)

  let default_to_value t = ignore (default_to_value_and_get t)

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_no_mutation

  let swap_equate_result = function
    | Equal_mutated_first -> Equal_mutated_second
    | Equal_mutated_second -> Equal_mutated_first
    | (Unequal | Equal_no_mutation) as r -> r

  let equal_const_const c1 c2 =
    match c1, c2 with
    | Void, Void
    | Value, Value
    | Float64, Float64
    | Float32, Float32
    | Word, Word
    | Bits32, Bits32
    | Bits64, Bits64 ->
      Equal_no_mutation
    | (Void | Value | Float64 | Float32 | Word | Bits32 | Bits64), _ -> Unequal

  let rec equate_var_const v1 c2 =
    match !v1 with
    | Some s1 -> equate_sort_const s1 c2
    | None ->
      set v1 (Some (of_const c2));
      Equal_mutated_first

  and equate_var v1 s2 =
    match s2 with
    | Const c2 -> equate_var_const v1 c2
    | Var v2 -> equate_var_var v1 v2

  and equate_var_var v1 v2 =
    if v1 == v2
    then Equal_no_mutation
    else
      match !v1, !v2 with
      | Some s1, _ -> swap_equate_result (equate_var v2 s1)
      | _, Some s2 -> equate_var v1 s2
      | None, None ->
        set v1 (Some (of_var v2));
        Equal_mutated_first

  and equate_sort_const s1 c2 =
    match s1 with
    | Const c1 -> equal_const_const c1 c2
    | Var v1 -> equate_var_const v1 c2

  let equate_tracking_mutation s1 s2 =
    match s1 with
    | Const c1 -> swap_equate_result (equate_sort_const s2 c1)
    | Var v1 -> equate_var v1 s2

  (* Don't expose whether or not mutation happened; we just need that for [Jkind] *)
  let equate s1 s2 =
    match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation -> true

  let rec is_void_defaulting = function
    | Const Void -> true
    | Var v -> (
      match !v with
      (* CR layouts v5: this should probably default to void now *)
      | None ->
        set v some_value;
        false
      | Some s -> is_void_defaulting s)
    | Const (Value | Float64 | Float32 | Word | Bits32 | Bits64) -> false

  (*** pretty printing ***)

  let to_string s =
    match get s with Var v -> Var.name v | Const c -> Const.to_string c

  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  (*** debug printing **)

  module Debug_printers = struct
    open Format

    let rec t ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Const c ->
        fprintf ppf
          (match c with
          | Void -> "Void"
          | Value -> "Value"
          | Float64 -> "Float64"
          | Float32 -> "Float32"
          | Word -> "Word"
          | Bits32 -> "Bits32"
          | Bits64 -> "Bits64")

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v = fprintf ppf "{ contents = %a }" opt_t !v
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

module Layout = struct
  type 'sort layout =
    | Sort of 'sort
    | Any

  module Const = struct
    type t = Sort.const layout

    module Legacy = struct
      type t =
        | Any
        | Any_non_null
        | Value_or_null
        | Value
        | Void
        | Immediate64
        | Immediate
        | Float64
        | Float32
        | Word
        | Bits32
        | Bits64
    end
  end

  type t = Sort.t layout
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal
end

module Nullability = struct
  type t =
    | Non_null
    | Maybe_null
end

module Modes = Mode.Alloc.Const

module Jkind_desc = struct
  type 'type_expr t =
    { layout : Layout.t;
      modes_upper_bounds : Modes.t;
      externality_upper_bound : Externality.t;
      nullability_upper_bound : Nullability.t
    }
end

(* A history of conditions placed on a jkind.

   INVARIANT: at most one sort variable appears in this history.
   This is a natural consequence of producing this history by comparing
   jkinds.
*)
type 'type_expr history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        lhs_jkind : 'type_expr Jkind_desc.t;
        lhs_history : 'type_expr history;
        rhs_jkind : 'type_expr Jkind_desc.t;
        rhs_history : 'type_expr history
      }
  | Creation of Jkind_intf.History.creation_reason

type 'type_expr t =
  { jkind : 'type_expr Jkind_desc.t;
    history : 'type_expr history;
    has_warned : bool
  }

module Const = struct
  type 'type_expr t =
    { layout : Layout.Const.t;
      modes_upper_bounds : Modes.t;
      externality_upper_bound : Externality.t;
      nullability_upper_bound : Nullability.t
    }
end

type 'type_expr const = 'type_expr Const.t

type 'type_expr annotation = 'type_expr const * Jane_syntax.Jkind.annotation
