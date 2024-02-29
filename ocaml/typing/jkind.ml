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

[@@@warning "+9"]

(* CR layouts v2.8: remove this *)
module Legacy = struct
  type const =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate
    | Float64
    | Word
    | Bits32
    | Bits64

  let const_of_attribute : Builtin_attributes.jkind_attribute -> _ = function
    | Immediate -> Immediate
    | Immediate64 -> Immediate64

  (** The function name is suffixed with "unchecked" to suggest that
    it doesn't check whether the layouts extension is enabled.
    It should be inverse to [string_of_const].
  *)
  let const_of_user_written_annotation_unchecked :
      Jane_syntax.Jkind.t -> const option = function
    | Primitive_layout_or_abbreviation { txt = name } -> (
      match name with
      | "any" -> Some Any
      | "value" -> Some Value
      | "void" -> Some Void
      | "immediate64" -> Some Immediate64
      | "immediate" -> Some Immediate
      | "float64" -> Some Float64
      | "word" -> Some Word
      | "bits32" -> Some Bits32
      | "bits64" -> Some Bits64
      | _ -> None)
    | Default | Mod _ | With _ | Kind_of _ ->
      Misc.fatal_error "XXX unimplemented"

  let string_of_const const =
    match const with
    | Any -> "any"
    | Value -> "value"
    | Void -> "void"
    | Immediate64 -> "immediate64"
    | Immediate -> "immediate"
    | Float64 -> "float64"
    | Word -> "word"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"

  let equal_const c1 c2 =
    match c1, c2 with
    | Any, Any
    | Immediate64, Immediate64
    | Immediate, Immediate
    | Void, Void
    | Value, Value
    | Float64, Float64
    | Word, Word
    | Bits32, Bits32
    | Bits64, Bits64 ->
      true
    | ( ( Any | Immediate64 | Immediate | Void | Value | Float64 | Word | Bits32
        | Bits64 ),
        _ ) ->
      false
end

(* A *sort* is the information the middle/back ends need to be able to
   compile a manipulation (storing, passing, etc) of a runtime value. *)
module Sort = struct
  (* CR layouts v2.8: Refactor to use a Const module *)
  type const =
    | Void
    | Value
    | Float64
    | Word
    | Bits32
    | Bits64

  type t =
    | Var of var
    | Const of const

  and var = t option ref

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change = var * t option

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let log_change change = !change_log change

  let undo_change (v, t_op) = v := t_op

  let var_name : var -> string =
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

  let set : var -> t option -> unit =
   fun v t_op ->
    log_change (v, !v);
    v := t_op

  let void = Const Void

  let value = Const Value

  let float64 = Const Float64

  let word = Const Word

  let bits32 = Const Bits32

  let bits64 = Const Bits64

  let some_value = Some value

  let of_const = function
    | Void -> void
    | Value -> value
    | Float64 -> float64
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

  let memoized_word : t option = Some (Const Word)

  let memoized_bits32 : t option = Some (Const Bits32)

  let memoized_bits64 : t option = Some (Const Bits64)

  let[@inline] get_memoized = function
    | Value -> memoized_value
    | Void -> memoized_void
    | Float64 -> memoized_float64
    | Word -> memoized_word
    | Bits32 -> memoized_bits32
    | Bits64 -> memoized_bits64

  let rec get_default_value : t -> const = function
    | Const c -> c
    | Var r -> (
      match !r with
      | None ->
        set r memoized_value;
        Value
      | Some s ->
        let result = get_default_value s in
        set r (get_memoized result);
        (* path compression *)
        result)

  let default_to_value t = ignore (get_default_value t)

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
    | Word, Word
    | Bits32, Bits32
    | Bits64, Bits64 ->
      Equal_no_mutation
    | (Void | Value | Float64 | Word | Bits32 | Bits64), _ -> Unequal

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

  let equal_const c1 c2 =
    match c1, c2 with
    | Void, Void
    | Value, Value
    | Float64, Float64
    | Word, Word
    | Bits32, Bits32
    | Bits64, Bits64 ->
      true
    | Void, (Value | Float64 | Word | Bits32 | Bits64)
    | Value, (Void | Float64 | Word | Bits32 | Bits64)
    | Float64, (Value | Void | Word | Bits32 | Bits64)
    | Word, (Value | Void | Float64 | Bits32 | Bits64)
    | Bits32, (Value | Void | Float64 | Word | Bits64)
    | Bits64, (Value | Void | Float64 | Word | Bits32) ->
      false

  let rec is_void_defaulting = function
    | Const Void -> true
    | Var v -> (
      match !v with
      (* CR layouts v5: this should probably default to void now *)
      | None ->
        set v some_value;
        false
      | Some s -> is_void_defaulting s)
    | Const (Value | Float64 | Word | Bits32 | Bits64) -> false

  (*** pretty printing ***)

  let string_of_const = function
    | Value -> "value"
    | Void -> "void"
    | Float64 -> "float64"
    | Word -> "word"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"

  let to_string s =
    match get s with Var v -> var_name v | Const c -> string_of_const c

  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  let format_const ppf const = Format.fprintf ppf "%s" (string_of_const const)

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

  let for_constructor_arg = value

  let for_object = value

  let for_lazy_body = value

  let for_tuple_element = value

  let for_instance_var = value

  let for_class_arg = value

  let for_method = value

  let for_initializer = value

  let for_module = value

  let for_tuple = value

  let for_array_get_result = value

  let for_array_element = value

  let for_list_element = value
end

type sort = Sort.t

(* A *layout* of a type describes the way values of that type are stored at
   runtime, including details like width, register convention, calling
   convention, etc. A layout may be *representable* or *unrepresentable*.  The
   middle/back ends are unable to cope with values of types with an
   unrepresentable layout. The only unrepresentable layout is `any`, which is
   the top of the layout lattice. *)
module Layout = struct
  module Const = struct
    type t =
      | Sort of Sort.const
      | Any

    let max = Any

    let equal c1 c2 =
      match c1, c2 with
      | Sort s1, Sort s2 -> Sort.equal_const s1 s2
      | Any, Any -> true
      | (Any | Sort _), _ -> false

    let sub c1 c2 : Misc.Le_result.t =
      match c1, c2 with
      | _ when equal c1 c2 -> Equal
      | _, Any -> Less
      | Any, Sort _ | Sort _, Sort _ -> Not_le
  end

  type t =
    | Sort of Sort.t
    | Any

  let max = Any

  let equate_or_equal ~allow_mutation t1 t2 =
    match t1, t2 with
    | Sort s1, Sort s2 -> (
      match Sort.equate_tracking_mutation s1 s2 with
      | (Equal_mutated_first | Equal_mutated_second) when not allow_mutation ->
        Misc.fatal_errorf "Jkind.equal: Performed unexpected mutation"
      | Unequal -> false
      | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second -> true)
    | Any, Any -> true
    | (Any | Sort _), _ -> false

  let sub t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | Any, Any -> Equal
    | _, Any -> Less
    | Any, _ -> Not_le
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Equal else Not_le

  let intersection t1 t2 =
    match t1, t2 with
    | _, Any -> Some t1
    | Any, _ -> Some t2
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Some t1 else None

  let of_new_sort_var () =
    let sort = Sort.new_var () in
    Sort sort, sort

  let value = Sort Sort.value

  let void = Sort Sort.void

  let float64 = Sort Sort.float64

  let word = Sort Sort.word

  let bits32 = Sort Sort.bits32

  let bits64 = Sort Sort.bits64

  module Debug_printers = struct
    open Format

    let t ppf = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" Sort.Debug_printers.t s
  end
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal

  (* CR layouts v2.8: Either use this or remove it *)
  let _to_string = function
    | External -> "external"
    | External64 -> "external64"
    | Internal -> "internal"

  let max = Internal

  let min = External

  let equal e1 e2 =
    match e1, e2 with
    | External, External -> true
    | External64, External64 -> true
    | Internal, Internal -> true
    | (External | External64 | Internal), _ -> false

  let less_or_equal t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | External, External -> Equal
    | External, (External64 | Internal) -> Less
    | External64, External -> Not_le
    | External64, External64 -> Equal
    | External64, Internal -> Less
    | Internal, (External | External64) -> Not_le
    | Internal, Internal -> Equal

  let le t1 t2 = Misc.Le_result.is_le (less_or_equal t1 t2)

  let meet t1 t2 =
    match t1, t2 with
    | External, (External | External64 | Internal)
    | (External64 | Internal), External ->
      External
    | External64, (External64 | Internal) | Internal, External64 -> External64
    | Internal, Internal -> Internal

  module Debug_printers = struct
    open Format

    let t ppf = function
      | External -> fprintf ppf "External"
      | External64 -> fprintf ppf "External64"
      | Internal -> fprintf ppf "Internal"
  end
end

module Modes = struct
  include Alloc.Const

  let less_or_equal a b : Misc.Le_result.t =
    match le a b, le b a with
    | true, true -> Equal
    | true, false -> Less
    | false, _ -> Not_le

  let equal a b = Misc.Le_result.is_equal (less_or_equal a b)
end

module Const = struct
  type t =
    { layout : Layout.Const.t;
      modes_upper_bounds : Modes.t;
      externality_upper_bound : Externality.t
    }

  let max =
    { layout = Layout.Const.max;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.max
    }

  (* CR layouts v2.8: remove this *)
  let to_legacy_jkind
      { layout; modes_upper_bounds = _; externality_upper_bound } : Legacy.const
      =
    match layout, externality_upper_bound with
    | Any, _ -> Any
    | Sort Value, Internal -> Value
    | Sort Value, External64 -> Immediate64
    | Sort Value, External -> Immediate
    | Sort Void, _ -> Void
    | Sort Float64, _ -> Float64
    | Sort Word, _ -> Word
    | Sort Bits32, _ -> Bits32
    | Sort Bits64, _ -> Bits64

  (* CR layouts v2.8: do a better job here *)
  let to_string t = Legacy.string_of_const (to_legacy_jkind t)

  let equal
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2
      } =
    Layout.Const.equal lay1 lay2
    && Modes.equal modes1 modes2
    && Externality.equal ext1 ext2

  let sub
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2
      } =
    Misc.Le_result.combine_list
      [ Layout.Const.sub lay1 lay2;
        Modes.less_or_equal modes1 modes2;
        Externality.less_or_equal ext1 ext2 ]
end

module Desc = struct
  type t =
    | Const of Const.t
    | Var of Sort.var (* all modes will be [max] *)

  let format ppf =
    let open Format in
    function
    | Const c -> fprintf ppf "%s" (Const.to_string c)
    | Var v -> fprintf ppf "%s" (Sort.var_name v)

  (* considers sort variables < Any. Two sort variables are in a [sub]
     relationship only when they are equal.
     Never does mutation.
     Pre-condition: no filled-in sort variables. *)
  let sub d1 d2 : Misc.Le_result.t =
    match d1, d2 with
    | Const c1, Const c2 -> Const.sub c1 c2
    | Var _, Const c when Const.equal Const.max c -> Less
    | Var v1, Var v2 -> if v1 == v2 then Equal else Not_le
    | Const _, Var _ | Var _, Const _ -> Not_le
end

module Jkind_desc = struct
  type t =
    { layout : Layout.t;
      modes_upper_bounds : Modes.t;
      externality_upper_bound : Externality.t
    }

  let max =
    { layout = Layout.max;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.max
    }

  let equate_or_equal ~allow_mutation
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2
      } =
    Layout.equate_or_equal ~allow_mutation lay1 lay2
    && Modes.equal modes1 modes2
    && Externality.equal ext1 ext2

  let sub
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2
      } =
    Misc.Le_result.combine_list
      [ Layout.sub lay1 lay2;
        Modes.less_or_equal modes1 modes2;
        Externality.less_or_equal ext1 ext2 ]

  let intersection
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2
      } =
    Option.bind (Layout.intersection lay1 lay2) (fun layout ->
        Some
          { layout;
            modes_upper_bounds = Modes.meet modes1 modes2;
            externality_upper_bound = Externality.meet ext1 ext2
          })

  let of_new_sort_var () =
    let layout, sort = Layout.of_new_sort_var () in
    { max with layout }, sort

  let any = max

  let value = { max with layout = Layout.value }

  let void =
    { layout = Layout.void;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.min
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
    { layout = Layout.value;
      modes_upper_bounds =
        { locality = Global; linearity = Many; uniqueness = Unique };
      externality_upper_bound = External64
    }

  let immediate =
    { layout = Layout.value;
      modes_upper_bounds =
        { locality = Global; linearity = Many; uniqueness = Unique };
      externality_upper_bound = External
    }

  let float64 =
    { layout = Layout.float64;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = External
    }

  let word =
    { layout = Layout.word;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = External
    }

  let bits32 =
    { layout = Layout.bits32;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = External
    }

  let bits64 =
    { layout = Layout.bits64;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = External
    }

  (* Post-condition: If the result is [Var v], then [!v] is [None]. *)
  let get { layout; modes_upper_bounds; externality_upper_bound } : Desc.t =
    match layout with
    | Any -> Const { layout = Any; modes_upper_bounds; externality_upper_bound }
    | Sort s -> (
      match Sort.get s with
      | Const s ->
        Const { layout = Sort s; modes_upper_bounds; externality_upper_bound }
      | Var v -> Var v)

  module Debug_printers = struct
    open Format

    let t ppf { layout; modes_upper_bounds; externality_upper_bound } =
      fprintf ppf
        "{ layout = %a;@ modes_upper_bounds = %a;@ externality_upper_bound = \
         %a }"
        Layout.Debug_printers.t layout Modes.print modes_upper_bounds
        Externality.Debug_printers.t externality_upper_bound
  end
end

(*** reasons for jkinds **)
type concrete_jkind_reason =
  | Match
  | Constructor_declaration of int
  | Label_declaration of Ident.t
  | Unannotated_type_parameter of Path.t
  | Record_projection
  | Record_assignment
  | Let_binding
  | Function_argument
  | Function_result
  | Structure_item_expression
  | External_argument
  | External_result
  | Statement
  | Wildcard
  | Unification_var
  | Optional_arg_default
  | Layout_poly_in_external

type value_creation_reason =
  | Class_let_binding
  | Tuple_element
  | Probe
  | Object
  | Instance_variable
  | Object_field
  | Class_field
  | Boxed_record
  | Boxed_variant
  | Extensible_variant
  | Primitive of Ident.t
  | Type_argument of
      { parent_path : Path.t;
        position : int;
        arity : int
      }
    (* [position] is 1-indexed *)
  | Tuple
  | Row_variable
  | Polymorphic_variant
  | Arrow
  | Tfield
  | Tnil
  | First_class_module
  | Separability_check
  | Univar
  | Polymorphic_variant_field
  | Default_type_jkind
  | Float_record_field
  | Existential_type_variable
  | Array_element
  | Lazy_expression
  | Class_type_argument
  | Class_term_argument
  | Structure_element
  | Debug_printer_argument
  | V1_safety_check
  | Captured_in_object
  | Recmod_fun_arg
  | Unknown of string

type immediate_creation_reason =
  | Empty_record
  | Enumeration
  | Primitive of Ident.t
  | Immediate_polymorphic_variant

type immediate64_creation_reason = Separability_check

type void_creation_reason = |

type any_creation_reason =
  | Missing_cmi of Path.t
  | Initial_typedecl_env
  | Dummy_jkind
  | Type_expression_call
  | Inside_of_Tarrow
  | Wildcard
  | Unification_var

type float64_creation_reason = Primitive of Ident.t

type word_creation_reason = Primitive of Ident.t

type bits32_creation_reason = Primitive of Ident.t

type bits64_creation_reason = Primitive of Ident.t

type annotation_context =
  | Type_declaration of Path.t
  | Type_parameter of Path.t * string option
  | Newtype_declaration of string
  | Constructor_type_parameter of Path.t * string
  | Univar of string
  | Type_variable of string
  | Type_wildcard of Location.t
  | With_error_message of string * annotation_context

type creation_reason =
  | Annotated of annotation_context * Location.t
  | Missing_cmi of Path.t
  | Value_creation of value_creation_reason
  | Immediate_creation of immediate_creation_reason
  | Immediate64_creation of immediate64_creation_reason
  | Void_creation of void_creation_reason
  | Any_creation of any_creation_reason
  | Float64_creation of float64_creation_reason
  | Word_creation of word_creation_reason
  | Bits32_creation of bits32_creation_reason
  | Bits64_creation of bits64_creation_reason
  | Concrete_creation of concrete_jkind_reason
  | Imported
  | Imported_type_argument of
      { parent_path : Path.t;
        position : int;
        arity : int
      }
  (* [position] is 1-indexed *)
  | Generalized of Ident.t option * Location.t

type interact_reason =
  | Gadt_equation of Path.t
  | Tyvar_refinement_intersection
  (* CR layouts: this needs to carry a type_expr, but that's loopy *)
  | Subjkind

(* A history of conditions placed on a jkind.

   INVARIANT: at most one sort variable appears in this history.
   This is a natural consequence of producing this history by comparing
   jkinds.
*)
type history =
  | Interact of
      { reason : interact_reason;
        lhs_jkind : Jkind_desc.t;
        lhs_history : history;
        rhs_jkind : Jkind_desc.t;
        rhs_history : history
      }
  | Creation of creation_reason

type t =
  { jkind : Jkind_desc.t;
    history : history
  }

let fresh_jkind jkind ~why = { jkind; history = Creation why }

(******************************)
(* constants *)

let any_dummy_jkind =
  { jkind = Jkind_desc.max; history = Creation (Any_creation Dummy_jkind) }

let value_v1_safety_check =
  { jkind = Jkind_desc.value;
    history = Creation (Value_creation V1_safety_check)
  }

(* CR layouts: Should we be doing more memoization here? *)
let any ~why =
  match why with
  | Dummy_jkind -> any_dummy_jkind (* share this one common case *)
  | _ -> fresh_jkind Jkind_desc.any ~why:(Any_creation why)

let void ~why = fresh_jkind Jkind_desc.void ~why:(Void_creation why)

let value ~(why : value_creation_reason) =
  match why with
  | V1_safety_check -> value_v1_safety_check
  | _ -> fresh_jkind Jkind_desc.value ~why:(Value_creation why)

let immediate64 ~why =
  fresh_jkind Jkind_desc.immediate64 ~why:(Immediate64_creation why)

let immediate ~why =
  fresh_jkind Jkind_desc.immediate ~why:(Immediate_creation why)

let float64 ~why = fresh_jkind Jkind_desc.float64 ~why:(Float64_creation why)

let word ~why = fresh_jkind Jkind_desc.word ~why:(Word_creation why)

let bits32 ~why = fresh_jkind Jkind_desc.bits32 ~why:(Bits32_creation why)

let bits64 ~why = fresh_jkind Jkind_desc.bits64 ~why:(Bits64_creation why)

(******************************)
(*** user errors ***)
type error =
  | Insufficient_level of
      { jkind : Legacy.const;
        required_layouts_level : Language_extension.maturity
      }
  | Unknown_jkind of Jane_syntax.Jkind.t
  | Multiple_jkinds of
      { from_annotation : Legacy.const;
        from_attribute : Legacy.const
      }

exception User_error of Location.t * error

let raise ~loc err = raise (User_error (loc, err))

(*** extension requirements ***)
(* The [annotation_context] parameter can be used to allow annotations / kinds
   in different contexts to be enabled with different extension settings.
   At some points in time, we will not care about the context, and so this
   parameter might effectively be unused.
*)
(* CR layouts: When everything is stable, remove this function. *)
let get_required_layouts_level (context : annotation_context)
    (jkind : Legacy.const) : Language_extension.maturity =
  match context, jkind with
  | _, (Value | Immediate | Immediate64 | Any | Float64 | Word | Bits32 | Bits64)
    ->
    Stable
  | _, Void -> Alpha

(******************************)
(* construction *)

let of_new_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var () in
  fresh_jkind jkind ~why:(Concrete_creation why), sort

let of_new_sort ~why = fst (of_new_sort_var ~why)

(* CR layouts v2.8: remove this function *)
let of_const ~why : Legacy.const -> t = function
  | Any -> fresh_jkind Jkind_desc.any ~why
  | Immediate -> fresh_jkind Jkind_desc.immediate ~why
  | Immediate64 -> fresh_jkind Jkind_desc.immediate64 ~why
  | Value -> fresh_jkind Jkind_desc.value ~why
  | Void -> fresh_jkind Jkind_desc.void ~why
  | Float64 -> fresh_jkind Jkind_desc.float64 ~why
  | Word -> fresh_jkind Jkind_desc.word ~why
  | Bits32 -> fresh_jkind Jkind_desc.bits32 ~why
  | Bits64 -> fresh_jkind Jkind_desc.bits64 ~why

let const_of_user_written_annotation ~context Location.{ loc; txt = annot } =
  match Legacy.const_of_user_written_annotation_unchecked annot with
  | None -> raise ~loc (Unknown_jkind annot)
  | Some const ->
    let required_layouts_level = get_required_layouts_level context const in
    if not (Language_extension.is_at_least Layouts required_layouts_level)
    then
      raise ~loc (Insufficient_level { jkind = const; required_layouts_level });
    const

let of_annotated_const ~context ~const ~const_loc =
  of_const ~why:(Annotated (context, const_loc)) const

let of_annotation ~context (annot : _ Location.loc) =
  let const = const_of_user_written_annotation ~context annot in
  let jkind = of_annotated_const ~const ~const_loc:annot.loc ~context in
  jkind, (const, annot)

let of_annotation_option_default ~default ~context =
  Option.fold ~none:(default, None) ~some:(fun annot ->
      let t, annot = of_annotation ~context annot in
      t, Some annot)

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let const = Legacy.const_of_attribute attribute.txt in
  of_annotated_const ~context ~const ~const_loc:attribute.loc, const

let of_type_decl ~context (decl : Parsetree.type_declaration) =
  let jkind_of_annotation =
    Jane_syntax.Layouts.of_type_declaration decl
    |> Option.map (fun (annot, attrs) ->
           let t, const = of_annotation ~context annot in
           t, const, attrs)
  in
  let jkind_of_attribute =
    Builtin_attributes.jkind decl.ptype_attributes
    |> Option.map (fun attr ->
           let t, const = of_attribute ~context attr in
           (* This is a bit of a lie: the "annotation" here is being
              forged based on the jkind attribute. But: the jkind
              annotation is just used in printing/untypeast, and the
              all strings valid to use as a jkind attribute are
              valid (and equivalent) to write as an annotation, so
              this lie is harmless.
           *)
           let annot =
             Location.map
               (fun attr ->
                 let jkind_of_prim_name name =
                   Jane_syntax.Jkind.Primitive_layout_or_abbreviation name
                 in
                 Builtin_attributes.jkind_attribute_to_string attr
                 |> Location.mknoloc |> jkind_of_prim_name)
               attr
           in
           t, (const, annot), decl.ptype_attributes)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | (Some _ as x), None | None, (Some _ as x) -> x
  | Some (_, (from_annotation, _), _), Some (_, (from_attribute, _), _) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let of_type_decl_default ~context ~default (decl : Parsetree.type_declaration) =
  match of_type_decl ~context decl with
  | Some (t, const, attrs) -> t, Some const, attrs
  | None -> default, None, decl.ptype_attributes

let for_boxed_record ~all_void =
  if all_void then immediate ~why:Empty_record else value ~why:Boxed_record

let for_boxed_variant ~all_voids =
  if all_voids then immediate ~why:Enumeration else value ~why:Boxed_variant

(******************************)
(* elimination and defaulting *)

let get_default_value
    { jkind = { layout; modes_upper_bounds; externality_upper_bound }; _ } :
    Const.t =
  match layout with
  | Any -> { layout = Any; modes_upper_bounds; externality_upper_bound }
  | Sort s ->
    { layout = Sort (Sort.get_default_value s);
      modes_upper_bounds;
      externality_upper_bound
    }

let default_to_value t = ignore (get_default_value t)

let get t = Jkind_desc.get t.jkind

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let sort_of_jkind l =
  match get l with
  | Const { layout = Sort s; _ } -> Sort.of_const s
  | Const { layout = Any; _ } -> Misc.fatal_error "Jkind.sort_of_jkind"
  | Var v -> Sort.of_var v

let get_layout jk : Layout.Const.t option =
  match jk.jkind.layout with
  | Any -> Some Any
  | Sort s -> (
    match Sort.get s with Const s -> Some (Sort s) | Var _ -> None)

let get_modal_upper_bounds jk = jk.jkind.modes_upper_bounds

let get_externality_upper_bound jk = jk.jkind.externality_upper_bound

(*********************************)
(* pretty printing *)

let to_string jkind =
  match get jkind with Const c -> Const.to_string c | Var v -> Sort.var_name v

let format ppf t = Format.fprintf ppf "%s" (to_string t)

(***********************************)
(* jkind histories *)
let has_imported_history t =
  match t.history with Creation Imported -> true | _ -> false

let update_reason t reason = { t with history = Creation reason }

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
module Format_history : sig
  val format_history :
    intro:(Format.formatter -> unit) -> Format.formatter -> t -> unit
end = struct
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

  let format_concrete_jkind_reason ppf : concrete_jkind_reason -> unit =
    function
    | Match -> fprintf ppf "a value of this type is matched against a pattern"
    | Constructor_declaration _ ->
      fprintf ppf "it's the type of a constructor field"
    | Label_declaration lbl ->
      fprintf ppf "it is the type of record field %s" (Ident.name lbl)
    | Unannotated_type_parameter path ->
      fprintf ppf "it instantiates an unannotated type parameter of %a"
        !printtyp_path path
    | Record_projection ->
      fprintf ppf "it's the record type used in a projection"
    | Record_assignment ->
      fprintf ppf "it's the record type used in an assignment"
    | Let_binding -> fprintf ppf "it's the type of a variable bound by a `let`"
    | Function_argument -> fprintf ppf "it's the type of a function argument"
    | Function_result -> fprintf ppf "it's the type of a function result"
    | Structure_item_expression ->
      fprintf ppf "it's the type of an expression in a structure"
    | External_argument ->
      fprintf ppf "it's the type of an argument in an external declaration"
    | External_result ->
      fprintf ppf "it's the type of the result of an external declaration"
    | Statement -> fprintf ppf "it's the type of a statement"
    | Wildcard -> fprintf ppf "it's a _ in the type"
    | Unification_var -> fprintf ppf "it's a fresh unification variable"
    | Optional_arg_default ->
      fprintf ppf "it's the type of an optional argument default"
    | Layout_poly_in_external ->
      fprintf ppf
        "it's the layout polymorphic type in an external declaration@ \
         ([@@layout_poly] forces all variables of layout 'any' to be@ \
         representable at call sites)"

  let rec format_annotation_context ppf : annotation_context -> unit = function
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
    | Univar name -> fprintf ppf "the universal variable %s" name
    | Type_variable name -> fprintf ppf "the type variable %s" name
    | Type_wildcard loc ->
      fprintf ppf "the wildcard _ at %a" Location.print_loc_in_lowercase loc
    | With_error_message (_message, context) ->
      (* message gets printed in [format_flattened_history] so we ignore it here *)
      format_annotation_context ppf context

  let format_any_creation_reason ppf : any_creation_reason -> unit = function
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Wildcard -> format_with_notify_js ppf "there's a _ in the type"
    | Unification_var ->
      format_with_notify_js ppf "it's a fresh unification variable"
    | Initial_typedecl_env ->
      format_with_notify_js ppf
        "a dummy layout of any is used to check mutually recursive datatypes"
    | Dummy_jkind ->
      format_with_notify_js ppf
        "it's assigned a dummy layout that should have been overwritten"
    (* CR layouts: Improve output or remove this constructor ^^ *)
    | Type_expression_call ->
      format_with_notify_js ppf
        "there's a call to [type_expression] via the ocaml API"
    | Inside_of_Tarrow -> fprintf ppf "argument or result of a function type"

  let format_immediate_creation_reason ppf : immediate_creation_reason -> _ =
    function
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

  let format_immediate64_creation_reason ppf = function
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"

  let format_value_creation_reason ppf : value_creation_reason -> _ = function
    | Class_let_binding ->
      fprintf ppf "it's the type of a let-bound variable in a class expression"
    | Tuple_element -> fprintf ppf "it's the type of a tuple element"
    | Probe -> format_with_notify_js ppf "it's a probe"
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
      fprintf ppf "the %stype argument of %a has layout value"
        (format_position ~arity position)
        !printtyp_path parent_path
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
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"
    | Univar ->
      fprintf ppf "it is or unifies with an unannotated universal variable"
    | Polymorphic_variant_field ->
      fprintf ppf "it's the type of the field of a polymorphic variant"
    | Default_type_jkind ->
      fprintf ppf "an abstract type has the value layout by default"
    | Float_record_field -> fprintf ppf "it's the type of a float record field"
    | Existential_type_variable ->
      fprintf ppf "it's an unannotated existential type variable"
    | Array_element -> fprintf ppf "it's the type of an array element"
    | Lazy_expression -> fprintf ppf "it's the type of a lazy expression"
    | Class_type_argument ->
      fprintf ppf "it's a type argument to a class constructor"
    | Class_term_argument ->
      fprintf ppf
        "it's the type of a term-level argument to a class constructor"
    | Structure_element ->
      fprintf ppf "it's the type of something stored in a module structure"
    | Debug_printer_argument ->
      format_with_notify_js ppf
        "it's the type of an argument to a debugger printer function"
    | V1_safety_check ->
      fprintf ppf "it has to be value for the V1 safety check"
    | Captured_in_object ->
      fprintf ppf "it's the type of a variable captured in an object"
    | Recmod_fun_arg ->
      fprintf ppf
        "it's the type of the first argument to a function in a recursive \
         module"
    | Unknown s ->
      fprintf ppf
        "unknown @[(please alert the Jane Street@;\
         compilers team with this message: %s)@]" s

  let format_float64_creation_reason ppf : float64_creation_reason -> _ =
    function
    | Primitive id ->
      fprintf ppf "it is the primitive float64 type %s" (Ident.name id)

  let format_word_creation_reason ppf : word_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive word type %s" (Ident.name id)

  let format_bits32_creation_reason ppf : bits32_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive bits32 type %s" (Ident.name id)

  let format_bits64_creation_reason ppf : bits64_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive bits64 type %s" (Ident.name id)

  let format_creation_reason ppf : creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Any_creation any -> format_any_creation_reason ppf any
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Immediate64_creation immediate64 ->
      format_immediate64_creation_reason ppf immediate64
    | Void_creation _ -> .
    | Value_creation value -> format_value_creation_reason ppf value
    | Float64_creation float -> format_float64_creation_reason ppf float
    | Word_creation word -> format_word_creation_reason ppf word
    | Bits32_creation bits32 -> format_bits32_creation_reason ppf bits32
    | Bits64_creation bits64 -> format_bits64_creation_reason ppf bits64
    | Concrete_creation concrete -> format_concrete_jkind_reason ppf concrete
    | Imported ->
      fprintf ppf "of layout requirements from an imported definition"
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has this layout"
        (format_position ~arity position)
        !printtyp_path parent_path
    | Generalized (id, loc) ->
      let format_id ppf = function
        | Some id -> fprintf ppf " of %s" (Ident.name id)
        | None -> ()
      in
      fprintf ppf "of the definition%a at %a" format_id id
        Location.print_loc_in_lowercase loc

  let format_interact_reason ppf = function
    | Gadt_equation name ->
      fprintf ppf "a GADT match on the constructor %a" !printtyp_path name
    | Tyvar_refinement_intersection -> fprintf ppf "updating a type variable"
    | Subjkind -> fprintf ppf "sublayout check"

  (* CR layouts: An older implementation of format_flattened_history existed
      which displays more information not limited to one layout and one creation_reason
      around commit 66a832d70bf61d9af3b0ec6f781dcf0a188b324d in main.

      Consider revisiting that if the current implementation becomes insufficient. *)

  let format_flattened_history ~intro ppf t =
    let jkind_desc = Jkind_desc.get t.jkind in
    fprintf ppf "@[<v 2>%t" intro;
    (match t.history with
    | Creation reason -> (
      fprintf ppf ", because@ %a" format_creation_reason reason;
      match reason, jkind_desc with
      | Concrete_creation _, Const _ ->
        fprintf ppf ", defaulted to layout %a" Desc.format jkind_desc
      | _ -> ())
    | _ -> assert false);
    fprintf ppf ".";
    (match t.history with
    | Creation (Annotated (With_error_message (message, _), _)) ->
      fprintf ppf "@ @[%s@]" message
    | _ -> ());
    fprintf ppf "@]"

  (* this isn't really formatted for user consumption *)
  let format_history_tree ~intro ppf t =
    let rec in_order ppf = function
      | Interact
          { reason; lhs_history; rhs_history; lhs_jkind = _; rhs_jkind = _ } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]" in_order lhs_history
          format_interact_reason reason in_order rhs_history
      | Creation c -> format_creation_reason ppf c
    in
    fprintf ppf "@;%t has this layout history:@;@[<v 2>  %a@]" intro in_order
      t.history

  let format_history ~intro ppf t =
    if display_histories
    then
      if flattened_histories
      then format_flattened_history ~intro ppf t
      else format_history_tree ~intro ppf t
end

include Format_history

(******************************)
(* errors *)

module Violation = struct
  open Format

  type violation =
    | Not_a_subjkind of t * t
    | No_intersection of t * t

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

  let of_ ?missing_cmi violation = { violation; missing_cmi }

  let is_missing_cmi viol = Option.is_some viol.missing_cmi

  let report_general preamble pp_former former ppf t =
    let subjkind_format verb l2 =
      match get l2 with
      | Var _ -> dprintf "%s representable" verb
      | Const _ -> dprintf "%s a sublayout of %a" verb format l2
    in
    let l1, l2, fmt_l1, fmt_l2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (l1, l2); missing_cmi } -> (
        let missing_cmi =
          match missing_cmi with
          | None -> (
            match l1.history with
            | Creation (Missing_cmi p) -> Some p
            | Creation (Any_creation (Missing_cmi p)) -> Some p
            | _ -> None)
          | Some _ -> missing_cmi
        in
        match missing_cmi with
        | None ->
          ( l1,
            l2,
            dprintf "layout %a" format l1,
            subjkind_format "is not" l2,
            None )
        | Some p ->
          ( l1,
            l2,
            dprintf "an unknown layout",
            subjkind_format "might not be" l2,
            Some p ))
      | { violation = No_intersection (l1, l2); missing_cmi } ->
        assert (Option.is_none missing_cmi);
        ( l1,
          l2,
          dprintf "layout %a" format l1,
          dprintf "does not overlap with %a" format l2,
          None )
    in
    if display_histories
    then
      let connective =
        match t.violation, get l2 with
        | Not_a_subjkind _, Const _ -> dprintf "be a sublayout of %a" format l2
        | No_intersection _, Const _ -> dprintf "overlap with %a" format l2
        | _, Var _ -> dprintf "be representable"
      in
      fprintf ppf "@[<v>%a@;%a@]"
        (format_history
           ~intro:(dprintf "The layout of %a is %a" pp_former former format l1))
        l1
        (format_history
           ~intro:
             (dprintf "But the layout of %a must %t" pp_former former connective))
        l2
    else
      fprintf ppf "@[<hov 2>%s%a has %t,@ which %t.@]" preamble pp_former former
        fmt_l1 fmt_l2;
    report_missing_cmi ppf missing_cmi_option

  let pp_t ppf x = fprintf ppf "%t" x

  let report_with_offender ~offender = report_general "" pp_t offender

  let report_with_offender_sort ~offender =
    report_general "A representable layout was expected, but " pp_t offender

  let report_with_name ~name = report_general "" pp_print_string name
end

(******************************)
(* relations *)

let equate_or_equal ~allow_mutation { jkind = jkind1; history = _ }
    { jkind = jkind2; history = _ } =
  Jkind_desc.equate_or_equal ~allow_mutation jkind1 jkind2

(* CR layouts v2.8: Switch this back to ~allow_mutation:false *)
let equal = equate_or_equal ~allow_mutation:true

let equate = equate_or_equal ~allow_mutation:true

(* Not all jkind history reasons are created equal. Some are more helpful than others.
    This function encodes that information.

    The reason with higher score should get preserved when combined with one of lower
    score. *)
let score_reason = function
  (* error_message annotated by the user should always take priority *)
  | Creation (Annotated (With_error_message _, _)) -> 1
  (* Concrete creation is quite vague, prefer more specific reasons *)
  | Creation (Concrete_creation _) -> -1
  | _ -> 0

let combine_histories reason lhs rhs =
  if flattened_histories
  then
    match Desc.sub (Jkind_desc.get lhs.jkind) (Jkind_desc.get rhs.jkind) with
    | Less -> lhs.history
    | Not_le ->
      rhs.history
      (* CR layouts: this will be wrong if we ever have a non-trivial meet in the layout lattice *)
    | Equal ->
      if score_reason lhs.history >= score_reason rhs.history
      then lhs.history
      else rhs.history
  else
    Interact
      { reason;
        lhs_jkind = lhs.jkind;
        lhs_history = lhs.history;
        rhs_jkind = rhs.jkind;
        rhs_history = rhs.history
      }

let intersection ~reason t1 t2 =
  match Jkind_desc.intersection t1.jkind t2.jkind with
  | None -> Error (Violation.of_ (No_intersection (t1, t2)))
  | Some jkind -> Ok { jkind; history = combine_histories reason t1 t2 }

(* this is hammered on; it must be fast! *)
let check_sub sub super = Jkind_desc.sub sub.jkind super.jkind

let sub sub super = Misc.Le_result.is_le (check_sub sub super)

let sub_or_error t1 t2 =
  if sub t1 t2 then Ok () else Error (Violation.of_ (Not_a_subjkind (t1, t2)))

let sub_with_history sub super =
  match check_sub sub super with
  | Less | Equal ->
    Ok { sub with history = combine_histories Subjkind sub super }
  | Not_le -> Error (Violation.of_ (Not_a_subjkind (sub, super)))

let is_void_defaulting = function
  | { jkind = { layout = Sort s; _ }; _ } -> Sort.is_void_defaulting s
  | _ -> false

let is_any jkind = match jkind.jkind.layout with Any -> true | _ -> false

(*********************************)
(* debugging *)

module Debug_printers = struct
  open Format

  let concrete_jkind_reason ppf : concrete_jkind_reason -> unit = function
    | Match -> fprintf ppf "Match"
    | Constructor_declaration idx ->
      fprintf ppf "Constructor_declaration %d" idx
    | Label_declaration lbl ->
      fprintf ppf "Label_declaration %a" Ident.print lbl
    | Unannotated_type_parameter path ->
      fprintf ppf "Unannotated_type_parameter %a" !printtyp_path path
    | Record_projection -> fprintf ppf "Record_projection"
    | Record_assignment -> fprintf ppf "Record_assignment"
    | Let_binding -> fprintf ppf "Let_binding"
    | Function_argument -> fprintf ppf "Function_argument"
    | Function_result -> fprintf ppf "Function_result"
    | Structure_item_expression -> fprintf ppf "Structure_item_expression"
    | External_argument -> fprintf ppf "External_argument"
    | External_result -> fprintf ppf "External_result"
    | Statement -> fprintf ppf "Statement"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Optional_arg_default -> fprintf ppf "Optional_arg_default"
    | Layout_poly_in_external -> fprintf ppf "Layout_poly_in_external"

  let rec annotation_context ppf : annotation_context -> unit = function
    | Type_declaration p -> fprintf ppf "Type_declaration %a" Path.print p
    | Type_parameter (p, var) ->
      fprintf ppf "Type_parameter (%a, %a)" Path.print p
        (Misc.Stdlib.Option.print Misc.Stdlib.String.print)
        var
    | Newtype_declaration name -> fprintf ppf "Newtype_declaration %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "Constructor_type_parameter (%a, %S)" Path.print cstr name
    | Univar name -> fprintf ppf "Univar %S" name
    | Type_variable name -> fprintf ppf "Type_variable %S" name
    | Type_wildcard loc ->
      fprintf ppf "Type_wildcard (%a)" Location.print_loc loc
    | With_error_message (message, context) ->
      fprintf ppf "With_error_message (%s, %a)" message annotation_context
        context

  let any_creation_reason ppf : any_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Type_expression_call -> fprintf ppf "Type_expression_call"
    | Inside_of_Tarrow -> fprintf ppf "Inside_of_Tarrow"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"

  let immediate_creation_reason ppf : immediate_creation_reason -> _ = function
    | Empty_record -> fprintf ppf "Empty_record"
    | Enumeration -> fprintf ppf "Enumeration"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf "Immediate_polymorphic_variant"

  let immediate64_creation_reason ppf = function
    | Separability_check -> fprintf ppf "Separability_check"

  let value_creation_reason ppf : value_creation_reason -> _ = function
    | Class_let_binding -> fprintf ppf "Class_let_binding"
    | Tuple_element -> fprintf ppf "Tuple_element"
    | Probe -> fprintf ppf "Probe"
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
    | Separability_check -> fprintf ppf "Separability_check"
    | Univar -> fprintf ppf "Univar"
    | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
    | Default_type_jkind -> fprintf ppf "Default_type_jkind"
    | Float_record_field -> fprintf ppf "Float_record_field"
    | Existential_type_variable -> fprintf ppf "Existential_type_variable"
    | Array_element -> fprintf ppf "Array_element"
    | Lazy_expression -> fprintf ppf "Lazy_expression"
    | Class_type_argument -> fprintf ppf "Class_type_argument"
    | Class_term_argument -> fprintf ppf "Class_term_argument"
    | Structure_element -> fprintf ppf "Structure_element"
    | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
    | V1_safety_check -> fprintf ppf "V1_safety_check"
    | Captured_in_object -> fprintf ppf "Captured_in_object"
    | Recmod_fun_arg -> fprintf ppf "Recmod_fun_arg"
    | Unknown s -> fprintf ppf "Unknown %s" s

  let float64_creation_reason ppf : float64_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let word_creation_reason ppf : word_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let bits32_creation_reason ppf : bits32_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let bits64_creation_reason ppf : bits64_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let creation_reason ppf : creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Immediate64_creation immediate64 ->
      fprintf ppf "Immediate64_creation %a" immediate64_creation_reason
        immediate64
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Float64_creation float ->
      fprintf ppf "Float64_creation %a" float64_creation_reason float
    | Word_creation word ->
      fprintf ppf "Word_creation %a" word_creation_reason word
    | Bits32_creation bits32 ->
      fprintf ppf "Bits32_creation %a" bits32_creation_reason bits32
    | Bits64_creation bits64 ->
      fprintf ppf "Bits64_creation %a" bits64_creation_reason bits64
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_jkind_reason concrete
    | Imported -> fprintf ppf "Imported"
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "Imported_type_argument (pos %d, arity %d) of %a" position
        arity !printtyp_path parent_path
    | Generalized (id, loc) ->
      fprintf ppf "Generalized (%s, %a)"
        (match id with Some id -> Ident.unique_name id | None -> "")
        Location.print_loc loc

  let interact_reason ppf = function
    | Gadt_equation p -> fprintf ppf "Gadt_equation %a" Path.print p
    | Tyvar_refinement_intersection ->
      fprintf ppf "Tyvar_refinement_intersection"
    | Subjkind -> fprintf ppf "Subjkind"

  let rec history ppf = function
    | Interact { reason; lhs_jkind; lhs_history; rhs_jkind; rhs_history } ->
      fprintf ppf
        "Interact {@[reason = %a;@ lhs_jkind = %a;@ lhs_history = %a;@ \
         rhs_jkind = %a;@ rhs_history = %a}@]"
        interact_reason reason Jkind_desc.Debug_printers.t lhs_jkind history
        lhs_history Jkind_desc.Debug_printers.t rhs_jkind history rhs_history
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  let t ppf ({ jkind; history = h } : t) : unit =
    fprintf ppf "@[<v 2>{ jkind = %a@,; history = %a }@]"
      Jkind_desc.Debug_printers.t jkind history h
end

(*** formatting user errors ***)
let report_error ~loc = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]" Pprintast.jkind jkind
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%s) and a layout attribute \
       ([@@@@%s]).@]"
      (Legacy.string_of_const from_annotation)
      (Legacy.string_of_const from_attribute)
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
        "@[<v>Layout %s is more experimental than allowed by the enabled \
         layouts extension.@;\
         %t@]"
        (Legacy.string_of_const jkind)
        hint)

let () =
  Location.register_error_of_exn (function
    | User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(* CR layouts v2.8: Remove the definitions below by propagating changes
   outside of this file. *)

type const = Legacy.const =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate
  | Float64
  | Word
  | Bits32
  | Bits64

type annotation = const * Jane_syntax.Jkind.annotation

let string_of_const = Legacy.string_of_const

let equal_const = Legacy.equal_const

type desc =
  | Const of const
  | Var of Sort.var

let to_legacy_desc : Desc.t -> desc = function
  | Const c -> Const (Const.to_legacy_jkind c)
  | Var v -> Var v

let get t = to_legacy_desc (get t)

let get_default_value t = Const.to_legacy_jkind (get_default_value t)
