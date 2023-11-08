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

type sub_result =
  | Equal
  | Sub
  | Not_sub

(* Layouts *)

module Sort = struct
  type const =
    | Void
    | Value
    | Float64

  type t =
    | Var of var
    | Const of const
  and var = t option ref

  let var_name : var -> string =
    let next_id = ref 1 in
    let named = ref [] in
    fun v ->
      match List.assq_opt v (!named) with
      | Some name -> name
      | None ->
          let id = !next_id in
          let name = "'_representable_layout_" ^ Int.to_string id in
          next_id := id + 1;
          named := (v, name) :: !named;
          name

  let void = Const Void
  let value = Const Value
  let float64 = Const Float64

  let some_value = Some value

  let of_const = function
    | Void -> void
    | Value -> value
    | Float64 -> float64

  let of_var v = Var v

  let new_var () = Var (ref None)

  let rec get : t -> t = function
    | Const _ as t -> t
    | Var r as t -> begin match !r with
      | None -> t
      | Some s -> begin
          let result = get s in
          if result != s then r := Some result; (* path compression *)
          result
        end
      end

  (* This is constant-time if [var] was just returned from a previous call to
     [get]. That's because [var] will always be [None] in that case.
  *)
  let var_constraint : var -> const option = fun r ->
    match !r with
    | None -> None
    | Some t -> begin
        match get t with
        | Const const -> Some const
        | Var { contents = None } -> None
        | Var _ ->
            Misc.fatal_error "[get] should return [Const _] or [Var None]"
      end

  let default_value : t option = Some (Const Value)
  let default_void : t option = Some (Const Void)
  let default_float64 : t option = Some (Const Float64)

  let[@inline] default = function
    | Value -> default_value
    | Void -> default_void
    | Float64 -> default_float64

  let rec get_default_value : t -> const = function
    | Const c -> c
    | Var r -> begin match !r with
      | None -> r := default_value; Value
      | Some s -> begin
          let result = get_default_value s in
          r := default result; (* path compression *)
          result
        end
    end

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

  let equal_const_const c1 c2 = match c1, c2 with
    | Void, Void
    | Value, Value
    | Float64, Float64 -> Equal_no_mutation
    | (Void | Value | Float64), _ -> Unequal

  let rec equate_var_const v1 c2 = match !v1 with
    | Some s1 -> equate_sort_const s1 c2
    | None -> v1 := Some (of_const c2); Equal_mutated_first

  and equate_var v1 s2 = match s2 with
    | Const c2 -> equate_var_const v1 c2
    | Var v2 -> equate_var_var v1 v2

  and equate_var_var v1 v2 =
    if v1 == v2 then
      Equal_no_mutation
    else begin
      match !v1, !v2 with
      | Some s1, _ -> swap_equate_result (equate_var v2 s1)
      | _, Some s2 -> equate_var v1 s2
      | None, None -> v1 := Some (of_var v2); Equal_mutated_first
    end

  and equate_sort_const s1 c2 = match s1 with
    | Const c1 -> equal_const_const c1 c2
    | Var v1 -> equate_var_const v1 c2

  let equate_tracking_mutation s1 s2 = match s1 with
    | Const c1 -> swap_equate_result (equate_sort_const s2 c1)
    | Var v1 -> equate_var v1 s2

  (* Don't expose whether or not mutation happened; we just need that for [Layout] *)
  let equate s1 s2 = match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation -> true

  let equal_const c1 c2 =
    match c1, c2 with
    | Void, Void
    | Value, Value
    | Float64, Float64 -> true
    | Void, (Value | Float64)
    | Value, (Void | Float64)
    | Float64, (Value | Void) -> false

  let rec is_void_defaulting = function
    | Const Void -> true
    | Var v -> begin match !v with
      (* CR layouts v5: this should probably default to void now *)
               | None -> v := some_value; false
               | Some s -> is_void_defaulting s
               end
    | Const Value -> false
    | Const Float64 -> false

  (*** pretty printing ***)

  let string_of_const = function
    | Value -> "value"
    | Void -> "void"
    | Float64 -> "float64"

  let to_string s = match get s with
    | Var v -> var_name v
    | Const c -> string_of_const c

  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  (*** debug printing **)

  module Debug_printers = struct
    open Format

    let rec t ppf = function
      | Var v   -> fprintf ppf "Var %a" var v
      | Const c -> fprintf ppf (match c with
                                | Void  -> "Void"
                                | Value -> "Value"
                                | Float64 -> "Float64")

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None   -> fprintf ppf "None"

    and var ppf v = fprintf ppf "{ contents = %a }" opt_t (!v)
  end

  let for_function = value
  let for_predef_value = value
  let for_block_element = value
  let for_probe_body = value
  let for_poly_variant = value
  let for_record = value
  let for_record_field = value
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

module Layout = struct
  (*** reasons for layouts **)
  type concrete_layout_reason =
    | Match
    | Constructor_declaration of int
    | Label_declaration of Ident.t
    | Unannotated_type_parameter of string option
    | Record_projection
    | Record_assignment
    | Let_binding
    | Function_argument
    | Function_result
    | Structure_item_expression
    | V1_safety_check
    | External_argument
    | External_result
    | Statement

  type value_creation_reason =
    | Class_let_binding
    | Tuple_element
    | Probe
    | Package_hack
    | Object
    | Instance_variable
    | Object_field
    | Class_field
    | Boxed_record
    | Boxed_variant
    | Extensible_variant
    | Primitive of Ident.t
    | Type_argument of {parent_path: Path.t; position: int; arity: int}
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
    | Default_type_layout
    | Float_record_field
    | Existential_type_variable
    | Array_element
    | Lazy_expression
    | Class_argument
    | Structure_element
    | Debug_printer_argument
    | V1_safety_check
    | Captured_in_object
    | Unknown of string

  type immediate_creation_reason =
    | Empty_record
    | Enumeration
    | Primitive of Ident.t
    | Immediate_polymorphic_variant
    | Gc_ignorable_check

  type immediate64_creation_reason =
    | Local_mode_cross_check
    | Gc_ignorable_check
    | Separability_check

  type void_creation_reason = |

  type any_creation_reason =
    | Missing_cmi of Path.t
    | Wildcard
    | Unification_var
    | Initial_typedecl_env
    | Dummy_layout
    | Type_expression_call

  type float64_creation_reason =
    | Primitive of Ident.t

  type annotation_context =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string option
    | Newtype_declaration of string
    | Constructor_type_parameter of Path.t * string
    | Univar of string
    | Type_variable of string
    | Type_wildcard of Location.t

  type creation_reason =
    | Annotated of annotation_context * Location.t
    | Value_creation of value_creation_reason
    | Immediate_creation of immediate_creation_reason
    | Immediate64_creation of immediate64_creation_reason
    | Void_creation of void_creation_reason
    | Any_creation of any_creation_reason
    | Float64_creation of float64_creation_reason
    | Concrete_creation of concrete_layout_reason
    | Imported
    | Imported_type_argument of {parent_path: Path.t; position: int; arity: int}
    (* [position] is 1-indexed *)
    | Generalized of Ident.t option * Location.t

  type interact_reason =
    | Gadt_equation of Path.t
    | Tyvar_refinement_intersection
    (* CR layouts: this needs to carry a type_expr, but that's loopy *)
    | Sublayout

  (*** actual layout types ***)

  type internal =
    | Any
    | Sort of sort
    | Immediate64
    (** We know for sure that values of types of this layout are always immediate
        on 64-bit platforms. For other platforms, we know nothing about immediacy.
    *)
    | Immediate

  (* A history of conditions placed on a layout.

     INVARIANT: at most one sort variable appears in this history.
     This is a natural consequence of producing this history by comparing
     layouts.
  *)
  type history =
    | Interact of { reason : interact_reason
                  ; lhs_layout : internal
                  ; lhs_history : history
                  ; rhs_layout : internal
                  ; rhs_history : history
                  }
    | Creation of creation_reason

  type t =
    { layout : internal
    ; history : history }

  let fresh_layout layout ~why = { layout; history = Creation why }

  (******************************)
  (* constants *)

  let any_dummy_layout =
    { layout = Any; history = Creation (Any_creation Dummy_layout) }
  let value_v1_safety_check =
    { layout = Sort Sort.value;
      history = Creation (Value_creation V1_safety_check) }

  let any ~why = match why with
    | Dummy_layout -> any_dummy_layout  (* share this one common case *)
    | _ -> fresh_layout Any ~why:(Any_creation why)
  let void ~why =
    fresh_layout (Sort Sort.void) ~why:(Void_creation why)
  let value ~(why : value_creation_reason) = match why with
    | V1_safety_check -> value_v1_safety_check
    | _ -> fresh_layout (Sort Sort.value) ~why:(Value_creation why)
  let immediate64 ~why =
    fresh_layout Immediate64 ~why:(Immediate64_creation why)
  let immediate ~why =
    fresh_layout Immediate ~why:(Immediate_creation why)
  let float64 ~why =
    fresh_layout (Sort Sort.float64) ~why:(Float64_creation why)

  type const = Jane_asttypes.const_layout =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate
    | Float64

  let string_of_const : const -> _ = function
    | Any -> "any"
    | Value -> "value"
    | Void -> "void"
    | Immediate64 -> "immediate64"
    | Immediate -> "immediate"
    | Float64 -> "float64"

  let equal_const (c1 : const) (c2 : const) = match c1, c2 with
    | Any, Any -> true
    | Immediate64, Immediate64 -> true
    | Immediate, Immediate -> true
    | Void, Void -> true
    | Value, Value -> true
    | Float64, Float64 -> true
    | (Any | Immediate64 | Immediate | Void | Value | Float64), _ -> false

  let sub_const (c1 : const) (c2 : const) = match c1, c2 with
    | Any, Any -> Equal
    | _, Any -> Sub
    | c1, c2 when equal_const c1 c2 -> Equal
    | (Immediate | Immediate64), Value -> Sub
    | Immediate, Immediate64 -> Sub
    | (Any | Void | Value | Immediate64 | Immediate | Float64), _ -> Not_sub

  (******************************)
  (*** user errors ***)
  type error =
    | Insufficient_level of annotation_context * Jane_asttypes.const_layout

  exception User_error of Location.t * error

  let raise ~loc err = raise (User_error(loc, err))

  (*** extension requirements ***)
  let get_required_layouts_level
        (context : annotation_context) (layout : const) :
        Language_extension.maturity =
    match context, layout with
    | _, Value -> Stable
    | _, (Immediate | Immediate64 | Any) -> Beta
    | _, (Void | Float64) -> Alpha

  (******************************)
  (* construction *)

  let of_new_sort_var ~why =
    fresh_layout (Sort (Sort.new_var ())) ~why:(Concrete_creation why)

  let of_sort ~why s =
    fresh_layout (Sort s) ~why:(Concrete_creation why)

  let of_const ~why : const -> t = function
    | Any -> fresh_layout Any ~why
    | Immediate -> fresh_layout Immediate ~why
    | Immediate64 -> fresh_layout Immediate64 ~why
    | Value -> fresh_layout (Sort Sort.value) ~why
    | Void -> fresh_layout (Sort Sort.void) ~why
    | Float64 -> fresh_layout (Sort Sort.float64) ~why

  (* CR layouts v1.5: remove legacy_immediate *)
  let of_annotation ?(legacy_immediate=false) ~context Location.{ loc; txt = const } =
    begin match const with
    | Immediate | Immediate64 | Value when legacy_immediate -> ()
    | _ ->
      let required_layouts_level = get_required_layouts_level context const in
      if not (Language_extension.is_at_least Layouts required_layouts_level)
      then raise ~loc (Insufficient_level (context, const))
    end;
    of_const ~why:(Annotated (context, loc)) const

  let of_annotation_option ?legacy_immediate ~context =
    Option.map (of_annotation ?legacy_immediate ~context)

  let of_annotation_option_default ?legacy_immediate ~default ~context =
    Option.fold ~none:default ~some:(of_annotation ?legacy_immediate ~context)

  let of_attributes ~legacy_immediate ~context attrs =
    Builtin_attributes.layout ~legacy_immediate attrs |>
    Result.map (of_annotation_option ~legacy_immediate ~context)

  let of_attributes_default ~legacy_immediate ~context ~default attrs =
    Builtin_attributes.layout ~legacy_immediate attrs |>
    Result.map (of_annotation_option_default ~legacy_immediate ~default ~context)

  let for_boxed_record ~all_void =
    if all_void then immediate ~why:Empty_record else value ~why:Boxed_record

  let for_boxed_variant ~all_voids =
    if all_voids then immediate ~why:Enumeration else value ~why:Boxed_variant

  (******************************)
  (* elimination and defaulting *)

  type desc =
    | Const of const
    | Var of Sort.var

  let format_desc ppf = let open Format in function
    | Const c -> fprintf ppf "%s" (string_of_const c)
    | Var v -> fprintf ppf "%s" (Sort.var_name v)

  (* considers sort variables < Any, but otherwise just checks for equality.
     Never does mutation.
     Pre-condition: no filled-in sort variables. *)
  let sub_desc d1 d2 = match d1, d2 with
    | Const c1, Const c2 -> sub_const c1 c2
    | Var _, Const Any -> Sub
    | Var v1, Var v2 -> if v1 == v2 then Equal else Not_sub
    | Const _, Var _ | Var _, Const _ -> Not_sub

  let get_internal (lay : internal) : desc = match lay with
    | Any -> Const Any
    | Immediate -> Const Immediate
    | Immediate64 -> Const Immediate64
    | Sort s -> begin match Sort.get s with
      (* NB: this match isn't as silly as it looks: those are
         different constructors on the left than on the right *)
      | Const Void -> Const Void
      | Const Value -> Const Value
      | Const Float64 -> Const Float64
      | Var v -> Var v
    end

  let get_default_value (t : t) : const = match t.layout with
    | Any -> Any
    | Immediate -> Immediate
    | Immediate64 -> Immediate64
    | Sort s -> begin match Sort.get_default_value s with
      (* As above, this turns Sort.consts to Layout.consts *)
      | Value -> Value
      | Void -> Void
      | Float64 -> Float64
    end

  let default_to_value t = ignore (get_default_value t)

  let get t = get_internal t.layout

  (* CR layouts: this function is suspect; it seems likely to reisenberg
     that refactoring could get rid of it *)
  let sort_of_layout l =
    match get l with
    | Const Void -> Sort.void
    | Const (Value | Immediate | Immediate64) -> Sort.value
    | Const Float64 -> Sort.float64
    | Const Any -> Misc.fatal_error "Layout.sort_of_layout"
    | Var v -> Sort.of_var v

  (*********************************)
  (* pretty printing *)

  let to_string lay = match get lay with
    | Const c -> string_of_const c
    | Var v -> Sort.var_name v

  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  (***********************************)
  (* layout histories *)
  let has_imported_history t =
    match t with
    | {history = Creation Imported; } -> true
    | _ -> false

  let update_reason t reason =
    {t with history = Creation reason}

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
        | Pdot _ as p -> Some begin
            match root_module_name p with
            | "Location" | "Longident" -> "ocamlcommon"
            | mn -> mn
                    |> String.lowercase_ascii
                    |> delete_trailing_double_underscore
          end
        | Pident _ | Papply _ ->
            None
      in
      Option.iter
        (fprintf ppf "@,Hint: Adding \"%s\" to your dependencies might help.")
        (guess_library_name type_path)

    let report_missing_cmi ppf = function
      | Some p ->
        fprintf ppf "@,@[No .cmi file found containing %a.%a@]"
          !printtyp_path p
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

    let format_position ~arity position =
      let to_ordinal num = Int.to_string num ^ Misc.ordinal_suffix num in
      match arity with
      | 1 -> ""
      | _ -> to_ordinal position ^ " "

    let format_concrete_layout_reason ppf : concrete_layout_reason -> unit =
      function
      | Match ->
        fprintf ppf "a value of this type is matched against a pattern"
      | Constructor_declaration _ ->
        fprintf ppf "it's used as the type of a constructor field"
      | Label_declaration lbl ->
        fprintf ppf "it is the type of record field %s"
          (Ident.name lbl)
      | Unannotated_type_parameter name_opt ->
        let suffix = match name_opt with None -> "" | Some n -> " of " ^ n in
        fprintf ppf "it instantiates an unannotated type parameter%s" suffix
      | Record_projection ->
        fprintf ppf "it's used as the record in a projection"
      | Record_assignment ->
        fprintf ppf "it's used as the record in an assignment"
      | Let_binding ->
        fprintf ppf "it's the type of a variable bound by a `let`"
      | Function_argument ->
        fprintf ppf "it's used as a function argument"
      | Function_result ->
        fprintf ppf "it's used as a function result"
      | Structure_item_expression ->
        fprintf ppf "it's used as the type of an expression in a structure"
      | V1_safety_check ->
        fprintf ppf "it's part of the v1 safety check"
      | External_argument ->
        fprintf ppf "it's used as an argument in an external declaration"
      | External_result ->
        fprintf ppf "it's used as the result of an external declaration"
      | Statement ->
        fprintf ppf "it's used as a statement"

    let format_annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "the declaration of the type %a"
            !printtyp_path p
      | Type_parameter (path, var) ->
          let var_string = match var with None -> "_" | Some v -> "'" ^ v in
          fprintf ppf "@[%s@ in the declaration of the type@ %a@]"
            var_string
            !printtyp_path path
      | Newtype_declaration name ->
          fprintf ppf "the abstract type declaration for %s"
            name
      | Constructor_type_parameter (cstr, name) ->
          fprintf ppf "@[%s@ in the declaration of constructor@ %a@]"
            name
            !printtyp_path cstr
      | Univar name ->
          fprintf ppf "the universal variable %s"
            name
      | Type_variable name ->
          fprintf ppf "the type variable %s" name
      | Type_wildcard loc ->
          fprintf ppf "the wildcard _ at %a" Location.print_loc_in_lowercase loc

    let format_any_creation_reason ppf : any_creation_reason -> unit = function
      | Missing_cmi p ->
         fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
      | Wildcard ->
        fprintf ppf "there's a _ in the type"
      | Unification_var ->
         fprintf ppf "it's a fresh unification variable"
      | Initial_typedecl_env ->
         fprintf ppf "a dummy layout of any is used to check mutually recursive datatypes"
      | Dummy_layout ->
         fprintf ppf "@[it's assigned a dummy layout that should have been overwritten;@ \
                      Please notify the Jane Street compilers group if you see this output.@]"
      (* CR layouts: Improve output or remove this constructor ^^ *)
      | Type_expression_call ->
         fprintf ppf "there's a call to [type_expression] via the ocaml API"

    let format_immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record ->
         fprintf ppf "it's a record containing all void elements"
      | Enumeration ->
         fprintf ppf "it's an enumeration variant (all constructors are constant)"
      | Primitive id ->
         fprintf ppf "it is the primitive immediate type %s" (Ident.name id)
      | Immediate_polymorphic_variant ->
         fprintf ppf "it's an enumeration variant (all constructors are constant)"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"

    let format_immediate64_creation_reason ppf = function
      | Local_mode_cross_check ->
         fprintf ppf "the check for whether a local value can safely escape"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"
      | Separability_check ->
         fprintf ppf "the check that a type is definitely not `float`"

    let format_value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "it's let-bound in a class expression"
      | Tuple_element -> fprintf ppf "it's a tuple element"
      | Probe -> fprintf ppf "it's a probe"
      | Package_hack -> fprintf ppf "it's used as an element in a first-class module"
      | Object -> fprintf ppf "it's an object"
      | Instance_variable -> fprintf ppf "it's an instance variable"
      | Object_field -> fprintf ppf "it's an object field"
      | Class_field -> fprintf ppf "it's an class field"
      | Boxed_record -> fprintf ppf "it's a boxed record"
      | Boxed_variant -> fprintf ppf "it's a boxed variant"
      | Extensible_variant -> fprintf ppf "it's an extensible variant"
      | Primitive id ->
        fprintf ppf "it is the primitive value type %s" (Ident.name id)
      | Type_argument {parent_path; position; arity} ->
        fprintf ppf "the %stype argument of %a has layout value"
          (format_position ~arity position)
          !printtyp_path parent_path
      | Tuple -> fprintf ppf "it's a tuple type"
      | Row_variable -> fprintf ppf "it's a row variable"
      | Polymorphic_variant -> fprintf ppf "it's a polymorphic variant"
      | Arrow -> fprintf ppf "it's a function type"
      | Tfield -> fprintf ppf "it's an internal Tfield type (you shouldn't see this)"
      | Tnil -> fprintf ppf "it's an internal Tnil type (you shouldn't see this)"
      | First_class_module -> fprintf ppf "it's a first-class module type"
      | Separability_check ->
        fprintf ppf "the check that a type is definitely not `float`"
      | Univar -> fprintf ppf "it's an unannotated universal variable"
      | Polymorphic_variant_field -> fprintf ppf "it's a field of a polymorphic variant"
      | Default_type_layout ->
        fprintf ppf "an abstract type has the value layout by default"
      | Float_record_field ->
        fprintf ppf "it's a field of a float record"
      | Existential_type_variable ->
        fprintf ppf "it's an unannotated existential type variable"
      | Array_element ->
        fprintf ppf "it's an array element"
      | Lazy_expression ->
        fprintf ppf "it's a lazy expression"
      | Class_argument ->
        fprintf ppf "it's a term-level argument to a class constructor"
      | Structure_element ->
         fprintf ppf "it's stored in a module structure"
      | Debug_printer_argument ->
         fprintf ppf "it's used as the argument to a debugger printer function"
      | V1_safety_check ->
          fprintf ppf "it has to be value for the V1 safety check"
      | Captured_in_object -> fprintf ppf "it's captured in an object"
      | Unknown s -> fprintf ppf "unknown @[(please alert the Jane Street@;\
                       compilers team with this message: %s)@]" s

    let format_float64_creation_reason ppf : float64_creation_reason -> _ = function
      | Primitive id ->
        fprintf ppf "it is the primitive float64 type %s" (Ident.name id)

    let format_creation_reason ppf : creation_reason -> unit = function
      | Annotated (ctx, _) ->
          fprintf ppf "of the annotation on %a" format_annotation_context ctx
      | Any_creation any ->
         format_any_creation_reason ppf any
      | Immediate_creation immediate ->
         format_immediate_creation_reason ppf immediate
      | Immediate64_creation immediate64 ->
         format_immediate64_creation_reason ppf immediate64
      | Void_creation _ -> .
      | Value_creation value ->
         format_value_creation_reason ppf value
      | Float64_creation float ->
         format_float64_creation_reason ppf float
      | Concrete_creation concrete ->
         format_concrete_layout_reason ppf concrete
      | Imported ->
         fprintf ppf "of layout requirements from an imported definition"
      | Imported_type_argument {parent_path; position; arity} ->
        fprintf ppf "the %stype argument of %a has this layout"
          (format_position ~arity position)
          !printtyp_path parent_path
      | Generalized (id, loc) ->
        let format_id ppf = function
          | Some id -> fprintf ppf " of %s" (Ident.name id)
          | None -> () in
        fprintf ppf "of the definition%a at %a"
          format_id id Location.print_loc_in_lowercase loc

    let format_interact_reason ppf = function
      | Gadt_equation name ->
        fprintf ppf "a GADT match on the constructor %a" !printtyp_path name
      | Tyvar_refinement_intersection ->
        fprintf ppf "updating a type variable"
      | Sublayout ->
        fprintf ppf "sublayout check"

    (* CR layouts: An older implementation of format_flattened_history existed
       which displays more information not limited to one layout and one creation_reason
       around commit 66a832d70bf61d9af3b0ec6f781dcf0a188b324d in main.

       Consider revisiting that if the current implementation becomes insufficient. *)

    let format_flattened_history ~intro ppf t =
      let lay = get_internal t.layout in
      fprintf ppf "@[<v 2>%t %a"
        intro
        format_desc lay;
      begin match t.history with
      | Creation reason ->
        fprintf ppf ", because@ %a" format_creation_reason reason;
        begin match reason, lay with
        | Concrete_creation _, Const _ ->
          fprintf ppf ", defaulted to layout %a"
            format_desc lay
        | _ -> ()
        end
      | _ -> assert false
      end;
      fprintf ppf ".@]"


    (* this isn't really formatted for user consumption *)
    let format_history_tree ~intro ppf t =
      let rec in_order ppf = function
        | Interact { reason; lhs_history; rhs_history } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]"
          in_order lhs_history
          format_interact_reason reason
          in_order rhs_history
        | Creation c ->
          format_creation_reason ppf c
      in
      fprintf ppf "@;%t has this layout history:@;@[<v 2>  %a@]"
        intro
        in_order t.history

    let format_history ~intro ppf t =
      if display_histories then begin
        if flattened_histories
        then format_flattened_history ~intro ppf t
        else format_history_tree ~intro ppf t
      end
  end

  include Format_history

  (******************************)
  (* errors *)

  module Violation = struct
    open Format

    type violation =
      | Not_a_sublayout of t * t
      | No_intersection of t * t

    type nonrec t = { violation : violation
                    ; missing_cmi : Path.t option }
       (* [missing_cmi]: is this error a result of a missing cmi file?
          This is stored separately from the [violation] because it's
          used to change the behavior of [value_kind], and we don't
          want that function to inspect something that is purely about
          the choice of error message. (Though the [Path.t] payload *is*
          indeed just about the payload.) *)

    let of_ violation = { violation; missing_cmi = None }

    let record_missing_cmi ~missing_cmi_for t = { t with missing_cmi = Some missing_cmi_for }

    let is_missing_cmi { missing_cmi } = Option.is_some missing_cmi

    let report_general preamble pp_former former ppf t =
      let sublayout_format verb l2 = match get l2 with
        | Var _ -> dprintf "%s representable" verb
        | Const _ -> dprintf "%s a sublayout of %a" verb format l2
      in
      let l1, l2, fmt_l1, fmt_l2, missing_cmi_option = match t with
        | { violation = Not_a_sublayout(l1, l2); missing_cmi } ->
          begin match missing_cmi with
          | None ->
            l1, l2,
            dprintf "layout %a" format l1,
            sublayout_format "is not" l2, None
          | Some p ->
            l1, l2,
            dprintf "an unknown layout",
            sublayout_format "might not be" l2, Some p
          end
        | { violation = No_intersection(l1, l2); missing_cmi } ->
          assert (Option.is_none missing_cmi);
          l1, l2,
          dprintf "layout %a" format l1,
          dprintf "does not overlap with %a" format l2, None
      in
      if display_histories then begin
        let connective = match t.violation with
          | Not_a_sublayout _ -> "be a sublayout of"
          | No_intersection _ -> "overlap with"
        in
        fprintf ppf "@[<v>%a@;%a@]"
          (format_history ~intro:(dprintf "The layout of %a is" pp_former former)) l1
          (format_history ~intro:(dprintf "But the layout of %a must %s" pp_former former connective)) l2;
      end else begin
        fprintf ppf "@[<hov 2>%s%a has %t,@ which %t.@]"
          preamble
          pp_former former
          fmt_l1
          fmt_l2
      end;
      report_missing_cmi ppf missing_cmi_option

    let pp_t ppf x = fprintf ppf "%t" x

    let report_with_offender ~offender =
      report_general "" pp_t offender

    let report_with_offender_sort ~offender =
      report_general "A representable layout was expected, but " pp_t offender

    let report_with_name ~name =
      report_general "" pp_print_string name
  end

  (******************************)
  (* relations *)

  let equate_or_equal ~allow_mutation (l1 : t) (l2 : t) =
    match l1.layout, l2.layout with
    | Any, Any -> true
    | Immediate64, Immediate64 -> true
    | Immediate, Immediate -> true
    | Sort s1, Sort s2 -> begin
        match Sort.equate_tracking_mutation s1 s2 with
        | (Equal_mutated_first | Equal_mutated_second)
          when not allow_mutation ->
            Misc.fatal_errorf
              "Layouts.Layout.equal: Performed unexpected mutation"
        | Unequal -> false
        | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second -> true
      end
    | (Any | Immediate64 | Immediate | Sort _), _ -> false

  (* CR layouts v2.8: Switch this back to ~allow_mutation:false *)
  let equal = equate_or_equal ~allow_mutation:true

  let equate = equate_or_equal ~allow_mutation:true

  let combine_histories reason lhs rhs =
    if flattened_histories
    then begin match sub_desc (get_internal lhs.layout) (get_internal rhs.layout) with
      | Sub -> lhs.history
      | Not_sub -> rhs.history  (* CR layouts: this will be wrong if we ever have a non-trivial meet in the layout lattice *)
      | Equal -> begin match lhs.history, rhs.history with
        (* Prefer other creation_reasons over Concrete_creation *)
        | h, Creation (Concrete_creation _)
        | Creation (Concrete_creation _), h -> h
        | h, _ -> h
        end
    end else Interact { reason; lhs_layout = lhs.layout; lhs_history = lhs.history;
                        rhs_layout = rhs.layout; rhs_history = rhs.history }

  let intersection ~reason l1 l2 =
    match l1.layout, l2.layout with
    (* only update the history when something interesting happens; e.g.
       finding the intersection between a sublayout and its superlayout
       is not interesting *)
    | _, Any -> Ok l1
    | Any, _ -> Ok l2
    | (Immediate, Immediate) | (Immediate64, Immediate64) ->
      Ok { l1 with history = combine_histories reason l1 l2 }
    | Immediate, Immediate64 -> Ok l1
    | Immediate64, Immediate -> Ok l2
    | (Immediate | Immediate64), Sort s ->
      if Sort.equate s Sort.value
      then Ok l1
      else Error (Violation.of_ (No_intersection (l1, l2)))
    | Sort s, (Immediate | Immediate64) ->
      if Sort.equate s Sort.value
      then Ok l2
      else Error (Violation.of_ (No_intersection (l1, l2)))
    | Sort s1, Sort s2 ->
      if Sort.equate s1 s2
      then Ok { l1 with history = combine_histories reason l1 l2 }
      else Error (Violation.of_ (No_intersection (l1, l2)))

  (* this is hammered on; it must be fast! *)
  let check_sub sub super : sub_result =
    match sub.layout, super.layout with
      (* don't use [get], because that allocates *)
    | Any, Any -> Equal
    | _, Any -> Sub
    | Immediate, Immediate -> Equal
    | Immediate64, Immediate64 -> Equal
    | Immediate, Immediate64 -> Sub
    | Immediate64, Immediate -> Not_sub
    | (Immediate | Immediate64), Sort s ->
      if Sort.equate s Sort.value then Sub else Not_sub
    | Sort s1, Sort s2 ->
      if Sort.equate s1 s2 then Equal else Not_sub
    | Any, _ -> Not_sub
    | Sort _, (Immediate | Immediate64) -> Not_sub

  let sub sub super = match check_sub sub super with
    | Sub | Equal -> Ok ()
    | Not_sub -> Error (Violation.of_ (Not_a_sublayout (sub, super)))

  let sub_with_history sub super = match check_sub sub super with
    | Sub | Equal ->
      Ok { sub with history = combine_histories Sublayout sub super }
    | Not_sub -> Error (Violation.of_ (Not_a_sublayout (sub, super)))

  let is_void_defaulting = function
    | { layout = Sort s } -> Sort.is_void_defaulting s
    | _ -> false

  let is_any = function
    | { layout = Any } -> true
    | _ -> false

  (*********************************)
  (* debugging *)

  module Debug_printers = struct
    open Format

    let internal ppf : internal -> unit = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" Sort.Debug_printers.t s
      | Immediate64 -> fprintf ppf "Immediate64"
      | Immediate -> fprintf ppf "Immediate"

    let concrete_layout_reason ppf : concrete_layout_reason -> unit = function
      | Match ->
          fprintf ppf "Match"
      | Constructor_declaration idx ->
          fprintf ppf "Constructor_declaration %d" idx
      | Label_declaration lbl ->
          fprintf ppf "Label_declaration %a" Ident.print lbl
      | Unannotated_type_parameter name_opt ->
          fprintf ppf "Unannotated_type_parameter %a"
            (Misc.Stdlib.Option.print Misc.Stdlib.String.print) name_opt
      | Record_projection ->
          fprintf ppf "Record_projection"
      | Record_assignment ->
          fprintf ppf "Record_assignment"
      | Let_binding ->
          fprintf ppf "Let_binding"
      | Function_argument ->
          fprintf ppf "Function_argument"
      | Function_result ->
          fprintf ppf "Function_result"
      | Structure_item_expression ->
          fprintf ppf "Structure_item_expression"
      | V1_safety_check ->
          fprintf ppf "V1_safety_check"
      | External_argument ->
          fprintf ppf "External_argument"
      | External_result ->
          fprintf ppf "External_result"
      | Statement ->
          fprintf ppf "Statement"

    let annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "Type_declaration %a" Path.print p
      | Type_parameter (p, var) ->
          fprintf ppf "Type_parameter (%a, %a)"
            Path.print p
            (Misc.Stdlib.Option.print Misc.Stdlib.String.print) var
      | Newtype_declaration name ->
          fprintf ppf "Newtype_declaration %s" name
      | Constructor_type_parameter (cstr, name) ->
          fprintf ppf "Constructor_type_parameter (%a, %S)" Path.print cstr name
      | Univar name ->
          fprintf ppf "Univar %S" name
      | Type_variable name ->
          fprintf ppf "Type_variable %S" name
      | Type_wildcard loc ->
          fprintf ppf "Type_wildcard (%a)" Location.print_loc loc

    let any_creation_reason ppf : any_creation_reason -> unit = function
      | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
      | Wildcard -> fprintf ppf "Wildcard"
      | Unification_var -> fprintf ppf "Unification_var"
      | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
      | Dummy_layout -> fprintf ppf "Dummy_layout"
      | Type_expression_call -> fprintf ppf "Type_expression_call"

    let immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record -> fprintf ppf "Empty_record"
      | Enumeration -> fprintf ppf "Enumeration"
      | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
      | Immediate_polymorphic_variant ->
        fprintf ppf "Immediate_polymorphic_variant"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"

    let immediate64_creation_reason ppf = function
      | Local_mode_cross_check -> fprintf ppf "Local_mode_cross_check"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"
      | Separability_check -> fprintf ppf "Separability_check"

    let value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "Class_let_binding"
      | Tuple_element -> fprintf ppf "Tuple_element"
      | Probe -> fprintf ppf "Probe"
      | Package_hack -> fprintf ppf "Package_hack"
      | Object -> fprintf ppf "Object"
      | Instance_variable -> fprintf ppf "Instance_variable"
      | Object_field -> fprintf ppf "Object_field"
      | Class_field -> fprintf ppf "Class_field"
      | Boxed_record -> fprintf ppf "Boxed_record"
      | Boxed_variant -> fprintf ppf "Boxed_variant"
      | Extensible_variant -> fprintf ppf "Extensible_variant"
      | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
      | Type_argument {parent_path; position; arity} ->
        fprintf ppf "Type_argument (pos %d, arity %d) of %a"
          position arity !printtyp_path parent_path
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
      | Default_type_layout -> fprintf ppf "Default_type_layout"
      | Float_record_field -> fprintf ppf "Float_record_field"
      | Existential_type_variable -> fprintf ppf "Existential_type_variable"
      | Array_element -> fprintf ppf "Array_element"
      | Lazy_expression -> fprintf ppf "Lazy_expression"
      | Class_argument -> fprintf ppf "Class_argument"
      | Structure_element -> fprintf ppf "Structure_element"
      | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
      | V1_safety_check -> fprintf ppf "V1_safety_check"
      | Captured_in_object -> fprintf ppf "Captured_in_object"
      | Unknown s -> fprintf ppf "Unknown %s" s

    let float64_creation_reason ppf : float64_creation_reason -> _ = function
      | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

    let creation_reason ppf : creation_reason -> unit = function
      | Annotated (ctx, loc) ->
        fprintf ppf "Annotated (%a,%a)"
          annotation_context ctx
          Location.print_loc loc
      | Any_creation any ->
         fprintf ppf "Any_creation %a" any_creation_reason any
      | Immediate_creation immediate ->
         fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
      | Immediate64_creation immediate64 ->
         fprintf ppf "Immediate64_creation %a" immediate64_creation_reason immediate64
      | Value_creation value ->
         fprintf ppf "Value_creation %a" value_creation_reason value
      | Void_creation _ -> .
      | Float64_creation float ->
         fprintf ppf "Float64_creation %a" float64_creation_reason float
      | Concrete_creation concrete ->
         fprintf ppf "Concrete_creation %a" concrete_layout_reason concrete
      | Imported ->
         fprintf ppf "Imported"
      | Imported_type_argument {parent_path; position; arity}  ->
           fprintf ppf "Imported_type_argument (pos %d, arity %d) of %a"
           position arity !printtyp_path parent_path
      | Generalized (id, loc) ->
        fprintf ppf "Generalized (%s, %a)"
          (match id with
           | Some id -> Ident.unique_name id
           | None -> "")
           Location.print_loc loc

    let interact_reason ppf = function
      | Gadt_equation p ->
        fprintf ppf "Gadt_equation %a"
          Path.print p
      | Tyvar_refinement_intersection ->
        fprintf ppf "Tyvar_refinement_intersection"
      | Sublayout ->
        fprintf ppf "Sublayout"

    let rec history ppf = function
      | Interact
          { reason; lhs_layout; lhs_history; rhs_layout; rhs_history } ->
        fprintf ppf "Interact {@[reason = %a;@ \
                     lhs_layout = %a;@ \
                     lhs_history = %a;@ \
                     rhs_layout = %a;@ \
                     rhs_history = %a}@]"
          interact_reason reason
          internal lhs_layout
          history lhs_history
          internal rhs_layout
          history rhs_history
      | Creation c ->
        fprintf ppf "Creation (%a)"
          creation_reason c

    let t ppf ({ layout; history=h } : t) : unit =
      fprintf ppf "@[<v 2>{ layout = %a@,; history = %a }@]"
        internal layout
        history h
  end

  (*** formatting user errors ***)
  let report_error ~loc = function
  | Insufficient_level (context, layout) ->
    let required_layouts_level = get_required_layouts_level context layout in
    let hint ppf =
      Format.fprintf ppf "You must enable -extension %s to use this feature."
        (Language_extension.to_command_line_string
           Layouts required_layouts_level)
    in
    match Language_extension.get_command_line_string_if_enabled Layouts with
    | None ->
      Location.errorf ~loc
        "@[<v>The appropriate layouts extension is not enabled.@;%t@]"
        hint
    | Some cmd_line_string ->
      Location.errorf ~loc
        (* CR layouts errors: use the context to produce a better error message.
           When RAE tried this, some types got printed like [t/2], but the
           [/2] shouldn't be there. Investigate and fix. *)
        "@[<v>Layout %s is more experimental than allowed by -extension %s.@;%t@]"
        (string_of_const layout)
        cmd_line_string
        hint

  let () =
    Location.register_error_of_exn
      (function
        | User_error(loc, err) ->
          Some (report_error ~loc err)
        | _ -> None)
end

type layout = Layout.t
