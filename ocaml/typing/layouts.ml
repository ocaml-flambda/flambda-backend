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

  let of_const = function
    | Void -> void
    | Value -> value

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

  let default_value : t option = Some (Const Value)
  let default_void : t option = Some (Const Void)

  let[@inline] default = function
    | Value -> default_value
    | Void -> default_void

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
    | Value, Value -> Equal_no_mutation
    | Void, Value
    | Value, Void -> Unequal

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

  (* Pre-condition: all sort variables are filled in *)
  let rec is_void = function
    | Const Void -> true
    | Var v -> begin match !v with
               | None -> assert false
               | Some s -> is_void s
               end
    | Const Value -> false

  (*** pretty printing ***)

  let string_of_const = function
    | Value -> "value"
    | Void -> "void"

  let to_string s = match repr ~default:None s with
    | Var v -> var_name v
    | Const c -> string_of_const c

  let format ppf t = Format.fprintf ppf "%s" (to_string t)

  (*** defaulting ***)

  let get_defaulting ~default t =
    match repr ~default:(Some default) t with
    | Const result -> result
    | Var _ -> assert false

  let constrain_default_void = get_defaulting ~default:Void
  let can_make_void t = Void = constrain_default_void t

  let default_to_value t =
    ignore (get_defaulting ~default:Value t)

  (*** debug printing **)

  module Debug_printers = struct
    open Format

    let rec t ppf = function
      | Var v   -> fprintf ppf "Var %a" var v
      | Const c -> fprintf ppf (match c with
                                | Void  -> "Void"
                                | Value -> "Value")

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None   -> fprintf ppf "None"

    and var ppf v = fprintf ppf "{ contents = %a }" opt_t (!v)
  end
end

type sort = Sort.t

module Layout = struct
  (*** reasons for layouts **)

  type concrete_layout_reason =
    | Match
    | Constructor_declaration of int
    | Label_declaration of Ident.t
    | Unannotated_type_parameter
    | Record_projection
    | Record_assignment
    | Let_binding
    | Function_argument
    | Function_result

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
    | Type_argument
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
    | With_constraint_hack
    | V1_safety_check
    | Unknown of string

  type immediate_creation_reason =
    | Empty_record
    | Enumeration
    | Primitive of Ident.t
    | Immediate_polymorphic_variant
    | Gc_ignorable_check
    | Value_kind

  type immediate64_creation_reason =
    | Local_mode_cross_check
    | Gc_ignorable_check
    | Separability_check

  type void_creation_reason =
    | V1_safety_check

  type any_creation_reason =
    | Missing_cmi of Path.t
    | Wildcard
    | Unification_var
    | Initial_typedecl_env
    | Dummy_layout

  type annotation_context =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string
    | With_constraint of string
    | Newtype_declaration of string

  type creation_reason =
    | Annotated of annotation_context * Location.t
    | Value_creation of value_creation_reason
    | Immediate_creation of immediate_creation_reason
    | Immediate64_creation of immediate64_creation_reason
    | Void_creation of void_creation_reason
    | Any_creation of any_creation_reason
    | Concrete_creation of concrete_layout_reason
    | Imported

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
    | Dummy_layout -> any_dummy_layout  (* memoize this one common case *)
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

  type const = Asttypes.const_layout =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate

  let string_of_const : const -> _ = function
    | Any -> "any"
    | Value -> "value"
    | Void -> "void"
    | Immediate64 -> "immediate64"
    | Immediate -> "immediate"

  let equal_const (c1 : const) (c2 : const) = match c1, c2 with
    | Any, Any -> true
    | Immediate64, Immediate64 -> true
    | Immediate, Immediate -> true
    | Void, Void -> true
    | Value, Value -> true
    | (Any | Immediate64 | Immediate | Void | Value), _ -> false

  let sub_const (c1 : const) (c2 : const) = match c1, c2 with
    | Any, Any -> Equal
    | _, Any -> Sub
    | c1, c2 when equal_const c1 c2 -> Equal
    | (Immediate | Immediate64), Value -> Sub
    | Immediate, Immediate64 -> Sub
    | (Any | Void | Value | Immediate64 | Immediate), _ -> Not_sub

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

  let of_attributes ~legacy_immediate ~reason attrs =
    match Builtin_attributes.layout ~legacy_immediate attrs with
    | Ok None as a -> a
    | Ok (Some l) -> Ok (Some (of_const ~why:(Annotated (reason, l.loc))
                                 l.txt))
    | Error _ as e -> e

  let of_attributes_default ~legacy_immediate ~reason ~default attrs =
    match of_attributes ~legacy_immediate ~reason attrs with
    | Ok None -> Ok default
    | Ok (Some l) -> Ok l
    | Error _ as e -> e

  let for_boxed_record ~all_void =
    if all_void then immediate ~why:Empty_record else value ~why:Boxed_record

  let for_boxed_variant ~all_voids =
    if all_voids then immediate ~why:Enumeration else value ~why:Boxed_variant

  (******************************)
  (* elimination and defaulting *)

  (* The description of a layout. Do not store this for any length of time;
     it is meant to use just to inspect a layout and then be discarded. *)
  type desc =
    | Const of const
    | Var of Sort.var   (* this variable is never filled in *)

  let format_desc ppf = let open Format in function
    | Const c -> fprintf ppf "%s" (string_of_const c)
    | Var v -> fprintf ppf "%s" (Sort.var_name v)

  (* considers sort variables < Any, but otherwise just checks for equality.
     Never does mutation. *)
  let sub_desc d1 d2 = match d1, d2 with
    | Const c1, Const c2 -> sub_const c1 c2
    | Var _, Const Any -> Sub
    | Var v1, Var v2 -> if v1 == v2 then Equal else Not_sub
    | Const _, Var _ | Var _, Const _ -> Not_sub

  let get_internal ~default (lay : internal) : desc = match lay with
    | Any -> Const Any
    | Immediate -> Const Immediate
    | Immediate64 -> Const Immediate64
    | Sort s -> begin match Sort.get s with
      (* NB: this match isn't as silly as it looks: those are
         different constructors on the left than on the right *)
      | Const Void -> Const Void
      | Const Value -> Const Value
      | Var v -> Var v
    end

  let get_default_value (t : t) : const = match t.layout with
    | Any _ -> Any
    | Immediate -> Immediate
    | Immediate64 -> Immediate64
    | Sort s -> begin match Sort.get_default_value s with
      (* As above, this turns Sort.consts to Layout.consts *)
      | Value -> Value
      | Void -> Void
    end

  let default_to_value t = ignore (get_default_value t)

  let is_void t = Void = get_default_value t

  let get t = get_internal t.layout

  (* CR layouts: this function is suspect; it seems likely to reisenberg
     that refactoring could get rid of it *)
  let sort_of_layout l =
    match get l with
    | Const Void -> Sort.void
    | Const (Value | Immediate | Immediate64) -> Sort.value
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
        fprintf ppf "@,No .cmi file found containing %a." !printtyp_path p;
        missing_cmi_hint ppf p
      | None -> ()
  end

  include Report_missing_cmi

  (* This module is just to keep all the helper functions more locally
     scoped. *)
  module Format_history : sig
    val format_history :
      intro:(Format.formatter -> unit) -> Format.formatter -> t -> unit
  end = struct
    (* CR layouts: all the output in this section is subject to change;
       actually look closely at error messages once this is activated *)

    open Format

    let format_concrete_layout_reason ppf : concrete_layout_reason -> unit =
      function
      | Match ->
        fprintf ppf "matched on"
      | Constructor_declaration idx ->
        fprintf ppf "used as constructor field %d" idx
      | Label_declaration lbl ->
        fprintf ppf "used in the declaration of the record field \"%a\""
          Ident.print lbl
      | Unannotated_type_parameter ->
        fprintf ppf "appears as an unannotated type parameter"
      | Record_projection ->
        fprintf ppf "used as the record in a projection"
      | Record_assignment ->
        fprintf ppf "used as the record in an assignment"
      | Let_binding ->
        fprintf ppf "bound by a `let`"
      | Function_argument ->
        fprintf ppf "used as a function argument"
      | Function_result ->
        fprintf ppf "used as a function result"

    let format_annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "the declaration of the type %a"
            !printtyp_path p
      | Type_parameter (path, var) ->
          fprintf ppf "@[%s@ in the declaration of the type@ %a@]"
            var
            !printtyp_path path
      | With_constraint s ->
          fprintf ppf "the `with` constraint for %s" s
      | Newtype_declaration name ->
          fprintf ppf "the abstract type declaration for %s"
            name

    let format_any_creation_reason ppf : any_creation_reason -> unit = function
      | Missing_cmi p ->
         fprintf ppf "a missing .cmi file for %a" !printtyp_path p
      | Wildcard ->
         fprintf ppf "a _ in a type"
      | Unification_var ->
         fprintf ppf "a fresh unification variable"
      | Initial_typedecl_env ->
         fprintf ppf "a dummy layout used in checking mutually recursive datatypes"
      | Dummy_layout ->
         fprintf ppf "@[a dummy layout that should have been overwritten;@ \
                      Please notify the Jane Street compilers group if you see this output."
    (* CR layouts: Improve output or remove this constructor *)

    let format_immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record ->
         fprintf ppf "a record containing all void elements"
      | Enumeration ->
         fprintf ppf "an enumeration variant (all constructors are constant)"
      | Primitive id ->
         fprintf ppf "the primitive immediate type %s" (Ident.name id)
      | Immediate_polymorphic_variant ->
         fprintf ppf "an immediate polymorphic variant"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"
      | Value_kind ->
         fprintf ppf
           "the check to see whether a polymorphic variant is immediate"

    let format_immediate64_creation_reason ppf = function
      | Local_mode_cross_check ->
         fprintf ppf "the check for whether a local value can safely escape"
      | Gc_ignorable_check ->
         fprintf ppf "the check to see whether a value can be ignored by GC"
      | Separability_check ->
         fprintf ppf "the check that a type is definitely not `float`"

    let format_value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "let-bound in a class expression"
      | Function_argument -> fprintf ppf "a function argument"
      | Function_result -> fprintf ppf "a function result"
      | Tuple_element -> fprintf ppf "a tuple element"
      | Probe -> fprintf ppf "a probe"
      | Package_hack -> fprintf ppf "used as an element in a first-class module"
      | Object -> fprintf ppf "an object"
      | Instance_variable -> fprintf ppf "an instance variable"
      | Object_field -> fprintf ppf "an object field"
      | Class_field -> fprintf ppf "an class field"
      | Boxed_record -> fprintf ppf "a boxed record"
      | Boxed_variant -> fprintf ppf "a boxed variant"
      | Extensible_variant -> fprintf ppf "an extensible variant"
      | Primitive id -> fprintf ppf "the primitive value type %s" (Ident.name id)
      | Type_argument -> fprintf ppf "a type argument defaulted to have layout value"
      | Tuple -> fprintf ppf "a tuple type"
      | Row_variable -> fprintf ppf "a row variable"
      | Polymorphic_variant -> fprintf ppf "a polymorphic variant"
      | Arrow -> fprintf ppf "a function type"
      | Tfield -> fprintf ppf "an internal Tfield type (you shouldn't see this)"
      | Tnil -> fprintf ppf "an internal Tnil type (you shouldn't see this)"
      | First_class_module -> fprintf ppf "a first-class module type"
      | Separability_check ->
        fprintf ppf "the check that a type is definitely not `float`"
      | Univar -> fprintf ppf "an unannotated universal variable"
      | Polymorphic_variant_field -> fprintf ppf "a field of a polymorphic variant"
      | Default_type_layout ->
        fprintf ppf "the default layout for an abstract type"
      | Float_record_field ->
        fprintf ppf "a field of a float record"
      | Existential_type_variable ->
        fprintf ppf "an unannotated existential type variable"
      | Array_element ->
        fprintf ppf "an array element"
      | Lazy_expression ->
        fprintf ppf "a lazy expression"
      | Class_argument ->
        fprintf ppf "a term-level argument to a class constructor"
      | Structure_element ->
         fprintf ppf "stored in a module structure"
      | Debug_printer_argument ->
         fprintf ppf "used as the argument to a debugger printer function"
      | With_constraint_hack ->
         fprintf ppf "used in a `with` constraint"
      | V1_safety_check ->
          fprintf ppf "to be value for the V1 safety check"
      | Unknown s -> fprintf ppf "unknown @[(please alert the Jane Street@;\
                       compilers team with this message: %s)@]" s


    let format_void_creation_reason ppf : void_creation_reason -> _ = function
      | V1_safety_check -> fprintf ppf "check to make sure there are no voids"
        (* CR layouts: remove this when we remove its uses *)

    let format_creation_reason ppf : creation_reason -> unit = function
      | Annotated (ctx, _) ->
          fprintf ppf "the annotation on %a" format_annotation_context ctx
      | Any_creation any ->
         format_any_creation_reason ppf any
      | Immediate_creation immediate ->
         format_immediate_creation_reason ppf immediate
      | Immediate64_creation immediate64 ->
         format_immediate64_creation_reason ppf immediate64
      | Void_creation void ->
        format_void_creation_reason ppf void
      | Value_creation value ->
         format_value_creation_reason ppf value
      | Concrete_creation concrete ->
         format_concrete_layout_reason ppf concrete
      | Imported ->
         fprintf ppf "imported from another compilation unit"

    let format_interact_reason ppf = function
      | Gadt_equation name ->
        fprintf ppf "a GADT match on the constructor %a" !printtyp_path name
      | Tyvar_refinement_intersection ->
        fprintf ppf "updating a type variable"
      | Sublayout ->
        fprintf ppf "sublayout check"

    (* CR layouts: should this be configurable? In the meantime, you
       may want to change these to experiment / debug. *)

    (* should we print histories at all? *)
    let display_histories = false

    (* should we print histories in a way users can understand?
       The alternative is to print out all the data, which may be useful
       during debugging. *)
    let flattened_histories = true

    (* a flattened_history describes the history of a layout L. That
       layout has been constrained to be a sublayout of layouts L1..Ln.
       Each element in a flattened_history includes a layout desc Li and the
       set of circumstances that gave rise to a constraint of that layout.
       Any layouts Lk such that an Li < Lk doesn't contribute to the choice
       of L and is thus omitted from a flattened_history.

       INVARIANT: the creation_reasons within a list all are reasons for
       the layout they are paired with.
       INVARIANT: L is a sublayout of all the Li in a flattened_history.
       INVARIANT: If Li and Lj are stored in different entries in a
       flattened_history, then not (Li <= Lj) and not (Lj <= Li).
       This implies that no two elements in a flattened_history have the
       same layout in them.
       INVARIANT: no list in this structure is empty

       Both levels of list are unordered.

       Because a flattened_history stores [desc]s, it should be discarded
       promptly after use.

       This type could be more efficient in several ways, but there is
       little incentive to do so. *)
    type flattened_row = desc * creation_reason list
    type flattened_history = flattened_row list

    (* first arg is the layout L whose history we are flattening *)
    let flatten_history : internal -> history -> flattened_history =
      let add layout reason =
        let layout_desc = get_internal layout in
        let rec go acc = function
          | ((key, value) as row) :: rest ->
            begin match sub_desc layout_desc key with
            | Sub -> go acc rest
            | Equal -> (key, reason :: value) :: acc @ rest
            | Not_sub -> go (row :: acc) rest
            end
          | [] -> (layout_desc, [reason]) :: acc
        in
        go []
      in
      let rec history acc internal = function
        | Interact { reason = _
                   ; lhs_layout
                   ; lhs_history
                   ; rhs_layout
                   ; rhs_history } ->
          let fh1 = history acc lhs_layout lhs_history in
          let fh2 = history fh1 rhs_layout rhs_history in
          fh2
        | Creation reason ->
          add internal reason acc
      in
      history []

    let format_flattened_row ppf (lay, reasons) =
      fprintf ppf "%a, due to" format_desc lay;
      match reasons with
      | [reason] -> fprintf ppf "@ %a." format_creation_reason reason
      | _ ->
          fprintf ppf " all of the following:@ @[<v 2>  %a@]"
            (pp_print_list format_creation_reason) reasons

    let format_flattened_history ~intro ppf t =
      let fh = flatten_history t.layout t.history in
      fprintf ppf "@;@[<v 2>%t " intro;
      begin match fh with
      | [row] -> format_flattened_row ppf row
      | _ -> fprintf ppf "a sublayout of all of the following:@ @[<v 2>  %a@]"
               (pp_print_list format_flattened_row) fh
      end;
      fprintf ppf "@]"

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

    type nonrec t =
      | Not_a_sublayout of t * t
      | No_intersection of t * t
      | Missing_cmi of Path.t * t * t

    let report_general preamble pp_former former ppf t =
      let sublayout_format verb l2 = match get l2 with
        | Var _ -> dprintf "%s representable" verb
        | Const _ -> dprintf "%s a sublayout of %a" verb format l2
      in
      let l1, l2, fmt_l1, fmt_l2, missing_cmi_option = match t with
        | Not_a_sublayout(l1, l2) ->
          l1, l2,
          dprintf "layout %a" format l1,
          sublayout_format "is not" l2, None
        | No_intersection(l1, l2) ->
          l1, l2,
          dprintf "layout %a" format l1,
          dprintf "does not overlap with %a" format l2, None
        | Missing_cmi(p, l1, l2) ->
          l1, l2,
          dprintf "an unknown layout",
          sublayout_format "might not be" l2, Some p
      in
      fprintf ppf "@[<v>@[<hov 2>%s%a has %t,@ which %t.@]%a%a@]"
        preamble
        pp_former former
        fmt_l1
        fmt_l2
        (format_history ~intro:(dprintf "The layout of %a is" pp_former former)) l1
        (format_history ~intro:(dprintf "But the layout of %a is required to be" pp_former former)) l2;
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

  (* CR layouts v2: Switch this back to ~allow_mutation:false *)
  let equal = equate_or_equal ~allow_mutation:true

  let equate = equate_or_equal ~allow_mutation:true

  let combine_histories reason lhs rhs =
    Interact { reason; lhs_layout = lhs.layout; lhs_history = lhs.history;
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
      else Error (Violation.No_intersection (l1, l2))
    | Sort s, (Immediate | Immediate64) ->
      if Sort.equate s Sort.value
      then Ok l2
      else Error (Violation.No_intersection (l1, l2))
    | Sort s1, Sort s2 ->
      if Sort.equate s1 s2
      then Ok { l1 with history = combine_histories reason l1 l2 }
      else Error (Violation.No_intersection (l1, l2))

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
    | Not_sub -> Error (Violation.Not_a_sublayout (sub, super))

  let sub_with_history sub super = match check_sub sub super with
    | Sub | Equal ->
      Ok { sub with history = combine_histories Sublayout sub super }
    | Not_sub -> Error (Violation.Not_a_sublayout (sub, super))

  let is_void = function
    | { layout = Sort s } -> Sort.is_void s
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
      | Unannotated_type_parameter ->
          fprintf ppf "Unannotated_type_parameter"
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

    let annotation_context ppf : annotation_context -> unit = function
      | Type_declaration p ->
          fprintf ppf "Type_declaration %a" Path.print p
      | Type_parameter (p, var) ->
          fprintf ppf "Type_parameter (%a, %S)" Path.print p var
      | With_constraint s ->
          fprintf ppf "With_constraint %S" s
      | Newtype_declaration name ->
          fprintf ppf "Newtype_declaration %s" name

    let any_creation_reason ppf : any_creation_reason -> unit = function
      | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
      | Wildcard -> fprintf ppf "Wildcard"
      | Unification_var -> fprintf ppf "Unification_var"
      | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
      | Dummy_layout -> fprintf ppf "Dummy_layout"

    let immediate_creation_reason ppf : immediate_creation_reason -> _ =
      function
      | Empty_record -> fprintf ppf "Empty_record"
      | Enumeration -> fprintf ppf "Enumeration"
      | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
      | Immediate_polymorphic_variant ->
        fprintf ppf "Immediate_polymorphic_variant"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"
      | Value_kind -> fprintf ppf "Value_kind"

    let immediate64_creation_reason ppf = function
      | Local_mode_cross_check -> fprintf ppf "Local_mode_cross_check"
      | Gc_ignorable_check -> fprintf ppf "Gc_ignorable_check"
      | Separability_check -> fprintf ppf "Separability_check"

    let value_creation_reason ppf : value_creation_reason -> _ = function
      | Class_let_binding -> fprintf ppf "Class_let_binding"
      | Function_argument -> fprintf ppf "Function_argument"
      | Function_result -> fprintf ppf "Function_result"
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
      | Type_argument -> fprintf ppf "Type_argument"
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
      | With_constraint_hack -> fprintf ppf "With_constraint_hack"
      | V1_safety_check -> fprintf ppf "V1_safety_check"
      | Unknown s -> fprintf ppf "Unknown %s" s

    let void_creation_reason ppf : void_creation_reason -> _ = function
      | V1_safety_check -> fprintf ppf "V1_safety_check"

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
      | Void_creation void ->
         fprintf ppf "Void_creation %a" void_creation_reason void
      | Concrete_creation concrete ->
         fprintf ppf "Concrete_creation %a" concrete_layout_reason concrete
      | Imported ->
         fprintf ppf "Imported"

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
end

type layout = Layout.t
