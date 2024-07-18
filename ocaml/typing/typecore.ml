(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typechecking for the core language *)

open Misc
open Asttypes
open Parsetree
open Types
open Mode
open Typedtree
open Btype
open Ctype
open Uniqueness_analysis

type comprehension_type =
  | List_comprehension
  | Array_comprehension of mutability

type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | When_guard
  | Comprehension_in_iterator of comprehension_type
  | Comprehension_for_start
  | Comprehension_for_stop
  | Comprehension_when
  | Error_message_attr of string

type type_expected = {
  ty: type_expr;
  explanation: type_forcing_context option;
}

module Datatype_kind = struct
  type t = Record | Variant

  let type_name = function
    | Record -> "record"
    | Variant -> "variant"

  let label_name = function
    | Record -> "field"
    | Variant -> "constructor"
end

type wrong_name = {
  type_path: Path.t;
  kind: Datatype_kind.t;
  name: string loc;
  valid_names: string list;
}

type wrong_kind_context =
  | Pattern
  | Expression of type_forcing_context option

type wrong_kind_sort =
  | Constructor
  | Record
  | Boolean
  | List
  | Unit

type contains_gadt =
  | Contains_gadt
  | No_gadt

let wrong_kind_sort_of_constructor (lid : Longident.t) =
  match lid with
  | Lident "true" | Lident "false" | Ldot(_, "true") | Ldot(_, "false") ->
      Boolean
  | Lident "[]" | Lident "::" | Ldot(_, "[]") | Ldot(_, "::") -> List
  | Lident "()" | Ldot(_, "()") -> Unit
  | _ -> Constructor

type existential_restriction =
  | At_toplevel (** no existential types at the toplevel *)
  | In_group (** nor with let ... and ... *)
  | In_rec (** or recursive definition *)
  | With_attributes (** or let[@any_attribute] = ... *)
  | In_class_args (** or in class arguments *)
  | In_class_def  (** or in [class c = let ... in ...] *)
  | In_self_pattern (** or in self pattern *)

type submode_reason =
  | Application of type_expr
  | Other

type contention_context =
  | Read_mutable
  | Write_mutable

type error =
  | Constructor_arity_mismatch of Longident.t * int * int
  | Constructor_labeled_arg
  | Partial_tuple_pattern_bad_type
  | Extra_tuple_label of string option * type_expr
  | Missing_tuple_label of string option * type_expr
  | Label_mismatch of Longident.t * Errortrace.unification_error
  | Pattern_type_clash :
      Errortrace.unification_error * Parsetree.pattern_desc option -> error
  | Or_pattern_type_clash of Ident.t * Errortrace.unification_error
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of
      Errortrace.unification_error * type_forcing_context option
      * Parsetree.expression_desc option
  | Function_arity_type_clash of
      { syntactic_arity :  int;
        type_constraint : type_expr;
        trace : Errortrace.unification_error;
      }
  (* [Function_arity_type_clash { syntactic_arity = n; type_constraint; trace }]
     is the type error for the specific case where an n-ary function is
     constrained at a type with an arity less than n, e.g.:
     {[
       type (_, _) eq = Eq : ('a, 'a) eq
       let bad : type a. ?opt:(a, int -> int) eq -> unit -> a =
         fun ?opt:(Eq = assert false) () x -> x + 1
     ]}
     [type_constraint] is the user-written polymorphic type (in this example
     [?opt:(a, int -> int) eq -> unit -> a]) that causes this type clash, and
     [trace] is the unification error that signaled the issue.
  *)
  | Apply_non_function of {
      funct : Typedtree.expression;
      func_ty : type_expr;
      res_ty : type_expr;
      previous_arg_loc : Location.t;
      extra_arg_loc : Location.t;
    }
  | Apply_wrong_label of arg_label * type_expr * bool
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Longident.t
  | Wrong_name of string * type_expected * wrong_name
  | Name_type_mismatch of
      Datatype_kind.t * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Not_an_object of type_expr * type_forcing_context option
  | Not_a_value of Jkind.Violation.t * type_forcing_context option
  | Undefined_method of type_expr * string * string list option
  | Undefined_self_method of string * string list
  | Virtual_class of Longident.t
  | Private_type of type_expr
  | Private_label of Longident.t * type_expr
  | Private_constructor of constructor_description * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of string
  | Not_subtype of Errortrace.Subtype.error
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      Errortrace.expanded_type * Errortrace.unification_error * bool
  | Not_a_function of type_expr * type_forcing_context option
  | Too_many_arguments of type_expr * type_forcing_context option
  | Abstract_wrong_label of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      ; explanation   : type_forcing_context option
      }
  | Scoping_let_module of string * type_expr
  | Not_a_polymorphic_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * Errortrace.unification_error
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Unexpected_existential of existential_restriction * string * string list
  | Invalid_interval
  | Invalid_for_loop_index
  | Invalid_comprehension_for_range_iterator_index
  | No_value_clauses
  | Exception_pattern_disallowed
  | Mixed_value_and_exception_patterns_under_guard
  | Inlined_record_escape
  | Inlined_record_expected
  | Unrefuted_pattern of pattern
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Probe_format
  | Probe_name_format of string
  | Probe_name_undefined of string
  | Probe_is_enabled_format
  | Extension_not_enabled : _ Language_extension.t -> error
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Float32_literal of string
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Letop_type_clash of string * Errortrace.unification_error
  | Andop_type_clash of string * Errortrace.unification_error
  | Bindings_type_clash of Errortrace.unification_error
  | Unbound_existential of Ident.t list * type_expr
  | Missing_type_constraint
  | Wrong_expected_kind of wrong_kind_sort * wrong_kind_context * type_expr
  | Expr_not_a_record_type of type_expr
  | Submode_failed of
      Value.error * submode_reason *
      Env.closure_context option *
      contention_context option *
      Env.shared_context option
  | Local_application_complete of arg_label * [`Prefix|`Single_arg|`Entire_apply]
  | Param_mode_mismatch of Alloc.equate_error
  | Uncurried_function_escapes of Alloc.error
  | Local_return_annotation_mismatch of Location.t
  | Function_returns_local
  | Tail_call_local_returning
  | Bad_tail_annotation of [`Conflict|`Not_a_tailcall]
  | Optional_poly_param
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Unboxed_int_literals_not_supported
  | Function_type_not_rep of type_expr * Jkind.Violation.t
  | Modes_on_pattern
  | Invalid_label_for_src_pos of arg_label
  | Nonoptional_call_pos_label of string

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

type in_function =
  { ty_fun: type_expected;
    loc_fun: Location.t;
    (** [region_locked] is whether the function has its own region. *)
    region_locked: bool;
  }

let error_of_filter_arrow_failure ~explanation ~first ty_fun
  : filter_arrow_failure -> _ = function
  | Unification_error unif_err ->
      Expr_type_clash(unif_err, explanation, None)
  | Label_mismatch { got; expected; expected_type} ->
      Abstract_wrong_label { got; expected; expected_type; explanation }
  | Not_a_function -> begin
      if first
      then Not_a_function(ty_fun, explanation)
      else Too_many_arguments(ty_fun, explanation)
    end
  | Jkind_error (ty, err) -> Function_type_not_rep (ty, err)

(* Forward declaration, to be filled in by Typemod.type_module *)

let type_module =
  ref ((fun _env _md -> assert false) :
       Env.t -> Parsetree.module_expr -> Typedtree.module_expr * Shape.t)

(* Forward declaration, to be filled in by Typemod.type_open *)

let type_open :
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref =
  ref (fun ?used_slot:_ _ -> assert false)

let type_open_decl :
  (?used_slot:bool ref -> Env.t -> Parsetree.open_declaration
   -> open_declaration * Types.signature * Env.t)
    ref =
  ref (fun ?used_slot:_ _ -> assert false)

(* Forward declaration, to be filled in by Typemod.type_package *)

let type_package =
  ref (fun _ -> assert false)

(* Forward declaration, to be filled in by Typeclass.class_structure *)
let type_object =
  ref (fun _env _s -> assert false :
       Env.t -> Location.t -> Parsetree.class_structure ->
         Typedtree.class_structure * string list)

(*
  Saving and outputting type information.
  We keep these function names short, because they have to be
  called each time we create a record of type [Typedtree.expression]
  or [Typedtree.pattern] that will end up in the typed AST.
*)
let re node =
  Cmt_format.add_saved_type (Cmt_format.Partial_expression node);
  node

let rp node =
  Cmt_format.add_saved_type (Cmt_format.Partial_pattern (Value, node));
  node

let rcp node =
  Cmt_format.add_saved_type (Cmt_format.Partial_pattern (Computation, node));
  node


(* Context for inline record arguments; see [type_ident] *)

type recarg =
  | Allowed
  | Required
  | Rejected

let probe_name_max_length = 100
let check_probe_name name loc env =
  if String.length name > probe_name_max_length then
    Location.prerr_warning loc (Warnings.Probe_name_too_long name);
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> ()
    | _ -> raise (Error (loc, env, (Probe_name_format name)))
  ) name

let mk_expected ?explanation ty = { ty; explanation; }

let case lhs rhs =
  {c_lhs = lhs; c_guard = None; c_rhs = rhs}

type position_in_function = FTail | FNontail


type position_in_region =
  (* not the tail of a region*)
  | RNontail
  (* tail of a region,
     together with the mode of that region,
     and whether it is also the tail of a function
     (for tail call escape detection) *)
  | RTail of Regionality.r * position_in_function

(* CR mode-hint: unify the mode error hinting. *)
type expected_mode =
  { position : position_in_region;

    closure_context : Env.closure_context option;
    (** Explains why regionality axis of [mode] is low. *)

    contention_context : contention_context option;
    (** Explains why contention axis of [mode] is low. *)

    mode : Value.r;
    (** The upper bound, hence r (right) *)

    strictly_local : bool;
    (** Indicates that the expression was directly annotated with [local], which
    should force any allocations to be on the stack. No invariant between this
    field and [mode]: this field being [true] while [mode] being [global] is
    sensible, but not very useful as it will fail all expressions. *)

    tuple_modes : Value.r list;
    (** For t in tuple_modes, t <= regional_to_global mode *)
  }

type position_and_mode = {
  apply_position : apply_position;
  (** Runtime tail call behaviour of the application *)
  region_mode : Regionality.r option;
  (** INVARIANT: [Some m] iff [apply_position] is [Tail], where [m] is the mode
     of the surrounding region *)
}

let position_and_mode_default = {
  apply_position = Default;
  region_mode = None;
}

(** Decides the runtime tail call behaviour based on lexical structures and user
    annotation. *)
let position_and_mode env (expected_mode : expected_mode) sexp
  : position_and_mode =
  let fail err =
    raise (Error (sexp.pexp_loc, env, Bad_tail_annotation err))
  in
  let requested =
    match Builtin_attributes.tailcall sexp.pexp_attributes with
    | Ok r -> r
    | Error `Conflict -> fail `Conflict
  in
  match expected_mode.position with
  | RTail (m ,FTail) -> begin
      match requested with
      | Some `Tail | Some `Tail_if_possible | None ->
          {apply_position = Tail; region_mode = Some m}
      | Some `Nontail -> {apply_position = Nontail; region_mode = None}
    end
  | RNontail | RTail(_, FNontail) -> begin
      match requested with
      | None | Some `Tail_if_possible ->
          {apply_position = Default; region_mode = None}
      | Some `Nontail -> {apply_position = Nontail; region_mode = None}
      | Some `Tail -> fail `Not_a_tailcall
  end

(* ap_mode is the return mode of the current application *)
let check_tail_call_local_returning loc env ap_mode {region_mode; _} =
  match region_mode with
  | Some region_mode -> begin
    (* This application will be performed after the current region is closed; if
       ap_mode is local, the application allocates in the outer
       region, and thus [region_mode] needs to be marked local as well*)
      match
        Regionality.submode (locality_as_regionality ap_mode) region_mode
      with
      | Ok () -> ()
      | Error _ -> raise (Error (loc, env, Tail_call_local_returning))
    end
  | None -> ()

let meet_regional mode =
  let mode = Value.disallow_left mode in
  Value.meet [mode; (Value.max_with (Comonadic Areality) Regionality.regional)]

let value_regional_to_local mode =
  mode
  |> value_to_alloc_r2l
  |> alloc_as_value

let mode_default mode =
  { position = RNontail;
    closure_context = None;
    contention_context = None;
    mode = Value.disallow_left mode;
    strictly_local = false;
    tuple_modes = [] }

let mode_legacy = mode_default Value.legacy

let mode_modality modality expected_mode =
  expected_mode.mode
  |> Modality.Value.Const.apply modality
  |> mode_default

(* used when entering a function;
mode is the mode of the function region *)
let mode_return mode =
  { (mode_default (meet_regional mode)) with
    position = RTail (Regionality.disallow_left
      (Value.proj (Comonadic Areality) mode), FTail);
    closure_context = Some Return;
  }

(* used when entering a region.*)
let mode_region mode =
  { (mode_default (meet_regional mode)) with
    position =
      RTail (Regionality.disallow_left
        (Value.proj (Comonadic Areality) mode), FNontail);
    closure_context = None;
  }

let mode_max =
  mode_default Value.max

let mode_with_position mode position =
  { (mode_default mode) with position }

let mode_max_with_position position =
  { mode_max with position }

let mode_exclave expected_mode =
  let mode =
    Value.join_with (Comonadic Areality)
      Regionality.Const.Local expected_mode.mode
  in
  { (mode_default mode)
    with strictly_local = true
  }

let mode_strictly_local expected_mode =
  { expected_mode
    with strictly_local = true
  }

let mode_coerce mode expected_mode =
  let mode = Value.meet [expected_mode.mode; mode] in
  { expected_mode with mode; tuple_modes = [] }

let mode_tailcall_function mode =
  { (mode_default mode) with
    closure_context = Some Tailcall_function }

let mode_tailcall_argument mode =
  { (mode_default mode) with
    closure_context = Some Tailcall_argument }


let mode_partial_application expected_mode =
  let mode = alloc_as_value (value_to_alloc_r2g expected_mode.mode) in
  { expected_mode with
    mode;
    closure_context = Some Partial_application }


let mode_trywith expected_mode =
  { expected_mode with position = RNontail }

let mode_tuple mode tuple_modes =
  let tuple_modes = Value.List.disallow_left tuple_modes in
  { (mode_default mode) with
    tuple_modes }

(** Takes [marg:Alloc.lr] extracted from the arrow type and returns the real
mode of argument, after taking into consideration partial application and
tail-call. Returns [expected_mode] and [Value.lr] which are backed by the same
mode variable. We encode extra position information in the former. We need the
latter to the both left and right mode because of how it will be used. *)
let mode_argument ~funct ~index ~position_and_mode ~partial_app marg =
  let vmode , _ = Value.newvar_below (alloc_as_value marg) in
  if partial_app then mode_default vmode, vmode
  else match funct.exp_desc, index, position_and_mode.apply_position with
  | Texp_ident (_, _, {val_kind =
      Val_prim {Primitive.prim_name = ("%sequor"|"%sequand")}},
                Id_prim _, _), 1, Tail ->
     (* RHS of (&&) and (||) is at the tail of function region if the
        application is. The argument mode is not constrained otherwise. *)
     mode_with_position vmode (RTail (Option.get position_and_mode.region_mode, FTail)),
     vmode
  | Texp_ident (_, _, _, Id_prim _, _), _, _ ->
     (* Other primitives cannot be tail-called *)
     mode_default vmode, vmode
  | _, _, (Nontail | Default) ->
     mode_default vmode, vmode
  | _, _, Tail -> begin
    Regionality.submode_exn (Value.proj (Comonadic Areality) vmode)
      Regionality.regional;
    mode_tailcall_argument vmode, vmode
  end

(* expected_mode.closure_context explains why expected_mode.mode is low;
   shared_context explains why mode.uniqueness is high *)
let submode ~loc ~env ?(reason = Other) ?shared_context mode expected_mode =
  let res =
    match expected_mode.tuple_modes with
    | [] -> Value.submode mode expected_mode.mode
    | ts -> Value.submode mode (Value.meet ts)
  in
  match res with
  | Ok () -> ()
  | Error failure_reason ->
      let closure_context = expected_mode.closure_context in
      let contention_context = expected_mode.contention_context in
      let error =
        Submode_failed(failure_reason, reason, closure_context,
          contention_context, shared_context)
      in
      raise (Error(loc, env, error))

let actual_submode ~loc ~env ?reason (actual_mode : Env.actual_mode)
    expected_mode =
  submode ~loc ~env ?reason ?shared_context:actual_mode.context actual_mode.mode
    expected_mode

let escape ~loc ~env ~reason m =
  submode ~loc ~env ~reason m mode_legacy

type expected_pat_mode =
  { mode : Value.l;
    tuple_modes : Value.l list; }

let simple_pat_mode mode =
  { mode = Value.disallow_right mode; tuple_modes = [] }

let tuple_pat_mode mode tuple_modes =
  let mode = Value.disallow_right mode in
  let tuple_modes = Value.List.disallow_right tuple_modes in
  { mode; tuple_modes }

let allocations : Alloc.r list ref = Local_store.s_ref []

let reset_allocations () = allocations := []

let register_allocation_mode alloc_mode =
  let alloc_mode = Alloc.disallow_left alloc_mode in
  allocations := alloc_mode :: !allocations

let register_allocation_value_mode mode =
  let alloc_mode = value_to_alloc_r2g mode in
  register_allocation_mode alloc_mode;
  let mode = alloc_as_value alloc_mode in
  alloc_mode, mode

(** Register as allocation the expression constrained by the given
    [expected_mode]. Returns the mode of the allocation, and the expected mode
    of potential subcomponents. *)
let register_allocation (expected_mode : expected_mode) =
  let alloc_mode, mode = register_allocation_value_mode expected_mode.mode in
  alloc_mode, mode_default mode

let optimise_allocations () =
  (* CR zqian: Ideally we want to optimise all axes relavant to allocation. For
  example, pushing an allocation to [contended] is useful to the middle-end.
  However, a [contended] value in a module causes extra modality in printing.
  Therefore, here we only optimise allocation for stack/heap. Proper solutions:
  - Remove [Contention] axis from [Alloc].
  - Add it back when middle-end can really utilize this information. *)
  List.iter
    (fun mode ->
      Locality.zap_to_ceil (Alloc.proj (Comonadic Areality) mode)
      |> ignore)
    !allocations;
  reset_allocations ()

(* Typing of constants *)

let type_constant: Typedtree.constant -> type_expr = function
    Const_int _ -> instance Predef.type_int
  | Const_char _ -> instance Predef.type_char
  | Const_string _ -> instance Predef.type_string
  | Const_float _ -> instance Predef.type_float
  | Const_float32 _ -> instance Predef.type_float32
  | Const_unboxed_float _ -> instance Predef.type_unboxed_float
  | Const_unboxed_float32 _ -> instance Predef.type_unboxed_float32
  | Const_int32 _ -> instance Predef.type_int32
  | Const_int64 _ -> instance Predef.type_int64
  | Const_nativeint _ -> instance Predef.type_nativeint
  | Const_unboxed_int32 _ -> instance Predef.type_unboxed_int32
  | Const_unboxed_int64 _ -> instance Predef.type_unboxed_int64
  | Const_unboxed_nativeint _ -> instance Predef.type_unboxed_nativeint

type constant_integer_result =
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint

type constant_integer_error =
  | Int32_literal_overflow
  | Int64_literal_overflow
  | Nativeint_literal_overflow
  | Unknown_constant_literal

let constant_integer i ~suffix :
    (constant_integer_result, constant_integer_error) result =
  match suffix with
  | 'l' ->
    begin
      try Ok (Int32 (Misc.Int_literal_converter.int32 i))
      with Failure _ -> Error Int32_literal_overflow
    end
  | 'L' ->
    begin
      try Ok (Int64 (Misc.Int_literal_converter.int64 i))
      with Failure _ -> Error Int64_literal_overflow
    end
  | 'n' ->
    begin
      try Ok (Nativeint (Misc.Int_literal_converter.nativeint i))
      with Failure _ -> Error Nativeint_literal_overflow
    end
  | _ -> Error Unknown_constant_literal

let constant : Parsetree.constant -> (Typedtree.constant, error) result =
  function
  | Pconst_integer (i, Some suffix) ->
    begin match constant_integer i ~suffix with
      | Ok (Int32 v) -> Ok (Const_int32 v)
      | Ok (Int64 v) -> Ok (Const_int64 v)
      | Ok (Nativeint v) -> Ok (Const_nativeint v)
      | Error Int32_literal_overflow -> Error (Literal_overflow "int32")
      | Error Int64_literal_overflow -> Error (Literal_overflow "int64")
      | Error Nativeint_literal_overflow -> Error (Literal_overflow "nativeint")
      | Error Unknown_constant_literal -> Error (Unknown_literal (i, suffix))
    end
  | Pconst_integer (i,None) ->
     begin
       try Ok (Const_int (Misc.Int_literal_converter.int i))
       with Failure _ -> Error (Literal_overflow "int")
     end
  | Pconst_char c -> Ok (Const_char c)
  | Pconst_string (s,loc,d) -> Ok (Const_string (s,loc,d))
  | Pconst_float (f,None)-> Ok (Const_float f)
  | Pconst_float (f,Some 's') ->
    if Language_extension.is_enabled Small_numbers then Ok (Const_float32 f)
    else Error (Float32_literal f)
  | Pconst_float (f,Some c) -> Error (Unknown_literal (f, c))

let constant_or_raise env loc cst =
  match constant cst with
  | Ok c -> c
  | Error err -> raise (Error (loc, env, err))

let unboxed_constant : Jane_syntax.Layouts.constant -> (Typedtree.constant, error) result
  = function
  | Float (f, None) -> Ok (Const_unboxed_float f)
  | Float (f, Some 's') ->
    if Language_extension.is_enabled Small_numbers then Ok (Const_unboxed_float32 f)
    else Error (Float32_literal (Misc.format_as_unboxed_literal f))
  | Float (x, Some c) ->
    Error (Unknown_literal (Misc.format_as_unboxed_literal x, c))
  | Integer (i, suffix) ->
    begin match constant_integer i ~suffix with
      | Ok (Int32 v) -> Ok (Const_unboxed_int32 v)
      | Ok (Int64 v) -> Ok (Const_unboxed_int64 v)
      | Ok (Nativeint v) -> Ok (Const_unboxed_nativeint v)
      | Error Int32_literal_overflow -> Error (Literal_overflow "int32#")
      | Error Int64_literal_overflow -> Error (Literal_overflow "int64#")
      | Error Nativeint_literal_overflow -> Error (Literal_overflow "nativeint#")
      | Error Unknown_constant_literal ->
        Error (Unknown_literal (Misc.format_as_unboxed_literal i, suffix))
    end

let unboxed_constant_or_raise env loc cst =
  match unboxed_constant cst with
  | Ok c -> c
  | Error err -> raise (Error (loc, env, err))

(* Specific version of type_option, using newty rather than newgenty *)

let type_option ty =
  newty (Tconstr(Predef.path_option,[ty], ref Mnil))

let mkexp exp_desc exp_type exp_loc exp_env =
  { exp_desc; exp_type;
    exp_loc; exp_env; exp_extra = []; exp_attributes = [] }

let type_option_none env ty loc =
  let lid = Longident.Lident "None" in
  let cnone = Env.find_ident_constructor Predef.ident_none env in
  mkexp (Texp_construct(mknoloc lid, cnone, [], None)) ty loc env

let extract_option_type env ty =
  match get_desc (expand_head env ty) with
    Tconstr(path, [ty], _) when Path.same path Predef.path_option -> ty
  | _ -> assert false

let protect_expansion env ty =
  if Env.has_local_constraints env then generic_instance ty else ty

let src_pos loc attrs env =
  { exp_desc = Texp_src_pos
  ; exp_loc = loc
  ; exp_extra = []
  ; exp_type = instance Predef.type_lexing_position
  ; exp_attributes = attrs
  ; exp_env = env
  }

type record_extraction_result =
  | Record_type of Path.t * Path.t * Types.label_declaration list * record_representation
  | Not_a_record_type
  | Maybe_a_record_type

let extract_concrete_typedecl_protected env ty =
  extract_concrete_typedecl env (protect_expansion env ty)

let extract_concrete_record env ty =
  match extract_concrete_typedecl_protected env ty with
  | Typedecl(p0, p, {type_kind=Type_record (fields, repres)}) ->
    Record_type (p0, p, fields, repres)
  | Has_no_typedecl | Typedecl(_, _, _) -> Not_a_record_type
  | May_have_typedecl -> Maybe_a_record_type

type variant_extraction_result =
  | Variant_type of Path.t * Path.t * Types.constructor_declaration list
  | Not_a_variant_type
  | Maybe_a_variant_type

let extract_concrete_variant env ty =
  match extract_concrete_typedecl_protected env ty with
  | Typedecl(p0, p, {type_kind=Type_variant (cstrs, _)}) ->
    Variant_type (p0, p, cstrs)
  | Typedecl(p0, p, {type_kind=Type_open}) ->
    Variant_type (p0, p, [])
  | Has_no_typedecl | Typedecl(_, _, _) -> Not_a_variant_type
  | May_have_typedecl -> Maybe_a_variant_type

let extract_label_names env ty =
  match extract_concrete_record env ty with
  | Record_type (_, _,fields, _) -> List.map (fun l -> l.Types.ld_id) fields
  | Not_a_record_type | Maybe_a_record_type -> assert false

let has_poly_constraint spat =
  match spat.ppat_desc with
  | Ppat_constraint(_, styp) -> begin
      match styp.ptyp_desc with
      | Ptyp_poly _ -> true
      | _ -> false
    end
  | _ -> false

(** Mode cross a left mode *)
let mode_cross_left env ty mode =
  let mode =
    if not (is_principal ty) then mode else
    let jkind = type_jkind_purely env ty in
    (* FIXME jbachurski: What should happen for higher jkinds here?
       Analogous question for other uses of to_type_jkind in this file. *)
    let upper_bounds = Jkind.Type.get_modal_upper_bounds (Jkind.to_type_jkind jkind) in
    let upper_bounds = Const.alloc_as_value upper_bounds in
    Value.meet_const upper_bounds mode
  in
  mode |> Value.disallow_right

let actual_mode_cross_left env ty (actual_mode : Env.actual_mode)
  : Env.actual_mode =
  let mode = mode_cross_left env ty actual_mode.mode in
  {actual_mode with mode}

(** Mode cross a mode whose monadic fragment is a right mode, and whose comonadic
    fragment is a left mode. *)
let alloc_mode_cross_to_max_min env ty { monadic; comonadic } =
  let monadic = Alloc.Monadic.disallow_left monadic in
  let comonadic = Alloc.Comonadic.disallow_right comonadic in
  if not (is_principal ty) then { monadic; comonadic } else
  let jkind = type_jkind_purely env ty in
  let upper_bounds = Jkind.Type.get_modal_upper_bounds (Jkind.to_type_jkind jkind) in
  let upper_bounds = Alloc.Const.split upper_bounds in
  let comonadic = Alloc.Comonadic.meet_const upper_bounds.comonadic comonadic in
  let monadic = Alloc.Monadic.imply upper_bounds.monadic monadic in
  { monadic; comonadic }

(** Mode cross a right mode *)
let expect_mode_cross env ty (expected_mode : expected_mode) =
  if not (is_principal ty) then expected_mode else
  let jkind = type_jkind_purely env ty in
  let upper_bounds = Jkind.Type.get_modal_upper_bounds (Jkind.to_type_jkind jkind) in
  let upper_bounds = Const.alloc_as_value upper_bounds in
  let mode = Value.imply upper_bounds expected_mode.mode in
  (* - [strict_local] doesn't need to be updated, because it's only relavant for
     functions, which don't cross locality.
     - [mode_tuples] doesn't need to be updated, because [mode] being higher
     won't violate the invariant. *)
  { expected_mode with mode }

(* Value binding elaboration can insert alloc mode attributes on the forged
   [Pexp_constraint] node. Use this function to detect
   and remove these inserted attributes.
*)
let alloc_mode_from_pexp_constraint_typ_attrs styp =
  let modes, ptyp_attributes =
    Jane_syntax.Mode_expr.of_attrs styp.ptyp_attributes
  in
  Typemode.transl_alloc_mode modes, { styp with ptyp_attributes }

let alloc_mode_from_ppat_constraint_typ_attrs styp =
  let modes, ptyp_attributes =
    Jane_syntax.Mode_expr.of_attrs styp.ptyp_attributes
  in
  Typemode.transl_alloc_mode modes, { styp with ptyp_attributes }

let mode_annots_from_pat_attrs pat =
  let modes, ppat_attributes =
    Jane_syntax.Mode_expr.of_attrs pat.ppat_attributes
  in
  Typemode.transl_mode_annots modes, {pat with ppat_attributes}

let apply_mode_annots ~loc ~env (m : Alloc.Const.Option.t) mode =
  let error axis =
    raise (Error(loc, env, Param_mode_mismatch axis))
  in
  let min = Alloc.Const.Option.value ~default:Alloc.Const.min m in
  let max = Alloc.Const.Option.value ~default:Alloc.Const.max m in
  (match Alloc.submode (Alloc.of_const min) mode with
  | Ok () -> ()
  | Error e -> error (Left_le_right, e));
  (match Alloc.submode mode (Alloc.of_const max) with
  | Ok () -> ()
  | Error e -> error (Right_le_left, e))

(** Given the parameter [m0] on mutable, return the mode of future writes. *)
let mutable_mode m0 =
  let m0 =
    Alloc.Const.merge
      {comonadic = m0;
       monadic = Alloc.Monadic.Const.min}
  in
  m0 |> Const.alloc_as_value |> Value.of_const

(** Takes the mutability on a field, and expected mode of the record (adjusted
    for allocation), check that the construction would be allowed. *)
let check_construct_mutability ~loc ~env mutability argument_mode =
  match mutability with
  | Immutable -> ()
  | Mutable m0 ->
      let m0 = mutable_mode m0 in
      submode ~loc ~env m0 argument_mode

(** The [expected_mode] of the record when projecting a mutable field. *)
let mode_project_mutable =
  let mode =
    Contention.Const.Uncontended
    |> Contention.of_const
    |> Value.max_with (Monadic Contention)
  in
  { (mode_default mode) with
    contention_context = Some Read_mutable }

(** The [expected_mode] of the record when mutating a mutable field. *)
let mode_mutate_mutable =
  let mode =
    Contention.Const.Uncontended
    |> Contention.of_const
    |> Value.max_with (Monadic Contention)
  in
  { (mode_default mode) with
    contention_context = Some Write_mutable }

let check_project_mutability ~loc ~env mutability mode =
  if Types.is_mutable mutability then
    submode ~loc ~env mode mode_project_mutable

(* Typing of patterns *)

(* unification inside type_exp and type_expect *)
let unify_exp_types loc env ty expected_ty =
  (* Format.eprintf "@[%a@ %a@]@." Printtyp.raw_type_expr exp.exp_type
    Printtyp.raw_type_expr expected_ty; *)
  try
    unify env ty expected_ty
  with
    Unify err ->
      raise(Error(loc, env, Expr_type_clash(err, None, None)))
  | Tags(l1,l2) ->
      raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))

(* level at which to create the local type declarations *)
let gadt_equations_level = ref None
let get_gadt_equations_level () =
  match !gadt_equations_level with
    Some y -> y
  | None -> assert false

let nothing_equated = TypePairs.create 0

(* unification inside type_pat*)
let unify_pat_types_return_equated_pairs ?(refine = None) loc env ty ty' =
  try
    match refine with
    | Some allow_recursive_equations ->
        unify_gadt ~equations_level:(get_gadt_equations_level ())
          ~allow_recursive_equations env ty ty'
    | None ->
        unify !env ty ty';
        nothing_equated
  with
  | Unify err ->
      raise(Error(loc, !env, Pattern_type_clash(err, None)))
  | Tags(l1,l2) ->
      raise(Typetexp.Error(loc, !env, Typetexp.Variant_tags (l1, l2)))

let unify_pat_types ?refine loc env ty ty' =
  ignore (unify_pat_types_return_equated_pairs ?refine loc env ty ty')


(** [sdesc_for_hint] is used by error messages to report literals in their
    original formatting *)
let unify_pat ?refine ?sdesc_for_hint env pat expected_ty =
  try unify_pat_types ?refine pat.pat_loc env pat.pat_type expected_ty
  with Error (loc, env, Pattern_type_clash(err, None)) ->
    raise(Error(loc, env, Pattern_type_clash(err, sdesc_for_hint)))

(* unification of a type with a Tconstr with freshly created arguments *)
let unify_head_only ~refine loc env ty constr =
  let path = cstr_type_path constr in
  let decl = Env.find_type path !env in
  let ty' = Ctype.newconstr path (Ctype.instance_list decl.type_params) in
  unify_pat_types ~refine loc env ty' ty

(* Creating new conjunctive types is not allowed when typing patterns *)
(* make all Reither present in open variants *)
let finalize_variant pat tag opat r =
  let row =
    match get_desc (expand_head pat.pat_env pat.pat_type) with
      Tvariant row -> r := row; row
    | _ -> assert false
  in
  let f = get_row_field tag row in
  begin match row_field_repr f with
  | Rabsent -> () (* assert false *)
  | Reither (true, [], _) when not (row_closed row) ->
      link_row_field_ext ~inside:f (rf_present None)
  | Reither (false, ty::tl, _) when not (row_closed row) ->
      link_row_field_ext ~inside:f (rf_present (Some ty));
      begin match opat with None -> assert false
      | Some pat ->
          let env = ref pat.pat_env in List.iter (unify_pat env pat) (ty::tl)
      end
  | Reither (c, _l, true) when not (has_fixed_explanation row) ->
      link_row_field_ext ~inside:f (rf_either [] ~no_arg:c ~matched:false)
  | _ -> ()
  end
  (* Force check of well-formedness   WHY? *)
  (* unify_pat pat.pat_env pat
    (newty(Tvariant{row_fields=[]; row_more=newvar(); row_closed=false;
                    row_bound=(); row_fixed=false; row_name=None})); *)

let has_variants p =
  exists_general_pattern
    { f = fun (type k) (p : k general_pattern) -> match p.pat_desc with
     | (Tpat_variant _) -> true
     | _ -> false } p

let finalize_variants p =
  iter_general_pattern
    { f = fun (type k) (p : k general_pattern) -> match p.pat_desc with
     | Tpat_variant(tag, opat, r) ->
        finalize_variant p tag opat r
     | _ -> () } p

(* pattern environment *)
type pattern_variable =
  {
    pv_id: Ident.t;
    pv_uid: Uid.t;
    pv_mode: Value.l;
    pv_type: type_expr;
    pv_loc: Location.t;
    pv_as_var: bool;
    pv_attributes: attributes;
  }

type module_variable =
  {
    mv_id: Ident.t;
    mv_name: string Location.loc;
    mv_loc: Location.t;
    mv_uid: Uid.t
  }

(* Whether or not patterns of the form (module M) are accepted. (If they are,
   the idents will be created at the provided scope.) When module patterns are
   allowed, the caller should take care to check that the introduced module
   bindings' types don't escape their scope; see the callsites in [type_let]
   and [type_cases] for examples.
   [Modules_ignored] indicates that the typing of patterns should not accumulate
   a list of module patterns to unpack. It's no different than using
   [Modules_allowed] and then ignoring the accumulated [module_variables] list,
   but signals more clearly that the module patterns aren't used in an
   interesting way.
*)
type module_patterns_restriction =
  | Modules_allowed of { scope: int }
  | Modules_rejected
  | Modules_ignored

(* A parallel type to [module_patterns_restriction], though also
   tracking the module variables encountered.
*)
type module_variables =
  | Modvars_allowed of
      { scope: int;
        module_variables: module_variable list;
      }
  | Modvars_rejected
  | Modvars_ignored

type type_pat_state =
  { mutable tps_pattern_variables: pattern_variable list;
    mutable tps_pattern_force: (unit -> unit) list;
    mutable tps_module_variables: module_variables;
    (* Mutation will not change the constructor of [tps_module_variables], just
       the contained [module_variables] list. [module_variables] could be made
       mutable instead, but we felt this made the code more awkward.
    *)
  }

let create_type_pat_state allow_modules =
  let tps_module_variables =
    match allow_modules with
    | Modules_allowed { scope } ->
        Modvars_allowed { scope; module_variables = [] }
    | Modules_ignored -> Modvars_ignored
    | Modules_rejected -> Modvars_rejected
  in
  { tps_pattern_variables = [];
    tps_module_variables;
    tps_pattern_force = [];
  }

(* Copy mutable fields. Used in typechecking or-patterns. *)
let copy_type_pat_state
      { tps_pattern_variables;
        tps_module_variables;
        tps_pattern_force;
      }
  =
  { tps_pattern_variables;
    tps_module_variables;
    tps_pattern_force;
  }

let blit_type_pat_state ~src ~dst =
  dst.tps_pattern_variables <- src.tps_pattern_variables;
  dst.tps_module_variables <- src.tps_module_variables;
  dst.tps_pattern_force <- src.tps_pattern_force;
;;

let maybe_add_pattern_variables_ghost loc_let env pv =
  List.fold_right
    (fun {pv_id; _} env ->
       let name = Ident.name pv_id in
       if Env.bound_value name env then env
       else begin
         Env.enter_unbound_value name
           (Val_unbound_ghost_recursive loc_let) env
       end
    ) pv env

let iter_pattern_variables_type f : pattern_variable list -> unit =
  List.iter (fun {pv_type; _} -> f pv_type)

let add_pattern_variables ?check ?check_as env pv =
  List.fold_right
    (fun {pv_id; pv_uid; pv_mode; pv_type; pv_loc; pv_as_var; pv_attributes}
      env ->
       let check = if pv_as_var then check_as else check in
       Env.add_value ?check ~mode:pv_mode pv_id
         {val_type = pv_type; val_kind = Val_reg; Types.val_loc = pv_loc;
          val_attributes = pv_attributes; val_modalities = Modality.Value.id;
          val_zero_alloc = Builtin_attributes.Default_zero_alloc;
          val_uid = pv_uid
         } env
    )
    pv env

let add_module_variables env module_variables =
  let module_variables_as_list =
    match module_variables with
    | Modvars_allowed mvs -> mvs.module_variables
    | Modvars_ignored | Modvars_rejected -> []
  in
  List.fold_left (fun env { mv_id; mv_loc; mv_name; mv_uid } ->
    Typetexp.TyVarEnv.with_local_scope begin fun () ->
      (* This code is parallel to the typing of Pexp_letmodule. However we
         omit the call to [Mtype.lower_nongen] as it's not necessary here.
         For Pexp_letmodule, the call to [type_module] is done in a raised
         level and so needs to be modified to have the correct, outer level.
         Here, on the other hand, we're calling [type_module] outside the
         raised level, so there's no extra step to take.
      *)
      let modl, md_shape =
        !type_module env
          Ast_helper.(
            Mod.unpack ~loc:mv_loc
              (Exp.ident ~loc:mv_name.loc
                  (mkloc (Longident.Lident mv_name.txt)
                    mv_name.loc)))
      in
      let pres =
        match modl.mod_type with
        | Mty_alias _ -> Mp_absent
        | _ -> Mp_present
      in
      let md =
        { md_type = modl.mod_type; md_attributes = [];
          md_loc = mv_name.loc;
          md_uid = mv_uid; }
      in
      Env.add_module_declaration ~shape:md_shape ~check:true mv_id pres md env
    end
  ) env module_variables_as_list

let enter_variable
      ?(is_module=false) ?(is_as_variable=false) tps loc name mode ty attrs =
  if List.exists (fun {pv_id; _} -> Ident.name pv_id = name.txt)
      tps.tps_pattern_variables
  then raise(Error(loc, Env.empty, Multiply_bound_variable name.txt));
  let id =
    if is_module then begin
      (* Unpack patterns result in both a module declaration and a value
         variable of the same name being entered into the environment. (The
         module is via [tps_module_variables], and the variable is via
         [tps_pattern_variables].) *)
      match tps.tps_module_variables with
      | Modvars_ignored -> Ident.create_local name.txt
      | Modvars_rejected ->
          raise (Error (loc, Env.empty, Modules_not_allowed));
      | Modvars_allowed { scope; module_variables } ->
          escape ~loc ~env:Env.empty ~reason:Other mode;
          let id = Ident.create_scoped name.txt ~scope in
          let module_variables =
            { mv_id = id;
              mv_name = name;
              mv_loc = loc;
              mv_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
            } :: module_variables
          in
          tps.tps_module_variables <-
            Modvars_allowed { scope; module_variables; };
          id
    end else
      Ident.create_local name.txt
  in
  let pv_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
  tps.tps_pattern_variables <-
    {pv_id = id;
     pv_uid;
     pv_mode = Value.disallow_right mode;
     pv_type = ty;
     pv_loc = loc;
     pv_as_var = is_as_variable;
     pv_attributes = attrs} :: tps.tps_pattern_variables;
  id, pv_uid

let sort_pattern_variables vs =
  List.sort
    (fun {pv_id = x; _} {pv_id = y; _} ->
      Stdlib.compare (Ident.name x) (Ident.name y))
    vs

let enter_orpat_variables loc env  p1_vs p2_vs =
  (* unify_vars operate on sorted lists *)

  let p1_vs = sort_pattern_variables p1_vs
  and p2_vs = sort_pattern_variables p2_vs in

  let rec unify_vars p1_vs p2_vs =
    let vars vs = List.map (fun {pv_id; _} -> pv_id) vs in
    match p1_vs, p2_vs with
      | ({pv_id = x1; pv_type = t1; pv_mode = m1; _} as pv1)::rem1,
        {pv_id = x2; pv_type = t2; pv_mode = m2; _}::rem2
        when Ident.equal x1 x2 ->
          if x1==x2 then
            let vars, alist = unify_vars rem1 rem2 in
            pv1 :: vars, alist
          else begin
            begin try
              unify_var env (newvar (Jkind.Primitive.top ~why:Dummy_jkind)) t1;
              unify env t1 t2
            with
            | Unify err ->
                raise(Error(loc, env, Or_pattern_type_clash(x1, err)))
            end;
            let m = Value.join [m1; m2] in
            let var = { pv1 with pv_mode = m } in
            let vars, alist = unify_vars rem1 rem2 in
            var :: vars, (x2, x1) :: alist
          end
      | [],[] -> [], []
      | {pv_id; _}::_, [] | [],{pv_id; _}::_ ->
          raise (Error (loc, env, Orpat_vars (pv_id, [])))
      | {pv_id = x; _}::_, {pv_id = y; _}::_ ->
          let err =
            if Ident.name x < Ident.name y
            then Orpat_vars (x, vars p2_vs)
            else Orpat_vars (y, vars p1_vs) in
          raise (Error (loc, env, err)) in
  unify_vars p1_vs p2_vs

let rec build_as_type_and_mode ~refine ~mode (env : Env.t ref) p =
  let as_ty, as_mode = build_as_type_aux ~refine ~mode env p in
  let as_ty =
    (* Cf. #1655 *)
    List.fold_left (fun as_ty (extra, _loc, _attrs) ->
      match extra with
      | Tpat_type _ | Tpat_open _ | Tpat_unpack -> as_ty
      | Tpat_constraint cty ->
        (* [generic_instance] can only be used if the variables of the original
           type ([cty.ctyp_type] here) are not at [generic_level], which they are
           here.
           If we used [generic_instance] we would lose the sharing between
           [instance ty] and [ty].  *)
        let ty =
          with_local_level ~post:generalize_structure
            (fun () -> instance cty.ctyp_type)
        in
        (* This call to unify can't fail since the pattern is well typed. *)
        unify_pat_types ~refine p.pat_loc env (instance as_ty) (instance ty);
        ty
    ) as_ty p.pat_extra
  in
  as_ty, as_mode

and build_as_type_aux ~refine ~mode (env : Env.t ref) p =
  let build_as_type env p =
    fst (build_as_type_and_mode ~refine ~mode env p) in
  match p.pat_desc with
    Tpat_alias(p1,_, _, _, _) -> build_as_type_and_mode ~refine ~mode env p1
  | Tpat_tuple pl ->
      let labeled_tyl =
        List.map (fun (label, p) -> label, build_as_type env p) pl in
      newty (Ttuple labeled_tyl), mode
  | Tpat_construct(_, cstr, pl, vto) ->
      let priv = (cstr.cstr_private = Private) in
      let mode =
        if priv || pl <> [] then mode
        else Value.newvar ()
      in
      let keep =
        priv || cstr.cstr_existentials <> [] ||
        vto <> None (* be lazy and keep the type for node constraints *) in
      let ty =
        if keep then p.pat_type else
        let tyl = List.map (build_as_type env) pl in
        let ty_args, ty_res, _ =
          instance_constructor Keep_existentials_flexible cstr
        in
        List.iter2
          (fun (p,ty) {Types.ca_type=arg; _} ->
             unify_pat ~refine env {p with pat_type = ty} arg)
          (List.combine pl tyl) ty_args;
        ty_res
      in
      ty, mode
  | Tpat_variant(l, p', _) ->
      let ty = Option.map (build_as_type env) p' in
      let mode =
        if p' = None then Value.newvar ()
        else mode
      in
      let ty =
        let fields = [l, rf_present ty] in
        newty (Tvariant (create_row ~fields
                           ~more:(newvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))
                         ~name:None ~fixed:None ~closed:false))
      in
      ty, mode
  | Tpat_record (lpl,_) ->
      let lbl = snd3 (List.hd lpl) in
      if lbl.lbl_private = Private then p.pat_type, mode else
      (* The jkind here is filled in via unification with [ty_res] in
         [unify_pat]. *)
      (* XXX layouts v2: This should be a sort variable and could be now (but
         think about when it gets defaulted.)

         RAE: why? It looks fine as-is. *)
      let ty = newvar (Jkind.Primitive.top ~why:Dummy_jkind) in
      let ppl = List.map (fun (_, l, p) -> l.lbl_num, p) lpl in
      let do_label lbl =
        let _, ty_arg, ty_res = instance_label false lbl in
        unify_pat ~refine env {p with pat_type = ty} ty_res;
        let refinable =
          lbl.lbl_mut = Immutable && List.mem_assoc lbl.lbl_num ppl &&
          match get_desc lbl.lbl_arg with Tpoly _ -> false | _ -> true in
        if refinable then begin
          let arg = List.assoc lbl.lbl_num ppl in
          unify_pat ~refine env
            {arg with pat_type = build_as_type env arg} ty_arg
        end else begin
          let _, ty_arg', ty_res' = instance_label false lbl in
          unify_pat_types ~refine p.pat_loc env ty_arg ty_arg';
          unify_pat ~refine env p ty_res'
        end in
      Array.iter do_label lbl.lbl_all;
      ty, mode
  | Tpat_or(p1, p2, row) ->
      begin match row with
        None ->
          let ty1, mode1 = build_as_type_and_mode ~refine ~mode env p1 in
          let ty2, mode2 = build_as_type_and_mode ~refine ~mode env p2 in
          unify_pat ~refine env {p2 with pat_type = ty2} ty1;
          ty1, Value.join [mode1; mode2]
      | Some row ->
          let Row {fields; fixed; name} = row_repr row in
          let all_constant =
            List.for_all
              (fun (_,f) -> match row_field_repr f with
                | (Rpresent (Some _) | Reither (false, _, _)) -> false
                | _ -> true)
              fields
          in
          let mode =
            if all_constant then Value.newvar ()
            else mode
          in
          let ty =
            newty (Tvariant (create_row ~fields ~fixed ~name
                               ~closed:false
                               ~more:(newvar
                                        (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))))
          in
          ty, mode
      end
  | Tpat_constant _
  | Tpat_any | Tpat_var _
  | Tpat_array _ | Tpat_lazy _ ->
      p.pat_type, mode

(* Constraint solving during typing of patterns *)

let solve_Ppat_alias ~refine ~mode env pat =
  with_local_level ~post:(fun (ty_var, _) -> generalize ty_var)
    (fun () -> build_as_type_and_mode ~refine ~mode env pat)

(* Extracts the first element from a list matching a label. Roughly:
     pat <- List.assoc_opt label patl;
     return (pat, List.remove_assoc label patl)
  *)
let extract_pat label patl =
  let rec extract_pat_aux acc = function
  | [] -> None
  | ((label', t) as pat) :: rest ->
      if Option.equal String.equal label label' then
        Some (t, List.rev_append acc rest)
      else
        extract_pat_aux (pat::acc) rest
  in
  extract_pat_aux [] patl

let extract_or_mk_pat label rem closed =
  match extract_pat label rem, closed with
  (* Take the first match from patl *)
  | (Some _ as pat_and_rem), _ -> pat_and_rem
  (* No match, but the partial pattern allows us to generate a _ *)
  | None, Open -> Some (Ast_helper.Pat.mk Ppat_any, rem)
  | None, Closed -> None

(* Reorders [patl] to match the label order in [labeled_tl], erroring if [patl]
   is missing a label or has an a extra label (unlabeled components morally
   share the same special label).

   If [closed] is [Open], then no "missing label" errors are possible; instead,
   [_] patterns will be generated for those labels. An unnecessarily [Open]
   pattern results in a warning.

   (Note: an alternative approach to creating [_] patterns could be to add a
    [closed] flag to the typedtree)
   *)
let reorder_pat loc env patl closed labeled_tl expected_ty =
  let take_next (taken, rem) (label, _) =
    match extract_or_mk_pat label rem closed with
    | Some (pat, rem) -> (label, pat) :: taken, rem
    | None ->
      raise (Error (loc, !env, Missing_tuple_label(label, expected_ty)))
  in
  match List.fold_left take_next ([], patl) labeled_tl with
  | taken, [] ->
    if closed = Open
        && Int.equal (List.length labeled_tl) (List.length patl) then
      Location.prerr_warning loc Warnings.Unnecessarily_partial_tuple_pattern;
    List.rev taken
  | _, (extra_label, _) :: _ ->
    raise
      (Error (loc, !env, Extra_tuple_label(extra_label, expected_ty)))

(* This assumes the [args] have already been reordered according to the
   [expected_ty], if needed.  *)
let solve_Ppat_tuple ~refine ~alloc_mode loc env args expected_ty =
  let arity = List.length args in
  let arg_modes =
    if List.compare_length_with alloc_mode.tuple_modes arity = 0 then
      alloc_mode.tuple_modes
    else
      List.init arity (fun _ -> alloc_mode.mode)
  in
  let ann =
    (* CR layouts v5: restriction to value here to be relaxed. *)
    List.map2
      (fun (label, p) mode ->
        ( label,
          p,
          newgenvar (Jkind.Type.Primitive.value ~why:Tuple_element |> Jkind.of_type_jkind),
          simple_pat_mode mode ))
      args arg_modes
  in
  let ty = newgenty (Ttuple (List.map (fun (lbl, _, t, _) -> lbl, t) ann)) in
  let expected_ty = generic_instance expected_ty in
  unify_pat_types ~refine loc env ty expected_ty;
  ann

let solve_constructor_annotation tps env name_list sty ty_args ty_ex =
  let expansion_scope = get_gadt_equations_level () in
  let ids =
    List.map
      (fun name ->
         (* CR layouts v1.5: I expect this needs to change when we allow jkind
            annotations on explicitly quantified vars in gadt constructors.
            See: https://github.com/ocaml/ocaml/pull/9584/ *)
        let decl = new_local_type ~loc:name.loc
                     ~jkind_annot:None
                     (Jkind.Type.Primitive.value ~why:Existential_type_variable |> Jkind.of_type_jkind) in
        let (id, new_env) =
          Env.enter_type ~scope:expansion_scope name.txt decl !env in
        env := new_env;
        {name with txt = id})
      name_list
  in
  let cty, ty, force =
    with_local_level ~post:(fun (_,ty,_) -> generalize_structure ty)
      (fun () ->
         Typetexp.transl_simple_type_delayed !env Alloc.Const.legacy sty)
  in
  tps.tps_pattern_force <- force :: tps.tps_pattern_force;
  let ty_args =
    let ty1 = instance ty and ty2 = instance ty in
    match ty_args with
      [] -> assert false
    | [ty_arg] ->
        unify_pat_types cty.ctyp_loc env ty1 ty_arg;
        [ty2]
    | _ ->
        unify_pat_types cty.ctyp_loc env ty1
          (newty (Ttuple (List.map (fun t -> None, t) ty_args)));
        match get_desc (expand_head !env ty2) with
          Ttuple tyl -> (List.map snd tyl)
        | _ -> assert false
  in
  if ids <> [] then ignore begin
    let ids = List.map (fun x -> x.txt) ids in
    let rem =
      List.fold_left
        (fun rem tv ->
          match get_desc tv with
            Tconstr(Path.Pident id, [], _) when List.mem id rem ->
              list_remove id rem
          | _ ->
              raise (Error (cty.ctyp_loc, !env,
                            Unbound_existential (ids, ty))))
        ids ty_ex
    in
    if rem <> [] then
      raise (Error (cty.ctyp_loc, !env,
                    Unbound_existential (ids, ty)))
  end;
  ty_args, Some (ids, cty)

let solve_Ppat_construct ~refine tps env loc constr no_existentials
        existential_styp expected_ty =
  (* if constructor is gadt, we must verify that the expected type has the
     correct head *)
  if constr.cstr_generalized then
    unify_head_only ~refine loc env (instance expected_ty) constr;

  (* PR#7214: do not use gadt unification for toplevel lets *)
  let unify_res ty_res expected_ty =
    let refine =
      match refine, no_existentials with
      | None, None when constr.cstr_generalized -> Some false
      | _ -> refine
    in
    unify_pat_types_return_equated_pairs ~refine loc env ty_res expected_ty
  in

  let ty_args_ty, ty_args_gf, equated_types, existential_ctyp =
    with_local_level_iter ~post: generalize_structure begin fun () ->
      let expected_ty = instance expected_ty in
      let expansion_scope = get_gadt_equations_level () in
      let ty_args_ty, ty_args_gf, ty_res, equated_types, existential_ctyp =
        match existential_styp with
          None ->
            let ty_args, ty_res, _ =
              instance_constructor
                (Make_existentials_abstract { env; scope = expansion_scope })
                constr
            in
            let ty_args_ty, ty_args_gf =
              List.split
                (List.map (fun ca -> ca.Types.ca_type, ca.Types.ca_modalities) ty_args)
            in
            ty_args_ty, ty_args_gf, ty_res, unify_res ty_res expected_ty, None
        | Some (name_list, sty) ->
            let existential_treatment =
              if name_list = [] then
                Make_existentials_abstract { env; scope = expansion_scope }
              else
                (* we will unify them (in solve_constructor_annotation) with the
                   local types provided by the user *)
                Keep_existentials_flexible
            in
            let ty_args, ty_res, ty_ex =
              instance_constructor existential_treatment constr
            in
            let equated_types = unify_res ty_res expected_ty in
            let ty_args_ty, ty_args_gf =
              List.split
                (List.map (fun ca -> ca.Types.ca_type, ca.Types.ca_modalities) ty_args)
            in
            let ty_args_ty, existential_ctyp =
              solve_constructor_annotation tps env name_list sty ty_args_ty
                ty_ex
            in
            ty_args_ty, ty_args_gf, ty_res, equated_types, existential_ctyp
      in
      if constr.cstr_existentials <> [] then
        lower_variables_only !env expansion_scope ty_res;
      ((ty_args_ty, ty_args_gf, equated_types, existential_ctyp),
       expected_ty :: ty_res :: ty_args_ty)
    end
  in
  if !Clflags.principal && refine = None then begin
    (* Do not warn for counter-examples *)
    let exception Warn_only_once in
    try
      TypePairs.iter
        (fun (t1, t2) ->
          generalize_structure t1;
          generalize_structure t2;
          if not (fully_generic t1 && fully_generic t2) then
            let msg =
              Format.asprintf
                "typing this pattern requires considering@ %a@ and@ %a@ as \
                equal.@,\
                But the knowledge of these types"
                    Printtyp.type_expr t1
                    Printtyp.type_expr t2
            in
            Location.prerr_warning loc (Warnings.Not_principal msg);
            raise Warn_only_once)
        equated_types
    with Warn_only_once -> ()
  end;
  (ty_args_ty, ty_args_gf, existential_ctyp)

let solve_Ppat_record_field ~refine loc env label label_lid record_ty =
  with_local_level_iter ~post:generalize_structure begin fun () ->
    let (_, ty_arg, ty_res) = instance_label false label in
    begin try
      unify_pat_types ~refine loc env ty_res (instance record_ty)
    with Error(_loc, _env, Pattern_type_clash(err, _)) ->
      raise(Error(label_lid.loc, !env,
                  Label_mismatch(label_lid.txt, err)))
    end;
    (ty_arg, [ty_res; ty_arg])
  end

let solve_Ppat_array ~refine loc env mutability expected_ty =
  let type_some_array =
    if Types.is_mutable mutability then Predef.type_array
    else Predef.type_iarray
  in
  let jkind, arg_sort = Jkind.of_new_sort_var ~why:Array_element in
  let ty_elt = newgenvar jkind in
  let expected_ty = generic_instance expected_ty in
  unify_pat_types ~refine
    loc env (type_some_array ty_elt) expected_ty;
  ty_elt, arg_sort

let solve_Ppat_lazy  ~refine loc env expected_ty =
  let nv = newgenvar (Jkind.Type.Primitive.value ~why:Lazy_expression |> Jkind.of_type_jkind) in
  unify_pat_types ~refine loc env (Predef.type_lazy_t nv)
    (generic_instance expected_ty);
  nv

let solve_Ppat_constraint ~refine tps loc env mode sty expected_ty =
  let cty, ty, force =
    with_local_level ~post:(fun (_, ty, _) -> generalize_structure ty)
      (fun () -> Typetexp.transl_simple_type_delayed !env mode sty)
  in
  tps.tps_pattern_force <- force :: tps.tps_pattern_force;
  let ty, expected_ty' = instance ty, ty in
  unify_pat_types ~refine loc env ty (instance expected_ty);
  let expected_ty' =
    match get_desc expected_ty' with
    | Tpoly (expected_ty', tl) ->
        snd (instance_poly ~keep_names:true false tl expected_ty')
    | _ -> expected_ty'
  in
  (cty, ty, expected_ty')

let solve_Ppat_variant ~refine loc env tag no_arg expected_ty =
  (* CR layouts v5: relax the restriction to value here. *)
  let arg_type =
    if no_arg
    then []
    else [newgenvar (Jkind.Type.Primitive.value ~why:Polymorphic_variant_field |> Jkind.of_type_jkind)]
  in
  let fields = [tag, rf_either ~no_arg arg_type ~matched:true] in
  let make_row more =
    create_row ~fields ~closed:false ~more ~fixed:None ~name:None
  in
  let row = make_row (newgenvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind)) in
  let expected_ty = generic_instance expected_ty in
  (* PR#7404: allow some_private_tag blindly, as it would not unify with
     the abstract row variable *)
  if tag <> Parmatch.some_private_tag then
    unify_pat_types ~refine loc env (newgenty(Tvariant row)) expected_ty;
  (arg_type, make_row (newvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind)),
   instance expected_ty)

(* Building the or-pattern corresponding to a polymorphic variant type *)
let build_or_pat env loc lid =
  let path, decl = Env.lookup_type ~loc:lid.loc lid.txt env in
  (* CR layouts: the use of value here is wrong:
     there could be other jkinds in a polymorphic variant argument;
     see Test 24 in tests/typing-layouts/basics_alpha.ml *)
  let arity = List.length decl.type_params in
  let tyl = List.mapi (fun i _ ->
    newvar (
      Jkind.Type.Primitive.value
      ~why:(Type_argument {parent_path = path; position = i+1; arity})
      |> Jkind.of_type_jkind
    )
  ) decl.type_params in
  let row0 =
    let ty = expand_head env (newty(Tconstr(path, tyl, ref Mnil))) in
    match get_desc ty with
      Tvariant row when static_row row -> row
    | _ -> raise(Error(lid.loc, env, Not_a_polymorphic_variant_type lid.txt))
  in
  let pats, fields =
    List.fold_left
      (fun (pats,fields) (l,f) ->
        match row_field_repr f with
          Rpresent None ->
            let f = rf_either [] ~no_arg:true ~matched:true in
            (l,None) :: pats,
            (l, f) :: fields
        | Rpresent (Some ty) ->
            let f = rf_either [ty] ~no_arg:false ~matched:true in
            (l, Some {pat_desc=Tpat_any; pat_loc=Location.none; pat_env=env;
                      pat_type=ty; pat_extra=[];
                      pat_attributes=[]})
            :: pats,
            (l, f) :: fields
        | _ -> pats, fields)
      ([],[]) (row_fields row0) in
  let fields = List.rev fields in
  let name = Some (path, tyl) in
  let make_row more =
    create_row ~fields ~more ~closed:false ~fixed:None ~name in
  let ty = newty (Tvariant (make_row
                              (newvar
                                 (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))))
  in
  let gloc = Location.ghostify loc in
  let row' = ref (make_row (newvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))) in
  let pats =
    List.map
      (fun (l,p) ->
        {pat_desc=Tpat_variant(l,p,row'); pat_loc=gloc;
         pat_env=env; pat_type=ty;
         pat_extra=[]; pat_attributes=[]})
      pats
  in
  match pats with
    [] ->
      (* empty polymorphic variants: not possible with the concrete language
         but valid at the ast level *)
      raise(Error(lid.loc, env, Not_a_polymorphic_variant_type lid.txt))
  | pat :: pats ->
      let r =
        List.fold_left
          (fun pat pat0 ->
            {pat_desc=Tpat_or(pat0,pat,Some row0); pat_extra=[];
             pat_loc=gloc; pat_env=env; pat_type=ty;
             pat_attributes=[]})
          pat pats in
      (path, rp { r with pat_loc = loc })

(* When typing a for-loop index or similar, we need to restrict ourselves to the
   [Ppat_any] and [Ppat_var] cases, and construct a [pattern_variable] with
   consistent fields.  However, in the case where we're reifying a name for
   [Ppat_any], we don't need a pattern variable at all.  This function reifies
   that pattern match and construction: [any] controls what happens with the
   synthesized name for an [_] ([Ppat_any]), and [var] takes all the fields
   necessary for a [pattern_variable] so that one can be created or, similarly,
   so [enter_variable] can be called, depending on the usage. *)
let type_for_loop_like_index ~error ~loc ~env ~param ~any ~var =
  match param.ppat_desc with
  | Ppat_any ->
    any (Ident.create_local "_for", Uid.mk ~current_unit:(Env.get_unit_name ()))
  | Ppat_var name ->
      var ~name
          ~pv_mode:Value.min
          ~pv_type:(instance Predef.type_int)
          ~pv_loc:loc
          ~pv_as_var:false
          ~pv_attributes:[]
  | _ ->
      raise (Error (param.ppat_loc, env, error))

let type_for_loop_index ~loc ~env ~param =
  type_for_loop_like_index
    ~error:Invalid_for_loop_index
    ~loc
    ~env
    ~param
    (* We don't add the synthesized name for [_] to the environment because it
       can't have been referenced later. *)
    ~any:(fun wildcard_name -> wildcard_name, env)
    ~var:(fun ~name:{txt; loc = _}
              ~pv_mode
              ~pv_type
              ~pv_loc
              ~pv_as_var
              ~pv_attributes
          ->
            let check s = Warnings.Unused_for_index s in
            let pv_id = Ident.create_local txt in
            let pv_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
            let pv =
              { pv_id; pv_uid; pv_mode=Value.disallow_right pv_mode; pv_type; pv_loc; pv_as_var; pv_attributes }
            in
            (pv_id, pv_uid), add_pattern_variables ~check ~check_as:check env [pv])

let type_comprehension_for_range_iterator_index ~loc ~env ~param tps =
  type_for_loop_like_index
    ~error:Invalid_comprehension_for_range_iterator_index
    ~loc
    ~env
    ~param
    (* We don't [enter_variable] the synthesized name for [_] to the environment
       because it can't have been referenced later so we don't need to track it
       for duplicates or anything else. *)
    ~any:Fun.id
    ~var:(fun ~name ~pv_mode ~pv_type ~pv_loc ~pv_as_var ~pv_attributes ->
          enter_variable
            ~is_as_variable:pv_as_var
            tps
            pv_loc
            name
            pv_mode
            pv_type
            pv_attributes)


(* Type paths *)

let rec expand_path env p =
  let decl =
    try Some (Env.find_type p env) with Not_found -> None
  in
  match decl with
    Some {type_manifest = Some ty} ->
      begin match get_desc ty with
        Tconstr(p,_,_) -> expand_path env p
      | _ -> assert false
      end
  | _ ->
      let p' = Env.normalize_type_path None env p in
      if Path.same p p' then p else expand_path env p'

let compare_type_path env tpath1 tpath2 =
  Path.same (expand_path env tpath1) (expand_path env tpath2)

(* Records *)
exception Wrong_name_disambiguation of Env.t * wrong_name

let get_constr_type_path ty =
  match get_desc ty with
  | Tconstr(p, _, _) -> p
  | _ -> assert false

module NameChoice(Name : sig
  type t
  type usage
  val kind: Datatype_kind.t
  val get_name: t -> string
  val get_type: t -> type_expr
  val lookup_all_from_type:
    Location.t -> usage -> Path.t -> Env.t -> (t * (unit -> unit)) list

  (** Some names (for example the fields of inline records) are not
      in the typing environment -- they behave as structural labels
      rather than nominal labels.*)
  val in_env: t -> bool
end) = struct
  open Name

  let get_type_path d = get_constr_type_path (get_type d)

  let lookup_from_type env type_path usage lid =
    let descrs = lookup_all_from_type lid.loc usage type_path env in
    match lid.txt with
    | Longident.Lident name -> begin
        match
          List.find (fun (nd, _) -> get_name nd = name) descrs
        with
        | descr, use ->
            use ();
            descr
        | exception Not_found ->
            let valid_names = List.map (fun (nd, _) -> get_name nd) descrs in
            raise (Wrong_name_disambiguation (env, {
                    type_path;
                    name = { lid with txt = name };
                    kind;
                    valid_names;
              }))
      end
    | _ -> raise Not_found

  let rec unique eq acc = function
      [] -> List.rev acc
    | x :: rem ->
        if List.exists (eq x) acc then unique eq acc rem
        else unique eq (x :: acc) rem

  let ambiguous_types env lbl others =
    let tpath = get_type_path lbl in
    let others =
      List.map (fun (lbl, _) -> get_type_path lbl) others in
    let tpaths = unique (compare_type_path env) [tpath] others in
    match tpaths with
      [_] -> []
    | _ -> let open Printtyp in
        wrap_printing_env ~error:true env (fun () ->
            reset(); strings_of_paths (Some Type) tpaths)

  let disambiguate_by_type env tpath lbls =
    match lbls with
    | (Error _ : _ result) -> raise Not_found
    | Ok lbls ->
        let check_type (lbl, _) =
          let lbl_tpath = get_type_path lbl in
          compare_type_path env tpath lbl_tpath
        in
        List.find check_type lbls

  (* warn if there are several distinct candidates in scope *)
  let warn_if_ambiguous warn lid env lbl rest =
    if Warnings.is_active (Ambiguous_name ([],[],false,"")) then begin
      Printtyp.Conflicts.reset ();
      let paths = ambiguous_types env lbl rest in
      let expansion =
        Format.asprintf "%t" Printtyp.Conflicts.print_explanations in
      if paths <> [] then
        warn lid.loc
          (Warnings.Ambiguous_name ([Longident.last lid.txt],
                                    paths, false, expansion))
    end

  (* a non-principal type was used for disambiguation *)
  let warn_non_principal warn lid =
    let name = Datatype_kind.label_name kind in
    warn lid.loc
      (Warnings.Not_principal
         ("this type-based " ^ name ^ " disambiguation"))

  (* we selected a name out of the lexical scope *)
  let warn_out_of_scope warn lid env tpath =
    if Warnings.is_active (Name_out_of_scope ("",[],false)) then begin
      let path_s =
        Printtyp.wrap_printing_env ~error:true env
          (fun () -> Printtyp.string_of_path tpath) in
      warn lid.loc
        (Warnings.Name_out_of_scope (path_s, [Longident.last lid.txt], false))
    end

  (* warn if the selected name is not the last introduced in scope
     -- in these cases the resolution is different from pre-disambiguation OCaml
     (this warning is not enabled by default, it is specifically for people
      wishing to write backward-compatible code).
   *)
  let warn_if_disambiguated_name warn lid lbl scope =
    match scope with
    | Ok ((lab1,_) :: _) when lab1 == lbl -> ()
    | _ ->
        warn lid.loc
          (Warnings.Disambiguated_name (get_name lbl))

  let force_error : ('a, _) result -> 'a = function
    | Ok lbls -> lbls
    | Error (loc', env', err) ->
       Env.lookup_error loc' env' err

  type candidate = t * (unit -> unit)
  type nonempty_candidate_filter =
    candidate list -> (candidate list, candidate list) result
  (** This type is used for candidate filtering functions.
      Filtering typically proceeds in several passes, filtering
      candidates through increasingly precise conditions.

      We assume that the input list is non-empty, and the output is one of
      - [Ok result] for a non-empty list [result] of valid candidates
      - [Error candidates] with there are no valid candidates,
        and [candidates] is a non-empty subset of the input, typically
        the result of the last non-empty filtering step.
   *)

  (** [disambiguate] selects a concrete description for [lid] using
     some contextual information:
     - An optional [expected_type].
     - A list of candidates labels in the current lexical scope,
       [candidates_in_scope], that is actually at the type
       [(label_descr list, lookup_error) result] so that the
       lookup error is only raised when necessary.
     - A filtering criterion on candidates in scope [filter_candidates],
       representing extra contextual information that can help
       candidate selection (see [disambiguate_label_by_ids]).
   *)
  let disambiguate
        ?(warn=Location.prerr_warning)
        ?(filter : nonempty_candidate_filter = Result.ok)
        usage lid env
        expected_type
        candidates_in_scope =
    let lbl = match expected_type with
    | None ->
        (* no expected type => no disambiguation *)
        begin match filter (force_error candidates_in_scope) with
        | Ok [] | Error [] -> assert false
        | Error((lbl, _use) :: _rest) -> lbl (* will fail later *)
        | Ok((lbl, use) :: rest) ->
            use ();
            warn_if_ambiguous warn lid env lbl rest;
            lbl
        end
    | Some(tpath0, tpath, principal) ->
       (* If [expected_type] is available, the candidate selected
          will correspond to the type-based resolution.
          There are two reasons to still check the lexical scope:
          - for warning purposes
          - for extension types, the type environment does not contain
            a list of constructors, so using only type-based selection
            would fail.
        *)
        (* note that [disambiguate_by_type] does not
           force [candidates_in_scope]: we just skip this case if there
           are no candidates in scope *)
        begin match disambiguate_by_type env tpath candidates_in_scope with
        | lbl, use ->
          use ();
          if not principal then begin
            (* Check if non-principal type is affecting result *)
            match (candidates_in_scope : _ result) with
            | Error _ -> warn_non_principal warn lid
            | Ok lbls ->
            match filter lbls with
            | Error _ -> warn_non_principal warn lid
            | Ok [] -> assert false
            | Ok ((lbl', _use') :: rest) ->
            let lbl_tpath = get_type_path lbl' in
            (* no principality warning if the non-principal
               type-based selection corresponds to the last
               definition in scope *)
            if not (compare_type_path env tpath lbl_tpath)
            then warn_non_principal warn lid
            else warn_if_ambiguous warn lid env lbl rest;
          end;
          lbl
        | exception Not_found ->
        (* look outside the lexical scope *)
        match lookup_from_type env tpath usage lid with
        | lbl ->
          (* warn only on nominal labels;
             structural labels cannot be qualified anyway *)
          if in_env lbl then warn_out_of_scope warn lid env tpath;
          if not principal then warn_non_principal warn lid;
          lbl
        | exception Not_found ->
        match filter (force_error candidates_in_scope) with
        | Ok lbls | Error lbls ->
        let tp = (tpath0, expand_path env tpath) in
        let tpl =
          List.map
            (fun (lbl, _) ->
               let tp0 = get_type_path lbl in
               let tp = expand_path env tp0 in
               (tp0, tp))
            lbls
        in
        raise (Error (lid.loc, env,
                      Name_type_mismatch (kind, lid.txt, tp, tpl)));
        end
    in
    (* warn only on nominal labels *)
    if in_env lbl then
      warn_if_disambiguated_name warn lid lbl candidates_in_scope;
    lbl
end

let wrap_disambiguate msg ty f x =
  try f x with
  | Wrong_name_disambiguation (env, wrong_name) ->
    raise (Error (wrong_name.name.loc, env, Wrong_name (msg, ty, wrong_name)))

module Label = NameChoice (struct
  type t = label_description
  type usage = Env.label_usage
  let kind = Datatype_kind.Record
  let get_name lbl = lbl.lbl_name
  let get_type lbl = lbl.lbl_res
  let lookup_all_from_type loc usage path env =
    Env.lookup_all_labels_from_type ~loc usage path env
  let in_env lbl =
    match lbl.lbl_repres with
    | Record_boxed _ | Record_float | Record_ufloat | Record_unboxed
    | Record_mixed _ -> true
    | Record_inlined _ -> false
end)

(* In record-construction expressions and patterns, we have many labels
   at once; find a candidate type in the intersection of the candidates
   of each label. In the [closed] expression case, this candidate must
   contain exactly all the labels.

   If our successive refinements result in an empty list,
   return [Error] with the last non-empty list of candidates
   for use in error messages.
*)
let disambiguate_label_by_ids closed ids labels  : (_, _) result =
  let check_ids (lbl, _) =
    let lbls = Hashtbl.create 8 in
    Array.iter (fun lbl -> Hashtbl.add lbls lbl.lbl_name ()) lbl.lbl_all;
    List.for_all (Hashtbl.mem lbls) ids
  and check_closed (lbl, _) =
    (not closed || List.length ids = Array.length lbl.lbl_all)
  in
  match List.filter check_ids labels with
  | [] -> Error labels
  | labels ->
  match List.filter check_closed labels with
  | [] -> Error labels
  | labels ->
  Ok labels

(* Only issue warnings once per record constructor/pattern *)
let disambiguate_sort_lid_a_list loc closed env usage expected_type lid_a_list =
  let ids = List.map (fun (lid, _) -> Longident.last lid.txt) lid_a_list in
  let w_pr = ref false and w_amb = ref []
  and w_scope = ref [] and w_scope_ty = ref "" in
  let warn loc msg =
    let open Warnings in
    match msg with
    | Not_principal _ -> w_pr := true
    | Ambiguous_name([s], l, _, ex) -> w_amb := (s, l, ex) :: !w_amb
    | Name_out_of_scope(ty, [s], _) ->
        w_scope := s :: !w_scope; w_scope_ty := ty
    | _ -> Location.prerr_warning loc msg
  in
  let process_label lid =
    let scope = Env.lookup_all_labels ~loc:lid.loc usage lid.txt env in
    let filter : Label.nonempty_candidate_filter =
      disambiguate_label_by_ids closed ids in
    Label.disambiguate ~warn ~filter usage lid env expected_type scope in
  let lbl_a_list =
    (* If one label is qualified [{ foo = ...; M.bar = ... }],
       we will disambiguate all labels using one of the qualifying modules,
       as if the user had written [{ M.foo = ...; M.bar = ... }].

       #11630: It is important to process first the
       user-qualified labels, instead of processing all labels in
       order, so that error messages coming from the lookup of
       M (maybe no such module/path exists) are shown to the user
       in context of a qualified field [M.bar] they wrote
       themselves, instead of the "ghost" qualification [M.foo]
       that does not come from the source program. *)
    let lbl_list =
      List.map (fun (lid, _) ->
          match lid.txt with
          | Longident.Ldot _ -> Some (process_label lid)
          | _ -> None
        ) lid_a_list
    in
    (* Find a module prefix (if any) to qualify unqualified labels *)
    let qual =
      List.find_map (function
          | {txt = Longident.Ldot (modname, _); _}, _ -> Some modname
          | _ -> None
        ) lid_a_list
    in
    (* Prefix unqualified labels with [qual] and resolve them.

       Prefixing unqualified labels does not change the final
       disambiguation result, it restricts the set of candidates
       without removing any valid choice.
       It matters if users activated warnings for ambiguous or
       out-of-scope resolutions -- they get less warnings by
       qualifying at least one of the fields. *)
    List.map2 (fun lid_a lbl ->
        match lbl, lid_a with
        | Some lbl, (lid, a) -> lid, lbl, a
        | None, (lid, a) ->
            let qual_lid =
              match qual, lid.txt with
              | Some modname, Longident.Lident s ->
                  {lid with txt = Longident.Ldot (modname, s)}
              | _ -> lid
            in
            lid, process_label qual_lid, a
      ) lid_a_list lbl_list
  in
  if !w_pr then
    Location.prerr_warning loc
      (Warnings.Not_principal "this type-based record disambiguation")
  else begin
    match List.rev !w_amb with
      (_,types,ex)::_ as amb ->
        let paths =
          List.map (fun (_,lbl,_) -> Label.get_type_path lbl) lbl_a_list in
        let path = List.hd paths in
        let fst3 (x,_,_) = x in
        if List.for_all (compare_type_path env path) (List.tl paths) then
          Location.prerr_warning loc
            (Warnings.Ambiguous_name (List.map fst3 amb, types, true, ex))
        else
          List.iter
            (fun (s,l,ex) -> Location.prerr_warning loc
                (Warnings.Ambiguous_name ([s],l,false, ex)))
            amb
    | _ -> ()
  end;
  if !w_scope <> [] then
    Location.prerr_warning loc
      (Warnings.Name_out_of_scope (!w_scope_ty, List.rev !w_scope, true));
  (* Invariant: records are sorted in the typed tree *)
  List.sort
    (fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_num lbl2.lbl_num)
    lbl_a_list

let map_fold_cont f xs k =
  List.fold_right (fun x k ys -> f x (fun y -> k (y :: ys)))
    xs (fun ys -> k (List.rev ys)) []

(* Checks over the labels mentioned in a record pattern:
   no duplicate definitions (error); properly closed (warning) *)

let check_recordpat_labels loc lbl_pat_list closed =
  match lbl_pat_list with
  | [] -> ()                            (* should not happen *)
  | (_, label1, _) :: _ ->
      let all = label1.lbl_all in
      let defined = Array.make (Array.length all) false in
      let check_defined (_, label, _) =
        if defined.(label.lbl_num)
        then raise(Error(loc, Env.empty, Label_multiply_defined label.lbl_name))
        else defined.(label.lbl_num) <- true in
      List.iter check_defined lbl_pat_list;
      if closed = Closed
      && Warnings.is_active (Warnings.Missing_record_field_pattern "")
      then begin
        let undefined = ref [] in
        for i = 0 to Array.length all - 1 do
          if not defined.(i) then undefined := all.(i).lbl_name :: !undefined
        done;
        if !undefined <> [] then begin
          let u = String.concat ", " (List.rev !undefined) in
          Location.prerr_warning loc (Warnings.Missing_record_field_pattern u)
        end
      end

(* Constructors *)

module Constructor = NameChoice (struct
  type t = constructor_description
  type usage = Env.constructor_usage
  let kind = Datatype_kind.Variant
  let get_name cstr = cstr.cstr_name
  let get_type cstr = cstr.cstr_res
  let lookup_all_from_type loc usage path env =
    match Env.lookup_all_constructors_from_type ~loc usage path env with
    | _ :: _ as x -> x
    | [] ->
        match (Env.find_type path env).type_kind with
        | Type_open ->
            (* Extension constructors cannot be found by looking at the type
               declaration.
               We scan the whole environment to get an accurate spellchecking
               hint in the subsequent error message *)
            let filter lbl =
              compare_type_path env
                path (get_constr_type_path @@ get_type lbl) in
            let add_valid x acc = if filter x then (x,ignore)::acc else acc in
            Env.fold_constructors add_valid None env []
        | _ -> []
  let in_env _ = true
end)

(* Typing of patterns *)

(* "untyped" cases are prior to checking the pattern. *)
type untyped_case = Parsetree.pattern Parmatch.parmatch_case

(* "half typed" cases are produced in [map_half_typed_cases] when we've just
   typechecked the pattern but haven't type-checked the body yet. At this point
   we might have added some type equalities to the environment, but haven't yet
   added identifiers bound by the pattern. *)
type ('case_pattern, 'case_data) half_typed_case =
  { typed_pat: 'case_pattern;
    pat_type_for_unif: type_expr;
    untyped_case: untyped_case;
    case_data : 'case_data;
    branch_env: Env.t;
    pat_vars: pattern_variable list;
    module_vars: module_variables;
    contains_gadt: bool; }

(* Used to split patterns into value cases and exception cases. *)
let split_half_typed_cases env zipped_cases =
  let add_case lst htc data = function
    | None -> lst
    | Some split_pat ->
        ({ htc.untyped_case with pattern = split_pat }, data) :: lst
  in
  List.fold_right (fun (htc, data) (vals, exns) ->
      let pat = htc.typed_pat in
      match split_pattern pat with
      | Some _, Some _ when htc.untyped_case.has_guard ->
          raise (Error (pat.pat_loc, env,
                        Mixed_value_and_exception_patterns_under_guard))
      | vp, ep -> add_case vals htc data vp, add_case exns htc data ep
    ) zipped_cases ([], [])

let rec has_literal_pattern p =
  match Jane_syntax.Pattern.of_ast p with
  | Some (jpat, _attrs) -> has_literal_pattern_jane_syntax jpat
  | None      -> match p.ppat_desc with
  | Ppat_constant _
  | Ppat_interval _ ->
     true
  | Ppat_any
  | Ppat_variant (_, None)
  | Ppat_construct (_, None)
  | Ppat_type _
  | Ppat_var _
  | Ppat_unpack _
  | Ppat_extension _ ->
     false
  | Ppat_exception p
  | Ppat_variant (_, Some p)
  | Ppat_construct (_, Some (_, p))
  | Ppat_constraint (p, _)
  | Ppat_alias (p, _)
  | Ppat_lazy p
  | Ppat_open (_, p) ->
     has_literal_pattern p
  | Ppat_tuple ps
  | Ppat_array ps ->
     List.exists has_literal_pattern ps
  | Ppat_record (ps, _) ->
     List.exists (fun (_,p) -> has_literal_pattern p) ps
  | Ppat_or (p, q) ->
     has_literal_pattern p || has_literal_pattern q
and has_literal_pattern_jane_syntax : Jane_syntax.Pattern.t -> _ = function
  | Jpat_immutable_array (Iapat_immutable_array ps) ->
     List.exists has_literal_pattern ps
  | Jpat_layout (Lpat_constant _) -> true
  | Jpat_tuple (labeled_ps, _) ->
     List.exists (fun (_, p) -> has_literal_pattern p) labeled_ps

let check_scope_escape loc env level ty =
  try Ctype.check_scope_escape env level ty
  with Escape esc ->
    (* We don't expand the type here because if we do, we might expand to the
       type that escaped, leading to confusing error messages. *)
    let trace = Errortrace.[Escape (map_escape trivial_expansion esc)] in
    raise (Error(loc,
                 env,
                 Pattern_type_clash(Errortrace.unification_error ~trace, None)))


(** The typedtree has two distinct syntactic categories for patterns,
   "value" patterns, matching on values, and "computation" patterns
   that match on the effect of a computation -- typically, exception
   patterns (exception p).

   On the other hand, the parsetree has an unstructured representation
   where all categories of patterns are mixed together. The
   decomposition according to the value/computation structure has to
   happen during type-checking.

   We don't want to duplicate the type-checking logic in two different
   functions, depending on the kind of pattern to be produced. In
   particular, there are both value and computation or-patterns, and
   the type-checking logic for or-patterns is horribly complex; having
   it in two different places would be twice as horirble.

   The solution is to pass a GADT tag to [type_pat] to indicate whether
   a value or computation pattern is expected. This way, there is a single
   place where [Ppat_or] nodes are type-checked, the checking logic is shared,
   and only at the end do we inspect the tag to decide to produce a value
   or computation pattern.
*)
let pure
  : type k . k pattern_category -> value general_pattern -> k general_pattern
  = fun category pat ->
  match category with
  | Value -> pat
  | Computation -> as_computation_pattern pat

let only_impure
  : type k . k pattern_category ->
             computation general_pattern -> k general_pattern
  = fun category pat ->
  match category with
  | Value ->
     (* LATER: this exception could be renamed/generalized *)
     raise (Error (pat.pat_loc, pat.pat_env,
                   Exception_pattern_disallowed))
  | Computation -> pat

let as_comp_pattern
  : type k . k pattern_category ->
             k general_pattern -> computation general_pattern
  = fun category pat ->
  match category with
  | Value -> as_computation_pattern pat
  | Computation -> pat

(** [type_pat] propagates the expected type, and
    unification may update the typing environment. *)
let rec type_pat
  : type k . type_pat_state -> k pattern_category ->
      no_existentials: existential_restriction option ->
      alloc_mode:expected_pat_mode ->
      env: Env.t ref -> Parsetree.pattern -> type_expr -> k general_pattern
  = fun tps category ~no_existentials ~alloc_mode ~env sp expected_ty ->
  Builtin_attributes.warning_scope sp.ppat_attributes
    (fun () ->
       type_pat_aux tps category ~no_existentials
         ~alloc_mode ~env sp expected_ty
    )

and type_pat_aux
  : type k . type_pat_state -> k pattern_category -> no_existentials:_ ->
         alloc_mode:expected_pat_mode -> env:_ -> _ -> _ -> k general_pattern
  = fun tps category ~no_existentials ~alloc_mode ~env sp expected_ty ->
  let type_pat tps category ?(alloc_mode=alloc_mode) ?(env=env) =
    type_pat tps category ~no_existentials ~alloc_mode ~env
  in
  let loc = sp.ppat_loc in
  let refine = None in
  let solve_expected (x : pattern) : pattern =
    unify_pat ~refine ~sdesc_for_hint:sp.ppat_desc env x (instance expected_ty);
    x
  in
  let crp (x : k general_pattern) : k general_pattern =
    match category with
    | Value -> rp x
    | Computation -> rcp x
  in
  (* record {general,value,computation} pattern *)
  let rp = crp
  and rvp x = crp (pure category x)
  and rcp x = crp (only_impure category x) in
  let type_pat_array mutability spl pat_attributes =
    (* Sharing the code between the two array cases means we're guaranteed to
       keep them in sync, at the cost of a worse diff with upstream; it
       shouldn't be too bad.  We can inline this when we upstream this code and
       combine the two array pattern constructors. *)
    let ty_elt, arg_sort = solve_Ppat_array ~refine loc env mutability expected_ty in
    let modalities =
      if Types.is_mutable mutability then Typemode.mutable_implied_modalities
      else Modality.Value.Const.id
    in
    check_project_mutability ~loc ~env:!env mutability alloc_mode.mode;
    let alloc_mode = Modality.Value.Const.apply modalities alloc_mode.mode in
    let alloc_mode = simple_pat_mode alloc_mode in
    let pl = List.map (fun p -> type_pat ~alloc_mode tps Value p ty_elt) spl in
    rvp {
      pat_desc = Tpat_array (mutability, arg_sort, pl);
      pat_loc = loc; pat_extra=[];
      pat_type = instance expected_ty;
      pat_attributes;
      pat_env = !env }
  in
  let type_tuple_pat spl closed =
    let args =
      match get_desc (expand_head !env expected_ty) with
      (* If it's a principally-known tuple pattern, try to reorder *)
      | Ttuple labeled_tl when is_principal expected_ty ->
        reorder_pat loc env spl closed labeled_tl expected_ty
      (* If not, it's not allowed to be open (partial) *)
      | _ ->
        match closed with
        | Open -> raise (Error (loc, !env, Partial_tuple_pattern_bad_type))
        | Closed -> spl
    in
    let spl_ann =
      solve_Ppat_tuple ~refine ~alloc_mode loc env args expected_ty
    in
    let pl =
      List.map (fun (lbl, p, t, alloc_mode) ->
        lbl, type_pat tps Value ~alloc_mode p t)
        spl_ann
    in
    rvp {
      pat_desc = Tpat_tuple pl;
      pat_loc = loc; pat_extra=[];
      pat_type = newty (Ttuple (List.map (fun (lbl, p) -> lbl, p.pat_type) pl));
      pat_attributes = sp.ppat_attributes;
      pat_env = !env }
  in
  match Jane_syntax.Mode_expr.maybe_of_attrs sp.ppat_attributes with
  | Some modes, _ -> raise (Error (modes.loc, !env, Modes_on_pattern))
  | None, _ ->
  match Jane_syntax.Pattern.of_ast sp with
  | Some (jpat, attrs) -> begin
      (* Normally this would go to an auxiliary function, but this function
         takes so many parameters, has such a complex type, and uses so many
         local definitions, it seems better to just put the pattern matching
         here.  This shouldn't mess up the diff *too* much. *)
      match jpat with
      | Jpat_immutable_array (Iapat_immutable_array spl) ->
          type_pat_array Immutable spl attrs
      | Jpat_layout (Lpat_constant cst) ->
          let cst = unboxed_constant_or_raise !env loc cst in
          rvp @@ solve_expected {
            pat_desc = Tpat_constant cst;
            pat_loc = loc; pat_extra=[];
            pat_type = type_constant cst;
            pat_attributes = attrs;
            pat_env = !env }
      | Jpat_tuple (spl, closed) ->
          type_tuple_pat spl closed
    end
  | None ->
  match sp.ppat_desc with
    Ppat_any ->
      rvp {
        pat_desc = Tpat_any;
        pat_loc = loc; pat_extra=[];
        pat_type = instance expected_ty;
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
  | Ppat_var name ->
      let ty = instance expected_ty in
      let alloc_mode = mode_cross_left !env expected_ty alloc_mode.mode in
      let id, uid =
        enter_variable tps loc name alloc_mode ty sp.ppat_attributes
      in
      rvp {
        pat_desc = Tpat_var (id, name, uid, alloc_mode);
        pat_loc = loc; pat_extra=[];
        pat_type = ty;
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
  | Ppat_unpack name ->
      let t = instance expected_ty in
      begin match name.txt with
      | None ->
          rvp {
            pat_desc = Tpat_any;
            pat_loc = sp.ppat_loc;
            pat_extra=[Tpat_unpack, name.loc, sp.ppat_attributes];
            pat_type = t;
            pat_attributes = [];
            pat_env = !env }
      | Some s ->
          let v = { name with txt = s } in
          (* We're able to pass ~is_module:true here without an error because
             [Ppat_unpack] is a case identified by [may_contain_modules]. See
             the comment on [may_contain_modules]. *)
          let id, uid = enter_variable tps loc v alloc_mode.mode
                          t ~is_module:true sp.ppat_attributes in
          rvp {
            pat_desc = Tpat_var (id, v, uid, alloc_mode.mode);
            pat_loc = sp.ppat_loc;
            pat_extra=[Tpat_unpack, loc, sp.ppat_attributes];
            pat_type = t;
            pat_attributes = [];
            pat_env = !env }
      end
  | Ppat_alias(sq, name) ->
      let q = type_pat tps Value sq expected_ty in
      let ty_var, mode = solve_Ppat_alias ~refine ~mode:alloc_mode.mode env q in
      let mode = mode_cross_left !env expected_ty mode in
      let id, uid =
        enter_variable ~is_as_variable:true tps name.loc name mode ty_var
          sp.ppat_attributes
      in
      rvp { pat_desc = Tpat_alias(q, id, name, uid, mode);
            pat_loc = loc; pat_extra=[];
            pat_type = q.pat_type;
            pat_attributes = sp.ppat_attributes;
            pat_env = !env }
  | Ppat_constant cst ->
      let cst = constant_or_raise !env loc cst in
      rvp @@ solve_expected {
        pat_desc = Tpat_constant cst;
        pat_loc = loc; pat_extra=[];
        pat_type = type_constant cst;
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
  | Ppat_interval (Pconst_char c1, Pconst_char c2) ->
      let open Ast_helper.Pat in
      let gloc = Location.ghostify loc in
      let rec loop c1 c2 =
        if c1 = c2 then constant ~loc:gloc (Pconst_char c1)
        else
          or_ ~loc:gloc
            (constant ~loc:gloc (Pconst_char c1))
            (loop (Char.chr(Char.code c1 + 1)) c2)
      in
      let p = if c1 <= c2 then loop c1 c2 else loop c2 c1 in
      let p = {p with ppat_loc=loc} in
      type_pat tps category p expected_ty
        (* TODO: record 'extra' to remember about interval *)
  | Ppat_interval _ ->
      raise (Error (loc, !env, Invalid_interval))
  | Ppat_tuple spl ->
      type_tuple_pat (List.map (fun sp -> None, sp) spl) Closed
  | Ppat_construct(lid, sarg) ->
      let expected_type =
        match extract_concrete_variant !env expected_ty with
        | Variant_type(p0, p, _) ->
            Some (p0, p, is_principal expected_ty)
        | Maybe_a_variant_type -> None
        | Not_a_variant_type ->
            let srt = wrong_kind_sort_of_constructor lid.txt in
            let error = Wrong_expected_kind(srt, Pattern, expected_ty) in
            raise (Error (loc, !env, error))
      in
      let constr =
        let candidates =
          Env.lookup_all_constructors Env.Pattern ~loc:lid.loc lid.txt !env in
        wrap_disambiguate "This variant pattern is expected to have"
          (mk_expected expected_ty)
          (Constructor.disambiguate Env.Pattern lid !env expected_type)
          candidates
      in
      begin match no_existentials, constr.cstr_existentials with
      | None, _ | _, [] -> ()
      | Some r, (_ :: _ as exs)  ->
          let exs = List.map (Ctype.existential_name constr) exs in
          let name = constr.cstr_name in
          raise (Error (loc, !env, Unexpected_existential (r, name, exs)))
      end;
      let sarg', existential_styp =
        match sarg with
          None -> None, None
        | Some (vl, {ppat_desc = Ppat_constraint (sp, sty)})
          when vl <> [] || constr.cstr_arity > 1 ->
            Some sp, Some (vl, sty)
        | Some ([], sp) ->
            Some sp, None
        | Some (_, sp) ->
            raise (Error (sp.ppat_loc, !env, Missing_type_constraint))
      in
      let sargs =
        match sarg' with
          None -> []
        | Some sarg' ->
        match Jane_syntax.Pattern.of_ast sarg' with
        | Some (Jpat_tuple (_, _), attrs) when
            constr.cstr_arity > 1 || Builtin_attributes.explicit_arity attrs
          -> raise (Error(loc, !env, Constructor_labeled_arg))
        | Some ((Jpat_immutable_array _, _)
               | (Jpat_layout _, _)
               | (Jpat_tuple _, _)) -> [sarg']
        | None -> match sarg' with
        | {ppat_desc = Ppat_tuple spl} as sp when
            constr.cstr_arity > 1 ||
            Builtin_attributes.explicit_arity sp.ppat_attributes
          -> spl
        | {ppat_desc = Ppat_any} as sp when
            constr.cstr_arity = 0 && existential_styp = None
          ->
            Location.prerr_warning sp.ppat_loc
              Warnings.Wildcard_arg_to_constant_constr;
            []
        | {ppat_desc = Ppat_any} as sp when constr.cstr_arity > 1 ->
            replicate_list sp constr.cstr_arity
        | sp -> [sp] in
      if Builtin_attributes.warn_on_literal_pattern constr.cstr_attributes then
        begin match List.filter has_literal_pattern sargs with
        | sp :: _ ->
           Location.prerr_warning sp.ppat_loc Warnings.Fragile_literal_pattern
        | _ -> ()
        end;
      if List.length sargs <> constr.cstr_arity then
        raise(Error(loc, !env, Constructor_arity_mismatch(lid.txt,
                                     constr.cstr_arity, List.length sargs)));

      let (ty_args_ty, ty_args_gf, existential_ctyp) =
        solve_Ppat_construct ~refine tps env loc constr no_existentials
          existential_styp expected_ty
      in

      let rec check_non_escaping p =
        match p.ppat_desc with
        | Ppat_or (p1, p2) ->
            check_non_escaping p1;
            check_non_escaping p2
        | Ppat_alias (p, _) ->
            check_non_escaping p
        | Ppat_constraint _ ->
            raise (Error (p.ppat_loc, !env, Inlined_record_escape))
        | _ ->
            ()
      in
      if constr.cstr_inlined <> None then begin
        List.iter check_non_escaping sargs;
        Option.iter (fun (_, sarg) -> check_non_escaping sarg) sarg
      end;

      let args =
        List.map2
          (fun p (ty, gf) ->
             let alloc_mode = Modality.Value.Const.apply gf alloc_mode.mode in
             let alloc_mode = simple_pat_mode alloc_mode in
             type_pat ~alloc_mode tps Value p ty)
          sargs (List.combine ty_args_ty ty_args_gf)
      in
      rvp { pat_desc=Tpat_construct(lid, constr, args, existential_ctyp);
            pat_loc = loc; pat_extra=[];
            pat_type = instance expected_ty;
            pat_attributes = sp.ppat_attributes;
            pat_env = !env }
  | Ppat_variant(tag, sarg) ->
      assert (tag <> Parmatch.some_private_tag);
      let constant = (sarg = None) in
      let arg_type, row, pat_type =
        solve_Ppat_variant ~refine loc env tag constant expected_ty in
      let arg =
        (* PR#6235: propagate type information *)
        match sarg, arg_type with
          Some sp, [ty] -> Some (type_pat tps Value sp ty)
        | _             -> None
      in
      rvp {
        pat_desc = Tpat_variant(tag, arg, ref row);
        pat_loc = loc; pat_extra = [];
        pat_type = pat_type;
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
  | Ppat_record(lid_sp_list, closed) ->
      assert (lid_sp_list <> []);
      let expected_type, record_ty =
        match extract_concrete_record !env expected_ty with
        | Record_type(p0, p, _, _) ->
            let ty = generic_instance expected_ty in
            Some (p0, p, is_principal expected_ty), ty
        | Maybe_a_record_type ->
          None, newvar (Jkind.Type.Primitive.value ~why:Boxed_record |> Jkind.of_type_jkind)
        | Not_a_record_type ->
          let error = Wrong_expected_kind(Record, Pattern, expected_ty) in
          raise (Error (loc, !env, error))
      in
      let type_label_pat (label_lid, label, sarg) =
        let ty_arg =
          solve_Ppat_record_field ~refine loc env label label_lid record_ty in
        check_project_mutability ~loc ~env:!env label.lbl_mut alloc_mode.mode;
        let mode =
          Modality.Value.Const.apply label.lbl_modalities alloc_mode.mode
        in
        let alloc_mode = simple_pat_mode mode in
        (label_lid, label, type_pat tps Value ~alloc_mode sarg ty_arg)
      in
      let make_record_pat lbl_pat_list =
        check_recordpat_labels loc lbl_pat_list closed;
        {
          pat_desc = Tpat_record (lbl_pat_list, closed);
          pat_loc = loc; pat_extra=[];
          pat_type = instance record_ty;
          pat_attributes = sp.ppat_attributes;
          pat_env = !env;
        }
      in
      let lbl_a_list =
        wrap_disambiguate "This record pattern is expected to have"
          (mk_expected expected_ty)
          (disambiguate_sort_lid_a_list loc false !env Env.Projection expected_type)
          lid_sp_list
      in
      let lbl_a_list = List.map type_label_pat lbl_a_list in
      rvp @@ solve_expected (make_record_pat lbl_a_list)
  | Ppat_array spl ->
      type_pat_array (Mutable Alloc.Comonadic.Const.legacy)
         spl sp.ppat_attributes
  | Ppat_or(sp1, sp2) ->
      (* Reset pattern forces for just [tps2] because later we append
         [tps1] and [tps2]'s pattern forces, and we don't want to
         duplicate [tps]'s pattern forces.
      *)
      let tps1 = copy_type_pat_state tps in
      let tps2 = {(copy_type_pat_state tps) with tps_pattern_force = []} in
      let equation_level = !gadt_equations_level in
      let outter_lev = get_current_level () in
      (* Introduce a new scope using with_local_level without generalizations *)
      let env1, p1, env2, p2 =
        with_local_level begin fun () ->
          let lev = get_current_level () in
          gadt_equations_level := Some lev;
          let type_pat_rec tps env sp =
            type_pat tps category sp expected_ty ~env
          in
          let env1 = ref !env in
          let p1 = type_pat_rec tps1 env1 sp1 in
          let env2 = ref !env in
          let p2 = type_pat_rec tps2 env2 sp2 in
          (env1, p1, env2, p2)
        end ~post:(fun _ -> gadt_equations_level := equation_level)
      in
      let p1_variables = tps1.tps_pattern_variables in
      let p2_variables = tps2.tps_pattern_variables in
      (* Make sure no variable with an ambiguous type gets added to the
         environment. *)
      List.iter (fun { pv_type; pv_loc; _ } ->
        check_scope_escape pv_loc !env1 outter_lev pv_type
      ) p1_variables;
      List.iter (fun { pv_type; pv_loc; _ } ->
        check_scope_escape pv_loc !env2 outter_lev pv_type
      ) p2_variables;
      let vars, alpha_env =
        enter_orpat_variables loc !env p1_variables p2_variables in
      (* Propagate the outcome of checking the or-pattern back to
         the type_pat_state that the caller passed in.
      *)
      blit_type_pat_state
        ~src:
          { tps_pattern_variables = vars;
            (* We want to propagate all pattern forces, regardless of
               which branch they were found in.
            *)
            tps_pattern_force =
              tps2.tps_pattern_force @ tps1.tps_pattern_force;
            tps_module_variables = tps1.tps_module_variables;
          }
        ~dst:tps;
      let p2 = alpha_pat alpha_env p2 in
      rp { pat_desc = Tpat_or (p1, p2, None);
           pat_loc = loc; pat_extra = [];
           pat_type = instance expected_ty;
           pat_attributes = sp.ppat_attributes;
           pat_env = !env }
  | Ppat_lazy sp1 ->
      let nv = solve_Ppat_lazy ~refine loc env expected_ty in
      let p1 = type_pat tps Value sp1 nv in
      rvp {
        pat_desc = Tpat_lazy p1;
        pat_loc = loc; pat_extra=[];
        pat_type = instance expected_ty;
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
  | Ppat_constraint(sp_constrained, sty) ->
      (* Pretend separate = true *)
      let cty, ty, expected_ty' =
        let type_modes, sty = alloc_mode_from_ppat_constraint_typ_attrs sty in
        solve_Ppat_constraint ~refine tps loc env type_modes sty expected_ty
      in
      let p = type_pat ~alloc_mode tps category sp_constrained expected_ty' in
      let extra = (Tpat_constraint cty, loc, sp_constrained.ppat_attributes) in
      { p with pat_type = ty; pat_extra = extra::p.pat_extra }
  | Ppat_type lid ->
      let (path, p) = build_or_pat !env loc lid in
      pure category @@ solve_expected
        { p with pat_extra = (Tpat_type (path, lid), loc, sp.ppat_attributes)
        :: p.pat_extra }
  | Ppat_open (lid,p) ->
      let path, new_env =
        !type_open Asttypes.Fresh !env sp.ppat_loc lid in
      env := new_env;
      let p = type_pat tps category ~env p expected_ty in
      let new_env = !env in
      begin match Env.remove_last_open path new_env with
      | None -> assert false
      | Some closed_env -> env := closed_env
      end;
      { p with pat_extra = (Tpat_open (path,lid,new_env),
                                loc, sp.ppat_attributes) :: p.pat_extra }
  | Ppat_exception p ->
      let alloc_mode = simple_pat_mode Value.legacy in
      let p_exn = type_pat tps Value ~alloc_mode p Predef.type_exn in
      rcp {
        pat_desc = Tpat_exception p_exn;
        pat_loc = sp.ppat_loc;
        pat_extra = [];
        pat_type = expected_ty;
        pat_env = !env;
        pat_attributes = sp.ppat_attributes;
      }
  | Ppat_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

let type_pat tps category ?no_existentials
    ?(lev=get_current_level()) ~alloc_mode env sp expected_ty =
  Misc.protect_refs [Misc.R (gadt_equations_level, Some lev)] (fun () ->
        type_pat tps category ~no_existentials ~alloc_mode ~env sp expected_ty
    )

let type_pattern category ~lev ~alloc_mode env spat expected_ty allow_modules =
  let tps = create_type_pat_state allow_modules in
  let new_env = ref env in
  let pat = type_pat tps category ~lev ~alloc_mode new_env spat expected_ty in
  let { tps_pattern_variables = pvs;
        tps_module_variables = mvs;
        tps_pattern_force = forces;
      } = tps in
  (pat, !new_env, forces, pvs, mvs)

let type_pattern_list
    category no_existentials env spatl expected_tys allow_modules
  =
  let tps = create_type_pat_state allow_modules in
  let new_env = ref env in
  let type_pat (attrs, pat_mode, exp_mode, pat) ty =
    Builtin_attributes.warning_scope ~ppwarning:false attrs
      (fun () ->
         exp_mode,
         type_pat tps category
           ~no_existentials ~alloc_mode:pat_mode new_env pat ty
      )
  in
  let patl = List.map2 type_pat spatl expected_tys in
  let { tps_pattern_variables = pvs;
        tps_module_variables = mvs;
        tps_pattern_force = forces;
      } = tps in
  (patl, !new_env, forces, pvs, mvs)

let type_class_arg_pattern cl_num val_env met_env l spat =
  let pvs, pat =
    with_local_level_if_principal begin fun () ->
      let tps = create_type_pat_state Modules_rejected in
      let nv = newvar (Jkind.Type.Primitive.value ~why:Class_term_argument |> Jkind.of_type_jkind) in
      let alloc_mode = simple_pat_mode Value.legacy in
      let pat =
        type_pat tps Value ~no_existentials:In_class_args ~alloc_mode
          (ref val_env) spat nv in
      if has_variants pat then begin
        Parmatch.pressure_variants val_env [pat];
        finalize_variants pat;
      end;
      List.iter (fun f -> f()) tps.tps_pattern_force;
      (* CR layouts v5: value restriction here to be relaxed *)
      if is_optional l then
        unify_pat (ref val_env) pat
          (type_option (newvar Predef.option_argument_jkind));
      tps.tps_pattern_variables, pat
    end
      ~post:(fun (pvs, _) -> iter_pattern_variables_type generalize_structure
                               pvs)
  in
  let (pv, val_env, met_env) =
    List.fold_right
      (fun {pv_id; pv_uid; pv_type; pv_loc; pv_as_var; pv_attributes}
        (pv, val_env, met_env) ->
         let check s =
           if pv_as_var then Warnings.Unused_var s
           else Warnings.Unused_var_strict s in
         let id' = Ident.rename pv_id in
         let val_env =
          Env.add_value ~mode:Mode.Value.legacy pv_id
            { val_type = pv_type
            ; val_kind = Val_reg
            ; val_attributes = pv_attributes
            ; val_zero_alloc = Builtin_attributes.Default_zero_alloc
            ; val_modalities = Modality.Value.id
            ; val_loc = pv_loc
            ; val_uid = pv_uid
            }
            val_env
         in
         let met_env =
          Env.add_value ~mode:Mode.Value.legacy id' ~check
            { val_type = pv_type
            ; val_kind = Val_ivar (Immutable, cl_num)
            ; val_attributes = pv_attributes
            ; val_zero_alloc = Builtin_attributes.Default_zero_alloc
            ; val_modalities = Modality.Value.id
            ; val_loc = pv_loc
            ; val_uid = pv_uid
            }
            met_env
         in
         ((id', pv_id, pv_type)::pv, val_env, met_env))
      pvs ([], val_env, met_env)
  in
  (pat, pv, val_env, met_env)

let type_self_pattern env spat =
  let open Ast_helper in
  let spat = Pat.mk(Ppat_alias (spat, mknoloc "selfpat-*")) in
  let tps = create_type_pat_state Modules_rejected in
  let nv = newvar (Jkind.Type.Primitive.value ~why:Object |> Jkind.of_type_jkind) in
  let alloc_mode = simple_pat_mode Value.legacy in
  let pat =
    type_pat tps Value ~no_existentials:In_self_pattern ~alloc_mode
      (ref env) spat nv in
  List.iter (fun f -> f()) tps.tps_pattern_force;
  pat, tps.tps_pattern_variables

type pat_tuple_arity =
  | Not_local_tuple
  | Maybe_local_tuple
  | Local_tuple of int

let combine_pat_tuple_arity a b =
  match a, b with
  | Not_local_tuple, _ -> Not_local_tuple
  | _, Not_local_tuple -> Not_local_tuple
  | Maybe_local_tuple, Maybe_local_tuple -> Maybe_local_tuple
  | Maybe_local_tuple, Local_tuple _ -> b
  | Local_tuple _, Maybe_local_tuple -> a
  | Local_tuple ai, Local_tuple bi ->
      if ai = bi then a
      else Not_local_tuple

let rec pat_tuple_arity spat =
  match Jane_syntax.Pattern.of_ast spat with
  | Some (jpat, _attrs) -> pat_tuple_arity_jane_syntax jpat
  | None      ->
  match spat.ppat_desc with
  | Ppat_tuple args -> Local_tuple (List.length args)
  | Ppat_any | Ppat_exception _ | Ppat_var _ -> Maybe_local_tuple
  | Ppat_constant _
  | Ppat_interval _ | Ppat_construct _ | Ppat_variant _
  | Ppat_record _ | Ppat_array _ | Ppat_type _ | Ppat_lazy _
  | Ppat_unpack _ | Ppat_extension _ -> Not_local_tuple
  | Ppat_or(sp1, sp2) ->
      combine_pat_tuple_arity (pat_tuple_arity sp1) (pat_tuple_arity sp2)
  | Ppat_constraint(p, _) | Ppat_open(_, p) | Ppat_alias(p, _) -> pat_tuple_arity p

and pat_tuple_arity_jane_syntax : Jane_syntax.Pattern.t -> _ = function
  | Jpat_immutable_array (Iapat_immutable_array _) -> Not_local_tuple
  | Jpat_layout (Lpat_constant _) -> Not_local_tuple
  | Jpat_tuple (args, _) -> Local_tuple (List.length args)

let rec cases_tuple_arity cases =
  match cases with
  | [] -> Maybe_local_tuple
  | { pc_lhs; _ } :: rest ->
    match pat_tuple_arity pc_lhs with
    | Not_local_tuple -> Not_local_tuple
    | arity -> combine_pat_tuple_arity arity (cases_tuple_arity rest)


(** In [check_counter_example_pat], we will check a counter-example candidate
    produced by Parmatch. This is a pattern that represents a set of values by
    using or-patterns (p_1 | ... | p_n) to enumerate all alternatives in the
    counter-example search. These or-patterns occur at every choice point,
    possibly deep inside the pattern.

    Parmatch does not use type information, so this pattern may
    exhibit two issues:
    - some parts of the pattern may be ill-typed due to GADTs, and
    - some wildcard patterns may not match any values: their type is
      empty.

    The aim of [check_counter_example_pat] is to refine this untyped pattern
    into a well-typed pattern, and ensure that it matches at least one
    concrete value.
    - It filters ill-typed branches of or-patterns.
      (see {!splitting_mode} below)
    - It tries to check that wildcard patterns are non-empty.
      (see {!explosion_fuel})
  *)

type counter_example_checking_info = {
    explosion_fuel: int;
    splitting_mode: splitting_mode;
  }
(**
    [explosion_fuel] controls the checking of wildcard patterns.  We
    eliminate potentially-empty wildcard patterns by exploding them
    into concrete sub-patterns, for example (K1 _ | K2 _) or
    { l1: _; l2: _ }. [explosion_fuel] is the depth limit on wildcard
    explosion. Such depth limit is required to avoid non-termination
    and compilation-time blowups.

    [splitting_mode] controls the handling of or-patterns.  In
    [Counter_example] mode, we only need to select one branch that
    leads to a well-typed pattern. Checking all branches is expensive,
    we use different search strategies (see {!splitting_mode}) to
    reduce the number of explored alternatives.
 *)

(** Due to GADT constraints, an or-pattern produced within
    a counter-example may have ill-typed branches. Consider for example

    {[
      type _ tag = Int : int tag | Bool : bool tag
    ]}

    then [Parmatch] will propose the or-pattern [Int | Bool] whenever
    a pattern of type [tag] is required to form a counter-example. For
    example, a function expects a (int tag option) and only [None] is
    handled by the user-written pattern. [Some (Int | Bool)] is not
    well-typed in this context, only the sub-pattern [Some Int] is.
    In this example, the expected type coming from the context
    suffices to know which or-pattern branch must be chosen.

    In the general case, choosing a branch can have non-local effects
    on the typability of the term. For example, consider a tuple type
    ['a tag * ...'a...], where the first component is a GADT.  All
    constructor choices for this GADT lead to a well-typed branch in
    isolation (['a] is unconstrained), but choosing one of them adds
    a constraint on ['a] that may make the other tuple elements
    ill-typed.

    In general, after choosing each possible branch of the or-pattern,
    [check_counter_example_pat] has to check the rest of the pattern to
    tell if this choice leads to a well-typed term. This may lead to an
    explosion of typing/search work -- the rest of the term may in turn
    contain alternatives.

    We use careful strategies to try to limit counterexample-checking
    time; [splitting_mode] represents those strategies.
*)
and splitting_mode =
  | Backtrack_or
  (** Always backtrack in or-patterns.

      [Backtrack_or] selects a single alternative from an or-pattern
      by using backtracking, trying to choose each branch in turn, and
      to complete it into a valid sub-pattern. We call this
      "splitting" the or-pattern.

      We use this mode when looking for unused patterns or sub-patterns,
      in particular to check a refutation clause (p -> .).
    *)
  | Refine_or of { inside_nonsplit_or: bool; }
  (** Only backtrack when needed.

      [Refine_or] tries another approach for refining or-pattern.

      Instead of always splitting each or-pattern, It first attempts to
      find branches that do not introduce new constraints (because they
      do not contain GADT constructors). Those branches are such that,
      if they fail, all other branches will fail.

      If we find one such branch, we attempt to complete the subpattern
      (checking what's outside the or-pattern), ignoring other
      branches -- we never consider another branch choice again. If all
      branches are constrained, it falls back to splitting the
      or-pattern.

      We use this mode when checking exhaustivity of pattern matching.
  *)

(** This exception is only used internally within [check_counter_example_pat],
    to jump back to the parent or-pattern in the [Refine_or] strategy.

    Such a parent exists precisely when [inside_nonsplit_or = true];
    it's an invariant that we always setup an exception handler for
    [Need_backtrack] when we set this flag. *)
exception Need_backtrack

(** This exception is only used internally within [check_counter_example_pat].
    We use it to discard counter-example candidates that do not match any
    value. *)
exception Empty_branch

type abort_reason = Adds_constraints | Empty

(** Remember current typing state for backtracking.
    No variable information, as we only backtrack on
    patterns without variables (cf. assert statements).
    In the GADT mode, [env] may be extended by unification,
    and therefore it needs to be saved along with a [snapshot]. *)
type unification_state =
 { snapshot: snapshot;
   env: Env.t; }
let save_state env =
  { snapshot = Btype.snapshot ();
    env = !env; }
let set_state s env =
  Btype.backtrack s.snapshot;
  env := s.env

(** Find the first alternative in the tree of or-patterns for which
    [f] does not raise an error. If all fail, the last error is
    propagated *)
let rec find_valid_alternative f pat =
  match pat.pat_desc with
  | Tpat_or(p1,p2,_) ->
      (try find_valid_alternative f p1 with
       | Empty_branch | Error _ -> find_valid_alternative f p2
      )
  | _ -> f pat

let no_explosion info = { info with explosion_fuel = 0 }

let enter_nonsplit_or info =
  let splitting_mode = match info.splitting_mode with
  | Backtrack_or ->
      (* in Backtrack_or mode, or-patterns are always split *)
      assert false
  | Refine_or _ ->
      Refine_or {inside_nonsplit_or = true}
  in { info with splitting_mode }

let rec check_counter_example_pat
          ~info ~env type_pat_state tp expected_ty k =
  let check_rec ?(info=info) ?(env=env) =
    check_counter_example_pat ~info ~env type_pat_state in
  let loc = tp.pat_loc in
  let refine = Some true in
  let alloc_mode = simple_pat_mode Value.min in
  let solve_expected (x : pattern) : pattern =
    unify_pat ~refine env x (instance expected_ty);
    x
  in
  (* "make pattern" and "make pattern then continue" *)
  let mp ?(pat_type = expected_ty) desc =
    { pat_desc = desc; pat_loc = loc; pat_extra=[];
      pat_type = instance pat_type; pat_attributes = []; pat_env = !env } in
  let mkp k ?pat_type desc = k (mp ?pat_type desc) in
  let must_backtrack_on_gadt =
    match info.splitting_mode with
    | Backtrack_or -> false
    | Refine_or {inside_nonsplit_or} -> inside_nonsplit_or
  in
  match tp.pat_desc with
    Tpat_any | Tpat_var _ ->
      let k' () = mkp k tp.pat_desc in
      if info.explosion_fuel <= 0 then k' () else
      let decrease n = {info with explosion_fuel = info.explosion_fuel - n} in
      begin match Parmatch.pats_of_type !env expected_ty with
      | [] -> raise Empty_branch
      | [{pat_desc = Tpat_any}] -> k' ()
      | [tp] -> check_rec ~info:(decrease 1) tp expected_ty k
      | tp :: tpl ->
          if must_backtrack_on_gadt then raise Need_backtrack;
          let tp =
            List.fold_left
              (fun tp tp' -> {tp with pat_desc = Tpat_or (tp, tp', None)})
              tp tpl
          in
          check_rec ~info:(decrease 5) tp expected_ty k
      end
  | Tpat_alias (p, _, _, _, _) -> check_rec ~info p expected_ty k
  | Tpat_constant cst ->
      let cst =
        match Untypeast.constant cst with
        | `Parsetree cst -> constant_or_raise !env loc cst
        | `Jane_syntax cst -> unboxed_constant_or_raise !env loc cst
      in
      k @@ solve_expected (mp (Tpat_constant cst) ~pat_type:(type_constant cst))
  | Tpat_tuple tpl ->
      let tpl_ann =
        solve_Ppat_tuple ~refine ~alloc_mode loc env tpl
          expected_ty
      in
      map_fold_cont (fun (l,p,t,_) k -> check_rec p t (fun p -> k (l, p)))
        tpl_ann
        (fun pl ->
           mkp k (Tpat_tuple pl)
             ~pat_type:(newty (Ttuple (List.map (fun (l,p) -> (l,p.pat_type))
                                         pl))))
  | Tpat_construct(cstr_lid, constr, targs, _) ->
      if constr.cstr_generalized && must_backtrack_on_gadt then
        raise Need_backtrack;
      let (ty_args, _, existential_ctyp) =
        solve_Ppat_construct ~refine type_pat_state env loc constr None None
          expected_ty
      in
      map_fold_cont
        (fun (p,t) -> check_rec p t)
        (List.combine targs ty_args)
        (fun args ->
          mkp k (Tpat_construct(cstr_lid, constr, args, existential_ctyp)))
  | Tpat_variant(tag, targ, _) ->
      let constant = (targ = None) in
      let arg_type, row, pat_type =
        solve_Ppat_variant ~refine loc env tag constant expected_ty in
      let k arg =
        mkp k ~pat_type (Tpat_variant(tag, arg, ref row))
      in begin
        (* PR#6235: propagate type information *)
        match targ, arg_type with
          Some p, [ty] -> check_rec p ty (fun p -> k (Some p))
        | _            -> k None
      end
  | Tpat_record(fields, closed) ->
      let record_ty = generic_instance expected_ty in
      let type_label_pat (label_lid, label, targ) k =
        let ty_arg =
          solve_Ppat_record_field ~refine loc env label label_lid record_ty in
        check_rec targ ty_arg (fun arg -> k (label_lid, label, arg))
      in
      map_fold_cont type_label_pat fields
        (fun fields -> mkp k (Tpat_record (fields, closed)))
  | Tpat_array (mut, original_arg_sort, tpl) ->
      let ty_elt, arg_sort = solve_Ppat_array ~refine loc env mut expected_ty in
      assert (Jkind.Type.Sort.equate original_arg_sort arg_sort);
      map_fold_cont (fun p -> check_rec p ty_elt) tpl
        (fun pl -> mkp k (Tpat_array (mut, arg_sort, pl)))
  | Tpat_or(tp1, tp2, _) ->
      (* We are in counter-example mode, but try to avoid backtracking *)
      let must_split =
        match info.splitting_mode with
        | Backtrack_or -> true
        | Refine_or _ -> false in
      let state = save_state env in
      let split_or tp =
        let type_alternative pat =
          set_state state env; check_rec pat expected_ty k in
        find_valid_alternative type_alternative tp
      in
      if must_split then split_or tp else
      let check_rec_result env tp : (_, abort_reason) result =
        let info = enter_nonsplit_or info in
        match check_rec ~info tp expected_ty ~env (fun x -> x) with
        | res -> Ok res
        | exception Need_backtrack -> Error Adds_constraints
        | exception Empty_branch -> Error Empty
      in
      let p1 = check_rec_result (ref !env) tp1 in
      let p2 = check_rec_result (ref !env) tp2 in
      begin match p1, p2 with
      | Error Empty, Error Empty ->
          raise Empty_branch
      | Error Adds_constraints, Error _
      | Error _, Error Adds_constraints ->
          let inside_nonsplit_or =
            match info.splitting_mode with
            | Backtrack_or -> false
            | Refine_or {inside_nonsplit_or} -> inside_nonsplit_or in
          if inside_nonsplit_or
          then raise Need_backtrack
          else split_or tp
      | Ok p, Error _
      | Error _, Ok p ->
          k p
      | Ok p1, Ok p2 ->
          mkp k (Tpat_or (p1, p2, None))
      end
  | Tpat_lazy tp1 ->
      let nv = solve_Ppat_lazy ~refine loc env expected_ty in
      (* do not explode under lazy: PR#7421 *)
      check_rec ~info:(no_explosion info) tp1 nv
        (fun p1 -> mkp k (Tpat_lazy p1))

let check_counter_example_pat ~counter_example_args
    ?(lev=get_current_level()) env tp expected_ty =
  (* [check_counter_example_pat] doesn't use [type_pat_state] in an interesting
     way -- one of the functions it calls writes an entry into
     [tps_pattern_forces] -- so we can just ignore module patterns. *)
  let type_pat_state = create_type_pat_state Modules_ignored in
  Misc.protect_refs [Misc.R (gadt_equations_level, Some lev)] (fun () ->
    check_counter_example_pat
      ~info:counter_example_args ~env type_pat_state tp expected_ty (fun x -> x)
    )

(* this function is passed to Partial.parmatch
   to type check gadt nonexhaustiveness *)
let partial_pred ~lev ~splitting_mode ?(explode=0)
      env expected_ty p =
  let env = ref env in
  let state = save_state env in
  let counter_example_args =
      {
        splitting_mode;
        explosion_fuel = explode;
      } in
  try
    let typed_p =
      check_counter_example_pat ~lev ~counter_example_args env p
        expected_ty
    in
    set_state state env;
    (* types are invalidated but we don't need them here *)
    Some typed_p
  with Error _ | Empty_branch ->
    set_state state env;
    None

let check_partial
      ?(lev=get_current_level ()) env expected_ty loc cases
  =
  let explode = match cases with [_] -> 5 | _ -> 0 in
  let splitting_mode = Refine_or {inside_nonsplit_or = false} in
  Parmatch.check_partial
    (partial_pred ~lev ~splitting_mode ~explode env expected_ty)
    loc cases

let check_unused
      ?(lev=get_current_level ()) env expected_ty cases
  =
  Parmatch.check_unused
    (fun refute pat ->
      match
        partial_pred ~lev ~splitting_mode:Backtrack_or ~explode:5
          env expected_ty pat
      with
        Some pat' when refute ->
          raise (Error (pat.pat_loc, env, Unrefuted_pattern pat'))
      | r -> r)
    cases

(** Some delayed checks, to be executed after typing the whole
    compilation unit or toplevel phrase *)
let delayed_checks = ref []
let reset_delayed_checks () = delayed_checks := []
let add_delayed_check f =
  delayed_checks := (f, Warnings.backup ()) :: !delayed_checks

let force_delayed_checks () =
  (* checks may change type levels *)
  let snap = Btype.snapshot () in
  let w_old = Warnings.backup () in
  List.iter
    (fun (f, w) -> Warnings.restore w; f ())
    (List.rev !delayed_checks);
  Warnings.restore w_old;
  reset_delayed_checks ();
  Btype.backtrack snap

let rec final_subexpression exp =
  match exp.exp_desc with
    Texp_let (_, _, e)
  | Texp_sequence (_, _, e)
  | Texp_try (e, _)
  | Texp_ifthenelse (_, e, _)
  | Texp_match (_, _, {c_rhs=e} :: _, _)
  | Texp_letmodule (_, _, _, _, e)
  | Texp_letexception (_, e)
  | Texp_open (_, e)
    -> final_subexpression e
  | _ -> exp

let is_prim ~name funct =
  match funct.exp_desc with
  | Texp_ident (_, _, {val_kind=Val_prim{Primitive.prim_name; _}}, Id_prim _, _) ->
      prim_name = name
  | _ -> false

(* List labels in a function type, and whether return type is a variable *)
let rec list_labels_aux env visited ls ty_fun =
  let ty = expand_head env ty_fun in
  if TypeSet.mem ty visited then
    List.rev ls, false
  else match get_desc ty with
    Tarrow ((l,_,_), _, ty_res, _) ->
      list_labels_aux env (TypeSet.add ty visited) (l::ls) ty_res
  | _ ->
      List.rev ls, is_Tvar ty

let list_labels env ty =
  wrap_trace_gadt_instances env (list_labels_aux env TypeSet.empty []) ty

(* Collecting arguments for function applications *)

type untyped_apply_arg =
  | Known_arg of
      { sarg : Parsetree.expression;
        ty_arg : type_expr;
        ty_arg0 : type_expr;
        sort_arg : Jkind.Type.sort;
        commuted : bool;
        mode_fun : Alloc.lr;
        mode_arg : Alloc.lr;
        wrapped_in_some : bool; }
  | Unknown_arg of
      { sarg : Parsetree.expression;
        ty_arg_mono : type_expr;
        sort_arg : Jkind.Type.sort;
        mode_fun : Alloc.lr;
        mode_arg : Alloc.lr}
  | Eliminated_optional_arg of
      { expected_label: arg_label;
        mode_fun: Alloc.lr;
        ty_arg : type_expr;
        sort_arg : Jkind.Type.sort;
        mode_arg : Alloc.lr;
        level: int; }

type untyped_omitted_param =
  { mode_fun: Alloc.lr;
    ty_arg : type_expr;
    mode_arg : Alloc.lr;
    level: int;
    sort_arg : Jkind.Type.sort }

let is_partial_apply args =
  List.exists
    (fun (_, arg) ->
       match arg with
       | Omitted _ -> true
       | Arg _ -> false)
    args

let remaining_function_type ty_ret mode_ret rev_args =
  let ty_ret, _, _ =
    List.fold_left
      (fun (ty_ret, mode_ret, closed_args) (lbl, arg) ->
         match arg with
         | Arg (Unknown_arg { mode_arg; _ } | Known_arg { mode_arg; _ }) ->
             let closed_args = mode_arg :: closed_args in
             (ty_ret, mode_ret, closed_args)
         | Arg (Eliminated_optional_arg
                  { mode_fun; ty_arg; mode_arg; level; _ })
         | Omitted { mode_fun; ty_arg; mode_arg; level } ->
             let arrow_desc = lbl, mode_arg, mode_ret in
             let ty_ret =
               newty2 ~level
                 (Tarrow (arrow_desc, ty_arg, ty_ret, commu_ok))
             in
             let mode_ret, _ =
               Alloc.newvar_above (Alloc.join (mode_fun :: closed_args))
             in
             (ty_ret, mode_ret, closed_args))
      (ty_ret, mode_ret, []) rev_args
  in
  ty_ret

(* Check that within a single application, the return modes of curried arrows
   increase along the application. That is, check that this is not an
   unparenthesized over-application of a local function that returns a global
   function.

   This check is not required for soundness, but including it simplifies the
   principal types of applications, making the inferred types more sensible
   in ml files that lack an mli. *)
let check_local_application_complete ~env ~app_loc args =
  let arg_mode_fun (_lbl, arg) =
    match arg with
    | Arg ( Known_arg { mode_fun; _ }
          | Unknown_arg { mode_fun; _ }
          | Eliminated_optional_arg { mode_fun; _ })
    | Omitted { mode_fun; _ } -> mode_fun
  in
  let rec loop has_commuted = function
    | [] | [_] -> ()
    | (lbl, ( Arg ( Known_arg { mode_fun; mode_arg; _ }
                  | Unknown_arg { mode_fun; mode_arg; _ }
                  | Eliminated_optional_arg { mode_fun; mode_arg; _ })
            | Omitted { mode_fun; mode_arg; _ } as arg))
      :: ((next :: _) as rest) ->
      let mode_ret = arg_mode_fun next in
      let has_commuted =
        has_commuted ||
        match arg with
        | Arg (Known_arg { commuted }) -> commuted
        | _ -> false
      in
      let submode m1 m2 =
        match Alloc.submode m1 m2 with
        | Ok () -> ()
        | Error _ ->
          let loc, loc_kind =
            match arg with
            | Arg (Known_arg {sarg; _} | Unknown_arg {sarg; _}) ->
              if has_commuted then
                sarg.pexp_loc, `Single_arg
              else
                Location.{ loc_start = app_loc.loc_start;
                           loc_end = sarg.pexp_loc.loc_end;
                           loc_ghost = app_loc.loc_ghost || sarg.pexp_loc.loc_ghost },
                `Prefix
            | _ ->
              app_loc, `Entire_apply
          in
          raise (Error(loc, env, Local_application_complete (lbl, loc_kind)))
      in
      submode mode_fun mode_ret;
      submode mode_arg mode_ret;
      loop has_commuted rest
  in
  loop false args

let collect_unknown_apply_args env funct ty_fun mode_fun rev_args sargs ret_tvar =
  let labels_match ~param ~arg =
    param = arg
    || !Clflags.classic && arg = Nolabel && not (is_omittable param)
  in
  let has_label l ty_fun =
    let ls, tvar = list_labels env ty_fun in
    tvar || List.mem l ls
  in
  let rec loop ty_fun mode_fun rev_args sargs =
    match sargs with
    | [] -> ty_fun, mode_fun, List.rev rev_args
    | (lbl, sarg) :: rest ->
        let (sort_arg, mode_arg, ty_arg_mono, mode_ret, ty_res) =
          let ty_fun = expand_head env ty_fun in
          match get_desc ty_fun with
          | Tvar _ ->
              let ty_arg_mono, sort_arg = new_rep_var ~why:Function_argument () in
              let ty_arg = newmono ty_arg_mono in
              let ty_res =
                newvar (Jkind.of_new_sort ~why:Function_result)
              in
              if ret_tvar &&
                 not (is_prim ~name:"%identity" funct) &&
                 not (is_prim ~name:"%obj_magic" funct)
              then
                Location.prerr_warning sarg.pexp_loc
                  Warnings.Ignored_extra_argument;
              let mode_arg = Alloc.newvar () in
              let mode_ret = Alloc.newvar () in
              let kind = (lbl, mode_arg, mode_ret) in
              unify env ty_fun
                (newty (Tarrow(kind,ty_arg,ty_res,commu_var ())));
              (sort_arg, mode_arg, ty_arg_mono, mode_ret, ty_res)
        | Tarrow ((l, mode_arg, mode_ret), ty_arg, ty_res, _)
          when labels_match ~param:l ~arg:lbl ->
            let sort_arg =
              match type_sort ~why:Function_argument env ty_arg with
              | Ok sort -> sort
              | Error err -> raise(Error(funct.exp_loc, env,
                                         Function_type_not_rep (ty_arg,err)))
            in
            (sort_arg, mode_arg, tpoly_get_mono ty_arg, mode_ret, ty_res)
        | td ->
            let ty_fun = match td with Tarrow _ -> newty td | _ -> ty_fun in
            let ty_res = remaining_function_type ty_fun mode_fun rev_args in
            match get_desc ty_res with
            | Tarrow _ ->
                if !Clflags.classic || not (has_label lbl ty_fun) then
                  raise (Error(sarg.pexp_loc, env,
                               Apply_wrong_label(lbl, ty_res, false)))
                else
                  raise (Error(funct.exp_loc, env, Incoherent_label_order))
            | _ ->
                let previous_arg_loc =
                  (* [rev_args] is the arguments typed until now, in reverse
                     order of appearance. Not all arguments have a location
                     attached (eg. an optional argument that is not passed). *)
                  (* CR ccasinghino: the above comment is confusing - these
                     arguments are in reverse order according to the function
                     type, but not according to their positions in the source
                     program.  We diverge from upstream here by not trying to
                     provide a good location in the [Eliminated_optional_arg]
                     case - maybe fix one day if it is noticeable. *)
                  rev_args
                  |> List.find_map
                       (function
                         | (_, Arg ( Known_arg { sarg; _ }
                                   | Unknown_arg { sarg; _ })) ->
                           Some sarg.pexp_loc
                         | (_, Arg (Eliminated_optional_arg _))
                         | (_, Omitted _) -> None)
                  |> Option.value ~default:funct.exp_loc
                in
                raise(Error(funct.exp_loc, env, Apply_non_function {
                    funct;
                    func_ty = expand_head env funct.exp_type;
                    res_ty = expand_head env ty_res;
                    previous_arg_loc;
                    extra_arg_loc = sarg.pexp_loc; }))
        in
        let arg =
          Unknown_arg { sarg; ty_arg_mono; mode_fun; mode_arg; sort_arg }
        in
        loop ty_res mode_ret ((lbl, Arg arg) :: rev_args) rest
  in
  loop ty_fun mode_fun rev_args sargs

let collect_apply_args env funct ignore_labels ty_fun ty_fun0 mode_fun sargs ret_tvar =
  let warned = ref false in
  let rec loop ty_fun ty_fun0 mode_fun rev_args sargs =
    let type_unknown_args () =
      (* We're not looking at a *known* function type anymore, or there are no
         arguments left. *)
      collect_unknown_apply_args env funct ty_fun0 mode_fun rev_args sargs ret_tvar
    in
    if sargs = [] then type_unknown_args () else
    let ty_fun' = expand_head env ty_fun in
    match get_desc ty_fun', get_desc (expand_head env ty_fun0), sargs with
    | Tarrow (ad, ty_arg, ty_ret, com),
      Tarrow (_, ty_arg0, ty_ret0, _),
      (_, sarg1) :: _
      when is_commu_ok com ->
        let lv = get_level ty_fun' in
        let (l, mode_arg, mode_ret) = ad in
        let may_warn loc w =
          if not !warned && !Clflags.principal && lv <> generic_level
          then begin
            warned := true;
            Location.prerr_warning loc w
          end
        in
        let sort_arg = match type_sort ~why:Function_argument env ty_arg with
          | Ok sort -> sort
          | Error err -> raise(Error(sarg1.pexp_loc, env,
                                     Function_type_not_rep(ty_arg, err)))
        in
        let name = label_name l
        and optional = is_optional l
        and omittable = is_omittable l in
        let use_arg ~commuted sarg l' =
          let wrapped_in_some = optional && not (is_optional l') in
          if wrapped_in_some then
            may_warn sarg.pexp_loc
              (Warnings.Not_principal "using an optional argument here");
          Arg (Known_arg
            { sarg; ty_arg; ty_arg0; commuted; sort_arg;
              mode_fun; mode_arg; wrapped_in_some })
        in
        let eliminate_omittable_arg expected_label =
          may_warn funct.exp_loc
            (Warnings.Non_principal_labels "eliminated omittable argument");
          Arg
            (Eliminated_optional_arg
               { mode_fun; ty_arg; mode_arg
               ; sort_arg; level = lv; expected_label})
        in
        let remaining_sargs, arg =
          if ignore_labels then begin
            (* No reordering is allowed, process arguments in order *)
            match sargs with
            | [] -> assert false
            | (l', sarg) :: remaining_sargs ->
                if name = label_name l' || (not omittable && l' = Nolabel) then
                  (remaining_sargs, use_arg ~commuted:false sarg l')
                else if
                  omittable &&
                  not (List.exists (fun (l, _) -> name = label_name l)
                         remaining_sargs) &&
                  List.exists (function (Nolabel, _) -> true | _ -> false)
                    sargs
                then
                  (sargs, eliminate_omittable_arg l)
                else
                  raise(Error(sarg.pexp_loc, env,
                              Apply_wrong_label(l', ty_fun', omittable)))
          end else
            (* Arguments can be commuted, try to fetch the argument
               corresponding to the first parameter. *)
            match extract_label name sargs with
            | Some (l', sarg, commuted, remaining_sargs) ->
                if commuted then begin
                  may_warn sarg.pexp_loc
                    (Warnings.Not_principal "commuting this argument")
                end;
                if not optional && is_optional l' then (
                  let label = Printtyp.string_of_label l in
                  if is_position l
                  then
                    raise
                      (Error
                         ( sarg.pexp_loc
                         , env
                         , Nonoptional_call_pos_label label))
                  else
                    Location.prerr_warning
                      sarg.pexp_loc
                      (Warnings.Nonoptional_label label));
                remaining_sargs, use_arg ~commuted sarg l'
            | None ->
                sargs,
                if omittable && List.mem_assoc Nolabel sargs then
                  eliminate_omittable_arg l
                else begin
                  (* No argument was given for this parameter, we abstract over
                     it. *)
                  may_warn funct.exp_loc
                    (Warnings.Non_principal_labels "commuted an argument");
                  Omitted { mode_fun; ty_arg; mode_arg; level = lv; sort_arg }
                end
        in
        loop ty_ret ty_ret0 mode_ret ((l, arg) :: rev_args) remaining_sargs
    | _ ->
        type_unknown_args ()
  in
  loop ty_fun ty_fun0 mode_fun [] sargs

let type_omitted_parameters expected_mode env ty_ret mode_ret args =
  let ty_ret, mode_ret, _, _, args =
    List.fold_left
      (fun (ty_ret, mode_ret, open_args, closed_args, args) (lbl, arg) ->
         match arg with
         | Arg (exp, marg, sort) ->
             let open_args = (exp, marg) :: open_args in
             let args = (lbl, Arg (exp, sort)) :: args in
             (ty_ret, mode_ret, open_args, closed_args, args)
         | Omitted { mode_fun; ty_arg; mode_arg; level; sort_arg } ->
             let arrow_desc = (lbl, mode_arg, mode_ret) in
             let ty_ret =
               newty2 ~level
                 (Tarrow (arrow_desc, ty_arg, ty_ret, commu_ok))
             in
             let new_closed_args =
               List.map
                 (fun (exp, marg) ->
                    submode ~loc:exp.exp_loc ~env ~reason:Other
                      marg (mode_partial_application expected_mode);
                    value_to_alloc_r2l marg)
                 open_args
             in
             let closed_args = new_closed_args @ closed_args in
             let open_args = [] in
             let mode_closed_args = List.map Alloc.close_over closed_args in
             let mode_partial_fun = Alloc.partial_apply mode_fun in
             let mode_closure, _ =
               Alloc.newvar_above (Alloc.join
                (mode_partial_fun:: mode_closed_args))
             in
             register_allocation_mode mode_closure;
             let arg =
              Omitted {
                mode_closure = Alloc.disallow_left mode_closure;
                mode_arg = Alloc.disallow_right mode_arg;
                mode_ret = Alloc.disallow_right mode_ret; sort_arg }
             in
             let args = (lbl, arg) :: args in
             (ty_ret, mode_closure, open_args, closed_args, args))
      (ty_ret, mode_ret, [], [], []) (List.rev args)
  in
  ty_ret, mode_ret, args

(* Generalization criterion for expressions *)

let rec is_nonexpansive exp =
  match exp.exp_desc with
  | Texp_ident _
  | Texp_constant _
  | Texp_unreachable
  | Texp_function _
  | Texp_probe_is_enabled _
  | Texp_src_pos
  | Texp_array (_, _, [], _) -> true
  | Texp_let(_rec_flag, pat_exp_list, body) ->
      List.for_all (fun vb -> is_nonexpansive vb.vb_expr) pat_exp_list &&
      is_nonexpansive body
  | Texp_apply(e, (_,Omitted _)::el, _, _, _) ->
      is_nonexpansive e && List.for_all is_nonexpansive_arg (List.map snd el)
  | Texp_match(e, _, cases, _) ->
     (* Not sure this is necessary, if [e] is nonexpansive then we shouldn't
         care if there are exception patterns. But the previous version enforced
         that there be none, so... *)
      let contains_exception_pat pat =
        exists_general_pattern { f = fun (type k) (p : k general_pattern) ->
          match p.pat_desc with
          | Tpat_exception _ -> true
          | _ -> false } pat
      in
      is_nonexpansive e &&
      List.for_all
        (fun {c_lhs; c_guard; c_rhs} ->
           is_nonexpansive_opt c_guard && is_nonexpansive c_rhs
           && not (contains_exception_pat c_lhs)
        ) cases
  | Texp_probe {handler} -> is_nonexpansive handler
  | Texp_tuple (el, _) ->
      List.for_all (fun (_,e) -> is_nonexpansive e) el
  | Texp_construct(_, _, el, _) ->
      List.for_all is_nonexpansive el
  | Texp_variant(_, arg) -> is_nonexpansive_opt (Option.map fst arg)
  | Texp_record { fields; extended_expression } ->
      Array.for_all
        (fun (lbl, definition) ->
           match definition with
           | Overridden (_, exp) ->
               lbl.lbl_mut = Immutable && is_nonexpansive exp
           | Kept _ -> true)
        fields
      && is_nonexpansive_opt extended_expression
  | Texp_field(exp, _, _, _) -> is_nonexpansive exp
  | Texp_ifthenelse(_cond, ifso, ifnot) ->
      is_nonexpansive ifso && is_nonexpansive_opt ifnot
  | Texp_sequence (_e1, _jkind, e2) -> is_nonexpansive e2  (* PR#4354 *)
  | Texp_new (_, _, cl_decl, _) -> Btype.class_type_arity cl_decl.cty_type > 0
  (* Note: nonexpansive only means no _observable_ side effects *)
  | Texp_lazy e -> is_nonexpansive e
  | Texp_object ({cstr_fields=fields; cstr_type = { csig_vars=vars}}, _) ->
      let count = ref 0 in
      List.for_all
        (fun field -> match field.cf_desc with
            Tcf_method _ -> true
          | Tcf_val (_, _, _, Tcfk_concrete (_, e), _) ->
              incr count; is_nonexpansive e
          | Tcf_val (_, _, _, Tcfk_virtual _, _) ->
              incr count; true
          | Tcf_initializer e -> is_nonexpansive e
          | Tcf_constraint _ -> true
          | Tcf_inherit _ -> false
          | Tcf_attribute _ -> true)
        fields &&
      Vars.fold (fun _ (mut,_,_) b -> decr count; b && mut = Asttypes.Immutable)
        vars true &&
      !count = 0
  | Texp_letmodule (_, _, _, mexp, e)
  | Texp_open ({ open_expr = mexp; _}, e) ->
      is_nonexpansive_mod mexp && is_nonexpansive e
  | Texp_pack mexp ->
      is_nonexpansive_mod mexp
  (* Computations which raise exceptions are nonexpansive, since (raise e) is
     equivalent to (raise e; diverge), and a nonexpansive "diverge" can be
     produced using lazy values or the relaxed value restriction.
     See GPR#1142 *)
  | Texp_assert (exp, _) ->
      is_nonexpansive exp
  | Texp_apply (
      { exp_desc = Texp_ident (_, _, {val_kind =
             Val_prim {Primitive.prim_name =
                         ("%raise" | "%reraise" | "%raise_notrace")}},
             Id_prim _, _) },
      [Nolabel, Arg (e, _)], _, _, _) ->
     is_nonexpansive e
  | Texp_array (_, _, _ :: _, _)
  | Texp_apply _
  | Texp_try _
  | Texp_setfield _
  | Texp_list_comprehension _
  | Texp_array_comprehension _
  | Texp_while _
  | Texp_for _
  | Texp_send _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_letexception _
  | Texp_letop _
  | Texp_extension_constructor _ ->
    false
  | Texp_exclave e -> is_nonexpansive e

and is_nonexpansive_mod mexp =
  match mexp.mod_desc with
  | Tmod_ident _
  | Tmod_functor _ -> true
  | Tmod_unpack (e, _) -> is_nonexpansive e
  | Tmod_constraint (m, _, _, _) -> is_nonexpansive_mod m
  | Tmod_structure str ->
      List.for_all
        (fun item -> match item.str_desc with
          | Tstr_eval _ | Tstr_primitive _ | Tstr_type _
          | Tstr_modtype _ | Tstr_class_type _  -> true
          | Tstr_value (_, pat_exp_list) ->
              List.for_all (fun vb -> is_nonexpansive vb.vb_expr) pat_exp_list
          | Tstr_module {mb_expr=m;_}
          | Tstr_open {open_expr=m;_}
          | Tstr_include {incl_mod=m;_} -> is_nonexpansive_mod m
          | Tstr_recmodule id_mod_list ->
              List.for_all (fun {mb_expr=m;_} -> is_nonexpansive_mod m)
                id_mod_list
          | Tstr_exception {tyexn_constructor = {ext_kind = Text_decl _}} ->
              false (* true would be unsound *)
          | Tstr_exception {tyexn_constructor = {ext_kind = Text_rebind _}} ->
              true
          | Tstr_typext te ->
              List.for_all
                (function {ext_kind = Text_decl _} -> false
                        | {ext_kind = Text_rebind _} -> true)
                te.tyext_constructors
          | Tstr_class _ -> false (* could be more precise *)
          | Tstr_attribute _ -> true
        )
        str.str_items
  | Tmod_apply _ | Tmod_apply_unit _ -> false

and is_nonexpansive_opt = function
  | None -> true
  | Some e -> is_nonexpansive e

and is_nonexpansive_arg = function
  | Omitted _ -> true
  | Arg (e, _) -> is_nonexpansive e

let maybe_expansive e = not (is_nonexpansive e)

let annotate_recursive_bindings env valbinds =
  let ids = let_bound_idents valbinds in
  List.map
    (fun {vb_pat; vb_expr; vb_rec_kind = _; vb_sort; vb_attributes; vb_loc} ->
       match (Value_rec_check.is_valid_recursive_expression ids vb_expr) with
       | None ->
         raise(Error(vb_expr.exp_loc, env, Illegal_letrec_expr))
       | Some vb_rec_kind ->
         { vb_pat; vb_expr; vb_rec_kind; vb_sort; vb_attributes; vb_loc})
    valbinds

let check_recursive_class_bindings env ids exprs =
  List.iter
    (fun expr ->
       if not (Value_rec_check.is_valid_class_expr ids expr) then
         raise(Error(expr.cl_loc, env, Illegal_class_expr)))
    exprs

module Is_local_returning : sig
  val function_body : Jane_syntax.N_ary_functions.function_body -> bool
end = struct

  (* Is the return value annotated with "local_"?
     [assert false] can work either way *)

  type local_returning_flag =
    | Local of Location.t  (* location of a local return *)
    | Not of Location.t  (* location of a non-local return *)
    | Either

  let combine flag1 flag2 =
    match flag1, flag2 with
    | (Local _ as flag), Local _
    | (Local _ as flag), Either
    | (Not _ as flag), Not _
    | (Not _ as flag), Either
    | Either, (Local _ as flag)
    | Either, (Not _ as flag)
    | (Either as flag), Either ->
      flag

    | Local local_loc, Not not_local_loc
    | Not not_local_loc, Local local_loc ->
       raise(Error(not_local_loc, Env.empty,
                   Local_return_annotation_mismatch local_loc))

  let expr e =
    let rec loop e =
      match Jane_syntax.Expression.of_ast e with
      | Some (jexp, _attrs) -> begin
          match jexp with
          | Jexp_comprehension   _ -> Not e.pexp_loc
          | Jexp_immutable_array _ -> Not e.pexp_loc
          | Jexp_layout (Lexp_constant _) -> Not e.pexp_loc
          | Jexp_layout (Lexp_newtype (_, _, e)) -> loop e
          | Jexp_n_ary_function _ -> Not e.pexp_loc
          | Jexp_tuple _ -> Not e.pexp_loc
          | Jexp_modes (Coerce (modes, exp)) ->
              if List.exists
                  (fun m ->
                     let {txt; _} =
                       (m : Jane_syntax.Mode_expr.Const.t :> _ Location.loc)
                     in
                     txt = "local")
                  modes.txt
              then Local e.pexp_loc
              else loop exp
        end
      | None      ->
      match e.pexp_desc with
      | Pexp_assert { pexp_desc = Pexp_construct ({ txt = Lident "false" },
                                                  None) } ->
          Either
      | Pexp_ident _ | Pexp_constant _ | Pexp_apply _ | Pexp_tuple _
      | Pexp_construct _ | Pexp_variant _ | Pexp_record _ | Pexp_field _
      | Pexp_setfield _ | Pexp_array _ | Pexp_while _ | Pexp_for _ | Pexp_send _
      | Pexp_new _ | Pexp_setinstvar _ | Pexp_override _ | Pexp_assert _
      | Pexp_lazy _ | Pexp_object _ | Pexp_pack _ | Pexp_function _ | Pexp_fun _
      | Pexp_letop _ | Pexp_extension _ | Pexp_unreachable ->
          Not e.pexp_loc
      | Pexp_let(_, _, e) | Pexp_sequence(_, e) | Pexp_constraint(e, _)
      | Pexp_coerce(e, _, _) | Pexp_letmodule(_, _, e) | Pexp_letexception(_, e)
      | Pexp_poly(e, _) | Pexp_newtype(_, e) | Pexp_open(_, e)
      | Pexp_ifthenelse(_, e, None)->
          loop e
      | Pexp_ifthenelse(_, e1, Some e2)-> combine (loop e1) (loop e2)
      | Pexp_match(_, cases) -> begin
          match cases with
          | [] -> Not e.pexp_loc
          | first :: rest ->
              List.fold_left
                (fun acc pc -> combine acc (loop pc.pc_rhs))
                (loop first.pc_rhs) rest
        end
      | Pexp_try(e, cases) ->
          List.fold_left
            (fun acc pc -> combine acc (loop pc.pc_rhs))
            (loop e) cases
    in
    loop e

  let cases cs =
    match cs with
    | [] -> Either
    | case :: cases ->
        let is_local_returning_case case =
          expr case.pc_rhs
        in
        List.fold_left
          (fun acc case -> combine acc (is_local_returning_case case))
          (is_local_returning_case case) cases

  let function_body (body : Jane_syntax.N_ary_functions.function_body) =
    match body with
    | Pfunction_body body -> expr body
    | Pfunction_cases (cs, _, _) -> cases cs

  let is_strictly_local = function
    | Local _ -> true
    | Either | Not _ -> false
        (* [fun _ -> assert false] must not be local-returning for
          backward compatibility *)

  (* for exporting from this module *)

  let function_body body = is_strictly_local (function_body body)
end

(* The "rest of the function" extends from the start of the first parameter
   to the end of the overall function. The parser does not construct such
   a location so we forge one for type errors.
*)
let loc_rest_of_function
    ~(loc_function : Location.t) params_suffix body : Location.t
  =
  let open Jane_syntax.N_ary_functions in
  match params_suffix, body with
  | { pparam_loc } :: _, _ ->
      let loc_start = pparam_loc.loc_start in
      { loc_start; loc_end = loc_function.loc_end; loc_ghost = true }
  | [], Pfunction_body pexp -> pexp.pexp_loc
  | [], Pfunction_cases (_, loc_cases, _) -> loc_cases

(* Approximate the type of an expression, for better recursion *)

(* CR layouts v5: This any is fine because we don't allow you to make types
   that could be matched on and have anys in them.  But once we do, this
   should probably be sort variable.  See Test 22 in typing-layouts/basics.ml
   (which mentions approx_type) for why it can't be value.  *)
(* CR layouts v2: RAE thinks this any is fine in perpetuity. Before changing
   this, let's talk. *)
let approx_type_default () = newvar (Jkind.Primitive.top ~why:Dummy_jkind)

let rec approx_type env sty =
  match Jane_syntax.Core_type.of_ast sty with
  | Some (jty, attrs) -> approx_type_jst env attrs jty
  | None ->
  match sty.ptyp_desc with
  | Ptyp_arrow (p, ({ ptyp_desc = Ptyp_poly _ } as arg_sty), sty) ->
      let p = Typetexp.transl_label p (Some arg_sty) in
      (* CR layouts v5: value requirement here to be relaxed *)
      if is_optional p then newvar Predef.option_argument_jkind
      else begin
        let arg_mode = Typetexp.get_alloc_mode arg_sty in
        let arg_ty =
          (* Polymorphic types will only unify with types that match all of their
           polymorphic parts, so we need to fully translate the type here
           unlike in the monomorphic case *)
          Typetexp.transl_simple_type ~new_var_jkind:Any env ~closed:false arg_mode arg_sty
        in
        let ret = approx_type env sty in
        let marg = Alloc.of_const arg_mode in
        let mret = Alloc.newvar () in
        newty (Tarrow ((p,marg,mret), arg_ty.ctyp_type, ret, commu_ok))
      end
  | Ptyp_arrow (p, arg_sty, sty) ->
      let arg_mode = Typetexp.get_alloc_mode arg_sty in
      let p = Typetexp.transl_label p (Some arg_sty) in
      let arg =
        if is_optional p
        then type_option (newvar Predef.option_argument_jkind)
        else newvar (Jkind.Type.Primitive.any ~why:Inside_of_Tarrow |> Jkind.of_type_jkind)
      in
      let ret = approx_type env sty in
      let marg = Alloc.of_const arg_mode in
      let mret = Alloc.newvar () in
      newty (Tarrow ((p,marg,mret), newmono arg, ret, commu_ok))
  | Ptyp_tuple args ->
      newty (Ttuple (List.map (fun t -> None, approx_type env t) args))
  | Ptyp_constr (lid, ctl) ->
      let path, decl = Env.lookup_type ~use:false ~loc:lid.loc lid.txt env in
      if List.length ctl <> decl.type_arity
      then newvar (Jkind.Primitive.top ~why:Dummy_jkind)
      else begin
        let tyl = List.map (approx_type env) ctl in
        newconstr path tyl
      end
  | _ -> approx_type_default ()

and approx_type_jst env _attrs : Jane_syntax.Core_type.t -> _ = function
  | Jtyp_layout (Ltyp_var _) -> approx_type_default ()
  | Jtyp_layout (Ltyp_poly _) -> approx_type_default ()
  | Jtyp_layout (Ltyp_alias _) -> approx_type_default ()
  | Jtyp_tuple args ->
      newty
        (Ttuple (List.map (fun (label, t) -> label, approx_type env t) args))

let type_pattern_approx_jane_syntax : Jane_syntax.Pattern.t -> _ = function
  | Jpat_immutable_array _
  | Jpat_layout (Lpat_constant _) -> ()
  | Jpat_tuple _
    -> ()

let type_pattern_approx env spat ty_expected =
  match Jane_syntax.Pattern.of_ast spat with
  | Some (jpat, _attrs) -> type_pattern_approx_jane_syntax jpat
  | None      ->
  match spat.ppat_desc with
  | Ppat_constraint(_, ({ptyp_desc=Ptyp_poly _} as sty)) ->
      let arg_type_mode, sty = alloc_mode_from_ppat_constraint_typ_attrs sty in
      let ty_pat =
        Typetexp.transl_simple_type ~new_var_jkind:Any env ~closed:false arg_type_mode sty
      in
      begin try unify env ty_pat.ctyp_type ty_expected with Unify trace ->
        raise(Error(spat.ppat_loc, env, Pattern_type_clash(trace, None)))
      end;
  | _ -> ()

let type_approx_constraint ~loc env constraint_ ty_expected =
  let open Jane_syntax.N_ary_functions in
  match constraint_ with
  | Pconstraint sty ->
      let ty_expected' = approx_type env sty in
      begin try unify env ty_expected' ty_expected with Unify err ->
        raise (Error (loc, env, Expr_type_clash (err, None, None)))
      end;
      ty_expected'
  | Pcoerce (_sty1, sty2) ->
      let ty = approx_type env sty2 in
      begin try unify env ty ty_expected with Unify trace ->
        raise (Error (loc, env, Expr_type_clash (trace, None, None)))
      end;
      ty_expected

let type_approx_constraint_opt ~loc env constraint_ ty_expected =
  let open Jane_syntax.N_ary_functions in
  match constraint_ with
  | None -> ty_expected
  | Some { type_constraint; mode_annotations = _ } ->
      type_approx_constraint ~loc env type_constraint ty_expected

let type_approx_fun_one_param
    env loc label spato ty_expected ~first ~in_function
  =
  let mode_annots, has_poly =
    match spato with
    | None -> None, false
    | Some spat ->
        let mode_annots, spat = mode_annots_from_pat_attrs spat in
        let has_poly = has_poly_constraint spat in
        if has_poly && is_optional label then
          raise(Error(spat.ppat_loc, env, Optional_poly_param));
        Some mode_annots, has_poly
  in
  let loc_fun, ty_fun = in_function in
  let { ty_arg; arg_mode; ty_ret; _ } =
    try filter_arrow env ty_expected label ~force_tpoly:(not has_poly)
    with Filter_arrow_failed err ->
      let err =
        error_of_filter_arrow_failure ~explanation:None ty_fun err ~first
      in
      raise (Error(loc_fun, env, err))
  in
  Option.iter
    (fun mode_annots ->
      apply_mode_annots ~loc ~env mode_annots arg_mode)
    mode_annots;
  if has_poly then begin
    match spato with
    | None -> ()
    | Some spat -> type_pattern_approx env spat ty_arg
  end;
  ty_ret

let rec type_approx env sexp ty_expected =
  let loc = sexp.pexp_loc in
  match Jane_syntax.Expression.of_ast sexp with
  | Some (jexp, _attrs) -> type_approx_aux_jane_syntax ~loc env jexp ty_expected
  | None      -> match sexp.pexp_desc with
    Pexp_let (_, _, e) -> type_approx env e ty_expected
  | Pexp_fun _ | Pexp_function _ ->
      Misc.fatal_error
        "Unexpected [Pexp_fun]/[Pexp_function] outside of Jane Syntax construct"
  | Pexp_match (_, {pc_rhs=e}::_) -> type_approx env e ty_expected
  | Pexp_try (e, _) -> type_approx env e ty_expected
  | Pexp_tuple l ->
    type_tuple_approx env sexp.pexp_loc ty_expected
      (List.map (fun e -> None, e) l)
  | Pexp_ifthenelse (_,e,_) -> type_approx env e ty_expected
  | Pexp_sequence (_,e) -> type_approx env e ty_expected
  | Pexp_constraint (e, sty) ->
      let ty_expected =
        type_approx_constraint env (Pconstraint sty) ty_expected ~loc
      in
      type_approx env e ty_expected
  | Pexp_coerce (_, sty1, sty2) ->
      ignore
        (type_approx_constraint env (Pcoerce (sty1, sty2)) ty_expected ~loc
           : type_expr)
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({txt = "extension.escape"}, PStr []) },
       [Nolabel, e]) ->
    type_approx env e ty_expected
  | _ -> ()

and type_approx_aux_jane_syntax
    ~loc
    env
    (jexp : Jane_syntax.Expression.t)
    ty_expected
  =
  match jexp with
  | Jexp_comprehension _
  | Jexp_immutable_array _
  | Jexp_layout (Lexp_constant _)
  | Jexp_layout (Lexp_newtype _) -> ()
  | Jexp_n_ary_function (params, c, body) ->
      type_approx_function ~loc env params c body ty_expected
  | Jexp_tuple l ->
      type_tuple_approx env loc ty_expected l
  | Jexp_modes (Coerce (_, e)) -> type_approx env e ty_expected

and type_tuple_approx (env: Env.t) loc ty_expected l =
  let labeled_tys = List.map
    (fun (label, _) -> label, newvar (Jkind.Type.Primitive.value ~why:Tuple_element |> Jkind.of_type_jkind)) l
  in
  let ty = newty (Ttuple labeled_tys) in
  begin try unify env ty ty_expected with Unify err ->
    raise(Error(loc, env, Expr_type_clash (err, None, None)))
  end;
  List.iter2
    (fun (_, e) (_, ty) -> type_approx env e ty)
    l labeled_tys

and type_approx_function =
  let rec loop env params c body ty_expected ~in_function ~first =
    let open Jane_syntax.N_ary_functions in
    let loc_function, _ = in_function in
    let loc = loc_rest_of_function ~loc_function params body in
    (* We can approximate types up to the first newtype parameter, whereupon
      we give up.
    *)
    match params with
    | { pparam_desc = Pparam_newtype _ } :: _ -> ()
    | { pparam_desc = Pparam_val (label, _, pat) } :: params ->
        let label, pat = Typetexp.transl_label_from_pat label pat in
        let ty_res =
          type_approx_fun_one_param env loc label (Some pat) ty_expected
            ~first ~in_function
        in
        loop env params c body ty_res ~in_function ~first:false
    | [] ->
        (* In the [Pconstraint] case, we override the [ty_expected] that
           gets passed to the approximating of the rest of the type.
        *)
        let ty_expected =
          type_approx_constraint_opt env c ty_expected ~loc
        in
        match body with
        | Pfunction_body body ->
            type_approx env body ty_expected
        | Pfunction_cases ({pc_rhs = e} :: _, _, _) ->
            let ty_res =
              type_approx_fun_one_param env loc Nolabel None ty_expected
                ~in_function ~first
            in
            type_approx env e ty_res
        | Pfunction_cases ([], _, _) -> ()
  in
  fun ~loc env params c body ty_expected : unit ->
    loop env params c body ty_expected
      ~in_function:(loc, ty_expected) ~first:true

(* Check that all univars are safe in a type. Both exp.exp_type and
   ty_expected should already be generalized. *)
let check_univars env kind exp ty_expected vars =
  let error ty ty_expected errs =
    let ty_expected = instance ty_expected in
    let trace =
      (Ctype.expanded_diff env ~got:ty ~expected:ty_expected) :: errs
    in
    raise (Error(exp.exp_loc, env,
                 Less_general(kind, Errortrace.unification_error ~trace)))
  in
  let pty = instance ty_expected in
  let exp_ty, vars =
    with_local_level_iter ~post:generalize begin fun () ->
      match get_desc pty with
        Tpoly (body, tl) ->
          (* Enforce scoping for type_let:
             since body is not generic,  instance_poly only makes
             copies of nodes that have a Tunivar as descendant *)
          let univars, ty' = instance_poly true tl body in
          let vars, exp_ty = instance_parameterized_type vars exp.exp_type in
          List.iter2 (fun uvar var ->
            (* This checks that the term doesn't require more specific jkinds
               than allowed by the univars. *)
            (* CR layouts: expand_head here is needed for examples like:

               type 'a t = 'a
               let id (x : 'a t) = x
               let foo : 'a . 'a -> 'a = fun x -> id x

               Here, while checking foo, ['a] gets unified with ['a t].  This is
               fine because ['a t] is actually just ['a], but it does mean we need
               to expand the var to find the variable with the jkind we want to
               check.

               However, I should come back and think about his more carefully:
               1) [polyfy], which is called below, also does this expansion.
                  It would be nice to just move the jkind check there,
                  but there was some reason I didn't do this originally (something
                  about unifications statefully changing things between now and
                  then).  Revisit.
               2) [polyfy] actually calls [expand_head] twice!  why?!
            *)
            match get_desc (expand_head env var) with
            | Tvar { jkind = jkind2; } -> begin
                match check_type_jkind env uvar jkind2 with
                | Ok _ -> ()
                | Error err ->
                  error exp_ty ty_expected
                    [Errortrace.Bad_jkind (uvar,err)]
              end
            | _ ->
              (* It would be semantically correct for this case to error. But
                 these errors are caught below anyway, and erroring earlier
                 results in small differences in error messages vs upstream. *)
              ())
            univars vars;
          unify_exp_types exp.exp_loc env exp_ty ty';
          ((exp_ty, vars), exp_ty::vars)
      | _ -> assert false
    end
  in
  let ty, complete = polyfy env exp_ty vars in
  if not complete then error ty ty_expected []

let generalize_and_check_univars env kind exp ty_expected vars =
  generalize exp.exp_type;
  generalize ty_expected;
  List.iter generalize vars;
  check_univars env kind exp ty_expected vars

(* [check_statement] implements the [non-unit-statement] check.

   This check is called in contexts where the value of the expression is known
   to be discarded (eg. the lhs of a sequence). We check that [exp] has type
   unit, or has an explicit type annotation; otherwise we raise the
   [non-unit-statement] warning. *)

let check_statement exp =
  let ty = get_desc (expand_head exp.exp_env exp.exp_type) in
  match ty with
  | Tconstr (p, _, _)  when Path.same p Predef.path_unit -> ()
  (* CR layouts v5: when we have unboxed unit, add a case here for it *)
  | Tvar _ -> ()
  | _ ->
      let rec loop {exp_loc; exp_desc; exp_extra; _} =
        match exp_desc with
        | Texp_let (_, _, e)
        | Texp_sequence (_, _, e)
        | Texp_letexception (_, e)
        | Texp_letmodule (_, _, _, _, e) ->
            loop e
        | _ ->
            let loc =
              match List.find_opt (function
                  | (Texp_constraint _, _, _) -> true
                  | _ -> false) exp_extra
              with
              | Some (_, loc, _) -> loc
              | None -> exp_loc
            in
            Location.prerr_warning loc Warnings.Non_unit_statement
      in
      loop exp


(* [check_partial_application] implements the [ignored-partial-application]
   warning (and if [statement] is [true], also [non-unit-statement]).

   If [exp] has a function type, we check that it is not syntactically the
   result of a function application, as this is often a bug in certain contexts
   (eg the rhs of a let-binding or in the argument of [ignore]). For example,
   [ignore (List.map print_int)] written by mistake instead of [ignore (List.map
   print_int li)].

   The check can be disabled by explicitly annotating the expression with a type
   constraint, eg [(e : _ -> _)].

   If [statement] is [true] and the [ignored-partial-application] is {em not}
   triggered, then the [non-unit-statement] check is performed (see
   [check_statement]).

   If the type of [exp] is not known at the time this function is called, the
   check is retried again after typechecking. *)

let check_partial_application ~statement exp =
  let check_statement () = if statement then check_statement exp in
  let doit () =
    let ty = get_desc (expand_head exp.exp_env exp.exp_type) in
    match ty with
    | Tarrow _ ->
        let rec check {exp_desc; exp_loc; exp_extra; _} =
          if List.exists (function
              | (Texp_constraint _, _, _) -> true
              | _ -> false) exp_extra then check_statement ()
          else begin
            match exp_desc with
            | Texp_ident _ | Texp_constant _ | Texp_tuple _
            | Texp_construct _ | Texp_variant _ | Texp_record _
            | Texp_field _ | Texp_setfield _ | Texp_array _
            | Texp_list_comprehension _ | Texp_array_comprehension _
            | Texp_while _ | Texp_for _ | Texp_instvar _
            | Texp_setinstvar _ | Texp_override _ | Texp_assert _
            | Texp_lazy _ | Texp_object _ | Texp_pack _ | Texp_unreachable
            | Texp_extension_constructor _ | Texp_ifthenelse (_, _, None)
            | Texp_probe _ | Texp_probe_is_enabled _ | Texp_src_pos
            | Texp_function _ ->
                check_statement ()
            | Texp_match (_, _, cases, _) ->
                List.iter (fun {c_rhs; _} -> check c_rhs) cases
            | Texp_try (e, cases) ->
                check e; List.iter (fun {c_rhs; _} -> check c_rhs) cases
            | Texp_ifthenelse (_, e1, Some e2) ->
                check e1; check e2
            | Texp_let (_, _, e) | Texp_sequence (_, _, e) | Texp_open (_, e)
            | Texp_letexception (_, e) | Texp_letmodule (_, _, _, _, e)
            | Texp_exclave e ->
                check e
            | Texp_apply _ | Texp_send _ | Texp_new _ | Texp_letop _ ->
                Location.prerr_warning exp_loc
                  Warnings.Ignored_partial_application
          end
        in
        check exp
    | _ ->
        check_statement ()
  in
  let ty = get_desc (expand_head exp.exp_env exp.exp_type) in
  match ty with
  | Tvar _ ->
      (* The type of [exp] is not known. Delay the check until after
         typechecking in order to give a chance for the type to become known
         through unification. *)
      add_delayed_check doit
  | _ ->
      doit ()

let pattern_needs_partial_application_check p =
  let rec check : type a. a general_pattern -> bool = fun p ->
    not (List.exists (function (Tpat_constraint _, _, _) -> true | _ -> false)
          p.pat_extra) &&
    match p.pat_desc with
    | Tpat_any -> true
    | Tpat_exception _ -> true
    | Tpat_or (p1, p2, _) -> check p1 && check p2
    | Tpat_value p -> check (p :> value general_pattern)
    | _ -> false
  in
  check p

(* Check that a type is generalizable at some level *)
let generalizable level ty =
  let rec check ty =
    if not_marked_node ty then
      if get_level ty <= level then raise Exit else
      (flip_mark_node ty; iter_type_expr check ty)
  in
  try check ty; unmark_type ty; true
  with Exit -> unmark_type ty; false

(* Hack to allow coercion of self. Will clean-up later. *)
let self_coercion = ref ([] : (Path.t * Location.t list ref) list)

(* Helpers for type_cases *)

let contains_variant_either ty =
  let rec loop ty =
    if try_mark_node ty then
      begin match get_desc ty with
        Tvariant row ->
          if not (is_fixed row) then
            List.iter
              (fun (_,f) ->
                match row_field_repr f with Reither _ -> raise Exit | _ -> ())
              (row_fields row);
          iter_row loop row
      | _ ->
          iter_type_expr loop ty
      end
  in
  try loop ty; unmark_type ty; false
  with Exit -> unmark_type ty; true

let shallow_iter_ppat_jane_syntax f : Jane_syntax.Pattern.t -> _ = function
  | Jpat_immutable_array (Iapat_immutable_array pats) -> List.iter f pats
  | Jpat_layout (Lpat_constant _) -> ()
  | Jpat_tuple (lst, _) ->  List.iter (fun (_,p) -> f p) lst

let shallow_iter_ppat f p =
  match Jane_syntax.Pattern.of_ast p with
  | Some (jpat, _attrs) -> shallow_iter_ppat_jane_syntax f jpat
  | None      ->
  match p.ppat_desc with
  | Ppat_any | Ppat_var _ | Ppat_constant _ | Ppat_interval _
  | Ppat_construct (_, None)
  | Ppat_extension _
  | Ppat_type _ | Ppat_unpack _ -> ()
  | Ppat_array pats -> List.iter f pats
  | Ppat_or (p1,p2) -> f p1; f p2
  | Ppat_variant (_, arg) -> Option.iter f arg
  | Ppat_tuple lst -> List.iter f lst
  | Ppat_construct (_, Some (_, p))
  | Ppat_exception p | Ppat_alias (p,_)
  | Ppat_open (_,p)
  | Ppat_constraint (p,_) | Ppat_lazy p -> f p
  | Ppat_record (args, _flag) -> List.iter (fun (_,p) -> f p) args

let exists_ppat f p =
  let exception Found in
  let rec loop p =
    if f p then raise Found else ();
    shallow_iter_ppat loop p in
  match loop p with
  | exception Found -> true
  | () -> false

let contains_polymorphic_variant p =
  exists_ppat
    (function
     | {ppat_desc = (Ppat_variant _ | Ppat_type _)} -> true
     | _ -> false)
    p

let contains_gadt p =
  exists_general_pattern { f = fun (type k) (p : k general_pattern) ->
     match p.pat_desc with
     | Tpat_construct (_, cd, _, _) when cd.cstr_generalized -> true
     | _ -> false } p

(* There are various things that we need to do in presence of GADT constructors
   that aren't required if there are none.
   However, because of disambiguation, we can't know for sure whether the
   patterns contain some GADT constructors. So we conservatively assume that
   any constructor might be a GADT constructor. *)
let may_contain_gadts p =
  exists_ppat
  (function
   | {ppat_desc = Ppat_construct _} -> true
   | _ -> false)
  p

(* One of the things we do in the presence of GADT constructors (see above
   definition) is treat `let p = e in ...` as a match `match e with p -> ...`.
   This changes the way type inference works to check the expression first, and
   using its type in the checking of the pattern.  We want that behavior for
   labeled tuple patterns as well.  *)
let turn_let_into_match p =
  exists_ppat (fun p ->
    match Jane_syntax.Pattern.of_ast p with
    | Some (Jpat_tuple (_, _), _) -> true
    | Some ((Jpat_layout _ | Jpat_immutable_array _), _) -> false
    | None -> match p.ppat_desc with
    | Ppat_construct _ -> true
    | _ -> false) p

(* There are various things that we need to do in presence of module patterns
   that aren't required if there are none. Most notably, we need to ensure the
   modules are entered at the appropriate scope. The caller should use
   [may_contain_modules] as an indication to set up the proper scope handling
   code (via [allow_modules]) to permit module patterns.
   The class of patterns identified here should stay in sync with the patterns
   whose typing involves [enter_variable ~is_module:true], as these calls
   will error if the scope handling isn't set up.
*)
let may_contain_modules p =
  exists_ppat
  (function
   | {ppat_desc = Ppat_unpack _} -> true
   | _ -> false)
  p

let check_absent_variant env =
  iter_general_pattern { f = fun (type k) (pat : k general_pattern) ->
    match pat.pat_desc with
    | Tpat_variant (s, arg, row) ->
      let row = !row in
      if List.exists (fun (s',fi) -> s = s' && row_field_repr fi <> Rabsent)
          (row_fields row)
      || not (is_fixed row) && not (static_row row)  (* same as Ctype.poly *)
      then () else
      let ty_arg =
        match arg with None -> [] | Some p -> [correct_levels p.pat_type] in
      let fields = [s, rf_either ty_arg ~no_arg:(arg=None) ~matched:true] in
      let row' =
        create_row ~fields
          ~more:(newvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))
          ~closed:false ~fixed:None ~name:None
      in
      (* Should fail *)
      unify_pat (ref env) {pat with pat_type = newty (Tvariant row')}
                          (correct_levels pat.pat_type)
    | _ -> () }

(* Getting proper location of already typed expressions.

   Used to avoid confusing locations on type error messages in presence of
   type constraints.
   For example:

       (* Before patch *)
       # let x : string = (5 : int);;
                           ^
       (* After patch *)
       # let x : string = (5 : int);;
                          ^^^^^^^^^
*)
let proper_exp_loc exp =
  let rec aux = function
    | [] -> exp.exp_loc
    | ((Texp_constraint _ | Texp_coerce _), loc, _) :: _ -> loc
    | _ :: rest -> aux rest
  in
  aux exp.exp_extra

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create_local default
  | p :: rem ->
    match p.pat_desc with
      Tpat_var (id, _, _, _) -> id
    | Tpat_alias(_, id, _, _, _) -> id
    | _ -> name_pattern default rem

let name_cases default lst =
  name_pattern default (List.map (fun c -> c.c_lhs) lst)

(* Typing of expressions *)

(** [sdesc_for_hint] is used by error messages to report literals in their
    original formatting *)
let unify_exp ?sdesc_for_hint env exp expected_ty =
  let loc = proper_exp_loc exp in
  try
    unify_exp_types loc env exp.exp_type expected_ty
  with Error(loc, env, Expr_type_clash(err, tfc, None)) ->
    raise (Error(loc, env, Expr_type_clash(err, tfc, sdesc_for_hint)))

let is_exclave_extension_node = function
  | "extension.exclave" | "ocaml.exclave" | "exclave" -> true
  | _ -> false

(* If [is_inferred e] is true, [e] will be typechecked without using
   the "expected type" provided by the context. *)

let rec is_inferred sexp =
  match Jane_syntax.Expression.of_ast sexp with
  | Some (jexp, _attrs) -> is_inferred_jane_syntax jexp
  | None      -> match sexp.pexp_desc with
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({ txt }, PStr []) },
        [Nolabel, sbody]) when is_exclave_extension_node txt ->
      is_inferred sbody
  | Pexp_ident _ | Pexp_apply _ | Pexp_field _ | Pexp_constraint _
  | Pexp_coerce _ | Pexp_send _ | Pexp_new _ -> true
  | Pexp_sequence (_, e) | Pexp_open (_, e) -> is_inferred e
  | Pexp_ifthenelse (_, e1, Some e2) -> is_inferred e1 && is_inferred e2
  | _ -> false

and is_inferred_jane_syntax : Jane_syntax.Expression.t -> _ = function
  | Jexp_comprehension _
  | Jexp_immutable_array _
  | Jexp_layout (Lexp_constant _ | Lexp_newtype _) -> false
  | Jexp_n_ary_function _ -> false
  | Jexp_tuple _ -> false
  | Jexp_modes (Coerce (_, exp)) -> is_inferred exp

(* check if the type of %apply or %revapply matches the type expected by
   the specialized typing rule for those primitives.
*)
type apply_prim =
  | Apply
  | Revapply
let check_apply_prim_type prim typ =
  match get_desc typ with
  | Tarrow ((Nolabel,_,_),a,b,_) when tpoly_is_mono a ->
      let a = tpoly_get_mono a in
      begin match get_desc b with
      | Tarrow((Nolabel,_,_),c,d,_) when tpoly_is_mono c ->
          let c = tpoly_get_mono c in
          let f, x, res =
            match prim with
            | Apply -> a, c, d
            | Revapply -> c, a, d
          in
          begin match get_desc f with
          | Tarrow((Nolabel,_,_),fl,fr,_) ->
              let fl = tpoly_get_mono fl in
              is_Tvar fl && is_Tvar fr && is_Tvar x && is_Tvar res
              && Types.eq_type fl x && Types.eq_type fr res
          | _ -> false
          end
      | _ -> false
      end
  | _ -> false

(* Merge explanation to type clash error *)

let with_explanation explanation f =
  match explanation with
  | None -> f ()
  | Some explanation ->
      try f ()
      with Error (loc', env', Expr_type_clash(err', None, exp'))
        when not loc'.Location.loc_ghost ->
        let err = Expr_type_clash(err', Some explanation, exp') in
        raise (Error (loc', env', err))

let unique_use ~loc ~env mode_l mode_r  =
  let uniqueness = Uniqueness.disallow_left (Value.proj (Monadic Uniqueness) mode_r) in
  let linearity = Linearity.disallow_right (Value.proj (Comonadic Linearity) mode_l) in
  if not (Language_extension.is_enabled Unique) then begin
    (* if unique extension is not enabled, we will not run uniqueness analysis;
       instead, we force all uses to be shared and many. This is equivalent to
       running a UA which forces everything *)
    (match Uniqueness.submode Uniqueness.shared uniqueness with
    | Ok () -> ()
    | Error e ->
        let e : Mode.Value.error = Error (Monadic Uniqueness, e) in
        raise (Error(loc, env, Submode_failed(e, Other, None, None, None)))
    );
    (match Linearity.submode linearity Linearity.many with
    | Ok () -> ()
    | Error e ->
        let e : Mode.Value.error = Error (Comonadic Linearity, e) in
        raise (Error (loc, env, Submode_failed(e, Other, None, None, None)))
    );
    (Uniqueness.disallow_left Uniqueness.shared,
     Linearity.disallow_right Linearity.many)
  end
  else (uniqueness, linearity)

(** The body of a constraint or coercion. The "body" may be either an expression
    or a list of function cases. This type is polymorphic in the data returned
    out of typing so that typing an expression body can return an expression
    and typing a function cases body can return the cases.
*)
type 'ret constraint_arg =
  { type_without_constraint: Env.t -> expected_mode -> 'ret * type_expr;
    (** [type_without_constraint] types a body (e :> t) where there is no
        constraint.
    *)
    type_with_constraint: Env.t -> expected_mode -> type_expr -> 'ret;
    (** [type_with_constraint] types a body (e : t) or (e : t :> t') in
        the presence of a constraint.
    *)
    is_self: 'ret -> bool;
  }

(* The result of splitting a function type into its argument/return types along
   with some extra information relevant to typechecking. The "extra information"
   is documented on the fields of [t] below.

   As a running example, we'll suppose the type of a function
   [f = fun x_1 ... x_n -> e] is [a_1 -> a_2 -> ... -> a_n -> b], and we're
   currently typechecking [a_i -> a_{i+1} -> ... -> b] for [i <= n].
 *)
type split_function_ty =
  { (* The result of calling [Ctype.filter_arrow] on
       [a_i -> a_{i+1} -> ... -> b].
    *)
    filtered_arrow: filtered_arrow;
    arg_sort : Jkind.Type.sort;
    ret_sort : Jkind.Type.sort;
    (* An instance of [a_i], unless [x_i] is annotated as polymorphic,
       in which case it's just [a_i] (not an instance).
    *)
    ty_arg_mono: type_expr;
    (* [expected_pat_mode] and [expected_inner_mode] are the arguments you
       should pass to [type_cases]. (As opposed to, say, using
       [filtered_arrow.arg_mode] and [filtered_arrow.ret_mode].) They are
       related to the [filtered_arrow] modes, but also consult whether mode
       crossing is available or if the function has a region.
      *)
    expected_pat_mode: expected_pat_mode;
    expected_inner_mode: expected_mode;
    (* [alloc_mode] is the mode of [fun x_i ... x_n -> e].
       This needs to be a left mode for the construction of the [fp_curry] field
       of the outer function. *)
    alloc_mode: Mode.Alloc.lr;
  }

(** Return the updated environment (e.g. it may have a closure lock)
    as well as the split function type. This function is called for
    each value parameter of a function.

    The mode, type, and location arguments are for the "rest of the
    function"; i.e., the rest of the parameter list (starting at the current
    parameter) followed by the body.

    @param arg_label label for the relevant parameter
    @param has_poly whether the parameter has a polymorphic type annotation
    @param mode_annots mode annotations placed on the function parameter
    @param in_function Information about the [Pexp_function] node that's in
      the process of being typechecked (its overall type and its location).
*)
let split_function_ty
    env (expected_mode : expected_mode) ty_expected loc ~arg_label ~has_poly
    ~mode_annots ~in_function ~is_first_val_param ~is_final_val_param
  =
  let alloc_mode =
      (* Unlike most allocations which can be the highest mode allowed by
         [expected_mode] and their [alloc_mode] identical to [expected_mode] ,
         functions have more constraints. For example, an outer function needs
         to be made global if its inner function is global. As a result, a
         function deserves a separate allocation mode.
      *)
      let mode, _ = Value.newvar_below expected_mode.mode in
      fst (register_allocation_value_mode mode)
  in
  if expected_mode.strictly_local then
    Locality.submode_exn Locality.local (Alloc.proj (Comonadic Areality) alloc_mode);
  let { ty_fun = { ty = ty_fun; explanation }; loc_fun; region_locked } =
    in_function
  in
  let separate = !Clflags.principal || Env.has_local_constraints env in
  let { ty_arg; ty_ret; arg_mode; ret_mode } as filtered_arrow =
    with_local_level_if separate begin fun () ->
      let force_tpoly =
        (* If [has_poly] is true then we rely on the later call to
           type_pat to enforce the invariant that the parameter type
           be a [Tpoly] node *)
        not has_poly
      in
      try filter_arrow env (instance ty_expected) arg_label ~force_tpoly
      with Filter_arrow_failed err ->
        let err =
          error_of_filter_arrow_failure ~explanation ~first:is_first_val_param
            ty_fun err
        in
        raise (Error(loc_fun, env, err))
    end
      ~post:(fun {ty_arg; ty_ret; _} ->
          generalize_structure ty_arg;
          generalize_structure ty_ret)
  in
  apply_mode_annots ~loc:loc_fun ~env mode_annots arg_mode;
  if not has_poly && not (tpoly_is_mono ty_arg) && !Clflags.principal
      && get_level ty_arg < Btype.generic_level then begin
    let snap = Btype.snapshot () in
    let really_poly =
      try
        unify env (newmono (newvar (Jkind.Primitive.top ~why:Dummy_jkind))) ty_arg;
        false
      with Unify _ -> true
    in
    Btype.backtrack snap;
    if really_poly then
      Location.prerr_warning loc
        (Warnings.Not_principal "this higher-rank function");
  end;
  let env =
    match is_first_val_param with
    | false -> env
    | true ->
        let env =
          Env.add_closure_lock
            ?closure_context:expected_mode.closure_context
            (alloc_as_value alloc_mode).comonadic
            env
        in
        if region_locked then Env.add_region_lock env
        else env
  in
  let ret_value_mode = alloc_as_value ret_mode in
  let expected_inner_mode =
    if not is_final_val_param then
      (* no need to check mode crossing in this case because ty_res always a
      function *)
      mode_default ret_value_mode
    else
      let ret_value_mode =
        if region_locked then mode_return ret_value_mode
        else begin
          (* if the function has no region, we force the ret_mode to be local *)
          match
            Locality.submode Locality.local (Alloc.proj (Comonadic Areality) ret_mode)
          with
          | Ok () -> mode_default ret_value_mode
          | Error _ -> raise (Error (loc_fun, env, Function_returns_local))
        end
      in
      let ret_value_mode = expect_mode_cross env ty_ret ret_value_mode in
      ret_value_mode
  in
  let ty_arg_mono =
    if has_poly then ty_arg
    else begin
      let ty, vars = tpoly_get_poly ty_arg in
      if vars = [] then ty
      else begin
        with_level ~level:generic_level
          (fun () -> snd (instance_poly ~keep_names:true false vars ty))
      end
    end
  in
  let arg_value_mode =
    if region_locked then alloc_to_value_l2r arg_mode
    else Value.disallow_right (alloc_as_value arg_mode)
  in
  let expected_pat_mode = simple_pat_mode arg_value_mode in
  let type_sort ~why ty =
    match Ctype.type_sort ~why env ty with
    | Ok sort -> sort
    | Error err -> raise (Error (loc_fun, env, Function_type_not_rep (ty, err)))
  in
  let arg_sort = type_sort ~why:Function_argument ty_arg in
  let ret_sort = type_sort ~why:Function_result ty_ret in
  env,
  { filtered_arrow; arg_sort; ret_sort;
    alloc_mode; ty_arg_mono;
    expected_inner_mode; expected_pat_mode
  }

type type_function_result_param =
  { param : function_param;
    has_poly : bool;
  }

(* The result of calling [type_function]. For the outer call to
   [type_function], it's the result of typechecking the entire function;
   for recursive calls to [type_function], it's the result of typechecking
   the "rest of the function": the parameter suffix followed by the body.
*)
type type_function_result =
  { function_ :
      type_expr * type_function_result_param list * function_body;
    (* The uninterrupted prefix of newtypes of the parameter suffix. *)
    newtypes: (string loc * Jkind.annotation option) list;
    (* Whether any of the value parameters contains a GADT pattern. *)
    params_contain_gadt: contains_gadt;
    (* The alloc mode of the "rest of the function". None only for recursive
       calls to [type_function] when there are no parameters left. This needs to
       be a left mode for the construction of the [fp_curry] field of the outer
       function.
    *)
    fun_alloc_mode: Mode.Alloc.lr option;
    (* Information about the return of the function. None only for
       recursive calls to [type_function] when there are no parameters
       left.
    *)
    ret_info: type_function_ret_info option;
  }

and type_function_ret_info =
  { (* The mode the function returns at. *)
    ret_mode: Mode.Alloc.l;
    (* The sort returned by the function. *)
    ret_sort: Jkind.Type.sort;
  }

(* Generalize expressions *)
let generalize_structure_exp exp = generalize_structure exp.exp_type
let may_lower_contravariant_then_generalize env exp =
  if maybe_expansive exp then lower_contravariant env exp.exp_type;
  generalize exp.exp_type

(* This added mode attribute is read and removed by
    [alloc_mode_from_pexp_constraint_typ_attrs] or
    [alloc_mode_from_ppat_constraint_typ_attrs]. *)
let add_mode_annot_attrs mode_annot_attr typ =
  match mode_annot_attr with
  | None -> typ
  | Some attr -> { typ with ptyp_attributes = attr :: typ.ptyp_attributes }

(* value binding elaboration *)

let vb_exp_constraint {pvb_expr=expr; pvb_pat=pat; pvb_constraint=ct; pvb_attributes=attrs; _ } =
  let open Ast_helper in
  let mode_annot_attr, _ = Jane_syntax.Mode_expr.extract_attr attrs in
  match ct with
  | None -> expr
  | Some (Pvc_constraint { locally_abstract_univars=[]; typ }) ->
      begin match typ.ptyp_desc with
      | Ptyp_poly _ -> expr
      | _ ->
          let loc = { expr.pexp_loc with Location.loc_ghost = true } in
          Exp.constraint_ ~loc expr (add_mode_annot_attrs mode_annot_attr typ)
      end
  | Some (Pvc_coercion { ground; coercion}) ->
      let loc = { expr.pexp_loc with Location.loc_ghost = true } in
      Exp.coerce ~loc expr ground coercion
  | Some (Pvc_constraint { locally_abstract_univars=vars;typ}) ->
      let loc_start = pat.ppat_loc.Location.loc_start in
      let loc = { expr.pexp_loc with loc_start; loc_ghost=true } in
      let expr = Exp.constraint_ ~loc expr (add_mode_annot_attrs mode_annot_attr typ) in
      List.fold_right (Exp.newtype ~loc) vars expr

let vb_pat_constraint
      ({pvb_pat=pat; pvb_expr = exp; pvb_attributes = attrs; _ } as vb) =
  let mode_annot_attr, _ = Jane_syntax.Mode_expr.extract_attr attrs in
  let spat =
    let open Ast_helper in
    match vb.pvb_constraint, pat.ppat_desc, exp.pexp_desc with
    | Some (Pvc_constraint {locally_abstract_univars=[]; typ}
           | Pvc_coercion { coercion=typ; _ }),
      _, _ ->
        let typ = add_mode_annot_attrs mode_annot_attr typ in
        Pat.constraint_ ~loc:{pat.ppat_loc with Location.loc_ghost=true} pat typ
    | Some (Pvc_constraint {locally_abstract_univars=vars; typ }), _, _ ->
        let varified = Typ.varify_constructors vars typ in
        let t = Typ.poly ~loc:typ.ptyp_loc vars varified in
        let loc_end = typ.ptyp_loc.Location.loc_end in
        let loc =  { pat.ppat_loc with loc_end; loc_ghost=true } in
        let t = add_mode_annot_attrs mode_annot_attr t in
        Pat.constraint_ ~loc pat t
    | None, (Ppat_any | Ppat_constraint _), _ -> pat
    | None, _, Pexp_coerce (_, _, sty)
    | None, _, Pexp_constraint (_, sty) when !Clflags.principal ->
        (* propagate type annotation to pattern,
           to allow it to be generalized in -principal mode *)
        let sty = add_mode_annot_attrs mode_annot_attr sty in
        Pat.constraint_ ~loc:{pat.ppat_loc with Location.loc_ghost=true} pat sty
    | _ -> pat
  in
  vb.pvb_attributes, spat

let pat_modes ~force_toplevel rec_mode_var (attrs, spat) =
  let pat_mode, exp_mode =
    if force_toplevel
    then simple_pat_mode Value.legacy, mode_legacy
    else match rec_mode_var with
    | None -> begin
        match pat_tuple_arity spat with
        | Not_local_tuple | Maybe_local_tuple ->
            let mode = Value.newvar () in
            simple_pat_mode mode, mode_default mode
        | Local_tuple arity ->
            let modes = List.init arity (fun _ -> Value.newvar ()) in
            let mode =
              value_regional_to_local (fst (Value.newvar_above (Value.join modes)))
            in
            tuple_pat_mode mode modes, mode_tuple mode modes
      end
    | Some mode ->
        simple_pat_mode mode, mode_default mode
  in
  attrs, pat_mode, exp_mode, spat

let add_check_attribute expr attributes =
  let open Builtin_attributes in
  let to_string : zero_alloc_attribute -> string = function
    | Check { strict; loc = _} ->
      Printf.sprintf "assert_zero_alloc%s"
        (if strict then " strict" else "")
    | Assume { strict; loc = _} ->
      Printf.sprintf "assume_zero_alloc%s"
        (if strict then " strict" else "")
    | Ignore_assert_all ->
      "ignore_zero_alloc"
    | Default_zero_alloc -> assert false
  in
  match expr.exp_desc with
  | Texp_function fn ->
    let default_arity = function_arity fn.params fn.body in
    let za =
      get_zero_alloc_attribute ~in_signature:false ~default_arity attributes
    in
    begin match za with
    | Default_zero_alloc -> expr
    | (Ignore_assert_all | Check _ | Assume _) as check ->
      begin match fn.zero_alloc with
      | Default_zero_alloc -> ()
      | Ignore_assert_all | Assume _ | Check _ ->
        Location.prerr_warning expr.exp_loc
          (Warnings.Duplicated_attribute (to_string fn.zero_alloc));
      end;
      let exp_desc = Texp_function { fn with zero_alloc = check } in
      { expr with exp_desc }
    end
  | _ -> expr

let zero_alloc_of_application ~num_args attrs funct =
  let zero_alloc =
    Builtin_attributes.get_zero_alloc_attribute ~in_signature:false
      ~default_arity:num_args attrs
  in
  let zero_alloc =
    match zero_alloc with
    | Assume _ | Ignore_assert_all | Check _ ->
      (* The user wrote a zero_alloc attribute on the application - keep it.
         (Note that `ignore` and `check` aren't really allowed here, and will be
         rejected by the call to `Builtin_attributes.assume_zero_alloc` below.)
       *)
      zero_alloc
    | Default_zero_alloc ->
      (* We assume the call is zero_alloc if the function is known to be
         zero_alloc. If the function is zero_alloc opt, then we need to be sure
         that the opt checks were run to license this assumption. We judge
         whether the opt checks were run based on the argument to the
         [-zero-alloc-check] command line flag. *)
      let use_opt =
        match !Clflags.zero_alloc_check with
        | Check_default | No_check -> false
        | Check_all | Check_opt_only -> true
      in
      match funct.exp_desc with
      | Texp_ident (_, _, { val_zero_alloc = (Check c); _ }, _, _)
        when c.arity = num_args && (use_opt || not c.opt) ->
        Builtin_attributes.Assume {
          strict = c.strict;
          never_returns_normally = false;
          never_raises = false;
          arity = c.arity;
          loc = c.loc
        }
      | _ -> Builtin_attributes.Default_zero_alloc
  in
  Builtin_attributes.assume_zero_alloc ~is_check_allowed:false zero_alloc

let rec type_exp ?recarg env expected_mode sexp =
  (* We now delegate everything to type_expect *)
  type_expect ?recarg env expected_mode sexp
    (mk_expected (newvar (Jkind.Primitive.top ~why:Dummy_jkind)))

(* Typing of an expression with an expected type.
   This provide better error messages, and allows controlled
   propagation of return type information.
   In the principal case, structural nodes of [type_expected_explained] may be
   at [generic_level] (but its variables no higher than [!current_level]).
 *)

and type_expect ?recarg env
      (expected_mode : expected_mode) sexp ty_expected_explained =
  let previous_saved_types = Cmt_format.get_saved_types () in
  let exp =
    Builtin_attributes.warning_scope sexp.pexp_attributes
      (fun () ->
         type_expect_ ?recarg env expected_mode sexp ty_expected_explained
      )
  in
  Cmt_format.set_saved_types
    (Cmt_format.Partial_expression exp :: previous_saved_types);
  exp

and type_expect_
    ?(recarg=Rejected)
    env (expected_mode : expected_mode) sexp ty_expected_explained =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let loc = sexp.pexp_loc in
  let desc = sexp.pexp_desc in
  (* Record the expression type before unifying it with the expected type *)
  let with_explanation = with_explanation explanation in
  (* Unify the result with [ty_expected], enforcing the current level *)
  let rue exp =
    with_explanation (fun () ->
      unify_exp ~sdesc_for_hint:desc env (re exp) (instance ty_expected));
    exp
  in
  match Jane_syntax.Expression.of_ast sexp with
  | Some (jexp, attributes) ->
      type_expect_jane_syntax
        ~loc
        ~env
        ~expected_mode
        ~ty_expected
        ~explanation
        ~rue
        ~attributes
        jexp
  | None -> match desc with
  | Pexp_ident lid ->
      let path, (actual_mode : Env.actual_mode), desc, kind =
        type_ident env ~recarg lid
      in
      let exp_desc =
        match desc.val_kind with
        | Val_ivar (_, cl_num) ->
            let (self_path, _) =
              Env.find_value_by_name
                (Longident.Lident ("self-" ^ cl_num)) env
            in
            Texp_instvar(self_path, path,
                         match lid.txt with
                             Longident.Lident txt -> { txt; loc = lid.loc }
                           | _ -> assert false)
        | Val_self (_, _, _, cl_num) ->
            let (path, _) =
              Env.find_value_by_name (Longident.Lident ("self-" ^ cl_num)) env
            in
            Texp_ident(path, lid, desc, kind,
              unique_use ~loc ~env actual_mode.mode expected_mode.mode)
        | _ ->
            Texp_ident(path, lid, desc, kind,
              unique_use ~loc ~env actual_mode.mode expected_mode.mode)
      in
      let exp = rue {
        exp_desc; exp_loc = loc; exp_extra = [];
        exp_type = desc.val_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
      in
      actual_submode ~loc ~env actual_mode expected_mode;
      exp
  | Pexp_constant(Pconst_string (str, _, _) as cst) -> (
      let cst = constant_or_raise env loc cst in
      (* Terrible hack for format strings *)
      let ty_exp = expand_head env (protect_expansion env ty_expected) in
      let fmt6_path =
        Path.(Pdot (Pident (Ident.create_persistent "CamlinternalFormatBasics"),
                    "format6"))
      in
      let is_format = match get_desc ty_exp with
        | Tconstr(path, _, _) when Path.same path fmt6_path ->
          if !Clflags.principal && get_level ty_exp <> generic_level then
            Location.prerr_warning loc
              (Warnings.Not_principal "this coercion to format6");
          true
        | _ -> false
      in
      if is_format then
        let format_parsetree =
          { (type_format loc str env) with pexp_loc = sexp.pexp_loc }  in
        type_expect env expected_mode
          format_parsetree ty_expected_explained
      else
        rue {
          exp_desc = Texp_constant cst;
          exp_loc = loc; exp_extra = [];
          exp_type = instance Predef.type_string;
          exp_attributes = sexp.pexp_attributes;
          exp_env = env }
  )
  | Pexp_constant cst ->
      let cst = constant_or_raise env loc cst in
      rue {
        exp_desc = Texp_constant cst;
        exp_loc = loc; exp_extra = [];
        exp_type = type_constant cst;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_let(Nonrecursive,
             [{pvb_pat=spat; pvb_attributes=[]; _ } as vb], sbody)
    when turn_let_into_match spat ->
      (* TODO: allow non-empty attributes? *)
      let sval = vb_exp_constraint vb in
      type_expect env expected_mode
        {sexp with
         pexp_desc = Pexp_match (sval, [Ast_helper.Exp.case spat sbody])}
        ty_expected_explained
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let existential_context =
        if rec_flag = Recursive then In_rec
        else if List.compare_length_with spat_sexp_list 1 > 0 then In_group
        else With_attributes in
      let may_contain_modules =
        List.exists (fun pvb -> may_contain_modules pvb.pvb_pat) spat_sexp_list
      in
      let outer_level = get_current_level () in
      let (pat_exp_list, body, _new_env) =
        (* If the patterns contain module unpacks, there is a possibility that
           the types of the let body or bound expressions mention types
           introduced by those unpacks. The below code checks for scope escape
           via both of these pathways (body, bound expressions).
        *)
        with_local_level_if may_contain_modules begin fun () ->
          let allow_modules =
            if may_contain_modules
            then
              let scope = create_scope () in
              Modules_allowed { scope }
            else Modules_rejected
          in
          let (pat_exp_list, new_env) =
            type_let existential_context env rec_flag spat_sexp_list
              allow_modules
          in
          let body =
            type_expect
              new_env expected_mode sbody ty_expected_explained
          in
          let pat_exp_list = match rec_flag with
            | Recursive -> annotate_recursive_bindings env pat_exp_list
            | Nonrecursive -> pat_exp_list
          in
          (* The "bound expressions" component of the scope escape check.

             This kind of scope escape is relevant only for recursive
             module definitions.
          *)
          if rec_flag = Recursive && may_contain_modules then begin
            List.iter
              (fun vb ->
                 (* [type_let] already generalized bound expressions' types
                    in-place. We first take an instance before checking scope
                    escape at the outer level to avoid losing generality of
                    types added to [new_env].
                 *)
                let bound_exp = vb.vb_expr in
                let bound_exp_type = Ctype.instance bound_exp.exp_type in
                let loc = proper_exp_loc bound_exp in
                let outer_var =
                  newvar2 outer_level (Jkind.Primitive.top ~why:Dummy_jkind)
                in
                (* Checking unification within an environment extended with the
                   module bindings allows us to correctly accept more programs.
                   This environment allows unification to identify more cases
                   where a type introduced by the module is equal to a type
                   introduced at an outer scope. *)
                unify_exp_types loc new_env bound_exp_type outer_var)
              pat_exp_list
          end;
          (pat_exp_list, body, new_env)
        end
        ~post:(fun (_pat_exp_list, body, new_env) ->
          (* The "body" component of the scope escape check. *)
          unify_exp new_env body (newvar (Jkind.Primitive.top ~why:Dummy_jkind)))
      in
      re {
        exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_fun _ ->
      Misc.fatal_error "non-Jane-Syntax [Pexp_fun] made it to typechecking"
  | Pexp_function _ ->
      Misc.fatal_error "non-Jane-Syntax [Pexp_function] made it to typechecking"
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({txt = "extension.escape"}, PStr []) },
       [Nolabel, sbody]) ->
      submode ~loc ~env ~reason:Other Value.legacy expected_mode;
      let exp =
        type_expect ~recarg env mode_legacy sbody
          ty_expected_explained
      in
      {exp with exp_loc = loc}
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({ txt }, PStr []) },
       [Nolabel, sbody]) when is_exclave_extension_node txt ->
      if (txt = "extension.exclave") && not (Language_extension.is_enabled Mode) then
          raise (Typetexp.Error (loc, Env.empty, Unsupported_extension Mode));
      begin
        match expected_mode.position with
        | RNontail ->
          raise (Error (loc, env, Exclave_in_nontail_position))
        | RTail (regionality, _) ->
          (* The middle-end relies on all functions which allocate into their
             parent's region having a return mode of local. *)
          (match Regionality.submode Regionality.local regionality with
          | Ok () -> ()
          | Error _ -> raise (Error(loc, env, Exclave_returns_not_local))
          );
          (* mode' is RNontail, because currently our language cannot construct
             region in the tail of another region.*)
          let mode' = mode_exclave expected_mode in
          let new_env = Env.add_exclave_lock env in
          let exp =
            type_expect ~recarg new_env mode' sbody ty_expected_explained
          in
          submode ~loc ~env ~reason:Other
            (Value.min_with (Comonadic Areality) Regionality.regional) expected_mode;
          { exp_desc = Texp_exclave exp;
            exp_loc = loc;
            exp_extra = [];
            exp_type = exp.exp_type;
            exp_env = env;
            exp_attributes = sexp.pexp_attributes;
          }
      end
  | Pexp_apply(sfunct, sargs) ->
      assert (sargs <> []);
      let pm = position_and_mode env expected_mode sexp in
      let funct_mode, funct_expected_mode =
        match pm.apply_position with
        | Tail ->
          let mode, _ =
            Value.newvar_below
              (Value.max_with (Comonadic Areality) Regionality.regional)
          in
          mode, mode_tailcall_function mode
        | Nontail | Default ->
          let mode = Value.newvar () in
          mode, mode_default mode
      in
      (* does the function return a tvar which is too generic? *)
      let rec ret_tvar seen ty_fun =
        let ty = expand_head env ty_fun in
        if TypeSet.mem ty seen then false else
          match get_desc ty with
            Tarrow (_l, ty_arg, ty_fun, _com) ->
              (try enforce_current_level env ty_arg
               with Unify _ -> assert false);
              ret_tvar (TypeSet.add ty seen) ty_fun
          | Tvar _ ->
              let v = newvar (Jkind.Primitive.top ~why:Dummy_jkind) in
              let rt = get_level ty > get_level v in
              unify_var env v ty;
              rt
          | _ ->
            let v = newvar (Jkind.Primitive.top ~why:Dummy_jkind) in
            unify_var env v ty;
            false
      in
      let type_sfunct sfunct =
        (* one more level for warning on non-returning functions *)
        let funct, ty =
          with_local_level
            begin fun () ->
              let funct =
                with_local_level_if_principal
                  (fun () -> type_exp env funct_expected_mode sfunct)
                  ~post: generalize_structure_exp
              in
              let ty = instance funct.exp_type in
              (funct, ty)
            end
        in
        let rt = wrap_trace_gadt_instances env (ret_tvar TypeSet.empty) ty in
        rt, funct
      in
      let type_sfunct_args sfunct extra_args =
        match Jane_syntax.Expression.of_ast sfunct, sfunct.pexp_desc with
        | None, Pexp_apply (sfunct, args) ->
           type_sfunct sfunct, args @ extra_args
        | _ ->
           type_sfunct sfunct, extra_args
      in
      let (rt, funct), sargs =
        let rt, funct = type_sfunct sfunct in
        match funct.exp_desc, sargs with
        | Texp_ident (_, _, {val_kind = Val_prim {prim_name = "%revapply"}; val_type},
                      Id_prim _, _),
          [Nolabel, sarg; Nolabel, actual_sfunct]
          when is_inferred actual_sfunct
            && check_apply_prim_type Revapply val_type ->
            type_sfunct_args actual_sfunct [Nolabel, sarg]
        | Texp_ident (_, _, {val_kind = Val_prim {prim_name = "%apply"}; val_type},
                      Id_prim _, _),
          [Nolabel, actual_sfunct; Nolabel, sarg]
          when check_apply_prim_type Apply val_type ->
            type_sfunct_args actual_sfunct [Nolabel, sarg]
        | _ ->
            (rt, funct), sargs
      in
      let (args, ty_res, ap_mode, pm) =
        type_application env loc expected_mode pm funct funct_mode sargs rt
      in
      let assume_zero_alloc =
        zero_alloc_of_application ~num_args:(List.length args)
          sfunct.pexp_attributes funct
      in

      rue {
        exp_desc = Texp_apply(funct, args, pm.apply_position, ap_mode,
                              assume_zero_alloc);
        exp_loc = loc; exp_extra = [];
        exp_type = ty_res;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_match(sarg, caselist) ->
      let arg_pat_mode, arg_expected_mode =
        match cases_tuple_arity caselist with
        | Not_local_tuple | Maybe_local_tuple ->
          let mode = Value.newvar () in
          simple_pat_mode mode, mode_default mode
        | Local_tuple arity ->
          let modes = List.init arity (fun _ -> Value.newvar ()) in
          let mode, _ = Value.newvar_above (Value.join modes) in
          let mode = value_regional_to_local mode in
          tuple_pat_mode mode modes, mode_tuple mode modes
      in
      let arg, sort =
        with_local_level begin fun () ->
          let expected_ty, sort = new_rep_var ~why:Match () in
          let arg =
            type_expect env arg_expected_mode sarg (mk_expected expected_ty)
          in
          arg, sort
        end ~post:(fun (arg, _) ->
          may_lower_contravariant_then_generalize env arg)
      in
      let cases, partial =
        type_cases Computation env arg_pat_mode expected_mode
          arg.exp_type ty_expected_explained
          ~check_if_total:true loc caselist in
      if
        List.for_all (fun c -> pattern_needs_partial_application_check c.c_lhs)
          cases
      then check_partial_application ~statement:false arg;
      re {
        exp_desc = Texp_match(arg, sort, cases, partial);
        exp_loc = loc; exp_extra = [];
        exp_type = instance ty_expected;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_try(sbody, caselist) ->
      let body =
        type_expect env (mode_trywith expected_mode)
          sbody ty_expected_explained
      in
      let arg_mode = simple_pat_mode Value.legacy in
      let cases, _ =
        type_cases Value env arg_mode expected_mode
          Predef.type_exn ty_expected_explained
          ~check_if_total:false loc caselist in
      re {
        exp_desc = Texp_try(body, cases);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_tuple sexpl ->
      type_tuple ~loc ~env ~expected_mode ~ty_expected ~explanation
        ~attributes:sexp.pexp_attributes (List.map (fun e -> None, e) sexpl)
  | Pexp_construct(lid, sarg) ->
      type_construct env expected_mode loc lid
        sarg ty_expected_explained sexp.pexp_attributes
  | Pexp_variant(l, sarg) ->
      (* Keep sharing *)
      let ty_expected1 = protect_expansion env ty_expected in
      let ty_expected0 = instance ty_expected in
      begin try match
        sarg, get_desc (expand_head env ty_expected1),
        get_desc (expand_head env ty_expected0)
      with
      | Some sarg, Tvariant row, Tvariant row0 ->
          begin match
            row_field_repr (get_row_field l row),
            row_field_repr (get_row_field l row0)
          with
            Rpresent (Some ty), Rpresent (Some ty0) ->
              let alloc_mode, argument_mode = register_allocation expected_mode in
              let arg = type_argument env argument_mode sarg ty ty0 in
              re { exp_desc = Texp_variant(l, Some (arg, alloc_mode));
                   exp_loc = loc; exp_extra = [];
                   exp_type = ty_expected0;
                   exp_attributes = sexp.pexp_attributes;
                   exp_env = env }
          | _ -> raise Exit
          end
      | _ -> raise Exit
      with Exit ->
        let arg = match sarg with
        | None -> None
        | Some sarg ->
            let ty_expected =
              newvar (Jkind.Type.Primitive.value ~why:Polymorphic_variant_field |> Jkind.of_type_jkind)
            in
            let alloc_mode, argument_mode = register_allocation expected_mode in
            let arg =
              type_expect env argument_mode sarg (mk_expected ty_expected)
            in
            Some (arg, alloc_mode)
        in
        let arg_type = Option.map (fun (arg, _) -> arg.exp_type) arg in
        let row =
          create_row
            ~fields: [l, rf_present arg_type]
            ~more:   (newvar (Jkind.Type.Primitive.value ~why:Row_variable |> Jkind.of_type_jkind))
            ~closed: false
            ~fixed:  None
            ~name:   None
        in
        rue {
          exp_desc = Texp_variant(l, arg);
          exp_loc = loc; exp_extra = [];
          exp_type = newty (Tvariant row);
          exp_attributes = sexp.pexp_attributes;
          exp_env = env }
      end
  | Pexp_record(lid_sexp_list, opt_sexp) ->
      assert (lid_sexp_list <> []);
      let opt_exp =
        match opt_sexp with
          None -> None
        | Some sexp ->
            let exp, mode =
              with_local_level_if_principal begin fun () ->
                let mode = Value.newvar () in
                let exp = type_exp ~recarg env (mode_default mode) sexp in
                exp, mode
              end ~post:(fun (exp, _) -> generalize_structure_exp exp)
            in
            Some (exp, mode)
      in
      let ty_record, expected_type =
        let expected_opath =
          match extract_concrete_record env ty_expected with
          | Record_type (p0, p, _, _) -> Some (p0, p, is_principal ty_expected)
          | Maybe_a_record_type -> None
          | Not_a_record_type ->
            let error =
              Wrong_expected_kind(Record, Expression explanation, ty_expected)
            in
            raise (Error (loc, env, error))
        in
        let opt_exp_opath =
          match opt_exp with
          | None -> None
          | Some (exp, _) ->
            match extract_concrete_record env exp.exp_type with
            | Record_type (p0, p, _, _) -> Some (p0, p, is_principal exp.exp_type)
            | Maybe_a_record_type -> None
            | Not_a_record_type ->
              let error = Expr_not_a_record_type exp.exp_type in
              raise (Error (exp.exp_loc, env, error))
        in
        match expected_opath, opt_exp_opath with
        | None, None ->
          newvar (Jkind.of_new_sort ~why:Record_projection), None
        | Some _, None -> ty_expected, expected_opath
        | Some(_, _, true), Some _ -> ty_expected, expected_opath
        | (None | Some (_, _, false)), Some (_, p', _) ->
            let decl = Env.find_type p' env in
            let ty =
              with_local_level ~post:generalize_structure
                (fun () -> newconstr p' (instance_list decl.type_params))
            in
            ty, opt_exp_opath
      in
      let closed = (opt_sexp = None) in
      let lbl_a_list =
        wrap_disambiguate "This record expression is expected to have"
          (mk_expected ty_record)
          (disambiguate_sort_lid_a_list loc closed env Env.Construct expected_type)
          lid_sexp_list
      in
      let alloc_mode, argument_mode =
        if List.exists
            (fun (_, {lbl_repres; _}, _) ->
              match lbl_repres with
              | Record_unboxed | Record_inlined (_, Variant_unboxed) -> false
              | _ -> true)
            lbl_a_list then
          let alloc_mode, argument_mode = register_allocation expected_mode in
          Some alloc_mode, argument_mode
        else
          None, expected_mode
      in
      let type_label_exp ((_, label, _) as x) =
        check_construct_mutability ~loc ~env label.lbl_mut argument_mode;
        let argument_mode = mode_modality label.lbl_modalities argument_mode in
        type_label_exp true env argument_mode loc ty_record x
      in
      let lbl_exp_list = List.map type_label_exp lbl_a_list in
      with_explanation (fun () ->
        unify_exp_types loc env (instance ty_record) (instance ty_expected));
      (* note: check_duplicates would better be implemented in
         disambiguate_sort_lid_a_list directly *)
      let rec check_duplicates = function
        | (_, lbl1, _) :: (_, lbl2, _) :: _ when lbl1.lbl_num = lbl2.lbl_num ->
          raise(Error(loc, env, Label_multiply_defined lbl1.lbl_name))
        | _ :: rem ->
            check_duplicates rem
        | [] -> ()
      in
      check_duplicates lbl_exp_list;
      let opt_exp, label_definitions =
        let (_lid, lbl, _lbl_exp) = List.hd lbl_exp_list in
        let matching_label lbl =
          List.find
            (fun (_, lbl',_) -> lbl'.lbl_num = lbl.lbl_num)
            lbl_exp_list
        in
        match opt_exp with
          None ->
            let label_definitions =
              Array.map (fun lbl ->
                  match matching_label lbl with
                  | (lid, _lbl, lbl_exp) ->
                      Overridden (lid, lbl_exp)
                  | exception Not_found ->
                      let present_indices =
                        List.map (fun (_, lbl, _) -> lbl.lbl_num) lbl_exp_list
                      in
                      let label_names = extract_label_names env ty_expected in
                      let rec missing_labels n = function
                          [] -> []
                        | lbl :: rem ->
                            if List.mem n present_indices
                            then missing_labels (n + 1) rem
                            else lbl :: missing_labels (n + 1) rem
                      in
                      let missing = missing_labels 0 label_names in
                      raise(Error(loc, env, Label_missing missing)))
                lbl.lbl_all
            in
            None, label_definitions
        | Some (exp, mode) ->
            let ty_exp = instance exp.exp_type in
            let unify_kept lbl =
              let _, ty_arg1, ty_res1 = instance_label false lbl in
              unify_exp_types exp.exp_loc env ty_exp ty_res1;
              match matching_label lbl with
              | lid, _lbl, lbl_exp ->
                  (* do not connect result types for overridden labels *)
                  Overridden (lid, lbl_exp)
              | exception Not_found -> begin
                  let _, ty_arg2, ty_res2 = instance_label false lbl in
                  unify_exp_types loc env ty_arg1 ty_arg2;
                  with_explanation (fun () ->
                    unify_exp_types loc env (instance ty_expected) ty_res2);
                  check_project_mutability ~loc:exp.exp_loc ~env lbl.lbl_mut mode;
                  let mode = Modality.Value.Const.apply lbl.lbl_modalities mode in
                  check_construct_mutability ~loc ~env lbl.lbl_mut argument_mode;
                  let argument_mode =
                    mode_modality lbl.lbl_modalities argument_mode
                  in
                  submode ~loc ~env mode argument_mode;
                  Kept (ty_arg1, lbl.lbl_mut,
                        unique_use ~loc ~env mode argument_mode.mode)
                end
            in
            let label_definitions = Array.map unify_kept lbl.lbl_all in
            Some {exp with exp_type = ty_exp}, label_definitions
      in
      let num_fields =
        match lbl_exp_list with [] -> assert false
        | (_, lbl,_)::_ -> Array.length lbl.lbl_all in
      if opt_sexp <> None && List.length lid_sexp_list = num_fields then
        Location.prerr_warning loc Warnings.Useless_record_with;
      let label_descriptions, representation =
        let (_, { lbl_all; lbl_repres }, _) = List.hd lbl_exp_list in
        lbl_all, lbl_repres
      in
      let fields =
        Array.map2 (fun descr def -> descr, def)
          label_descriptions label_definitions
      in
      re {
        exp_desc = Texp_record {
            fields; representation;
            extended_expression = opt_exp;
            alloc_mode
          };
        exp_loc = loc; exp_extra = [];
        exp_type = instance ty_expected;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_field(srecord, lid) ->
      let (record, rmode, label, _) =
        type_label_access env srecord Env.Projection lid
      in
      let ty_arg =
        with_local_level_if_principal begin fun () ->
          (* [ty_arg] is the type of field, [ty_res] is the type of record, they
           could share type variables, which are now instantiated *)
          let (_, ty_arg, ty_res) = instance_label false label in
          (* we now link the two record types *)
          unify_exp env record ty_res;
          ty_arg
        end ~post:generalize_structure
      in
      check_project_mutability ~loc:record.exp_loc ~env label.lbl_mut rmode;
      let mode = Modality.Value.Const.apply label.lbl_modalities rmode in
      let boxing : texp_field_boxing =
        let is_float_boxing =
          match label.lbl_repres with
          | Record_float -> true
          | Record_mixed mixed -> begin
              match Types.get_mixed_product_element mixed label.lbl_num with
              | Flat_suffix Float_boxed -> true
              | Flat_suffix (Float64 | Float32 | Imm | Bits32 | Bits64 | Word) -> false
              | Value_prefix -> false
            end
          | _ -> false
        in
        match is_float_boxing with
        | true ->
          let alloc_mode, argument_mode = register_allocation expected_mode in
          let mode = mode_cross_left env Predef.type_unboxed_float mode in
          submode ~loc ~env mode argument_mode;
          let uu = unique_use ~loc ~env mode argument_mode.mode in
          Boxing (alloc_mode, uu)
        | false ->
          let mode = mode_cross_left env ty_arg mode in
          submode ~loc ~env mode expected_mode;
          let uu = unique_use ~loc ~env mode expected_mode.mode in
          Non_boxing uu
      in
      rue {
        exp_desc = Texp_field(record, lid, label, boxing);
        exp_loc = loc; exp_extra = [];
        exp_type = ty_arg;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_setfield(srecord, lid, snewval) ->
      let (record, rmode, label, expected_type) =
        type_label_access env srecord Env.Mutation lid in
      let ty_record =
        if expected_type = None
        then newvar (Jkind.of_new_sort ~why:Record_assignment)
        else record.exp_type
      in
      let (label_loc, label, newval) =
        match label.lbl_mut with
        | Mutable m0 ->
          submode ~loc:record.exp_loc ~env rmode mode_mutate_mutable;
          let mode = mutable_mode m0 |> mode_default in
          let mode = mode_modality label.lbl_modalities mode in
          type_label_exp false env mode loc ty_record (lid, label, snewval)
        | Immutable ->
          raise(Error(loc, env, Label_not_mutable lid.txt))
      in
      unify_exp env record ty_record;
      rue {
        exp_desc = Texp_setfield(record,
          Locality.disallow_right (regional_to_local
            (Value.proj (Comonadic Areality) rmode)),
          label_loc, label, newval);
        exp_loc = loc; exp_extra = [];
        exp_type = instance Predef.type_unit;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_array(sargl) ->
      type_generic_array
        ~loc
        ~env
        ~expected_mode
        ~ty_expected
        ~explanation
        ~mutability:(Mutable Alloc.Comonadic.Const.legacy)
        ~attributes:sexp.pexp_attributes
        sargl
  | Pexp_ifthenelse(scond, sifso, sifnot) ->
      let cond =
        type_expect env mode_max scond
          (mk_expected ~explanation:If_conditional Predef.type_bool)
      in
      begin match sifnot with
        None ->
          let ifso =
            type_expect env expected_mode sifso
              (mk_expected ~explanation:If_no_else_branch Predef.type_unit) in
          rue {
            exp_desc = Texp_ifthenelse(cond, ifso, None);
            exp_loc = loc; exp_extra = [];
            exp_type = ifso.exp_type;
            exp_attributes = sexp.pexp_attributes;
            exp_env = env }
      | Some sifnot ->
          let ifso =
            type_expect env expected_mode sifso ty_expected_explained
          in
          let ifnot =
            type_expect env expected_mode sifnot ty_expected_explained
          in
          (* Keep sharing *)
          unify_exp env ifnot ifso.exp_type;
          re {
            exp_desc = Texp_ifthenelse(cond, ifso, Some ifnot);
            exp_loc = loc; exp_extra = [];
            exp_type = ifso.exp_type;
            exp_attributes = sexp.pexp_attributes;
            exp_env = env }
      end
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1, sort1 =
        type_statement ~explanation:Sequence_left_hand_side env sexp1
      in
      let exp2 = type_expect env expected_mode sexp2 ty_expected_explained in
      re {
        exp_desc = Texp_sequence(exp1, sort1, exp2);
        exp_loc = loc; exp_extra = [];
        exp_type = exp2.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_while(scond, sbody) ->
      let env = Env.add_share_lock While_loop env in
      let cond_env = Env.add_region_lock env in
      let mode = mode_region Value.max in
      let wh_cond =
        type_expect cond_env mode scond
          (mk_expected ~explanation:While_loop_conditional Predef.type_bool)
      in
      let body_env = Env.add_region_lock env in
      let position = RTail (Regionality.disallow_left Regionality.local, FNontail) in
      let wh_body, wh_body_sort =
        type_statement ~explanation:While_loop_body
          ~position body_env sbody
      in
      rue {
        exp_desc =
          Texp_while {wh_cond; wh_body; wh_body_sort};
        exp_loc = loc; exp_extra = [];
        exp_type = instance Predef.type_unit;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_for(param, slow, shigh, dir, sbody) ->
      let for_from =
        type_expect env (mode_region Value.max) slow
          (mk_expected ~explanation:For_loop_start_index Predef.type_int)
      in
      let for_to =
        type_expect env (mode_region Value.max) shigh
          (mk_expected ~explanation:For_loop_stop_index Predef.type_int)
      in
      let env = Env.add_share_lock For_loop env in
      (* When we'll want to add Uid to for loops, we can take it from here. *)
      let (for_id, _for_uid), new_env =
        type_for_loop_index ~loc ~env ~param
      in
      let new_env = Env.add_region_lock new_env in
      let position = RTail (Regionality.disallow_left Regionality.local, FNontail) in
      let for_body, for_body_sort =
        type_statement ~explanation:For_loop_body ~position new_env sbody
      in
      rue {
        exp_desc = Texp_for {for_id; for_pat = param; for_from; for_to;
                             for_dir = dir; for_body; for_body_sort };
        exp_loc = loc; exp_extra = [];
        exp_type = instance Predef.type_unit;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_constraint (sarg, sty) ->
      let type_mode, sty = alloc_mode_from_pexp_constraint_typ_attrs sty in
      let (ty, exp_extra) =
        type_constraint env sty type_mode
      in
      let ty' = instance ty in
      let error_message_attr_opt =
        Builtin_attributes.error_message_attr sexp.pexp_attributes in
      let explanation = Option.map (fun msg -> Error_message_attr msg)
                          error_message_attr_opt in
      let arg = type_argument ?explanation env expected_mode sarg ty (instance ty) in
      rue {
        exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_attributes = arg.exp_attributes;
        exp_env = env;
        exp_extra = (exp_extra, loc, sexp.pexp_attributes) :: arg.exp_extra;
      }
  | Pexp_coerce(sarg, sty, sty') ->
      let arg, ty', exp_extra =
        type_coerce (expression_constraint sarg) env expected_mode loc sty sty'
          (* CR modes: We could consider changing value binding elaboration to
             put modes on forged [Pexp_coerce] nodes, as we do for
             [Pexp_constraint]. Then we could use that mode here instead of
             legacy.
          *)
          Alloc.Const.legacy  ~loc_arg:sarg.pexp_loc
      in
      rue {
        exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_attributes = arg.exp_attributes;
        exp_env = env;
        exp_extra = (exp_extra, loc, sexp.pexp_attributes) :: arg.exp_extra;
      }
  | Pexp_send (e, {txt=met}) ->
      submode ~loc ~env Mode.Value.legacy expected_mode;
      let pm = position_and_mode env expected_mode sexp in
      let (obj,meth,typ) =
        with_local_level_if_principal
          (fun () -> type_send env loc explanation e met)
          ~post:(fun (_,_,typ) -> generalize_structure typ)
      in
      let typ =
        match get_desc typ with
        | Tpoly (ty, []) ->
            instance ty
        | Tpoly (ty, tl) ->
            if !Clflags.principal && get_level typ <> generic_level then
              Location.prerr_warning loc
                (Warnings.Not_principal "this use of a polymorphic method");
            snd (instance_poly false tl ty)
        | Tvar _ ->
            let ty' = newvar (Jkind.Type.Primitive.value ~why:Object_field |> Jkind.of_type_jkind) in
            unify env (instance typ) (newty(Tpoly(ty',[])));
            (* if not !Clflags.nolabels then
               Location.prerr_warning loc (Warnings.Unknown_method met); *)
            ty'
        | _ ->
            assert false
      in
      rue {
        exp_desc = Texp_send(obj, meth, pm.apply_position);
        exp_loc = loc; exp_extra = [];
        exp_type = typ;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_new cl ->
      submode ~loc ~env Value.legacy expected_mode;
      let (cl_path, cl_decl, cl_mode) =
        Env.lookup_class ~loc:cl.loc cl.txt env
      in
      Value.submode_exn cl_mode Value.legacy;
      let pm = position_and_mode env expected_mode sexp in
      begin match cl_decl.cty_new with
          None ->
            raise(Error(loc, env, Virtual_class cl.txt))
        | Some ty ->
            rue {
              exp_desc =
                Texp_new (cl_path, cl, cl_decl, pm.apply_position);
              exp_loc = loc; exp_extra = [];
              exp_type = instance ty;
              exp_attributes = sexp.pexp_attributes;
              exp_env = env }
        end
  | Pexp_setinstvar (lab, snewval) -> begin
      let (path, mut, cl_num, ty) =
        Env.lookup_instance_variable ~loc lab.txt env
      in
      match mut with
      | Mutable ->
          let newval =
            type_expect env mode_legacy snewval
              (mk_expected (instance ty))
          in
          let (path_self, _) =
            Env.find_value_by_name (Longident.Lident ("self-" ^ cl_num)) env
          in
          rue {
            exp_desc = Texp_setinstvar(path_self, path, lab, newval);
            exp_loc = loc; exp_extra = [];
            exp_type = instance Predef.type_unit;
            exp_attributes = sexp.pexp_attributes;
            exp_env = env }
      | _ ->
          raise(Error(loc, env, Instance_variable_not_mutable lab.txt))
    end
  | Pexp_override lst ->
      submode ~loc ~env Value.legacy expected_mode;
      let _ =
       List.fold_right
        (fun (lab, _) l ->
           if List.exists (fun l -> l.txt = lab.txt) l then
             raise(Error(loc, env,
                         Value_multiply_overridden lab.txt));
           lab::l)
        lst
        [] in
      begin match
        try
          Env.find_value_by_name (Longident.Lident "selfpat-*") env,
          Env.find_value_by_name (Longident.Lident "self-*") env
        with Not_found ->
          raise(Error(loc, env, Outside_class))
      with
        (_, {val_type = self_ty; val_kind = Val_self (sign, _, vars, _)}),
        (path_self, _) ->
          let type_override (lab, snewval) =
            begin try
              let id = Vars.find lab.txt vars in
              let ty = Btype.instance_variable_type lab.txt sign in
              (id, lab, type_expect env mode_legacy snewval (mk_expected (instance ty)))
            with
              Not_found ->
                let vars = Vars.fold (fun var _ li -> var::li) vars [] in
                raise(Error(loc, env,
                            Unbound_instance_variable (lab.txt, vars)))
            end
          in
          let modifs = List.map type_override lst in
          rue {
            exp_desc = Texp_override(path_self, modifs);
            exp_loc = loc; exp_extra = [];
            exp_type = self_ty;
            exp_attributes = sexp.pexp_attributes;
            exp_env = env }
      | _ ->
          assert false
      end
  | Pexp_letmodule(name, smodl, sbody) ->
      let lv = get_current_level () in
      let (id, pres, modl, _, body) =
        with_local_level begin fun () ->
          let modl, pres, id, new_env =
            Typetexp.TyVarEnv.with_local_scope begin fun () ->
              let modl, md_shape = !type_module env smodl in
              Mtype.lower_nongen lv modl.mod_type;
              let pres =
                match modl.mod_type with
                | Mty_alias _ -> Mp_absent
                | _ -> Mp_present
              in
              let scope = create_scope () in
              let md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) in
              let md_shape = Shape.set_uid_if_none md_shape md_uid in
              let md =
                { md_type = modl.mod_type; md_attributes = [];
                  md_loc = name.loc;
                  md_uid; }
              in
              let (id, new_env) =
                match name.txt with
                | None -> None, env
                | Some name ->
                    let id, env =
                      Env.enter_module_declaration
                        ~scope ~shape:md_shape name pres md env
                    in
                    Some id, env
              in
              modl, pres, id, new_env
            end
          in
          (* Ideally, we should catch Expr_type_clash errors
             in type_expect triggered by escaping identifiers
             from the local module and refine them into
             Scoping_let_module errors
           *)
          let body = type_expect new_env expected_mode sbody ty_expected_explained in
          (id, pres, modl, new_env, body)
        end
        ~post: begin fun (_id, _pres, _modl, new_env, body) ->
          (* Ensure that local definitions do not leak. *)
          (* required for implicit unpack *)
          enforce_current_level new_env body.exp_type
        end
      in
      re {
        exp_desc = Texp_letmodule(id, name, pres, modl, body);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_letexception(cd, sbody) ->
      let (cd, newenv, _shape) = Typedecl.transl_exception env cd in
      let body =
        type_expect newenv expected_mode sbody ty_expected_explained
      in
      re {
        exp_desc = Texp_letexception(cd, body);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }

  | Pexp_assert (e) ->
      let cond =
        type_expect env mode_max e
          (mk_expected ~explanation:Assert_condition Predef.type_bool)
      in
      let exp_type =
        match cond.exp_desc with
        | Texp_construct(_, {cstr_name="false"}, _, _) ->
            instance ty_expected
        | _ ->
            instance Predef.type_unit
      in
      let rec innermost_location loc_stack =
        match loc_stack with
        | [] -> loc
        | [l] -> l
        | _ :: s -> innermost_location s
      in
      rue {
        exp_desc = Texp_assert (cond, innermost_location sexp.pexp_loc_stack);
        exp_loc = loc; exp_extra = [];
        exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env;
      }
  | Pexp_lazy e ->
      submode ~loc ~env Value.legacy expected_mode;
      let ty = newgenvar (Jkind.Type.Primitive.value ~why:Lazy_expression |> Jkind.of_type_jkind) in
      let to_unify = Predef.type_lazy_t ty in
      with_explanation (fun () ->
        unify_exp_types loc env to_unify (generic_instance ty_expected));
      let env = Env.add_escape_lock Lazy env in
      let env = Env.add_share_lock Lazy env in
      let arg = type_expect env mode_legacy e (mk_expected ty) in
      re {
        exp_desc = Texp_lazy arg;
        exp_loc = loc; exp_extra = [];
        exp_type = instance ty_expected;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env;
      }
  | Pexp_object s ->
      submode ~loc ~env Value.legacy expected_mode;
      let desc, meths = !type_object env loc s in
      rue {
        exp_desc = Texp_object (desc, meths);
        exp_loc = loc; exp_extra = [];
        exp_type = desc.cstr_type.csig_self;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env;
      }
  | Pexp_poly(sbody, sty) ->
      let ty, cty =
        with_local_level_if_principal
          ~post:(fun (ty,_) -> generalize_structure ty)
          begin fun () ->
            match sty with None -> protect_expansion env ty_expected, None
            | Some sty ->
                let sty = Ast_helper.Typ.force_poly sty in
                let cty =
                  Typetexp.transl_simple_type ~new_var_jkind:Any env ~closed:false
                    Alloc.Const.legacy sty
                in
                cty.ctyp_type, Some cty
          end
      in
      if sty <> None then
        with_explanation (fun () ->
          unify_exp_types loc env (instance ty) (instance ty_expected));
      let exp =
        match get_desc (expand_head env ty) with
          Tpoly (ty', []) ->
            let exp = type_expect env expected_mode sbody (mk_expected ty') in
            { exp with exp_type = instance ty }
        | Tpoly (ty', tl) ->
            (* One more level to generalize locally *)
            let (exp,_) =
              with_local_level begin fun () ->
                let vars, ty'' =
                  with_local_level_if_principal
                    (fun () -> instance_poly true tl ty')
                    ~post:(fun (_,ty'') -> generalize_structure ty'')
                in
                let exp = type_expect env expected_mode sbody (mk_expected ty'') in
                (exp, vars)
              end
              ~post: begin fun (exp,vars) ->
                generalize_and_check_univars env "method" exp ty_expected vars
              end
            in
            { exp with exp_type = instance ty }
        | Tvar _ ->
            let exp = type_exp env expected_mode sbody in
            let exp = {exp with exp_type = newmono exp.exp_type} in
            unify_exp env exp ty;
            exp
        | _ -> assert false
      in
      re { exp with exp_extra =
             (Texp_poly cty, loc, sexp.pexp_attributes) :: exp.exp_extra }
  | Pexp_newtype(name, sbody) ->
    type_newtype_expr ~loc ~env ~expected_mode ~rue ~attributes:sexp.pexp_attributes
      name None sbody
  | Pexp_pack m ->
      (* CR zqian: pass [expected_mode] to [type_package] *)
      submode ~loc ~env Value.legacy expected_mode;
      let (p, fl) =
        match get_desc (Ctype.expand_head env (instance ty_expected)) with
          Tpackage (p, fl) ->
            if !Clflags.principal &&
              get_level (Ctype.expand_head env
                           (protect_expansion env ty_expected))
                < Btype.generic_level
            then
              Location.prerr_warning loc
                (Warnings.Not_principal "this module packing");
            (p, fl)
        | Tvar _ ->
            raise (Error (loc, env, Cannot_infer_signature))
        | _ ->
            raise (Error (loc, env, Not_a_packed_module ty_expected))
      in
      let (modl, fl') = !type_package env m p fl in
      rue {
        exp_desc = Texp_pack modl;
        exp_loc = loc; exp_extra = [];
        exp_type = newty (Tpackage (p, fl'));
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_open (od, e) ->
      let tv = newvar (Jkind.Primitive.top ~why:Dummy_jkind) in
      let (od, _, newenv) = !type_open_decl env od in
      let exp = type_expect newenv expected_mode e ty_expected_explained in
      (* Force the return type to be well-formed in the original
         environment. *)
      unify_var newenv tv exp.exp_type;
      re {
        exp_desc = Texp_open (od, exp);
        exp_type = exp.exp_type;
        exp_loc = loc;
        exp_extra = [];
        exp_attributes = sexp.pexp_attributes;
        exp_env = env;
      }
  | Pexp_letop{ let_ = slet; ands = sands; body = sbody } ->
      submode ~loc ~env Value.legacy expected_mode;
      let rec loop spat_acc ty_acc ty_acc_sort sands =
        match sands with
        | [] -> spat_acc, ty_acc, ty_acc_sort
        | { pbop_pat = spat; _} :: rest ->
            (* CR layouts v5: eliminate value requirement *)
            let ty = newvar (Jkind.Type.Primitive.value ~why:Tuple_element |> Jkind.of_type_jkind) in
            let loc = Location.ghostify slet.pbop_op.loc in
            let spat_acc = Ast_helper.Pat.tuple ~loc [spat_acc; spat] in
            let ty_acc = newty (Ttuple [None, ty_acc; None, ty]) in
            loop spat_acc ty_acc Jkind.Type.Sort.value rest
      in
      let op_path, op_desc, op_type, spat_params, ty_params, param_sort,
          ty_func_result, body_sort, ty_result, op_result_sort,
          ty_andops, sort_andops =
        with_local_level_iter_if_principal
          ~post:generalize_structure begin fun () ->
          let let_loc = slet.pbop_op.loc in
          let op_path, op_desc = type_binding_op_ident env slet.pbop_op in
          let op_type = op_desc.val_type in
          let spat_params, ty_params, param_sort =
            let initial_jkind, initial_sort = match sands with
              | [] ->
                Jkind.of_new_sort_var ~why:Function_argument
              (* CR layouts v5: eliminate value requirement for tuple elements *)
              | _ -> Jkind.Type.Primitive.value ~why:Tuple_element |> Jkind.of_type_jkind, Jkind.Type.Sort.value
            in
            loop slet.pbop_pat (newvar initial_jkind) initial_sort sands
          in
          let ty_func_result, body_sort = new_rep_var ~why:Function_result () in
          let arrow_desc = Nolabel, Alloc.legacy, Alloc.legacy in
          let ty_func =
            newty (Tarrow(arrow_desc, newmono ty_params, ty_func_result,
                          commu_ok))
          in
          let ty_result, op_result_sort = new_rep_var ~why:Function_result () in
          let ty_andops, sort_andops = new_rep_var ~why:Function_argument () in
          let ty_op =
            newty (Tarrow(arrow_desc, newmono ty_andops,
              newty (Tarrow(arrow_desc, newmono ty_func, ty_result, commu_ok)),
                     commu_ok))
          in
          begin try
            unify env op_type ty_op
          with Unify err ->
            raise(Error(let_loc, env, Letop_type_clash(slet.pbop_op.txt, err)))
          end;
          ((op_path, op_desc, op_type, spat_params, ty_params, param_sort,
            ty_func_result, body_sort, ty_result, op_result_sort,
            ty_andops, sort_andops),
           [ty_andops; ty_params; ty_func_result; ty_result])
        end
      in
      let exp, exp_sort, ands =
        type_andops env slet.pbop_exp sands sort_andops ty_andops
      in
      let body_env = Env.add_escape_lock Letop env in
      let body_env = Env.add_share_lock Letop body_env in
      let scase = Ast_helper.Exp.case spat_params sbody in
      let cases, partial =
        type_cases Value body_env
          (simple_pat_mode Value.legacy) (mode_return Value.legacy)
          ty_params (mk_expected ty_func_result)
          ~check_if_total:true loc [scase]
      in
      let body =
        match cases with
        | [case] -> case
        | _ -> assert false
      in
      let param = name_cases "param" cases in
      let let_ =
        { bop_op_name = slet.pbop_op;
          bop_op_path = op_path;
          bop_op_val = op_desc;
          bop_op_type = op_type;
          bop_op_return_sort = op_result_sort;
          bop_exp = exp;
          bop_exp_sort = exp_sort;
          bop_loc = slet.pbop_loc; }
      in
      let desc =
        Texp_letop{let_; ands; param; param_sort; body; body_sort; partial}
      in
      rue { exp_desc = desc;
            exp_loc = sexp.pexp_loc;
            exp_extra = [];
            exp_type = instance ty_result;
            exp_env = env;
            exp_attributes = sexp.pexp_attributes; }

  | Pexp_extension ({ txt = ("ocaml.extension_constructor"
                             |"extension_constructor"); _ },
                    payload) ->
      begin match payload with
      | PStr [ { pstr_desc =
                   Pstr_eval ({ pexp_desc = Pexp_construct (lid, None); _ }, _)
               } ] ->
          let path =
            let cd =
              Env.lookup_constructor Env.Positive ~loc:lid.loc lid.txt env
            in
            match cd.cstr_tag with
            | Extension (path,_) -> path
            | _ -> raise (Error (lid.loc, env, Not_an_extension_constructor))
          in
          rue {
            exp_desc = Texp_extension_constructor (lid, path);
            exp_loc = loc; exp_extra = [];
            exp_type = instance Predef.type_extension_constructor;
            exp_attributes = sexp.pexp_attributes;
            exp_env = env }
      | _ ->
          raise (Error (loc, env, Invalid_extension_constructor_payload))
      end
  | Pexp_extension ({ txt = ("probe" | "ocaml.probe"); _ }, payload) ->
    begin match Builtin_attributes.get_tracing_probe_payload payload with
    | Error () -> raise (Error (loc, env, Probe_format))
    | Ok { name; name_loc; enabled_at_init; arg; } ->
        check_probe_name name name_loc env;
        let env = Env.add_escape_lock Probe env in
        let env = Env.add_share_lock Probe env in
        Env.add_probe name;
        let exp = type_expect env mode_legacy arg
                    (mk_expected Predef.type_unit) in
        rue {
          exp_desc = Texp_probe {name; handler=exp; enabled_at_init};
          exp_loc = loc; exp_extra = [];
          exp_type = instance Predef.type_unit;
          exp_attributes = sexp.pexp_attributes;
          exp_env = env }
    end
  | Pexp_extension ({ txt = ("probe_is_enabled"
                            |"ocaml.probe_is_enabled"); _ }, payload) ->
      begin match payload with
      | PStr ([{ pstr_desc =
                   Pstr_eval
                     ({pexp_desc=(Pexp_constant (Pconst_string(name,_,None)));
                       pexp_loc = name_loc;
                       _ } ,
                      _)}]) ->
        check_probe_name name name_loc env;
        add_delayed_check
          (fun () ->
             if not (Env.has_probe name) then
               raise(Error(name_loc, env, (Probe_name_undefined name))));
        rue {
          exp_desc = Texp_probe_is_enabled {name};
          exp_loc = loc; exp_extra = [];
          exp_type = instance Predef.type_bool;
          exp_attributes = sexp.pexp_attributes;
          exp_env = env }
      | _ -> raise (Error (loc, env, Probe_is_enabled_format))
    end
  | Pexp_extension ({ txt = "src_pos"; _ }, _) ->
      rue (src_pos loc sexp.pexp_attributes env)
  | Pexp_extension ext ->
    raise (Error_forward (Builtin_attributes.error_of_extension ext))

  | Pexp_unreachable ->
      re { exp_desc = Texp_unreachable;
           exp_loc = loc; exp_extra = [];
           exp_type = instance ty_expected;
           exp_attributes = sexp.pexp_attributes;
           exp_env = env }

and expression_constraint pexp =
  { type_without_constraint = (fun env expected_mode ->
        let expr = type_exp env expected_mode pexp in
        expr, expr.exp_type);
    type_with_constraint =
      (fun env expected_mode ty ->
         type_argument env expected_mode pexp ty (instance ty));
    is_self =
      (fun expr ->
         match expr.exp_desc with
         | Texp_ident (_, _, { val_kind = Val_self _ }, _, _) -> true
         | _ -> false);
  }

(** Types a body in the scope of a coercion (with an optional constraint)
    and returns the inferred type. See the comment on {!constraint_arg} for
    an explanation of how this typechecking is polymorphic in the body.
*)
and type_coerce
  : type a. a constraint_arg -> _ -> _ -> _ -> _ -> _ -> _ -> loc_arg:_
         -> a * type_expr * exp_extra =
  fun constraint_arg env expected_mode loc sty sty' type_mode ~loc_arg ->
  (* Pretend separate = true, 1% slowdown for lablgtk *)
  (* Also see PR#7199 for a problem with the following:
      let separate = !Clflags.principal || Env.has_local_constraints env in*)
  let { is_self; type_with_constraint; type_without_constraint } =
    constraint_arg
  in
  match sty with
  | None ->
    let (cty', ty', force) =
      Typetexp.transl_simple_type_delayed env type_mode sty'
    in
    let arg, arg_type, gen =
      let lv = get_current_level () in
      with_local_level begin fun () ->
          let arg, arg_type = type_without_constraint env expected_mode in
          arg, arg_type, generalizable lv arg_type
        end
        ~post:(fun (_, arg_type, _) -> enforce_current_level env arg_type)
    in
    begin match !self_coercion, get_desc ty' with
      | ((path, r) :: _, Tconstr (path', _, _))
        when is_self arg && Path.same path path' ->
          (* prerr_endline "self coercion"; *)
          r := loc :: !r;
          force ()
      | _ when free_variables ~env arg_type = []
            && free_variables ~env ty' = [] ->
          if not gen && (* first try a single coercion *)
            let snap = snapshot () in
            let ty, _b = enlarge_type env ty' in
            try
              force (); Ctype.unify env arg_type ty; true
            with Unify _ ->
              backtrack snap; false
          then ()
          else begin try
            let force' = subtype env arg_type ty' in
            force (); force' ();
            if not gen && !Clflags.principal then
              Location.prerr_warning loc
                (Warnings.Not_principal "this ground coercion");
          with Subtype err ->
            (* prerr_endline "coercion failed"; *)
            raise (Error (loc, env, Not_subtype err))
          end;
      | _ ->
          let ty, b = enlarge_type env ty' in
          force ();
          begin try Ctype.unify env arg_type ty with Unify err ->
            let expanded = full_expand ~may_forget_scope:true env ty' in
            raise(Error(loc_arg, env,
                        Coercion_failure ({ ty = ty'; expanded }, err, b)))
          end
      end;
      (arg, ty', Texp_coerce (None, cty'))
  | Some sty ->
      let cty, ty, force, cty', ty', force' =
        with_local_level_iter ~post:generalize_structure begin fun () ->
          let (cty, ty, force) =
            Typetexp.transl_simple_type_delayed env type_mode sty
          and (cty', ty', force') =
            Typetexp.transl_simple_type_delayed env type_mode sty'
          in
          ((cty, ty, force, cty', ty', force'),
           [ ty; ty' ])
        end
      in
      begin try
        let force'' = subtype env (instance ty) (instance ty') in
        force (); force' (); force'' ()
      with Subtype err ->
        raise (Error (loc, env, Not_subtype err))
      end;
      (type_with_constraint env expected_mode ty,
       instance ty', Texp_coerce (Some cty, cty'))

and type_constraint env sty type_mode =
  (* Pretend separate = true, 1% slowdown for lablgtk *)
  let cty =
    with_local_level begin fun () ->
      Typetexp.transl_simple_type ~new_var_jkind:Any env ~closed:false type_mode sty
    end
      ~post:(fun cty -> generalize_structure cty.ctyp_type)
  in
  cty.ctyp_type, Texp_constraint cty

(** Types a body in the scope of a coercion (:>) or a constraint (:), and
    unifies the inferred type with the expected type.
    @param loc the location of the overall constraint
    @param loc_arg the location of the thing being constrained
*)
and type_constraint_expect
  : type a. a constraint_arg -> _ -> _ -> _ -> loc_arg:_ -> _ -> _ -> a * _ * _
  =
  fun constraint_arg env expected_mode loc ~loc_arg constraint_ ty_expected ->
  let ret, ty, exp_extra =
    let open Jane_syntax.N_ary_functions in
    let { type_constraint = constraint_; mode_annotations } = constraint_ in
    let type_mode = Typemode.transl_alloc_mode mode_annotations in
    match constraint_ with
    | Pcoerce (ty_constrain, ty_coerce) ->
        type_coerce constraint_arg env expected_mode loc ty_constrain ty_coerce
          type_mode ~loc_arg
    | Pconstraint ty_constrain ->
        let ty, exp_extra = type_constraint env ty_constrain type_mode in
        constraint_arg.type_with_constraint env expected_mode ty, ty, exp_extra
  in
  unify_exp_types loc env ty (instance ty_expected);
  ret, ty, exp_extra

and type_ident env ?(recarg=Rejected) lid =
  let path, desc, actual_mode = Env.lookup_value ~loc:lid.loc lid.txt env in
  (* Mode crossing here is needed only because of the strange behaviour of
  [type_let] - it checks the LHS before RHS. Had it checks the RHS before LHS,
  identifiers would be mode crossed when being added to the environment. *)
  let actual_mode = actual_mode_cross_left env desc.val_type actual_mode in
  let is_recarg =
    match get_desc desc.val_type with
    | Tconstr(p, _, _) -> Path.is_constructor_typath p
    | _ -> false
  in
  begin match is_recarg, recarg, get_desc desc.val_type with
  | _, Allowed, _
  | true, Required, _
  | false, Rejected, _ -> ()
  | true, Rejected, _
  | false, Required, (Tvar _ | Tconstr _) ->
      raise (Error (lid.loc, env, Inlined_record_escape))
  | false, Required, _  -> () (* will fail later *)
  end;
  let val_type, kind =
    match desc.val_kind with
    | Val_prim prim ->
       let ty, mode, sort = instance_prim prim desc.val_type in
       let ty = instance ty in
       begin match prim.prim_native_repr_res, mode with
       (* if the locality of returned value of the primitive is poly
          we then register allocation for further optimization *)
       | (Prim_poly, _), Some mode ->
           register_allocation_mode
             (Alloc.meet [Alloc.max_with (Comonadic Areality) mode;
                          Alloc.max_with (Comonadic Linearity) Linearity.many])
       | _ -> ()
       end;
       ty, Id_prim (Option.map Locality.disallow_right mode, sort)
    | _ ->
       instance desc.val_type, Id_value in
  path, actual_mode, { desc with val_type }, kind

and type_binding_op_ident env s =
  let loc = s.loc in
  let lid = Location.mkloc (Longident.Lident s.txt) loc in
  let path, actual_mode, desc, kind = type_ident env lid in
  actual_submode ~env ~loc:lid.loc ~reason:Other actual_mode mode_legacy;
  let path =
    match desc.val_kind with
    | Val_ivar _ ->
        fatal_error "Illegal name for instance variable"
    | Val_self (_, _, _, cl_num) ->
        let path, _ =
          Env.find_value_by_name (Longident.Lident ("self-" ^ cl_num)) env
        in
        path
    | _ -> path
  in
  (* FIXME is [external (let+)] valid? *)
  assert (kind = Id_value);
  path, desc

(* Typecheck parameters one at a time followed by the body. Later parameters
   are checked in the scope of earlier ones. That's necessary to support
   constructs like [fun (type a) (x : a) -> ...] and
   [fun (module M : S) (x : M.t) -> ...].

   Operates like [type_expect] in that it unifies the "type of the remaining
   function params + body" with [ty_expected], and returns out the inferred
   type.

   See [split_function_ty] for the meaning of [first] and [in_function].

   See [type_function_result] for the meaning of the returned type.
*)
and type_function
      env (expected_mode : expected_mode) ty_expected
      params_suffix body_constraint body ~first ~in_function
  : type_function_result
  =
  let open Jane_syntax.N_ary_functions in
  let { ty_fun; loc_fun; _ } = in_function in
  (* The "rest of the function" extends from the start of the first parameter
     to the end of the overall function. The parser does not construct such
     a location so we forge one for type errors.
  *)
  let loc : Location.t =
    match params_suffix, body with
    | param :: _, _ ->
        { loc_start = param.pparam_loc.loc_start;
          loc_end = loc_fun.loc_end;
          loc_ghost = true;
        }
    | [], Pfunction_body pexp -> pexp.pexp_loc
    | [], Pfunction_cases (_, loc_cases, _) -> loc_cases
  in
  match params_suffix with
  | { pparam_desc = Pparam_newtype (newtype_var, jkind_annot) } :: rest ->
      (* Check everything else in the scope of (type a). *)
      let (params, body, newtypes, contains_gadt, fun_alloc_mode, ret_info),
          exp_type, jkind_annot =
        type_newtype env newtype_var jkind_annot (fun env ->
          let { function_ = exp_type, params, body;
                newtypes; params_contain_gadt = contains_gadt;
                fun_alloc_mode; ret_info;
              }
            =
            (* mimic the typing of Pexp_newtype by minting a new type var,
                like [type_exp].
            *)
            type_function env expected_mode
              (newvar (Jkind.Primitive.top ~why:Dummy_jkind))
              rest body_constraint body ~in_function ~first
          in
          (params, body, newtypes, contains_gadt, fun_alloc_mode, ret_info),
          exp_type)
      in
      let newtype = newtype_var, jkind_annot in
      with_explanation ty_fun.explanation (fun () ->
          unify_exp_types loc env exp_type (instance ty_expected));
      { function_ = exp_type, params, body;
        params_contain_gadt = contains_gadt; newtypes = newtype :: newtypes;
        fun_alloc_mode; ret_info;
      }
  | { pparam_desc = Pparam_val (arg_label, default_arg, pat); pparam_loc }
      :: rest
    ->
      let typed_arg_label, pat =
        Typetexp.transl_label_from_pat arg_label pat
      in
      let mode_annots, pat = mode_annots_from_pat_attrs pat in
      let has_poly = has_poly_constraint pat in
      if has_poly && is_optional_parsetree arg_label then
        raise(Error(pat.ppat_loc, env, Optional_poly_param));
      if has_poly
      && not (Language_extension.is_enabled Polymorphic_parameters) then
        raise (Typetexp.Error (loc, env,
                               Unsupported_extension Polymorphic_parameters));
      let is_final_val_param =
        match body with
        | Pfunction_cases _ -> false
        | Pfunction_body _ ->
            (* This may appear quadratic but it's actually linear when amortized
               over the outer call to [type_function], as we visit each
               [Pparam_newtype] only once. *)
            List.for_all
              (fun { pparam_desc } ->
                match pparam_desc with
                | Pparam_val _ -> false
                | Pparam_newtype _ -> true)
              rest
      in
      let env,
          { filtered_arrow = { ty_arg; arg_mode; ty_ret; ret_mode };
            arg_sort; ret_sort;
            ty_arg_mono; expected_pat_mode; expected_inner_mode;
            alloc_mode;
          } =
        split_function_ty env expected_mode ty_expected loc
          ~is_first_val_param:first ~is_final_val_param
          ~arg_label:typed_arg_label ~in_function ~has_poly ~mode_annots
      in
      (* [ty_arg_internal] is the type of the parameter viewed internally
         to the function. This is different than [ty_arg_mono] exactly for
         optional arguments with defaults, where the external [ty_arg_mono]
         is optional and the internal view is not optional.
      *)
      let ty_arg_internal, default_arg =
        match default_arg with
        | None -> ty_arg_mono, None
        | Some default ->
            let arg_label =
              match arg_label with
              | Optional arg_label -> arg_label
              | Nolabel | Labelled _ ->
                Misc.fatal_error "[default] allowed only with optional argument"
            in
            let default_arg_jkind, default_arg_sort =
              Jkind.of_new_sort_var ~why:Optional_arg_default
            in
            let ty_default_arg = newvar default_arg_jkind in
            begin
              try unify env (type_option ty_default_arg) ty_arg_mono
              with Unify _ -> assert false;
            end;
            (* Issue#12668: Retain type-directed disambiguation of
               ?x:(y : Variant.t = Constr)
            *)
            let default =
              match pat.ppat_desc with
              | Ppat_constraint (_, sty) ->
                  let gloc = { default.pexp_loc with loc_ghost = true } in
                  Ast_helper.Exp.constraint_ default sty ~loc:gloc
              | _ -> default
            in
            (* Defaults are always global. They can be moved out of the
               function's region by Simplf.split_default_wrapper. *)
            let default_arg =
              type_expect env mode_legacy default (mk_expected ty_default_arg)
            in
            ty_default_arg, Some (default_arg, arg_label, default_arg_sort)
      in
      let (pat, params, body, ret_info, newtypes, contains_gadt, curry), partial =
        (* Check everything else in the scope of the parameter. *)
        map_half_typed_cases Value env expected_pat_mode
          ty_arg_internal ty_ret pat.ppat_loc
          ~check_if_total:true
          (* We don't make use of [case_data] here so we pass unit. *)
          [ { pattern = pat; has_guard = false; needs_refute = false }, () ]
          ~type_body:begin
            fun () pat ~ext_env ~ty_expected ~ty_infer:_
              ~contains_gadt:param_contains_gadt ->
              let { function_ = _, params_suffix, body;
                    newtypes; params_contain_gadt = suffix_contains_gadt;
                    fun_alloc_mode; ret_info;
                  }
                =
                type_function ext_env expected_inner_mode ty_expected
                  rest body_constraint body
                  ~in_function ~first:false
              in
              let contains_gadt =
                if param_contains_gadt then
                  Contains_gadt
                else
                  suffix_contains_gadt
              in
              let curry =
                match fun_alloc_mode with
                (* See the comment on the [fun_alloc_mode] field for its
                   corralation to [is_final_val_param]. *)
                | None ->
                  assert(is_final_val_param);
                  Final_arg
                | Some fun_alloc_mode ->
                  assert(not is_final_val_param);
                  (* Handle mode crossing of [arg_mode]. Note that [close_over]
                     uses the [arg_mode.comonadic] as a left mode, and
                     [arg_mode.monadic] as a right mode, hence they need to be
                     mode-crossed differently. *)
                  let arg_mode = alloc_mode_cross_to_max_min env ty_arg arg_mode in
                  begin match
                    Alloc.submode (Alloc.close_over arg_mode) fun_alloc_mode
                  with
                    | Ok () -> ()
                    | Error e ->
                      raise (Error(loc_fun, env, Uncurried_function_escapes e))
                  end;
                  begin match
                    Alloc.submode (Alloc.partial_apply alloc_mode) fun_alloc_mode
                  with
                    | Ok () -> ()
                    | Error e ->
                      raise (Error(loc_fun, env, Uncurried_function_escapes e))
                  end;
                  More_args {partial_mode = Alloc.disallow_right fun_alloc_mode}
              in
              pat, params_suffix, body, ret_info, newtypes, contains_gadt, curry
          end
        |> function
          (* The result must be a singleton because we passed a singleton
             list above. *)
        | [ result ], partial -> result, partial
        | ([] | _ :: _ :: _), _ -> assert false
      in
      let exp_type =
        instance
          (newgenty
             (Tarrow
                ((typed_arg_label, arg_mode, ret_mode), ty_arg, ty_ret, commu_ok)))
      in
      (* This is quadratic, as it operates over the entire tail of the
         type for each new parameter. Now that functions are n-ary, we
         could possibly run this once.
      *)
      with_explanation ty_fun.explanation (fun () ->
          unify_exp_types loc env exp_type (instance ty_expected));
      (* This is quadratic, as it extracts all of the parameters from an arrow
         type for each parameter that's added. Now that functions are n-ary,
         there might be an opportunity to improve this.
      *)
      let not_nolabel_function ty =
        (* [list_labels] does expansion and is potentially expensive; only
           call this when necessary. *)
        let ls, tvar = list_labels env ty in
        List.for_all (( <> ) Nolabel) ls && not tvar
      in
      if is_optional typed_arg_label && not_nolabel_function ty_ret then
        Location.prerr_warning pat.pat_loc
          Warnings.Unerasable_optional_argument
      else if is_position typed_arg_label && not_nolabel_function ty_ret then
        Location.prerr_warning pat.pat_loc
          Warnings.Unerasable_position_argument;
      let fp_kind, fp_param =
        match default_arg with
        | None ->
            let param = name_pattern "param" [ pat ] in
            Tparam_pat pat, param
        | Some (default_arg, arg_label, default_arg_sort) ->
            let param = Ident.create_local ("*opt*" ^ arg_label) in
            Tparam_optional_default (pat, default_arg, default_arg_sort), param
      in
      let param =
        { has_poly;
          param =
            { fp_kind;
              fp_arg_label = typed_arg_label;
              fp_param;
              fp_partial = partial;
              fp_newtypes = newtypes;
              fp_sort = arg_sort;
              fp_mode = Alloc.disallow_right arg_mode;
              fp_curry = curry;
              fp_loc = pparam_loc;
            };
        }
      in
      let ret_info =
        match ret_info with
        | Some _ as x -> x
        | None -> Some { ret_sort; ret_mode = Alloc.disallow_right ret_mode }
      in
      { function_ = exp_type, param :: params, body;
        newtypes = []; params_contain_gadt = contains_gadt;
        ret_info; fun_alloc_mode = Some alloc_mode;
      }
  | [] ->
    let exp_type, body, fun_alloc_mode, ret_info =
      match body with
      | Pfunction_body body ->
          let body =
            match body_constraint with
            | None -> type_expect env expected_mode body (mk_expected ty_expected)
            | Some constraint_ ->
                let body_loc = body.pexp_loc in
                let body, exp_type, exp_extra =
                  type_constraint_expect (expression_constraint body)
                    env expected_mode body_loc ~loc_arg:body_loc constraint_ ty_expected
                in
                { body with
                    exp_extra = (exp_extra, body_loc, []) :: body.exp_extra;
                    exp_type;
                }
          in
          body.exp_type, Tfunction_body body, None, None
      | Pfunction_cases (cases, _, attributes) ->
          let type_cases_expect env expected_mode ty_expected =
            type_function_cases_expect
              env expected_mode ty_expected loc cases attributes ~in_function
              ~first
          in
          let (cases, exp_type, fun_alloc_mode, ret_info), exp_extra =
            match body_constraint with
            | None -> type_cases_expect env expected_mode ty_expected, None
            | Some constraint_ ->
              (* The typing of function case coercions/constraints is
                  analogous to the typing of expression coercions/constraints.

                  - [type_with_constraint]: If there is a constraint, then call
                    [type_argument] on the cases, and discard the cases'
                    inferred type in favor of the constrained type. (Function
                    cases aren't inferred, so [type_argument] would just call
                    [type_expect] straightaway, so we do the same here.)
                  - [type_without_constraint]: If there is just a coercion and
                    no constraint, call [type_exp] on the cases and surface the
                    cases' inferred type to [type_constraint_expect]. *)
              let function_cases_constraint_arg =
                { is_self = (fun _ -> false);
                  type_with_constraint = (fun env expected_mode ty ->
                    let cases, _, fun_alloc_mode, ret_info =
                      type_cases_expect env expected_mode ty
                    in
                    cases, fun_alloc_mode, ret_info);
                  type_without_constraint = (fun env expected_mode ->
                    let cases, ty_fun, fun_alloc_mode, ret_info =
                      (* The analogy to [type_exp] for expressions. *)
                      type_cases_expect env expected_mode
                        (newvar (Jkind.Primitive.top ~why:Dummy_jkind))
                    in
                    (cases, fun_alloc_mode, ret_info), ty_fun);
                }
              in
              let (body, fun_alloc_mode, ret_info), exp_type, exp_extra =
                type_constraint_expect function_cases_constraint_arg
                  env expected_mode loc constraint_ ty_expected ~loc_arg:loc
              in
              (body, exp_type, fun_alloc_mode, ret_info), Some exp_extra
          in
          let cases =
            match exp_extra with
            | None -> cases
            | Some _ as fc_exp_extra -> { cases with fc_exp_extra }
          in
          exp_type, Tfunction_cases cases, Some fun_alloc_mode, Some ret_info
     in
     { function_ = exp_type, [], body; newtypes = [];
     (* [No_gadt] is fine because this return value is only meant to indicate
        whether [params] (here, the empty list) contains any GADT, not whether
        the body is a [Tfunction_cases] whose patterns include a GADT.
     *)
       params_contain_gadt = No_gadt;
       ret_info; fun_alloc_mode;
     }

and type_label_access env srecord usage lid =
  let mode = Value.newvar () in
  let record =
    with_local_level_if_principal ~post:generalize_structure_exp
      (fun () -> type_exp ~recarg:Allowed env (mode_default mode) srecord)
  in
  let ty_exp = record.exp_type in
  let expected_type =
    match extract_concrete_record env ty_exp with
    | Record_type(p0, p, _, _) ->
        Some(p0, p, is_principal ty_exp)
    | Maybe_a_record_type -> None
    | Not_a_record_type ->
        let error = Expr_not_a_record_type ty_exp in
        raise (Error (record.exp_loc, env, error))
  in
  let labels = Env.lookup_all_labels ~loc:lid.loc usage lid.txt env in
  let label =
    wrap_disambiguate "This expression has" (mk_expected ty_exp)
      (Label.disambiguate usage lid env expected_type) labels in
  (record, mode, label, expected_type)

(* Typing format strings for printing or reading.
   These formats are used by functions in modules Printf, Format, and Scanf.
   (Handling of * modifiers contributed by Thorsten Ohl.) *)

and type_format loc str env =
  let loc = Location.ghostify loc in
  try
    CamlinternalFormatBasics.(CamlinternalFormat.(
      let mk_exp_loc pexp_desc = {
        pexp_desc = pexp_desc;
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      } and mk_lid_loc lid = {
        txt = lid;
        loc = loc;
      } in
      let mk_constr name args =
        let lid = Longident.(Ldot(Lident "CamlinternalFormatBasics", name)) in
        let arg = match args with
          | []          -> None
          | [ e ]       -> Some e
          | _ :: _ :: _ -> Some (mk_exp_loc (Pexp_tuple args)) in
        mk_exp_loc (Pexp_construct (mk_lid_loc lid, arg)) in
      let mk_cst cst = mk_exp_loc (Pexp_constant cst) in
      let mk_int n = mk_cst (Pconst_integer (Int.to_string n, None))
      and mk_string str = mk_cst (Pconst_string (str, loc, None))
      and mk_char chr = mk_cst (Pconst_char chr) in
      let rec mk_formatting_lit fmting = match fmting with
        | Close_box ->
          mk_constr "Close_box" []
        | Close_tag ->
          mk_constr "Close_tag" []
        | Break (org, ns, ni) ->
          mk_constr "Break" [ mk_string org; mk_int ns; mk_int ni ]
        | FFlush ->
          mk_constr "FFlush" []
        | Force_newline ->
          mk_constr "Force_newline" []
        | Flush_newline ->
          mk_constr "Flush_newline" []
        | Magic_size (org, sz) ->
          mk_constr "Magic_size" [ mk_string org; mk_int sz ]
        | Escaped_at ->
          mk_constr "Escaped_at" []
        | Escaped_percent ->
          mk_constr "Escaped_percent" []
        | Scan_indic c ->
          mk_constr "Scan_indic" [ mk_char c ]
      and mk_formatting_gen : type a b c d e f .
          (a, b, c, d, e, f) formatting_gen -> Parsetree.expression =
        fun fmting -> match fmting with
        | Open_tag (Format (fmt', str')) ->
          mk_constr "Open_tag" [ mk_format fmt' str' ]
        | Open_box (Format (fmt', str')) ->
          mk_constr "Open_box" [ mk_format fmt' str' ]
      and mk_format : type a b c d e f .
          (a, b, c, d, e, f) CamlinternalFormatBasics.fmt -> string ->
          Parsetree.expression = fun fmt str ->
        mk_constr "Format" [ mk_fmt fmt; mk_string str ]
      and mk_side side = match side with
        | Left  -> mk_constr "Left"  []
        | Right -> mk_constr "Right" []
        | Zeros -> mk_constr "Zeros" []
      and mk_iconv iconv = match iconv with
        | Int_d  -> mk_constr "Int_d"  [] | Int_pd -> mk_constr "Int_pd" []
        | Int_sd -> mk_constr "Int_sd" [] | Int_i  -> mk_constr "Int_i"  []
        | Int_pi -> mk_constr "Int_pi" [] | Int_si -> mk_constr "Int_si" []
        | Int_x  -> mk_constr "Int_x"  [] | Int_Cx -> mk_constr "Int_Cx" []
        | Int_X  -> mk_constr "Int_X"  [] | Int_CX -> mk_constr "Int_CX" []
        | Int_o  -> mk_constr "Int_o"  [] | Int_Co -> mk_constr "Int_Co" []
        | Int_u  -> mk_constr "Int_u"  [] | Int_Cd -> mk_constr "Int_Cd" []
        | Int_Ci -> mk_constr "Int_Ci" [] | Int_Cu -> mk_constr "Int_Cu" []
      and mk_fconv fconv =
        let flag = match fst fconv with
        | Float_flag_ -> mk_constr "Float_flag_" []
        | Float_flag_p -> mk_constr "Float_flag_p" []
        | Float_flag_s -> mk_constr "Float_flag_s" [] in
        let kind = match snd fconv with
        | Float_f  -> mk_constr "Float_f"  []
        | Float_e  -> mk_constr "Float_e"  []
        | Float_E  -> mk_constr "Float_E"  []
        | Float_g  -> mk_constr "Float_g"  []
        | Float_G  -> mk_constr "Float_G"  []
        | Float_h  -> mk_constr "Float_h"  []
        | Float_H  -> mk_constr "Float_H"  []
        | Float_F  -> mk_constr "Float_F"  []
        | Float_CF -> mk_constr "Float_CF" [] in
        mk_exp_loc (Pexp_tuple [flag; kind])
      and mk_counter cnt = match cnt with
        | Line_counter  -> mk_constr "Line_counter"  []
        | Char_counter  -> mk_constr "Char_counter"  []
        | Token_counter -> mk_constr "Token_counter" []
      and mk_int_opt n_opt = match n_opt with
        | None ->
          let lid_loc = mk_lid_loc (Longident.Lident "None") in
          mk_exp_loc (Pexp_construct (lid_loc, None))
        | Some n ->
          let lid_loc = mk_lid_loc (Longident.Lident "Some") in
          mk_exp_loc (Pexp_construct (lid_loc, Some (mk_int n)))
      and mk_fmtty : type a b c d e f g h i j k l .
          (a, b, c, d, e, f, g, h, i, j, k, l) fmtty_rel -> Parsetree.expression
          =
      fun fmtty -> match fmtty with
        | Char_ty rest      -> mk_constr "Char_ty"      [ mk_fmtty rest ]
        | String_ty rest    -> mk_constr "String_ty"    [ mk_fmtty rest ]
        | Int_ty rest       -> mk_constr "Int_ty"       [ mk_fmtty rest ]
        | Int32_ty rest     -> mk_constr "Int32_ty"     [ mk_fmtty rest ]
        | Nativeint_ty rest -> mk_constr "Nativeint_ty" [ mk_fmtty rest ]
        | Int64_ty rest     -> mk_constr "Int64_ty"     [ mk_fmtty rest ]
        | Float_ty rest     -> mk_constr "Float_ty"     [ mk_fmtty rest ]
        | Bool_ty rest      -> mk_constr "Bool_ty"      [ mk_fmtty rest ]
        | Alpha_ty rest     -> mk_constr "Alpha_ty"     [ mk_fmtty rest ]
        | Theta_ty rest     -> mk_constr "Theta_ty"     [ mk_fmtty rest ]
        | Any_ty rest       -> mk_constr "Any_ty"       [ mk_fmtty rest ]
        | Reader_ty rest    -> mk_constr "Reader_ty"    [ mk_fmtty rest ]
        | Ignored_reader_ty rest ->
          mk_constr "Ignored_reader_ty" [ mk_fmtty rest ]
        | Format_arg_ty (sub_fmtty, rest) ->
          mk_constr "Format_arg_ty" [ mk_fmtty sub_fmtty; mk_fmtty rest ]
        | Format_subst_ty (sub_fmtty1, sub_fmtty2, rest) ->
          mk_constr "Format_subst_ty"
            [ mk_fmtty sub_fmtty1; mk_fmtty sub_fmtty2; mk_fmtty rest ]
        | End_of_fmtty -> mk_constr "End_of_fmtty" []
      and mk_ignored : type a b c d e f .
          (a, b, c, d, e, f) ignored -> Parsetree.expression =
      fun ign -> match ign with
        | Ignored_char ->
          mk_constr "Ignored_char" []
        | Ignored_caml_char ->
          mk_constr "Ignored_caml_char" []
        | Ignored_string pad_opt ->
          mk_constr "Ignored_string" [ mk_int_opt pad_opt ]
        | Ignored_caml_string pad_opt ->
          mk_constr "Ignored_caml_string" [ mk_int_opt pad_opt ]
        | Ignored_int (iconv, pad_opt) ->
          mk_constr "Ignored_int" [ mk_iconv iconv; mk_int_opt pad_opt ]
        | Ignored_int32 (iconv, pad_opt) ->
          mk_constr "Ignored_int32" [ mk_iconv iconv; mk_int_opt pad_opt ]
        | Ignored_nativeint (iconv, pad_opt) ->
          mk_constr "Ignored_nativeint" [ mk_iconv iconv; mk_int_opt pad_opt ]
        | Ignored_int64 (iconv, pad_opt) ->
          mk_constr "Ignored_int64" [ mk_iconv iconv; mk_int_opt pad_opt ]
        | Ignored_float (pad_opt, prec_opt) ->
          mk_constr "Ignored_float" [ mk_int_opt pad_opt; mk_int_opt prec_opt ]
        | Ignored_bool pad_opt ->
          mk_constr "Ignored_bool" [ mk_int_opt pad_opt ]
        | Ignored_format_arg (pad_opt, fmtty) ->
          mk_constr "Ignored_format_arg" [ mk_int_opt pad_opt; mk_fmtty fmtty ]
        | Ignored_format_subst (pad_opt, fmtty) ->
          mk_constr "Ignored_format_subst" [
            mk_int_opt pad_opt; mk_fmtty fmtty ]
        | Ignored_reader ->
          mk_constr "Ignored_reader" []
        | Ignored_scan_char_set (width_opt, char_set) ->
          mk_constr "Ignored_scan_char_set" [
            mk_int_opt width_opt; mk_string char_set ]
        | Ignored_scan_get_counter counter ->
          mk_constr "Ignored_scan_get_counter" [
            mk_counter counter
          ]
        | Ignored_scan_next_char ->
          mk_constr "Ignored_scan_next_char" []
      and mk_padding : type x y . (x, y) padding -> Parsetree.expression =
      fun pad -> match pad with
        | No_padding         -> mk_constr "No_padding" []
        | Lit_padding (s, w) -> mk_constr "Lit_padding" [ mk_side s; mk_int w ]
        | Arg_padding s      -> mk_constr "Arg_padding" [ mk_side s ]
      and mk_precision : type x y . (x, y) precision -> Parsetree.expression =
      fun prec -> match prec with
        | No_precision    -> mk_constr "No_precision" []
        | Lit_precision w -> mk_constr "Lit_precision" [ mk_int w ]
        | Arg_precision   -> mk_constr "Arg_precision" []
      and mk_fmt : type a b c d e f .
          (a, b, c, d, e, f) fmt -> Parsetree.expression =
      fun fmt -> match fmt with
        | Char rest ->
          mk_constr "Char" [ mk_fmt rest ]
        | Caml_char rest ->
          mk_constr "Caml_char" [ mk_fmt rest ]
        | String (pad, rest) ->
          mk_constr "String" [ mk_padding pad; mk_fmt rest ]
        | Caml_string (pad, rest) ->
          mk_constr "Caml_string" [ mk_padding pad; mk_fmt rest ]
        | Int (iconv, pad, prec, rest) ->
          mk_constr "Int" [
            mk_iconv iconv; mk_padding pad; mk_precision prec; mk_fmt rest ]
        | Int32 (iconv, pad, prec, rest) ->
          mk_constr "Int32" [
            mk_iconv iconv; mk_padding pad; mk_precision prec; mk_fmt rest ]
        | Nativeint (iconv, pad, prec, rest) ->
          mk_constr "Nativeint" [
            mk_iconv iconv; mk_padding pad; mk_precision prec; mk_fmt rest ]
        | Int64 (iconv, pad, prec, rest) ->
          mk_constr "Int64" [
            mk_iconv iconv; mk_padding pad; mk_precision prec; mk_fmt rest ]
        | Float (fconv, pad, prec, rest) ->
          mk_constr "Float" [
            mk_fconv fconv; mk_padding pad; mk_precision prec; mk_fmt rest ]
        | Bool (pad, rest) ->
          mk_constr "Bool" [ mk_padding pad; mk_fmt rest ]
        | Flush rest ->
          mk_constr "Flush" [ mk_fmt rest ]
        | String_literal (s, rest) ->
          mk_constr "String_literal" [ mk_string s; mk_fmt rest ]
        | Char_literal (c, rest) ->
          mk_constr "Char_literal" [ mk_char c; mk_fmt rest ]
        | Format_arg (pad_opt, fmtty, rest) ->
          mk_constr "Format_arg" [
            mk_int_opt pad_opt; mk_fmtty fmtty; mk_fmt rest ]
        | Format_subst (pad_opt, fmtty, rest) ->
          mk_constr "Format_subst" [
            mk_int_opt pad_opt; mk_fmtty fmtty; mk_fmt rest ]
        | Alpha rest ->
          mk_constr "Alpha" [ mk_fmt rest ]
        | Theta rest ->
          mk_constr "Theta" [ mk_fmt rest ]
        | Formatting_lit (fmting, rest) ->
          mk_constr "Formatting_lit" [ mk_formatting_lit fmting; mk_fmt rest ]
        | Formatting_gen (fmting, rest) ->
          mk_constr "Formatting_gen" [ mk_formatting_gen fmting; mk_fmt rest ]
        | Reader rest ->
          mk_constr "Reader" [ mk_fmt rest ]
        | Scan_char_set (width_opt, char_set, rest) ->
          mk_constr "Scan_char_set" [
            mk_int_opt width_opt; mk_string char_set; mk_fmt rest ]
        | Scan_get_counter (cnt, rest) ->
          mk_constr "Scan_get_counter" [ mk_counter cnt; mk_fmt rest ]
        | Scan_next_char rest ->
          mk_constr "Scan_next_char" [ mk_fmt rest ]
        | Ignored_param (ign, rest) ->
          mk_constr "Ignored_param" [ mk_ignored ign; mk_fmt rest ]
        | End_of_format ->
          mk_constr "End_of_format" []
        | Custom _ ->
          (* Custom formatters have no syntax so they will never appear
             in formats parsed from strings. *)
          assert false
      in
      let legacy_behavior = not !Clflags.strict_formats in
      let Fmt_EBB fmt = fmt_ebb_of_string ~legacy_behavior str in
      mk_constr "Format" [ mk_fmt fmt; mk_string str ]
    ))
  with Failure msg ->
    raise (Error (loc, env, Invalid_format msg))

and type_option_some env expected_mode sarg ty ty0 =
  let ty' = extract_option_type env ty in
  let ty0' = extract_option_type env ty0 in
  let alloc_mode, argument_mode = register_allocation expected_mode in
  let arg = type_argument env argument_mode sarg ty' ty0' in
  let lid = Longident.Lident "Some" in
  let csome = Env.find_ident_constructor Predef.ident_some env in
  mkexp (Texp_construct(mknoloc lid , csome, [arg], Some alloc_mode))
    (type_option arg.exp_type) arg.exp_loc arg.exp_env

(* [expected_mode] is the expected mode of the field. It's already adjusted for
   allocation, mutation and modalities. *)
and type_label_exp create env (arg_mode : expected_mode) loc ty_expected
          (lid, label, sarg) =
  (* Here also ty_expected may be at generic_level *)
  let separate = !Clflags.principal || Env.has_local_constraints env in
  (* #4682: we try two type-checking approaches for [arg] using backtracking:
     - first try: we try with [ty_arg] as expected type;
     - second try; if that fails, we backtrack and try without
  *)
  let (vars, ty_arg, snap, arg) =
    (* try the first approach *)
    with_local_level begin fun () ->
      let (vars, ty_arg) =
        with_local_level_iter_if separate begin fun () ->
          let (vars, ty_arg, ty_res) =
            with_local_level_iter_if separate ~post:generalize_structure
              begin fun () ->
                let ((_, ty_arg, ty_res) as r) = instance_label true label in
                (r, [ty_arg; ty_res])
              end
          in
          begin try
            unify env (instance ty_res) (instance ty_expected)
          with Unify err ->
            raise (Error(lid.loc, env, Label_mismatch(lid.txt, err)))
          end;
          (* Instantiate so that we can generalize internal nodes *)
          let ty_arg = instance ty_arg in
          ((vars, ty_arg), [ty_arg])
        end
        ~post:generalize_structure
      in
      if label.lbl_private = Private then
        if create then
          raise (Error(loc, env, Private_type ty_expected))
        else
          raise (Error(lid.loc, env, Private_label(lid.txt, ty_expected)));
      let snap = if vars = [] then None else Some (Btype.snapshot ()) in
      let arg = type_argument env arg_mode sarg ty_arg (instance ty_arg) in
      (vars, ty_arg, snap, arg)
    end
    (* Note: there is no generalization logic here as could be expected,
       because it is part of the backtracking logic below. *)
  in
  let arg =
    try
      if (vars = []) then arg
      else begin
        (* We detect if the first try failed here,
           during generalization. *)
        if maybe_expansive arg then
          lower_contravariant env arg.exp_type;
        generalize_and_check_univars env "field value" arg label.lbl_arg vars;
        {arg with exp_type = instance arg.exp_type}
      end
    with first_try_exn when maybe_expansive arg -> try
      (* backtrack and try the second approach *)
      Option.iter Btype.backtrack snap;
      let arg = with_local_level (fun () -> type_exp env arg_mode sarg)
          ~post:(fun arg -> lower_contravariant env arg.exp_type)
      in
      let arg =
        with_local_level begin fun () ->
          let arg = {arg with exp_type = instance arg.exp_type} in
          unify_exp env arg (instance ty_arg);
          arg
        end
        ~post: begin fun arg ->
          generalize_and_check_univars env "field value" arg label.lbl_arg vars
        end
      in
      {arg with exp_type = instance arg.exp_type}
    with Error (_, _, Less_general _) as e -> raise e
    | _ -> raise first_try_exn
  in
  (lid, label, arg)

and type_argument ?explanation ?recarg env (mode : expected_mode) sarg
      ty_expected' ty_expected =
  (* ty_expected' may be generic *)
  let no_labels ty =
    let ls, tvar = list_labels env ty in
    not tvar && List.for_all ((=) Nolabel) ls
  in
  let inferred = is_inferred sarg in
  let rec loosen_arrow_modes ty' ty =
    let expty = expand_head env ty in
    let expty' = expand_head env ty' in
    let lv = get_level expty in
    let lv' = get_level expty' in
    match get_desc expty', get_desc expty with
    | Tarrow((l, marg, mret), ty_arg', ty_res', _),
      Tarrow(_, ty_arg,  ty_res,  _)
      when lv' = generic_level || not !Clflags.principal ->
      let ty_res', ty_res, changed = loosen_arrow_modes ty_res' ty_res in
      let {comonadic; monadic} = mret in
      let comonadic, changed' = Alloc.Comonadic.newvar_below comonadic in
      let mret = {comonadic; monadic} in
      let marg, changed'' = Alloc.newvar_above marg in
      if changed || changed' || changed'' then
        newty2 ~level:lv' (Tarrow((l, marg, mret), ty_arg', ty_res', commu_ok)),
        newty2 ~level:lv  (Tarrow((l, marg, mret), ty_arg,  ty_res,  commu_ok)),
        true
      else
        ty', ty, false
    | _ ->
      ty', ty, false
  in
  let ty_expected', ty_expected =
    if inferred then
      (* Do not expand local constraints unnecessarily (PR#10277) *)
      let snap =
        if Env.has_local_constraints env
        then Some (Btype.snapshot ())
        else None
      in
      let t', t, changed = loosen_arrow_modes ty_expected' ty_expected in
      if not changed then Option.iter Btype.backtrack snap;
      t', t
    else
      ty_expected', ty_expected
  in
  let may_coerce =
    if not inferred then None else
    let work () =
      let te = expand_head env ty_expected' in
      match get_desc te with
        Tarrow((Nolabel,_,_),_,ty_res0,_) ->
          Some (no_labels ty_res0, get_level te)
      | _ -> None
    in
    (* Need to be careful not to expand local constraints here *)
    if Env.has_local_constraints env then
      let snap = Btype.snapshot () in
      try_finally ~always:(fun () -> Btype.backtrack snap) work
    else work ()
  in
  match may_coerce with
    Some (safe_expect, lv) ->
      (* apply omittable arguments when expected type is "" *)
      (* we must be very careful about not breaking the semantics *)
      let exp_mode, _ = Value.newvar_below mode.mode in
      let texp =
        with_local_level_if_principal ~post:generalize_structure_exp
          (fun () -> type_exp env {mode with mode = Value.disallow_left exp_mode} sarg)
      in
      let rec make_args args ty_fun =
        match get_desc (expand_head env ty_fun) with
        | Tarrow ((l,_marg,_mret),ty_arg,ty_fun,_) when is_optional l ->
            let ty =
              type_option_none env (instance (tpoly_get_mono ty_arg))
                sarg.pexp_loc
            in
            (* CR layouts v5: change value assumption below when we allow
               non-values in structures. *)
            make_args ((l, Arg (ty, Jkind.Type.Sort.value)) :: args) ty_fun
        | Tarrow ((l,_marg,_mret),_,ty_fun,_) when is_position l ->
            let arg = src_pos (Location.ghostify sarg.pexp_loc) [] env in
            make_args ((l, Arg (arg, Jkind.Type.Sort.value)) :: args) ty_fun
        | Tarrow ((l,_,_),_,ty_res',_) when l = Nolabel || !Clflags.classic ->
            List.rev args, ty_fun, no_labels ty_res'
        | Tvar _ ->  List.rev args, ty_fun, false
        |  _ -> [], texp.exp_type, false
      in
      (* If make_args ends in Tvar, then simple_res is false, no_labels *)
      let args, ty_fun', simple_res = make_args [] texp.exp_type
      and texp = {texp with exp_type = instance texp.exp_type} in
      if not (simple_res || safe_expect) then begin
        unify_exp env texp ty_expected;
        texp
      end else begin
      let warn = !Clflags.principal &&
        (lv <> generic_level || get_level ty_fun' <> generic_level)
      and ty_fun = instance ty_fun' in
      let marg, ty_arg, mret, ty_res =
        match get_desc (expand_head env ty_expected) with
          Tarrow((Nolabel,marg,mret),ty_arg,ty_res,_) ->
           marg, ty_arg, mret, ty_res
        | _ -> assert false
      in
      unify_exp env {texp with exp_type = ty_fun} ty_expected;
      if args = [] then texp else begin
      let alloc_mode, mode_subcomponent = register_allocation mode in
      submode ~loc:sarg.pexp_loc ~env ~reason:Other
        exp_mode mode_subcomponent;
      (* eta-expand to avoid side effects *)
      let var_pair ~(mode : Value.lr) name ty =
        let id = Ident.create_local name in
        let desc =
          { val_type = ty; val_kind = Val_reg;
            val_attributes = [];
            val_zero_alloc = Builtin_attributes.Default_zero_alloc;
            val_modalities = Modality.Value.id;
            val_loc = Location.none;
            val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
          }
        in
        let exp_env = Env.add_value ~mode id desc env in
        let uu = unique_use ~loc:sarg.pexp_loc ~env mode mode in
        {pat_desc = Tpat_var (id, mknoloc name, desc.val_uid, Value.disallow_right mode);
         pat_type = ty;
         pat_extra=[];
         pat_attributes = [];
         pat_loc = Location.none; pat_env = env},
        {exp_type = ty; exp_loc = Location.none; exp_env = exp_env;
         exp_extra = []; exp_attributes = [];
         exp_desc =
         Texp_ident(Path.Pident id, mknoloc (Longident.Lident name),
                    desc, Id_value, uu)}
      in
      let eta_mode, _ = Value.newvar_below (alloc_as_value marg) in
      Regionality.submode_exn
        (Value.proj (Comonadic Areality) eta_mode) Regionality.regional;
      let eta_pat, eta_var = var_pair ~mode:eta_mode "eta" ty_arg in
      (* CR layouts v10: When we add abstract jkinds, the eta expansion here
         becomes impossible in some cases - we'll need better errors.  For test
         cases, look toward the end of
         typing-layouts-missing-cmi/function_arg.ml *)
      let type_sort ~why ty =
        match type_sort ~why env ty with
        | Ok sort -> sort
        | Error err ->
          raise(Error(sarg.pexp_loc, env, Function_type_not_rep (ty, err)))
      in
      let arg_sort = type_sort ~why:Function_argument ty_arg in
      let ret_sort = type_sort ~why:Function_result ty_res in
      let func texp =
        let ret_mode = alloc_as_value mret in
        let e =
          {texp with exp_type = ty_res; exp_desc =
           Texp_apply
             (texp,
              args @ [Nolabel, Arg (eta_var, arg_sort)], Nontail,
              ret_mode
              |> Value.proj (Comonadic Areality)
              |> regional_to_global
              |> Locality.disallow_right,
              Zero_alloc_utils.Assume_info.none)}
        in
        let cases = [ case eta_pat e ] in
        let cases_loc = { texp.exp_loc with loc_ghost = true } in
        let param = name_cases "param" cases in
        { texp with exp_type = ty_fun; exp_desc =
          Texp_function
            { params = [];
              body =
                Tfunction_cases
                  { fc_cases = cases; fc_partial = Total; fc_param = param;
                    fc_env = env; fc_ret_type = ty_res;
                    fc_loc = cases_loc; fc_exp_extra = None;
                    fc_attributes = []; fc_arg_mode = Alloc.disallow_right marg;
                    fc_arg_sort = arg_sort;
                  };
              ret_mode = Alloc.disallow_right mret;
              ret_sort;
              alloc_mode;
              region = false;
              zero_alloc = Default_zero_alloc
            }
        }
      in
      Location.prerr_warning texp.exp_loc
        (Warnings.Eliminated_optional_arguments
           (List.map (fun (l, _) -> Printtyp.string_of_label l) args));
      if warn then Location.prerr_warning texp.exp_loc
          (Warnings.Non_principal_labels "eliminated omittable argument");
      (* let-expand to have side effects *)
      let let_pat, let_var = var_pair ~mode:exp_mode "arg" texp.exp_type in
      re { texp with exp_type = ty_fun;
             exp_desc =
               Texp_let (Nonrecursive,
                         [{vb_pat=let_pat; vb_expr=texp; vb_sort=arg_sort;
                           vb_attributes=[]; vb_loc=Location.none;
                           vb_rec_kind = Dynamic;
                          }],
                         func let_var) }
      end
      end
  | None ->
      let mode = expect_mode_cross env ty_expected' mode in
      let texp = type_expect ?recarg env mode sarg
        (mk_expected ?explanation ty_expected') in
      unify_exp env texp ty_expected;
      texp

and type_apply_arg env ~app_loc ~funct ~index ~position_and_mode ~partial_app (lbl, arg) =
  match arg with
  | Arg (Unknown_arg { sarg; ty_arg_mono; mode_arg; sort_arg }) ->
      let expected_mode, mode_arg =
        mode_argument ~funct ~index ~position_and_mode ~partial_app mode_arg in
      let arg =
        type_expect env expected_mode sarg (mk_expected ty_arg_mono)
      in
      if is_optional lbl then
        (* CR layouts v5: relax value requirement *)
        unify_exp env arg
          (type_option(newvar Predef.option_argument_jkind));
      (lbl, Arg (arg, mode_arg, sort_arg))
  | Arg (Known_arg { sarg; ty_arg; ty_arg0;
                     mode_arg; wrapped_in_some; sort_arg }) ->
      let expected_mode, mode_arg =
        mode_argument ~funct ~index ~position_and_mode ~partial_app mode_arg in
      let ty_arg', vars = tpoly_get_poly ty_arg in
      let arg =
        if vars = [] then begin
          let ty_arg0' = tpoly_get_mono ty_arg0 in
          if wrapped_in_some then begin
            type_option_some env expected_mode sarg ty_arg' ty_arg0'
          end else begin
            type_argument env expected_mode sarg ty_arg' ty_arg0'
          end
        end else begin
          if !Clflags.principal
             && get_level ty_arg < Btype.generic_level then begin
            let snap = Btype.snapshot () in
            let really_poly =
              try
                unify env (newmono (newvar (Jkind.Primitive.top ~why:Dummy_jkind)))
                  ty_arg;
                false
              with Unify _ -> true
            in
            Btype.backtrack snap;
            if really_poly then
              Location.prerr_warning app_loc
                (Warnings.Not_principal "applying a higher-rank function here");
          end;
          let separate =
            !Clflags.principal || Env.has_local_constraints env
          in
          let arg, _, _ =
            with_local_level begin fun () ->
              let vars, ty_arg' =
                with_local_level_if separate begin fun () ->
                  instance_poly false vars ty_arg'
                end ~post:(fun (_, ty_arg') -> generalize_structure ty_arg')
              in
              let (ty_arg0', vars0) = tpoly_get_poly ty_arg0 in
              let vars0, ty_arg0' = instance_poly false vars0 ty_arg0' in
              List.iter2 (fun ty ty' -> unify_var env ty ty') vars vars0;
              let arg =
                type_argument env expected_mode sarg ty_arg' ty_arg0'
              in
              arg, ty_arg, vars
            end
            ~post:(fun (arg, ty_arg, vars) ->
              if maybe_expansive arg then
                lower_contravariant env arg.exp_type;
              generalize_and_check_univars env "argument" arg ty_arg vars);
          in
          {arg with exp_type = instance arg.exp_type}
        end
      in
      (lbl, Arg (arg, mode_arg, sort_arg))
  | Arg (Eliminated_optional_arg { ty_arg; sort_arg; expected_label; _ }) ->
      (match expected_label with
      | Optional _ ->
          let arg = type_option_none env (instance ty_arg) Location.none in
          (lbl, Arg (arg, Mode.Value.legacy, sort_arg))
      | Position _ ->
          let arg = src_pos (Location.ghostify app_loc) [] env in
          (lbl, Arg (arg, Mode.Value.legacy, sort_arg))
      | Labelled _ | Nolabel -> assert false)
  | Omitted _ as arg -> (lbl, arg)

and type_application env app_loc expected_mode position_and_mode
      funct funct_mode sargs ret_tvar =
  let is_ignore funct =
    is_prim ~name:"%ignore" funct &&
    (try ignore (filter_arrow_mono env (instance funct.exp_type) Nolabel); true
     with Filter_arrow_mono_failed -> false)
  in
  match sargs with
  | (* Special case for ignore: avoid discarding warning *)
    [Parsetree.Nolabel, sarg] when is_ignore funct ->
      let {ty_arg; arg_mode; ty_ret; ret_mode} =
        with_local_level_if_principal (fun () ->
          filter_arrow_mono env (instance funct.exp_type) Nolabel
        ) ~post:(fun {ty_ret; _} -> generalize_structure ty_ret)
      in
      let type_sort ~why ty =
        match Ctype.type_sort ~why env ty with
        | Ok sort -> sort
        | Error err -> raise (Error (app_loc, env, Function_type_not_rep (ty, err)))
      in
      let arg_sort = type_sort ~why:Function_argument ty_arg in
      let ap_mode = Locality.disallow_right (Alloc.proj (Comonadic Areality) ret_mode) in
      let mode_res =
        mode_cross_left env ty_ret (alloc_as_value ret_mode)
      in
      submode ~loc:app_loc ~env ~reason:Other
        mode_res expected_mode;
      let arg_mode, _ =
        mode_argument ~funct ~index:0 ~position_and_mode
          ~partial_app:false arg_mode
      in
      let exp = type_expect env arg_mode sarg (mk_expected ty_arg) in
      check_partial_application ~statement:false exp;
      ([Nolabel, Arg (exp, arg_sort)], ty_ret, ap_mode, position_and_mode)
  | _ ->
      let ty = funct.exp_type in
      let ignore_labels =
        !Clflags.classic ||
        begin
          let ls, tvar = list_labels env funct.exp_type in
          not tvar &&
          let labels = List.filter (fun l -> not (is_omittable l)) ls in
          List.length labels = List.length sargs &&
          List.for_all (fun (l,_) -> l = Parsetree.Nolabel) sargs &&
          List.exists (fun l -> l <> Nolabel) labels &&
          (Location.prerr_warning
             funct.exp_loc
             (Warnings.Labels_omitted
                (List.map Printtyp.string_of_label
                          (List.filter ((<>) Nolabel) labels)));
           true)
        end
      in
      let ty_ret, mode_ret, args, position_and_mode =
        with_local_level_if_principal begin fun () ->
          let sargs = List.map
            (* Application will never contain Position labels, so no need to pass
               argument type here. When checking against the function type,
               Labelled arguments will be matched up to Position parameters
               based on label names *)
            (fun (label, e) -> Typetexp.transl_label label None, e) sargs
          in
          let ty_ret, mode_ret, untyped_args =
            collect_apply_args env funct ignore_labels ty (instance ty)
              (value_to_alloc_r2l funct_mode) sargs ret_tvar
          in
          let partial_app = is_partial_apply untyped_args in
          let position_and_mode =
            if partial_app then position_and_mode_default else position_and_mode
          in
          let args =
            List.mapi (fun index arg ->
                type_apply_arg env ~app_loc ~funct ~index
                  ~position_and_mode ~partial_app arg)
              untyped_args
          in
          let ty_ret, mode_ret, args =
            type_omitted_parameters expected_mode env ty_ret mode_ret args
          in
          check_local_application_complete ~env ~app_loc untyped_args;
          ty_ret, mode_ret, args, position_and_mode
        end ~post:(fun (ty_ret, _, _, _) -> generalize_structure ty_ret)
      in
      let ap_mode = Locality.disallow_right (Alloc.proj (Comonadic Areality) mode_ret) in
      let mode_ret =
        mode_cross_left env ty_ret (alloc_as_value mode_ret)
      in
      submode ~loc:app_loc ~env ~reason:(Application ty_ret)
        mode_ret expected_mode;

      check_tail_call_local_returning app_loc env ap_mode position_and_mode;
      args, ty_ret, ap_mode, position_and_mode

and type_tuple ~loc ~env ~(expected_mode : expected_mode) ~ty_expected
    ~explanation ~attributes sexpl =
  let arity = List.length sexpl in
  assert (arity >= 2);
  let alloc_mode, argument_mode = register_allocation_value_mode expected_mode.mode in
  (* CR layouts v5: non-values in tuples *)
  let labeled_subtypes =
    List.map (fun (label, _) -> label,
                                newgenvar (Jkind.Type.Primitive.value ~why:Tuple_element |> Jkind.of_type_jkind))
    sexpl
  in
  let to_unify = newgenty (Ttuple labeled_subtypes) in
  with_explanation explanation (fun () ->
    unify_exp_types loc env to_unify (generic_instance ty_expected));
  let argument_modes =
    if List.compare_length_with expected_mode.tuple_modes arity = 0 then
      expected_mode.tuple_modes
    else List.init arity (fun _ -> argument_mode)
  in
  let types_and_modes = List.combine labeled_subtypes argument_modes in
  let expl =
    List.map2
      (fun (label, body) ((_, ty), argument_mode) ->
        let argument_mode = mode_default argument_mode in
        let argument_mode = expect_mode_cross env ty argument_mode in
          (label, type_expect env argument_mode body (mk_expected ty)))
      sexpl types_and_modes
  in
  re {
    exp_desc = Texp_tuple (expl, alloc_mode);
    exp_loc = loc; exp_extra = [];
    (* Keep sharing *)
    exp_type = newty (Ttuple (List.map (fun (label, e) -> label, e.exp_type) expl));
    exp_attributes = attributes;
    exp_env = env }

and type_construct env (expected_mode : expected_mode) loc lid sarg
      ty_expected_explained attrs =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let expected_type =
    match extract_concrete_variant env ty_expected with
    | Variant_type(p0, p,_) ->
        Some(p0, p, is_principal ty_expected)
    | Maybe_a_variant_type -> None
    | Not_a_variant_type ->
        let srt = wrong_kind_sort_of_constructor lid.txt in
        let ctx = Expression explanation in
        let error = Wrong_expected_kind(srt, ctx, ty_expected) in
        raise (Error (loc, env, error))
  in
  let constrs =
    Env.lookup_all_constructors ~loc:lid.loc Env.Positive lid.txt env
  in
  let constr =
    wrap_disambiguate "This variant expression is expected to have"
      ty_expected_explained
      (Constructor.disambiguate Env.Positive lid env expected_type) constrs
  in
  let sargs =
    match sarg with
    | None -> []
    | Some se -> begin
        match Jane_syntax.Expression.of_ast se with
        | Some (Jexp_tuple (_ : _ list), _) when
            constr.cstr_arity > 1 || Builtin_attributes.explicit_arity attrs ->
          raise(Error(loc, env, Constructor_labeled_arg))
        | Some (( Jexp_tuple _
                | Jexp_comprehension _
                | Jexp_immutable_array _
                | Jexp_n_ary_function _
                | Jexp_layout _
                | Jexp_modes _ ), _) -> [se]
        | None -> match se.pexp_desc with
        | Pexp_tuple sel when
            constr.cstr_arity > 1 || Builtin_attributes.explicit_arity attrs
          -> sel
        | _ -> [se]
      end
  in
  if List.length sargs <> constr.cstr_arity then
    raise(Error(loc, env, Constructor_arity_mismatch
                            (lid.txt, constr.cstr_arity, List.length sargs)));
  let separate = !Clflags.principal || Env.has_local_constraints env in
  let ty_args, ty_res, texp =
    with_local_level_if separate begin fun () ->
      let ty_args, ty_res, texp =
        with_local_level_if separate begin fun () ->
          let (ty_args, ty_res, _) =
            instance_constructor Keep_existentials_flexible constr
          in
          let texp =
            re {
            exp_desc = Texp_construct(lid, constr, [], None);
            exp_loc = loc; exp_extra = [];
            exp_type = ty_res;
            exp_attributes = attrs;
            exp_env = env } in
          (ty_args, ty_res, texp)
        end
        ~post: begin fun (_, ty_res, texp) ->
          generalize_structure ty_res;
          with_explanation explanation (fun () ->
            unify_exp env {texp with exp_type = instance ty_res}
              (instance ty_expected));
        end
      in
      (ty_args, ty_res, texp)
    end
      ~post:(fun (ty_args, ty_res, _) ->
        generalize_structure ty_res;
        List.iter (fun {Types.ca_type=ty; _} -> generalize_structure ty) ty_args)
  in
  let ty_args0, ty_res =
    match instance_list (ty_res :: (List.map (fun ca -> ca.Types.ca_type) ty_args)) with
      t :: tl -> tl, t
    | _ -> assert false
  in
  let texp = {texp with exp_type = ty_res} in
  if not separate then unify_exp env texp (instance ty_expected);
  let recarg =
    match constr.cstr_inlined with
    | None -> Rejected
    | Some _ ->
      begin match sargs with
      | [{pexp_desc =
            Pexp_ident _ |
            Pexp_record (_, (Some {pexp_desc = Pexp_ident _}| None))}] ->
        Required
      | _ ->
        raise (Error(loc, env, Inlined_record_expected))
      end
  in
  let (argument_mode, alloc_mode) =
    match constr.cstr_repr with
    | Variant_unboxed -> expected_mode, None
    | Variant_boxed _ when constr.cstr_constant -> expected_mode, None
    | Variant_boxed _ | Variant_extensible ->
       let alloc_mode, argument_mode = register_allocation expected_mode in
       argument_mode, Some alloc_mode
  in
  let args =
    List.map2
      (fun e ({Types.ca_type=ty; ca_modalities=gf; _},t0) ->
         let argument_mode = mode_modality gf argument_mode in
         type_argument ~recarg env argument_mode e ty t0)
      sargs (List.combine ty_args ty_args0)
  in
  if constr.cstr_private = Private then
    begin match constr.cstr_repr with
    | Variant_extensible ->
        raise(Error(loc, env, Private_constructor (constr, ty_res)))
    | Variant_boxed _ | Variant_unboxed ->
        raise (Error(loc, env, Private_type ty_res));
    end;
  (* NOTE: shouldn't we call "re" on this final expression? -- AF *)
  { texp with
    exp_desc = Texp_construct(lid, constr, args, alloc_mode) }

(* Typing of statements (expressions whose values are discarded) *)

and type_statement ?explanation ?(position=RNontail) env sexp =
  (* Raise the current level to detect non-returning functions *)
  let exp =
    with_local_level
      (fun () -> type_exp env (mode_max_with_position position) sexp)
  in
  let ty = expand_head env exp.exp_type in
  if is_Tvar ty && get_level ty > get_current_level () then
    Location.prerr_warning
      (final_subexpression exp).exp_loc
      Warnings.Nonreturning_statement;
  if !Clflags.strict_sequence then
    (* CR layouts v5: when we have unboxed unit, allow it for -strict-sequence *)
    let expected_ty = instance Predef.type_unit in
    with_explanation explanation (fun () ->
      unify_exp env exp expected_ty);
    exp, Jkind.Type.Sort.value
  else begin
    (* We're requiring the statement to have a representable jkind.  But that
       doesn't actually rule out things like "assert false"---we'll just end up
       getting a sort variable for its jkind. *)
    (* CR layouts v10: Abstract jkinds will introduce cases where we really have
       [any] and can't get a sort here. *)
    let tv, sort = new_rep_var ~why:Statement () in
    check_partial_application ~statement:true exp;
    with_explanation explanation (fun () ->
      try unify_var env ty tv
      with Unify err ->
        raise(Error(exp.exp_loc, env,
          Expr_type_clash(err, None, Some sexp.pexp_desc))));
    exp, sort
  end

(* Most of the arguments are the same as [type_cases].

   Takes a callback which is responsible for typing the body of the case.
   The arguments are documented inline in the type signature.

   It takes a callback rather than returning the half-typed cases directly
   because the typing of the body must take place at an increased level.

   The overall function returns:
     - The data returned by the callback
     - Whether the cases' patterns are partial or total
*)
and map_half_typed_cases
  : type k ret case_data.
    ?additional_checks_for_split_cases:((_ * ret) list -> unit)
    -> k pattern_category -> _ -> _ -> _ -> _ -> _
    -> (untyped_case * case_data) list
    -> type_body:(
        case_data
        -> k general_pattern (* the typed pattern *)
        -> ext_env:_ (* environment with module variables / pattern variables *)
        -> ty_expected:_ (* type to check body in scope of *)
        -> ty_infer:_ (* type to infer for the body *)
        -> contains_gadt:_ (* whether the pattern contains a GADT *)
        -> ret)
    -> check_if_total:bool
    -> ret list * partial
  = fun ?additional_checks_for_split_cases
    category env pat_mode
    ty_arg ty_res loc caselist ~type_body ~check_if_total ->
  let patterns = List.map (fun ((x : untyped_case), _) -> x.pattern) caselist in
  let contains_polyvars = List.exists contains_polymorphic_variant patterns in
  let erase_either = contains_polyvars && contains_variant_either ty_arg in
  let may_contain_gadts = List.exists may_contain_gadts patterns in
  let may_contain_modules = List.exists may_contain_modules patterns in
  let create_inner_level = may_contain_gadts || may_contain_modules in
  let ty_arg =
    if (may_contain_gadts || erase_either) && not !Clflags.principal
    then correct_levels ty_arg else ty_arg
  in
  let rec is_var spat =
    match spat.ppat_desc with
      Ppat_any | Ppat_var _ -> true
    | Ppat_alias (spat, _) -> is_var spat
    | _ -> false in
  let needs_exhaust_check =
    match caselist with
      [ ({ needs_refute = true }, _) ] -> true
    | [ ({ pattern }, _) ] when is_var pattern -> false
    | _ -> true
  in
  let outer_level = get_current_level () in
  with_local_level_iter_if create_inner_level begin fun () ->
  let lev = get_current_level () in
  let allow_modules =
    if may_contain_modules
    then
      (* The corresponding check for scope escape is done together with
         the check for GADT-induced existentials by
         [with_local_level_iter_if create_inner_level].
      *)
      Modules_allowed { scope = lev }
    else Modules_rejected
  in
  let take_partial_instance =
    if erase_either
    then Some false else None
  in
  let half_typed_cases, ty_res, do_copy_types, ty_arg' =
   (* propagation of the argument *)
    with_local_level begin fun () ->
      let pattern_force = ref [] in
      (*  Format.printf "@[%i %i@ %a@]@." lev (get_current_level())
          Printtyp.raw_type_expr ty_arg; *)
      let half_typed_cases =
        List.map
        (fun ({ Parmatch.pattern; _ } as untyped_case, case_data) ->
          let htc =
            with_local_level_if_principal begin fun () ->
              let ty_arg =
                (* propagation of pattern *)
                with_local_level ~post:generalize_structure
                  (fun () -> instance ?partial:take_partial_instance ty_arg)
              in
              let (pat, ext_env, force, pvs, mvs) =
                type_pattern category ~lev ~alloc_mode:pat_mode env pattern ty_arg
                allow_modules
              in
              pattern_force := force @ !pattern_force;
              { typed_pat = pat;
                pat_type_for_unif = ty_arg;
                untyped_case;
                case_data;
                branch_env = ext_env;
                pat_vars = pvs;
                module_vars = mvs;
                contains_gadt = contains_gadt (as_comp_pattern category pat); }
            end
            ~post: begin fun htc ->
              iter_pattern_variables_type generalize_structure htc.pat_vars;
            end
          in
          (* Ensure that no ambivalent pattern type escapes its branch *)
          check_scope_escape htc.typed_pat.pat_loc env outer_level
            htc.pat_type_for_unif;
          let pat = htc.typed_pat in
          {htc with typed_pat = { pat with pat_type = instance pat.pat_type }}
        )
        caselist in
      let patl =
        List.map (fun { typed_pat; _ } -> typed_pat) half_typed_cases in
      let does_contain_gadt =
        List.exists (fun { contains_gadt; _ } -> contains_gadt) half_typed_cases
      in
      let ty_res, do_copy_types =
        if does_contain_gadt && not !Clflags.principal then
          correct_levels ty_res, Env.make_copy_of_types env
        else ty_res, (fun env -> env)
      in
      (* Unify all cases (delayed to keep it order-free) *)
      let ty_arg' = newvar (Jkind.Primitive.top ~why:Dummy_jkind) in
      let unify_pats ty =
        List.iter (fun { typed_pat = pat; pat_type_for_unif = pat_ty; _ } ->
          unify_pat_types pat.pat_loc (ref env) pat_ty ty
        ) half_typed_cases
      in
      unify_pats ty_arg';
      (* Check for polymorphic variants to close *)
      if List.exists has_variants patl then begin
        Parmatch.pressure_variants_in_computation_pattern env
          (List.map (as_comp_pattern category) patl);
        List.iter finalize_variants patl
      end;
      (* `Contaminating' unifications start here *)
      List.iter (fun f -> f()) !pattern_force;
      (* Post-processing and generalization *)
      if take_partial_instance <> None then unify_pats (instance ty_arg);
      List.iter (fun { pat_vars; _ } ->
        iter_pattern_variables_type (enforce_current_level env) pat_vars
      ) half_typed_cases;
      (half_typed_cases, ty_res, do_copy_types, ty_arg')
    end
    ~post: begin fun (half_typed_cases, _, _, ty_arg') ->
      generalize ty_arg';
      List.iter (fun { pat_vars; _ } ->
        iter_pattern_variables_type generalize pat_vars
      ) half_typed_cases
    end
  in
  (* type bodies *)
  let ty_res' = instance ty_res in
  let result = with_local_level_if_principal ~post:ignore begin fun () ->
    List.map
      (fun { typed_pat = pat; branch_env = ext_env;
             pat_vars = pvs; module_vars = mvs;
             case_data; contains_gadt; _ }
        ->
        let ext_env =
          if contains_gadt then
            do_copy_types ext_env
          else
            ext_env
        in
        (* Before handing off the cases to the callback, first set up the the
           branch environments by adding the variables (and module variables)
           from the patterns.
        *)
        let ext_env =
          add_pattern_variables ext_env pvs
            ~check:(fun s -> Warnings.Unused_var_strict s)
            ~check_as:(fun s -> Warnings.Unused_var s)
        in
        let ext_env = add_module_variables ext_env mvs in
        let ty_expected =
          if contains_gadt && not !Clflags.principal then
            (* Take a generic copy of [ty_res] again to allow propagation of
               type information from preceding branches *)
            correct_levels ty_res
          else ty_res in
        type_body case_data pat ~ext_env ~ty_expected ~ty_infer:ty_res'
          ~contains_gadt)
      half_typed_cases
  end in
  let do_init = may_contain_gadts || needs_exhaust_check in
  let ty_arg_check =
    if do_init then
      (* Hack: use the [Subst] machinery to copy types, even though
         we don't intend on persisting the type to disk.

         We added a new [Duplicate_variables] variant specifically for this
         callsite, but it's still a bit of a hack (e.g. the type IDs produced
         for duplicated nodes are all negative, like they are in cmi files).
      *)
      Subst.type_expr
        (Subst.with_additional_action Duplicate_variables Subst.identity)
        ty_arg'
    else ty_arg'
  in
  (* Split the cases into val and exn cases so we can do the appropriate checks
     for exhaustivity and unused variables.
     The caller of this function can define custom checks. For some of these
     checks, the half-typed case doesn't provide enough info on its own -- for
     instance, the check for ambiguous bindings in when guards needs to know the
     case body's expression -- so the code pairs each case with its
     corresponding element in [result] before handing it off to the caller's
     custom checks.
  *)
  let val_cases_with_result, exn_cases_with_result =
    match category with
    | Value ->
        let val_cases =
          List.map2
            (fun htc res ->
               { htc.untyped_case with pattern = htc.typed_pat }, res)
            half_typed_cases
            result
        in
        (val_cases : (pattern Parmatch.parmatch_case * ret) list), []
    | Computation ->
        split_half_typed_cases env (List.combine half_typed_cases result)
  in
  let val_cases = List.map fst val_cases_with_result in
  let exn_cases = List.map fst exn_cases_with_result in
  if val_cases = [] && exn_cases <> [] then
    raise (Error (loc, env, No_value_clauses));
  let partial =
    if check_if_total then
      check_partial ~lev env ty_arg_check loc val_cases
    else
      Partial
  in
  let unused_check delayed =
    List.iter (fun { typed_pat; branch_env; _ } ->
      check_absent_variant branch_env (as_comp_pattern category typed_pat)
    ) half_typed_cases;
    with_level_if delayed ~level:lev begin fun () ->
      check_unused ~lev env ty_arg_check val_cases ;
      check_unused ~lev env Predef.type_exn exn_cases ;
    end;
  in
  if contains_polyvars then
    add_delayed_check (fun () -> unused_check true)
  else
    (* Check for unused cases, do not delay because of gadts *)
    unused_check false;
  begin
    match additional_checks_for_split_cases with
    | None -> ()
    | Some check ->
        check val_cases_with_result;
        check exn_cases_with_result;
  end;
  (result, partial), [ty_res']
  end
  (* Ensure that existential types do not escape *)
  ~post:(fun ty_res' -> enforce_current_level env ty_res')

(* Typing of match cases *)
and type_cases
    : type k . k pattern_category ->
           _ -> _ -> _ -> _ -> _ -> check_if_total:bool -> _ -> Parsetree.case list ->
           k case list * partial
  = fun category env pat_mode expr_mode
        ty_arg ty_res_explained ~check_if_total loc caselist ->
  let { ty = ty_res; explanation } = ty_res_explained in
  let caselist =
    List.map (fun case -> Parmatch.untyped_case case, case) caselist
  in
  (* Most of the work is done by [map_half_typed_cases]. All that's left
     is to typecheck the guards and the cases, and then to check for some
     warnings that can fire in the presence of guards.
  *)
  map_half_typed_cases category env pat_mode ty_arg ty_res loc caselist ~check_if_total
    ~type_body:begin
      fun { pc_guard; pc_rhs } pat ~ext_env ~ty_expected ~ty_infer
          ~contains_gadt:_ ->
        let guard =
          match pc_guard with
          | None -> None
          | Some scond ->
            Some
              (type_expect ext_env mode_max scond
                (mk_expected ~explanation:When_guard Predef.type_bool))
        in
        let exp =
          type_expect ext_env expr_mode pc_rhs (mk_expected ?explanation ty_expected)
        in
        {
          c_lhs = pat;
          c_guard = guard;
          c_rhs = {exp with exp_type = ty_infer}
        }
    end
    ~additional_checks_for_split_cases:(fun cases ->
      let cases =
        List.map
          (fun (case_with_pat, case) ->
             { case with c_lhs = case_with_pat.Parmatch.pattern }) cases
      in
      Parmatch.check_ambiguous_bindings cases)


(** A version of [type_expect], but that operates over function cases instead
    of expressions. The input type is like the [ty_expected] argument to
    [type_expect], and the returned type is like the [exp_type] of the
    expression returned by [type_expect].

    See [split_function_ty] for the meaning of [first] and [in_function].
*)
and type_function_cases_expect
    env expected_mode ty_expected loc cases attrs ~first ~in_function =
  Builtin_attributes.warning_scope attrs begin fun () ->
    let env,
        { filtered_arrow = { ty_arg; ty_ret; arg_mode; ret_mode };
          arg_sort; ret_sort;
          ty_arg_mono; expected_pat_mode; expected_inner_mode; alloc_mode;
        } =
      split_function_ty env expected_mode ty_expected loc ~arg_label:Nolabel
        ~in_function ~has_poly:false ~mode_annots:Mode.Alloc.Const.Option.none
        ~is_first_val_param:first ~is_final_val_param:true
    in
    let cases, partial =
      type_cases Value env
        expected_pat_mode expected_inner_mode ty_arg_mono (mk_expected ty_ret)
        ~check_if_total:true loc cases
    in
    let ty_fun =
      instance
        (newgenty
           (Tarrow ((Nolabel, arg_mode, ret_mode), ty_arg, ty_ret, commu_ok)))
    in
    unify_exp_types loc env ty_fun (instance ty_expected);
    let param = name_cases "param" cases in
    let cases =
      { fc_cases = cases;
        fc_partial = partial;
        fc_param = param;
        fc_loc = loc;
        fc_exp_extra = None;
        fc_env = env;
        fc_ret_type = ty_ret;
        fc_attributes = [];
        fc_arg_mode = Alloc.disallow_right arg_mode;
        fc_arg_sort = arg_sort;
      }
    in
    cases, ty_fun, alloc_mode,
      { ret_sort;
        ret_mode = Alloc.disallow_right ret_mode }
  end

(** Typecheck the body of a newtype. The "body" of a newtype may be:
    - an expression
    - a suffix of function parameters together with a function body
      That's why this function is polymorphic over the body.

      @param type_body A function that produces a type for the body given the
      environment. When typechecking an expression, this is [type_exp].
      @return The type returned by [type_body] but with the Tconstr
      nodes for the newtype properly linked, and the jkind annotation written
      by the user.
*)
and type_newtype
  : type a. _ -> _ -> _ -> (Env.t -> a * type_expr)
    -> a * type_expr * Jkind.annotation option =
  fun env name jkind_annot_opt type_body  ->
  let { txt = name; loc = name_loc } : _ Location.loc = name in
  let jkind, jkind_annot =
    Jkind.of_annotation_option_default ~context:(Newtype_declaration name)
      ~default:(Jkind.Type.Primitive.value ~why:Univar |> Jkind.of_type_jkind) jkind_annot_opt
  in
  let ty =
    if Typetexp.valid_tyvar_name name then
      newvar ~name jkind
    else
      newvar jkind
  in
  (* Use [with_local_level] just for scoping *)
  with_local_level begin fun () ->
    (* Create a fake abstract type declaration for name. *)
    let decl = new_local_type ~loc:name_loc jkind ~jkind_annot in
    let scope = create_scope () in
    let (id, new_env) = Env.enter_type ~scope name decl env in

    let result, exp_type = type_body new_env in
    (* Replace every instance of this type constructor in the resulting
       type. *)
    let seen = Hashtbl.create 8 in
    let rec replace t =
      if Hashtbl.mem seen (get_id t) then ()
      else begin
        Hashtbl.add seen (get_id t) ();
        match get_desc t with
        | Tconstr (Path.Pident id', _, _) when id == id' -> link_type t ty
        | _ -> Btype.iter_type_expr replace t
      end
    in
    let ety = Subst.type_expr Subst.identity exp_type in
    replace ety;
    (result, ety, jkind_annot)
  end

(** [type_newtype] where the "body" is just an expression. *)
and type_newtype_expr
    ~loc ~env ~expected_mode ~rue ~attributes name jkind_annot_opt sbody =
  let body, ety, jkind_annot =
    type_newtype env name jkind_annot_opt (fun env ->
      let expr = type_exp env expected_mode sbody in
      expr, expr.exp_type)
  in
  (* non-expansive if the body is non-expansive, so we don't introduce
     any new extra node in the typed AST. *)
  rue { body with exp_loc = loc; exp_type = ety;
        exp_extra =
        (Texp_newtype (name.txt, jkind_annot),
         loc, attributes) :: body.exp_extra }

(* Typing of let bindings *)

and type_let ?check ?check_strict ?(force_toplevel = false)
    existential_context env rec_flag spat_sexp_list allow_modules =
  let rec sexp_is_fun sexp =
    match Jane_syntax.Expression.of_ast sexp with
    | Some (jexp, _attrs) -> jexp_is_fun jexp
    | None      -> match sexp.pexp_desc with
    | Pexp_fun _ | Pexp_function _ -> true
    | Pexp_constraint (e, _)
    | Pexp_newtype (_, e) -> sexp_is_fun e
    | _ -> false
  and jexp_is_fun : Jane_syntax.Expression.t -> _ = function
    | Jexp_comprehension _
    | Jexp_immutable_array _
    | Jexp_layout (Lexp_constant _) -> false
    | Jexp_layout (Lexp_newtype (_, _, e)) -> sexp_is_fun e
    | Jexp_n_ary_function _ -> true
    | Jexp_tuple _ -> false
    | Jexp_modes (Coerce (_, e)) -> sexp_is_fun e
  in
  let vb_is_fun { pvb_expr = sexp; _ } = sexp_is_fun sexp in
  let entirely_functions = List.for_all vb_is_fun spat_sexp_list in
  let rec_mode_var =
    match rec_flag with
    | Recursive when entirely_functions -> Some (Value.newvar ())
    | Recursive -> Some Value.legacy
    | Nonrecursive -> None
  in
  let spatl = List.map vb_pat_constraint spat_sexp_list in
  let spatl = List.map (pat_modes ~force_toplevel rec_mode_var) spatl in
  let attrs_list = List.map (fun (attrs, _, _, _) -> attrs) spatl in
  let is_recursive = (rec_flag = Recursive) in

  let (pat_list, exp_list, new_env, mvs, sorts, _pvs) =
    with_local_level begin fun () ->
      if existential_context = At_toplevel then Typetexp.TyVarEnv.reset ();
      let (pat_list, new_env, force, pvs, mvs), sorts =
        with_local_level_if_principal begin fun () ->
          let nvs, sorts =
            List.split (List.map (fun _ -> new_rep_var ~why:Let_binding ())
                          spatl)
          in
          let (pat_list, _new_env, _force, _pvs, _mvs as res) =
            with_local_level_if is_recursive (fun () ->
              type_pattern_list Value existential_context env spatl nvs
                allow_modules
            ) ~post:(fun (_, _, _, pvs, _) ->
                       iter_pattern_variables_type generalize pvs)
          in
          (* If recursive, first unify with an approximation of the
             expression *)
          if is_recursive then
            List.iter2
              (fun (_, pat) binding ->
                let pat =
                  match get_desc pat.pat_type with
                  | Tpoly (ty, tl) ->
                      {pat with pat_type =
                       snd (instance_poly ~keep_names:true false tl ty)}
                  | _ -> pat
                in
                let bound_expr = vb_exp_constraint binding in
                type_approx env bound_expr pat.pat_type)
              pat_list spat_sexp_list;
          (* Polymorphic variant processing *)
          List.iter
            (fun (_, pat) ->
              if has_variants pat then begin
                Parmatch.pressure_variants env [pat];
                finalize_variants pat
              end)
            pat_list;
          res, sorts
        end
        ~post: begin fun ((pat_list, _, _, pvs, _), _) ->
          (* Generalize the structure *)
          iter_pattern_variables_type generalize_structure pvs;
          List.iter (fun (_, pat) -> generalize_structure pat.pat_type) pat_list
        end
      in
      (* Note [add_module_variables after checking expressions]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         Don't call [add_module_variables] here, because its use of
         [type_module] will fail until after we have type-checked the expression
         of the let. Example: [let m : (module S) = ... in let (module M) = m in
         ...] We learn the signature [S] from the type of [m] in the RHS of the
         second let, and we need that knowledge for [type_module] to succeed. If
         we type-checked expressions before patterns, then we could call
         [add_module_variables] here.
      *)
      let new_env = add_pattern_variables new_env pvs in
      let mode_pat_typ_list =
        List.map
          (fun (m, pat) ->
             let ty = pat.pat_type in
             m, {pat with pat_type = instance ty}, ty)
          pat_list
      in
      (* Only bind pattern variables after generalizing *)
      List.iter (fun f -> f()) force;

      let exp_list =
        (* See Note [add_module_variables after checking expressions]
           We can't defer type-checking module variables with recursive
           definitions, so things like [let rec (module M) = m in ...] always
           fail, even if the type of [m] is known.
        *)
        let exp_env =
          if is_recursive then add_module_variables new_env mvs else env
        in
        type_let_def_wrap_warnings ?check ?check_strict ~is_recursive
          ~entirely_functions
          ~exp_env ~new_env ~spat_sexp_list ~attrs_list ~mode_pat_typ_list ~pvs
          (fun exp_env ({pvb_attributes; _} as vb) mode expected_ty ->
            let sexp = vb_exp_constraint vb in
            match get_desc expected_ty with
            | Tpoly (ty, tl) ->
                let vars, ty' =
                  with_local_level_if_principal
                    ~post:(fun (_,ty') -> generalize_structure ty')
                    (fun () -> instance_poly ~keep_names:true true tl ty)
                in
                let exp =
                  Builtin_attributes.warning_scope pvb_attributes (fun () ->
                    type_expect exp_env mode sexp (mk_expected ty'))
                in
                exp, Some vars
            | _ ->
                let exp =
                  Builtin_attributes.warning_scope pvb_attributes (fun () ->
                    type_expect exp_env mode sexp (mk_expected expected_ty))
                in
                exp, None)
      in
      List.iter2
        (fun (_, pat, _) (attrs, exp) ->
          Builtin_attributes.warning_scope ~ppwarning:false attrs
            (fun () ->
              let case = Parmatch.typed_case (case pat exp) in
              ignore(check_partial env pat.pat_type pat.pat_loc
                       [case] : Typedtree.partial)
            )
        )
        mode_pat_typ_list
        (List.map2 (fun (attrs, _, _, _) (e, _) -> attrs, e) spatl exp_list);
      (mode_pat_typ_list, exp_list, new_env, mvs, sorts,
       List.map (fun pv -> { pv with pv_type = instance pv.pv_type}) pvs)
    end
    ~post: begin fun (mode_pat_typ_list, exp_list, _, _, _, pvs) ->
      List.iter2
        (fun (_, pat, _) (exp, _) ->
          if maybe_expansive exp then lower_contravariant env pat.pat_type)
        mode_pat_typ_list exp_list;
      iter_pattern_variables_type generalize pvs;
      (* update pattern variable jkind reasons *)
      List.iter
        (fun pv ->
          Ctype.check_and_update_generalized_ty_jkind
            ~name:pv.pv_id ~loc:pv.pv_loc pv.pv_type)
        pvs;
      List.iter2
        (fun (_, _, expected_ty) (exp, vars) ->
          match vars with
          | None ->
          (* We generalize expressions even if they are not bound to a variable
             and do not have an expliclit polymorphic type annotation.  This is
             not needed in general, however those types may be shown by the
             interactive toplevel, for example:
             {[
               let _ = Array.get;;
               - : 'a array -> int -> 'a = <fun>
             ]}
             so we do it anyway. *)
              generalize exp.exp_type
          | Some vars ->
              if maybe_expansive exp then
                lower_contravariant env exp.exp_type;
              generalize_and_check_univars env "definition"
                exp expected_ty vars)
        mode_pat_typ_list exp_list;
      let update_exp_jkind (_, p, _) (exp, _) =
        let pat_name =
          match p.pat_desc with
            Tpat_var (id, _, _, _) -> Some id
          | Tpat_alias(_, id, _, _, _) -> Some id
          | _ -> None in
        Ctype.check_and_update_generalized_ty_jkind
          ?name:pat_name ~loc:exp.exp_loc exp.exp_type
      in
      List.iter2 update_exp_jkind mode_pat_typ_list exp_list;
    end
  in
  let l = List.combine pat_list exp_list in
  let l = List.combine sorts l in
  let l =
    List.map2
      (fun (s, ((_,p,_), (e, _))) pvb ->
        (* We check for [zero_alloc] attributes written on the [let] and move
           them to the function. *)
        let e = add_check_attribute e pvb.pvb_attributes in
        (* vb_rec_kind will be computed later for recursive bindings *)
        {vb_pat=p; vb_expr=e; vb_sort = s; vb_attributes=pvb.pvb_attributes;
         vb_loc=pvb.pvb_loc; vb_rec_kind = Dynamic;
        })
      l spat_sexp_list
  in
  if is_recursive then
    List.iter
      (fun {vb_pat=pat} -> match pat.pat_desc with
           Tpat_var _ -> ()
         | _ -> raise(Error(pat.pat_loc, env, Illegal_letrec_pat)))
      l;
  List.iter (fun vb ->
      if pattern_needs_partial_application_check vb.vb_pat then
        check_partial_application ~statement:false vb.vb_expr
    ) l;
  (* See Note [add_module_variables after checking expressions] *)
  let new_env = add_module_variables new_env mvs in
  (l, new_env)

and type_let_def_wrap_warnings
    ?(check = fun s -> Warnings.Unused_var s)
    ?(check_strict = fun s -> Warnings.Unused_var_strict s)
    ~is_recursive ~entirely_functions ~exp_env ~new_env ~spat_sexp_list
    ~attrs_list ~mode_pat_typ_list ~pvs
    type_def =
  let is_fake_let =
    match spat_sexp_list with
    | [{pvb_expr={pexp_desc=Pexp_match(
           {pexp_desc=Pexp_ident({ txt = Longident.Lident name})},_)}}]
      when String.starts_with ~prefix:"*opt" name ->
        true (* the fake let-declaration introduced by fun ?(x = e) -> ... *)
    | _ ->
        false
  in
  let check = if is_fake_let then check_strict else check in
  let warn_about_unused_bindings =
    List.exists
      (fun attrs ->
         Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
           Warnings.is_active (check "") || Warnings.is_active (check_strict "")
           || (is_recursive && (Warnings.is_active Warnings.Unused_rec_flag))))
      attrs_list
  in
  let exp_env =
    if not is_recursive && entirely_functions then begin
      (* Add ghost bindings to help detecting missing "rec" keywords.

         We only add those if the body of the definition is obviously a
         function. The rationale is that, in other cases, the hint is probably
         wrong (and the user is using "advanced features" anyway (lazy,
         recursive values...)).

         [pvb_loc] (below) is the location of the first let-binding (in case of
         a let .. and ..), and is where the missing "rec" hint suggests to add a
         "rec" keyword. *)
      match spat_sexp_list with
      | {pvb_loc; _} :: _ ->
          maybe_add_pattern_variables_ghost pvb_loc exp_env pvs
      | _ -> assert false
    end
    else exp_env
  in
  (* Algorithm to detect unused declarations in recursive bindings:
     - During type checking of the definitions, we capture the 'value_used'
       events on the bound identifiers and record them in a slot corresponding
       to the current definition (!current_slot).
       In effect, this creates a dependency graph between definitions.

     - After type checking the definition (!current_slot = None),
       when one of the bound identifier is effectively used, we trigger
       again all the events recorded in the corresponding slot.
       The effect is to traverse the transitive closure of the graph created
       in the first step.

     We also keep track of whether *all* variables in a given pattern
     are unused. If this is the case, for local declarations, the issued
     warning is 26, not 27.
   *)
  let current_slot = ref None in
  let rec_needed = ref false in
  let mode_typ_slot_list =
    List.map2
      (fun attrs (mode, pat, expected_ty) ->
        Builtin_attributes.warning_scope ~ppwarning:false attrs (fun () ->
          if not warn_about_unused_bindings then mode, expected_ty, None
          else
            let some_used = ref false in
            (* has one of the identifier of this pattern been used? *)
            let slot = ref [] in
            List.iter
              (fun id ->
                let vd = Env.find_value (Path.Pident id) new_env in
                (* note: Env.find_value does not trigger the value_used
                   event *)
                let name = Ident.name id in
                let used = ref false in
                if not (name = "" || name.[0] = '_' || name.[0] = '#') then
                  add_delayed_check
                    (fun () ->
                      if not !used then
                        Location.prerr_warning vd.Subst.Lazy.val_loc
                          ((if !some_used then check_strict else check) name)
                    );
                Env.set_value_used_callback
                  vd
                  (fun () ->
                    match !current_slot with
                    | Some slot ->
                        slot := vd.val_uid :: !slot; rec_needed := true
                    | None ->
                        List.iter Env.mark_value_used (get_ref slot);
                        used := true;
                        some_used := true
                  )
              )
              (Typedtree.pat_bound_idents pat);
            mode, expected_ty, Some slot
           ))
      attrs_list
      mode_pat_typ_list
  in
  let exp_list =
    List.map2
      (fun case (mode, expected_ty, slot) ->
        if is_recursive then current_slot := slot;
        type_def exp_env case mode expected_ty)
      spat_sexp_list mode_typ_slot_list
  in
  current_slot := None;
  if is_recursive && not !rec_needed then begin
    let {pvb_pat; pvb_attributes} = List.hd spat_sexp_list in
    (* See PR#6677 *)
    Builtin_attributes.warning_scope ~ppwarning:false pvb_attributes
      (fun () ->
         Location.prerr_warning pvb_pat.ppat_loc Warnings.Unused_rec_flag
      )
  end;
  exp_list

and type_andops env sarg sands expected_sort expected_ty =
  (* Pass arguments to [loop] to avoid allocating closure; [env] and [let_sarg]
     get passed down unchanged. *)
  let rec loop env let_sarg rev_sands expected_sort expected_ty =
    match rev_sands with
    | [] ->
        type_expect env mode_legacy let_sarg
          (mk_expected expected_ty),
        expected_sort,
        []
    | { pbop_op = sop; pbop_exp = sexp; pbop_loc = loc; _ } :: rest ->
        let op_path, op_desc, op_type, ty_arg, sort_arg, ty_rest, sort_rest,
            ty_result, op_result_sort =
          with_local_level_iter_if_principal begin fun () ->
            let op_path, op_desc = type_binding_op_ident env sop in
            let op_type = op_desc.val_type in
            let ty_arg, sort_arg = new_rep_var ~why:Function_argument () in
            let ty_rest, sort_rest = new_rep_var ~why:Function_argument () in
            let ty_result, op_result_sort =
              new_rep_var ~why:Function_result ()
            in
            let arrow_desc = (Nolabel, Alloc.legacy, Alloc.legacy) in
            let ty_rest_fun =
              newty (Tarrow(arrow_desc, newmono ty_arg, ty_result, commu_ok)) in
            let ty_op =
              newty (Tarrow(arrow_desc, newmono ty_rest, ty_rest_fun, commu_ok)) in
            begin try
              unify env op_type ty_op
            with Unify err ->
              raise(Error(sop.loc, env, Andop_type_clash(sop.txt, err)))
            end;
            ((op_path, op_desc, op_type, ty_arg, sort_arg, ty_rest, sort_rest,
              ty_result, op_result_sort),
             [ty_rest; ty_arg; ty_result])
          end
          ~post:generalize_structure
        in
        let let_arg, sort_let_arg, rest =
          loop env let_sarg rest sort_rest ty_rest
        in
        let exp = type_expect env mode_legacy sexp (mk_expected ty_arg) in
        begin try
          unify env (instance ty_result) (instance expected_ty)
        with Unify err ->
          raise(Error(loc, env, Bindings_type_clash(err)))
        end;
        let andop =
          { bop_op_name = sop;
            bop_op_path = op_path;
            bop_op_val = op_desc;
            bop_op_type = op_type;
            bop_op_return_sort = op_result_sort;
            bop_exp = exp;
            bop_exp_sort = sort_arg;
            bop_loc = loc }
        in
        let_arg, sort_let_arg, andop :: rest
  in
  let let_arg, sort_let_arg, rev_ands =
    loop env sarg (List.rev sands) expected_sort expected_ty
  in
  let_arg, sort_let_arg, List.rev rev_ands

(* Can be re-inlined when we upstream immutable arrays *)
and type_generic_array
      ~loc
      ~env
      ~(expected_mode : expected_mode)
      ~ty_expected
      ~explanation
      ~mutability
      ~attributes
      sargl
  =
  let alloc_mode, argument_mode = register_allocation expected_mode in
  let type_, modalities =
    if Types.is_mutable mutability then
      Predef.type_array, Typemode.mutable_implied_modalities
    else
      Predef.type_iarray, Modality.Value.Const.id
  in
  check_construct_mutability ~loc ~env mutability argument_mode;
  let argument_mode = mode_modality modalities argument_mode in
  let jkind, elt_sort = Jkind.of_new_sort_var ~why:Array_element in
  let ty = newgenvar jkind in
  let to_unify = type_ ty in
  with_explanation explanation (fun () ->
    unify_exp_types loc env to_unify (generic_instance ty_expected));
  let argument_mode = expect_mode_cross env ty argument_mode in
  let argl =
    List.map
      (fun sarg -> type_expect env argument_mode sarg (mk_expected ty))
      sargl
  in
  re {
    exp_desc = Texp_array (mutability, elt_sort, argl, alloc_mode);
    exp_loc = loc; exp_extra = [];
    exp_type = instance ty_expected;
    exp_attributes = attributes;
    exp_env = env }

and type_expect_jane_syntax
      ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue ~attributes
  : Jane_syntax.Expression.t -> _ = function
  | Jexp_comprehension x ->
      type_comprehension_expr
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue ~attributes x
  | Jexp_immutable_array x ->
      type_immutable_array
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue ~attributes x
  | Jexp_layout x ->
      type_jkind_expr
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue ~attributes x
  | Jexp_n_ary_function x ->
      type_n_ary_function
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~attributes x
  | Jexp_tuple x ->
      type_tuple
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~attributes x
  | Jexp_modes x ->
      type_mode_expr
        ~loc ~env ~expected_mode ~ty_expected ~explanation ~attributes x

and type_mode_expr
    ~loc ~env ~expected_mode ~ty_expected ~explanation ~attributes
  : Jane_syntax.Modes.expression -> _ = function
  | Coerce (m, sbody) ->
    let modes = Typemode.transl_mode_annots m in
    let min = Alloc.Const.Option.value ~default:Alloc.Const.min modes |> Const.alloc_as_value in
    let max = Alloc.Const.Option.value ~default:Alloc.Const.max modes |> Const.alloc_as_value in
    submode ~loc ~env ~reason:Other (Value.of_const min) expected_mode;
    let expected_mode = mode_coerce (Value.of_const max) expected_mode in
    let expected_mode =
      match modes.areality with
      | Some Local -> mode_strictly_local expected_mode
      | _ -> expected_mode
    in
    let exp =
      type_expect env expected_mode sbody (mk_expected ty_expected ?explanation)
    in
    {exp with
     (* CR modes: We should consider not overriding [exp_loc] here -- that would
        be more consistent to the typing of [Pexp_constraint].
     *)
     exp_loc = loc;
     exp_extra = (Texp_mode_coerce m, loc, attributes) :: exp.exp_extra}

and type_n_ary_function
      ~loc ~env ~(expected_mode : expected_mode) ~ty_expected
      ~explanation ~attributes
      ((params, constraint_, body) : Jane_syntax.N_ary_functions.expression)
    =
    let region_locked = not (Is_local_returning.function_body body) in
    let in_function =
      { ty_fun = mk_expected (instance ty_expected) ?explanation;
        loc_fun = loc;
        region_locked;
      }
    in
    let { function_ = exp_type, result_params, body;
          newtypes; params_contain_gadt = contains_gadt;
          ret_info; fun_alloc_mode;
        } =
      type_function env expected_mode ty_expected params constraint_ body
        ~in_function ~first:true
    in
    let fun_alloc_mode, { ret_mode; ret_sort } =
      match fun_alloc_mode, ret_info with
      | Some x, Some y -> x, y
      | None, _ ->
          Misc.fatal_error
            "[fun_alloc_mode] can't be None -- that indicates a function with \
             no parameters."
      | _, None ->
          Misc.fatal_error
            "[ret_info] can't be None -- that indicates a function with \
             no parameters."
    in
    let params = List.map (fun { param } -> param) result_params in
    let syntactic_arity = function_arity params body in
    (* Require that the n-ary function is known to have at least n arrows
        in the type. This prevents GADT equations introduced by the parameters
        from hiding arrows from the resulting type.
        Performance hack: Only do this check when any of [params] contains a
        GADT, as this is the only opportunity for arrows to be hidden from the
        resulting type.
    *)
    begin match contains_gadt with
    | No_gadt -> ()
    | Contains_gadt ->
        (* Assert that [ty] is a function, and return its return type. *)
        let filter_ty_ret_exn ty arg_label ~force_tpoly =
          match filter_arrow env ty arg_label ~force_tpoly with
          | { ty_ret; _ } -> ty_ret
          | exception (Filter_arrow_failed error) ->
              let trace =
                match error with
                | Unification_error trace -> trace
                | Not_a_function ->
                    let tarrow =
                      let new_ty_var why = newvar (Jkind.of_new_sort ~why) in
                      let new_mode_var () = Mode.Alloc.newvar () in
                      (newty
                         (Tarrow
                            ( (arg_label, new_mode_var (), new_mode_var ())
                            , new_ty_var Function_argument
                            , new_ty_var Function_result
                            , commu_ok )));
                    in
                    (* We go to some trouble to try to generate a unification
                       error to help the error printing code's heuristic to
                       identify the type equation at fault.
                    *)
                    (try
                       unify env tarrow ty;
                       fatal_error "unification unexpectedly succeeded"
                     with Unify trace -> trace)
                | Label_mismatch _ ->
                    fatal_error
                      "Label_mismatch not expected as this point; this should \
                       have been caught when the function was typechecked."
                | Jkind_error _ ->
                    fatal_error
                      "Jkind_error not expected as this point; this should \
                       have been caught when the function was typechecked."
              in
              let err =
                Function_arity_type_clash
                  { syntactic_arity;
                    type_constraint = exp_type;
                    trace;
                  }
              in
              raise (Error (loc, env, err))
        in
        let ret_ty =
          List.fold_left (fun ret_ty { param; has_poly } ->
              filter_ty_ret_exn ret_ty param.fp_arg_label
                ~force_tpoly:(not has_poly))
            exp_type
            result_params
        in
        match body with
        | Tfunction_body _ -> ()
        | Tfunction_cases _ ->
            ignore
              (filter_ty_ret_exn ret_ty Nolabel ~force_tpoly:true : type_expr)
    end;
    let zero_alloc =
      Builtin_attributes.get_zero_alloc_attribute ~in_signature:false
        ~default_arity:syntactic_arity attributes
    in
    re
      { exp_desc =
          Texp_function
            { params; body; region = region_locked; ret_sort;
              alloc_mode = Mode.Alloc.disallow_left fun_alloc_mode; ret_mode;
              zero_alloc
            };
        exp_loc = loc;
        exp_extra =
          List.map
            (fun ({ txt; loc }, layout) -> Texp_newtype (txt, layout), loc, [])
            newtypes;
        exp_type;
        exp_attributes = attributes;
        exp_env = env;
      }

(* What modes should comprehensions use?  Let us be generic over the sequence
   type we use for comprehensions, calling it [sequence] (standing for either
   [list] or [array]) and writing [[? ... ?]].  If we ignore modes, we may
   consider a comprehension as having been typechecked per the following
   modeless rule:

   {[
     G |- a type
     G |- b type
     G |- seq : a sequence
     G |- low : int
     G |- high : int
     G, x : a, i : int |- cond : bool
     G, x : a, i : int |- body : b
     -----------------------------------------------------------------------
     G |- [? body for x in seq and i = low to high when cond ?] : b sequence
   ]}

   To reason about modes, we have to separately consider the modes of [body],
   [x], [seq], [i], [low], [high], [cond], and the entire comprehension.

   - The modes of [i], [low], [high], and [cond] are simple: We may be
     *polymorphic* in each of them individually.  As [int] and [bool] are
     immediates, values of these types may freely be used at any mode.  We thus
     don't need to consider these modes any further.

   - The modes of [x] and [seq] must be *the same as each other*, as we do not
     distinguish between the "spine mode" and the "value mode" for types; a list
     or array must be as local or as global as its elements are.  (If these were
     separate concepts, we could unconditionally allocate a list or array
     "spine-locally", and handle [x]'s mode separately.)  We'll refer to this as
     the "input mode" below.

   - By the same token, the modes of [body] and the entire comprehension must be
     *the same as each other*, as we are generating a sequence made up of the
     result of evaluating [body] repeatedly.  We'll refer to this as the "output
     mode" below.

   - The input mode must be *below* the output mode.  Clearly, the two can be
     the same as each other; and if the input is local, then the output surely
     cannot be global, as it can refer to the input and we cannot have heap
     pointers pointing into the stack.  However, if the input is global, then it
     is perfectly safe for a local sequence to contain references to it, and so
     there is no harm in allowing the output to be local.

   Thus, the question turns on what mode we are to use for the output, the mode
   of [body] and the entire comprehension.  While it would be nice to be
   polymorphic here, *we are unfortunately currently constrained to check
   comprehensions at global mode*.  This is not a fundamental limitation, and
   would just require updating the translation code to be jkind-aware as it
   happens after inference.  The changes this would require for list and array
   comprehensions are different:

   - For list comprehensions: List comprehensions are desugared in terms of
     functions and types from [CamlinternalComprehension]; as part of regular
     OCaml, they cannot cannot have the desired (or any) mode polymorphism.
     However, as there are only two modes, we could duplicate the module to
     contain two nearly-identical copies of the code: one that operates on the
     current spine-local but element-global intermediate type and constructs a
     global list at th end; and the other that operates on a very similar spine-
     *and* element-local intermediate type and constructs a local list at the
     end.

   - For array comprehensions: We currently only have global arrays, and do not
     currently allow there to be such a thing as a local array at all.  If this
     changed, we could add mode-directed support for allocating the resulting
     array apropriately.

   Until we make either of these changes, we do not pass modes to other
   functions for typechecking comprehensions.

   In order to understand the reasoning about modes for comprehensions, anywhere
   we need to provide modes while typechecking comprehensions, we will reference
   this comment by its incipit (the initial question, right at the start). *)

and type_comprehension_expr
      ~loc ~env ~expected_mode:_ ~ty_expected ~explanation:_ ~rue:_~attributes
      cexpr =
  let open Jane_syntax.Comprehensions in
  (* - [comprehension_type]:
         For printing nicer error messages.
     - [container_type]:
         For type checking [for]-[in] iterators and the type of the whole
         comprehension.
     - [make_texp]:
         For building the final typedtree node containing the translated
         comprehension.
     - [{body = sbody; clauses}]:
         The actual comprehension to be translated. *)
  let comprehension_type, container_type, make_texp,
      {body = sbody; clauses}, jkind =
    match cexpr with
    | Cexp_list_comprehension comp ->
        List_comprehension,
        Predef.type_list,
        (fun tcomp -> Texp_list_comprehension tcomp),
        comp,
        Predef.list_argument_jkind
    | Cexp_array_comprehension (amut, comp) ->
        let container_type, mut = match amut with
          | Mutable   -> Predef.type_array, Mutable Alloc.Comonadic.Const.legacy
          | Immutable -> Predef.type_iarray, Immutable
        in
        Array_comprehension mut,
        container_type,
        (fun tcomp ->
          Texp_array_comprehension
            (mut, Jkind.Type.Sort.for_array_comprehension_element, tcomp)),
        comp,
        (* CR layouts v4: When this changes from [value], you will also have to
           update the use of [transl_exp] in transl_array_comprehension.ml. See
           a companion CR layouts v4 at the point of interest in that file. *)
        Jkind.Type.Primitive.value ~why:Jkind.Type.History.Array_comprehension_element |> Jkind.of_type_jkind
  in
  let element_ty =
    with_local_level_if_principal begin fun () ->
      let element_ty = newvar jkind in
      unify_exp_types
        loc
        env
        (instance (container_type element_ty))
        (instance ty_expected);
      element_ty
    end ~post:generalize_structure
  in
  let new_env, comp_clauses =
    (* To understand why we don't provide modes here, see "What modes should
       comprehensions use?", above *)
    type_comprehension_clauses
      ~loc ~env ~comprehension_type ~container_type clauses
  in
  let comp_body =
    (* To understand why comprehension bodies are checked at [mode_global], see
       "What modes should comprehensions use?", above *)
    type_expect new_env mode_legacy sbody (mk_expected element_ty)
  in
  re { exp_desc       = make_texp { comp_body ; comp_clauses }
     ; exp_loc        = loc
     ; exp_extra      = []
     ; exp_type       = instance (container_type comp_body.exp_type)
     ; exp_attributes = attributes
     ; exp_env        = env }

and type_comprehension_clauses
      ~loc ~env ~comprehension_type ~container_type clauses =
  List.fold_left_map
    (type_comprehension_clause ~loc ~comprehension_type ~container_type)
    env
    clauses

(* Calls [reset_pattern] *)
and type_comprehension_clause ~loc ~comprehension_type ~container_type env
  : Jane_syntax.Comprehensions.clause -> _ = function
  | For bindings ->
      (* TODO: fix handling of first-class module patterns *)
      let tps = create_type_pat_state Modules_rejected in
      let tbindings =
        List.map
          (type_comprehension_binding
             ~loc ~comprehension_type ~container_type ~env tps)
          bindings
      in
      let env =
        let check s = Warnings.Unused_var s in
        let pvs = tps.tps_pattern_variables in
        add_pattern_variables ~check ~check_as:check env pvs
      in
      env, Texp_comp_for tbindings
  | When cond ->
      let tcond =
        (* To understand why [when] conditions can be checked at an arbitrary
           mode, see "What modes should comprehensions use?" in
           [type_comprehension_expr]*)
        type_expect
          env
          mode_max
          cond
          (mk_expected ~explanation:Comprehension_when Predef.type_bool)
      in
      env, Texp_comp_when tcond

and type_comprehension_binding
      ~loc
      ~comprehension_type
      ~container_type
      ~env
      tps
      Jane_syntax.Comprehensions.{ pattern; iterator; attributes } =
  { comp_cb_iterator =
      type_comprehension_iterator
        ~loc ~env ~comprehension_type ~container_type tps pattern iterator
  ; comp_cb_attributes =
      attributes
  }

and type_comprehension_iterator
      ~loc ~env ~comprehension_type ~container_type tps pattern
  : Jane_syntax.Comprehensions.iterator -> _ = function
  | Range { start; stop; direction } ->
      let tbound ~explanation bound =
        (* To understand why [for ... = ...] iterator range endpoints can be
           checked at an arbitrary mode, see "What modes should comprehensions
           use?" in [type_comprehension_expr]*)
        type_expect
          env
          mode_max
          bound
          (mk_expected ~explanation Predef.type_int)
      in
      let start = tbound ~explanation:Comprehension_for_start start in
      let stop  = tbound ~explanation:Comprehension_for_stop  stop  in
      (* When we'll want to add Uid to comprehension bindings,
         we can take it from here. *)
      let (ident, _uid) =
        type_comprehension_for_range_iterator_index
          tps
          ~loc
          ~env
          ~param:pattern
      in
      Texp_comp_range { ident; pattern; start; stop; direction }
  | In seq ->
      let item_ty = newvar (Jkind.Type.Primitive.any ~why:Dummy_jkind |> Jkind.of_type_jkind) in
      let seq_ty = container_type item_ty in
      let sequence =
        (* To understand why we can currently only iterate over [mode_global]
           (and not local) sequences, see "What modes should comprehensions
           use?" in [type_comprehension_expr]*)
        type_expect
          env
          mode_legacy
          seq
          (mk_expected
             ~explanation:(Comprehension_in_iterator comprehension_type)
             seq_ty)
      in
      let pattern =
        (* To understand why we can currently only provide [global] bindings for
           the contents of sequences comprehensions iterate over, see "What
           modes should comprehensions use?" in [type_comprehension_expr]*)
        type_pat
          tps
          Value
          ~no_existentials:In_self_pattern
          ~alloc_mode:(simple_pat_mode Value.legacy)
          (ref env)
          pattern
          item_ty
      in
      Texp_comp_in { pattern; sequence }

and type_immutable_array
      ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue:_ ~attributes
    : Jane_syntax.Immutable_arrays.expression -> _ = function
  | Iaexp_immutable_array elts ->
      type_generic_array
        ~loc
        ~env
        ~expected_mode
        ~ty_expected
        ~explanation
        ~mutability:Immutable
        ~attributes
        elts

and type_jkind_expr
      ~loc ~env ~expected_mode ~ty_expected:_ ~explanation:_ ~rue ~attributes
  : Jane_syntax.Layouts.expression -> _ = function
  | Lexp_constant x -> type_unboxed_constant ~loc ~env ~rue ~attributes x
  | Lexp_newtype (name, jkind_annot, sbody) ->
    type_newtype_expr ~loc ~env ~expected_mode ~rue ~attributes
      name (Some jkind_annot) sbody

and type_unboxed_constant ~loc ~env ~rue ~attributes cst =
  let cst = unboxed_constant_or_raise env loc cst in
  rue {
    exp_desc = Texp_constant cst;
    exp_loc = loc;
    exp_extra = [];
    exp_type = type_constant cst;
    exp_attributes = attributes;
    exp_env = env }

(* Typing of method call *)
and type_send env loc explanation e met =
  let obj = type_exp env mode_legacy e in
  let (meth, typ) =
    match obj.exp_desc with
    | Texp_ident(_, _, {val_kind = Val_self(sign, meths, _, _)}, _, _) ->
        let id, typ =
          match meths with
          | Self_concrete meths ->
              let id =
                match Meths.find met meths with
                | id -> id
                | exception Not_found ->
                    let valid_methods =
                      Meths.fold (fun lab _ acc -> lab :: acc) meths []
                    in
                    raise (Error(e.pexp_loc, env,
                                 Undefined_self_method (met, valid_methods)))
              in
              let typ = Btype.method_type met sign in
              id, typ
          | Self_virtual meths_ref -> begin
              match Meths.find met !meths_ref with
              | id -> id, Btype.method_type met sign
              | exception Not_found ->
                  let id = Ident.create_local met in
                  let ty = newvar (Jkind.Type.Primitive.value ~why:Object_field |> Jkind.of_type_jkind) in
                  meths_ref := Meths.add met id !meths_ref;
                  add_method env met Private Virtual ty sign;
                  Location.prerr_warning loc
                    (Warnings.Undeclared_virtual_method met);
                  id, ty
          end
        in
        Tmeth_val id, typ
    | Texp_ident(_, _, {val_kind = Val_anc (sign, meths, cl_num)}, _, _) ->
        let id =
          match Meths.find met meths with
          | id -> id
          | exception Not_found ->
              let valid_methods =
                Meths.fold (fun lab _ acc -> lab :: acc) meths []
              in
              raise (Error(e.pexp_loc, env,
                           Undefined_self_method (met, valid_methods)))
        in
        let typ = Btype.method_type met sign in
        let (self_path, _) =
          Env.find_value_by_name
            (Longident.Lident ("self-" ^ cl_num)) env
        in
        Tmeth_ancestor(id, self_path), typ
    | _ ->
        let ty =
          match filter_method env met obj.exp_type with
          | ty -> ty
          | exception Filter_method_failed err ->
              let error =
                match err with
                | Unification_error err ->
                    Expr_type_clash(err, explanation, None)
                | Not_an_object ty ->
                    Not_an_object(ty, explanation)
                | Not_a_method ->
                    let valid_methods =
                      match get_desc (expand_head env obj.exp_type) with
                      | Tobject (fields, _) ->
                          let (fields, _) = Ctype.flatten_fields fields in
                          let collect_fields li (meth, meth_kind, _meth_ty) =
                            if field_kind_repr meth_kind = Fpublic
                            then meth::li else li
                          in
                          Some (List.fold_left collect_fields [] fields)
                      | _ -> None
                    in
                    Undefined_method(obj.exp_type, met, valid_methods)
                | Not_a_value err ->
                    Not_a_value (err, explanation)
              in
              raise (Error(e.pexp_loc, env, error))
        in
        Tmeth_name met, ty
  in
  (obj,meth,typ)


let maybe_check_uniqueness_exp exp =
  if Language_extension.is_enabled Unique then
    check_uniqueness_exp exp

let maybe_check_uniqueness_value_bindings vbl =
  if Language_extension.is_enabled Unique then
    check_uniqueness_value_bindings vbl

(* Typing of toplevel bindings *)

let type_binding env rec_flag ?force_toplevel spat_sexp_list =
  let (pat_exp_list, new_env) =
    type_let
      ~check:(fun s -> Warnings.Unused_value_declaration s)
      ~check_strict:(fun s -> Warnings.Unused_value_declaration s)
      ?force_toplevel
      At_toplevel
      env rec_flag spat_sexp_list Modules_rejected
  in
  maybe_check_uniqueness_value_bindings pat_exp_list;
  (pat_exp_list, new_env)

let type_let existential_ctx env rec_flag spat_sexp_list =
  let (pat_exp_list, new_env) =
    type_let existential_ctx env rec_flag spat_sexp_list Modules_rejected
  in
  maybe_check_uniqueness_value_bindings pat_exp_list;
  (pat_exp_list, new_env)

(* Typing of toplevel expressions *)

let type_expression env jkind sexp =
  let exp =
    with_local_level begin fun () ->
      Typetexp.TyVarEnv.reset ();
      let expected = mk_expected (newvar jkind) in
      type_expect env mode_legacy sexp expected
    end
    ~post:(may_lower_contravariant_then_generalize env)
  in
  let exp =
    match sexp.pexp_desc with
      Pexp_ident lid ->
        let loc = sexp.pexp_loc in
        (* Special case for keeping type variables when looking-up a variable *)
        let (_path, desc, _actual_mode) =
          Env.lookup_value ~use:false ~loc lid.txt env
        in
        {exp with exp_type = desc.val_type}
    | _ -> exp
  in
  maybe_check_uniqueness_exp exp; exp

let type_representable_expression ~why env sexp =
  let jkind, sort = Jkind.of_new_sort_var ~why in
  let exp = type_expression env jkind sexp in
  exp, sort

let type_expression env sexp =
  type_expression env (Jkind.Primitive.top ~why:Type_expression_call) sexp

(* Error report *)

let spellcheck ppf unbound_name valid_names =
  Misc.did_you_mean ppf (fun () ->
    Misc.spellcheck valid_names unbound_name
  )

let spellcheck_idents ppf unbound valid_idents =
  spellcheck ppf (Ident.name unbound) (List.map Ident.name valid_idents)

open Format

let longident = Printtyp.longident

let tuple_component ~print_article ppf lbl =
  let article =
    match print_article, lbl with
    | true, Some _ -> "a "
    | true, None -> "an "
    | false, _ -> ""
  in
  match lbl with
  | Some s -> fprintf ppf "%scomponent with label %s" article s
  | None -> fprintf ppf "%sunlabeled component" article

(* Returns the first diff of the trace *)
let type_clash_of_trace trace =
  Errortrace.(explain trace (fun ~prev:_ -> function
    | Diff diff -> Some diff
    | _ -> None
  ))

(* Hint on type error on integer literals
   To avoid confusion, it is disabled on float literals
   and when the expected type is `int` *)
(* CR layouts v2.5: Should we add a case here for float#?  Test it, if so. *)
let report_literal_type_constraint expected_type const =
  let const_str = match const with
    | Pconst_integer (s, _) -> Some s
    | _ -> None
  in
  let suffix =
    if Path.same expected_type Predef.path_int32 then
      Some 'l'
    else if Path.same expected_type Predef.path_int64 then
      Some 'L'
    else if Path.same expected_type Predef.path_nativeint then
      Some 'n'
    else if Path.same expected_type Predef.path_float32 then
      Some 's'
    else if Path.same expected_type Predef.path_float then
      Some '.'
    else None
  in
  match const_str, suffix with
  | Some c, Some s -> [ Location.msg "@[@{<hint>Hint@}: Did you \
                                      mean `%s%c'?@]" c s ]
  | _, _ -> []

let report_literal_type_constraint const = function
  | Some tr ->
      begin match get_desc Errortrace.(tr.expected.ty) with
        Tconstr (typ, [], _) ->
          report_literal_type_constraint typ const
      | _ -> []
      end
  | None -> []

let report_partial_application = function
  | Some tr -> begin
      match get_desc tr.Errortrace.got.Errortrace.expanded with
      | Tarrow _ ->
          [ Location.msg
              "@[@{<hint>Hint@}: This function application is partial,@ \
               maybe some arguments are missing.@]" ]
      | _ -> []
    end
  | None -> []

let report_expr_type_clash_hints exp diff =
  match exp with
  | Some (Pexp_constant const) -> report_literal_type_constraint const diff
  | Some (Pexp_apply _) -> report_partial_application diff
  | _ -> []

let report_pattern_type_clash_hints pat diff =
  match pat with
  | Some (Ppat_constant const) -> report_literal_type_constraint const diff
  | _ -> []

let report_type_expected_explanation expl ppf =
  let because expl_str = fprintf ppf "@ because it is in %s" expl_str in
  match expl with
  | If_conditional ->
      because "the condition of an if-statement"
  | If_no_else_branch ->
      because "the result of a conditional with no else branch"
  | While_loop_conditional ->
      because "the condition of a while-loop"
  | While_loop_body ->
      because "the body of a while-loop"
  | For_loop_start_index ->
      because "a for-loop start index"
  | For_loop_stop_index ->
      because "a for-loop stop index"
  | For_loop_body ->
      because "the body of a for-loop"
  | Assert_condition ->
      because "the condition of an assertion"
  | Sequence_left_hand_side ->
      because "the left-hand side of a sequence"
  | When_guard ->
      because "a when-guard"
  | Comprehension_in_iterator comp_ty ->
      let a_comp_ty =
        match comp_ty with
        | List_comprehension            -> "a list"
        | Array_comprehension (Mutable _)   -> "an array"
        | Array_comprehension Immutable -> "an immutable array"
      in
      because ("a for-in iterator in " ^ a_comp_ty ^ " comprehension")
  | Comprehension_for_start ->
      because "a range-based for iterator start index in a comprehension"
  | Comprehension_for_stop ->
      because "a range-based for iterator stop index in a comprehension"
  | Comprehension_when ->
      because "a when-clause in a comprehension"
  | Error_message_attr msg ->
      fprintf ppf "@\n@[%s@]" msg

let escaping_hint (failure_reason : Value.error) submode_reason
      (context : Env.closure_context option) =
  begin match failure_reason, context with
  | Error (Comonadic Areality, e), Some h ->
    begin match e, h with
    | {left=Local; right=Regional}, Return ->
      (* Only hint to use exclave_, when the user wants to return local, but
         expected mode is regional. If the expected mode is as strict as
         global, then exclave_ won't solve the problem. *)
      [ Location.msg
          "@[Hint: Cannot return a local value without an@ \
           \"exclave_\" annotation.@]" ]
    | _, Return -> []
    | _, Tailcall_argument ->
      [ Location.msg
          "@[Hint: This argument cannot be local,@ \
           because it is an argument in a tail call.@]" ]
    | _, Tailcall_function ->
      [ Location.msg
          "@[Hint: This function cannot be local,@ \
           because it is the function in a tail call.@]" ]
    | _, Partial_application ->
      [ Location.msg
          "@[Hint: It is captured by a partial application.@]" ]
    end
  | _, _ -> []
  end
  @
  begin match submode_reason with
  (* TODO: generalize this to other axis as well *)
  | Application result_ty ->
    (* [get_non_local_arity ty] returns [Some (n_args, sureness)] iff [ty] is a
       function type with [n_args] arguments and its return type is
       local. [sureness] <=> the return type is definitely local. *)
    let get_non_local_arity ty =
      let rec loop sureness n ty =
        match get_desc ty with
        | Tarrow ((_, _, res_mode), _, res_ty, _) ->
          begin match
            Locality.Guts.check_const (Alloc.proj (Comonadic Areality) res_mode)
          with
          | Some Global ->
            Some (n+1, true)
          | (None | Some Local) as res_mode ->
            let sureness = sureness && Option.is_some res_mode in
            loop sureness (n+1) res_ty
          end
        | _ ->
          if n = 0
          then None
          else Some (n, sureness)
      in
      loop true 0 ty
    in
    begin match get_non_local_arity result_ty with
    | Some (n, sure) ->
      let args = if n = 1 then "argument" else "arguments" in
      let qualifier = if sure then "will" else "may" in
      [ Location.msg
          "Hint: @[This is a partial application@,\
                   Adding %d more %s %s make the value non-local@]"
          n args qualifier ]
    | None -> []
    end
  | Other -> []
  end


let contention_hint _fail_reason _submode_reason context =
  match context with
  | Some Read_mutable ->
      [Location.msg
        "@[Hint: In order to read from the mutable fields,@ \
        this record needs to be uncontended.@]"]
  | Some Write_mutable ->
      [Location.msg
        "@[Hint: In order to write into the mutable fields,@ \
        this record needs to be uncontended.@]"]
  | None -> []

let report_type_expected_explanation_opt expl ppf =
  match expl with
  | None -> ()
  | Some expl -> report_type_expected_explanation expl ppf

let report_unification_error ~loc ?sub env err
    ?type_expected_explanation txt1 txt2 =
  Location.error_of_printer ~loc ?sub (fun ppf () ->
    Printtyp.report_unification_error ppf env err
      ?type_expected_explanation txt1 txt2
  ) ()

let report_this_function ppf funct =
  if Typedtree.exp_is_nominal funct then
    let pexp = Untypeast.untype_expression funct in
    Format.fprintf ppf "The function '%a'" Pprintast.expression pexp
  else Format.fprintf ppf "This function"

let report_too_many_arg_error ~funct ~func_ty ~previous_arg_loc
    ~extra_arg_loc ~returns_unit loc =
  let open Location in
  let cnum_offset off (pos : Lexing.position) =
    { pos with pos_cnum = pos.pos_cnum + off }
  in
  let app_loc =
    (* Span the application, including the extra argument. *)
    { loc_start = loc.loc_start;
      loc_end = extra_arg_loc.loc_end;
      loc_ghost = false }
  and tail_loc =
    (* Possible location for a ';'. The location is widened to overlap the end
       of the argument. *)
    let arg_end = previous_arg_loc.loc_end in
    { loc_start = cnum_offset ~-1 arg_end;
      loc_end = cnum_offset ~+1 arg_end;
      loc_ghost = false }
  in
  let hint_semicolon = if returns_unit then [
      msg ~loc:tail_loc "@{<hint>Hint@}: Did you forget a ';'?";
    ] else [] in
  let sub = hint_semicolon @ [
    msg ~loc:extra_arg_loc "This extra argument is not expected.";
  ] in
  errorf ~loc:app_loc ~sub
    "@[<v>@[<2>%a has type@ %a@]\
     @ It is applied to too many arguments@]"
    report_this_function funct Printtyp.type_expr func_ty

let report_error ~loc env = function
  | Constructor_arity_mismatch(lid, expected, provided) ->
      Location.errorf ~loc
       "@[The constructor %a@ expects %i argument(s),@ \
        but is applied here to %i argument(s)@]"
       longident lid expected provided
  | Constructor_labeled_arg ->
      Location.errorf ~loc
       "Constructors cannot have labeled arguments. \
        Consider using an inline record instead."
  | Partial_tuple_pattern_bad_type ->
      Location.errorf ~loc
        "Could not determine the type of this partial tuple pattern."
  | Extra_tuple_label (lbl, typ) ->
      Location.errorf ~loc
        "This pattern was expected to match values of type@ %a,@ but it \
         contains an extra %a."
        Printtyp.type_expr typ
        (tuple_component ~print_article:false) lbl;
  | Missing_tuple_label (lbl, typ) ->
      let hint ppf () =
        (* We only hint if the missing component is labeled.  This is
           unlikely to be a correct fix for traditional tuples. *)
        match lbl with
        | Some _ -> fprintf ppf "@ Hint: use .. to ignore some components."
        | None -> ()
      in
      Location.errorf ~loc
        "This pattern was expected to match values of type@ %a,@ but it is \
         missing %a.%a"
        Printtyp.type_expr typ
        (tuple_component ~print_article:true) lbl
        hint ()
  | Label_mismatch(lid, err) ->
      report_unification_error ~loc env err
        (function ppf ->
           fprintf ppf "The record field %a@ belongs to the type"
                   longident lid)
        (function ppf ->
           fprintf ppf "but is mixed here with fields of type")
  | Pattern_type_clash (err, pat) ->
      let diff = type_clash_of_trace err.trace in
      let sub = report_pattern_type_clash_hints pat diff in
      report_unification_error ~loc ~sub env err
        (function ppf ->
          fprintf ppf "This pattern matches values of type")
        (function ppf ->
          fprintf ppf "but a pattern was expected which matches values of \
                       type");
  | Or_pattern_type_clash (id, err) ->
      report_unification_error ~loc env err
        (function ppf ->
          fprintf ppf "The variable %s on the left-hand side of this \
                       or-pattern has type" (Ident.name id))
        (function ppf ->
          fprintf ppf "but on the right-hand side it has type")
  | Multiply_bound_variable name ->
      Location.errorf ~loc
        "Variable %s is bound several times in this matching"
        name
  | Orpat_vars (id, valid_idents) ->
      Location.error_of_printer ~loc (fun ppf () ->
        fprintf ppf
          "Variable %s must occur on both sides of this | pattern"
          (Ident.name id);
        spellcheck_idents ppf id valid_idents
      ) ()
  | Expr_type_clash (err, explanation, exp) ->
      let diff = type_clash_of_trace err.trace in
      let sub = report_expr_type_clash_hints exp diff in
      report_unification_error ~loc ~sub env err
        ~type_expected_explanation:
          (report_type_expected_explanation_opt explanation)
        (function ppf ->
           fprintf ppf "This expression has type")
        (function ppf ->
           fprintf ppf "but an expression was expected of type");
  | Function_arity_type_clash {
      syntactic_arity; type_constraint; trace = { trace };
    } ->
    (* The last diff's expected type will be the locally-abstract type
       that the GADT pattern introduced an equation on.
    *)
    let type_with_local_equation =
      let last_diff =
        List.find_map
          (function Errortrace.Diff diff -> Some diff | _ -> None)
          (List.rev trace)
      in
      match last_diff with
      | None -> None
      | Some diff -> Some diff.expected.ty
    in
    (* [syntactic_arity>1] for this error, so "arguments" is always plural. *)
    Location.errorf ~loc
      "@[\
       @[\
       The syntactic arity of the function doesn't match the type constraint:@ \
       @[<2>\
       This function has %d syntactic arguments, but its type is constrained \
       to@ %a.\
       @]@ \
       @]@ \
       @[\
       @[<2>@{<hint>Hint@}: \
       consider splitting the function definition into@ %s@ \
       where %s is the pattern with the GADT constructor that@ \
       introduces the local type equation%t.\
       @]"
      syntactic_arity
      Printtyp.type_expr type_constraint
      "fun ... gadt_pat -> fun ..."
      "gadt_pat"
      (fun ppf ->
         Option.iter
           (fprintf ppf " on %a" Printtyp.type_expr)
           type_with_local_equation)
  | Apply_non_function {
      funct; func_ty; res_ty; previous_arg_loc; extra_arg_loc
    } ->
      begin match get_desc func_ty with
        Tarrow _ ->
          let returns_unit = match get_desc res_ty with
            | Tconstr (p, _, _) -> Path.same p Predef.path_unit
            | _ -> false
          in
          report_too_many_arg_error ~funct ~func_ty ~previous_arg_loc
            ~extra_arg_loc ~returns_unit loc
      | _ ->
          Location.errorf ~loc "@[<v>@[<2>This expression has type@ %a@]@ %s@]"
            Printtyp.type_expr func_ty
            "This is not a function; it cannot be applied."
      end
  | Apply_wrong_label (l, ty, extra_info) ->
      let print_label ppf = function
        | Nolabel -> fprintf ppf "without label"
        |(Labelled _ | Optional _) as l -> fprintf ppf "with label %s"
                                           (prefixed_label_name l)
        | Position _ -> assert false
          (* Since Position labels never occur in function applications,
             this case is never run *)
      in
      let extra_info =
        if not extra_info then
          []
        else
          [ Location.msg
              "Since OCaml 4.11, optional arguments do not commute when \
               -nolabels is given" ]
      in
      Location.errorf ~loc ~sub:extra_info
        "@[<v>@[<2>The function applied to this argument has type@ %a@]@.\
         This argument cannot be applied %a@]"
        Printtyp.type_expr ty print_label l
  | Label_multiply_defined s ->
      Location.errorf ~loc "The record field label %s is defined several times"
        s
  | Label_missing labels ->
      let print_labels ppf =
        List.iter (fun lbl -> fprintf ppf "@ %s" (Ident.name lbl)) in
      Location.errorf ~loc "@[<hov>Some record fields are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lid ->
      Location.errorf ~loc "The record field %a is not mutable" longident lid
  | Wrong_name (eorp, ty_expected, { type_path; kind; name; valid_names; }) ->
      Location.error_of_printer ~loc (fun ppf () ->
        Printtyp.wrap_printing_env ~error:true env (fun () ->
          let { ty; explanation } = ty_expected in
          if Path.is_constructor_typath type_path then begin
            fprintf ppf
              "@[The field %s is not part of the record \
               argument for the %a constructor@]"
              name.txt
              Printtyp.type_path type_path;
          end else begin
            fprintf ppf
              "@[@[<2>%s type@ %a%t@]@ \
               There is no %s %s within type %a@]"
              eorp Printtyp.type_expr ty
              (report_type_expected_explanation_opt explanation)
              (Datatype_kind.label_name kind)
              name.txt (*kind*) Printtyp.type_path type_path;
          end;
          spellcheck ppf name.txt valid_names
      )) ()
  | Name_type_mismatch (kind, lid, tp, tpl) ->
      let type_name = Datatype_kind.type_name kind in
      let name = Datatype_kind.label_name kind in
      Location.error_of_printer ~loc (fun ppf () ->
        Printtyp.report_ambiguous_type_error ppf env tp tpl
          (function ppf ->
             fprintf ppf "The %s %a@ belongs to the %s type"
               name longident lid type_name)
          (function ppf ->
             fprintf ppf "The %s %a@ belongs to one of the following %s types:"
               name longident lid type_name)
          (function ppf ->
             fprintf ppf "but a %s was expected belonging to the %s type"
               name type_name)
      ) ()
  | Invalid_format msg ->
      Location.errorf ~loc "%s" msg
  | Not_an_object (ty, explanation) ->
    Location.error_of_printer ~loc (fun ppf () ->
      fprintf ppf "This expression is not an object;@ \
                   it has type %a"
        Printtyp.type_expr ty;
      report_type_expected_explanation_opt explanation ppf
    ) ()
  | Not_a_value (err, explanation) ->
    Location.error_of_printer ~loc (fun ppf () ->
      fprintf ppf "Object types must have layout value.@ %a"
        (Jkind.Violation.report_with_name ~name:"the type of this expression")
        err;
      report_type_expected_explanation_opt explanation ppf)
      ()
  | Undefined_method (ty, me, valid_methods) ->
      Location.error_of_printer ~loc (fun ppf () ->
        Printtyp.wrap_printing_env ~error:true env (fun () ->
          fprintf ppf
            "@[<v>@[This expression has type@;<1 2>%a@]@,\
             It has no method %s@]" Printtyp.type_expr ty me;
          begin match valid_methods with
            | None -> ()
            | Some valid_methods -> spellcheck ppf me valid_methods
          end
      )) ()
  | Undefined_self_method (me, valid_methods) ->
      Location.error_of_printer ~loc (fun ppf () ->
        fprintf ppf "This expression has no method %s" me;
        spellcheck ppf me valid_methods;
      ) ()
  | Virtual_class cl ->
      Location.errorf ~loc "Cannot instantiate the virtual class %a"
        longident cl
  | Unbound_instance_variable (var, valid_vars) ->
      Location.error_of_printer ~loc (fun ppf () ->
        fprintf ppf "Unbound instance variable %s" var;
        spellcheck ppf var valid_vars;
      ) ()
  | Instance_variable_not_mutable v ->
      Location.errorf ~loc "The instance variable %s is not mutable" v
  | Not_subtype err ->
      Location.error_of_printer ~loc (fun ppf () ->
        Printtyp.Subtype.report_error ppf env err "is not a subtype of"
      ) ()
  | Outside_class ->
      Location.errorf ~loc
        "This object duplication occurs outside a method definition"
  | Value_multiply_overridden v ->
      Location.errorf ~loc
        "The instance variable %s is overridden several times"
        v
  | Coercion_failure (ty_exp, err, b) ->
      Location.error_of_printer ~loc (fun ppf () ->
        Printtyp.report_unification_error ppf env err
          (function ppf ->
             let ty_exp = Printtyp.prepare_expansion ty_exp in
             fprintf ppf "This expression cannot be coerced to type@;<1 2>%a;@ \
                          it has type"
             (Printtyp.type_expansion Type) ty_exp)
          (function ppf ->
             fprintf ppf "but is here used with type");
        if b then
          fprintf ppf ".@.@[<hov>%s@ @{<hint>Hint@}: Consider using a fully \
                      explicit coercion@ %s@]"
            "This simple coercion was not fully general."
            "of the form: `(foo : ty1 :> ty2)'."
      ) ()
  | Not_a_function (ty, explanation) ->
      Location.errorf ~loc
        "This expression should not be a function,@ \
         the expected type is@ %a%t"
        Printtyp.type_expr ty
        (report_type_expected_explanation_opt explanation)
  | Too_many_arguments (ty, explanation) ->
      Location.errorf ~loc
        "This function expects too many arguments,@ \
         it should have type@ %a%t"
        Printtyp.type_expr ty
        (report_type_expected_explanation_opt explanation)
  | Abstract_wrong_label {got; expected; expected_type; explanation} ->
      let label ~long l =
        match l with
        | Nolabel -> "unlabeled"
        | Position l -> sprintf "~(%s:[%%call_pos])" l
        | Labelled _ | Optional _ ->
            (if long then "labeled " else "") ^ prefixed_label_name l
      in
      let second_long = match got, expected with
        | Nolabel, _ | _, Nolabel -> true
        | _                       -> false
      in
      let maybe_positional_argument_hint =
        match got, expected with
        | Labelled _, Position _ ->
          "\nHint: Consider explicitly annotating the label with '[%call_pos]'"
        | _ -> ""
      in
      Location.errorf ~loc
        "@[<v>@[<2>This function should have type@ %a%t@]@,\
         @[but its first argument is %s@ instead of %s%s@]%s@]"
        Printtyp.type_expr expected_type
        (report_type_expected_explanation_opt explanation)
        (label ~long:true got)
        (if second_long then "being " else "")
        (label ~long:second_long expected)
        maybe_positional_argument_hint
  | Scoping_let_module(id, ty) ->
      Location.errorf ~loc
        "This `let module' expression has type@ %a@ \
         In this type, the locally bound module name %s escapes its scope"
        Printtyp.type_expr ty id
  | Private_type ty ->
      Location.errorf ~loc "Cannot create values of the private type %a"
        Printtyp.type_expr ty
  | Private_label (lid, ty) ->
      Location.errorf ~loc "Cannot assign field %a of the private type %a"
        longident lid Printtyp.type_expr ty
  | Private_constructor (constr, ty) ->
      Location.errorf ~loc
        "Cannot use private constructor %s to create values of type %a"
        constr.cstr_name Printtyp.type_expr ty
  | Not_a_polymorphic_variant_type lid ->
      Location.errorf ~loc "The type %a@ is not a variant type" longident lid
  | Incoherent_label_order ->
      Location.errorf ~loc
        "This function is applied to arguments@ \
        in an order different from other calls.@ \
        This is only allowed when the real type is known."
  | Less_general (kind, err) ->
      report_unification_error ~loc env err
        (fun ppf -> fprintf ppf "This %s has type" kind)
        (fun ppf -> fprintf ppf "which is less general than")
  | Modules_not_allowed ->
      Location.errorf ~loc "Modules are not allowed in this pattern."
  | Cannot_infer_signature ->
      Location.errorf ~loc
        "The signature for this packaged module couldn't be inferred."
  | Not_a_packed_module ty ->
      Location.errorf ~loc
        "This expression is packed module, but the expected type is@ %a"
        Printtyp.type_expr ty
  | Unexpected_existential (reason, name, types) ->
      let reason_str =
        match reason with
        | In_class_args ->
            "Existential types are not allowed in class arguments"
        | In_class_def ->
            "Existential types are not allowed in bindings inside \
             class definition"
        | In_self_pattern ->
            "Existential types are not allowed in self patterns"
        | At_toplevel ->
            "Existential types are not allowed in toplevel bindings"
        | In_group ->
            "Existential types are not allowed in \"let ... and ...\" bindings"
        | In_rec ->
            "Existential types are not allowed in recursive bindings"
        | With_attributes ->
            "Existential types are not allowed in presence of attributes"
      in
      begin match List.find (fun ty -> ty <> "$" ^ name) types with
      | example ->
          Location.errorf ~loc
            "%s,@ but this pattern introduces the existential type %s."
            reason_str example
      | exception Not_found ->
          Location.errorf ~loc
            "%s,@ but the constructor %s introduces existential types."
            reason_str name
      end
  | Invalid_interval ->
      Location.errorf ~loc
        "@[Only character intervals are supported in patterns.@]"
  | Invalid_for_loop_index ->
      Location.errorf ~loc
        "@[Invalid for-loop index: only variables and _ are allowed.@]"
  | Invalid_comprehension_for_range_iterator_index ->
      Location.errorf ~loc
        "@[Invalid pattern in comprehension for-range iterator: \
         only variables and _ are allowed.@]"
  | No_value_clauses ->
      Location.errorf ~loc
        "None of the patterns in this 'match' expression match values."
  | Exception_pattern_disallowed ->
      Location.errorf ~loc
        "@[Exception patterns are not allowed in this position.@]"
  | Mixed_value_and_exception_patterns_under_guard ->
      Location.errorf ~loc
        "@[Mixing value and exception patterns under when-guards is not \
         supported.@]"
  | Inlined_record_escape ->
      Location.errorf ~loc
        "@[This form is not allowed as the type of the inlined record could \
         escape.@]"
  | Inlined_record_expected ->
      Location.errorf ~loc
        "@[This constructor expects an inlined record argument.@]"
  | Unrefuted_pattern pat ->
      Location.errorf ~loc
        "@[%s@ %s@ %a@]"
        "This match case could not be refuted."
        "Here is an example of a value that would reach it:"
        Printpat.top_pretty pat
  | Invalid_extension_constructor_payload ->
      Location.errorf ~loc
        "Invalid [%%extension_constructor] payload, a constructor is expected."
  | Not_an_extension_constructor ->
      Location.errorf ~loc
        "This constructor is not an extension constructor."
  | Probe_name_format name ->
      Location.errorf ~loc
        "Illegal characters in probe name `%s'. \
         Probe names may only contain alphanumeric characters or \
         underscores."
        name
  | Probe_name_undefined name ->
      Location.errorf ~loc
        "Undefined probe name `%s' used in %%probe_is_enabled. \
         Not found [%%probe \"%s\" ...] in the same compilation unit."
        name name
  | Probe_format ->
      Location.errorf ~loc
        "Probe points must consist of a name, as a string literal, \
         optionally followed by ~enabled_at_init:true or ~enabled_at_init:false, \
         followed by a single expression of type unit."
  | Probe_is_enabled_format ->
      Location.errorf ~loc
        "%%probe_is_enabled points must specify a single probe name as a \
         string literal"
  | Extension_not_enabled ext ->
    let name = Language_extension.to_string ext in
    Location.errorf ~loc
        "Extension %s must be enabled to use this feature." name
  | Literal_overflow ty ->
      Location.errorf ~loc
        "Integer literal exceeds the range of representable integers of type %s"
        ty
  | Unknown_literal (n, m) ->
      Location.errorf ~loc "Unknown modifier '%c' for literal %s%c" m n m
  | Float32_literal f ->
      Location.errorf ~loc "Found 32-bit float literal %ss, but float32 is not enabled. \
                            You must enable -extension small_numbers to use this feature." f
  | Illegal_letrec_pat ->
      Location.errorf ~loc
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      Location.errorf ~loc
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Illegal_class_expr ->
      Location.errorf ~loc
        "This kind of recursive class expression is not allowed"
  | Letop_type_clash(name, err) ->
      report_unification_error ~loc env err
        (function ppf ->
          fprintf ppf "The operator %s has type" name)
        (function ppf ->
          fprintf ppf "but it was expected to have type")
  | Andop_type_clash(name, err) ->
      report_unification_error ~loc env err
        (function ppf ->
          fprintf ppf "The operator %s has type" name)
        (function ppf ->
          fprintf ppf "but it was expected to have type")
  | Bindings_type_clash(err) ->
      report_unification_error ~loc env err
        (function ppf ->
          fprintf ppf "These bindings have type")
        (function ppf ->
          fprintf ppf "but bindings were expected of type")
  | Unbound_existential (ids, ty) ->
      Location.errorf ~loc
        "@[<2>%s:@ @[type %s.@ %a@]@]"
        "This type does not bind all existentials in the constructor"
        (String.concat " " (List.map Ident.name ids))
        Printtyp.type_expr ty
  | Missing_type_constraint ->
      Location.errorf ~loc
        "@[%s@ %s@]"
        "Existential types introduced in a constructor pattern"
        "must be bound by a type constraint on the argument."
  | Wrong_expected_kind(sort, ctx, ty) ->
      let ctx, explanation =
        match ctx with
        | Expression explanation -> "expression", explanation
        | Pattern -> "pattern", None
      in
      let sort =
        match sort with
        | Constructor -> "constructor"
        | Boolean -> "boolean literal"
        | List -> "list literal"
        | Unit -> "unit literal"
        | Record -> "record"
      in
      Location.errorf ~loc
        "This %s should not be a %s,@ \
         the expected type is@ %a%t"
        ctx sort Printtyp.type_expr ty
        (report_type_expected_explanation_opt explanation)
  | Expr_not_a_record_type ty ->
      Location.errorf ~loc
        "This expression has type %a@ \
         which is not a record type."
        Printtyp.type_expr ty
  | Submode_failed(fail_reason, submode_reason, closure_context,
      contention_context, shared_context)
     ->
      let sub =
        match fail_reason with
        | Error (Comonadic Linearity, _) | Error (Monadic Uniqueness, _) ->
            shared_context
          |> Option.map
              (fun context -> Location.mknoloc
                (fun ppf -> Env.sharedness_hint ppf context))
          |> Option.to_list
        | Error (Comonadic Areality, _) ->
          escaping_hint fail_reason submode_reason closure_context
        | Error (Monadic Contention, _ ) ->
          contention_hint fail_reason submode_reason contention_context
        | Error (Comonadic Portability, _ ) -> []
      in
      Location.errorf ~loc ~sub "@[%t@]" begin
        match fail_reason with
        | Error (Comonadic Areality, _) ->
            Format.dprintf "This value escapes its region."
        | Error (ax, {left; right}) ->
            Format.dprintf "This value is %a but expected to be %a."
              (Value.Const.print_axis ax) left (Value.Const.print_axis ax) right
        end
  | Local_application_complete (lbl, loc_kind) ->
      let sub =
        match loc_kind with
        | `Prefix ->
          [Location.msg
             "@[Hint: Try wrapping the marked application in parentheses.@]"]
        | `Single_arg ->
          [Location.msg
             "@[Hint: Try splitting the application in two. The arguments that come@ \
              after this one in the function's type should be applied separately.@]"]
        | `Entire_apply ->
          let lbl =
            match lbl with
            | Nolabel -> "_"
            | Labelled s | Optional s | Position s -> s
          in
          [Location.msg
             "@[Hint: Try splitting the application in two. The arguments that come@ \
              after %s in the function's type should be applied separately.@]" lbl]
      in
      Location.errorf ~loc ~sub
        "@[This application is complete, but surplus arguments were provided afterwards.@ \
         When passing or calling a local value, extra arguments are passed in a separate application.@]"
  | Param_mode_mismatch (s, Error (ax, {left; right})) ->
      let actual, expected =
        match s with
        | Left_le_right -> left, right
        | Right_le_left -> right, left
      in
      Location.errorf ~loc
        "@[This function takes a parameter which is %a,@ \
        but was expected to take a parameter which is %a.@]"
        (Alloc.Const.print_axis ax) actual (Alloc.Const.print_axis ax) expected
  | Uncurried_function_escapes e -> begin
      match e with
      | Error (Comonadic Areality, _) ->
          Location.errorf ~loc
            "This function or one of its parameters escape their region@ \
            when it is partially applied."
      | Error (ax, {left; right}) ->
          Location.errorf ~loc
            "This function when partially applied returns a value which is %a,@ \
              but expected to be %a."
            (Alloc.Const.print_axis ax) left
            (Alloc.Const.print_axis ax) right
    end
  | Local_return_annotation_mismatch _ ->
      Location.errorf ~loc
        "This function return is not annotated with \"local_\"@ \
         whilst other returns were."
  | Bad_tail_annotation err ->
      Location.errorf ~loc
        "The tail-call annotation on this application %s."
        (match err with
         | `Conflict -> "is contradictory"
         | `Not_a_tailcall -> "is not on a tail call")
  | Exclave_in_nontail_position ->
      Location.errorf ~loc
        "Exclave expression should only be in tail position of the current region."
  | Exclave_returns_not_local ->
      Location.errorf ~loc
        "This expression was expected to be not local, but is an exclave expression,@ \
         which must be local."
  | Optional_poly_param ->
      Location.errorf ~loc
        "Optional parameters cannot be polymorphic"
  | Function_returns_local ->
      Location.errorf ~loc
        "This function is local-returning, but was expected otherwise."
  | Tail_call_local_returning ->
      Location.errorf ~loc
        "@[This application is local-returning, but is at the tail@ \
          position of a function that is not local-returning.@]"
  | Unboxed_int_literals_not_supported ->
      Location.errorf ~loc
        "@[Unboxed int literals aren't supported yet.@]"
  | Function_type_not_rep (ty,violation) ->
      Location.errorf ~loc
        "@[Function arguments and returns must be representable.@]@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) violation
  | Modes_on_pattern ->
      Location.errorf ~loc
        "@[Mode annotations on patterns are not supported yet.@]"
  | Invalid_label_for_src_pos arg_label ->
      Location.errorf ~loc
        "A position argument must not be %s."
        (match arg_label with
        | Nolabel -> "unlabelled"
        | Optional _ -> "optional"
        | Labelled _ | Position _ -> assert false )
  | Nonoptional_call_pos_label label ->
    Location.errorf ~loc
      "@[the argument labeled '%s' is a [%%call_pos] argument, filled in @ \
         automatically if ommitted. It cannot be passed with '?'.@]" label

let report_error ~loc env err =
  Printtyp.wrap_printing_env_error env
    (fun () -> report_error ~loc env err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (report_error ~loc env err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )

let () =
  Persistent_env.add_delayed_check_forward := add_delayed_check;
  Env.add_delayed_check_forward := add_delayed_check;
  ()

(* drop the need to call [Parmatch.typed_case] from the external API *)
let check_partial ?lev a b c cases =
  check_partial ?lev a b c (List.map Parmatch.typed_case cases)

(* drop unnecessary arguments from the external API
   and check for uniqueness *)
let type_expect env e ty =
  let exp = type_expect env mode_legacy e ty in
  maybe_check_uniqueness_exp exp; exp

let type_exp env e =
  let exp = type_exp env mode_legacy e in
  maybe_check_uniqueness_exp exp; exp

let type_argument env e t1 t2 =
  let exp = type_argument env mode_legacy e t1 t2 in
  maybe_check_uniqueness_exp exp; exp

let type_option_some env e t1 t2 =
  let exp = type_option_some env mode_legacy e t1 t2 in
  maybe_check_uniqueness_exp exp; exp
