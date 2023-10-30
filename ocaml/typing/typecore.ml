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
  | Array_comprehension of mutable_flag

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

type error =
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * Errortrace.unification_error
  | Pattern_type_clash :
      Errortrace.unification_error * Parsetree.pattern_desc option -> error
  | Or_pattern_type_clash of Ident.t * Errortrace.unification_error
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of
      Errortrace.unification_error * type_forcing_context option
      * Parsetree.expression_desc option
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
      Env.shared_context option
  | Local_application_complete of Asttypes.arg_label * [`Prefix|`Single_arg|`Entire_apply]
  | Param_mode_mismatch of type_expr * Alloc.error
  | Uncurried_function_escapes of Alloc.error
  | Local_return_annotation_mismatch of Location.t
  | Function_returns_local
  | Tail_call_local_returning
  | Bad_tail_annotation of [`Conflict|`Not_a_tailcall]
  | Optional_poly_param
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Unboxed_int_literals_not_supported
  | Unboxed_float_literals_not_supported
  | Function_type_not_rep of type_expr * Jkind.Violation.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

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
  | RTail of Regionality.t * position_in_function

type expected_mode =
  { position : position_in_region;
    closure_context : Env.closure_context option;
    (* the upper bound of mode*)
    mode : Value.t;
    (* in some scnearios, the above `mode` will be the exact mode of the
        expression to be typed, indicated by the `exact` field.

    - In any case, there is no risk of miscompilation in taking an upper bound
    as exact. We might lose some range and trigger some false mode errors.

    - Taking an exact as upper bound could cause issues. In particular
    for the inner function of an uncurried function.

    Therefore, if we just take it as exact regardless of the `exact`
    field, we should be safe. Moreover, note that for most allocations, they
    want to use expected_mode.mode as exact anyway, because that would be the
    only constraint and they want to be as local as possible. The only exception
    is uncurried functions where the mode constraints are tricky.
    *)
    exact : bool;

    (* Indicates that the expression was directly annotated with [local], which
    should force any allocations to be on the stack. If [true] the [mode] field
    must be greater than [local]. *)
    strictly_local : bool;

    tuple_modes : Value.t list;
    (* for t in tuple_modes, t <= regional_to_global mode *)
  }

type position_and_mode = {
  apply_position : apply_position;
  (** Runtime tail call behaviour of the application *)
  region_mode : Regionality.t option;
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
        Regionality.submode (Regionality.of_locality ap_mode) region_mode
      with
      | Ok () -> ()
      | Error _ -> raise (Error (loc, env, Tail_call_local_returning))
    end
  | None -> ()

(* Describes how a modality affects field projection. Returns the mode
   of the projection given the mode of the record. *)
let _modality_unbox_left global_flag mode =
  match global_flag with
  | Global ->
      mode |> Value.to_global |> Value.to_shared |> Value.to_many
  | Unrestricted -> mode

(* Describes how a modality affects record construction. Gives the
   expected mode of the field given the expected mode of the record. *)
let _modality_box_right global_flag mode =
  match global_flag with
  | Global ->
      mode |> Value.to_global |> Value.to_shared |> Value.to_many
  | Unrestricted -> mode

(* Describes how a modality affects field projection. Returns the mode
   of the projection given the mode of the record. *)
let modality_unbox_left global_flag mode =
  match global_flag with
  | Global ->
      mode |> Value.to_global |> Value.to_shared |> Value.to_many
  | Unrestricted -> mode

(* Describes how a modality affects record construction. Gives the
   expected mode of the field given the expected mode of the record. *)
let modality_box_right global_flag mode =
  match global_flag with
  | Global ->
      mode |> Value.to_global |> Value.to_shared |> Value.to_many
  | Unrestricted -> mode

let mode_default mode =
  { position = RNontail;
    closure_context = None;
    mode = mode;
    exact = false;
    strictly_local = false;
    tuple_modes = [] }

let mode_legacy = mode_default Value.legacy

(* used when entering a function;
mode is the mode of the function region *)
let mode_return mode =
  { (mode_default (Value.local_to_regional mode)) with
    position = RTail (Value.locality mode, FTail);
    closure_context = Some Return;
  }

(* used when entering a region.*)
let mode_region mode =
  { (mode_default (Value.local_to_regional mode)) with
    position = RTail (Value.locality mode, FNontail);
    closure_context = None;
  }

let mode_max =
  mode_default Value.max_mode

let mode_with_position mode position =
  { (mode_default mode) with position }

let mode_max_with_position position =
  { mode_max with position }

let mode_subcomponent expected_mode =
  mode_default (Value.regional_to_global expected_mode.mode)

let mode_box_modality gf expected_mode =
  mode_default (modality_box_right gf expected_mode.mode)

let mode_global expected_mode =
  { expected_mode with
    mode = Value.to_global expected_mode.mode }

let mode_local expected_mode =
  { expected_mode with
    mode = Value.to_local expected_mode.mode }

let mode_exclave expected_mode =
  { (mode_default (Value.to_local expected_mode.mode))
    with strictly_local = true
  }

let mode_strictly_local expected_mode =
  { (mode_local expected_mode)
    with strictly_local = true
  }

let mode_unique expected_mode =
  { expected_mode with
    mode = Value.to_unique expected_mode.mode }

let mode_once expected_mode =
  { expected_mode with
    mode = Value.to_once expected_mode.mode }

let mode_tailcall_function mode =
  { (mode_default mode) with
    closure_context = Some Tailcall_function }

let mode_tailcall_argument mode =
  { (mode_default mode) with
    closure_context = Some Tailcall_argument }


let mode_partial_application expected_mode =
  { (mode_default (Value.regional_to_global expected_mode.mode)) with
    closure_context = Some Partial_application }


let mode_trywith expected_mode =
  { expected_mode with position = RNontail }

let mode_tuple mode tuple_modes =
  { (mode_default mode) with
    tuple_modes }

let mode_exact mode =
  { (mode_default mode) with
    exact = true }

let mode_argument ~funct ~index ~position_and_mode ~partial_app alloc_mode =
  let vmode = Value.of_alloc alloc_mode in
  if partial_app then mode_default vmode
  else match funct.exp_desc, index, position_and_mode.apply_position with
  | Texp_ident (_, _, {val_kind =
      Val_prim {Primitive.prim_name = ("%sequor"|"%sequand")}},
                Id_prim _, _), 1, Tail ->
     (* RHS of (&&) and (||) is at the tail of function region if the
        application is. The argument mode is not constrained otherwise. *)
     mode_with_position vmode (RTail (Option.get position_and_mode.region_mode, FTail))
  | Texp_ident (_, _, _, Id_prim _, _), _, _ ->
     (* Other primitives cannot be tail-called *)
     mode_default vmode
  | _, _, (Nontail | Default) ->
     mode_default vmode
  | _, _, Tail ->
     mode_tailcall_argument (Value.local_to_regional vmode)

let mode_lazy expected_mode =
  { (mode_global expected_mode) with
    position = RTail (Regionality.global, FTail) }

(* expected_mode.closure_context explains why expected_mode.mode is low;
   shared_context explains why mode.uniqueness is high *)
let submode ~loc ~env ?(reason = Other) ?shared_context mode expected_mode =
  let res =
    match expected_mode.tuple_modes with
    | [] -> Value.submode mode expected_mode.mode
    | ts -> Value.submode_meet mode ts
  in
  match res with
  | Ok () -> ()
  | Error failure_reason ->
      let closure_context = expected_mode.closure_context in
      let error =
        Submode_failed(failure_reason, reason, closure_context, shared_context)
      in
      raise (Error(loc, env, error))

let escape ~loc ~env ~reason m =
  submode ~loc ~env ~reason m mode_legacy

type expected_pat_mode =
  { mode : Value.t;
    tuple_modes : Value.t list; }

let simple_pat_mode mode =
  { mode; tuple_modes = [] }

let tuple_pat_mode mode tuple_modes =
  { mode; tuple_modes }

let allocations : Alloc.t list ref = Local_store.s_ref []

let reset_allocations () = allocations := []

let register_allocation_mode alloc_mode =
  if not (Alloc.is_const alloc_mode) then
   allocations := alloc_mode :: !allocations

let register_allocation_value_mode mode =
  let alloc_mode = Value.regional_to_global_alloc mode in
  register_allocation_mode alloc_mode;
  alloc_mode

let register_allocation (expected_mode : expected_mode) =
  register_allocation_value_mode expected_mode.mode

let optimise_allocations () =
  List.iter
    (fun mode -> ignore (Alloc.constrain_upper mode))
    !allocations;
  reset_allocations ()

(* Typing of constants *)

let type_constant = function
    Const_int _ -> instance Predef.type_int
  | Const_char _ -> instance Predef.type_char
  | Const_string _ -> instance Predef.type_string
  | Const_float _ -> instance Predef.type_float
  | Const_int32 _ -> instance Predef.type_int32
  | Const_int64 _ -> instance Predef.type_int64
  | Const_nativeint _ -> instance Predef.type_nativeint

let type_constant_unboxed env loc
    : Jane_syntax.Layouts.constant -> _ = function
  | Float _ -> raise (Error (loc, env, Unboxed_float_literals_not_supported))
    (* CR layouts v2.5: This should be [instance Predef.type_unboxed_float] *)
  | Integer _ -> raise (Error (loc, env, Unboxed_int_literals_not_supported))

let constant_integer i ~suffix : (Asttypes.constant, error) result =
  match suffix with
  | 'l' ->
    begin
      try Ok (Const_int32 (Misc.Int_literal_converter.int32 i))
      with Failure _ -> Error (Literal_overflow "int32")
    end
  | 'L' ->
    begin
      try Ok (Const_int64 (Misc.Int_literal_converter.int64 i))
      with Failure _ -> Error (Literal_overflow "int64")
    end
  | 'n' ->
    begin
      try Ok (Const_nativeint (Misc.Int_literal_converter.nativeint i))
      with Failure _ -> Error (Literal_overflow "nativeint")
    end
  | c -> Error (Unknown_literal (i, c))

let constant : Parsetree.constant -> (Asttypes.constant, error) result =
  function
  | Pconst_integer (i, Some suffix) -> constant_integer i ~suffix
  | Pconst_integer (i,None) ->
     begin
       try Ok (Const_int (Misc.Int_literal_converter.int i))
       with Failure _ -> Error (Literal_overflow "int")
     end
  | Pconst_char c -> Ok (Const_char c)
  | Pconst_string (s,loc,d) -> Ok (Const_string (s,loc,d))
  | Pconst_float (f,None)-> Ok (Const_float f)
  | Pconst_float (f,Some c) -> Error (Unknown_literal (f, c))

let constant_or_raise env loc cst =
  match constant cst with
  | Ok c -> c
  | Error err -> raise (Error (loc, env, err))

let unboxed_constant :
    type a. Jane_syntax.Layouts.constant -> (a, error) result
  = function
  | Float (_, None) -> Error Unboxed_float_literals_not_supported
  | Float (x, Some c) -> Error (Unknown_literal ("#" ^ x, c))
  | Integer (_, _) -> Error Unboxed_int_literals_not_supported

(* CR layouts v2.5: this is missing the part where we actually typecheck
   unboxed literals.
*)
let unboxed_constant_or_raise env loc cst =
  let open struct
    type nothing = |
  end in
  match unboxed_constant cst with
  | Ok (_ : nothing) -> .
  | Error err -> raise (Error (loc, env, err))

(* Specific version of type_option, using newty rather than newgenty *)

let type_option ty =
  newty (Tconstr(Predef.path_option,[ty], ref Mnil))

let mkexp exp_desc exp_type exp_loc exp_env =
  { exp_desc; exp_type;
    exp_loc; exp_env; exp_extra = []; exp_attributes = [] }

let option_none env ty loc =
  let lid = Longident.Lident "None" in
  let cnone = Env.find_ident_constructor Predef.ident_none env in
  mkexp (Texp_construct(mknoloc lid, cnone, [], None)) ty loc env

let option_some env texp mode =
  let alloc_mode  = register_allocation_value_mode mode in
  let lid = Longident.Lident "Some" in
  let csome = Env.find_ident_constructor Predef.ident_some env in
  mkexp (Texp_construct(mknoloc lid , csome, [texp], Some alloc_mode))
    (type_option texp.exp_type) texp.exp_loc texp.exp_env

let extract_option_type env ty =
  match get_desc (expand_head env ty) with
    Tconstr(path, [ty], _) when Path.same path Predef.path_option -> ty
  | _ -> assert false

let protect_expansion env ty =
  if Env.has_local_constraints env then generic_instance ty else ty

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

let has_local_attr loc attrs =
  match Builtin_attributes.has_local attrs with
  | Ok l -> l
  | Error () ->
     raise(Typetexp.Error(loc, Env.empty, Unsupported_extension Local))

let has_local_attr_pat ppat =
  has_local_attr ppat.ppat_loc ppat.ppat_attributes

let has_local_attr_exp pexp =
  has_local_attr pexp.pexp_loc pexp.pexp_attributes

let has_poly_constraint spat =
  match spat.ppat_desc with
  | Ppat_constraint(_, styp) -> begin
      match styp.ptyp_desc with
      | Ptyp_poly _ -> true
      | _ -> false
    end
  | _ -> false

let mode_cross_to_min env ty mode =
  if mode_cross env ty then
    Value.min_mode
  else
    mode

let expect_mode_cross env ty (expected_mode : expected_mode) =
  if mode_cross env ty then
    { expected_mode with
      mode = Value.max_mode;
      exact = false;
      strictly_local = false }
  else expected_mode

let has_unique_attr loc attrs =
  match Builtin_attributes.has_unique attrs with
  | Ok l -> l
  | Error () ->
      raise(Typetexp.Error(loc, Env.empty, Unsupported_extension Unique))

let has_once_attr loc attrs =
  match Builtin_attributes.has_once attrs with
  | Ok l -> l
  | Error () ->
      raise(Typetexp.Error(loc, Env.empty, Unsupported_extension Unique))

let has_unique_attr_pat ppat =
  has_unique_attr ppat.ppat_loc ppat.ppat_attributes

let has_unique_attr_exp pexp =
  has_unique_attr pexp.pexp_loc pexp.pexp_attributes

let has_once_attr_pat ppat =
  has_once_attr ppat.ppat_loc ppat.ppat_attributes

let has_once_attr_exp pexp =
  has_once_attr pexp.pexp_loc pexp.pexp_attributes

let mode_annots_none =
  {locality = None; uniqueness = None; linearity = None}

let mode_annots_from_pat_attrs sp =
  let locality =
    if has_local_attr_pat sp then Some Locality.Const.Local
    else None
  and uniqueness =
    if has_unique_attr_pat sp then Some Uniqueness.Const.Unique
    else None
  and linearity =
    if has_once_attr_pat sp then Some Linearity.Const.Once
    else None
  in
  {locality; uniqueness; linearity}

let mode_annots_or_default annot ~default =
  let locality = Option.value annot.locality ~default:default.locality in
  let uniqueness = Option.value annot.uniqueness ~default:default.uniqueness in
  let linearity = Option.value annot.linearity ~default:default.linearity in
  {locality; uniqueness; linearity}

let mode_annots_from_exp_attrs exp =
  let locality =
    if has_local_attr_exp exp then Some Locality.Const.Local
    else None
  and uniqueness =
    if has_unique_attr_exp exp then Some Uniqueness.Const.Unique
    else None
  and linearity =
    if has_once_attr_exp exp then Some Linearity.Const.Once
    else None
  in
  {locality; uniqueness; linearity}

let apply_mode_annots ~loc ~env ~ty_expected ann mode =
  let error axis =
    raise (Error(loc, env, Param_mode_mismatch (ty_expected, axis)))
  in
  Option.iter (fun locality ->
    match Locality.equate (Locality.of_const locality) (Alloc.locality mode) with
    | Ok () -> ()
    | Error () -> error `Locality
    ) ann.locality;
  Option.iter (fun uniqueness ->
    match Uniqueness.equate (Uniqueness.of_const uniqueness) (Alloc.uniqueness mode) with
    | Ok () -> ()
    | Error () -> error `Uniqueness
    ) ann.uniqueness;
  Option.iter (fun linearity ->
    match Linearity.equate (Linearity.of_const linearity) (Alloc.linearity mode) with
    | Ok () -> ()
    | Error () -> error `Linearity
    ) ann.linearity

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
    pv_mode: Value.t;
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
    (fun {pv_id; pv_uid; pv_mode; pv_type; pv_loc; pv_as_var; pv_attributes} env ->
       let check = if pv_as_var then check_as else check in
       Env.add_value ?check ~mode:pv_mode pv_id
         {val_type = pv_type; val_kind = Val_reg; Types.val_loc = pv_loc;
          val_attributes = pv_attributes;
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
     pv_mode = mode;
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
              unify_var env (newvar (Jkind.any ~why:Dummy_jkind)) t1;
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
      let tyl = List.map (build_as_type env) pl in
      newty (Ttuple tyl), mode
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
          (fun (p,ty) (arg, _) ->
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
                           ~more:(newvar (Jkind.value ~why:Row_variable))
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
      let ty = newvar (Jkind.any ~why:Dummy_jkind) in
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
                                        (Jkind.value ~why:Row_variable))))
          in
          ty, mode
      end
  | Tpat_constant _ ->
      let mode =
        if Ctype.is_immediate !env p.pat_type
        then Value.newvar ()
        else mode
      in
      p.pat_type, mode
  | Tpat_any | Tpat_var _
  | Tpat_array _ | Tpat_lazy _ ->
      p.pat_type, mode

(* Constraint solving during typing of patterns *)

let solve_Ppat_alias ~refine ~mode env pat =
  with_local_level ~post:(fun (ty_var, _) -> generalize ty_var)
    (fun () -> build_as_type_and_mode ~refine ~mode env pat)

let solve_Ppat_tuple (type a) ~refine ~alloc_mode loc env (args : a list) expected_ty =
  let arity = List.length args in
  assert (arity >= 2);
  let arg_modes =
    if List.compare_length_with alloc_mode.tuple_modes arity = 0 then
      alloc_mode.tuple_modes
    else
      List.init arity (fun _ -> alloc_mode.mode)
  in
  let ann =
    (* CR layouts v5: restriction to value here to be relaxed. *)
    List.map2
      (fun p mode -> (p, newgenvar (Jkind.value ~why:Tuple_element),
                      simple_pat_mode mode))
      args arg_modes
  in
  let ty = newgenty (Ttuple (List.map snd3 ann)) in
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
                     (Jkind.value ~why:Existential_type_variable) in
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
        unify_pat_types cty.ctyp_loc env ty1 (newty (Ttuple ty_args));
        match get_desc (expand_head !env ty2) with
          Ttuple tyl -> tyl
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
            let ty_args_ty, ty_args_gf = List.split ty_args in
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
            let ty_args_ty, ty_args_gf = List.split ty_args in
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
  let type_some_array = match mutability with
    | Immutable -> Predef.type_iarray
    | Mutable -> Predef.type_array
  in
  (* CR layouts v4: The code below is written this way to make it easier to update
     when we generalize array to contain non-value jkinds. When that happens,
     change the next two lines to use [Jkind.of_new_sort_var]. *)
  let jkind = Jkind.value ~why:Array_element in
  let arg_sort = Jkind.sort_of_jkind jkind in
  let ty_elt = newgenvar jkind in
  let expected_ty = generic_instance expected_ty in
  unify_pat_types ~refine
    loc env (type_some_array ty_elt) expected_ty;
  ty_elt, arg_sort

let solve_Ppat_lazy  ~refine loc env expected_ty =
  let nv = newgenvar (Jkind.value ~why:Lazy_expression) in
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
    else [newgenvar (Jkind.value ~why:Polymorphic_variant_field)]
  in
  let fields = [tag, rf_either ~no_arg arg_type ~matched:true] in
  let make_row more =
    create_row ~fields ~closed:false ~more ~fixed:None ~name:None
  in
  let row = make_row (newgenvar (Jkind.value ~why:Row_variable)) in
  let expected_ty = generic_instance expected_ty in
  (* PR#7404: allow some_private_tag blindly, as it would not unify with
     the abstract row variable *)
  if tag <> Parmatch.some_private_tag then
    unify_pat_types ~refine loc env (newgenty(Tvariant row)) expected_ty;
  (arg_type, make_row (newvar (Jkind.value ~why:Row_variable)),
   instance expected_ty)

(* Building the or-pattern corresponding to a polymorphic variant type *)
let build_or_pat env loc lid =
  let path, decl = Env.lookup_type ~loc:lid.loc lid.txt env in
  (* CR layouts: the use of value here is wrong:
     there could be other jkinds in a polymorphic variant argument;
     see Test 24 in tests/typing-layouts/basics_alpha.ml *)
  let tyl = List.map (fun _ -> newvar (Jkind.value ~why:Type_argument))
              decl.type_params in
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
                                 (Jkind.value ~why:Row_variable))))
  in
  let gloc = Location.ghostify loc in
  let row' = ref (make_row (newvar (Jkind.value ~why:Row_variable))) in
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

let split_cases env cases =
  let add_case lst case = function
    | None -> lst
    | Some c_lhs -> { case with c_lhs } :: lst
  in
  List.fold_right (fun ({ c_lhs; c_guard } as case) (vals, exns) ->
    match split_pattern c_lhs with
    | Some _, Some _ when c_guard <> None ->
      raise (Error (c_lhs.pat_loc, env,
                    Mixed_value_and_exception_patterns_under_guard))
    | vp, ep -> add_case vals case vp, add_case exns case ep
  ) cases ([], [])

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
          ~pv_mode:Value.min_mode
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
              { pv_id; pv_uid; pv_mode; pv_type; pv_loc; pv_as_var; pv_attributes }
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
    | Record_boxed _ | Record_float | Record_ufloat | Record_unboxed -> true
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
let disambiguate_lid_a_list loc closed env usage expected_type lid_a_list =
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
  lbl_a_list

let map_fold_cont f xs k =
  List.fold_right (fun x k ys -> f x (fun y -> k (y :: ys)))
    xs (fun ys -> k (List.rev ys)) []

let type_label_a_list loc closed env usage type_lbl_a expected_type lid_a_list =
  let lbl_a_list =
    disambiguate_lid_a_list loc closed env usage expected_type lid_a_list
  in
  (* Invariant: records are sorted in the typed tree *)
  let lbl_a_list =
    List.sort
      (fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_num lbl2.lbl_num)
      lbl_a_list
  in
  List.map type_lbl_a lbl_a_list

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

(* "half typed" cases are produced in [type_cases] when we've just typechecked
   the pattern but haven't type-checked the body yet.
   At this point we might have added some type equalities to the environment,
   but haven't yet added identifiers bound by the pattern. *)
type 'case_pattern half_typed_case =
  { typed_pat: 'case_pattern;
    pat_type_for_unif: type_expr;
    untyped_case: Parsetree.case;
    branch_env: Env.t;
    pat_vars: pattern_variable list;
    module_vars: module_variables;
    contains_gadt: bool; }

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
    let alloc_mode =
      match mutability with
      | Mutable -> simple_pat_mode Value.legacy
      | Immutable -> alloc_mode
    in
    let pl = List.map (fun p -> type_pat ~alloc_mode tps Value p ty_elt) spl in
    rvp {
      pat_desc = Tpat_array (mutability, arg_sort, pl);
      pat_loc = loc; pat_extra=[];
      pat_type = instance expected_ty;
      pat_attributes;
      pat_env = !env }
  in
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
          let desc = unboxed_constant_or_raise !env loc cst in
          rvp @@ solve_expected {
            pat_desc = desc;
            pat_loc = loc; pat_extra=[];
            pat_type = type_constant_unboxed !env loc cst;
            pat_attributes = attrs;
            pat_env = !env }
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
      let alloc_mode = mode_cross_to_min !env expected_ty alloc_mode.mode in
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
      let mode = mode_cross_to_min !env expected_ty mode in
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
      let spl_ann =
        solve_Ppat_tuple ~refine ~alloc_mode loc env spl expected_ty
      in
      let pl =
        List.map (fun (p,t,alloc_mode) -> type_pat tps Value ~alloc_mode p t)
          spl_ann
      in
      rvp {
        pat_desc = Tpat_tuple pl;
        pat_loc = loc; pat_extra=[];
        pat_type = newty (Ttuple(List.map (fun p -> p.pat_type) pl));
        pat_attributes = sp.ppat_attributes;
        pat_env = !env }
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
        | Some {ppat_desc = Ppat_tuple spl} when
            constr.cstr_arity > 1 ||
            Builtin_attributes.explicit_arity sp.ppat_attributes
          -> spl
        | Some({ppat_desc = Ppat_any} as sp) when
            constr.cstr_arity = 0 && existential_styp = None
          ->
            Location.prerr_warning sp.ppat_loc
              Warnings.Wildcard_arg_to_constant_constr;
            []
        | Some({ppat_desc = Ppat_any} as sp) when constr.cstr_arity > 1 ->
            replicate_list sp constr.cstr_arity
        | Some sp -> [sp] in
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
             let alloc_mode = modality_unbox_left gf alloc_mode.mode in
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
          None, newvar (Jkind.value ~why:Boxed_record)
        | Not_a_record_type ->
          let error = Wrong_expected_kind(Record, Pattern, expected_ty) in
          raise (Error (loc, !env, error))
      in
      let type_label_pat (label_lid, label, sarg) =
        let ty_arg =
          solve_Ppat_record_field ~refine loc env label label_lid record_ty in
        let alloc_mode =
          modality_unbox_left label.lbl_global alloc_mode.mode
        in
        let alloc_mode = simple_pat_mode alloc_mode in
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
          (type_label_a_list loc false !env Env.Projection
             type_label_pat expected_type)
          lid_sp_list
      in
      rvp @@ solve_expected (make_record_pat lbl_a_list)
  | Ppat_array spl ->
      type_pat_array Mutable spl sp.ppat_attributes
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
        let mode_annots = mode_annots_from_pat_attrs sp in
        let type_modes =
          mode_annots_or_default mode_annots ~default:Alloc.Const.legacy
        in
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
      let nv = newvar (Jkind.value ~why:Class_argument) in
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
          (type_option (newvar (Jkind.value ~why:Type_argument)));
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
          Env.add_value pv_id
            { val_type = pv_type
            ; val_kind = Val_reg
            ; val_attributes = pv_attributes
            ; val_loc = pv_loc
            ; val_uid = pv_uid
            }
            val_env
         in
         let met_env =
          Env.add_value id' ~check
            { val_type = pv_type
            ; val_kind = Val_ivar (Immutable, cl_num)
            ; val_attributes = pv_attributes
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
  let nv = newvar (Jkind.value ~why:Object) in
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
  let alloc_mode = simple_pat_mode Value.min_mode in
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
      let cst = constant_or_raise !env loc (Untypeast.constant cst) in
      k @@ solve_expected (mp (Tpat_constant cst) ~pat_type:(type_constant cst))
  | Tpat_tuple tpl ->
      let tpl_ann =
        solve_Ppat_tuple ~refine ~alloc_mode loc env tpl
          expected_ty
      in
      map_fold_cont (fun (p,t,_) -> check_rec p t) tpl_ann
        (fun pl ->
           mkp k (Tpat_tuple pl)
             ~pat_type:(newty (Ttuple(List.map (fun p -> p.pat_type) pl))))
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
  | Tpat_array (mut, tpl) ->
      let ty_elt = solve_Ppat_array ~refine loc env mut expected_ty in
      map_fold_cont (fun p -> check_rec p ty_elt) tpl
        (fun pl -> mkp k (Tpat_array (mut, pl)))
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
        sort_arg : Jkind.sort;
        commuted : bool;
        mode_fun : Alloc.t;
        mode_arg : Alloc.t;
        wrapped_in_some : bool; }
  | Unknown_arg of
      { sarg : Parsetree.expression;
        ty_arg_mono : type_expr;
        sort_arg : Jkind.sort;
        mode_fun : Alloc.t;
        mode_arg : Alloc.t}
  | Eliminated_optional_arg of
      { mode_fun: Alloc.t;
        ty_arg : type_expr;
        sort_arg : Jkind.sort;
        mode_arg : Alloc.t;
        level: int;
        next_arg_loc: Location.t option }

type untyped_omitted_param =
  { mode_fun: Alloc.t;
    ty_arg : type_expr;
    mode_arg : Alloc.t;
    level: int;
    sort_arg : Jkind.sort }

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
                  { mode_fun; ty_arg; mode_arg; level })
         | Omitted { mode_fun; ty_arg; mode_arg; level } ->
             let arrow_desc = lbl, mode_arg, mode_ret in
             let ty_ret =
               newty2 ~level
                 (Tarrow (arrow_desc, ty_arg, ty_ret, commu_ok))
             in
             let mode_ret =
               Alloc.join (mode_fun :: closed_args)
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
    || !Clflags.classic && arg = Nolabel && not (is_optional param)
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
                  rev_args
                  |> List.find_map
                       (function
                         | (_, Arg ( Known_arg { sarg; _ }
                                   | Unknown_arg { sarg; _ })) ->
                           Some sarg.pexp_loc
                         | (_,
                            Arg (Eliminated_optional_arg { next_arg_loc })) ->
                           next_arg_loc
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
        and optional = is_optional l in
        let use_arg ~commuted sarg l' =
          let wrapped_in_some = optional && not (is_optional l') in
          if wrapped_in_some then
            may_warn sarg.pexp_loc
              (Warnings.Not_principal "using an optional argument here");
          Arg (Known_arg
            { sarg; ty_arg; ty_arg0; commuted; sort_arg;
              mode_fun; mode_arg; wrapped_in_some })
        in
        let eliminate_optional_arg next_arg_loc =
          may_warn funct.exp_loc
            (Warnings.Non_principal_labels "eliminated optional argument");
          Arg
            (Eliminated_optional_arg
               { mode_fun; ty_arg; mode_arg; sort_arg; level = lv;
                 next_arg_loc })
        in
        let remaining_sargs, arg =
          if ignore_labels then begin
            (* No reordering is allowed, process arguments in order *)
            match sargs with
            | [] -> assert false
            | (l', sarg) :: remaining_sargs ->
                if name = label_name l' || (not optional && l' = Nolabel) then
                  (remaining_sargs, use_arg ~commuted:false sarg l')
                else if
                  optional &&
                  not (List.exists (fun (l, _) -> name = label_name l)
                         remaining_sargs) &&
                  List.exists (function (Nolabel, _) -> true | _ -> false)
                    sargs
                then
                  (sargs, eliminate_optional_arg (Some sarg.pexp_loc))
                else
                  raise(Error(sarg.pexp_loc, env,
                              Apply_wrong_label(l', ty_fun', optional)))
          end else
            (* Arguments can be commuted, try to fetch the argument
               corresponding to the first parameter. *)
            match extract_label name sargs with
            | Some (l', sarg, commuted, remaining_sargs) ->
                if commuted then begin
                  may_warn sarg.pexp_loc
                    (Warnings.Not_principal "commuting this argument")
                end;
                if not optional && is_optional l' then
                  Location.prerr_warning sarg.pexp_loc
                    (Warnings.Nonoptional_label (Printtyp.string_of_label l));
                remaining_sargs, use_arg ~commuted sarg l'
            | None ->
                sargs,
                if optional && List.mem_assoc Nolabel sargs then
                  eliminate_optional_arg None
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
         | Arg (exp, exp_mode, sort) ->
             let open_args = (exp_mode, exp) :: open_args in
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
                 (fun (marg, exp) ->
                    submode ~loc:exp.exp_loc ~env ~reason:Other
                      marg (mode_partial_application expected_mode);
                    Value.regional_to_local_alloc marg)
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
             let arg = Omitted { mode_closure; mode_arg; mode_ret; sort_arg } in
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
  | Texp_array (_, [], _) -> true
  | Texp_let(_rec_flag, pat_exp_list, body) ->
      List.for_all (fun vb -> is_nonexpansive vb.vb_expr) pat_exp_list &&
      is_nonexpansive body
  | Texp_apply(e, (_,Omitted _)::el, _, _) ->
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
      List.for_all is_nonexpansive el
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
  | Texp_field(exp, _, _, _, _) -> is_nonexpansive exp
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
      Vars.fold (fun _ (mut,_,_) b -> decr count; b && mut = Immutable)
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
      [Nolabel, Arg (e, _)], _, _) ->
     is_nonexpansive e
  | Texp_array (_, _ :: _, _)
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

let check_recursive_bindings env valbinds =
  let ids = let_bound_idents valbinds in
  List.iter
    (fun {vb_expr} ->
       if not (Rec_check.is_valid_recursive_expression ids vb_expr) then
         raise(Error(vb_expr.exp_loc, env, Illegal_letrec_expr))
    )
    valbinds

let check_recursive_class_bindings env ids exprs =
  List.iter
    (fun expr ->
       if not (Rec_check.is_valid_class_expr ids expr) then
         raise(Error(expr.cl_loc, env, Illegal_class_expr)))
    exprs

module Is_local_returning : sig
  val function_ : Parsetree.case list -> bool
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
        end
      | None      ->
      match e.pexp_desc with
      | Pexp_apply
          ({ pexp_desc = Pexp_extension(
             {txt = "extension.local"|"ocaml.local"|"local"}, PStr []) },
           [Nolabel, _]) ->
          Local e.pexp_loc
      | Pexp_apply
          ({ pexp_desc = Pexp_extension(
             {txt = "extension.unique"|"ocaml.unique"|"unique"}, PStr []) },
           [Nolabel, exp]) ->
          loop exp
      | Pexp_apply
          ({ pexp_desc = Pexp_extension(
            {txt = "extension.once" | "ocaml.once" | "once"}, PStr []) },
          [Nolabel, exp]) ->
          loop exp
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

  let function_ cases =
    let rec loop_cases cases =
      match cases with
      | [] -> Either
      | [{pc_lhs = _; pc_guard = None; pc_rhs = e}] ->
          loop_body e
      | case :: cases ->
          let is_local_returning_case case =
            expr case.pc_rhs
          in
          List.fold_left
            (fun acc case -> combine acc (is_local_returning_case case))
            (is_local_returning_case case) cases
    and loop_body e =
      if Builtin_attributes.has_curry e.pexp_attributes then
        expr e
      else begin
        match Jane_syntax.Expression.of_ast e with
        | Some (jexp, _attrs) -> begin
            match jexp with
            | Jexp_n_ary_function (_, _, Pfunction_cases (cases, _, _)) ->
                loop_cases cases
            | Jexp_n_ary_function (_, _, Pfunction_body body) ->
                loop_body body
            | Jexp_comprehension _ | Jexp_immutable_array _ ->
                expr e
            | Jexp_layout (Lexp_constant _) ->
                Not e.pexp_loc
            | Jexp_layout (Lexp_newtype (_, _, body)) ->
                loop_body body
          end
        | None -> match e.pexp_desc, e.pexp_attributes with
        | Pexp_fun(_, _, _, e), _ -> loop_body e
        | Pexp_function cases, _ -> loop_cases cases
        | Pexp_constraint (e, _), _ -> loop_body e
        | Pexp_let (Nonrecursive, _, e),
            [{Parsetree.attr_name = {txt="#default"};_}] -> loop_body e
        | _ -> expr e
      end
    in
    match loop_cases cases with
    | Local _ -> true
    | Either | Not _ -> false
      (* [fun _ -> assert false] must not be local-returning for
         backward compatibility *)
end

let rec is_an_uncurried_function e =
  if Builtin_attributes.has_curry e.pexp_attributes then false
  else begin
    match e.pexp_desc, e.pexp_attributes with
    | (Pexp_fun _ | Pexp_function _), _ -> true
    | Pexp_poly (e, _), _
    | Pexp_newtype (_, e), _  (* also works correctly for Lexp_newtype *)
    | Pexp_coerce (e, _, _), _
    | Pexp_constraint (e, _), _ -> is_an_uncurried_function e
    | Pexp_let (Nonrecursive, _, e),
      [{Parsetree.attr_name = {txt="#default"};_}] ->
       is_an_uncurried_function e
    | _ -> false
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
let approx_type_default () = newvar (Jkind.any ~why:Dummy_jkind)

let rec approx_type env sty =
  match Jane_syntax.Core_type.of_ast sty with
  | Some (jty, attrs) -> approx_type_jst env attrs jty
  | None ->
  match sty.ptyp_desc with
  | Ptyp_arrow (p, ({ ptyp_desc = Ptyp_poly _ } as arg_sty), sty) ->
      (* CR layouts v5: value requirement here to be relaxed *)
      if is_optional p then newvar (Jkind.value ~why:Type_argument)
      else begin
        let arg_mode = Typetexp.get_alloc_mode arg_sty in
        let arg_ty =
          (* Polymorphic types will only unify with types that match all of their
           polymorphic parts, so we need to fully translate the type here
           unlike in the monomorphic case *)
          Typetexp.transl_simple_type env ~closed:false arg_mode arg_sty
        in
        let ret = approx_type env sty in
        let marg = Alloc.of_const arg_mode in
        let mret = Alloc.newvar () in
        newty (Tarrow ((p,marg,mret), arg_ty.ctyp_type, ret, commu_ok))
      end
  | Ptyp_arrow (p, arg_sty, sty) ->
      let arg_mode = Typetexp.get_alloc_mode arg_sty in
      let arg =
        if is_optional p
        then type_option (newvar (Jkind.value ~why:Type_argument))
        else newvar (Jkind.of_new_sort ~why:Function_argument)
      in
      let ret = approx_type env sty in
      let marg = Alloc.of_const arg_mode in
      let mret = Alloc.newvar () in
      newty (Tarrow ((p,marg,mret), newmono arg, ret, commu_ok))
  | Ptyp_tuple args ->
      newty (Ttuple (List.map (approx_type env) args))
  | Ptyp_constr (lid, ctl) ->
      let path, decl = Env.lookup_type ~use:false ~loc:lid.loc lid.txt env in
      if List.length ctl <> decl.type_arity
      then newvar (Jkind.any ~why:Dummy_jkind)
      else begin
        let tyl = List.map (approx_type env) ctl in
        newconstr path tyl
      end
  | _ -> approx_type_default ()

and approx_type_jst _env _attrs : Jane_syntax.Core_type.t -> _ = function
  | Jtyp_layout (Ltyp_var _) -> approx_type_default ()
  | Jtyp_layout (Ltyp_poly _) -> approx_type_default ()
  | Jtyp_layout (Ltyp_alias _) -> approx_type_default ()

let type_pattern_approx_jane_syntax : Jane_syntax.Pattern.t -> _ = function
  | Jpat_immutable_array _
  | Jpat_layout (Lpat_constant _) -> ()

let type_pattern_approx env spat ty_expected =
  match Jane_syntax.Pattern.of_ast spat with
  | Some (jpat, _attrs) -> type_pattern_approx_jane_syntax jpat
  | None      ->
  match spat.ppat_desc with
  | Ppat_constraint(_, ({ptyp_desc=Ptyp_poly _} as sty)) ->
      let mode_annots = mode_annots_from_pat_attrs spat in
      let arg_type_mode =
        mode_annots_or_default mode_annots ~default:Alloc.Const.legacy
      in
      let ty_pat =
        Typetexp.transl_simple_type env ~closed:false arg_type_mode sty
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
        let mode_annots = mode_annots_from_pat_attrs spat in
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
        error_of_filter_arrow_failure ~explanation:None ~first ty_fun err
      in
      raise (Error(loc_fun, env, err))
  in
  Option.iter
    (fun mode_annots ->
      apply_mode_annots ~loc ~env ~ty_expected mode_annots arg_mode)
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
  | Pexp_fun (lbl, def, pat, body) ->
      let open Jane_syntax.N_ary_functions in
      type_approx_function ~loc env
        [ { pparam_desc = Pparam_val (lbl, def, pat);
            pparam_loc = pat.ppat_loc
          }
        ]
        None (Pfunction_body body) ty_expected
  | Pexp_function cases ->
      let open Jane_syntax.N_ary_functions in
      type_approx_function ~loc
        env [] None (Pfunction_cases (cases, sexp.pexp_loc, [])) ty_expected
  | Pexp_match (_, {pc_rhs=e}::_) -> type_approx env e ty_expected
  | Pexp_try (e, _) -> type_approx env e ty_expected
  | Pexp_tuple l ->
      let tys = List.map
                  (fun _ -> newvar (Jkind.value ~why:Tuple_element)) l
      in
      let ty = newty (Ttuple tys) in
      begin try unify env ty ty_expected with Unify err ->
        raise(Error(loc, env, Expr_type_clash (err, None, None)))
      end;
      List.iter2
        (fun e ty -> type_approx env e ty)
        l tys
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
      ({ pexp_desc = Pexp_extension(
         {txt = "extension.local"|"ocaml.local"|"local"}, PStr []) },
       [Nolabel, e]) ->
    type_approx env e ty_expected
  | Pexp_apply
      ({ pexp_desc = Pexp_extension(
         {txt = "extension.unique" | "ocaml.unique" | "unique"}, PStr []) },
       [Nolabel, e]) ->
    type_approx env e ty_expected
  | Pexp_apply
      ({ pexp_desc = Pexp_extension(
        {txt = "extension.once" | "ocaml.once" | "once"}, PStr []) },
      [Nolabel, e]) ->
    type_approx env e ty_expected
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
            | Texp_probe _ | Texp_probe_is_enabled _
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
  | Ppat_tuple lst ->  List.iter f lst
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
          ~more:(newvar (Jkind.value ~why:Row_variable))
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

(* If [is_inferred e] is true, [e] will be typechecked without using
   the "expected type" provided by the context. *)

let rec is_inferred sexp =
  match Jane_syntax.Expression.of_ast sexp with
  | Some (jexp, _attrs) -> is_inferred_jane_syntax jexp
  | None      -> match sexp.pexp_desc with
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
  let uniqueness = Value.uniqueness mode_r in
  let linearity = Value.linearity mode_l in
  if not (Language_extension.is_enabled Unique) then begin
    (* if unique extension is not enabled, we will not run uniqueness analysis;
       instead, we force all uses to be shared and many. This is equivalent to
       running a UA which forces everything *)
    (match Uniqueness.submode Uniqueness.shared uniqueness with
    | Ok () -> ()
    | Error () ->
        raise (Error(loc, env, Submode_failed(`Uniqueness, Other, None, None)))
    );
    (match Linearity.submode linearity Linearity.many with
    | Ok () -> ()
    | Error () ->
        raise (Error (loc, env, Submode_failed(`Linearity, Other, None, None)))
    );
    (Uniqueness.shared, Linearity.many)
  end
  else (uniqueness, linearity)

(* Generalize expressions *)
let generalize_structure_exp exp = generalize_structure exp.exp_type
let may_lower_contravariant_then_generalize env exp =
  if maybe_expansive exp then lower_contravariant env exp.exp_type;
  generalize exp.exp_type

(* value binding elaboration *)

let vb_exp_constraint {pvb_expr=expr; pvb_pat=pat; pvb_constraint=ct; pvb_attributes=attrs; _ } =
  let open Ast_helper in
  let mode_annot_attrs =
    Builtin_attributes.filter_attributes
      Builtin_attributes.mode_annotation_attributes_filter
      attrs
  in
  match ct with
  | None -> expr
  | Some (Pvc_constraint { locally_abstract_univars=[]; typ }) ->
      begin match typ.ptyp_desc with
      | Ptyp_poly _ -> expr
      | _ ->
          let loc = { expr.pexp_loc with Location.loc_ghost = true } in
          Exp.constraint_ ~loc ~attrs:mode_annot_attrs expr typ
      end
  | Some (Pvc_coercion { ground; coercion}) ->
      let loc = { expr.pexp_loc with Location.loc_ghost = true } in
      Exp.coerce ~loc expr ground coercion
  | Some (Pvc_constraint { locally_abstract_univars=vars;typ}) ->
      let loc_start = pat.ppat_loc.Location.loc_start in
      let loc = { expr.pexp_loc with loc_start; loc_ghost=true } in
      let expr = Exp.constraint_ ~loc ~attrs:mode_annot_attrs expr typ in
      List.fold_right (Exp.newtype ~loc) vars expr

let vb_pat_constraint ~force_toplevel rec_mode_var
      ({pvb_pat=pat; pvb_expr = exp; pvb_attributes = attrs; _ } as vb) =
  let mode_annot_attrs =
    Builtin_attributes.filter_attributes
      Builtin_attributes.mode_annotation_attributes_filter
      attrs
  in
  let spat =
    let open Ast_helper in
    match vb.pvb_constraint, pat.ppat_desc, exp.pexp_desc with
    | Some (Pvc_constraint {locally_abstract_univars=[]; typ}
           | Pvc_coercion { coercion=typ; _ }),
      _, _ ->
        Pat.constraint_ ~loc:{pat.ppat_loc with Location.loc_ghost=true} pat typ
          ~attrs:mode_annot_attrs
    | Some (Pvc_constraint {locally_abstract_univars=vars; typ }), _, _ ->
        let varified = Typ.varify_constructors vars typ in
        let t = Typ.poly ~loc:typ.ptyp_loc vars varified in
        let loc_end = typ.ptyp_loc.Location.loc_end in
        let loc =  { pat.ppat_loc with loc_end; loc_ghost=true } in
        Pat.constraint_ ~loc pat t
          ~attrs:mode_annot_attrs
    | None, (Ppat_any | Ppat_constraint _), _ -> pat
    | None, _, Pexp_coerce (_, _, sty)
    | None, _, Pexp_constraint (_, sty) when !Clflags.principal ->
        (* propagate type annotation to pattern,
           to allow it to be generalized in -principal mode *)
        Pat.constraint_ ~loc:{pat.ppat_loc with Location.loc_ghost=true} pat sty
          ~attrs:mode_annot_attrs
    | _ -> pat
  in
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
            let mode = Value.regional_to_local (Value.join modes) in
            tuple_pat_mode mode modes, mode_tuple mode modes
      end
    | Some mode ->
        simple_pat_mode mode, mode_exact mode
  in
  vb.pvb_attributes, pat_mode, exp_mode, spat


let rec type_exp ?recarg env expected_mode sexp =
  (* We now delegate everything to type_expect *)
  type_expect ?recarg env expected_mode sexp
    (mk_expected (newvar (Jkind.any ~why:Dummy_jkind)))

(* Typing of an expression with an expected type.
   This provide better error messages, and allows controlled
   propagation of return type information.
   In the principal case, structural nodes of [type_expected_explained] may be
   at [generic_level] (but its variables no higher than [!current_level]).
 *)

and type_expect ?in_function ?recarg env
      (expected_mode : expected_mode) sexp ty_expected_explained =
  let previous_saved_types = Cmt_format.get_saved_types () in
  let exp =
    Builtin_attributes.warning_scope sexp.pexp_attributes
      (fun () ->
         type_expect_ ?in_function ?recarg env
           expected_mode sexp ty_expected_explained
      )
  in
  Cmt_format.set_saved_types
    (Cmt_format.Partial_expression exp :: previous_saved_types);
  exp

and type_expect_
    ?in_function ?(recarg=Rejected)
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
  let ruem ~mode ~expected_mode exp =
    let exp = rue exp in
    submode ~env ~loc:exp.exp_loc ~reason:Other mode expected_mode;
    exp
  in
  match Jane_syntax.Expression.of_ast sexp with
  | Some (jexp, attributes) ->
      type_expect_jane_syntax
        ~loc
        ~loc_stack:sexp.pexp_loc_stack
        ~env
        ~expected_mode
        ~ty_expected
        ~explanation
        ~rue
        ~attributes
        jexp
  | None -> match desc with
  | Pexp_ident lid ->
      let path, mode, shared_context, desc, kind = type_ident env ~recarg lid in
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
              unique_use ~loc ~env mode expected_mode.mode)
        | _ ->
            Texp_ident(path, lid, desc, kind,
              unique_use ~loc ~env mode expected_mode.mode)
      in
      let exp = rue {
        exp_desc; exp_loc = loc; exp_extra = [];
        exp_type = desc.val_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
      in
      submode ~loc ~env ?shared_context mode expected_mode;
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
        type_expect ?in_function env expected_mode
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
    when may_contain_gadts spat ->
      (* TODO: allow non-empty attributes? *)
      let sval = vb_exp_constraint vb in
      type_expect ?in_function env expected_mode
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
          let in_function =
            match sexp.pexp_attributes with
            | [{Parsetree.attr_name = {txt="#default"};_}] -> in_function
            | _ -> None
          in
          let body =
            type_expect ?in_function
              new_env expected_mode sbody ty_expected_explained
          in
          let () =
            if rec_flag = Recursive then
              check_recursive_bindings env pat_exp_list
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
                  newvar2 outer_level (Jkind.any ~why:Dummy_jkind)
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
          unify_exp new_env body (newvar (Jkind.any ~why:Dummy_jkind)))
      in
      re {
        exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_fun (l, Some default, spat, sbody) ->
      let param_suffix =
        match l with
        | Optional name -> name
        | Nolabel | Labelled _ ->
          Misc.fatal_error "[default] allowed only with optional argument"
      in
      let open Ast_helper in
      let default_loc = default.pexp_loc in
      let default_ghost = {default.pexp_loc with loc_ghost = true} in
      (* Defaults are always global. They can be moved out of the function's
         region by Simplf.split_default_wrapper, or they could be evaluated
         later than expected by Translcore.push_defaults *)
      if has_poly_constraint spat then
        raise(Error(spat.ppat_loc, env, Optional_poly_param));
      let scases = [
        Exp.case
          (Pat.construct ~loc:default_ghost
             (mknoloc (Longident.(Ldot (Lident "*predef*", "Some"))))
             (Some ([], Pat.var ~loc:default_ghost (mknoloc "*sth*"))))
          (Exp.ident ~loc:default_ghost (mknoloc (Longident.Lident "*sth*")));

        Exp.case
          (Pat.construct ~loc:default_loc
             (mknoloc (Longident.(Ldot (Lident "*predef*", "None"))))
             None)
          (Exp.apply ~loc:default_loc
             (Exp.extension (mknoloc "extension.escape", PStr []))
             [Nolabel, default]);
       ]
      in
      let sloc =
        { Location.loc_start = spat.ppat_loc.Location.loc_start;
          loc_end = default_loc.Location.loc_end;
          loc_ghost = true }
      in
      let param_name = "*opt*" ^ param_suffix in
      let smatch =
        Exp.match_ ~loc:sloc
          (Exp.ident ~loc (mknoloc (Longident.Lident param_name)))
          scases
      in
      let pat = Pat.var ~loc:sloc (mknoloc param_name) in
      let body =
        Exp.let_ ~loc Nonrecursive
          ~attrs:[Attr.mk (mknoloc "#default") (PStr [])]
          [Vb.mk spat smatch] sbody
      in
      let mode_annots = mode_annots_from_pat_attrs spat in
      type_function ?in_function loc sexp.pexp_attributes env
                    expected_mode ty_expected_explained
                    l ~mode_annots ~has_poly:false [Exp.case pat body]
  | Pexp_fun (l, None, spat, sbody) ->
      let mode_annots = mode_annots_from_pat_attrs spat in
      let has_poly = has_poly_constraint spat in
      if has_poly && is_optional l then
        raise(Error(spat.ppat_loc, env, Optional_poly_param));
      if has_poly
         && not (Language_extension.is_enabled Polymorphic_parameters) then
        raise (Typetexp.Error (loc, env,
          Unsupported_extension Polymorphic_parameters));
      type_function ?in_function loc sexp.pexp_attributes env
                    expected_mode ty_expected_explained l ~mode_annots
                    ~has_poly [Ast_helper.Exp.case spat sbody]
  | Pexp_function caselist ->
      let mode_annots = mode_annots_none in
      type_function ?in_function
        loc sexp.pexp_attributes env expected_mode
        ty_expected_explained Nolabel ~mode_annots ~has_poly:false caselist
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({
            txt = ("ocaml.unique" | "unique" | "extension.unique" as txt)}, PStr []) },
       [Nolabel, sbody]) ->
      if txt = "extension.unique" && not (Language_extension.is_enabled Unique) then
        raise (Typetexp.Error (loc, Env.empty, Unsupported_extension Unique));
      let expected_mode = mode_unique expected_mode in
      let expected_mode = expect_mode_cross env ty_expected expected_mode in
      let exp =
        type_expect ?in_function ~recarg env expected_mode sbody
          ty_expected_explained
      in
      {exp with exp_loc = loc}
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({
          txt = ("ocaml.once" | "once" | "extension.once" as txt)}, PStr []) },
      [Nolabel, sbody]) ->
      if txt = "extension.once" && not (Language_extension.is_enabled Unique) then
        raise (Typetexp.Error (loc, Env.empty, Unsupported_extension Unique));
      let expected_mode = expect_mode_cross env ty_expected expected_mode in
      submode ~loc ~env ~reason:Other
        (Value.min_with_linearity Linearity.once) expected_mode;
      let expected_mode = mode_once expected_mode in
      let exp =
        type_expect ?in_function ~recarg env expected_mode sbody
          ty_expected_explained
      in
      {exp with exp_loc = loc}
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({
          txt = ("ocaml.local" | "local" | "extension.local" as txt)}, PStr []) },
       [Nolabel, sbody]) ->
      if txt = "extension.local" && not (Language_extension.is_enabled Local) then
        raise (Typetexp.Error (loc, Env.empty, Unsupported_extension Local));
      let expected_mode = expect_mode_cross env ty_expected expected_mode in
      submode ~loc ~env ~reason:Other
        (Value.min_with_locality Regionality.local) expected_mode;
      let expected_mode = mode_strictly_local expected_mode in
      let exp =
        type_expect ?in_function ~recarg env expected_mode sbody ty_expected_explained
      in
      {exp with exp_loc = loc}
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({txt = "extension.escape"}, PStr []) },
       [Nolabel, sbody]) ->
      submode ~loc ~env ~reason:Other Value.legacy expected_mode;
      let exp =
        type_expect ?in_function ~recarg env mode_legacy sbody
          ty_expected_explained
      in
      {exp with exp_loc = loc}
  | Pexp_apply
      ({ pexp_desc = Pexp_extension({
         txt = "extension.exclave" | "ocaml.exclave" | "exclave" as txt}, PStr []) },
       [Nolabel, sbody]) ->
      if (txt = "extension.exclave") && not (Language_extension.is_enabled Local) then
          raise (Typetexp.Error (loc, Env.empty, Unsupported_extension Local));
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
            type_expect ?in_function ~recarg new_env mode' sbody ty_expected_explained
          in
          submode ~loc ~env ~reason:Other
            (Value.min_with_locality Regionality.regional) expected_mode;
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
          let mode = Value.local_to_regional (Value.newvar ()) in
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
              let v = newvar (Jkind.any ~why:Dummy_jkind) in
              let rt = get_level ty > get_level v in
              unify_var env v ty;
              rt
          | _ ->
            let v = newvar (Jkind.any ~why:Dummy_jkind) in
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

      rue {
        exp_desc = Texp_apply(funct, args, pm.apply_position, ap_mode);
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
          let mode = Value.regional_to_local (Value.join modes) in
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
          arg.exp_type ty_expected_explained true loc caselist in
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
          Predef.type_exn ty_expected_explained false loc caselist in
      re {
        exp_desc = Texp_try(body, cases);
        exp_loc = loc; exp_extra = [];
        exp_type = body.exp_type;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_tuple sexpl ->
      let arity = List.length sexpl in
      assert (arity >= 2);
      let alloc_mode = register_allocation expected_mode in
      (* CR layouts v5: non-values in tuples *)
      let subtypes =
        List.map (fun _ -> newgenvar (Jkind.value ~why:Tuple_element))
          sexpl
      in
      let to_unify = newgenty (Ttuple subtypes) in
      with_explanation (fun () ->
        unify_exp_types loc env to_unify (generic_instance ty_expected));
      let argument_modes =
        if List.compare_length_with expected_mode.tuple_modes arity = 0 then
          expected_mode.tuple_modes
        else begin
          let arg_mode = Value.regional_to_global expected_mode.mode in
          List.init arity (fun _ -> arg_mode)
        end
      in
      let types_and_modes = List.combine subtypes argument_modes in
      let expl =
        List.map2
          (fun body (ty, argument_mode) ->
            let argument_mode = mode_default argument_mode in
            let argument_mode = expect_mode_cross env ty argument_mode in
             type_expect env argument_mode
               body (mk_expected ty))
          sexpl types_and_modes
      in
      re {
        exp_desc = Texp_tuple (expl, alloc_mode);
        exp_loc = loc; exp_extra = [];
        (* Keep sharing *)
        exp_type = newty (Ttuple (List.map (fun e -> e.exp_type) expl));
        exp_attributes = sexp.pexp_attributes;
        exp_env = env }
  | Pexp_construct(lid, sarg) ->
      type_construct env expected_mode loc lid
        sarg ty_expected_explained sexp.pexp_attributes
  | Pexp_variant(l, sarg) ->
      (* Keep sharing *)
      let ty_expected1 = protect_expansion env ty_expected in
      let ty_expected0 = instance ty_expected in
      let argument_mode = mode_subcomponent expected_mode in
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
              let arg = type_argument env argument_mode sarg ty ty0 in
              let alloc_mode = register_allocation expected_mode in
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
              newvar (Jkind.value ~why:Polymorphic_variant_field)
            in
            let arg =
              type_expect env argument_mode sarg (mk_expected ty_expected)
            in
            let alloc_mode = register_allocation expected_mode in
            Some (arg, alloc_mode)
        in
        let arg_type = Option.map (fun (arg, _) -> arg.exp_type) arg in
        let row =
          create_row
            ~fields: [l, rf_present arg_type]
            ~more:   (newvar (Jkind.value ~why:Row_variable))
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
                (* TODO: mode can be more relaxed than this if fields are global *)
                let mode = Value.newvar () in
                submode ~loc ~env mode (mode_subcomponent expected_mode);
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
      let lbl_exp_list =
        wrap_disambiguate "This record expression is expected to have"
          (mk_expected ty_record)
          (type_label_a_list loc closed env Env.Construct
             (type_label_exp true env expected_mode loc ty_record)
             expected_type)
          lid_sexp_list
      in
      with_explanation (fun () ->
        unify_exp_types loc env (instance ty_record) (instance ty_expected));
      let alloc_mode =
        if List.exists
            (function
              | _, { lbl_repres = Record_unboxed; _ }, _ -> false
              | _ -> true)
            lbl_exp_list then
          Some (register_allocation expected_mode)
        else
          None
      in
      (* type_label_a_list returns a list of labels sorted by lbl_num *)
      (* note: check_duplicates would better be implemented in
         type_label_a_list directly *)
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
                  let mode = modality_unbox_left lbl.lbl_global mode in
                  let rmode =
                    (* We skip a potential [mode_subcomponent] since
                       it does not affect uniqueness. *)
                    expected_mode
                  in
                  let expected_mode =
                    mode_box_modality lbl.lbl_global rmode
                  in
                  Kept (ty_arg1, lbl.lbl_mut,
                        unique_use ~loc ~env mode expected_mode.mode)
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
      let alloc_mode = match label.lbl_repres with
      (* projecting out of packed-float-record needs allocation *)
        | Record_float -> Some (register_allocation expected_mode)
        | _ -> None
      in
      let mode = modality_unbox_left label.lbl_global rmode in
      let ty_arg =
        with_local_level_if_principal begin fun () ->
          (* ty_arg is the type of field *)
          (* ty_res is the type of record *)
          (* they could share type variables *)
          (* which are now instantiated *)
          let (_, ty_arg, ty_res) = instance_label false label in
          (* we now link the two record types *)
          unify_exp env record ty_res;
          ty_arg
        end ~post:generalize_structure
      in
      let mode = mode_cross_to_min env ty_arg mode in
      let uu = unique_use ~loc ~env mode expected_mode.mode in
      ruem ~mode ~expected_mode {
        exp_desc = Texp_field(record, lid, label, uu, alloc_mode);
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
        type_label_exp false env (mode_default rmode) loc
          ty_record (lid, label, snewval) in
      unify_exp env record ty_record;
      if label.lbl_mut = Immutable then
        raise(Error(loc, env, Label_not_mutable lid.txt));
      rue {
        exp_desc = Texp_setfield(record,
          (Alloc.locality (Value.regional_to_local_alloc rmode)),
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
        ~mutability:Mutable
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
      let mode = mode_region Value.max_mode in
      let wh_cond =
        type_expect cond_env mode scond
          (mk_expected ~explanation:While_loop_conditional Predef.type_bool)
      in
      let body_env = Env.add_region_lock env in
      let position = RTail (Regionality.local, FNontail) in
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
        type_expect env (mode_region Value.max_mode) slow
          (mk_expected ~explanation:For_loop_start_index Predef.type_int)
      in
      let for_to =
        type_expect env (mode_region Value.max_mode) shigh
          (mk_expected ~explanation:For_loop_stop_index Predef.type_int)
      in
      let env = Env.add_share_lock For_loop env in
      (* When we'll want to add Uid to for loops, we can take it from here. *)
      let (for_id, _for_uid), new_env =
        type_for_loop_index ~loc ~env ~param
      in
      let new_env = Env.add_region_lock new_env in
      let position = RTail (Regionality.local, FNontail) in
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
      (* Pretend separate = true, 1% slowdown for lablgtk *)
      let cty =
        with_local_level begin fun () ->
          let mode_annots = mode_annots_from_exp_attrs sexp in
          let type_mode =
            mode_annots_or_default mode_annots ~default:Alloc.Const.legacy
          in
          Typetexp.transl_simple_type env ~closed:false type_mode sty
        end
        ~post:(fun cty -> generalize_structure cty.ctyp_type)
      in
      let ty = cty.ctyp_type in
      let ty' = instance ty in
      let arg = type_argument env expected_mode sarg ty (instance ty) in
      rue {
        exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_attributes = arg.exp_attributes;
        exp_env = env;
        exp_extra =
          (Texp_constraint cty, loc, sexp.pexp_attributes) :: arg.exp_extra;
      }
  | Pexp_coerce(sarg, sty, sty') ->
      (* Pretend separate = true, 1% slowdown for lablgtk *)
      (* Also see PR#7199 for a problem with the following:
         let separate = !Clflags.principal || Env.has_local_constraints env in*)
      let mode_annots = mode_annots_from_exp_attrs sexp in
      let type_mode =
        mode_annots_or_default mode_annots ~default:Alloc.Const.legacy
      in
      let (arg, ty',cty,cty') =
        match sty with
        | None ->
            let (cty', ty', force) =
              Typetexp.transl_simple_type_delayed env type_mode sty'
            in
            let arg, gen =
              let lv = get_current_level () in
              with_local_level begin fun () ->
                let arg = type_exp env expected_mode sarg in
                (arg, generalizable lv arg.exp_type)
              end
              ~post:(fun (arg,_) -> enforce_current_level env arg.exp_type)
            in
            begin match arg.exp_desc, !self_coercion, get_desc ty' with
              Texp_ident(_, _, {val_kind=Val_self _}, _, _), (path,r) :: _,
              Tconstr(path',_,_) when Path.same path path' ->
                (* prerr_endline "self coercion"; *)
                r := loc :: !r;
                force ()
            | _ when free_variables ~env arg.exp_type = []
                  && free_variables ~env ty' = [] ->
                if not gen && (* first try a single coercion *)
                  let snap = snapshot () in
                  let ty, _b = enlarge_type env ty' in
                  try
                    force (); Ctype.unify env arg.exp_type ty; true
                  with Unify _ ->
                    backtrack snap; false
                then ()
                else begin try
                  let force' = subtype env arg.exp_type ty' in
                  force (); force' ();
                  if not gen && !Clflags.principal then
                    Location.prerr_warning loc
                      (Warnings.Not_principal "this ground coercion");
                with Subtype err ->
                  (* prerr_endline "coercion failed"; *)
                  raise (Error(loc, env, Not_subtype err))
                end;
            | _ ->
                let ty, b = enlarge_type env ty' in
                force ();
                begin try Ctype.unify env arg.exp_type ty with Unify err ->
                  let expanded = full_expand ~may_forget_scope:true env ty' in
                  raise(Error(sarg.pexp_loc, env,
                              Coercion_failure({ty = ty'; expanded}, err, b)))
                end
            end;
            (arg, ty', None, cty')
        | Some sty ->
            let cty, ty, force, cty', ty', force' =
              with_local_level_iter ~post:generalize_structure begin fun () ->
                let (cty, ty, force) =
                  Typetexp.transl_simple_type_delayed env type_mode sty
                and (cty', ty', force') =
                  Typetexp.transl_simple_type_delayed env type_mode sty'
                in
                ((cty, ty, force, cty', ty', force'),
                 [ty; ty'])
              end
            in
            begin try
              let force'' = subtype env (instance ty) (instance ty') in
              force (); force' (); force'' ()
            with Subtype err ->
              raise (Error(loc, env, Not_subtype err))
            end;
            (type_argument env expected_mode sarg ty (instance ty),
             instance ty', Some cty, cty')
      in
      rue {
        exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty';
        exp_attributes = arg.exp_attributes;
        exp_env = env;
        exp_extra = (Texp_coerce (cty, cty'), loc, sexp.pexp_attributes) ::
                       arg.exp_extra;
      }
  | Pexp_send (e, {txt=met}) ->
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
            let ty' = newvar (Jkind.value ~why:Object_field) in
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
      let (cl_path, cl_decl) = Env.lookup_class ~loc:cl.loc cl.txt env in
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
              let md =
                { md_type = modl.mod_type; md_attributes = [];
                  md_loc = name.loc;
                  md_uid = Uid.mk ~current_unit:(Env.get_unit_name ()); }
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
      let (cd, newenv) = Typedecl.transl_exception env cd in
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
      let ty = newgenvar (Jkind.value ~why:Lazy_expression) in
      let to_unify = Predef.type_lazy_t ty in
      with_explanation (fun () ->
        unify_exp_types loc env to_unify (generic_instance ty_expected));
      let env = Env.add_escape_lock Lazy env in
      let env = Env.add_share_lock Lazy env in
      let arg = type_expect env (mode_lazy expected_mode) e (mk_expected ty) in
      re {
        exp_desc = Texp_lazy arg;
        exp_loc = loc; exp_extra = [];
        exp_type = instance ty_expected;
        exp_attributes = sexp.pexp_attributes;
        exp_env = env;
      }
  | Pexp_object s ->
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
                  Typetexp.transl_simple_type env ~closed:false
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
  | Pexp_newtype({txt=name}, sbody) ->
    type_newtype ~loc ~env ~expected_mode ~rue ~attributes:sexp.pexp_attributes
      name None sbody
  | Pexp_pack m ->
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
      let tv = newvar (Jkind.any ~why:Dummy_jkind) in
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
      let rec loop spat_acc ty_acc ty_acc_sort sands =
        match sands with
        | [] -> spat_acc, ty_acc, ty_acc_sort
        | { pbop_pat = spat; _} :: rest ->
            (* CR layouts v5: eliminate value requirement *)
            let ty = newvar (Jkind.value ~why:Tuple_element) in
            let loc = Location.ghostify slet.pbop_op.loc in
            let spat_acc = Ast_helper.Pat.tuple ~loc [spat_acc; spat] in
            let ty_acc = newty (Ttuple [ty_acc; ty]) in
            loop spat_acc ty_acc Jkind.Sort.value rest
      in
      let op_path, op_desc, op_type, spat_params, ty_params, param_sort,
          ty_func_result, body_sort, ty_result, op_result_sort,
          ty_andops, sort_andops =
        with_local_level_iter_if_principal
          ~post:generalize_structure begin fun () ->
          let let_loc = slet.pbop_op.loc in
          let op_path, op_desc = type_binding_op_ident env slet.pbop_op in
          let op_type = instance op_desc.val_type in
          let spat_params, ty_params, param_sort =
            let initial_jkind, initial_sort = match sands with
              | [] ->
                Jkind.of_new_sort_var ~why:Function_argument
              (* CR layouts v5: eliminate value requirement for tuple elements *)
              | _ -> Jkind.value ~why:Tuple_element, Jkind.Sort.value
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
          (simple_pat_mode Value.legacy)
          (mode_return Value.legacy)
          ty_params (mk_expected ty_func_result) true loc [scase]
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
      let warnings = Warnings.backup () in
      let desc =
        Texp_letop{let_; ands; param; param_sort; body; body_sort; partial;
                   warnings}
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
    let name, name_loc, args =
      match payload with
      | PStr
          ([{ pstr_desc =
                Pstr_eval
                  ({ pexp_desc =
                       (Pexp_apply
                          ({ pexp_desc=
                               (Pexp_constant (Pconst_string(name,_,None)));
                             pexp_loc = name_loc;
                             _ }
                          , args))
                   ; _ }
                  , _)}]) -> name, name_loc, args
      | _ -> raise (Error (loc, env, Probe_format))
    in
    let bool_of_string = function
      | "true" -> true
      | "false" -> false
      | _ -> raise (Error (loc, env, Probe_format))
    in
    let arg, enabled_at_init =
      match args with
      | [Nolabel, arg] -> arg, false
      | [Labelled "enabled_at_init",
         { pexp_desc =
             Pexp_construct({ txt = Longident.Lident b; _ },
                            None); _ };
         Nolabel, arg] -> arg, bool_of_string b
      | _ -> raise (Error (loc, env, Probe_format))
    in
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
  | Pexp_extension ext ->
    raise (Error_forward (Builtin_attributes.error_of_extension ext))

  | Pexp_unreachable ->
      re { exp_desc = Texp_unreachable;
           exp_loc = loc; exp_extra = [];
           exp_type = instance ty_expected;
           exp_attributes = sexp.pexp_attributes;
           exp_env = env }

and type_ident env ?(recarg=Rejected) lid =
  let (path, desc, mode, reason) = Env.lookup_value ~loc:lid.loc lid.txt env in
  (* Mode crossing here is needed only because of the strange behaviour of
  [type_let] - it checks the LHS before RHS. Had it checks the RHS before LHS,
  identifiers would be mode crossed when being added to the environment. *)
  let mode = mode_cross_to_min env desc.val_type mode in
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
       let ty, mode = instance_prim_mode prim (instance desc.val_type) in
       begin match prim.prim_native_repr_res, mode with
       (* if the locality of returning value of the primitive is poly
          we then register allocation for further optimization *)
       | (Prim_poly, _), Some mode ->
           register_allocation_mode
             (Alloc.prod mode Uniqueness.shared Linearity.many)
       | _ -> ()
       end;
       ty, Id_prim mode
    | _ ->
       instance desc.val_type, Id_value in
  path, mode, reason, { desc with val_type }, kind

and type_binding_op_ident env s =
  let loc = s.loc in
  let lid = Location.mkloc (Longident.Lident s.txt) loc in
  let path, mode, _reason, desc, kind = type_ident env lid in
  submode ~env ~loc:lid.loc ~reason:Other mode mode_legacy;
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

and type_function
    ?in_function loc attrs env (expected_mode : expected_mode)
      ty_expected_explained arg_label ~mode_annots ~has_poly caselist =
  let { ty = ty_expected; explanation } = ty_expected_explained in
  let alloc_mode = Value.regional_to_global_alloc expected_mode.mode in
  let alloc_mode =
    if expected_mode.exact then
      (* expected_mode.mode is exact *)
      alloc_mode
    else
      (* expected_mode.mode is upper bound *)
      fst (Alloc.newvar_below alloc_mode)
  in
  if expected_mode.strictly_local then
    Locality.submode_exn Locality.local (Alloc.locality alloc_mode);
  register_allocation_mode alloc_mode;
  let (loc_fun, ty_fun) =
    match in_function with
    | Some (loc_fun, ty_fun, _) -> (loc_fun, ty_fun)
    | None -> (loc, instance ty_expected)
  in
  let uncurried_function =
    match caselist with
    | [{pc_lhs = _; pc_guard = None; pc_rhs = e}] ->
        is_an_uncurried_function e
    | _ -> false
  in
  let separate = !Clflags.principal || Env.has_local_constraints env in
  let { ty_arg; arg_mode; arg_sort; ty_ret; ret_mode; ret_sort } =
    with_local_level_iter_if separate ~post:generalize_structure begin fun () ->
      let force_tpoly =
        (* If [has_poly] is true then we rely on the later call to
           type_pat to enforce the invariant that the parameter type
           be a [Tpoly] node *)
        not has_poly
      in
      let { ty_arg; ty_ret; _ } as filtered_arrow =
        try filter_arrow env (instance ty_expected) arg_label ~force_tpoly
        with Filter_arrow_failed err ->
          let first = Option.is_none in_function in
          let err =
            error_of_filter_arrow_failure ~explanation ~first ty_fun err
          in
          raise (Error(loc_fun, env, err))
      in
      (filtered_arrow, [ty_arg; ty_ret])
    end
  in
  apply_mode_annots ~loc ~env ~ty_expected mode_annots arg_mode;
  if not has_poly && not (tpoly_is_mono ty_arg) && !Clflags.principal
       && get_level ty_arg < Btype.generic_level then begin
    let snap = Btype.snapshot () in
    let really_poly =
      try
        unify env (newmono (newvar (Jkind.any ~why:Dummy_jkind))) ty_arg;
        false
      with Unify _ -> true
    in
    Btype.backtrack snap;
    if really_poly then
      Location.prerr_warning loc
        (Warnings.Not_principal "this higher-rank function");
  end;
  let env, region_locked =
    match in_function with
    | Some (_, _, region_locked) -> env, region_locked
    | None ->
      let region_locked = not (Is_local_returning.function_ caselist) in
      let env =
        Env.add_closure_lock
          ?closure_context:expected_mode.closure_context
          (Alloc.locality alloc_mode)
          (Alloc.linearity alloc_mode)
          env
      in
      let env =
        if region_locked then Env.add_region_lock env
        else env
      in
      env, region_locked
  in
  let arg_value_mode = Value.of_alloc arg_mode in
  let arg_value_mode =
    if region_locked then Value.local_to_regional arg_value_mode
    else arg_value_mode
  in
  let cases_expected_mode, curry =
    if uncurried_function then begin
      (* no need to check mode crossing in this case*)
      (* because ty_res always a function *)
      let inner_alloc_mode, _ = Alloc.newvar_below ret_mode in
      begin match
        Alloc.submode (Alloc.close_over arg_mode) inner_alloc_mode
      with
      | Ok () -> ()
      | Error e ->
        raise (Error(loc_fun, env, Uncurried_function_escapes e))
      end;
      begin match
        Alloc.submode (Alloc.partial_apply alloc_mode) inner_alloc_mode
      with
      | Ok () -> ()
      | Error e ->
        raise (Error(loc_fun, env, Uncurried_function_escapes e))
      end;
      mode_exact (Value.of_alloc inner_alloc_mode),
      More_args {partial_mode = inner_alloc_mode}
    end
    else begin
      let ret_value_mode = Value.of_alloc ret_mode in
      let ret_value_mode =
        if region_locked then mode_return ret_value_mode
        else begin
          (* if the function has no region, we force the ret_mode to be local *)
          match
            Locality.submode Locality.local (Alloc.locality ret_mode)
          with
          | Ok () -> mode_default ret_value_mode
          | Error () -> raise (Error (loc_fun, env, Function_returns_local))
        end
      in
      let ret_value_mode = expect_mode_cross env ty_ret ret_value_mode in
      ret_value_mode,
      Final_arg { partial_mode = Alloc.join [arg_mode; alloc_mode] }
    end
  in
  let in_function =
    if uncurried_function then
      Some (loc_fun, ty_fun, region_locked)
    else
      None
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
  let cases, partial =
    type_cases Value ?in_function env (simple_pat_mode arg_value_mode)
      cases_expected_mode ty_arg_mono (mk_expected ty_ret) true loc caselist in
  let not_nolabel_function ty =
    let ls, tvar = list_labels env ty in
    List.for_all ((<>) Nolabel) ls && not tvar
  in
  if is_optional arg_label && not_nolabel_function ty_ret then
    Location.prerr_warning (List.hd cases).c_lhs.pat_loc
      Warnings.Unerasable_optional_argument;
  let param = name_cases "param" cases in
  let region = region_locked && not uncurried_function in
  let warnings = Warnings.backup () in
  re {
    exp_desc =
      Texp_function
        { arg_label; param; cases; partial; region; curry; warnings;
          arg_mode; arg_sort; alloc_mode; ret_sort };
    exp_loc = loc; exp_extra = [];
    exp_type =
      instance (newgenty (Tarrow((arg_label,arg_mode,ret_mode),
                                 ty_arg, ty_ret, commu_ok)));
    exp_attributes = attrs;
    exp_env = env }


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

and type_label_exp create env (expected_mode : expected_mode) loc ty_expected
          (lid, label, sarg) =
  (* Here also ty_expected may be at generic_level *)
  let separate = !Clflags.principal || Env.has_local_constraints env in
  let rmode =
    match label.lbl_repres with
    | Record_unboxed | Record_inlined (_, Variant_unboxed) ->
      expected_mode
    | _ -> mode_subcomponent expected_mode
  in
  let arg_mode = mode_box_modality label.lbl_global rmode in
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
      let mret, changed' = Alloc.newvar_below_comonadic mret in
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
      (* apply optional arguments when expected type is "" *)
      (* we must be very careful about not breaking the semantics *)
      let exp_mode, _ = Value.newvar_below mode.mode in
      let texp =
        with_local_level_if_principal ~post:generalize_structure_exp
          (fun () -> type_exp env {mode with mode = exp_mode} sarg)
      in
      let rec make_args args ty_fun =
        match get_desc (expand_head env ty_fun) with
        | Tarrow ((l,_marg,_mret),ty_arg,ty_fun,_) when is_optional l ->
            let ty =
              option_none env (instance (tpoly_get_mono ty_arg))
                sarg.pexp_loc
            in
            (* CR layouts v5: change value assumption below when we allow
               non-values in structures. *)
            make_args ((l, Arg (ty, Jkind.Sort.value)) :: args) ty_fun
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
      (* In this case, we're allocating a new closure, so [sarg] needs
         to be valid at [mode_subcomponent mode], not just [mode] *)
      let alloc_mode = register_allocation mode in
      submode ~loc:sarg.pexp_loc ~env ~reason:Other
        exp_mode (mode_subcomponent mode);
      (* eta-expand to avoid side effects *)
      let var_pair ~mode name ty =
        let id = Ident.create_local name in
        let desc =
          { val_type = ty; val_kind = Val_reg;
            val_attributes = [];
            val_loc = Location.none;
            val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
          }
        in
        let exp_env = Env.add_value ~mode id desc env in
        let uu = unique_use ~loc:sarg.pexp_loc ~env mode mode in
        {pat_desc = Tpat_var (id, mknoloc name, desc.val_uid, mode);
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
      let eta_mode = Value.local_to_regional (Value.of_alloc marg) in
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
        let ret_mode = Value.of_alloc mret in
        let e =
          {texp with exp_type = ty_res; exp_desc =
           Texp_apply
             (texp,
              args @ [Nolabel, Arg (eta_var, arg_sort)], Nontail,
              ret_mode
              |> Value.locality
              |> Regionality.regional_to_global_locality)}
        in
        let cases = [case eta_pat e] in
        let param = name_cases "param" cases in
        let partial_mode =
          Alloc.join [marg; Value.regional_to_global_alloc mode.mode]
        in
        let curry = Final_arg {partial_mode} in
        { texp with exp_type = ty_fun;
            exp_desc = Texp_function { arg_label = Nolabel; param; cases;
                                       partial = Total; region = false; curry;
                                       warnings = Warnings.backup ();
                                       arg_mode = marg; arg_sort; ret_sort;
                                       alloc_mode } }
      in
      Location.prerr_warning texp.exp_loc
        (Warnings.Eliminated_optional_arguments
           (List.map (fun (l, _) -> Printtyp.string_of_label l) args));
      if warn then Location.prerr_warning texp.exp_loc
          (Warnings.Non_principal_labels "eliminated optional argument");
      (* let-expand to have side effects *)
      let let_pat, let_var = var_pair ~mode:exp_mode "arg" texp.exp_type in
      re { texp with exp_type = ty_fun;
             exp_desc =
               Texp_let (Nonrecursive,
                         [{vb_pat=let_pat; vb_expr=texp; vb_sort=arg_sort;
                           vb_attributes=[]; vb_loc=Location.none;
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
      let mode, _ = Alloc.newvar_below mode_arg in
      let expected_mode =
        mode_argument ~funct ~index ~position_and_mode ~partial_app mode in
      let arg =
        type_expect env expected_mode sarg (mk_expected ty_arg_mono)
      in
      if is_optional lbl then
        (* CR layouts v5: relax value requirement *)
        unify_exp env arg
          (type_option(newvar (Jkind.value ~why:Type_argument)));
      (lbl, Arg (arg, expected_mode.mode, sort_arg))
  | Arg (Known_arg { sarg; ty_arg; ty_arg0;
                     mode_arg; wrapped_in_some; sort_arg }) ->
      let mode, _ = Alloc.newvar_below mode_arg in
      let expected_mode =
        mode_argument ~funct ~index ~position_and_mode ~partial_app mode in
      let ty_arg', vars = tpoly_get_poly ty_arg in
      let arg =
        if vars = [] then begin
          let ty_arg0' = tpoly_get_mono ty_arg0 in
          if wrapped_in_some then begin
            option_some env
              (type_argument env (mode_subcomponent expected_mode) sarg
                 (extract_option_type env ty_arg')
                 (extract_option_type env ty_arg0'))
              expected_mode.mode
          end else begin
            type_argument env expected_mode sarg ty_arg' ty_arg0'
          end
        end else begin
          if !Clflags.principal
             && get_level ty_arg < Btype.generic_level then begin
            let snap = Btype.snapshot () in
            let really_poly =
              try
                unify env (newmono (newvar (Jkind.any ~why:Dummy_jkind)))
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
      (lbl, Arg (arg, expected_mode.mode, sort_arg))
  | Arg (Eliminated_optional_arg { ty_arg; sort_arg; _ }) ->
      let arg = option_none env (instance ty_arg) Location.none in
      (lbl, Arg (arg, Value.legacy, sort_arg))
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
    [Nolabel, sarg] when is_ignore funct ->
      let {ty_arg; arg_mode; arg_sort; ty_ret; ret_mode} =
        with_local_level_if_principal (fun () ->
          filter_arrow_mono env (instance funct.exp_type) Nolabel
        ) ~post:(fun {ty_ret; _} -> generalize_structure ty_ret)
      in
      let ap_mode = Alloc.locality ret_mode in
      let mode_res =
        mode_cross_to_min env ty_ret (Value.of_alloc ret_mode)
      in
      submode ~loc:app_loc ~env ~reason:Other
        mode_res expected_mode;
      let arg_mode =
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
          let labels = List.filter (fun l -> not (is_optional l)) ls in
          List.length labels = List.length sargs &&
          List.for_all (fun (l,_) -> l = Nolabel) sargs &&
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
          let ty_ret, mode_ret, untyped_args =
            collect_apply_args env funct ignore_labels ty (instance ty)
              (Value.regional_to_local_alloc funct_mode) sargs ret_tvar
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
      let ap_mode = Alloc.locality mode_ret in
      let mode_ret =
        mode_cross_to_min env ty_ret (Value.of_alloc mode_ret)
      in
      submode ~loc:app_loc ~env ~reason:(Application ty_ret)
        mode_ret expected_mode;

      check_tail_call_local_returning app_loc env ap_mode position_and_mode;
      args, ty_ret, ap_mode, position_and_mode

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
      None -> []
    | Some {pexp_desc = Pexp_tuple sel} when
        constr.cstr_arity > 1 || Builtin_attributes.explicit_arity attrs
      -> sel
    | Some se -> [se] in
  if List.length sargs <> constr.cstr_arity then
    raise(Error(loc, env, Constructor_arity_mismatch
                            (lid.txt, constr.cstr_arity, List.length sargs)));
  let separate = !Clflags.principal || Env.has_local_constraints env in
  let ty_args, ty_res, texp =
    with_local_level_iter_if separate ~post:generalize_structure begin fun () ->
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
      ((ty_args, ty_res, texp), ty_res::(List.map fst ty_args))
    end
  in
  let ty_args0, ty_res =
    match instance_list (ty_res :: (List.map fst ty_args)) with
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
       mode_subcomponent expected_mode,
       Some (register_allocation expected_mode)
  in
  let args =
    List.map2
      (fun e ((ty, gf),t0) ->
         let argument_mode = mode_box_modality gf argument_mode in
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
    exp, Jkind.Sort.value
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

(* Typing of match cases *)
and type_cases
    : type k . k pattern_category ->
           ?in_function:_ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> Parsetree.case list ->
           k case list * partial
  = fun category ?in_function env pmode emode
        ty_arg ty_res_explained partial_flag loc caselist ->
  (* ty_arg is _fully_ generalized *)
  let { ty = ty_res; explanation } = ty_res_explained in
  let patterns = List.map (fun {pc_lhs=p} -> p) caselist in
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
      [{pc_rhs = {pexp_desc = Pexp_unreachable}}] -> true
    | [{pc_lhs}] when is_var pc_lhs -> false
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
        (fun ({pc_lhs; pc_guard = _; pc_rhs = _} as case) ->
          let htc =
            with_local_level_if_principal begin fun () ->
              let ty_arg =
                (* propagation of pattern *)
                with_local_level ~post:generalize_structure
                  (fun () -> instance ?partial:take_partial_instance ty_arg)
              in
              let (pat, ext_env, force, pvs, mvs) =
                type_pattern category ~lev ~alloc_mode:pmode env pc_lhs ty_arg
                  allow_modules
              in
              pattern_force := force @ !pattern_force;
              { typed_pat = pat;
                pat_type_for_unif = ty_arg;
                untyped_case = case;
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
      let ty_arg' = newvar (Jkind.any ~why:Dummy_jkind) in
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
  let in_function = if List.length caselist = 1 then in_function else None in
  let ty_res' = instance ty_res in
  let cases = with_local_level_if_principal ~post:ignore begin fun () ->
    List.map
      (fun { typed_pat = pat; branch_env = ext_env;
             pat_vars = pvs; module_vars = mvs;
             untyped_case = {pc_lhs = _; pc_guard; pc_rhs};
             contains_gadt; _ }  ->
        let ext_env =
          if contains_gadt then
            do_copy_types ext_env
          else
            ext_env
        in
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
        let guard =
          match pc_guard with
          | None -> None
          | Some scond ->
              Some
                (type_expect ext_env mode_max scond
                   (mk_expected ~explanation:When_guard Predef.type_bool))
        in
        let exp =
          type_expect ?in_function ext_env emode
            pc_rhs (mk_expected ?explanation ty_expected)
        in
        {
         c_lhs = pat;
         c_guard = guard;
         c_rhs = {exp with exp_type = ty_res'}
        }
      )
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
  let val_cases, exn_cases =
    match category with
      | Value -> (cases : value case list), []
      | Computation -> split_cases env cases in
  if val_cases = [] && exn_cases <> [] then
    raise (Error (loc, env, No_value_clauses));
  let partial =
    if partial_flag then
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
    Parmatch.check_ambiguous_bindings val_cases ;
    Parmatch.check_ambiguous_bindings exn_cases
  in
  if contains_polyvars then
    add_delayed_check (fun () -> unused_check true)
  else
    (* Check for unused cases, do not delay because of gadts *)
    unused_check false;
  ((cases, partial), [ty_res'])
  end
  (* Ensure that existential types do not escape *)
  ~post:(fun ty_res' -> unify_exp_types loc env ty_res'
                          (newvar (Jkind.any ~why:Dummy_jkind)))

and type_newtype ~loc ~env ~expected_mode ~rue ~attributes
      name jkind_annot_opt sbody =
  let jkind =
    Jkind.of_annotation_option_default ~context:(Newtype_declaration name)
      ~default:(Jkind.value ~why:Univar) jkind_annot_opt
  in
  let ty =
    if Typetexp.valid_tyvar_name name then
      newvar ~name jkind
    else
      newvar jkind
  in
  (* Use [with_local_level] just for scoping *)
  let body, ety = with_local_level begin fun () ->
    (* Create a fake abstract type declaration for name. *)
    let decl = new_local_type ~loc jkind in
    let scope = create_scope () in
    let (id, new_env) = Env.enter_type ~scope name decl env in

    let body = type_exp new_env expected_mode sbody in
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
    let ety = Subst.type_expr Subst.identity body.exp_type in
    replace ety;
    (body, ety)
  end
  in
  (* non-expansive if the body is non-expansive, so we don't introduce
     any new extra node in the typed AST. *)
  rue { body with exp_loc = loc; exp_type = ety;
        exp_extra =
        (Texp_newtype (name, Option.map Location.get_txt jkind_annot_opt),
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
    | Pexp_newtype (_, e)
    | Pexp_apply
      ({ pexp_desc = Pexp_extension({
          txt = "extension.once" | "ocaml.once" | "once"}, PStr []) },
        [Nolabel, e])
    | Pexp_apply
      ({ pexp_desc = Pexp_extension({
          txt = "extension.unique" | "ocaml.unique" | "unique"}, PStr []) },
        [Nolabel, e])
    | Pexp_apply
      ({ pexp_desc = Pexp_extension(
          {txt = "extension.local"|"ocaml.local"|"local"}, PStr []) },
       [Nolabel, e]) -> sexp_is_fun e
    | _ -> false
  and jexp_is_fun : Jane_syntax.Expression.t -> _ = function
    | Jexp_comprehension _
    | Jexp_immutable_array _
    | Jexp_layout (Lexp_constant _) -> false
    | Jexp_layout (Lexp_newtype (_, _, e)) -> sexp_is_fun e
    | Jexp_n_ary_function _ -> true
  in
  let vb_is_fun { pvb_expr = sexp; _ } = sexp_is_fun sexp in
  let entirely_functions = List.for_all vb_is_fun spat_sexp_list in
  let rec_mode_var =
    match rec_flag with
    | Recursive when entirely_functions -> Some (Value.newvar ())
    | Recursive -> Some Value.legacy
    | Nonrecursive -> None
  in
  let spatl =
    List.map (vb_pat_constraint ~force_toplevel rec_mode_var) spat_sexp_list
  in
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
              ignore(check_partial env pat.pat_type pat.pat_loc
                       [case pat exp] : Typedtree.partial)
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
        mode_pat_typ_list exp_list
    end
  in
  let l = List.combine pat_list exp_list in
  let l = List.combine sorts l in
  let l =
    List.map2
      (fun (s, ((_,p,_), (e, _))) pvb ->
        {vb_pat=p; vb_expr=e; vb_sort = s; vb_attributes=pvb.pvb_attributes;
         vb_loc=pvb.pvb_loc;
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
            let op_type = instance op_desc.val_type in
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
      ~expected_mode
      ~ty_expected
      ~explanation
      ~mutability
      ~attributes
      sargl
  =
  let type_, base_argument_mode = match mutability with
    | Mutable -> Predef.type_array, mode_default Value.legacy
    | Immutable -> Predef.type_iarray, mode_subcomponent expected_mode
  in
  let alloc_mode = register_allocation expected_mode in
  (* CR layouts v4: non-values in arrays *)
  let ty = newgenvar (Jkind.value ~why:Array_element) in
  let to_unify = type_ ty in
  with_explanation explanation (fun () ->
    unify_exp_types loc env to_unify (generic_instance ty_expected));
  let argument_mode = expect_mode_cross env ty base_argument_mode in
  let argl =
    List.map
      (fun sarg -> type_expect env argument_mode sarg (mk_expected ty))
      sargl
  in
  re {
    exp_desc = Texp_array (mutability, argl, alloc_mode);
    exp_loc = loc; exp_extra = [];
    exp_type = instance ty_expected;
    exp_attributes = attributes;
    exp_env = env }

and type_expect_jane_syntax
      ~loc ~env ~expected_mode ~ty_expected ~explanation ~rue ~attributes
        ~loc_stack
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
          ~loc_stack

and type_n_ary_function
      ~loc ~loc_stack ~env ~expected_mode ~ty_expected ~explanation ~attributes
      ((params, constraint_, body) : Jane_syntax.N_ary_functions.expression)
    =
    (* TODO nroberts: in later commit, actually typecheck this. *)
    let open Ast_helper in
    let open Jane_syntax.N_ary_functions in
    let fun_body =
      match body with
      | Pfunction_body body -> body
      | Pfunction_cases (cases, loc, attrs) ->
          (Exp.function_ cases ~attrs ~loc [@alert "-prefer_jane_syntax"])
    in
    let constrained_body =
      let loc = { fun_body.pexp_loc with loc_ghost = true } in
      match constraint_ with
      | None -> fun_body
      | Some { mode_annotations; type_constraint } ->
          let body =
            match type_constraint with
            | Pconstraint constraint_ ->
                Exp.constraint_ fun_body constraint_ ~loc
            | Pcoerce (ty1, ty2) ->
                Exp.coerce fun_body ty1 ty2 ~loc
          in
          List.fold_left
            (fun body mode ->
               let attr_name =
                 (* this is hacky but will go away in the next PR *)
                 match mode.txt with
                 | Local -> "extension.local"
                 | Unique -> "extension.unique"
                 | Once -> "extension.once"
               in
                Exp.attr
                  body
                  { attr_name = mknoloc attr_name
                  ; attr_payload = PStr []
                  ; attr_loc = mode.loc
                  })
            body
            mode_annotations
    in
    let ast =
      (* Extract the least-parenthesized location for forging the fake
         location for the nested Pexp_fun nodes.
      *)
      let make_loc =
        let rec last loc_stack =
          match loc_stack with
          | [] -> loc
          | [ loc ] -> loc
          | _ :: tl -> last tl
        in
        let loc_function = last loc_stack in
        fun ~pparam_loc ->
          { loc_function with
              Location.loc_start = pparam_loc.Location.loc_start;
              loc_ghost = true;
          }
      in
      List.fold_right
        (fun { pparam_loc; pparam_desc } body ->
           let loc = make_loc ~pparam_loc in
           match pparam_desc with
           | Pparam_val (l, o, p) ->
               (Exp.fun_ l o p body ~loc [@alert "-prefer_jane_syntax"])
           | Pparam_newtype (newtype, jkind) ->
               match jkind with
               | None -> Exp.newtype newtype body ~loc
               | Some jkind ->
                   Jane_syntax.Layouts.expr_of ~loc
                     (Lexp_newtype (newtype, jkind, body)))
        params
        constrained_body
    in
    let ast =
      { pexp_desc = ast.pexp_desc;
        pexp_attributes = ast.pexp_attributes @ attributes;
        pexp_loc = loc;
        pexp_loc_stack = [];
      }
    in
    type_expect_
      env expected_mode ast { ty = ty_expected; explanation }

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
      {body = sbody; clauses}, reason =
    match cexpr with
    | Cexp_list_comprehension comp ->
        List_comprehension,
        Predef.type_list,
        (fun tcomp -> Texp_list_comprehension tcomp),
        comp,
        Jkind.Type_argument
    | Cexp_array_comprehension (amut, comp) ->
        let container_type = match amut with
          | Mutable   -> Predef.type_array
          | Immutable -> Predef.type_iarray
        in
        Array_comprehension amut,
        container_type,
        (fun tcomp -> Texp_array_comprehension (amut, tcomp)),
        comp,
        Jkind.Array_element
  in
  let element_ty =
    with_local_level_if_principal begin fun () ->
      (* CR layouts v4: When this changes from [value], you will also have to
         update the use of [transl_exp] in transl_array_comprehension.ml. See
         a companion CR layouts v4 at the point of interest in that file. *)
      let element_ty = newvar (Jkind.value ~why:reason) in
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
      let item_ty = newvar (Jkind.any ~why:Dummy_jkind) in
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
  | Lexp_newtype ({txt=name}, jkind_annot, sbody) ->
    type_newtype ~loc ~env ~expected_mode ~rue ~attributes
      name (Some jkind_annot) sbody

and type_unboxed_constant ~loc ~env ~rue ~attributes cst =
  rue {
    exp_desc = unboxed_constant_or_raise env loc cst;
    exp_loc = loc;
    exp_extra = [];
    exp_type = type_constant_unboxed env loc cst;
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
                  let ty = newvar (Jkind.value ~why:Object_field) in
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
        let (_path, desc, _mode, _reasons) =
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
  type_expression env (Jkind.any ~why:Type_expression_call) sexp

(* Error report *)

let spellcheck ppf unbound_name valid_names =
  Misc.did_you_mean ppf (fun () ->
    Misc.spellcheck valid_names unbound_name
  )

let spellcheck_idents ppf unbound valid_idents =
  spellcheck ppf (Ident.name unbound) (List.map Ident.name valid_idents)

open Format

let longident = Printtyp.longident

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
        | Array_comprehension Mutable   -> "an array"
        | Array_comprehension Immutable -> "an immutable array"
      in
      because ("a for-in iterator in " ^ a_comp_ty ^ " comprehension")
  | Comprehension_for_start ->
      because "a range-based for iterator start index in a comprehension"
  | Comprehension_for_stop ->
      because "a range-based for iterator stop index in a comprehension"
  | Comprehension_when ->
      because "a when-clause in a comprehension"

let escaping_hint failure_reason submode_reason
      (context : Env.closure_context option) =
  begin match failure_reason, context with
  | `Locality, Some Return ->
      [ Location.msg
          "@[Hint: Cannot return local value without an@ \
           \"exclave_\" annotation@]" ]
  | `Locality, Some Tailcall_argument ->
      [ Location.msg
          "@[Hint: This argument cannot be local, because this is a tail call@]" ]
  | `Locality, Some Tailcall_function ->
      [ Location.msg
          "@[Hint: This function cannot be local, because this is a tail call@]" ]
  | `Regionality, Some Partial_application ->
      [ Location.msg
          "@[Hint: It is captured by a partial application@]" ]
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
            Locality.check_const (Alloc.locality res_mode)
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

let sharedness_hint _fail_reason submode_reason context =
  (match context with
  | None -> []
  | Some Env.For_loop ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the for-loop.@]"]
  | Some Env.While_loop ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the while-loop.@]"]
  | Some Env.Comprehension ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the comprehension.@]"]
  | Some Env.Letop ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it was defined outside of the let-op.@]"]
  | Some Env.Class ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined in a class.@]"]
  | Some Env.Closure ->
    [Location.msg
        "@[Hint: This identifier was defined outside of the current closure.@ \
          Either this closure has to be once, or the identifier can be used only@ \
          as shared.@]"]
  | Some Env.Module ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined in a module.@]"]
  | Some Env.Probe ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined outside of the probe.@]"]
  | Some Env.Lazy ->
    [Location.msg
        "@[Hint: This identifier cannot be used uniquely,@ \
          because it is defined outside of the lazy expression.@]"]
  )
  @
  match submode_reason with
  | Application _ | Other -> []

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
        | l -> fprintf ppf "with label %s" (prefixed_label_name l)
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
      fprintf ppf "Method types must have layout value.@ %a"
        (Jkind.Violation.report_with_name ~name:"This expression")
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
      let label ~long = function
        | Nolabel -> "unlabeled"
        | l       -> (if long then "labeled " else "") ^ prefixed_label_name l
      in
      let second_long = match got, expected with
        | Nolabel, _ | _, Nolabel -> true
        | _                       -> false
      in
      Location.errorf ~loc
        "@[<v>@[<2>This function should have type@ %a%t@]@,\
         @[but its first argument is %s@ instead of %s%s@]@]"
        Printtyp.type_expr expected_type
        (report_type_expected_explanation_opt explanation)
        (label ~long:true got)
        (if second_long then "being " else "")
        (label ~long:second_long expected)
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
  | Submode_failed(fail_reason, submode_reason, closure_context, shared_context)
     ->
      let sub =
        match fail_reason with
        | `Linearity | `Uniqueness ->
          sharedness_hint fail_reason submode_reason shared_context
        | `Locality | `Regionality ->
          escaping_hint fail_reason submode_reason closure_context
      in
      Location.errorf ~loc ~sub begin
        match fail_reason with
        | `Locality -> "This local value escapes its region"
        | `Regionality -> "This value escapes its region"
        | `Uniqueness -> "Found a shared value where a unique value was expected"
        | `Linearity -> "Found a once value where a many value was expected"
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
            | Labelled s | Optional s -> s
          in
          [Location.msg
             "@[Hint: Try splitting the application in two. The arguments that come@ \
              after %s in the function's type should be applied separately.@]" lbl]
      in
      Location.errorf ~loc ~sub
        "@[This application is complete, but surplus arguments were provided afterwards.@ \
         When passing or calling a local value, extra arguments are passed in a separate application.@]"
  | Param_mode_mismatch (ty, mkind) ->
      let mkind =
        match mkind with
        | `Locality -> "local"
        | `Uniqueness -> "unique"
        | `Linearity -> "once"
      in
      Location.errorf ~loc
        "@[This function has a %s parameter, but was expected to have type:@ %a@]"
        mkind Printtyp.type_expr ty
  | Uncurried_function_escapes e -> begin
      match e with
      | `Locality ->
          Location.errorf ~loc "This function or one of its parameters escape their region @ \
          when it is partially applied."
      | `Uniqueness -> assert false
      | `Linearity ->
          Location.errorf ~loc "This function when partially applied returns a once value,@ \
          but expected to be many."
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
        "Exclave expression should only be in tail position of the current region"
  | Exclave_returns_not_local ->
      Location.errorf ~loc
        "The body of exclave expression should be local"
  | Optional_poly_param ->
      Location.errorf ~loc
        "Optional parameters cannot be polymorphic"
  | Function_returns_local ->
      Location.errorf ~loc
        "This function is local-returning, but was expected otherwise"
  | Tail_call_local_returning ->
      Location.errorf ~loc
        "@[This application is local-returning, but is at the tail @ \
          position of a function that is not local-returning@]"
  | Unboxed_int_literals_not_supported ->
      Location.errorf ~loc
        "@[Unboxed int literals aren't supported yet.@]"
  | Unboxed_float_literals_not_supported ->
      Location.errorf ~loc
        "@[Unboxed float literals aren't supported yet.@]"
  | Function_type_not_rep (ty,violation) ->
      Location.errorf ~loc
        "@[Function arguments and returns must be representable.@]@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) violation

let report_error ~loc env err =
  Printtyp.wrap_printing_env ~error:true env
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
