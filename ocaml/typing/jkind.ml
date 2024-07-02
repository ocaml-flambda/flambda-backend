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

[@@@warning "+9"]

(* A *sort* is the information the middle/back ends need to be able to
   compile a manipulation (storing, passing, etc) of a runtime value. *)
module Sort = Jkind_types.Sort

type sort = Sort.t

type type_expr = Types.type_expr

(* A *layout* of a type describes the way values of that type are stored at
   runtime, including details like width, register convention, calling
   convention, etc. A layout may be *representable* or *unrepresentable*.  The
   middle/back ends are unable to cope with values of types with an
   unrepresentable layout. The only unrepresentable layout is `any`, which is
   the top of the layout lattice. *)
module Layout = struct
  open Jkind_types.Layout

  type nonrec 'sort layout = 'sort layout

  module Const = struct
    type t = Sort.const layout

    let max = Any

    let equal c1 c2 =
      match c1, c2 with
      | Sort s1, Sort s2 -> Sort.Const.equal s1 s2
      | Any, Any -> true
      | (Any | Sort _), _ -> false

    let sub (c1 : t) (c2 : t) : Misc.Le_result.t =
      match c1, c2 with
      | _ when equal c1 c2 -> Equal
      | _, Any -> Less
      | Any, Sort _ | Sort _, Sort _ -> Not_le

    let value = Sort Sort.Value

    let void = Sort Sort.Void

    let float64 = Sort Sort.Float64

    let float32 = Sort Sort.Float32

    let word = Sort Sort.Word

    let bits32 = Sort Sort.Bits32

    let bits64 = Sort Sort.Bits64

    let get_sort : t -> Sort.Const.t option = function
      | Sort s -> Some s
      | Any -> None

    let to_string : t -> _ = function
      | Any -> "any"
      | Sort Void -> "void"
      | Sort Value -> "value"
      | Sort Float64 -> "float64"
      | Sort Float32 -> "float32"
      | Sort Word -> "word"
      | Sort Bits32 -> "bits32"
      | Sort Bits64 -> "bits64"

    module Legacy = struct
      (* CR layouts v2.8: get rid of this *)
      type t = Jkind_types.Layout.Const.Legacy.t =
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

      let to_string ~hide_null t =
        match t with
        | Any -> "any"
        (* CR layouts v3.0: remove this hack once [or_null] is out of [Alpha]. *)
        | Any_non_null when hide_null -> "any"
        | Any_non_null -> "any_non_null"
        | Value_or_null when hide_null -> "value"
        | Value_or_null -> "value_or_null"
        | Value -> "value"
        | Void -> "void"
        | Immediate64 -> "immediate64"
        | Immediate -> "immediate"
        | Float64 -> "float64"
        | Float32 -> "float32"
        | Word -> "word"
        | Bits32 -> "bits32"
        | Bits64 -> "bits64"
    end
  end

  type t = Sort.t layout

  let of_const const : t =
    match const with Sort s -> Sort (Const s) | Any -> Any

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

  let format ppf =
    let open Format in
    function
    | Any -> fprintf ppf "any"
    | Sort s -> (
      match Sort.get s with
      | Const s -> fprintf ppf "%a" Sort.Const.format s
      | Var v -> fprintf ppf "%s" (Sort.Var.name v))

  module Debug_printers = struct
    open Format

    let t ppf = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" Sort.Debug_printers.t s
  end
end

module Externality = struct
  type t = Jkind_types.Externality.t =
    | External
    | External64
    | Internal

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

  let print ppf = function
    | External -> Format.fprintf ppf "external_"
    | External64 -> Format.fprintf ppf "external64"
    | Internal -> Format.fprintf ppf "internal"
end

module Nullability = struct
  type t = Jkind_types.Nullability.t =
    | Non_null
    | Maybe_null

  let max = Maybe_null

  let equal n1 n2 =
    match n1, n2 with
    | Non_null, Non_null -> true
    | Maybe_null, Maybe_null -> true
    | (Non_null | Maybe_null), _ -> false

  let less_or_equal n1 n2 : Misc.Le_result.t =
    match n1, n2 with
    | Non_null, Non_null -> Equal
    | Non_null, Maybe_null -> Less
    | Maybe_null, Non_null -> Not_le
    | Maybe_null, Maybe_null -> Equal

  let le n1 n2 = Misc.Le_result.is_le (less_or_equal n1 n2)

  let meet n1 n2 =
    match n1, n2 with
    | Non_null, (Non_null | Maybe_null) | Maybe_null, Non_null -> Non_null
    | Maybe_null, Maybe_null -> Maybe_null

  let print ppf = function
    | Non_null -> Format.fprintf ppf "non_null"
    | Maybe_null -> Format.fprintf ppf "maybe_null"
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

module History = struct
  include Jkind_intf.History

  let has_imported_history t =
    match t.history with Creation Imported -> true | _ -> false

  let update_reason t reason = { t with history = Creation reason }

  let with_warning t = { t with has_warned = true }

  let has_warned t = t.has_warned
end

(* forward declare [Const.t] so we can use it for [Error.t] *)
type const = type_expr Jkind_types.Const.t

(******************************)
(*** user errors ***)

module Error = struct
  type t =
    | Insufficient_level of
        { jkind : const;
          required_layouts_level : Language_extension.maturity
        }
    | Unknown_jkind of Jane_syntax.Jkind.t
    | Unknown_mode of Jane_syntax.Mode_expr.Const.t
    | Multiple_jkinds of
        { from_annotation : const;
          from_attribute : const
        }

  exception User_error of Location.t * t
end

let raise ~loc err = raise (Error.User_error (loc, err))

module Const = struct
  open Jkind_types.Const

  type t = const

  let max =
    { layout = Layout.Const.max;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Nullability.max
    }

  let get_layout const = const.layout

  let get_modal_upper_bounds const = const.modes_upper_bounds

  let get_externality_upper_bound const = const.externality_upper_bound

  let get_legacy_layout
      { layout;
        modes_upper_bounds = _;
        externality_upper_bound;
        nullability_upper_bound
      } : Layout.Const.Legacy.t =
    match layout, externality_upper_bound, nullability_upper_bound with
    | Any, _, Maybe_null -> Any
    | Any, _, Non_null -> Any_non_null
    (* CR layouts v3.0: support [Immediate(64)_or_null]. *)
    | Sort Value, _, Maybe_null -> Value_or_null
    | Sort Value, Internal, Non_null -> Value
    | Sort Value, External64, Non_null -> Immediate64
    | Sort Value, External, Non_null -> Immediate
    | Sort Void, _, _ -> Void
    | Sort Float64, _, _ -> Float64
    | Sort Float32, _, _ -> Float32
    | Sort Word, _, _ -> Word
    | Sort Bits32, _, _ -> Bits32
    | Sort Bits64, _, _ -> Bits64

  let equal
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Layout.Const.equal lay1 lay2
    && Modes.equal modes1 modes2
    && Externality.equal ext1 ext2
    && Nullability.equal null1 null2

  let sub
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Misc.Le_result.combine_list
      [ Layout.Const.sub lay1 lay2;
        Modes.less_or_equal modes1 modes2;
        Externality.less_or_equal ext1 ext2;
        Nullability.less_or_equal null1 null2 ]

  let of_layout ~mode_crossing ~nullability layout =
    let modes_upper_bounds, externality_upper_bound =
      match mode_crossing with
      | true -> Modes.min, Externality.min
      | false -> Modes.max, Externality.max
    in
    { layout;
      modes_upper_bounds;
      externality_upper_bound;
      nullability_upper_bound = nullability
    }

  module Primitive = struct
    type nonrec t =
      { jkind : t;
        name : string
      }

    let any =
      { jkind = of_layout Any ~mode_crossing:false ~nullability:Maybe_null;
        name = "any"
      }

    let any_non_null =
      { jkind = of_layout Any ~mode_crossing:false ~nullability:Non_null;
        name = "any_non_null"
      }

    let value_or_null =
      { jkind =
          of_layout Layout.Const.value ~mode_crossing:false
            ~nullability:Maybe_null;
        name = "value_or_null"
      }

    let value =
      { jkind =
          of_layout Layout.Const.value ~mode_crossing:false
            ~nullability:Non_null;
        name = "value"
      }

    (* CR layouts v3: change to [or_null] when separability is implemented. *)
    let void =
      { jkind =
          of_layout Layout.Const.void ~mode_crossing:false ~nullability:Non_null;
        name = "void"
      }

    let immediate =
      { jkind =
          of_layout Layout.Const.value ~mode_crossing:true ~nullability:Non_null;
        name = "immediate"
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
      { jkind = { immediate.jkind with externality_upper_bound = External64 };
        name = "immediate64"
      }

    (* CR layouts v2.8: This should not mode cross, but we need syntax for mode
       crossing first *)
    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float64 =
      { jkind =
          of_layout Layout.Const.float64 ~mode_crossing:true
            ~nullability:Non_null;
        name = "float64"
      }

    (* CR layouts v2.8: This should not mode cross, but we need syntax for mode
       crossing first *)
    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float32 =
      { jkind =
          of_layout Layout.Const.float32 ~mode_crossing:true
            ~nullability:Non_null;
        name = "float32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let word =
      { jkind =
          of_layout Layout.Const.word ~mode_crossing:false ~nullability:Non_null;
        name = "word"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits32 =
      { jkind =
          of_layout Layout.Const.bits32 ~mode_crossing:false
            ~nullability:Non_null;
        name = "bits32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits64 =
      { jkind =
          of_layout Layout.Const.bits64 ~mode_crossing:false
            ~nullability:Non_null;
        name = "bits64"
      }

    let all =
      [ any;
        any_non_null;
        value_or_null;
        value;
        void;
        immediate;
        immediate64;
        float64;
        float32;
        word;
        bits32;
        bits64 ]

    (* CR layouts v3.0: remove this hack once [or_null] is out of [Alpha]. *)
    let all_non_null =
      [ any;
        { any_non_null with name = "any" };
        { value_or_null with name = "value" };
        value;
        void;
        immediate;
        immediate64;
        float64;
        float32;
        word;
        bits32;
        bits64 ]
  end

  module To_out_jkind_const = struct
    type printable_jkind =
      { base : string;
        modal_bounds : string list
      }

    module Bounds = struct
      type t =
        { alloc_bounds : Alloc.Const.t;
          externality_bound : Externality.t;
          nullability_bound : Nullability.t
        }

      let of_jkind jkind =
        { alloc_bounds = jkind.modes_upper_bounds;
          externality_bound = jkind.externality_upper_bound;
          nullability_bound = jkind.nullability_upper_bound
        }
    end

    let get_modal_bound ~le ~print ~base actual =
      match le actual base with
      | true -> (
        match le base actual with
        | true -> `Valid None
        | false -> `Valid (Some (Format.asprintf "%a" print actual)))
      | false -> `Invalid

    let get_modal_bounds ~(base : Bounds.t) (actual : Bounds.t) =
      [ get_modal_bound ~le:Locality.Const.le ~print:Locality.Const.print
          ~base:base.alloc_bounds.areality actual.alloc_bounds.areality;
        get_modal_bound ~le:Uniqueness.Const.le ~print:Uniqueness.Const.print
          ~base:base.alloc_bounds.uniqueness actual.alloc_bounds.uniqueness;
        get_modal_bound ~le:Linearity.Const.le ~print:Linearity.Const.print
          ~base:base.alloc_bounds.linearity actual.alloc_bounds.linearity;
        get_modal_bound ~le:Contention.Const.le ~print:Contention.Const.print
          ~base:base.alloc_bounds.contention actual.alloc_bounds.contention;
        get_modal_bound ~le:Portability.Const.le ~print:Portability.Const.print
          ~base:base.alloc_bounds.portability actual.alloc_bounds.portability;
        get_modal_bound ~le:Externality.le ~print:Externality.print
          ~base:base.externality_bound actual.externality_bound;
        get_modal_bound ~le:Nullability.le ~print:Nullability.print
          ~base:base.nullability_bound actual.nullability_bound ]
      |> List.rev
      |> List.fold_left
           (fun acc mode ->
             match acc, mode with
             | _, `Invalid | None, _ -> None
             | acc, `Valid None -> acc
             | Some acc, `Valid (Some mode) -> Some (mode :: acc))
           (Some [])

    (** Write [actual] in terms of [base] *)
    let convert_with_base ~(base : Primitive.t) actual =
      let matching_layouts =
        Layout.Const.equal base.jkind.layout actual.layout
      in
      let modal_bounds =
        get_modal_bounds
          ~base:(Bounds.of_jkind base.jkind)
          (Bounds.of_jkind actual)
      in
      match matching_layouts, modal_bounds with
      | true, Some modal_bounds -> Some { base = base.name; modal_bounds }
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
      (* For each primitive jkind, we try to print the jkind in terms of it (this is
         possible if the primitive is a subjkind of it). We then choose the "simplest". The
           "simplest" is taken to mean the one with the least number of modes that need to
         follow the [mod]. *)
      let simplest =
        (if Language_extension.(is_at_least Layouts Alpha)
        then Primitive.all
        else Primitive.all_non_null)
        |> List.filter_map (fun base -> convert_with_base ~base jkind)
        |> select_simplest
      in
      let printable_jkind =
        match simplest with
        | Some simplest -> simplest
        | None -> (
          (* CR layouts v2.8: sometimes there is no valid way to build a jkind from a
             built-in abbreviation. For now, we just pretend that the layout name is a valid
             jkind abbreviation whose modal bounds are all max, even though this is a
             lie. *)
          let out_jkind_verbose =
            convert_with_base
              ~base:
                { jkind =
                    { layout = jkind.layout;
                      modes_upper_bounds = Modes.max;
                      externality_upper_bound = Externality.max;
                      nullability_upper_bound = Nullability.Non_null
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
                      modes_upper_bounds = Modes.max;
                      externality_upper_bound = Externality.max;
                      nullability_upper_bound = Nullability.max
                    };
                  name = Layout.Const.to_string jkind.layout
                }
              jkind
          in
          (* convert_with_base is guaranteed to succeed since the layout matches and the
               modal bounds are all max *)
            Option.get out_jkind_verbose)
      in
      match printable_jkind with
      | { base; modal_bounds = _ :: _ as modal_bounds } ->
        Outcometree.Ojkind_const_mod
          (Ojkind_const_abbreviation base, modal_bounds)
      | { base; modal_bounds = [] } ->
        Outcometree.Ojkind_const_abbreviation base
  end

  let to_out_jkind_const = To_out_jkind_const.convert

  let format ppf jkind = to_out_jkind_const jkind |> !Oprint.out_jkind_const ppf

  let of_attribute : Builtin_attributes.jkind_attribute -> t = function
    | Immediate -> Primitive.immediate.jkind
    | Immediate64 -> Primitive.immediate64.jkind

  module ModeParser = struct
    type mode =
      | Areality of Locality.Const.t
      | Linearity of Linearity.Const.t
      | Uniqueness of Uniqueness.Const.t
      | Contention of Contention.Const.t
      | Portability of Portability.Const.t
      | Externality of Externality.t
      | Nullability of Nullability.t

    let parse_mode unparsed_mode =
      let { txt = name; loc } =
        (unparsed_mode : Jane_syntax.Mode_expr.Const.t :> _ Location.loc)
      in
      match name with
      | "global" -> Areality Global
      | "local" -> Areality Local
      | "many" -> Linearity Many
      | "once" -> Linearity Once
      | "unique" -> Uniqueness Unique
      | "shared" -> Uniqueness Shared
      | "internal" -> Externality Internal
      | "external64" -> Externality External64
      | "external_" -> Externality External
      | "contended" -> Contention Contended
      | "uncontended" -> Contention Uncontended
      | "portable" -> Portability Portable
      | "nonportable" -> Portability Nonportable
      | "non_null" -> Nullability Non_null
      | "maybe_null" -> Nullability Maybe_null
      | _ -> raise ~loc (Unknown_mode unparsed_mode)

    let parse_modes
        (Location.{ txt = modes; loc = _ } : Jane_syntax.Mode_expr.t) =
      List.map parse_mode modes
  end

  let rec of_user_written_annotation_unchecked_level
      (jkind : Jane_syntax.Jkind.t) : t =
    match jkind with
    | Abbreviation const -> (
      let { txt = name; loc } =
        (const : Jane_syntax.Jkind.Const.t :> _ Location.loc)
      in
      (* CR layouts 2.8: move this to predef *)
      match name with
      (* CR layouts 3.0: remove this hack once non-null jkinds are out of alpha.
         It is confusing, but preserves backwards compatibility for arrays. *)
      | "any" when Language_extension.(is_at_least Layouts Alpha) ->
        Primitive.any.jkind
      | "any" -> Primitive.any_non_null.jkind
      | "any_non_null" -> Primitive.any_non_null.jkind
      | "value_or_null" -> Primitive.value_or_null.jkind
      | "value" -> Primitive.value.jkind
      | "void" -> Primitive.void.jkind
      | "immediate64" -> Primitive.immediate64.jkind
      | "immediate" -> Primitive.immediate.jkind
      | "float64" -> Primitive.float64.jkind
      | "float32" -> Primitive.float32.jkind
      | "word" -> Primitive.word.jkind
      | "bits32" -> Primitive.bits32.jkind
      | "bits64" -> Primitive.bits64.jkind
      | _ -> raise ~loc (Unknown_jkind jkind))
    | Mod (jkind, modes) ->
      let base = of_user_written_annotation_unchecked_level jkind in
      (* for each mode, lower the corresponding modal bound to be that mode *)
      let parsed_modes = ModeParser.parse_modes modes in
      let meet_mode jkind (mode : ModeParser.mode) =
        match mode with
        | Areality areality ->
          { jkind with
            modes_upper_bounds =
              { jkind.modes_upper_bounds with
                areality =
                  Locality.Const.meet jkind.modes_upper_bounds.areality areality
              }
          }
        | Linearity linearity ->
          { jkind with
            modes_upper_bounds =
              Modes.meet jkind.modes_upper_bounds
                { jkind.modes_upper_bounds with
                  linearity =
                    Linearity.Const.meet jkind.modes_upper_bounds.linearity
                      linearity
                }
          }
        | Uniqueness uniqueness ->
          { jkind with
            modes_upper_bounds =
              Modes.meet jkind.modes_upper_bounds
                { jkind.modes_upper_bounds with
                  uniqueness =
                    Uniqueness.Const.meet jkind.modes_upper_bounds.uniqueness
                      uniqueness
                }
          }
        | Contention contention ->
          { jkind with
            modes_upper_bounds =
              Modes.meet jkind.modes_upper_bounds
                { jkind.modes_upper_bounds with
                  contention =
                    Contention.Const.meet jkind.modes_upper_bounds.contention
                      contention
                }
          }
        | Portability portability ->
          { jkind with
            modes_upper_bounds =
              Modes.meet jkind.modes_upper_bounds
                { jkind.modes_upper_bounds with
                  portability =
                    Portability.Const.meet jkind.modes_upper_bounds.portability
                      portability
                }
          }
        | Externality externality ->
          { jkind with
            externality_upper_bound =
              Externality.meet jkind.externality_upper_bound externality
          }
        | Nullability nullability ->
          { jkind with
            nullability_upper_bound =
              Nullability.meet jkind.nullability_upper_bound nullability
          }
      in
      List.fold_left meet_mode base parsed_modes
    | Default | With _ | Kind_of _ -> Misc.fatal_error "XXX unimplemented"

  module Sort = Sort.Const
  module Layout = Layout.Const
end

module Desc = struct
  type t =
    | Const of Const.t
    | Var of Sort.var (* all modes will be [max] *)

  let format ppf =
    let open Format in
    function
    | Const c -> fprintf ppf "%a" Const.format c
    | Var v -> fprintf ppf "%s" (Sort.Var.name v)

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
  open Jkind_types.Jkind_desc

  let of_const
      ({ layout;
         modes_upper_bounds;
         externality_upper_bound;
         nullability_upper_bound
       } :
        Const.t) =
    { layout = Layout.of_const layout;
      modes_upper_bounds;
      externality_upper_bound;
      nullability_upper_bound
    }

  let add_mode_crossing t =
    { t with
      modes_upper_bounds = Modes.min;
      externality_upper_bound = Externality.min
    }

  let add_portability_and_contention_crossing ~from t =
    let new_portability =
      Portability.Const.meet t.modes_upper_bounds.portability
        from.modes_upper_bounds.portability
    in
    let new_contention =
      Contention.Const.meet t.modes_upper_bounds.contention
        from.modes_upper_bounds.contention
    in
    let added_crossings =
      (not
         (Portability.Const.le t.modes_upper_bounds.portability new_portability))
      || not
           (Contention.Const.le t.modes_upper_bounds.contention new_contention)
    in
    ( { t with
        modes_upper_bounds =
          { t.modes_upper_bounds with
            portability = new_portability;
            contention = new_contention
          }
      },
      added_crossings )

  let max = of_const Const.max

  let equate_or_equal ~allow_mutation
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Layout.equate_or_equal ~allow_mutation lay1 lay2
    && Modes.equal modes1 modes2
    && Externality.equal ext1 ext2
    && Nullability.equal null1 null2

  let sub
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Misc.Le_result.combine_list
      [ Layout.sub lay1 lay2;
        Modes.less_or_equal modes1 modes2;
        Externality.less_or_equal ext1 ext2;
        Nullability.less_or_equal null1 null2 ]

  let intersection
      { layout = lay1;
        modes_upper_bounds = modes1;
        externality_upper_bound = ext1;
        nullability_upper_bound = null1
      }
      { layout = lay2;
        modes_upper_bounds = modes2;
        externality_upper_bound = ext2;
        nullability_upper_bound = null2
      } =
    Option.bind (Layout.intersection lay1 lay2) (fun layout ->
        Some
          { layout;
            modes_upper_bounds = Modes.meet modes1 modes2;
            externality_upper_bound = Externality.meet ext1 ext2;
            nullability_upper_bound = Nullability.meet null1 null2
          })

  let of_new_sort_var nullability_upper_bound =
    let layout, sort = Layout.of_new_sort_var () in
    ( { layout;
        modes_upper_bounds = Modes.max;
        externality_upper_bound = Externality.max;
        nullability_upper_bound
      },
      sort )

  module Primitive = struct
    let any = max

    let any_non_null = of_const Const.Primitive.any_non_null.jkind

    let value_or_null = of_const Const.Primitive.value_or_null.jkind

    let value = of_const Const.Primitive.value.jkind

    let void = of_const Const.Primitive.void.jkind

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

       * Portability: This is fine, because portability matters only for function
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
    let immediate64 = of_const Const.Primitive.immediate64.jkind

    let immediate = of_const Const.Primitive.immediate.jkind

    let float64 = of_const Const.Primitive.float64.jkind

    let float32 = of_const Const.Primitive.float32.jkind

    let word = of_const Const.Primitive.word.jkind

    let bits32 = of_const Const.Primitive.bits32.jkind

    let bits64 = of_const Const.Primitive.bits64.jkind
  end

  (* Post-condition: If the result is [Var v], then [!v] is [None]. *)
  let get
      { layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      } : Desc.t =
    match layout with
    | Any ->
      Const
        { layout = Any;
          modes_upper_bounds;
          externality_upper_bound;
          nullability_upper_bound
        }
    | Sort s -> (
      match Sort.get s with
      | Const s ->
        Const
          { layout = Sort s;
            modes_upper_bounds;
            externality_upper_bound;
            nullability_upper_bound
          }
      | Var v -> Var v)

  module Debug_printers = struct
    open Format

    let t ppf
        { layout;
          modes_upper_bounds;
          externality_upper_bound;
          nullability_upper_bound
        } =
      fprintf ppf
        "{ layout = %a;@ modes_upper_bounds = %a;@ externality_upper_bound = \
         %a;@ nullability_upper_bound = %a }"
        Layout.Debug_printers.t layout Modes.print modes_upper_bounds
        Externality.print externality_upper_bound Nullability.print
        nullability_upper_bound
  end
end

type t = type_expr Jkind_types.t

let fresh_jkind jkind ~why =
  { jkind; history = Creation why; has_warned = false }

(******************************)
(* constants *)

module Primitive = struct
  let any_dummy_jkind =
    { jkind = Jkind_desc.max;
      history = Creation (Any_creation Dummy_jkind);
      has_warned = false
    }

  (* CR layouts: Should we be doing more memoization here? *)
  let any ~(why : History.any_creation_reason) =
    match why with
    | Dummy_jkind -> any_dummy_jkind (* share this one common case *)
    | _ -> fresh_jkind Jkind_desc.Primitive.any ~why:(Any_creation why)

  let any_non_null ~why =
    fresh_jkind Jkind_desc.Primitive.any_non_null
      ~why:(Any_non_null_creation why)

  let value_v1_safety_check =
    { jkind = Jkind_desc.Primitive.value_or_null;
      history = Creation (Value_or_null_creation V1_safety_check);
      has_warned = false
    }

  let void ~why = fresh_jkind Jkind_desc.Primitive.void ~why:(Void_creation why)

  let value_or_null ~why =
    match (why : History.value_or_null_creation_reason) with
    | V1_safety_check -> value_v1_safety_check
    | _ ->
      fresh_jkind Jkind_desc.Primitive.value_or_null
        ~why:(Value_or_null_creation why)

  let value ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Primitive.value ~why:(Value_creation why)

  let immediate64 ~why =
    fresh_jkind Jkind_desc.Primitive.immediate64 ~why:(Immediate64_creation why)

  let immediate ~why =
    fresh_jkind Jkind_desc.Primitive.immediate ~why:(Immediate_creation why)

  let float64 ~why =
    fresh_jkind Jkind_desc.Primitive.float64 ~why:(Float64_creation why)

  let float32 ~why =
    fresh_jkind Jkind_desc.Primitive.float32 ~why:(Float32_creation why)

  let word ~why = fresh_jkind Jkind_desc.Primitive.word ~why:(Word_creation why)

  let bits32 ~why =
    fresh_jkind Jkind_desc.Primitive.bits32 ~why:(Bits32_creation why)

  let bits64 ~why =
    fresh_jkind Jkind_desc.Primitive.bits64 ~why:(Bits64_creation why)
end

let add_mode_crossing t =
  { t with jkind = Jkind_desc.add_mode_crossing t.jkind }

let add_portability_and_contention_crossing ~from t =
  let jkind, added_crossings =
    Jkind_desc.add_portability_and_contention_crossing ~from:from.jkind t.jkind
  in
  { t with jkind }, added_crossings

(*** extension requirements ***)
(* The [annotation_context] parameter can be used to allow annotations / kinds
   in different contexts to be enabled with different extension settings.
   At some points in time, we will not care about the context, and so this
   parameter might effectively be unused.
*)
(* CR layouts: When everything is stable, remove this function. *)
let get_required_layouts_level (context : History.annotation_context)
    (jkind : Const.t) : Language_extension.maturity =
  let legacy_layout = Const.get_legacy_layout jkind in
  match context, legacy_layout with
  | ( _,
      ( Value | Immediate | Immediate64 | Any | Float64 | Float32 | Word
      | Bits32 | Bits64 | Any_non_null ) ) ->
    (* CR layouts v3.0: we allow [Any_non_null] because, without [Alpha],
       explicit [Any] annotations are converted to [Any_non_null] to
       preserve compatibility with array arguments. *)
    Stable
  | _, (Value_or_null | Void) -> Alpha

(******************************)
(* construction *)

let of_new_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Maybe_null in
  fresh_jkind jkind ~why:(Concrete_creation why), sort

let of_new_sort ~why = fst (of_new_sort_var ~why)

let of_new_default_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Non_null in
  fresh_jkind jkind ~why:(Concrete_default_creation why), sort

let of_new_default_sort ~why = fst (of_new_default_sort_var ~why)

(* CR layouts v2.8: remove this function *)
let of_const ~why
    ({ layout;
       modes_upper_bounds;
       externality_upper_bound;
       nullability_upper_bound
     } :
      Const.t) =
  { jkind =
      { layout = Layout.of_const layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      };
    history = Creation why;
    has_warned = false
  }

let const_of_user_written_annotation ~context Location.{ loc; txt = annot } =
  let const = Const.of_user_written_annotation_unchecked_level annot in
  let required_layouts_level = get_required_layouts_level context const in
  if not (Language_extension.is_at_least Layouts required_layouts_level)
  then raise ~loc (Insufficient_level { jkind = const; required_layouts_level });
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
  let const = Const.of_attribute attribute.txt in
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
                 let name = Builtin_attributes.jkind_attribute_to_string attr in
                 Jane_syntax.Jkind.(Abbreviation (Const.mk name Location.none)))
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
  if all_void
  then Primitive.immediate ~why:Empty_record
  else Primitive.value ~why:Boxed_record

let for_boxed_variant ~all_voids =
  if all_voids
  then Primitive.immediate ~why:Enumeration
  else Primitive.value ~why:Boxed_variant

(******************************)
(* elimination and defaulting *)

let default_to_value_and_get
    { jkind =
        { layout;
          modes_upper_bounds;
          externality_upper_bound;
          nullability_upper_bound
        };
      _
    } : Const.t =
  match layout with
  | Any ->
    { layout = Any;
      modes_upper_bounds;
      externality_upper_bound;
      nullability_upper_bound
    }
  | Sort s ->
    { layout = Sort (Sort.default_to_value_and_get s);
      modes_upper_bounds;
      externality_upper_bound;
      nullability_upper_bound
    }

let default_to_value t = ignore (default_to_value_and_get t)

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

let set_externality_upper_bound jk externality_upper_bound =
  { jk with jkind = { jk.jkind with externality_upper_bound } }

(*********************************)
(* pretty printing *)

let format ppf jkind =
  match get jkind with
  | Const c -> Format.fprintf ppf "%a" Const.format c
  | Var v -> Format.fprintf ppf "%s" (Sort.Var.name v)

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
    | Layout_poly_in_external ->
      fprintf ppf
        "it's the layout polymorphic type in an external declaration@ \
         ([@@layout_poly] forces all variables of layout 'any' to be@ \
         representable at call sites)"

  let format_concrete_default_creation_reason ppf :
      History.concrete_default_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "it instantiates an unannotated type parameter of %a"
        !printtyp_path path
    | Wildcard -> fprintf ppf "it's a _ in the type"
    | Unification_var -> fprintf ppf "it's a fresh unification variable"
    | Array_element -> fprintf ppf "it's the type of an array element"

  let rec format_annotation_context ppf : History.annotation_context -> unit =
    function
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

  let format_any_non_null_creation_reason ppf :
      History.any_non_null_creation_reason -> unit = function
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

  let format_immediate64_creation_reason ppf :
      History.immediate64_creation_reason -> _ = function
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"

  let format_value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Tuple_element -> fprintf ppf "it's the type of a tuple element"
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"
    | Polymorphic_variant_field ->
      fprintf ppf "it's the type of the field of a polymorphic variant"
    | Structure_element ->
      fprintf ppf "it's the type of something stored in a module structure"
    | V1_safety_check ->
      fprintf ppf "it has to be value for the V1 safety check"

  let format_value_creation_reason ppf ~layout_or_kind :
      History.value_creation_reason -> _ = function
    | Class_let_binding ->
      fprintf ppf "it's the type of a let-bound variable in a class expression"
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

  let format_float64_creation_reason ppf : History.float64_creation_reason -> _
      = function
    | Primitive id ->
      fprintf ppf "it is the primitive float64 type %s" (Ident.name id)

  let format_float32_creation_reason ppf : History.float32_creation_reason -> _
      = function
    | Primitive id ->
      fprintf ppf "it is the primitive float32 type %s" (Ident.name id)

  let format_word_creation_reason ppf : History.word_creation_reason -> _ =
    function
    | Primitive id ->
      fprintf ppf "it is the primitive word type %s" (Ident.name id)

  let format_bits32_creation_reason ppf : History.bits32_creation_reason -> _ =
    function
    | Primitive id ->
      fprintf ppf "it is the primitive bits32 type %s" (Ident.name id)

  let format_bits64_creation_reason ppf : History.bits64_creation_reason -> _ =
    function
    | Primitive id ->
      fprintf ppf "it is the primitive bits64 type %s" (Ident.name id)

  let format_creation_reason ppf ~layout_or_kind :
      History.creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Any_creation any -> format_any_creation_reason ppf any
    | Any_non_null_creation any -> format_any_non_null_creation_reason ppf any
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Immediate64_creation immediate64 ->
      format_immediate64_creation_reason ppf immediate64
    | Void_creation _ -> .
    | Value_or_null_creation value ->
      format_value_or_null_creation_reason ppf value
    | Value_creation value ->
      format_value_creation_reason ppf ~layout_or_kind value
    | Float64_creation float -> format_float64_creation_reason ppf float
    | Float32_creation float -> format_float32_creation_reason ppf float
    | Word_creation word -> format_word_creation_reason ppf word
    | Bits32_creation bits32 -> format_bits32_creation_reason ppf bits32
    | Bits64_creation bits64 -> format_bits64_creation_reason ppf bits64
    | Concrete_creation concrete -> format_concrete_creation_reason ppf concrete
    | Concrete_default_creation concrete ->
      format_concrete_default_creation_reason ppf concrete
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
    | Creation reason -> (
      fprintf ppf "@ because %a" (format_creation_reason ~layout_or_kind) reason;
      match reason, jkind_desc with
      | Concrete_default_creation _, Const _ ->
        fprintf ppf ",@ defaulted to %s %a" layout_or_kind Desc.format
          jkind_desc
      | _ -> ())
    | _ -> assert false);
    fprintf ppf ".";
    (match t.history with
    | Creation (Annotated (With_error_message (message, _), _)) ->
      fprintf ppf "@ @[%s@]" message
    | _ -> ());
    fprintf ppf "@]"

  (* this isn't really formatted for user consumption *)
  let format_history_tree ~intro ~layout_or_kind ppf t =
    let rec in_order ppf = function
      | Interact
          { reason; lhs_history; rhs_history; lhs_jkind = _; rhs_jkind = _ } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]" in_order lhs_history
          format_interact_reason reason in_order rhs_history
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

let format_history = Format_history.format_history ~layout_or_kind:"kind"

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

  type locale =
    | Mode
    | Layout

  let report_general preamble pp_former former ppf t =
    let mismatch_type =
      match t.violation with
      | Not_a_subjkind (k1, k2) ->
        if Misc.Le_result.is_le (Layout.sub k1.jkind.layout k2.jkind.layout)
        then Mode
        else Layout
      | No_intersection _ -> Layout
    in
    let layout_or_kind =
      match mismatch_type with Mode -> "kind" | Layout -> "layout"
    in
    let format_layout_or_kind =
      match mismatch_type with
      | Mode -> fun ppf jkind -> Format.fprintf ppf "@,%a" format jkind
      | Layout -> fun ppf jkind -> Layout.format ppf jkind.jkind.layout
    in
    let subjkind_format verb k2 =
      match get k2 with
      | Var _ -> dprintf "%s representable" verb
      | Const _ ->
        dprintf "%s a sub%s of %a" verb layout_or_kind format_layout_or_kind k2
    in
    let k1, k2, fmt_k1, fmt_k2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (k1, k2); missing_cmi } -> (
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
          ( k1,
            k2,
            dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
            subjkind_format "is not" k2,
            None )
        | Some p ->
          ( k1,
            k2,
            dprintf "an unknown %s" layout_or_kind,
            subjkind_format "might not be" k2,
            Some p ))
      | { violation = No_intersection (k1, k2); missing_cmi } ->
        assert (Option.is_none missing_cmi);
        ( k1,
          k2,
          dprintf "%s %a" layout_or_kind format_layout_or_kind k1,
          dprintf "does not overlap with %a" format_layout_or_kind k2,
          None )
    in
    if display_histories
    then
      let connective =
        match t.violation, get k2 with
        | Not_a_subjkind _, Const _ ->
          dprintf "be a sub%s of %a" layout_or_kind format_layout_or_kind k2
        | No_intersection _, Const _ ->
          dprintf "overlap with %a" format_layout_or_kind k2
        | _, Var _ -> dprintf "be representable"
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
    report_missing_cmi ppf missing_cmi_option

  let pp_t ppf x = fprintf ppf "%t" x

  let report_with_offender ~offender = report_general "" pp_t offender

  let report_with_offender_sort ~offender =
    report_general "A representable layout was expected, but " pp_t offender

  let report_with_name ~name = report_general "" pp_print_string name
end

(******************************)
(* relations *)

let equate_or_equal ~allow_mutation
    { jkind = jkind1; history = _; has_warned = _ }
    { jkind = jkind2; history = _; has_warned = _ } =
  Jkind_desc.equate_or_equal ~allow_mutation jkind1 jkind2

(* CR layouts v2.8: Switch this back to ~allow_mutation:false *)
let equal = equate_or_equal ~allow_mutation:true

let () = Types.set_jkind_equal equal

let equate = equate_or_equal ~allow_mutation:true

(* Not all jkind history reasons are created equal. Some are more helpful than others.
    This function encodes that information.

    The reason with higher score should get preserved when combined with one of lower
    score. *)
let score_reason = function
  (* error_message annotated by the user should always take priority *)
  | Creation (Annotated (With_error_message _, _)) -> 1
  (* Concrete creation is quite vague, prefer more specific reasons *)
  | Creation (Concrete_creation _ | Concrete_default_creation _) -> -1
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

let has_intersection t1 t2 =
  Option.is_some (Jkind_desc.intersection t1.jkind t2.jkind)

let intersection_or_error ~reason t1 t2 =
  match Jkind_desc.intersection t1.jkind t2.jkind with
  | None -> Error (Violation.of_ (No_intersection (t1, t2)))
  | Some jkind ->
    Ok
      { jkind;
        history = combine_histories reason t1 t2;
        has_warned = t1.has_warned || t2.has_warned
      }

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

(* This doesn't do any mutation because mutating a sort variable can't make it
   any, and modal upper bounds are constant. *)
let is_max jkind = sub Primitive.any_dummy_jkind jkind

let has_layout_any jkind =
  match jkind.jkind.layout with Any -> true | _ -> false

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
    | Let_binding -> fprintf ppf "Let_binding"
    | Function_argument -> fprintf ppf "Function_argument"
    | Function_result -> fprintf ppf "Function_result"
    | Structure_item_expression -> fprintf ppf "Structure_item_expression"
    | External_argument -> fprintf ppf "External_argument"
    | External_result -> fprintf ppf "External_result"
    | Statement -> fprintf ppf "Statement"
    | Optional_arg_default -> fprintf ppf "Optional_arg_default"
    | Layout_poly_in_external -> fprintf ppf "Layout_poly_in_external"

  let concrete_default_creation_reason ppf :
      History.concrete_default_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "Unannotated_type_parameter %a" !printtyp_path path
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Array_element -> fprintf ppf "Array_element"

  let rec annotation_context ppf : History.annotation_context -> unit = function
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

  let any_creation_reason ppf : History.any_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Type_expression_call -> fprintf ppf "Type_expression_call"
    | Inside_of_Tarrow -> fprintf ppf "Inside_of_Tarrow"

  let any_non_null_creation_reason ppf :
      History.any_non_null_creation_reason -> unit = function
    | Array_type_argument -> fprintf ppf "Array_type_argument"

  let immediate_creation_reason ppf : History.immediate_creation_reason -> _ =
    function
    | Empty_record -> fprintf ppf "Empty_record"
    | Enumeration -> fprintf ppf "Enumeration"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf "Immediate_polymorphic_variant"

  let immediate64_creation_reason ppf : History.immediate64_creation_reason -> _
      = function
    | Separability_check -> fprintf ppf "Separability_check"

  let value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Tuple_element -> fprintf ppf "Tuple_element"
    | Separability_check -> fprintf ppf "Separability_check"
    | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
    | Structure_element -> fprintf ppf "Structure_element"
    | V1_safety_check -> fprintf ppf "V1_safety_check"

  let value_creation_reason ppf : History.value_creation_reason -> _ = function
    | Class_let_binding -> fprintf ppf "Class_let_binding"
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
    | Univar -> fprintf ppf "Univar"
    | Default_type_jkind -> fprintf ppf "Default_type_jkind"
    | Existential_type_variable -> fprintf ppf "Existential_type_variable"
    | Array_comprehension_element -> fprintf ppf "Array_comprehension_element"
    | Lazy_expression -> fprintf ppf "Lazy_expression"
    | Class_type_argument -> fprintf ppf "Class_type_argument"
    | Class_term_argument -> fprintf ppf "Class_term_argument"
    | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
    | Captured_in_object -> fprintf ppf "Captured_in_object"
    | Recmod_fun_arg -> fprintf ppf "Recmod_fun_arg"
    | Unknown s -> fprintf ppf "Unknown %s" s

  let float64_creation_reason ppf : History.float64_creation_reason -> _ =
    function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let float32_creation_reason ppf : History.float32_creation_reason -> _ =
    function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let word_creation_reason ppf : History.word_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let bits32_creation_reason ppf : History.bits32_creation_reason -> _ =
    function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let bits64_creation_reason ppf : History.bits64_creation_reason -> _ =
    function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let creation_reason ppf : History.creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Any_non_null_creation any ->
      fprintf ppf "Any_non_null_creation %a" any_non_null_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Immediate64_creation immediate64 ->
      fprintf ppf "Immediate64_creation %a" immediate64_creation_reason
        immediate64
    | Value_or_null_creation value ->
      fprintf ppf "Value_or_null_creation %a" value_or_null_creation_reason
        value
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Float64_creation float ->
      fprintf ppf "Float64_creation %a" float64_creation_reason float
    | Float32_creation float ->
      fprintf ppf "Float32_creation %a" float32_creation_reason float
    | Word_creation word ->
      fprintf ppf "Word_creation %a" word_creation_reason word
    | Bits32_creation bits32 ->
      fprintf ppf "Bits32_creation %a" bits32_creation_reason bits32
    | Bits64_creation bits64 ->
      fprintf ppf "Bits64_creation %a" bits64_creation_reason bits64
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_creation_reason concrete
    | Concrete_default_creation concrete ->
      fprintf ppf "Concrete_default_creation %a"
        concrete_default_creation_reason concrete
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

  let rec history ppf = function
    | Interact { reason; lhs_jkind; lhs_history; rhs_jkind; rhs_history } ->
      fprintf ppf
        "Interact {@[reason = %a;@ lhs_jkind = %a;@ lhs_history = %a;@ \
         rhs_jkind = %a;@ rhs_history = %a}@]"
        interact_reason reason Jkind_desc.Debug_printers.t lhs_jkind history
        lhs_history Jkind_desc.Debug_printers.t rhs_jkind history rhs_history
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  let t ppf ({ jkind; history = h; has_warned = _ } : t) : unit =
    fprintf ppf "@[<v 2>{ jkind = %a@,; history = %a }@]"
      Jkind_desc.Debug_printers.t jkind history h
end

(*** formatting user errors ***)
let report_error ~loc : Error.t -> _ = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]" Pprintast.jkind jkind
  | Unknown_mode mode ->
    Location.errorf ~loc "@[<v>Unknown mode %a@]" Pprintast.mode mode
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%a) and a layout attribute \
       ([@@@@%a]).@]"
      Const.format_no_hiding from_annotation Const.format from_attribute
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
        Const.format_no_hiding jkind hint)

let () =
  Location.register_error_of_exn (function
    | Error.User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(* CR layouts v2.8: Remove the definitions below by propagating changes
   outside of this file. *)

type annotation = Const.t * Jane_syntax.Jkind.annotation

let default_to_value_and_get t = default_to_value_and_get t
