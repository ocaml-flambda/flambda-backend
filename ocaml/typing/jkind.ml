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

  module Const = struct
    type t = Const.t =
      | Sort of Sort.const
      | Any

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

    module Debug_printers = struct
      open Format

      let t ppf = function
        | Any -> fprintf ppf "Any"
        | Sort s -> fprintf ppf "Sort %a" Sort.Const.Debug_printers.t s
    end
  end

  type nonrec t = t =
    | Sort of Sort.t
    | Union of Sort.t list
    | Any

  let of_const (const : Const.t) : t =
    match const with Sort s -> Sort (Const s) | Any -> Any

  let union_with_negative_polarity where =
    Misc.fatal_errorf "%s: Union of sorts used in a negative position" where

  let union_sort_consts cs : Const.t option =
    match cs with
    | [] -> None
    | c0 :: cs ->
      if List.for_all (fun c -> Sort.Const.equal c c0) cs
      then Some (Sort c0)
      else Some Any

  let repr_union ss =
    let cs, vs =
      List.partition_map
        (fun s -> match Sort.get s with Const s -> Left s | Var v -> Right v)
        ss
    in
    let layout = union_sort_consts cs in
    layout, vs

  (* Post-condition: If [t] is [Union],
     it contains at most one [Const] and at least one [Var]. *)
  let repr t =
    match t with
    | Any | Sort _ -> t
    | Union ss -> (
      match repr_union ss with
      | Some Any, _ -> Any
      | Some (Sort s), [] -> Sort (Sort.of_const s)
      | None, [v] -> Sort (Sort.of_var v)
      | _ -> t)

  let equate_or_equal ~allow_mutation t1 t2 =
    match repr t1, repr t2 with
    | _ when t1 == t2 -> true
    | Union _, _ | _, Union _ ->
      union_with_negative_polarity "Jkind.Layout.equate_or_equal"
    | Sort s1, Sort s2 -> (
      match Sort.equate_tracking_mutation s1 s2 with
      | (Equal_mutated_first | Equal_mutated_second) when not allow_mutation ->
        Misc.fatal_errorf "Jkind.equal: Performed unexpected mutation"
      | Unequal -> false
      | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second -> true)
    | Any, Any -> true
    | (Any | Sort _), _ -> false

  let rec sub t1 t2 : Misc.Le_result.t =
    match repr t1, repr t2 with
    | _ when t1 == t2 -> Equal
    | _, Union _ -> union_with_negative_polarity "Jkind.Layout.sub"
    | Union ss1, t2 ->
      Misc.Le_result.combine_list (List.map (fun s1 -> sub (Sort s1) t2) ss1)
    | Any, Any -> Equal
    | _, Any -> Less
    | Any, _ -> Not_le
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Equal else Not_le

  let intersection t1 t2 =
    match repr t1, repr t2 with
    | _ when t1 == t2 -> Some t1
    | Union _, _ | _, Union _ ->
      union_with_negative_polarity "Jkind.Layout.intersection"
    | _, Any -> Some t1
    | Any, _ -> Some t2
    | Sort s1, Sort s2 -> if Sort.equate s1 s2 then Some t1 else None

  let union t1 t2 =
    match repr t1, repr t2 with
    | _ when t1 == t2 -> t1
    | _, Any -> Any
    | Any, _ -> Any
    | Sort s1, Sort s2 -> Union [s1; s2]
    | Union ss1, Sort s2 -> Union (s2 :: ss1)
    | Sort s1, Union ss2 -> Union (s1 :: ss2)
    | Union ss1, Union ss2 -> Union (ss1 @ ss2)

  let of_new_sort_var () =
    let sort = Sort.new_var () in
    Sort sort, sort

  let format ppf =
    let open Format in
    let print_sort ppf s =
      match Sort.get s with
      | Const s -> fprintf ppf "%a" Sort.Const.format s
      | Var v -> fprintf ppf "%s" (Sort.Var.name v)
    in
    function
    | Any -> fprintf ppf "any"
    | Sort s -> print_sort ppf s
    | Union ss ->
      fprintf ppf "%a"
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf "%s" " | ")
           print_sort)
        ss

  module Debug_printers = struct
    open Format

    let t ppf = function
      | Any -> fprintf ppf "Any"
      | Sort s -> fprintf ppf "Sort %a" Sort.Debug_printers.t s
      | Union ss ->
        fprintf ppf "Union [%a]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf "; ")
             Sort.Debug_printers.t)
          ss
  end
end

module Externality = Jkind_types.Externality
module Nullability = Jkind_types.Nullability

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

  let is_imported t =
    match t.history with Creation Imported -> true | _ -> false

  let update_reason t reason = { t with history = Creation reason }

  let with_warning t = { t with has_warned = true }

  let has_warned t = t.has_warned
end

(* forward declare [Const.t] so we can use it for [Error.t] *)
type const = type_expr Jkind_types.Const.t

type higher_const = type_expr Jkind_types.Higher_const.t

(******************************)
(*** user errors ***)

module Error = struct
  type t =
    | Insufficient_level of
        { jkind : higher_const;
          required_layouts_level : Language_extension.maturity
        }
    | Unknown_jkind of Jane_syntax.Jkind.t
    | Multiple_jkinds of
        { from_annotation : higher_const;
          from_attribute : higher_const
        }
    | Unexpected_higher_jkind of Jane_syntax.Jkind.t

  exception User_error of Location.t * t
end

let raise ~loc err = raise (Error.User_error (loc, err))

module Const = struct
  open Jkind_types.Const

  type t = const

  type higher = Types.type_expr Higher_const.t

  let max =
    { layout = Layout.Const.max;
      modes_upper_bounds = Modes.max;
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Nullability.max
    }

  let get_layout const = const.layout

  let get_modal_upper_bounds const = const.modes_upper_bounds

  let get_externality_upper_bound const = const.externality_upper_bound

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

  module Builtin = struct
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
          of_layout (Sort Value) ~mode_crossing:false ~nullability:Maybe_null;
        name = "value_or_null"
      }

    let value =
      { jkind =
          of_layout (Sort Value) ~mode_crossing:false ~nullability:Non_null;
        name = "value"
      }

    let immutable_data =
      { jkind =
          { layout = Sort Value;
            modes_upper_bounds =
              { linearity = Linearity.Const.min;
                contention = Contention.Const.min;
                portability = Portability.Const.min;
                uniqueness = Uniqueness.Const.max;
                areality = Locality.Const.max
              };
            externality_upper_bound = Externality.max;
            nullability_upper_bound = Nullability.Non_null
          };
        name = "immutable_data"
      }

    let mutable_data =
      { jkind =
          { layout = Sort Value;
            modes_upper_bounds =
              { linearity = Linearity.Const.min;
                contention = Contention.Const.max;
                portability = Portability.Const.min;
                uniqueness = Uniqueness.Const.max;
                areality = Locality.Const.max
              };
            externality_upper_bound = Externality.max;
            nullability_upper_bound = Nullability.Non_null
          };
        name = "mutable_data"
      }

    (* CR layouts v3: change to [or_null] when separability is implemented. *)
    let void =
      { jkind = of_layout (Sort Void) ~mode_crossing:false ~nullability:Non_null;
        name = "void"
      }

    let immediate =
      { jkind = of_layout (Sort Value) ~mode_crossing:true ~nullability:Non_null;
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
          of_layout (Sort Float64) ~mode_crossing:true ~nullability:Non_null;
        name = "float64"
      }

    (* CR layouts v2.8: This should not mode cross, but we need syntax for mode
       crossing first *)
    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let float32 =
      { jkind =
          of_layout (Sort Float32) ~mode_crossing:true ~nullability:Non_null;
        name = "float32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let word =
      { jkind = of_layout (Sort Word) ~mode_crossing:false ~nullability:Non_null;
        name = "word"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits32 =
      { jkind =
          of_layout (Sort Bits32) ~mode_crossing:false ~nullability:Non_null;
        name = "bits32"
      }

    (* CR layouts v3: change to [Maybe_null] when separability is implemented. *)
    let bits64 =
      { jkind =
          of_layout (Sort Bits64) ~mode_crossing:false ~nullability:Non_null;
        name = "bits64"
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
        immutable_data;
        mutable_data;
        void;
        immediate;
        immediate64;
        float64;
        float32;
        word;
        bits32;
        bits64 ]
  end

  module To_out_jkind_const : sig
    (** Convert a [t] into a [Outcometree.out_jkind_const].
        The jkind is written in terms of the built-in jkind that requires the least amount
        of modes after the mod. For example,
        [value mod global many unique portable uncontended external_ non_null] could be
        written in terms of [value] (as it appears above), or in terms of [immediate]
        (which would just be [immediate]). Since the latter requires less modes to be
        printed, it is chosen. *)
    val convert : allow_null:bool -> t -> Outcometree.out_jkind_const
  end = struct
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
    let convert_with_base ~(base : Builtin.t) actual =
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

    let convert ~allow_null jkind =
      (* For each primitive jkind, we try to print the jkind in terms of it (this is
         possible if the primitive is a subjkind of it). We then choose the "simplest". The
           "simplest" is taken to mean the one with the least number of modes that need to
         follow the [mod]. *)
      let simplest =
        (* CR layouts v3.0: remove this hack once [or_null] is out of [Alpha]. *)
        (if allow_null then Builtin.all else Builtin.all_non_null)
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

  let to_out_jkind_const jkind =
    let allow_null = Language_extension.(is_at_least Layouts Alpha) in
    To_out_jkind_const.convert ~allow_null jkind

  let format ppf jkind = to_out_jkind_const jkind |> !Oprint.out_jkind_const ppf

  let format_no_hiding ppf jkind =
    To_out_jkind_const.convert ~allow_null:true jkind
    |> !Oprint.out_jkind_const ppf

  let rec format_higher_gen ?(hiding = true) ppf (t : higher) =
    let open Format in
    let format_higher = format_higher_gen ~hiding in
    match t with
    | Type t -> if hiding then format ppf t else format_no_hiding ppf t
    | Arrow ([((Type _ | Top) as arg)], result) ->
      fprintf ppf "%a => %a" format_higher arg format_higher result
    | Arrow (args, result) ->
      fprintf ppf "(%a) => %a"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") format_higher)
        args format_higher result
    | Top -> Format.fprintf ppf "top"

  let format_higher = format_higher_gen ~hiding:true

  let format_higher_no_hiding = format_higher_gen ~hiding:false

  let of_attribute : Builtin_attributes.jkind_attribute -> t = function
    | Immediate -> Builtin.immediate.jkind
    | Immediate64 -> Builtin.immediate64.jkind

  let rec of_user_written_annotation_unchecked_level ~loc
      (jkind : Jane_syntax.Jkind.t) : t =
    match jkind with
    | Abbreviation { txt = name; loc } -> (
      (* CR layouts v2.8: move this to predef *)
      match name with
      (* CR layouts v3.0: remove this hack once non-null jkinds are out of alpha.
         It is confusing, but preserves backwards compatibility for arrays. *)
      | "any" when Language_extension.(is_at_least Layouts Alpha) ->
        Builtin.any.jkind
      | "any" -> Builtin.any_non_null.jkind
      | "any_non_null" -> Builtin.any_non_null.jkind
      | "value_or_null" -> Builtin.value_or_null.jkind
      | "value" -> Builtin.value.jkind
      | "void" -> Builtin.void.jkind
      | "immediate64" -> Builtin.immediate64.jkind
      | "immediate" -> Builtin.immediate.jkind
      | "float64" -> Builtin.float64.jkind
      | "float32" -> Builtin.float32.jkind
      | "word" -> Builtin.word.jkind
      | "bits32" -> Builtin.bits32.jkind
      | "bits64" -> Builtin.bits64.jkind
      | _ -> raise ~loc (Unknown_jkind jkind))
    | Mod (jkind, modifiers) ->
      let base = of_user_written_annotation_unchecked_level ~loc jkind in
      (* for each mode, lower the corresponding modal bound to be that mode *)
      let parsed_modifiers = Typemodifier.transl_modifier_annots modifiers in
      { layout = base.layout;
        modes_upper_bounds =
          Alloc.Const.meet base.modes_upper_bounds
            (Alloc.Const.Option.value ~default:Alloc.Const.max
               parsed_modifiers.modal_upper_bounds);
        nullability_upper_bound =
          Nullability.meet base.nullability_upper_bound
            (Option.value ~default:Nullability.max
               parsed_modifiers.nullability_upper_bound);
        externality_upper_bound =
          Externality.meet base.externality_upper_bound
            (Option.value ~default:Externality.max
               parsed_modifiers.externality_upper_bound)
      }
    | Arrow _ -> raise ~loc (Unexpected_higher_jkind jkind)
    | Default | With _ | Kind_of _ -> Misc.fatal_error "XXX unimplemented"

  let rec higher_of_user_written_annotation_unchecked_level ~loc
      (jkind : Jane_syntax.Jkind.t) : higher =
    match jkind with
    | Abbreviation { txt = "top"; loc = _ } -> Top
    | Arrow (args, result) ->
      Arrow
        ( List.map (higher_of_user_written_annotation_unchecked_level ~loc) args,
          higher_of_user_written_annotation_unchecked_level ~loc result )
    | _ -> Type (of_user_written_annotation_unchecked_level ~loc jkind)

  (* The [annotation_context] parameter can be used to allow annotations / kinds
     in different contexts to be enabled with different extension settings.
     At some points in time, we will not care about the context, and so this
     parameter might effectively be unused.
  *)
  (* CR layouts: When everything is stable, remove this function. *)
  let get_required_layouts_level (_context : History.annotation_context)
      (jkind : higher) : Language_extension.maturity =
    match jkind with
    | Type jkind -> (
      match jkind.layout, jkind.nullability_upper_bound with
      | (Sort (Float64 | Float32 | Word | Bits32 | Bits64) | Any), _
      | Sort Value, Non_null ->
        Stable
      | Sort Void, _ | Sort Value, Maybe_null -> Alpha)
    | Arrow _ | Top -> Alpha

  let higher_of_user_written_annotation ~context Location.{ loc; txt = annot } =
    let const = higher_of_user_written_annotation_unchecked_level ~loc annot in
    let required_layouts_level = get_required_layouts_level context const in
    if not (Language_extension.is_at_least Layouts required_layouts_level)
    then
      raise ~loc (Insufficient_level { jkind = const; required_layouts_level });
    const

  let of_user_written_annotation ~context (annot : _ Location.loc) =
    match higher_of_user_written_annotation ~context annot with
    | Type ty -> ty
    | Arrow _ | Top -> raise ~loc:annot.loc (Unexpected_higher_jkind annot.txt)
end

module Desc = struct
  type t =
    | Const of Const.t
    | Var of Layout.Const.t option * Sort.var list (* all modes will be [max] *)

  (* considers sort variables < Any. Two sort variables are in a [sub]
     relationship only when they are equal.
     Never does mutation.
     Pre-condition: no filled-in sort variables. *)
  let sub d1 d2 : Misc.Le_result.t =
    match d1, d2 with
    | Const c1, Const c2 -> Const.sub c1 c2
    | Var _, Const c when Const.equal Const.max c -> Less
    | Var (None, [v1]), Var (None, [v2]) -> if v1 == v2 then Equal else Not_le
    | Var (l1, vs1), Var (l2, vs2) ->
      if l1 == l2 && vs1 == vs2 then Equal else Not_le
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

  let add_nullability_crossing t =
    { t with nullability_upper_bound = Nullability.min }

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

  let union
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
    { layout = Layout.union lay1 lay2;
      modes_upper_bounds = Modes.join modes1 modes2;
      externality_upper_bound = Externality.join ext1 ext2;
      nullability_upper_bound = Nullability.join null1 null2
    }

  let of_new_sort_var nullability_upper_bound =
    let layout, sort = Layout.of_new_sort_var () in
    ( { layout;
        modes_upper_bounds = Modes.max;
        externality_upper_bound = Externality.max;
        nullability_upper_bound
      },
      sort )

  module Builtin = struct
    let any = max

    let value_or_null = of_const Const.Builtin.value_or_null.jkind

    let value = of_const Const.Builtin.value.jkind

    let void = of_const Const.Builtin.void.jkind

    let immediate = of_const Const.Builtin.immediate.jkind
  end

  (* Post-condition: If the result is [Var v], then [!v] is [None]. *)
  let get
      { layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      } : Desc.t =
    match Layout.repr layout with
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
      | Var v -> Var (None, [v]))
    | Union ss ->
      (* [repr] already simplified as much as possible *)
      let oc, vs = Layout.repr_union ss in
      Var (oc, vs)

  let format ppf t =
    let print_vars ppf =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " | ")
        (fun ppf v -> Format.fprintf ppf "%s" (Sort.Var.name v))
        ppf
    in
    match get t with
    | Const c -> Format.fprintf ppf "%a" Const.format c
    | Var (None, vs) -> Format.fprintf ppf "%a" print_vars vs
    | Var (Some c, vs) ->
      Format.fprintf ppf "%a | %s" print_vars vs (Layout.Const.to_string c)

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

type higher = type_expr Jkind_types.higher_jkind

let fresh_jkind desc ~why = { desc; history = Creation why; has_warned = false }

(******************************)
(* constants *)

module Builtin = struct
  let any_dummy_jkind =
    { desc = Jkind_desc.max;
      history = Creation (Any_creation Dummy_jkind);
      has_warned = false
    }

  (* CR layouts: Should we be doing more memoization here? *)
  let any ~(why : History.any_creation_reason) =
    match why with
    | Dummy_jkind -> any_dummy_jkind (* share this one common case *)
    | _ -> fresh_jkind Jkind_desc.Builtin.any ~why:(Any_creation why)

  let value_v1_safety_check =
    { desc = Jkind_desc.Builtin.value_or_null;
      history = Creation (Value_or_null_creation V1_safety_check);
      has_warned = false
    }

  let void ~why = fresh_jkind Jkind_desc.Builtin.void ~why:(Void_creation why)

  let value_or_null ~why =
    match (why : History.value_or_null_creation_reason) with
    | V1_safety_check -> value_v1_safety_check
    | _ ->
      fresh_jkind Jkind_desc.Builtin.value_or_null
        ~why:(Value_or_null_creation why)

  let value ~(why : History.value_creation_reason) =
    fresh_jkind Jkind_desc.Builtin.value ~why:(Value_creation why)

  let immediate ~why =
    fresh_jkind Jkind_desc.Builtin.immediate ~why:(Immediate_creation why)
end

let add_mode_crossing t = { t with desc = Jkind_desc.add_mode_crossing t.desc }

let add_nullability_crossing t =
  { t with desc = Jkind_desc.add_nullability_crossing t.desc }

let add_portability_and_contention_crossing ~from t =
  let desc, added_crossings =
    Jkind_desc.add_portability_and_contention_crossing ~from:from.desc t.desc
  in
  { t with desc }, added_crossings

(******************************)
(* construction *)

let of_new_sort_var ~why =
  let desc, sort = Jkind_desc.of_new_sort_var Maybe_null in
  fresh_jkind desc ~why:(Concrete_creation why), sort

let of_new_sort ~why = fst (of_new_sort_var ~why)

let of_new_legacy_sort_var ~why =
  let jkind, sort = Jkind_desc.of_new_sort_var Non_null in
  fresh_jkind jkind ~why:(Concrete_legacy_creation why), sort

let of_new_legacy_sort ~why = fst (of_new_legacy_sort_var ~why)

let of_const ~why
    ({ layout;
       modes_upper_bounds;
       externality_upper_bound;
       nullability_upper_bound
     } :
      Const.t) =
  { desc =
      { layout = Layout.of_const layout;
        modes_upper_bounds;
        externality_upper_bound;
        nullability_upper_bound
      };
    history = Creation why;
    has_warned = false
  }

let rec higher_of_const ~why (const : higher_const) : higher =
  match const with
  | Type ty -> of_const ~why ty |> wrap_higher_jkind
  | Arrow (args, result) ->
    { hdesc =
        Arrow
          (List.map (higher_of_const ~why) args, (higher_of_const ~why) result);
      hhistory = Creation why
    }
  | Top -> { hdesc = Top; hhistory = Creation why }

let of_annotated_const ~context ~const ~const_loc =
  of_const ~why:(Annotated (context, const_loc)) const

let higher_of_annotated_const ~context ~const ~const_loc =
  higher_of_const ~why:(Annotated (context, const_loc)) const

let higher_of_annotation ~context (annot : _ Location.loc) =
  let const = Const.higher_of_user_written_annotation ~context annot in
  let jkind = higher_of_annotated_const ~const ~const_loc:annot.loc ~context in
  jkind, (const, annot)

let higher_of_annotation_option_default ~default ~context =
  Option.fold ~none:(default, None) ~some:(fun annot ->
      let t, annot = higher_of_annotation ~context annot in
      t, Some annot)

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let const = Const.of_attribute attribute.txt in
  ( of_annotated_const ~context ~const ~const_loc:attribute.loc,
    Higher_const.Type const )

let higher_of_type_decl ~context (decl : Parsetree.type_declaration) =
  let jkind_of_annotation =
    Jane_syntax.Layouts.of_type_declaration decl
    |> Option.map (fun (annot, attrs) ->
           let t, const = higher_of_annotation ~context annot in
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
           wrap_higher_jkind t, (const, annot), decl.ptype_attributes)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | (Some _ as x), None | None, (Some _ as x) -> x
  | Some (_, (from_annotation, _), _), Some (_, (from_attribute, _), _) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let higher_of_type_decl_default ~context ~default
    (decl : Parsetree.type_declaration) =
  match higher_of_type_decl ~context decl with
  | Some (t, const, attrs) -> t, Some const, attrs
  | None -> default, None, decl.ptype_attributes

let for_boxed_record ~all_void =
  if all_void
  then Builtin.immediate ~why:Empty_record
  else Builtin.value ~why:Boxed_record

let for_boxed_variant ~all_voids =
  if all_voids
  then Builtin.immediate ~why:Enumeration
  else Builtin.value ~why:Boxed_variant

let for_arrow =
  fresh_jkind
    { layout = Sort (Const Value);
      modes_upper_bounds =
        { linearity = Linearity.Const.max;
          areality = Locality.Const.max;
          uniqueness = Uniqueness.Const.min;
          portability = Portability.Const.max;
          contention = Contention.Const.min
        };
      externality_upper_bound = Externality.max;
      nullability_upper_bound = Non_null
    }
    ~why:(Value_creation Arrow)

(******************************)
(* elimination and defaulting *)

let default_to_value_and_get
    { desc =
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
  | Union ss ->
    { layout =
        Layout.union_sort_consts (List.map Sort.default_to_value_and_get ss)
        |> Option.value ~default:Const.Builtin.value.jkind.layout;
      modes_upper_bounds;
      externality_upper_bound;
      nullability_upper_bound
    }

let default_to_value t = ignore (default_to_value_and_get t)

let get t = Jkind_desc.get t.desc

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let sort_of_jkind l =
  match get l with
  | Const { layout = Sort s; _ } -> Sort.of_const s
  | Var (None, [v]) -> Sort.of_var v
  | Const { layout = Any; _ } -> Misc.fatal_error "Jkind.sort_of_jkind: Any"
  | Var (Some _, _) | Var (_, ([] | _ :: _ :: _)) ->
    Misc.fatal_error "Jkind.sort_of_jkind: union of Var"

let get_layout jk : Layout.Const.t option =
  match get jk with Const c -> Some c.layout | Var _ -> None

let get_modal_upper_bounds jk = jk.desc.modes_upper_bounds

let get_externality_upper_bound jk = jk.desc.externality_upper_bound

let set_externality_upper_bound jk externality_upper_bound =
  { jk with desc = { jk.desc with externality_upper_bound } }

(*********************************)
(* pretty printing *)

let format ppf jkind = Jkind_desc.format ppf jkind.desc

let rec format_higher ppf (jkind : higher) =
  let open Format in
  match jkind.hdesc with
  | Type ty -> format ppf ty
  | Arrow ([({ hdesc = Type _ | Top; hhistory = _ } as arg)], result) ->
    fprintf ppf "%a => %a" format_higher arg format_higher result
  | Arrow (args, result) ->
    fprintf ppf "(%a) => %a"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") format_higher)
      args format_higher result
  | Top -> fprintf ppf "top"

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

  let format_concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
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

  let format_top_creation_reason ppf : History.top_creation_reason -> unit =
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
    | Union_incompatible ->
      format_with_notify_js ppf "it's a union of two incompatible kinds"

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

  let format_value_or_null_creation_reason ppf :
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
    | Let_rec_variable v ->
      fprintf ppf "it's the type of the recursive variable %s" (Ident.name v)
    | Unknown s ->
      fprintf ppf
        "unknown @[(please alert the Jane Street@;\
         compilers team with this message: %s)@]" s

  let format_creation_reason ppf ~layout_or_kind :
      History.creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Top_creation top -> format_top_creation_reason ppf top
    | Any_creation any -> format_any_creation_reason ppf any
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Void_creation _ -> .
    | Value_or_null_creation value ->
      format_value_or_null_creation_reason ppf value
    | Value_creation value ->
      format_value_creation_reason ppf ~layout_or_kind value
    | Concrete_creation concrete -> format_concrete_creation_reason ppf concrete
    | Concrete_legacy_creation concrete ->
      format_concrete_legacy_creation_reason ppf concrete
    | Primitive id -> fprintf ppf "it is the primitive type %s" (Ident.name id)
    | Imported ->
      fprintf ppf "of %s requirements from an imported definition"
        layout_or_kind
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has this %s"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Inferred_from_application ->
      fprintf ppf "its jkind was inferred from a type application"
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
    fprintf ppf "@[<v 2>%t" intro;
    (match t.hhistory with
    | Creation reason -> (
      fprintf ppf "@ because %a" (format_creation_reason ~layout_or_kind) reason;
      let is_const =
        match t.hdesc with
        | Type ty -> ( match get ty with Const _ -> true | Var _ -> false)
        | Arrow _ | Top -> false
      in
      match reason, is_const with
      | Concrete_legacy_creation _, true ->
        fprintf ppf ",@ defaulted to %s %a" layout_or_kind format_higher t
      | _ -> ())
    | _ -> assert false);
    fprintf ppf ".";
    (match t.hhistory with
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
      in_order t.hhistory

  let format_history ~intro ~layout_or_kind ppf t =
    if display_histories
    then
      if flattened_histories
      then format_flattened_history ~intro ~layout_or_kind ppf t
      else format_history_tree ~intro ~layout_or_kind ppf t
end

let format_history_higher ~intro ppf t =
  Format_history.format_history ~intro ~layout_or_kind:"kind" ppf t

let format_history ~intro ppf t =
  format_history_higher ~intro ppf (wrap_higher_jkind t)

(******************************)
(* errors *)

module Violation = struct
  open Format

  type violation =
    | Not_a_subjkind of higher * higher
    | No_intersection of higher * higher

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
    | Mode_or_kind
    | Layout

  let report_general preamble pp_former former ppf t =
    let mismatch_type =
      match t.violation with
      | Not_a_subjkind ({ hdesc = Type k1; _ }, { hdesc = Type k2; _ }) ->
        if Misc.Le_result.is_le (Layout.sub k1.desc.layout k2.desc.layout)
        then Mode_or_kind
        else Layout
      | Not_a_subjkind (_, _) -> Mode_or_kind
      | No_intersection ({ hdesc = Type _; _ }, { hdesc = Type _; _ }) -> Layout
      | No_intersection (_, _) -> Mode_or_kind
    in
    let layout_or_kind =
      match mismatch_type with Mode_or_kind -> "kind" | Layout -> "layout"
    in
    let format_layout_or_kind =
      match mismatch_type with
      | Mode_or_kind ->
        fun ppf jkind -> Format.fprintf ppf "@,%a" format_higher jkind
      | Layout -> (
        fun ppf jkind ->
          match jkind.hdesc with
          | Type ty -> Layout.format ppf ty.desc.layout
          | Arrow _ | Top -> assert false)
    in
    let distinguish_for_format k =
      match k.hdesc with
      | Type ty -> ( match get ty with Var _ -> `Var | Const _ -> `Const)
      | Arrow _ | Top -> `Higher
    in
    let subjkind_format verb k2 =
      match distinguish_for_format k2 with
      | `Var -> dprintf "%s representable" verb
      | `Const ->
        dprintf "%s a sub%s of %a" verb layout_or_kind format_layout_or_kind k2
      | `Higher -> dprintf "%s a subkind of %a" verb format_layout_or_kind k2
    in
    let k1, k2, fmt_k1, fmt_k2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (k1, k2); missing_cmi } -> (
        let missing_cmi =
          match missing_cmi with
          | None -> (
            match k1.hhistory with
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
        match t.violation, distinguish_for_format k2 with
        | Not_a_subjkind _, (`Const | `Higher) ->
          dprintf "be a sub%s of %a" layout_or_kind format_layout_or_kind k2
        | No_intersection _, (`Const | `Higher) ->
          dprintf "overlap with %a" format_layout_or_kind k2
        | _, `Var -> dprintf "be representable"
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
    { desc = desc1; history = _; has_warned = _ }
    { desc = desc2; history = _; has_warned = _ } =
  Jkind_desc.equate_or_equal ~allow_mutation desc1 desc2

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
  | Creation (Concrete_creation _ | Concrete_legacy_creation _) -> -1
  | _ -> 0

let combine_histories_higher reason lhs rhs =
  if flattened_histories
  then
    let cmp : Misc.Le_result.t =
      match lhs.hdesc, rhs.hdesc with
      | Type ty, Type ty' ->
        Desc.sub (Jkind_desc.get ty.desc) (Jkind_desc.get ty'.desc)
      | _, _ -> Equal
    in
    match cmp with
    | Less -> lhs.hhistory
    | Not_le ->
      rhs.hhistory
      (* CR layouts: this will be wrong if we ever have a non-trivial meet in the layout lattice *)
    | Equal ->
      if score_reason lhs.hhistory >= score_reason rhs.hhistory
      then lhs.hhistory
      else rhs.hhistory
  else
    Interact
      { reason;
        lhs_jkind = lhs.hdesc;
        lhs_history = lhs.hhistory;
        rhs_jkind = rhs.hdesc;
        rhs_history = rhs.hhistory
      }

let combine_histories reason lhs rhs =
  combine_histories_higher reason (wrap_higher_jkind lhs)
    (wrap_higher_jkind rhs)

let has_intersection t1 t2 =
  Option.is_some (Jkind_desc.intersection t1.desc t2.desc)

let intersection_or_error ~reason t1 t2 =
  match Jkind_desc.intersection t1.desc t2.desc with
  | None ->
    Error
      (Violation.of_
         (No_intersection (wrap_higher_jkind t1, wrap_higher_jkind t2)))
  | Some desc ->
    Ok
      { desc;
        history = combine_histories reason t1 t2;
        has_warned = t1.has_warned || t2.has_warned
      }

let union ~reason t1 t2 =
  { desc = Jkind_desc.union t1.desc t2.desc;
    history = combine_histories reason t1 t2;
    has_warned = t1.has_warned || t2.has_warned
  }

(* this is hammered on; it must be fast! *)
let check_sub sub super = Jkind_desc.sub sub.desc super.desc

let sub sub super = Misc.Le_result.is_le (check_sub sub super)

let sub_or_error t1 t2 =
  if sub t1 t2
  then Ok ()
  else
    Error
      (Violation.of_
         (Not_a_subjkind (wrap_higher_jkind t1, wrap_higher_jkind t2)))

let sub_with_history sub super =
  match check_sub sub super with
  | Less | Equal ->
    Ok { sub with history = combine_histories Subjkind sub super }
  | Not_le ->
    Error
      (Violation.of_
         (Not_a_subjkind (wrap_higher_jkind sub, wrap_higher_jkind super)))

let is_void_defaulting = function
  | { desc = { layout = Sort s; _ }; _ } -> Sort.is_void_defaulting s
  | _ -> false

(* This doesn't do any mutation because mutating a sort variable can't make it
   any, and modal upper bounds are constant. *)
let is_max jkind = sub Builtin.any_dummy_jkind jkind

let has_layout_any jkind =
  match jkind.desc.layout with Any -> true | _ -> false

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

  let concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
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

  let top_creation_reason ppf : History.top_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Union_incompatible -> fprintf ppf "Union_incompatible"

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
    | Let_rec_variable v -> fprintf ppf "Let_rec_variable %a" Ident.print v
    | Unknown s -> fprintf ppf "Unknown %s" s

  let creation_reason ppf : History.creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Top_creation top -> fprintf ppf "Top_creation %a" top_creation_reason top
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Value_or_null_creation value ->
      fprintf ppf "Value_or_null_creation %a" value_or_null_creation_reason
        value
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_creation_reason concrete
    | Concrete_legacy_creation concrete ->
      fprintf ppf "Concrete_legacy_creation %a" concrete_legacy_creation_reason
        concrete
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.name id)
    | Inferred_from_application -> fprintf ppf "Inferred_from_application"
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
        interact_reason reason higher_desc lhs_jkind history lhs_history
        higher_desc rhs_jkind history rhs_history
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  and t ppf ({ desc; history = h; has_warned = _ } : t) : unit =
    fprintf ppf "@[<v 2>{ desc = %a@,; history = %a }@]"
      Jkind_desc.Debug_printers.t desc history h

  and higher_desc ppf = function
    | Type ty -> fprintf ppf "Type %a" t ty
    | Arrow (args, result) ->
      fprintf ppf "Arrow ([%a],@ %a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") higher)
        args higher result
    | Top -> fprintf ppf "Top"

  and higher ppf ({ hdesc; hhistory = h } : higher) : unit =
    fprintf ppf "@[<v 2>{ hdesc = %a@,; history = %a }@]" higher_desc hdesc
      history h

  module Const = struct
    let rec t ppf (jkind : Const.t) =
      fprintf ppf
        "@[{ layout = <v 2>%a@,\
         ; modes_upper_bounds = <v 2>%a@,\
         ; externality_upper_bound = <v 2>%a@,\
         ; nullability_upper_bound = <v 2>%a@,\
         }@]"
        Layout.Const.Debug_printers.t jkind.layout Modes.print
        jkind.modes_upper_bounds Externality.print jkind.externality_upper_bound
        Nullability.print jkind.nullability_upper_bound

    and higher ppf : higher_const -> unit = function
      | Type ty -> fprintf ppf "Type %a" t ty
      | Arrow (args, result) ->
        fprintf ppf "Arrow ([%a],@ %a)"
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") higher)
          args higher result
      | Top -> fprintf ppf "Top"
  end
end

(*** formatting user errors ***)
let report_error ~loc : Error.t -> _ = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]" Pprintast.jkind jkind
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%a) and a layout attribute \
       ([@@@@%a]).@]"
      Const.format_higher_no_hiding from_annotation Const.format_higher
      from_attribute
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
        Const.format_higher_no_hiding jkind hint)
  | Unexpected_higher_jkind jkind ->
    Location.errorf ~loc "@[<v>Unexpected higher jkind (%a) in this position@]"
      Pprintast.jkind jkind

let () =
  Location.register_error_of_exn (function
    | Error.User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(* CR layouts v2.8: Remove the definitions below by propagating changes
   outside of this file. *)
type annotation = Const.higher * Jane_syntax.Jkind.annotation

let default_to_value_and_get t = default_to_value_and_get t
