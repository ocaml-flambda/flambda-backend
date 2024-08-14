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
type alloc_list =
  { areality : Mode.Locality.Const.t list;
    uniqueness : Mode.Uniqueness.Const.t list;
    linearity : Mode.Linearity.Const.t list;
    portability : Mode.Portability.Const.t list;
    contention : Mode.Contention.Const.t list
  }

type alloc_loc_list =
  { areality_locs : Mode.Locality.Const.t Location.loc list;
    uniqueness_locs : Mode.Uniqueness.Const.t Location.loc list;
    linearity_locs : Mode.Linearity.Const.t Location.loc list;
    portability_locs : Mode.Portability.Const.t Location.loc list;
    contention_locs : Mode.Contention.Const.t Location.loc list
  }

type modifiers_loc_list =
  { modes_locs : alloc_loc_list;
    externality_locs : Jkind_types.Externality.t Location.loc list;
    nullability_locs : Jkind_types.Nullability.t Location.loc list
  }

type modifiers =
  { modes : Mode.Alloc.Const.Option.t;
    externality : Jkind_types.Externality.t option;
    nullability : Jkind_types.Nullability.t option
  }

type modelike_annot_type =
  | Mode
  | Modality

type annot_type =
  | Modifier
  | Mode_like of modelike_annot_type

type _ modal_axis =
  | Areality : Mode.Locality.Const.t modal_axis
  | Uniqueness : Mode.Uniqueness.Const.t modal_axis
  | Linearity : Mode.Linearity.Const.t modal_axis
  | Portability : Mode.Portability.Const.t modal_axis
  | Contention : Mode.Contention.Const.t modal_axis

type _ nonmodal_axis =
  | Externality : Jkind_types.Externality.t nonmodal_axis
  | Nullability : Jkind_types.Nullability.t nonmodal_axis

type _ axis =
  | Modal : 'a modal_axis -> 'a axis
  | Nonmodal : 'a nonmodal_axis -> 'a axis

type axis_pair = Axis_pair : 'a axis * 'a -> axis_pair

type error =
  | Duplicated_axis : _ axis -> error
  | Unrecognized_modifier : annot_type * string -> error

exception Error of Location.t * error

module Str_map = Map.Make (String)

let axis_name (type a) (axis : a axis) =
  match axis with
  | Modal Areality -> "locality"
  | Modal Linearity -> "linearity"
  | Modal Uniqueness -> "uniqueness"
  | Modal Portability -> "portability"
  | Modal Contention -> "contention"
  | Nonmodal Externality -> "externality"
  | Nonmodal Nullability -> "nullability"

let modifiers =
  let alist =
    let open Mode in
    let open Jkind_types in
    [ "local", Axis_pair (Modal Areality, Locality.Const.Local);
      "global", Axis_pair (Modal Areality, Locality.Const.Global);
      "unique", Axis_pair (Modal Uniqueness, Uniqueness.Const.Unique);
      "shared", Axis_pair (Modal Uniqueness, Uniqueness.Const.Shared);
      "once", Axis_pair (Modal Linearity, Linearity.Const.Once);
      "many", Axis_pair (Modal Linearity, Linearity.Const.Many);
      "nonportable", Axis_pair (Modal Portability, Portability.Const.Nonportable);
      "portable", Axis_pair (Modal Portability, Portability.Const.Portable);
      "contended", Axis_pair (Modal Contention, Contention.Const.Contended);
      "uncontended", Axis_pair (Modal Contention, Contention.Const.Uncontended);
      "maybe_null", Axis_pair (Nonmodal Nullability, Nullability.Maybe_null);
      "non_null", Axis_pair (Nonmodal Nullability, Nullability.Non_null);
      "internal", Axis_pair (Nonmodal Externality, Externality.Internal);
      "external64", Axis_pair (Nonmodal Externality, Externality.External64);
      "external_", Axis_pair (Nonmodal Externality, Externality.External) ]
  in
  List.fold_left
    (fun acc (name, axis_pair) -> Str_map.add name axis_pair acc)
    Str_map.empty alist

(* Raise an error if the user duplicated annotations along an axis, and a warning
   if they used a top. The warning is only output when annot_type is Modifier *)
let check_not_top (type a) ~(annot_type : annot_type) ~(axis : a axis)
    (annot : a Location.loc) =
  let is_top =
    match axis with
    | Modal Areality -> Mode.Locality.Const.(le max annot.txt)
    | Modal Linearity -> Mode.Linearity.Const.(le max annot.txt)
    | Modal Uniqueness -> Mode.Uniqueness.Const.(le max annot.txt)
    | Modal Portability -> Mode.Portability.Const.(le max annot.txt)
    | Modal Contention -> Mode.Contention.Const.(le max annot.txt)
    | Nonmodal Externality -> Jkind_types.Externality.(le max annot.txt)
    | Nonmodal Nullability -> Jkind_types.Nullability.(le max annot.txt)
  in
  match annot_type with
  | Modifier ->
    if is_top
    then
      (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
         results in 3 calls to transl_annots per user-written annotation. This results
         in the warning being reported 3 times. *)
      (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
      ()
  | Mode_like (Mode | Modality) -> ()

let add_to_axis (type a) ~annot_type ~(axis : a axis) acc
    (modifier : a Location.loc) =
  check_not_top ~annot_type ~axis modifier;
  match axis with
  | Modal axis ->
    let modes_locs =
      match axis with
      | Areality ->
        { acc.modes_locs with
          areality_locs = modifier :: acc.modes_locs.areality_locs
        }
      | Uniqueness ->
        { acc.modes_locs with
          uniqueness_locs = modifier :: acc.modes_locs.uniqueness_locs
        }
      | Linearity ->
        { acc.modes_locs with
          linearity_locs = modifier :: acc.modes_locs.linearity_locs
        }
      | Portability ->
        { acc.modes_locs with
          portability_locs = modifier :: acc.modes_locs.portability_locs
        }
      | Contention ->
        { acc.modes_locs with
          contention_locs = modifier :: acc.modes_locs.contention_locs
        }
    in
    { acc with modes_locs }
  | Nonmodal Externality ->
    { acc with externality_locs = modifier :: acc.externality_locs }
  | Nonmodal Nullability ->
    { acc with nullability_locs = modifier :: acc.nullability_locs }

let transl_annots ~annot_type ~required_mode_maturity annots =
  let transl_annot modifiers_so_far (annot : _ Location.loc) =
    Option.iter
      (fun maturity ->
        Jane_syntax_parsing.assert_extension_enabled ~loc:annot.loc Mode
          maturity)
      required_mode_maturity;
    let modifiers =
      match Str_map.find_opt annot.txt modifiers, annot_type with
      | Some (Axis_pair (Nonmodal _, _)), Mode_like (Mode | Modality) | None, _
        ->
        raise (Error (annot.loc, Unrecognized_modifier (annot_type, annot.txt)))
      | Some (Axis_pair (axis, mode)), _ ->
        add_to_axis ~annot_type ~axis modifiers_so_far
          { txt = mode; loc = annot.loc }
    in
    modifiers
  in
  let empty_modifiers =
    { modes_locs =
        { areality_locs = [];
          uniqueness_locs = [];
          linearity_locs = [];
          portability_locs = [];
          contention_locs = []
        };
      externality_locs = [];
      nullability_locs = []
    }
  in
  List.fold_left transl_annot empty_modifiers annots

let transl_modelike_annots ~annot_type ~required_mode_maturity annots =
  let modifiers =
    transl_annots ~annot_type:(Mode_like annot_type) ~required_mode_maturity
      annots
  in
  let assert_empty axis modifiers =
    match modifiers with
    | _ :: _ ->
      let error_message =
        Format.asprintf
          "Expected empty nonmodal modifiers when translating modes, but got \
           one along %s axis"
          (axis_name (Nonmodal axis))
      in
      Misc.fatal_error error_message
    | [] -> ()
  in
  assert_empty Externality modifiers.externality_locs;
  assert_empty Nullability modifiers.externality_locs;
  modifiers.modes_locs

let assert_no_duplicates ~axis (modifiers : _ Location.loc list) =
  (* The modifier list was built up backwards (compared to the order the user wrote) in
     transl_modelike_annots. In the case of a duplicate, we want to report the error on
     the second modifier along an axis that the user wrote, which corresponds to the
     second to last element of modifiers. So we reverse the list before matching on it. *)
  match List.rev modifiers with
  | [] -> None
  | [modifier] -> Some modifier.txt
  | _ :: dup :: _ -> raise (Error (dup.loc, Duplicated_axis axis))

let assert_no_duplicates_in_modes modes : Mode.Alloc.Const.Option.t =
  { areality = assert_no_duplicates ~axis:(Modal Areality) modes.areality_locs;
    uniqueness =
      assert_no_duplicates ~axis:(Modal Uniqueness) modes.uniqueness_locs;
    linearity =
      assert_no_duplicates ~axis:(Modal Linearity) modes.linearity_locs;
    portability =
      assert_no_duplicates ~axis:(Modal Portability) modes.portability_locs;
    contention =
      assert_no_duplicates ~axis:(Modal Contention) modes.contention_locs
  }

let assert_no_duplicates_in_modifiers modifiers =
  { modes = assert_no_duplicates_in_modes modifiers.modes_locs;
    externality =
      assert_no_duplicates ~axis:(Nonmodal Externality)
        modifiers.externality_locs;
    nullability =
      assert_no_duplicates ~axis:(Nonmodal Nullability)
        modifiers.nullability_locs
  }

let unpack_mode_annot : Parsetree.mode Location.loc -> string Location.loc =
 fun { txt = Mode s; loc } -> { txt = s; loc }

let unpack_modality_annot :
    Parsetree.modality Location.loc -> string Location.loc =
 fun { txt = Modality s; loc } -> { txt = s; loc }

let transl_modality_annots ?required_mode_maturity annots =
  let unpacked_annots = List.map unpack_modality_annot annots in
  let modalities = transl_modelike_annots ~annot_type:Modality ~required_mode_maturity
    unpacked_annots in
  let unpack_locs loc_list = List.map (fun ({txt; loc=_} : _ Location.loc) -> txt) loc_list in
  {
    areality = unpack_locs modalities.areality_locs;
    uniqueness = unpack_locs modalities.uniqueness_locs;
    linearity = unpack_locs modalities.linearity_locs;
    portability = unpack_locs modalities.portability_locs;
    contention = unpack_locs modalities.contention_locs;
  }

let transl_mode_annots ?required_mode_maturity annots =
  let unpacked_annots = List.map unpack_mode_annot annots in
  let modes =
    transl_modelike_annots ~annot_type:Mode ~required_mode_maturity
      unpacked_annots
  in
  assert_no_duplicates_in_modes modes

let transl_modifier_annots annots =
  let unpacked_annots = List.map unpack_mode_annot annots in
  let modifiers =
    transl_annots ~annot_type:Modifier ~required_mode_maturity:None
      unpacked_annots
  in
  assert_no_duplicates_in_modifiers modifiers

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis axis ->
    fprintf ppf "The %s axis has already been specified." (axis_name axis)
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with
      | Modifier -> "modifier"
      | Mode_like Mode -> "mode"
      | Mode_like Modality -> "modality"
    in
    fprintf ppf "Unrecognized %s name %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
