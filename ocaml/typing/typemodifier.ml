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
type modifiers =
  { modal_upper_bounds : Mode.Alloc.Const.Option.t;
    externality_upper_bound : Jkind_types.Externality.t option;
    nullability_upper_bound : Jkind_types.Nullability.t option
  }

type annot_type =
  | Modifier
  | Mode

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
let check_annot (type a) ~(annot_type : annot_type) ~(axis : a axis) ~old
    ~(new_ : a Location.loc) =
  let is_top =
    match axis, new_ with
    | Modal Areality, _ -> Mode.Locality.Const.(le max new_.txt)
    | Modal Linearity, _ -> Mode.Linearity.Const.(le max new_.txt)
    | Modal Uniqueness, _ -> Mode.Uniqueness.Const.(le max new_.txt)
    | Modal Portability, _ -> Mode.Portability.Const.(le max new_.txt)
    | Modal Contention, _ -> Mode.Contention.Const.(le max new_.txt)
    | Nonmodal Externality, _ -> Jkind_types.Externality.(le max new_.txt)
    | Nonmodal Nullability, _ -> Jkind_types.Nullability.(le max new_.txt)
  in
  (match annot_type with
  | Modifier ->
    if is_top
    then
      (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
         results in 3 calls to transl_annots per user-written annotation. This results
         in the warning being reported 3 times. *)
      (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
      ()
  | Mode -> ());
  match old with
  | None -> ()
  | Some _ -> raise (Error (new_.loc, Duplicated_axis axis))

let set_axis (type a) ~annot_type ~(axis : a axis) (acc : modifiers)
    (modifier : a Location.loc) : modifiers =
  let old : a option =
    match axis with
    | Modal Areality -> acc.modal_upper_bounds.areality
    | Modal Uniqueness -> acc.modal_upper_bounds.uniqueness
    | Modal Linearity -> acc.modal_upper_bounds.linearity
    | Modal Portability -> acc.modal_upper_bounds.portability
    | Modal Contention -> acc.modal_upper_bounds.contention
    | Nonmodal Externality -> acc.externality_upper_bound
    | Nonmodal Nullability -> acc.nullability_upper_bound
  in
  check_annot ~annot_type ~axis ~old ~new_:modifier;
  match axis with
  | Modal axis ->
    let modal_upper_bounds : Mode.Alloc.Const.Option.t =
      match axis with
      | Areality -> { acc.modal_upper_bounds with areality = Some modifier.txt }
      | Uniqueness ->
        { acc.modal_upper_bounds with uniqueness = Some modifier.txt }
      | Linearity ->
        { acc.modal_upper_bounds with linearity = Some modifier.txt }
      | Portability ->
        { acc.modal_upper_bounds with portability = Some modifier.txt }
      | Contention ->
        { acc.modal_upper_bounds with contention = Some modifier.txt }
    in
    { acc with modal_upper_bounds }
  | Nonmodal Externality ->
    { acc with externality_upper_bound = Some modifier.txt }
  | Nonmodal Nullability ->
    { acc with nullability_upper_bound = Some modifier.txt }

let transl_annots ~annot_type ~required_mode_maturity annots =
  let transl_annot modifiers_so_far annot =
    let ({ txt = Mode annot_txt; loc } : Parsetree.mode Location.loc) = annot in
    Option.iter
      (fun maturity ->
        Jane_syntax_parsing.assert_extension_enabled ~loc Mode maturity)
      required_mode_maturity;
    let modifiers =
      match Str_map.find_opt annot_txt modifiers, annot_type with
      | Some (Axis_pair (Nonmodal _, _)), Mode | None, _ ->
        raise (Error (loc, Unrecognized_modifier (annot_type, annot_txt)))
      | Some (Axis_pair (axis, mode)), _ ->
        set_axis ~annot_type ~axis modifiers_so_far { txt = mode; loc }
    in
    modifiers
  in
  let empty_modifiers =
    { modal_upper_bounds = Mode.Alloc.Const.Option.none;
      externality_upper_bound = None;
      nullability_upper_bound = None
    }
  in
  List.fold_left transl_annot empty_modifiers annots

let transl_mode_annots ?required_mode_maturity annots =
  let modifiers =
    transl_annots ~annot_type:Mode ~required_mode_maturity annots
  in
  let assert_empty axis bound =
    if Option.is_some bound
    then
      let error_message =
        Format.asprintf
          "Expected empty nonmodal modifiers when translating modes, but got \
           one along %s axis"
          (axis_name (Nonmodal axis))
      in
      Misc.fatal_error error_message
  in
  assert_empty Externality modifiers.externality_upper_bound;
  assert_empty Nullability modifiers.nullability_upper_bound;
  modifiers.modal_upper_bounds

let transl_modifier_annots annots =
  transl_annots ~annot_type:Modifier ~required_mode_maturity:None annots

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis axis ->
    fprintf ppf "The %s axis has already been specified." (axis_name axis)
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with Modifier -> "modifier" | Mode -> "mode"
    in
    fprintf ppf "Unrecognized %s name %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
