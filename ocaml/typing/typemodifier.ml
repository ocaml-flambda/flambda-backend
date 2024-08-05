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
  let rec loop acc = function
    | [] -> acc
    | (name, axis_pair) :: tl -> loop (Str_map.add name axis_pair acc) tl
  in
  loop Str_map.empty alist

(* Raise an error if the user duplicated annotations along an axis, and a warning
   if they used a top. The warning is only output when annot_type is Modifier *)
let check_annot (type a) ~(annot_type : annot_type) ~(axis : a axis) ~old
    ~(new' : a) ~(new_raw : _ Location.loc) =
  let is_top =
    match axis, new' with
    | Modal Areality, _ -> Mode.Locality.Const.(le max new')
    | Modal Linearity, _ -> Mode.Linearity.Const.(le max new')
    | Modal Uniqueness, _ -> Mode.Uniqueness.Const.(le max new')
    | Modal Portability, _ -> Mode.Portability.Const.(le max new')
    | Modal Contention, _ -> Mode.Contention.Const.(le max new')
    | Nonmodal Nullability, Maybe_null | Nonmodal Externality, Internal -> true
    | Nonmodal Nullability, Non_null
    | Nonmodal Externality, (External | External64) ->
      false
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
  | Some _ -> raise (Error (new_raw.loc, Duplicated_axis axis))

let set_axis (type a) ~annot_type ~(axis : a axis) (acc : modifiers)
    (modifier : a) raw : modifiers =
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
  check_annot ~annot_type ~axis ~old ~new':modifier ~new_raw:raw;
  match axis with
  | Modal axis ->
    let modal_upper_bounds : Mode.Alloc.Const.Option.t =
      match axis with
      | Areality -> { acc.modal_upper_bounds with areality = Some modifier }
      | Uniqueness -> { acc.modal_upper_bounds with uniqueness = Some modifier }
      | Linearity -> { acc.modal_upper_bounds with linearity = Some modifier }
      | Portability ->
        { acc.modal_upper_bounds with portability = Some modifier }
      | Contention -> { acc.modal_upper_bounds with contention = Some modifier }
    in
    { acc with modal_upper_bounds }
  | Nonmodal Externality -> { acc with externality_upper_bound = Some modifier }
  | Nonmodal Nullability -> { acc with nullability_upper_bound = Some modifier }

let transl_annots (annot_type : annot_type) annots =
  let rec loop acc = function
    | [] -> acc
    | annot :: tl ->
      let { txt = Mode annot_txt; loc } : Parsetree.mode Location.loc = annot in
      let modifiers =
        match Str_map.find_opt annot_txt modifiers, annot_type with
        | Some (Axis_pair (Nonmodal _, _)), Mode | None, _ ->
          raise
            (Error (loc, Unrecognized_modifier (annot_type, annot_txt)))
        | Some (Axis_pair (axis, mode)), _ ->
          set_axis ~annot_type ~axis acc mode annot
      in
      loop modifiers tl
  in
  let empty_modifiers =
    { modal_upper_bounds = Mode.Alloc.Const.Option.none;
      externality_upper_bound = None;
      nullability_upper_bound = None
    }
  in
  loop empty_modifiers annots

let transl_mode_annots annots = (transl_annots Mode annots).modal_upper_bounds

let transl_modifier_annots = transl_annots Modifier

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis axis ->
    let ax =
      match axis with
      | Modal Areality -> dprintf "locality"
      | Modal Linearity -> dprintf "linearity"
      | Modal Uniqueness -> dprintf "uniqueness"
      | Modal Portability -> dprintf "portability"
      | Modal Contention -> dprintf "contention"
      | Nonmodal Externality -> dprintf "externality"
      | Nonmodal Nullability -> dprintf "nullability"
    in
    fprintf ppf "The %t axis has already been specified." ax
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with Modifier -> "modifier" | Mode -> "mode"
    in
    fprintf ppf "Unrecognized %s name %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
