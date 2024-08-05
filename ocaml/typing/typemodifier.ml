type modifiers =
  { modal_upper_bounds : Mode.Alloc.Const.Option.t;
    externality_upper_bound : Jkind_types.Externality.t option;
    nullability_upper_bound : Jkind_types.Nullability.t option
  }

type _ annot_type =
  | Modifier : modifiers annot_type
  | Mode : Mode.Alloc.Const.Option.t annot_type

type _ modal_axis =
  | Areality : Mode.Locality.Const.t modal_axis
  | Uniqueness : Mode.Uniqueness.Const.t modal_axis
  | Linearity : Mode.Linearity.Const.t modal_axis
  | Portability : Mode.Portability.Const.t modal_axis
  | Contention : Mode.Contention.Const.t modal_axis

type _ nonmodal_axis =
  | Externality : Jkind_types.Externality.t nonmodal_axis
  | Nullability : Jkind_types.Nullability.t nonmodal_axis

type (_, _) axis =
  | Modal : 'a modal_axis -> ('a, Mode.Alloc.Const.Option.t) axis
  | Nonmodal : 'a nonmodal_axis -> ('a, modifiers) axis

type error =
  | Duplicated_axis : (_, _) axis -> error
  | Unrecognized_modifier : 'a annot_type * string -> error

exception Error of Location.t * error

type axis_pair = Axis_pair : ('a, 'b) axis * 'a -> axis_pair

module Str_map = Map.Make (String)

let modifiers =
  let alist =
    [ "local", Axis_pair (Modal Areality, Mode.Locality.Const.Local);
      "global", Axis_pair (Modal Areality, Mode.Locality.Const.Global);
      "unique", Axis_pair (Modal Uniqueness, Mode.Uniqueness.Const.Unique);
      "shared", Axis_pair (Modal Uniqueness, Mode.Uniqueness.Const.Shared);
      "once", Axis_pair (Modal Linearity, Mode.Linearity.Const.Once);
      "many", Axis_pair (Modal Linearity, Mode.Linearity.Const.Many);
      ( "nonportable",
        Axis_pair (Modal Portability, Mode.Portability.Const.Nonportable) );
      "portable", Axis_pair (Modal Portability, Mode.Portability.Const.Portable);
      "contended", Axis_pair (Modal Contention, Mode.Contention.Const.Contended);
      ( "uncontended",
        Axis_pair (Modal Contention, Mode.Contention.Const.Uncontended) );
      ( "maybe_null",
        Axis_pair (Nonmodal Nullability, Jkind_types.Nullability.Maybe_null) );
      ( "non_null",
        Axis_pair (Nonmodal Nullability, Jkind_types.Nullability.Non_null) );
      ( "internal",
        Axis_pair (Nonmodal Externality, Jkind_types.Externality.Internal) );
      ( "external64",
        Axis_pair (Nonmodal Externality, Jkind_types.Externality.External64) );
      ( "external_",
        Axis_pair (Nonmodal Externality, Jkind_types.Externality.External) ) ]
  in
  let rec loop acc = function
    | [] -> acc
    | (name, axis_pair) :: tl -> loop (Str_map.add name axis_pair acc) tl
  in
  loop Str_map.empty alist

(* Raise an error if the user duplicated annotations along an axis, and a warning
   if they used a top. The warning is only output when annot_type is Modifier *)
let check_annot (type a b c) ~(annot_type : a annot_type) ~(axis : (b, c) axis)
    ~old ~(new' : b) ~(new_raw : _ Location.loc) =
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

let set_modal_axis (type a) ~annot_type ~(axis : a modal_axis)
    (acc : Mode.Alloc.Const.Option.t) (modifier : a) raw :
    Mode.Alloc.Const.Option.t =
  let old : a option =
    match axis with
    | Areality -> acc.areality
    | Uniqueness -> acc.uniqueness
    | Linearity -> acc.linearity
    | Portability -> acc.portability
    | Contention -> acc.contention
  in
  check_annot ~annot_type ~axis:(Modal axis) ~old ~new':modifier ~new_raw:raw;
  match axis with
  | Areality -> { acc with areality = Some modifier }
  | Uniqueness -> { acc with uniqueness = Some modifier }
  | Linearity -> { acc with linearity = Some modifier }
  | Portability -> { acc with portability = Some modifier }
  | Contention -> { acc with contention = Some modifier }

let set_nonmodal_axis (type a) ~annot_type ~(axis : a nonmodal_axis)
    (acc : modifiers) (modifier : a) raw : modifiers =
  let old : a option =
    match axis with
    | Externality -> acc.externality_upper_bound
    | Nullability -> acc.nullability_upper_bound
  in
  check_annot ~annot_type ~axis:(Nonmodal axis) ~old ~new':modifier ~new_raw:raw;
  match axis with
  | Externality -> { acc with externality_upper_bound = Some modifier }
  | Nullability -> { acc with nullability_upper_bound = Some modifier }

let transl_annots (type a) (annot_type : a annot_type) annots
    : a =
  let rec loop (acc : a) = function
    | [] -> acc
    | annot :: tl ->
      let annot : Parsetree.mode Location.loc = annot in
      let Mode annot_txt = annot.txt in
      let modifiers : a =
        match Str_map.find_opt annot_txt modifiers, annot_type with
        | Some (Axis_pair (Modal axis, mode)), Mode ->
          set_modal_axis ~annot_type ~axis acc mode annot
        | Some (Axis_pair (Modal axis, mode)), Modifier ->
          { acc with
            modal_upper_bounds =
              set_modal_axis ~annot_type ~axis acc.modal_upper_bounds mode annot
          }
        | Some (Axis_pair (Nonmodal axis, modifier)), Modifier ->
          set_nonmodal_axis ~annot_type ~axis acc modifier annot
        | Some (Axis_pair (Nonmodal _, _)), Mode | None, _ ->
          raise
            (Error (annot.loc, Unrecognized_modifier (annot_type, annot_txt)))
      in
      loop modifiers tl
  in
  let empty_modifiers : a =
    match annot_type with
    | Mode -> Mode.Alloc.Const.Option.none
    | Modifier ->
      { modal_upper_bounds = Mode.Alloc.Const.Option.none;
        externality_upper_bound = None;
        nullability_upper_bound = None
      }
  in
  loop empty_modifiers annots

let transl_mode_annots = transl_annots Mode

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
