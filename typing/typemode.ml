open Location
open Mode
open Jkind_axis

(* CR zqian: kind modifier can be either a modaity or externality/nullability.
   I.e., mode-like modifiers are just modalities and should be represented as
   such. Therefore, [transl_modalities] (not dealing with
   externality/nullability) will stay in this file, while [transl_modifiers]
   should go into [typekind.ml] and calls [transl_modalities]. *)

type modal = private |

type maybe_nonmodal = private |

type 'm annot_type =
  | Modifier : maybe_nonmodal annot_type
  | Mode : modal annot_type
  | Modality : modal annot_type

type error =
  | Duplicated_axis : _ Axis.t -> error
  | Unrecognized_modifier : _ annot_type * string -> error

exception Error of Location.t * error

module Axis_pair = struct
  type 'm t =
    | Modal_axis_pair : 'a Axis.Modal.t * 'a -> modal t
    | Any_axis_pair : 'a Axis.t * 'a -> maybe_nonmodal t

  let of_string s =
    let open Mode in
    match s with
    | "local" -> Any_axis_pair (Modal Locality, Locality.Const.Local)
    | "global" -> Any_axis_pair (Modal Locality, Locality.Const.Global)
    | "unique" -> Any_axis_pair (Modal Uniqueness, Uniqueness.Const.Unique)
    | "aliased" -> Any_axis_pair (Modal Uniqueness, Uniqueness.Const.Aliased)
    | "once" -> Any_axis_pair (Modal Linearity, Linearity.Const.Once)
    | "many" -> Any_axis_pair (Modal Linearity, Linearity.Const.Many)
    | "nonportable" ->
      Any_axis_pair (Modal Portability, Portability.Const.Nonportable)
    | "portable" -> Any_axis_pair (Modal Portability, Portability.Const.Portable)
    | "contended" -> Any_axis_pair (Modal Contention, Contention.Const.Contended)
    | "uncontended" ->
      Any_axis_pair (Modal Contention, Contention.Const.Uncontended)
    | "maybe_null" ->
      Any_axis_pair (Nonmodal Nullability, Nullability.Maybe_null)
    | "non_null" -> Any_axis_pair (Nonmodal Nullability, Nullability.Non_null)
    | "internal" -> Any_axis_pair (Nonmodal Externality, Externality.Internal)
    | "external64" ->
      Any_axis_pair (Nonmodal Externality, Externality.External64)
    | "external_" -> Any_axis_pair (Nonmodal Externality, Externality.External)
    | _ -> raise Not_found
end

let transl_annot (type m) ~(annot_type : m annot_type) ~required_mode_maturity
    annot : m Axis_pair.t =
  Option.iter
    (fun maturity ->
      Jane_syntax_parsing.assert_extension_enabled ~loc:annot.loc Mode maturity)
    required_mode_maturity;
  match Axis_pair.of_string annot.txt, annot_type with
  | Any_axis_pair (Nonmodal _, _), (Mode | Modality) | (exception Not_found) ->
    raise (Error (annot.loc, Unrecognized_modifier (annot_type, annot.txt)))
  | Any_axis_pair (Modal axis, mode), Mode -> Modal_axis_pair (axis, mode)
  | Any_axis_pair (Modal axis, mode), Modality -> Modal_axis_pair (axis, mode)
  | pair, Modifier -> pair

let unpack_mode_annot { txt = Parsetree.Mode s; loc } = { txt = s; loc }

module Opt_axis_collection = Axis_collection (Option)

let transl_modifier_annots annots =
  let step modifiers_so_far annot =
    let (Any_axis_pair (type a) ((axis, mode) : a Axis.t * a)) =
      transl_annot ~annot_type:Modifier ~required_mode_maturity:None
      @@ unpack_mode_annot annot
    in
    let (module A : Axis_s with type t = a) = Axis.get axis in
    let is_top = A.le A.max mode in
    if is_top
    then
      (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
         results in 3 calls to transl_annots per user-written annotation. This results
         in the warning being reported 3 times. *)
      (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
      ();
    let is_dup =
      Option.is_some (Opt_axis_collection.get ~axis modifiers_so_far)
    in
    if is_dup then raise (Error (annot.loc, Duplicated_axis axis));
    Opt_axis_collection.set ~axis modifiers_so_far (Some mode)
  in
  let empty_modifiers =
    Opt_axis_collection.create { f = (fun ~axis:_ -> None) }
  in
  List.fold_left step empty_modifiers annots

let transl_mode_annots annots : Alloc.Const.Option.t =
  let step modifiers_so_far annot =
    let (Modal_axis_pair (type a) ((axis, mode) : a Axis.Modal.t * a)) =
      transl_annot ~annot_type:Mode ~required_mode_maturity:(Some Stable)
      @@ unpack_mode_annot annot
    in
    let axis = Axis.Modal axis in
    if Option.is_some (Opt_axis_collection.get ~axis modifiers_so_far)
    then raise (Error (annot.loc, Duplicated_axis axis));
    Opt_axis_collection.set ~axis modifiers_so_far (Some mode)
  in
  let empty_modifiers =
    Opt_axis_collection.create { f = (fun ~axis:_ -> None) }
  in
  let modes = List.fold_left step empty_modifiers annots in
  { areality = modes.locality;
    linearity = modes.linearity;
    uniqueness = modes.uniqueness;
    portability = modes.portability;
    contention = modes.contention
  }

let untransl_mode_annots ~loc (modes : Mode.Alloc.Const.Option.t) =
  let print_to_string_opt print a = Option.map (Format.asprintf "%a" print) a in
  let areality = print_to_string_opt Mode.Locality.Const.print modes.areality in
  let uniqueness =
    print_to_string_opt Mode.Uniqueness.Const.print modes.uniqueness
  in
  let linearity =
    print_to_string_opt Mode.Linearity.Const.print modes.linearity
  in
  let portability =
    print_to_string_opt Mode.Portability.Const.print modes.portability
  in
  let contention =
    print_to_string_opt Mode.Contention.Const.print modes.contention
  in
  List.filter_map
    (fun x -> Option.map (fun s -> { txt = Parsetree.Mode s; loc }) x)
    [areality; uniqueness; linearity; portability; contention]

let transl_modality ~maturity { txt = Parsetree.Modality modality; loc } =
  let axis_pair =
    transl_annot ~annot_type:Modality ~required_mode_maturity:(Some maturity)
      { txt = modality; loc }
  in
  match axis_pair with
  | Modal_axis_pair (Locality, mode) ->
    Modality.Atom
      (Comonadic Areality, Meet_with (Const.locality_as_regionality mode))
  | Modal_axis_pair (Linearity, mode) ->
    Modality.Atom (Comonadic Linearity, Meet_with mode)
  | Modal_axis_pair (Uniqueness, mode) ->
    Modality.Atom (Monadic Uniqueness, Join_with mode)
  | Modal_axis_pair (Portability, mode) ->
    Modality.Atom (Comonadic Portability, Meet_with mode)
  | Modal_axis_pair (Contention, mode) ->
    Modality.Atom (Monadic Contention, Join_with mode)

let untransl_modality (a : Modality.t) : Parsetree.modality loc =
  let s =
    match a with
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Global) -> "global"
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Local) -> "local"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Many) -> "many"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Once) -> "once"
    | Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Aliased) -> "aliased"
    | Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Unique) -> "unique"
    | Atom (Comonadic Portability, Meet_with Portability.Const.Portable) ->
      "portable"
    | Atom (Comonadic Portability, Meet_with Portability.Const.Nonportable) ->
      "nonportable"
    | Atom (Monadic Contention, Join_with Contention.Const.Contended) ->
      "contended"
    | Atom (Monadic Contention, Join_with Contention.Const.Uncontended) ->
      "uncontended"
    | _ -> failwith "BUG: impossible modality atom"
  in
  { txt = Modality s; loc = Location.none }

(* For now, mutable implies legacy modalities for both comonadic axes and
   monadic axes. In the future, implications on the comonadic axes will be
   removed (and can be experimented currently with using
   @no_mutable_implied_modalities). The implications on the monadic axes will
   stay. *)
(* CR zqian: decouple mutable and comonadic modalities *)
let mutable_implied_modalities (mut : Types.mutability) attrs =
  let comonadic : Modality.t list =
    [ Atom (Comonadic Areality, Meet_with Regionality.Const.legacy);
      Atom (Comonadic Linearity, Meet_with Linearity.Const.legacy);
      Atom (Comonadic Portability, Meet_with Portability.Const.legacy) ]
  in
  let monadic : Modality.t list =
    [ Atom (Monadic Uniqueness, Join_with Uniqueness.Const.legacy);
      Atom (Monadic Contention, Join_with Contention.Const.legacy) ]
  in
  match mut with
  | Immutable -> []
  | Mutable _ ->
    if Builtin_attributes.has_no_mutable_implied_modalities attrs
    then monadic
    else monadic @ comonadic

let transl_modalities ~maturity mut attrs modalities =
  let mut_modalities = mutable_implied_modalities mut attrs in
  let modalities = List.map (transl_modality ~maturity) modalities in
  (* mut_modalities is applied before explicit modalities *)
  Modality.Value.Const.id
  |> List.fold_right
       (fun atom m -> Modality.Value.Const.compose ~then_:atom m)
       mut_modalities
  (* For explicit modalities:
     type r = { x : string @@ foo bar hello }
     is interpreted as
     x = foo (bar (hello (r))) *)
  |> List.fold_right
       (fun atom m -> Modality.Value.Const.compose ~then_:atom m)
       modalities

let untransl_modalities mut attrs t =
  let l = Modality.Value.Const.to_list t in
  let l = List.filter (fun a -> not @@ Modality.is_id a) l in
  let mut_modalities = mutable_implied_modalities mut attrs in
  (* polymorphic equality suffices for now. *)
  let l = List.filter (fun x -> not @@ List.mem x mut_modalities) l in
  List.map untransl_modality l

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis axis ->
    fprintf ppf "The %s axis has already been specified." (Axis.name axis)
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with
      | Modifier -> "modifier"
      | Mode -> "mode"
      | Modality -> "modality"
    in
    fprintf ppf "Unrecognized %s %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
