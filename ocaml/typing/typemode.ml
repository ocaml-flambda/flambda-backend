open Location
open Mode

let transl_mode_annots modes =
  Typemodifier.transl_mode_annots ~required_mode_maturity:Stable modes

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

let untransl_modality (a : Modality.t) : Parsetree.modality loc =
  let s =
    match a with
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Global) -> "global"
    | Atom (Comonadic Areality, Meet_with Regionality.Const.Local) -> "local"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Many) -> "many"
    | Atom (Comonadic Linearity, Meet_with Linearity.Const.Once) -> "once"
    | Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Shared) -> "shared"
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

let locality_to_regionality : Mode.Locality.Const.t -> Mode.Regionality.Const.t
    = function
  | Local -> Local
  | Global -> Global

let transl_modalities ~maturity mut attrs modalities =
  let mut_modalities = mutable_implied_modalities mut attrs in
  let modalities_as_modes =
    Typemodifier.transl_modality_annots ~required_mode_maturity:maturity
      modalities
  in
  let modalities_for_monadic_axis axis =
    List.map (fun mode -> Modality.Atom (Monadic axis, Join_with mode))
  in
  let modalities_for_comonadic_axis axis =
    List.map (fun mode -> Modality.Atom (Comonadic axis, Meet_with mode))
  in
  let modalities =
    modalities_for_comonadic_axis Areality
      (List.map locality_to_regionality modalities_as_modes.areality)
    @ modalities_for_comonadic_axis Linearity modalities_as_modes.linearity
    @ modalities_for_monadic_axis Uniqueness modalities_as_modes.uniqueness
    @ modalities_for_comonadic_axis Portability modalities_as_modes.portability
    @ modalities_for_monadic_axis Contention modalities_as_modes.contention
  in
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
