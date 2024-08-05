open Location
open Mode

type error =
  | Unrecognized_modality of string

exception Error of Location.t * error

let transl_mode_annots modes = Typemodifier.transl_mode_annots modes

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

let transl_modality ~maturity m : Modality.t =
  let { txt; loc } = m in
  let (Parsetree.Modality s) = txt in
  Jane_syntax_parsing.assert_extension_enabled ~loc Mode maturity;
  match s with
  | "global" -> Atom (Comonadic Areality, Meet_with Regionality.Const.Global)
  | "local" -> Atom (Comonadic Areality, Meet_with Regionality.Const.Local)
  | "many" -> Atom (Comonadic Linearity, Meet_with Linearity.Const.Many)
  | "once" -> Atom (Comonadic Linearity, Meet_with Linearity.Const.Once)
  | "shared" -> Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Shared)
  | "unique" -> Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Unique)
  | "portable" ->
    Atom (Comonadic Portability, Meet_with Portability.Const.Portable)
  | "nonportable" ->
    Atom (Comonadic Portability, Meet_with Portability.Const.Nonportable)
  | "contended" ->
    Atom (Monadic Contention, Join_with Contention.Const.Contended)
  | "uncontended" ->
    Atom (Monadic Contention, Join_with Contention.Const.Uncontended)
  | s -> raise (Error (loc, Unrecognized_modality s))

let untransl_modalities ~loc m : Parsetree.modalities =
  let untransl_atom (a : Modality.t) =
    let s =
      match a with
      | Atom (Comonadic Areality, Meet_with Regionality.Const.Global) ->
        "global"
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
    { txt = Parsetree.Modality s; loc }
  in
  let is_not_id m = not (Modality.is_id m) in
  Modality.Value.Const.to_list m
  |> List.filter is_not_id |> List.map untransl_atom

let compose_modalities modalities =
  (* The ordering:
     type r = { x : string @@ foo bar hello }
     is interpreted as
     x = foo (bar (hello (r))) *)
  List.fold_right
    (fun atom m -> Modality.Value.Const.compose ~then_:atom m)
    modalities Modality.Value.Const.id

let mutable_implied_modalities : Modality.t list =
  [ Atom (Comonadic Areality, Meet_with Regionality.Const.Global);
    Atom (Comonadic Linearity, Meet_with Linearity.Const.Many);
    Atom (Monadic Uniqueness, Join_with Uniqueness.Const.Shared) ]

let is_mutable_implied_modality m =
  (* polymorphic equality suffices for now. *)
  List.mem m mutable_implied_modalities

let transl_modalities ~maturity ~has_mutable_implied_modalities modalities =
  let modalities = List.map (transl_modality ~maturity) modalities in
  let modalities =
    if has_mutable_implied_modalities
    then modalities @ mutable_implied_modalities
    else modalities
  in
  compose_modalities modalities

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

open Format

let report_error ppf = function
  | Unrecognized_modality s -> fprintf ppf "Unrecognized modality %s." s

let mutable_implied_modalities = compose_modalities mutable_implied_modalities

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
