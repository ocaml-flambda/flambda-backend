open Location
open Mode
open Jane_syntax

type error =
  | Duplicated_mode of Axis.t
  | Unrecognized_mode of string
  | Unrecognized_modality of string

exception Error of Location.t * error

let transl_mode_annots modes =
  let rec loop (acc : Alloc.Const.Option.t) = function
    | [] -> acc
    | m :: rest ->
      let { txt; loc } = (m : Mode_expr.Const.t :> _ Location.loc) in
      Jane_syntax_parsing.assert_extension_enabled ~loc Mode ();
      let acc : Alloc.Const.Option.t =
        match txt with
        (* CR zqian: We should interpret other mode names (global, shared, once)
           as well. We can't do that yet because of the CR below. *)
        | "local" -> (
          match acc.locality with
          | None -> { acc with locality = Some Local }
          | Some _ -> raise (Error (loc, Duplicated_mode `Locality)))
        | "unique" -> (
          match acc.uniqueness with
          | None -> { acc with uniqueness = Some Unique }
          | Some _ -> raise (Error (loc, Duplicated_mode `Uniqueness)))
        | "once" -> (
          match acc.linearity with
          | None -> { acc with linearity = Some Once }
          | Some _ -> raise (Error (loc, Duplicated_mode `Linearity)))
        | "global" ->
          (* CR zqian: global modality might leak to here by ppxes.
             This is a dirty fix that needs to be fixed ASAP. *)
          acc
        | s -> raise (Error (loc, Unrecognized_mode s))
      in
      loop acc rest
  in
  loop Alloc.Const.Option.none modes.txt

let transl_modalities modalities =
  let transl_modality m : _ Modality.Vector.axis * _ Modality.t * _ =
    let { txt; loc } = (m : Mode_expr.Const.t :> _ Location.loc) in
    Jane_syntax_parsing.assert_extension_enabled ~loc Mode ();
    match txt with
    | "global" -> Regionality, Global, loc
    | s -> raise (Error (loc, Unrecognized_modality s))
  in
  let apply_modality ax atom loc acc =
    let acc, red = Modality.Vector.cons ax atom acc in
    (match red with
    | Not_reducible -> ()
    | Reducible { outer; inner; reduced } ->
      Location.prerr_warning loc
        (Warnings.Reduced_modality
           ( Modality.to_string outer,
             Modality.to_string inner,
             Modality.to_string reduced )));
    acc
  in
  (* Note the ordering: When user write:
     type r = { x : string @@ foo bar hello}
     The mode [m'] of the field will be
     [m' = foo (bar (hello (m)))]
     where [m] is the mode of the record. I think this is more intuitive, because
     [foo] is closest to the field type [string] in the syntax, so it should be
     closest to the field mode [m'] in the semantics. *)
  List.fold_right
    (fun m acc ->
      let ax, atom, loc = transl_modality m in
      apply_modality ax atom loc acc)
    modalities.txt Modality.Vector.id

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

open Format

let report_error ppf = function
  | Duplicated_mode ax ->
    fprintf ppf "The %s axis has already been specified." (Axis.to_string ax)
  | Unrecognized_mode s -> fprintf ppf "Unrecognized mode name %s." s
  | Unrecognized_modality s -> fprintf ppf "Unrecognized modality %s." s

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
