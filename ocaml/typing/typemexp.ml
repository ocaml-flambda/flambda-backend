open Location
open Mode

type error =
  | Unrecognized_mode
  | Unrecognized_modality
  | Duplicated_mode of Axis.t
  | Duplicated_modality of Axis.t

exception Error of Location.t * error

let transl_mode_annots modes =
  let rec loop (acc : Alloc.Const.Option.t) : _ -> Alloc.Const.Option.t =
    function
    | [] -> acc
    | { txt; loc } :: rest ->
      Jane_syntax_parsing.assert_extension_enabled ~loc Mode ();
      let acc =
        match txt with
        | "local" -> (
          match acc.locality with
          | None -> { acc with locality = Some Locality.Const.Local }
          | Some _ -> raise (Error (loc, Duplicated_mode `Locality)))
        | "unique" -> (
          match acc.uniqueness with
          | None -> { acc with uniqueness = Some Uniqueness.Const.Unique }
          | Some _ -> raise (Error (loc, Duplicated_mode `Uniqueness)))
        | "once" -> (
          match acc.linearity with
          | None -> { acc with linearity = Some Linearity.Const.Once }
          | Some _ -> raise (Error (loc, Duplicated_mode `Linearity)))
        | _ -> raise (Error (loc, Unrecognized_mode))
      in
      loop acc rest
  in
  loop Alloc.Const.Option.none modes.txt

let transl_global_flags gfs =
  let rec loop (acc : Global_flag.t) : _ -> Global_flag.t = function
    | [] -> acc
    | { txt; loc } :: rest ->
      let acc =
        match txt with
        | "global" -> (
          Jane_syntax_parsing.assert_extension_enabled ~loc Mode ();
          match acc with
          | Unrestricted -> Global_flag.Global
          | _ -> raise (Error (loc, Duplicated_modality `Locality)))
        | _ -> raise (Error (loc, Unrecognized_modality))
      in
      loop acc rest
  in
  loop Unrestricted gfs.txt

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

open Format

let report_error ppf = function
  | Unrecognized_mode -> fprintf ppf "Unrecognized mode name."
  | Unrecognized_modality -> fprintf ppf "Unrecognized modality name."
  | Duplicated_mode ax ->
    fprintf ppf "The %s axis has already been specified." (Axis.string_of ax)
  | Duplicated_modality ax ->
    fprintf ppf "Modality in the %s axis has already be used."
      (Axis.string_of ax)

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
