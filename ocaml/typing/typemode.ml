open Location
open Mode
open Jane_syntax

type error =
  | Duplicated_mode : ('a, 'b) Axis.t -> error
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
          | Some _ -> raise (Error (loc, Duplicated_mode Areality)))
        | "unique" -> (
          match acc.uniqueness with
          | None -> { acc with uniqueness = Some Unique }
          | Some _ -> raise (Error (loc, Duplicated_mode Uniqueness)))
        | "once" -> (
          match acc.linearity with
          | None -> { acc with linearity = Some Once }
          | Some _ -> raise (Error (loc, Duplicated_mode Linearity)))
        | s -> raise (Error (loc, Unrecognized_mode s))
      in
      loop acc rest
  in
  loop Alloc.Const.Option.none modes.txt

let transl_global_flags modalities =
  let rec loop (acc : Global_flag.t Location.loc) = function
    | [] -> acc
    | m :: rest ->
      let ({ txt; loc }) = (m : Asttypes.modality Location.loc) in
      let acc : Global_flag.t Location.loc =
        let (Modality txt) = txt in
        match txt with
        | "global" -> (
          match acc.txt with
          | Unrestricted -> { txt = Global; loc }
          (* Duplicated modality is not an error, just silly and thus a warning.
             As we introduce more modalities, it might be in general difficult
             to detect all redundant modalities, but we should do our best. *)
          | Global ->
            Location.prerr_warning loc (Warnings.Redundant_modality txt);
            acc)
        | s -> raise (Error (loc, Unrecognized_modality s))
      in
      loop acc rest
  in
  loop { txt = Unrestricted; loc = Location.none } modalities

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

open Format

let report_error ppf = function
  | Duplicated_mode ax ->
    let ax =
      match ax with
      | Areality -> dprintf "locality"
      | _ -> dprintf "%a" Axis.print ax
    in
    fprintf ppf "The %t axis has already been specified." ax
  | Unrecognized_mode s -> fprintf ppf "Unrecognized mode name %s." s
  | Unrecognized_modality s -> fprintf ppf "Unrecognized modality %s." s

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
