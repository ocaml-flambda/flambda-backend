open Location
open Mode
open Jane_syntax

type error = Duplicated_mode of Axis.t

exception Error of Location.t * error

let transl_mode_annots modes =
  let rec loop (acc : Alloc.Const.Option.t) : _ -> Alloc.Const.Option.t =
    function
    | [] -> acc
    | m :: rest ->
      let { txt; loc } = (m : Mode_expr.Const.t :> _ Location.loc) in
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
        | "global" ->
          (* CR zqian: global modality might leak to here by ppxes.
             This is a dirty fix that needs to be fixed ASAP. *)
          acc
        | s ->
          Misc.fatal_errorf "Unrecognized mode %s at %a - should not parse" s
            Location.print_loc loc
      in
      loop acc rest
  in
  loop Alloc.Const.Option.none modes.txt

let transl_global_flags gfs =
  let rec loop (acc : Global_flag.t) : _ -> Global_flag.t = function
    | [] -> acc
    | m :: rest ->
      let { txt; loc } = (m : Mode_expr.Const.t :> _ Location.loc) in
      let acc =
        match txt with
        | "global" -> (
          Jane_syntax_parsing.assert_extension_enabled ~loc Mode ();
          match acc with
          | Unrestricted -> Global_flag.Global
          | Global ->
            Misc.fatal_errorf
              "Duplicated global modality at %a - should not parse"
              Location.print_loc loc)
        | s ->
          Misc.fatal_errorf "Unrecognized modality %s at %a - should not parse"
            s Location.print_loc loc
      in
      loop acc rest
  in
  loop Unrestricted gfs.txt

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

open Format

let report_error ppf = function
  | Duplicated_mode ax ->
    fprintf ppf "The %s axis has already been specified." (Axis.to_string ax)

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
