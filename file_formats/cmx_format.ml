(* Format of .cmx, .cmxa and .cmxs files *)

open Misc
open Config

module File_sections = Flambda_backend_utils.File_sections

type error =
    Not_a_unit_info of filepath
  | Corrupted_unit_info of filepath

exception Error of error

type machtype_component = Val | Addr | Int | Float | Vec128 | Float32
type machtype = machtype_component array

type apply_fn = machtype list * machtype * Lambda.alloc_mode

(* Curry/apply/send functions *)
type generic_fns =
  { curry_fun: (Lambda.function_kind * machtype list * machtype) list;
    apply_fun: apply_fn list;
    send_fun: apply_fn list }

type unit_infos =
  { mutable ui_unit: Compilation_unit.t;
    mutable ui_defines: Compilation_unit.t list;
    mutable ui_imports_cmi: Import_info.t list;
    mutable ui_imports_cmx: Import_info.t list;
    mutable ui_generic_fns: generic_fns;
    mutable ui_export_info: Flambda2_cmx.Flambda_cmx_format.t option;
    mutable ui_zero_alloc_info: Zero_alloc_info.t;
    mutable ui_force_link: bool;
    mutable ui_external_symbols: string list;
  }

type unit_infos_raw =
  { uir_unit: Compilation_unit.t;
    uir_defines: Compilation_unit.t list;
    uir_imports_cmi: Import_info.t array;
    uir_imports_cmx: Import_info.t array;
    uir_generic_fns: generic_fns;
    uir_export_info: Flambda2_cmx.Flambda_cmx_format.raw option;
    uir_zero_alloc_info: Zero_alloc_info.Raw.t;
    uir_force_link: bool;
    uir_section_toc: int array;    (* Byte offsets of sections in .cmx
                                      relative to byte immediately after
                                      this record *)
    uir_sections_length: int;      (* Byte length of all sections *)
    uir_external_symbols: string array;
  }

type lib_unit_info_raw =
  { lir_name: Compilation_unit.t;
    lir_crc: Digest.t;
    lir_defines: Compilation_unit.t list;
    lir_force_link: bool;
    lir_imports_cmi : Bitmap.t;  (* subset of libr_imports_cmi *)
    lir_imports_cmx : Bitmap.t;  (* subset of libr_imports_cmx *)
    lir_external_symbols: string array;
  }

type library_infos_raw =
  { libr_imports_cmi: Import_info.t array;
    libr_imports_cmx: Import_info.t array;
    libr_units: lib_unit_info_raw list;
    libr_generic_fns: generic_fns;
    libr_ccobjs: string list;
    libr_ccopts: string list }

type library_infos =
  { lib_imports_cmi: Import_info.t list;
    lib_imports_cmx: Import_info.t list;
    lib_units: (unit_infos * Digest.t) list;
    lib_generic_fns: generic_fns;
    lib_ccobjs: string list;
    lib_ccopts: string list }


let read_unit_info ~filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let uir = (input_value ic : unit_infos_raw) in
    let first_section_offset = pos_in ic in
    seek_in ic (first_section_offset + uir.uir_sections_length);
    let crc = Digest.input ic in
    (* This consumes the channel *)
    let sections = File_sections.create uir.uir_section_toc filename ic ~first_section_offset in
    let export_info =
      Option.map (Flambda2_cmx.Flambda_cmx_format.from_raw ~sections)
        uir.uir_export_info
    in
    let ui = {
      ui_unit = uir.uir_unit;
      ui_defines = uir.uir_defines;
      ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
      ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
      ui_generic_fns = uir.uir_generic_fns;
      ui_export_info = export_info;
      ui_zero_alloc_info = Zero_alloc_info.of_raw uir.uir_zero_alloc_info;
      ui_force_link = uir.uir_force_link;
      ui_external_symbols = uir.uir_external_symbols |> Array.to_list;
    }
    in
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let write_unit_info ~filename info =
  let raw_export_info, sections =
    match info.ui_export_info with
    | None -> None, File_sections.empty
    | Some info ->
      let info, sections = Flambda2_cmx.Flambda_cmx_format.to_raw info in
      Some info, sections
  in
  let serialized_sections, toc, total_length = File_sections.serialize sections in
  let raw_info = {
    uir_unit = info.ui_unit;
    uir_defines = info.ui_defines;
    uir_imports_cmi = Array.of_list info.ui_imports_cmi;
    uir_imports_cmx = Array.of_list info.ui_imports_cmx;
    uir_generic_fns = info.ui_generic_fns;
    uir_export_info = raw_export_info;
    uir_zero_alloc_info = Zero_alloc_info.to_raw info.ui_zero_alloc_info;
    uir_force_link = info.ui_force_link;
    uir_section_toc = toc;
    uir_sections_length = total_length;
    uir_external_symbols = Array.of_list info.ui_external_symbols;
  } in
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc (raw_info : unit_infos_raw);
  Array.iter (output_string oc) serialized_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let dummy_generic_fns : generic_fns =
  { curry_fun = []; apply_fun = []; send_fun = [] }

let read_library_info ~filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos_raw = (input_value ic : library_infos_raw) in
  close_in ic;
  let units =
    List.map (fun (lir : lib_unit_info_raw) ->
      let imports_list tbl bits =
        List.init (Array.length tbl) (fun i ->
          if Misc.Bitmap.get bits i then Some tbl.(i) else None)
        |> List.filter_map Fun.id
      in
      let ui = {
        ui_unit = lir.lir_name;
        ui_defines = lir.lir_defines;
        ui_imports_cmi = imports_list infos_raw.libr_imports_cmi lir.lir_imports_cmi;
        ui_imports_cmx = imports_list infos_raw.libr_imports_cmx lir.lir_imports_cmx;
        ui_generic_fns = dummy_generic_fns;
        ui_export_info = None;
        ui_zero_alloc_info = Zero_alloc_info.create ();
        ui_force_link = lir.lir_force_link;
        ui_external_symbols = lir.lir_external_symbols |> Array.to_list;
      } in
      ui, lir.lir_crc
    )
    infos_raw.libr_units
  in
  { lib_units = units;
    lib_imports_cmi = infos_raw.libr_imports_cmi |> Array.to_list;
    lib_imports_cmx = infos_raw.libr_imports_cmx |> Array.to_list;
    lib_generic_fns = infos_raw.libr_generic_fns;
    lib_ccobjs = infos_raw.libr_ccobjs;
    lib_ccopts = infos_raw.libr_ccopts }

let write_library_info ~filename:lib_name info =
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name)
    (fun () ->
       output_string outchan cmxa_magic_number;
       (* CR mshinwell: see comment in compilenv.ml
       let cmxs =
         Compilenv.ensure_sharing_between_cmi_and_cmx_imports cmis cmxs
       in
       *)
       let cmis = Array.of_list info.lib_imports_cmi in
       let cmxs = Array.of_list info.lib_imports_cmx in
       let cmi_index = Compilation_unit.Name.Tbl.create 42 in
       Array.iteri (fun i import ->
           Compilation_unit.Name.Tbl.add cmi_index (Import_info.name import) i)
         cmis;
       let cmx_index = Compilation_unit.Tbl.create 42 in
       Array.iteri (fun i import ->
           Compilation_unit.Tbl.add cmx_index (Import_info.cu import) i)
         cmxs;
       let mk_bitmap arr ix entries ~find ~get_name =
         let module B = Misc.Bitmap in
         let b = B.make (Array.length arr) in
         List.iter (fun import -> B.set b (find ix (get_name import))) entries;
         b
       in
       let units =
         List.map (fun (unit, crc) ->
           { lir_name = unit.ui_unit;
             lir_crc = crc;
             lir_defines = unit.ui_defines;
             lir_force_link = unit.ui_force_link;
             lir_imports_cmi =
               mk_bitmap cmis cmi_index unit.ui_imports_cmi
                 ~find:Compilation_unit.Name.Tbl.find
                 ~get_name:Import_info.name;
             lir_imports_cmx =
               mk_bitmap cmxs cmx_index unit.ui_imports_cmx
                 ~find:Compilation_unit.Tbl.find
                 ~get_name:Import_info.cu;
             lir_external_symbols = Array.of_list unit.ui_external_symbols })
         info.lib_units
       in
       let infos =
         { libr_units = units;
           libr_imports_cmi = cmis;
           libr_imports_cmx = cmxs;
           libr_generic_fns = info.lib_generic_fns;
           libr_ccobjs = !Clflags.ccobjs;
           libr_ccopts = !Clflags.all_ccopts } in
       output_value outchan (infos : library_infos_raw)
     )


(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
    fprintf ppf "%a@ is not a compilation unit description."
      Location.print_filename filename
  | Corrupted_unit_info filename ->
    fprintf ppf "Corrupted compilation unit description@ %a"
      Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
