(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** cms and cmsi files format. *)

let read_magic_number ic =
  let len_magic_number = String.length Config.cms_magic_number in
  really_input_string ic len_magic_number

type cms_infos = {
  cms_modname : Compilation_unit.t;
  cms_comments : (string * Location.t) list;
  cms_sourcefile : string option;
  cms_builddir : string;
  cms_source_digest : Digest.t option;
  cms_initial_env : Env.t option;
  cms_uid_to_loc : string Location.loc Shape.Uid.Tbl.t;
  cms_uid_to_attributes : Parsetree.attributes Shape.Uid.Tbl.t;
  cms_impl_shape : Shape.t option; (* None for mli *)
  cms_ident_occurrences :
    (Longident.t Location.loc * Shape_reduce.result) array
}

type error =
Not_a_shape of string

exception Error of error

let input_cms ic = (input_value ic : cms_infos)

let output_cms oc cms =
  output_string oc Config.cms_magic_number;
  output_value oc (cms : cms_infos)

let read filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
       let magic_number = read_magic_number ic in
        if magic_number = Config.cms_magic_number then
          input_cms ic
        else
          raise (Error (Not_a_shape filename))
    )

let toplevel_attributes = ref []

let register_toplevel_attributes uid ~attributes ~loc =
  let loc : _ Location.loc = { txt = ""; loc } in
  toplevel_attributes := (uid, loc, attributes) :: !toplevel_attributes

let uid_tables_of_binary_annots binary_annots =
  let cms_uid_to_loc = Types.Uid.Tbl.create 42 in
  let cms_uid_to_attributes = Types.Uid.Tbl.create 42 in
  List.iter (fun (uid, loc, attrs) ->
    Types.Uid.Tbl.add cms_uid_to_loc uid loc;
    Types.Uid.Tbl.add cms_uid_to_attributes uid attrs)
    !toplevel_attributes;
  Cmt_format.iter_declarations binary_annots
    ~f:(fun uid decl ->
      let loc = Typedtree.loc_of_decl ~uid decl in
      let attrs =
        match decl with
        | Value v -> v.val_attributes
        | Value_binding v -> v.vb_attributes
        | Type v -> v.typ_attributes
        | Constructor v -> v.cd_attributes
        | Extension_constructor v -> v.ext_attributes
        | Label v -> v.ld_attributes
        | Module v -> v.md_attributes
        | Module_substitution v -> v.ms_attributes
        | Module_binding v -> v.mb_attributes
        | Module_type v -> v.mtd_attributes
        | Class v -> v.ci_attributes
        | Class_type v -> v.ci_attributes
      in
      Types.Uid.Tbl.add cms_uid_to_loc uid loc;
      Types.Uid.Tbl.add cms_uid_to_attributes uid attrs
    );
  cms_uid_to_loc, cms_uid_to_attributes

let save_cms target modname binary_annots initial_env shape =
  if (!Clflags.binary_annotations_cms && not !Clflags.print_types) then begin
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] (Unit_info.Artifact.filename target)
       (fun _temp_file_name oc ->

        let sourcefile = Unit_info.Artifact.source_file target in
        let source_digest = Option.map Digest.file sourcefile in
        let cms_ident_occurrences, cms_initial_env =
          if !Clflags.store_occurrences then
            let cms_ident_occurrences = Cmt_format.index_occurrences binary_annots in
            let cms_initial_env = if Cmt_format.need_to_clear_env
              then Env.keep_only_summary initial_env else initial_env in
            cms_ident_occurrences, Some cms_initial_env
          else
            [| |], None
        in
        let cms_uid_to_loc, cms_uid_to_attributes =
          uid_tables_of_binary_annots binary_annots
        in
        let cms =
          {
            cms_modname = modname;
            cms_comments = Lexer.comments ();
            cms_sourcefile = sourcefile;
            cms_builddir = Location.rewrite_absolute_path (Sys.getcwd ());
            cms_source_digest = source_digest;
            cms_initial_env;
            cms_uid_to_loc;
            cms_uid_to_attributes;
            cms_impl_shape = shape;
            cms_ident_occurrences
          }
        in
        output_cms oc cms)
  end

let clear () = ()
