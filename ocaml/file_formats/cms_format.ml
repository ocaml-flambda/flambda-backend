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
  cms_uid_to_loc : Location.t Shape.Uid.Tbl.t;
  cms_uid_to_attributes : Parsetree.attributes Shape.Uid.Tbl.t;
  cms_impl_shape : Shape.t option; (* None for mli *)
  cmt_ident_occurrences :
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

let register_topevel_attributes uid ~attributes ~loc =
  toplevel_attributes := (uid, loc, attributes) :: !toplevel_attributes

let iter_decls binary_annots =
  let cms_uid_to_loc = Types.Uid.Tbl.create 42 in
  let cms_uid_to_attributes = Types.Uid.Tbl.create 42 in
  List.iter (fun (uid, loc, attrs) ->
    Types.Uid.Tbl.add cms_uid_to_loc uid loc;
    Types.Uid.Tbl.add cms_uid_to_attributes uid attrs)
    !toplevel_attributes;
  Cmt_format.iter_declarations binary_annots
  ~f:(fun uid decl ->
    let loc, attrs =
    match decl with
      | Value v -> v.val_loc, v.val_attributes
      | Value_binding v -> v.vb_loc, v.vb_attributes
      | Type v -> v.typ_loc, v.typ_attributes
      | Constructor v -> v.cd_loc, v.cd_attributes
      | Extension_constructor v -> v.ext_loc, v.ext_attributes
      | Label v -> v.ld_loc, v.ld_attributes
      | Module v -> v.md_loc, v.md_attributes
      | Module_substitution v -> v.ms_loc, v.ms_attributes
      | Module_binding v -> v.mb_loc, v.mb_attributes
      | Module_type v -> v.mtd_loc, v.mtd_attributes
      | Class v -> v.ci_loc, v.ci_attributes
      | Class_type v -> v.ci_loc, v.ci_attributes
    in
    Types.Uid.Tbl.add cms_uid_to_loc uid loc;
    Types.Uid.Tbl.add cms_uid_to_attributes uid attrs
  );
  cms_uid_to_loc, cms_uid_to_attributes

let save_cms filename modname binary_annots sourcefile shape =
  if (!Clflags.binary_annotations_cms && not !Clflags.print_types) then begin
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] filename
       (fun _temp_file_name oc ->
         let source_digest = Option.map Digest.file sourcefile in
         let cmt_ident_occurrences =
          if !Clflags.store_occurrences then
            Cmt_format.index_occurrences binary_annots
          else
            Array.of_list []
         in
         let cms_uid_to_loc, cms_uid_to_attributes = iter_decls binary_annots in
         let cms = {
           cms_modname = modname;
           cms_comments = Lexer.comments ();
           cms_sourcefile = sourcefile;
           cms_builddir = Location.rewrite_absolute_path (Sys.getcwd ());
           cms_source_digest = source_digest;
           (* CR poechsel: fix CMS file generation *)
           cms_uid_to_loc;
           cms_uid_to_attributes;
           cms_impl_shape = shape;
           cmt_ident_occurrences
         } in
         output_cms oc cms)
  end

let clear () = ()
