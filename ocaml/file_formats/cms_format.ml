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

let save_cms filename modname sourcefile shape =
  if (!Clflags.binary_annotations_cms && not !Clflags.print_types) then begin
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] filename
       (fun _temp_file_name oc ->
         let source_digest = Option.map Digest.file sourcefile in
         let cms = {
           cms_modname = modname;
           cms_comments = Lexer.comments ();
           cms_sourcefile = sourcefile;
           cms_builddir = Location.rewrite_absolute_path (Sys.getcwd ());
           cms_source_digest = source_digest;
           (* CR poechsel: fix CMS file generation *)
           cms_uid_to_loc = Types.Uid.Tbl.create 0;
           cms_uid_to_attributes = Types.Uid.Tbl.create 0;
           cms_impl_shape = shape;
         } in
         output_cms oc cms)
  end

let clear () = ()
