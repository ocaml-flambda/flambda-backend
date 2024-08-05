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

type cms_infos = {
  cms_modname : Compilation_unit.t;
  cms_comments : (string * Location.t) list;
  cms_sourcefile : string option;
  cms_builddir : string;
  cms_source_digest : string option;
  (* CR: cms_initial_env is likely unnecessary, as the initial_env can be reconstructed
     if we store a few relevant flags. *)
  cms_initial_env : Env.t;
  cms_uid_to_loc : (Shape.Uid.t * string Location.loc) Array.t;
  cms_uid_to_attributes : Parsetree.attributes Shape.Uid.Tbl.t;
  cms_impl_shape : Shape.t option; (* None for mli *)
  cms_ident_occurrences :
    (Longident.t Location.loc * Shape_reduce.result) array
}

type error =
  Not_a_shape of string

exception Error of error

(** [read filename] opens filename, and extract the cms_infos. It
    can be used with .cms and .cmsi files.
*)
val read : string -> cms_infos

(** [save_cms filename modname sourcefile shape]
    writes a cms(i) file.  *)
val save_cms :
  string ->  (* filename.cms to generate *)
  Compilation_unit.t ->  (* module name *)
  Cmt_format.binary_annots ->
  string option ->  (* source file *)
  Env.t -> (* initial env *)
  Shape.t option ->
  unit

val register_toplevel_attributes :
  Shape.Uid.t ->
  attributes:Parsetree.attribute list ->
  loc:Location.t ->
  unit

(* Miscellaneous functions *)

val read_magic_number : in_channel -> string

val clear : unit -> unit
