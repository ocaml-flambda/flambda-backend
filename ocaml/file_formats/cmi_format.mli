(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque

type kind =
  | Normal of {
      cmi_impl : Compilation_unit.t;
        (* If this module takes parameters, [cmi_impl] will be the functor that
           generates instances *)
      cmi_arg_for : Compilation_unit.Name.t option;
    }
  | Parameter

type 'sg cmi_infos_generic = {
    cmi_name : Compilation_unit.Name.t;
    cmi_kind : kind;
    cmi_sign : 'sg;
    cmi_params : Compilation_unit.Name.t list;
    cmi_crcs : Import_info.t array;
    cmi_flags : pers_flags list;
}

type cmi_infos_lazy = Subst.Lazy.signature cmi_infos_generic
type cmi_infos = Types.signature cmi_infos_generic

(* write the magic + the cmi information *)
val output_cmi : string -> out_channel -> cmi_infos_lazy -> Digest.t

(* read the cmi information (the magic is supposed to have already been read) *)
val input_cmi : in_channel -> cmi_infos
val input_cmi_lazy : in_channel -> cmi_infos_lazy

(* read a cmi from a filename, checking the magic *)
val read_cmi : string -> cmi_infos
val read_cmi_lazy : string -> cmi_infos_lazy

(* The contents of the module block *)
type module_block_layout =
  | Single_block
      (* One block whose contents are described by [cmi_sign] *)
  | Full_module_and_argument_form
      (* An outer block containing two blocks: the usual module block, and a
         block used as the value of the module when it's passed as an argument.
         The first block's signature is [cmi_sign] and the second block's
         signature is described by the .cmi file for [cmi_arg_for] ([cmi_kind]
         must be [Normal]). *)
val module_block_layout : _ cmi_infos_generic -> module_block_layout

(* Error report *)

type error =
  | Not_an_interface of filepath
  | Wrong_version_interface of filepath * string
  | Corrupted_interface of filepath

exception Error of error

open Format

val report_error: formatter -> error -> unit
