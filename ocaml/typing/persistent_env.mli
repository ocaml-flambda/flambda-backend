(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

module Consistbl_data : sig
  type t
end

module Consistbl : module type of struct
  include Consistbl.Make (Compilation_unit.Name) (Consistbl_data)
end

type error =
  | Illegal_renaming of Compilation_unit.Name.t * Compilation_unit.Name.t * filepath
  | Inconsistent_import of Compilation_unit.Name.t * filepath * filepath
  | Need_recursive_types of Compilation_unit.Name.t
  | Inconsistent_package_declaration_between_imports of
      filepath * Compilation_unit.t * Compilation_unit.t
  | Direct_reference_from_wrong_package of
      Compilation_unit.t * filepath * Compilation_unit.Prefix.t
  | Illegal_import_of_parameter of Compilation_unit.Name.t * filepath
  | Not_compiled_as_parameter of Compilation_unit.Name.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : Compilation_unit.Name.t;
        parameter : Compilation_unit.Name.t;
  }


exception Error of error

val report_error: Format.formatter -> error -> unit

module Persistent_signature : sig
  type t =
    { filename : string; (** Name of the file containing the signature. *)
      cmi : Cmi_format.cmi_infos_lazy;
      visibility : Load_path.visibility
    }

  (** Function used to load a persistent signature. The default is to look for
      the .cmi file in the load path. This function can be overridden to load
      it from memory, for instance to build a self-contained toplevel. *)
  val load :
    (allow_hidden:bool -> unit_name:Compilation_unit.Name.t -> t option) ref
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

type 'a t

val empty : unit -> 'a t

val clear : 'a t -> unit
val clear_missing : 'a t -> unit

val fold : 'a t -> (Compilation_unit.Name.t -> 'a -> 'b -> 'b) -> 'b -> 'b

type address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type 'a sig_reader =
  Subst.Lazy.signature
  -> Compilation_unit.Name.t
  -> Shape.Uid.t
  -> shape:Shape.t
  -> address:address
  -> flags:Cmi_format.pers_flags list
  -> 'a

(* If [add_binding] is false, reads the signature from the .cmi but does not
   bind the module name in the environment. *)
(* CR-someday lmaurer: [add_binding] is apparently always false, including in the
   [-instantiate] branch. We should remove this parameter. *)
val read : 'a t -> 'a sig_reader
  -> Compilation_unit.Name.t -> filepath -> add_binding:bool -> Subst.Lazy.signature
val find : allow_hidden:bool -> 'a t -> 'a sig_reader
  -> Compilation_unit.Name.t -> 'a

val find_in_cache : 'a t -> Compilation_unit.Name.t -> 'a option

val check : allow_hidden:bool -> 'a t -> 'a sig_reader
  -> loc:Location.t -> Compilation_unit.Name.t -> unit

(* Lets it be known that the given module is a parameter to this module and thus is
   expected to have been compiled as such. Raises an exception if the module has already
   been imported as a non-parameter. *)
val register_parameter : 'a t -> Compilation_unit.Name.t -> unit

(* [is_parameter_import penv md] checks if [md] is a parameter. Raises a fatal
   error if the module has not been imported. *)
val is_parameter_import : 'a t -> Compilation_unit.Name.t -> bool

(* [looked_up penv md] checks if one has already tried
   to read the signature for [md] in the environment
   [penv] (it may have failed) *)
val looked_up : 'a t -> Compilation_unit.Name.t -> bool

(* [is_imported penv md] checks if [md] has been successfully
   imported in the environment [penv] *)
val is_imported : 'a t -> Compilation_unit.Name.t -> bool

(* [is_imported_opaque penv md] checks if [md] has been imported
   in [penv] as an opaque module *)
val is_imported_opaque : 'a t -> Compilation_unit.Name.t -> bool

(* [register_import_as_opaque penv md] registers [md] in [penv] as an
   opaque module *)
val register_import_as_opaque : 'a t -> Compilation_unit.Name.t -> unit

(* [implemented_parameter penv md] returns the argument to [-as-argument-for]
   that [md] was compiled with. *)
val implemented_parameter : 'a t -> Compilation_unit.Name.t
  -> Compilation_unit.Name.t option

val make_cmi : 'a t
  -> Compilation_unit.Name.t
  -> Cmi_format.kind
  -> Subst.Lazy.signature
  -> alerts
  -> Cmi_format.cmi_infos_lazy

val save_cmi : 'a t -> Persistent_signature.t -> unit

val can_load_cmis : 'a t -> can_load_cmis
val set_can_load_cmis : 'a t -> can_load_cmis -> unit
val without_cmis : 'a t -> ('b -> 'c) -> 'b -> 'c
(* [without_cmis penv f arg] applies [f] to [arg], but does not
    allow [penv] to openi cmis during its execution *)

(* may raise Consistbl.Inconsistency *)
val import_crcs : 'a t -> source:filepath ->
  Import_info.t array -> unit

(* Return the set of compilation units imported, with their CRC *)
val imports : 'a t -> Import_info.t list

(* Return the list of imported modules (including parameters) that must be bound
   as parameters in a toplevel functor *)
val locally_bound_imports : 'a t -> (Compilation_unit.Name.t * Ident.t) list

(* Return the list of parameters registered to be exported from the current
   unit, in alphabetical order *)
val parameters : 'a t -> Compilation_unit.Name.t list

(* Return the CRC of the interface of the given compilation unit *)
val crc_of_unit: 'a t -> Compilation_unit.Name.t -> Digest.t

(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
