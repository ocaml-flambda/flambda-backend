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

module Consistbl_data = Import_info.Intf.Nonalias.Kind

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
  | Illegal_import_of_parameter of Global_module.Name.t * filepath
  | Not_compiled_as_parameter of Global_module.Name.t * filepath
  | Imported_module_has_unset_parameter of
      { imported : Global_module.Name.t;
        parameter : Global_module.Name.t;
      }
  | Imported_module_has_no_such_parameter of
      { imported : Compilation_unit.Name.t;
        valid_parameters : Global_module.Name.t list;
        parameter : Global_module.Name.t;
        value : Global_module.Name.t;
      }
  | Not_compiled_as_argument of
      { param : Global_module.Name.t;
        value : Global_module.Name.t;
        filename : filepath;
      }
  | Argument_type_mismatch of
      { value : Global_module.Name.t;
        filename : filepath;
        expected : Global_module.Name.t;
        actual : Global_module.Name.t;
      }
  | Inconsistent_global_name_resolution of
      { name : Global_module.Name.t;
        old_global : Global_module.t;
        new_global : Global_module.t;
        first_mentioned_by : Global_module.Name.t;
        now_mentioned_by : Global_module.Name.t;
      }
  | Unbound_module_as_argument_value of
      { instance : Global_module.Name.t; value : Global_module.Name.t; }



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

val fold : 'a t -> (Global_module.Name.t -> 'a -> 'b -> 'b) -> 'b -> 'b

type address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type 'a sig_reader =
  Subst.Lazy.signature
  -> Global_module.Name.t
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
  -> Global_module.Name.t -> Unit_info.Artifact.t -> add_binding:bool
  -> Subst.Lazy.signature
val find : allow_hidden:bool -> 'a t -> 'a sig_reader
  -> Global_module.Name.t -> 'a

val find_in_cache : 'a t -> Global_module.Name.t -> 'a option

val check : allow_hidden:bool -> 'a t -> 'a sig_reader
  -> loc:Location.t -> Global_module.Name.t -> unit

(* Lets it be known that the given module is a parameter to this module and thus is
   expected to have been compiled as such. Raises an exception if the module has already
   been imported as a non-parameter. *)
val register_parameter : 'a t -> Global_module.Name.t -> unit

(* [is_parameter_import penv md] checks if [md] is a parameter. Raises a fatal
   error if the module has not been imported. *)
val is_parameter_import : 'a t -> Global_module.Name.t -> bool

(* [looked_up penv md] checks if one has already tried
   to read the signature for [md] in the environment
   [penv] (it may have failed) *)
val looked_up : 'a t -> Global_module.Name.t -> bool

(* [is_imported_opaque penv md] checks if [md] has been imported
   in [penv] as an opaque module *)
val is_imported_opaque : 'a t -> Compilation_unit.Name.t -> bool

(* [register_import_as_opaque penv md] registers [md] in [penv] as an
   opaque module *)
val register_import_as_opaque : 'a t -> Compilation_unit.Name.t -> unit

(* [local_ident penv md] returns the local identifier generated for [md] if
   [md] is either a parameter or a dependency with a parameter. This is used
   strictly for code generation - types should always use persistent
   [Ident.t]s. *)
val local_ident : 'a t -> Global_module.Name.t -> Ident.t option

(* [implemented_parameter penv md] returns the argument to [-as-argument-for]
   that [md] was compiled with. *)
val implemented_parameter : 'a t
  -> Global_module.Name.t -> Global_module.Name.t option

val global_of_global_name : 'a t
  -> check:bool
  -> Global_module.Name.t
  -> Global_module.t

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
val import_crcs : 'a t -> source:filepath -> Import_info.Intf.t array -> unit

(* Return the set of compilation units imported, with their CRC *)
val imports : 'a t -> Import_info.Intf.t list

(* Return the set of imports represented as runtime parameters. If this module is indeed
   parameterised (that is, [parameters] returns a non-empty list), it will be compiled as
   a functor rather than a [struct] as usual, and the parameters to this functor are what
   we refer to as "runtime parameters." They include (a) all imported parameters (not all
   parameters are necessarily imported; see [parameters]) and (b) all imported
   parameterised modules.

   Note that the word "runtime" is a bit of a fiction reflecting a front-end view of the
   world. In fact we aim to inline away all passing of runtime parameters. *)
val runtime_parameters : 'a t -> (Global_module.Name.t * Ident.t) list

(* Return the list of parameters specified for the current unit, in alphabetical order.
   All of these will have been specified by [-parameter] but not all of them are
   necessarily imported - any that don't appear in the source are still considered
   parameters of the module but will not appear in [imports]. *)
val parameters : 'a t -> Global_module.Name.t list

(* Return the CRC of the interface of the given compilation unit *)
val crc_of_unit: 'a t -> Compilation_unit.Name.t -> Digest.t

(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
