(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

exception Exit_with_status of int

val output_prefix : string -> string
val extract_output : string option -> string
val default_output : string option -> string

val print_version_and_library : string -> 'a
val print_version_string : unit -> 'a
val print_standard_library : unit -> 'a
val fatal : string -> 'a

val first_ccopts : string list ref
val first_ppx : string list ref
val first_include_dirs : string list ref
val last_include_dirs : string list ref

(* return the list of objfiles, after OCAMLPARAM and List.rev *)
val get_objfiles : with_ocamlparam:bool -> string list
val last_objfiles : string list ref
val first_objfiles : string list ref

val stop_early : bool ref
val has_linker_inputs : bool ref

type filename = string

type readenv_position =
  Before_args | Before_compile of filename | Before_link

val readenv : Format.formatter -> readenv_position -> unit
val set_extra_params :
  (Format.formatter -> readenv_position -> string -> string -> bool) option ->
  unit
(* Enable/disable warning about discarding any unknown arguments.  *)
val warnings_for_discarded_params : bool ref

val setter :
    Format.formatter -> (bool -> 'a) -> string -> 'a ref list -> string -> unit
val int_setter : Format.formatter -> string -> int ref -> string -> unit
val check_bool : Format.formatter -> string -> string -> bool
val check_int : Format.formatter -> string -> string -> int option

(* Deferred actions of the compiler, while parsing arguments *)

type deferred_action =
  | ProcessImplementation of string
  | ProcessInterface of string
  | ProcessCFile of string
  | ProcessOtherFile of string
  | ProcessObjects of string list
  | ProcessDLLs of string list

val c_object_of_filename : string -> string

val defer : deferred_action -> unit
val anonymous : string -> unit
val impl : string -> unit
val intf : string -> unit

val process_deferred_actions :
  Format.formatter *
  (start_from:Clflags.Compiler_pass.t ->
   source_file:string -> output_prefix:string ->
   keep_symbol_tables:bool -> unit) *
  (* compile implementation *)
  (source_file:string -> output_prefix:string -> unit) *
  (* compile interface *)
  string * (* ocaml module extension *)
  string -> (* ocaml library extension *)
  unit
(* [parse_arguments ?current argv anon_arg program] will parse the arguments,
 using the arguments provided in [Clflags.arg_spec].
*)
val parse_arguments : ?current:(int ref)
      -> string array ref -> Arg.anon_fun -> string -> unit
