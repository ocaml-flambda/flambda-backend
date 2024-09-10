(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jakub Bachurski and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Types.type_expr Jkind_types.higher_jkind

val wrap : Jkind.t -> t

val unwrap : loc:string -> t -> Jkind.t

module History : sig
  include module type of struct
    include Jkind_intf.History
  end

  val is_imported : t -> bool

  val update_reason : t -> creation_reason -> t
end

(******************************)
(* constants *)

module Const : sig
  type t = Types.type_expr Jkind_types.Higher_const.t

  val wrap : Jkind.Const.t -> t

  val format : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

val to_const : t -> Const.t option

module Builtin : sig
  val top : why:History.top_creation_reason -> t

  val any : why:History.any_creation_reason -> t

  val void : why:History.void_creation_reason -> t

  val value_or_null : why:History.value_or_null_creation_reason -> t

  val value : why:History.value_creation_reason -> t

  val immediate : why:History.immediate_creation_reason -> t
end

(******************************)
(* construction *)

val of_const : why:History.creation_reason -> Const.t -> t

val of_annotation :
  context:History.annotation_context ->
  Jane_syntax.Jkind.annotation ->
  t * Jkind.annotation

val of_annotation_option_default :
  default:t ->
  context:History.annotation_context ->
  Jane_syntax.Jkind.annotation option ->
  t * Jkind.annotation option

val of_type_decl :
  context:History.annotation_context ->
  Parsetree.type_declaration ->
  (t * Jkind.annotation * Parsetree.attributes) option

val of_type_decl_default :
  context:Jkind.History.annotation_context ->
  default:t ->
  Parsetree.type_declaration ->
  t * Jkind.annotation option * Parsetree.attributes

(******************************)
(* elimination and defaulting *)

val default_to_value : t -> unit

(*********************************)
(* pretty printing *)

val format : Format.formatter -> t -> unit

val format_history :
  intro:(Format.formatter -> unit) -> Format.formatter -> t -> unit

(******************************)
(* errors *)

module Violation = Jkind.Violation

(******************************)
(* relations *)

val equate : t -> t -> bool

val equal : t -> t -> bool

val has_intersection : t -> t -> bool

val intersection_or_error :
  reason:History.interact_reason -> t -> t -> (t, Jkind.Violation.t) Result.t

val sub : t -> t -> bool

val sub_or_error : t -> t -> (unit, Jkind.Violation.t) result

val sub_with_history : t -> t -> (t, Jkind.Violation.t) result

val is_max : t -> bool

val has_layout_any : t -> bool

module Debug_printers : sig
  val t : Format.formatter -> t -> unit

  module Const : sig
    val t : Format.formatter -> Const.t -> unit
  end
end
