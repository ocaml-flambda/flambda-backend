(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The underlying implementation for [Variable], [Symbol], [Name],
    [Reg_width_const], [Simple], [Code_id] and [Code_id_or_symbol]. These values
    are all represented as integers. *)

module Const : sig
  type t = private Table_by_int_id.Id.t

  type exported

  include Container_types.S with type t := t

  val null : t

  val const_true : t

  val const_false : t

  val untagged_const_true : t

  val untagged_const_false : t

  val untagged_const_zero : t

  val untagged_const_int : Targetint_31_63.t -> t

  val const_zero : t

  val const_one : t

  val const_unit : t

  val const_int : Targetint_31_63.t -> t

  (** [naked_immediate] is similar to [naked_nativeint], but represents integers
      of width [n - 1] bits, where [n] is the native machine width. (By
      contrast, [naked_nativeint] represents integers of width [n] bits.) *)
  val naked_immediate : Targetint_31_63.t -> t

  val tagged_immediate : Targetint_31_63.t -> t

  val naked_float32 : Numeric_types.Float32_by_bit_pattern.t -> t

  val naked_float : Numeric_types.Float_by_bit_pattern.t -> t

  val naked_int32 : Int32.t -> t

  val naked_int64 : Int64.t -> t

  val naked_nativeint : Targetint_32_64.t -> t

  val naked_vec128 : Vector_types.Vec128.Bit_pattern.t -> t

  module Descr : sig
    type t = private
      | Naked_immediate of Targetint_31_63.t
      | Tagged_immediate of Targetint_31_63.t
      | Naked_float32 of Numeric_types.Float32_by_bit_pattern.t
      | Naked_float of Numeric_types.Float_by_bit_pattern.t
      | Naked_int32 of Int32.t
      | Naked_int64 of Int64.t
      | Naked_nativeint of Targetint_32_64.t
      | Naked_vec128 of Vector_types.Vec128.Bit_pattern.t
      | Null

    include Container_types.S with type t := t
  end

  val descr : t -> Descr.t

  val export : t -> exported

  val import : exported -> t
end

module Variable : sig
  type t = private Table_by_int_id.Id.t

  type exported

  include Container_types.S with type t := t

  val create : ?user_visible:unit -> string -> t

  val compilation_unit : t -> Compilation_unit.t

  val name : t -> string

  val name_stamp : t -> int

  val user_visible : t -> bool

  val export : t -> exported

  val import : exported -> t
end

module Symbol : sig
  type t = private Table_by_int_id.Id.t

  type exported

  include Container_types.S with type t := t

  (* CR lmaurer: This treats the [Linkage_name.t] as a string to be prefixed
     rather than the actual linkage name. That's not really consistent with the
     way linkage names are treated elsewhere. *)
  val create : Compilation_unit.t -> Linkage_name.t -> t

  val create_wrapped : Flambda2_import.Symbol.t -> t

  val unsafe_create : Compilation_unit.t -> Linkage_name.t -> t

  val compilation_unit : t -> Compilation_unit.t

  val linkage_name : t -> Linkage_name.t

  val linkage_name_as_string : t -> string

  val export : t -> exported

  val import : exported -> t

  val external_symbols_compilation_unit : unit -> Compilation_unit.t
end

module Name : sig
  type t = private Table_by_int_id.Id.t

  include Container_types.S with type t := t

  val var : Variable.t -> t

  val symbol : Symbol.t -> t

  val pattern_match :
    t -> var:(Variable.t -> 'a) -> symbol:(Symbol.t -> 'a) -> 'a
end

module Rec_info_expr : Rec_info_expr0.S with type variable = Variable.t

module Coercion :
  Coercion0.S
    with type variable = Variable.t
     and type rec_info_expr = Rec_info_expr.t

module Simple : sig
  type t = private Table_by_int_id.Id.t

  type exported

  include Container_types.S with type t := t

  val name : Name.t -> t

  val var : Variable.t -> t

  val vars : Variable.t list -> t list

  val symbol : Symbol.t -> t

  val const : Const.t -> t

  val coercion : t -> Coercion.t

  val with_coercion : t -> Coercion.t -> t

  (* This does not consult the grand table of [Simple]s. *)
  val has_coercion : t -> bool

  (* CR lmaurer: Should make [name] and [const] take a [coercion] argument to be
     sure we're not dropping coercions by accident. *)
  val pattern_match :
    t ->
    name:(Name.t -> coercion:Coercion.t -> 'a) ->
    const:(Const.t -> 'a) ->
    'a

  (* [same s1 s2] returns true iff they represent the same name or const i.e.
     [same s (with_coercion s coercion)] returns true *)
  val same : t -> t -> bool

  val export : t -> exported

  val import : exported -> t
end

module Code_id : sig
  type t = private Table_by_int_id.Id.t

  type exported

  include Container_types.S with type t := t

  module Lmap : Lmap.S with type key = t

  val initialise : unit -> unit

  val create : name:string -> Compilation_unit.t -> t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val linkage_name : t -> Linkage_name.t

  val name : t -> string

  (* The [rename] function, in addition to changing the stamp of the code ID,
     changes the compilation unit to the current one. *)
  val rename : t -> t

  val invert_map : t Map.t -> t Map.t

  val export : t -> exported

  val import : exported -> t
end

module Code_id_or_symbol : sig
  type t = private Table_by_int_id.Id.t

  include Container_types.S with type t := t

  module Lmap : Lmap.S with type key = t

  val create_code_id : Code_id.t -> t

  val create_symbol : Symbol.t -> t

  val compilation_unit : t -> Compilation_unit.t

  val set_of_code_id_set : Code_id.Set.t -> Set.t

  val set_of_symbol_set : Symbol.Set.t -> Set.t

  val pattern_match :
    t -> code_id:(Code_id.t -> 'a) -> symbol:(Symbol.t -> 'a) -> 'a
end

val initialise : unit -> unit

val reset : unit -> unit
