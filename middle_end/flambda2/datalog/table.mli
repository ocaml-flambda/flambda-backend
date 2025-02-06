(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Type : sig
  type (_, _) eq = Equal : ('a, 'a) eq
end

module Id : sig
  type ('t, 'k, 'v) t

  val print : Format.formatter -> ('t, 'k, 'v) t -> unit

  val hash : ('t, 'k, 'v) t -> int

  val uid : ('t, 'k, 'v) t -> int

  (* Will raise [Misc.Fatal_error] if the two [Id.t]s do not have the same
     [uid]. *)
  val provably_equal_keys_exn :
    ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> ('k1, 'k2) Type.eq

  val provably_equal_exn :
    ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> ('t1, 't2) Type.eq

  (* Will raise [Misc.Fatal_error] if the two [Id.t]s do not have the same
     [uid]. *)
  val cast_exn : ('t1, 'k1, 'v1) t -> ('t2, 'k2, 'v2) t -> 't1 -> 't2

  val equal : (_, _, _) t -> (_, _, _) t -> bool

  val compare : (_, _, _) t -> (_, _, _) t -> int

  val is_trie : ('t, 'k, 'v) t -> ('t, 'k, 'v) Trie.is_trie

  type ('k, 'v) poly = Id : ('t, 'k, 'v) t -> ('k, 'v) poly

  val create :
    name:string ->
    is_trie:('t, 'k, 'v) Trie.is_trie ->
    print_keys:(Format.formatter -> 'k Heterogenous_list.Constant.hlist -> unit) ->
    default_value:'v ->
    ('t, 'k, 'v) t

  val create_iterator :
    ('t, 'k, 'v) t -> 't ref * 'k Trie.Iterator.hlist * 'v ref
end

module Map : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val is_empty : t -> bool

  val get : ('t, 'k, 'v) Id.t -> t -> 't

  val set : ('t, 'k, 'v) Id.t -> 't -> t -> t

  val concat : earlier:t -> later:t -> t
end
