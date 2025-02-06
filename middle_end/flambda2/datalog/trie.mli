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

open Heterogenous_list

(** [('t, 'k, 'v) is_trie] is a witness that the type ['t] is a trie from keys
    of type ['k Constant.hlist] to values of type ['v]. *)
type ('t, 'k, 'v) is_trie

val trie_depth : ('t, 'k, 'v) is_trie -> int

val patricia_tree_is_trie : ('v Patricia_tree.map, int -> nil, 'v) is_trie

val patricia_tree_of_trie :
  ('s, 'b, 'v) is_trie -> ('s Patricia_tree.map, int -> 'b, 'v) is_trie

(** Existential witness for tries of given key and value types.

   {b Note}: This type is currently unused by the [Trie] module, but is provided
   as a convenience for users that need to create existentially quantified
   tries. *)
type ('k, 'v) is_any_trie =
  | Is_trie : ('t, 'k, 'v) is_trie -> ('k, 'v) is_any_trie

val empty : ('t, 'k, 'v) is_trie -> 't

val is_empty : ('t, 'k, 'v) is_trie -> 't -> bool

val singleton : ('t, 'k, 'v) is_trie -> 'k Constant.hlist -> 'v -> 't

val add_or_replace : ('t, 'k, 'v) is_trie -> 'k Constant.hlist -> 'v -> 't -> 't

val remove : ('t, 'k, 'v) is_trie -> 'k Constant.hlist -> 't -> 't

val union : ('t, 'k, 'v) is_trie -> ('v -> 'v -> 'v option) -> 't -> 't -> 't

val find_opt : ('t, 'k, 'v) is_trie -> 'k Constant.hlist -> 't -> 'v option

module Iterator : sig
  include Leapfrog.Iterator

  (** [create is_trie name input output] creates a trie iterator.

      The [input] reference is used to initialize the first iterator when [init]
      is called.

      The [output] reference is set to the corresponding value when [accept] is
      called on the last iterator.
  *)
  val create : ('m, 'k, 'v) is_trie -> string -> 'm Named_ref.t -> 'v Named_ref.t -> 'k hlist
end
