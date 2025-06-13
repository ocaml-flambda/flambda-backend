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

module Int = struct
  include Numbers.Int
  module Tree = Patricia_tree.Make (Numbers.Int)
  module Map = Tree.Map
  module Set = Tree.Set
end

type (_, _, _) is_trie =
  | Map_is_trie : ('v Int.Map.t, int -> nil, 'v) is_trie
  | Nested_trie : ('s, 'b, 'v) is_trie -> ('s Int.Map.t, int -> 'b, 'v) is_trie

type ('k, 'v) is_any_trie =
  | Is_trie : ('t, 'k, 'v) is_trie -> ('k, 'v) is_any_trie

let patricia_tree_is_trie = Map_is_trie

let patricia_tree_of_trie is_trie = Nested_trie is_trie

let empty : type t k v. (t, k, v) is_trie -> t = function
  | Map_is_trie -> Int.Map.empty
  | Nested_trie _ -> Int.Map.empty

let is_empty : type t k v. (t, k, v) is_trie -> t -> bool = function
  | Map_is_trie -> Int.Map.is_empty
  | Nested_trie _ -> Int.Map.is_empty

let rec find0 :
    type t k r v. (t, k -> r, v) is_trie -> k -> r Constant.hlist -> t -> v =
 fun w k ks t ->
  match ks, w with
  | [], Nested_trie _ -> .
  | [], Map_is_trie -> Int.Map.find k t
  | k' :: ks', Nested_trie w' -> find0 w' k' ks' (Int.Map.find k t)

let find : type t k v. (t, k, v) is_trie -> k Constant.hlist -> t -> v =
 fun w k t -> match k, w with [], _ -> . | k :: ks, _ -> find0 w k ks t

let find_opt w k t =
  match find w k t with exception Not_found -> None | datum -> Some datum

let rec singleton0 :
    type t k r v. (t, k -> r, v) is_trie -> k -> r Constant.hlist -> v -> t =
 fun w k ks v ->
  match ks, w with
  | [], Nested_trie _ -> .
  | [], Map_is_trie -> Int.Map.singleton k v
  | k' :: ks', Nested_trie w' -> Int.Map.singleton k (singleton0 w' k' ks' v)

let singleton : type t k v. (t, k, v) is_trie -> k Constant.hlist -> v -> t =
 fun w k v -> match k, w with [], _ -> . | k :: ks, _ -> singleton0 w k ks v

let rec add0 :
    type t k r v. (t, k -> r, v) is_trie -> k -> r Constant.hlist -> v -> t -> t
    =
 fun w k ks v t ->
  match ks, w with
  | [], Nested_trie _ -> .
  | [], Map_is_trie -> Int.Map.add k v t
  | k' :: ks', Nested_trie w' -> (
    match Int.Map.find_opt k t with
    | Some m -> Int.Map.add k (add0 w' k' ks' v m) t
    | None -> Int.Map.add k (singleton0 w' k' ks' v) t)

let add_or_replace :
    type t k v. (t, k, v) is_trie -> k Constant.hlist -> v -> t -> t =
 fun w k v t -> match k, w with [], _ -> . | k :: ks, _ -> add0 w k ks v t

let rec remove0 :
    type t k r v. (t, k -> r, v) is_trie -> k -> r Constant.hlist -> t -> t =
 fun w k ks t ->
  match ks, w with
  | [], Nested_trie _ -> .
  | [], Map_is_trie -> Int.Map.remove k t
  | k' :: ks', Nested_trie w' -> (
    match Int.Map.find_opt k t with
    | None -> t
    | Some m ->
      let m' = remove0 w' k' ks' m in
      if is_empty w' m' then Int.Map.remove k t else Int.Map.add k m' t)

let remove : type t k v. (t, k, v) is_trie -> k Constant.hlist -> t -> t =
 fun w k t -> match k, w with [], _ -> . | k :: ks, _ -> remove0 w k ks t

let rec union :
    type t k v. (t, k, v) is_trie -> (v -> v -> v option) -> t -> t -> t =
 fun w f t1 t2 ->
  match w with
  | Map_is_trie -> Int.Map.union (fun _ left right -> f left right) t1 t2
  | Nested_trie w' ->
    Int.Map.union
      (fun _ left right ->
        let s = union w' f left right in
        if is_empty w' s then None else Some s)
      t1 t2

module Iterator = struct
  include Leapfrog.Map (Int)

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let create_iterator = create

  let rec create :
      type m k v.
      (m, k, v) is_trie -> m Channel.receiver -> v Channel.sender -> k hlist =
   fun is_trie this_ref value_handler ->
    match is_trie with
    | Map_is_trie -> [create_iterator this_ref value_handler]
    | Nested_trie next_trie ->
      let send_next, recv_next = Channel.create (empty next_trie) in
      create_iterator this_ref send_next
      :: create next_trie recv_next value_handler

  let create is_trie this_ref value_handler =
    create is_trie this_ref value_handler
end
