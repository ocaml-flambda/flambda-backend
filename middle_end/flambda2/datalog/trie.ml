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
  type _ t =
    | Iterator :
        { mutable iterator : 'v Int.Map.iterator;
          map : 'v Int.Map.t Named_ref.t;
          handler : 'v Named_ref.t
        }
        -> int t

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let print_name (type a) ff (Iterator i : a t) = Named_ref.pp_name ff i.map

  let equal_key (type a) (Iterator _ : a t) : a -> a -> bool = Int.equal

  let compare_key (type a) (Iterator _ : a t) : a -> a -> int = Int.compare

  let current (type a) (Iterator i : a t) : a option =
    match Int.Map.current i.iterator with
    | Some (key, _) -> Some key
    | None -> None

  let advance (type a) (Iterator i : a t) : unit =
    i.iterator <- Int.Map.advance i.iterator

  let seek (type a) (Iterator i : a t) (k : a) : unit =
    i.iterator <- Int.Map.seek i.iterator k

  let init (type a) (Iterator i : a t) : unit =
    i.iterator <- Int.Map.iterator i.map.contents

  let accept (type a) (Iterator i : a t) : unit =
    match Int.Map.current i.iterator with
    | None -> invalid_arg "accept: iterator is exhausted"
    | Some (_, value) -> i.handler.contents <- value

  let create_iterator cell handler =
    Iterator { iterator = Int.Map.iterator Int.Map.empty; map = cell; handler }

  let rec create :
      type m k v.
      (m, k, v) is_trie ->
      int ->
      string ->
      m Named_ref.t ->
      v Named_ref.t ->
      k hlist =
   fun is_trie i name this_ref value_handler ->
    match is_trie with
    | Map_is_trie -> [create_iterator this_ref value_handler]
    | Nested_trie next_trie ->
      let next_ref : _ Named_ref.t =
        { contents = empty next_trie;
          printed_name = name ^ "." ^ string_of_int i
        }
      in
      create_iterator this_ref next_ref
      :: create next_trie (i + 1) name next_ref value_handler

  let create is_trie name this_ref value_handler =
    create is_trie 1 name this_ref value_handler
end
