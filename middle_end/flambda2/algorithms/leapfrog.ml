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

module type Iterator = sig
  type 'a t

  val current : 'a t -> 'a option

  val advance : 'a t -> unit

  val seek : 'a t -> 'a -> unit

  val init : 'a t -> unit

  val accept : 'a t -> unit

  val equal_key : 'a t -> 'a -> 'a -> bool

  val compare_key : 'a t -> 'a -> 'a -> int
end

module Map (T : Container_types.S_plus_iterator) = struct
  type _ t =
    | Iterator :
        { mutable iterator : 'v T.Map.iterator;
          map : 'v T.Map.t Channel.receiver;
          handler : 'v Channel.sender
        }
        -> T.t t

  let equal_key (type a) (Iterator _ : a t) : a -> a -> bool = T.equal

  let compare_key (type a) (Iterator _ : a t) : a -> a -> int = T.compare

  let current (type a) (Iterator i : a t) : a option =
    match T.Map.current i.iterator with
    | Some (key, _) -> Some key
    | None -> None

  let advance (type a) (Iterator i : a t) : unit =
    i.iterator <- T.Map.advance i.iterator

  let seek (type a) (Iterator i : a t) (k : a) : unit =
    i.iterator <- T.Map.seek i.iterator k

  let init (type a) (Iterator i : a t) : unit =
    i.iterator <- T.Map.iterator (Channel.recv i.map)

  let accept (type a) (Iterator i : a t) : unit =
    match T.Map.current i.iterator with
    | None -> invalid_arg "accept: iterator is exhausted"
    | Some (_, value) -> Channel.send i.handler value

  let create cell handler =
    Iterator { iterator = T.Map.iterator T.Map.empty; map = cell; handler }
end

module Join (Iterator : Iterator) : sig
  include Iterator

  val create : 'a Iterator.t list -> 'a t
end = struct
  type 'k t =
    { iterators : 'k Iterator.t array;
      mutable at_end : bool
    }

  let current (type a) ({ iterators; at_end } : a t) : a option =
    if at_end
    then None
    else Iterator.current iterators.(Array.length iterators - 1)

  let rec search : type a. a Iterator.t array -> int -> a -> a option =
   fun iterators index_of_lowest_key highest_key ->
    let iterator_with_lowest_key = iterators.(index_of_lowest_key) in
    let equal = Iterator.equal_key iterator_with_lowest_key in
    match Iterator.current iterator_with_lowest_key with
    | None -> None
    | Some lowest_key when equal lowest_key highest_key ->
      (* All iterators are on the same key. *)
      Some lowest_key
    | Some _ -> (
      Iterator.seek iterator_with_lowest_key highest_key;
      match Iterator.current iterator_with_lowest_key with
      | None -> None
      | Some new_highest_key ->
        search iterators
          ((index_of_lowest_key + 1) mod Array.length iterators)
          new_highest_key)

  let repair (type a) ({ iterators; at_end } as j : a t) =
    assert (not at_end);
    if Array.length iterators > 1
    then
      let iterator = iterators.(Array.length iterators - 1) in
      match Iterator.current iterator with
      | None -> j.at_end <- true
      | Some highest_key -> (
        match search iterators 0 highest_key with
        | None -> j.at_end <- true
        | Some _ -> ())

  let advance (type a) ({ iterators; at_end } as t : a t) =
    if not at_end
    then (
      let highest_iterator = iterators.(Array.length iterators - 1) in
      Iterator.advance highest_iterator;
      repair t)

  let seek (type a) (t : a t) (key : a) =
    let { iterators; at_end } = t in
    if not at_end
    then (
      let highest_iterator = iterators.(Array.length iterators - 1) in
      Iterator.seek highest_iterator key;
      repair t)

  exception Empty_iterator

  let init (type a) ({ iterators; _ } as j : a t) =
    try
      Array.iter
        (fun (it : a Iterator.t) ->
          Iterator.init it;
          match Iterator.current it with
          | None -> raise Empty_iterator
          | Some _ -> ())
        iterators;
      Array.sort
        (fun (it1 : a Iterator.t) (it2 : a Iterator.t) ->
          let compare = Iterator.compare_key it1 in
          Option.compare compare (Iterator.current it1) (Iterator.current it2))
        iterators;
      j.at_end <- false;
      repair j
    with Empty_iterator -> j.at_end <- true

  let accept (type a) ({ iterators; at_end } : a t) =
    if at_end then invalid_arg "Joined_iterator.accept: iterator is exhausted";
    Array.iter Iterator.accept iterators

  let equal_key { iterators; _ } = Iterator.equal_key iterators.(0)

  let compare_key { iterators; _ } = Iterator.compare_key iterators.(0)

  let create (iterators : _ Iterator.t list) : _ t =
    match iterators with
    | [] -> invalid_arg "Joined_iterator.create: cannot join an empty list"
    | _ -> { iterators = Array.of_list iterators; at_end = false }
end
[@@inline always]
