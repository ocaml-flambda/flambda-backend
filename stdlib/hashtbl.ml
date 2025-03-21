# 2 "hashtbl.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

(* Hash tables *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when the load factor becomes too high. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    seed: int;                        (* for randomization *)
    mutable initial_size: int;                (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of { mutable key: 'a;
              mutable data: 'b;
              mutable next: ('a, 'b) bucketlist }

(* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in place implementation of resizing.
*)

let ongoing_traversal h =
  Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
  || h.initial_size < 0

let flip_ongoing_traversal h =
  h.initial_size <- - h.initial_size

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = Atomic.make randomized_default

let randomize () = Atomic.Contended.set randomized true
let is_randomized () = Atomic.Contended.get randomized

module DLS = Domain.Safe.DLS

let prng_key = DLS.new_key Random.State.make_self_init

(* Functions which appear before the functorial interface must either be
   independent of the hash function or take it as a parameter (see #2202 and
   code below the functor definitions. *)

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = is_randomized ()) initial_size =
  let s = power_2_above 16 initial_size in
  let seed =
    if random
    then DLS.access (fun access -> Random.State.bits (DLS.get access prng_key))
    else 0
  in
  { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

let clear h =
  if h.size > 0 then begin
    h.size <- 0;
    Array.fill h.data 0 (Array.length h.data) Empty
  end

let reset h =
  let len = Array.length h.data in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = abs h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {key; data; next} ->
      let rec loop prec = function
        | Empty -> ()
        | Cons {key; data; next} ->
            let r = Cons {key; data; next} in
            begin match prec with
            | Empty -> assert false
            | Cons prec ->  prec.next <- r
            end;
            loop r next
      in
      let r = Cons {key; data; next} in
      loop r next;
      r

let copy h = { h with data = Array.map copy_bucketlist h.data }

let length h = h.size

let insert_all_buckets indexfun inplace odata ndata =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons {key; data; next} as cell ->
        let cell =
          if inplace then cell
          else Cons {key; data; next = Empty}
        in
        let nidx = indexfun key in
        begin match ndata_tail.(nidx) with
        | Empty -> ndata.(nidx) <- cell;
        | Cons tail -> tail.next <- cell;
        end;
        ndata_tail.(nidx) <- cell;
        insert_bucket next
  in
  for i = 0 to Array.length odata - 1 do
    insert_bucket odata.(i)
  done;
  if inplace then
    for i = 0 to nsize - 1 do
      match ndata_tail.(i) with
      | Empty -> ()
      | Cons tail -> tail.next <- Empty
    done

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    insert_all_buckets (indexfun h) inplace odata ndata
  end

let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons{key; data; next} ->
        f key data; do_bucket next in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h;
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_map_inplace_bucket f h i prec = function
  | Empty ->
      begin match prec with
      | Empty -> h.data.(i) <- Empty
      | Cons c -> c.next <- Empty
      end
  | (Cons ({key; data; next} as c)) as slot ->
      begin match f key data with
      | None ->
          h.size <- h.size - 1;
          filter_map_inplace_bucket f h i prec next
      | Some data ->
          begin match prec with
          | Empty -> h.data.(i) <- slot
          | Cons c -> c.next <- slot
          end;
          c.data <- data;
          filter_map_inplace_bucket f h i slot next
      end

let filter_map_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons{key; data; next} ->
        do_bucket next (f key data accu) in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons{next} -> bucket_length (accu + 1) next

let stats h =
  let mbl =
    Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }

(** {1 Iterators} *)

let to_seq tbl =
  (* capture current array, so that even if the table is resized we
     keep iterating on the same array *)
  let tbl_data = tbl.data in
  (* state: index * next bucket to traverse *)
  let rec aux i buck () = match buck with
    | Empty ->
        if i = Array.length tbl_data
        then Seq.Nil
        else aux(i+1) tbl_data.(i) ()
    | Cons {key; data; next} ->
        Seq.Cons ((key, data), aux i next)
  in
  aux 0 Empty

let to_seq_keys m = Seq.map fst (to_seq m)

let to_seq_values m = Seq.map snd (to_seq m)

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val seeded_hash: int -> t -> int
  end

module type S =
  sig
    type key
    type !'a t
    val create: int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
    val stats: 'a t -> statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module type SeededS =
  sig
    type key
    type !'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.seeded_hash h.seed key) land (Array.length h.data - 1)

    let add h key data =
      let i = key_index h key in
      let bucket = Cons{key; data; next=h.data.(i)} in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let rec remove_bucket h i key prec = function
      | Empty ->
          ()
      | (Cons {key=k; next}) as c ->
          if H.equal k key
          then begin
            h.size <- h.size - 1;
            match prec with
            | Empty -> h.data.(i) <- next
            | Cons c -> c.next <- next
          end
          else remove_bucket h i key c next

    let remove h key =
      let i = key_index h key in
      remove_bucket h i key Empty h.data.(i)

    let rec find_rec key = function
      | Empty ->
          raise Not_found
      | Cons{key=k; data; next} ->
          if H.equal key k then data else find_rec key next

    let find h key =
      match h.data.(key_index h key) with
      | Empty -> raise Not_found
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then d1 else
          match next1 with
          | Empty -> raise Not_found
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then d2 else
              match next2 with
              | Empty -> raise Not_found
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then d3 else find_rec key next3

    let rec find_rec_opt key = function
      | Empty ->
          None
      | Cons{key=k; data; next} ->
          if H.equal key k then Some data else find_rec_opt key next

    let find_opt h key =
      match h.data.(key_index h key) with
      | Empty -> None
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then Some d1 else
          match next1 with
          | Empty -> None
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then Some d2 else
              match next2 with
              | Empty -> None
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then Some d3 else find_rec_opt key next3

    let find_all h key =
      let[@tail_mod_cons] rec find_in_bucket = function
      | Empty ->
          []
      | Cons{key=k; data=d; next} ->
          if H.equal k key
          then d :: find_in_bucket next
          else find_in_bucket next in
      find_in_bucket h.data.(key_index h key)

    let rec replace_bucket key data = function
      | Empty ->
          true
      | Cons ({key=k; next} as slot) ->
          if H.equal k key
          then (slot.key <- key; slot.data <- data; false)
          else replace_bucket key data next

    let replace h key data =
      let i = key_index h key in
      let l = h.data.(i) in
      if replace_bucket key data l then begin
        h.data.(i) <- Cons{key; data; next=l};
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      end

    let rec mem_in_bucket key = function
      | Empty ->
          false
      | Cons{key=k; next} ->
          H.equal k key || mem_in_bucket key next

    let mem h key =
      mem_in_bucket key h.data.(key_index h key)

    let add_seq tbl i =
      Seq.iter (fun (k,v) -> add tbl k v) i

    let replace_seq tbl i =
      Seq.iter (fun (k,v) -> replace tbl k v) i

    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl

    let iter = iter
    let filter_map_inplace = filter_map_inplace
    let fold = fold
    let length = length
    let stats = stats
    let to_seq = to_seq
    let to_seq_keys = to_seq_keys
    let to_seq_values = to_seq_values
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let seeded_hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end

module MakeSeededPortable(H: sig @@ portable include SeededHashedType end)
  : sig @@ portable include SeededS with type key = H.t end =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.seeded_hash h.seed key) land (Array.length h.data - 1)

    let add h key data =
      let i = key_index h key in
      let bucket = Cons{key; data; next=h.data.(i)} in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let rec remove_bucket h i key prec = function
      | Empty ->
          ()
      | (Cons {key=k; next}) as c ->
          if H.equal k key
          then begin
            h.size <- h.size - 1;
            match prec with
            | Empty -> h.data.(i) <- next
            | Cons c -> c.next <- next
          end
          else remove_bucket h i key c next

    let remove h key =
      let i = key_index h key in
      remove_bucket h i key Empty h.data.(i)

    let rec find_rec key = function
      | Empty ->
          raise Not_found
      | Cons{key=k; data; next} ->
          if H.equal key k then data else find_rec key next

    let find h key =
      match h.data.(key_index h key) with
      | Empty -> raise Not_found
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then d1 else
          match next1 with
          | Empty -> raise Not_found
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then d2 else
              match next2 with
              | Empty -> raise Not_found
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then d3 else find_rec key next3

    let rec find_rec_opt key = function
      | Empty ->
          None
      | Cons{key=k; data; next} ->
          if H.equal key k then Some data else find_rec_opt key next

    let find_opt h key =
      match h.data.(key_index h key) with
      | Empty -> None
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then Some d1 else
          match next1 with
          | Empty -> None
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then Some d2 else
              match next2 with
              | Empty -> None
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then Some d3 else find_rec_opt key next3

    let find_all h key =
      let[@tail_mod_cons] rec find_in_bucket = function
      | Empty ->
          []
      | Cons{key=k; data=d; next} ->
          if H.equal k key
          then d :: find_in_bucket next
          else find_in_bucket next in
      find_in_bucket h.data.(key_index h key)

    let rec replace_bucket key data = function
      | Empty ->
          true
      | Cons ({key=k; next} as slot) ->
          if H.equal k key
          then (slot.key <- key; slot.data <- data; false)
          else replace_bucket key data next

    let replace h key data =
      let i = key_index h key in
      let l = h.data.(i) in
      if replace_bucket key data l then begin
        h.data.(i) <- Cons{key; data; next=l};
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      end

    let rec mem_in_bucket key = function
      | Empty ->
          false
      | Cons{key=k; next} ->
          H.equal k key || mem_in_bucket key next

    let mem h key =
      mem_in_bucket key h.data.(key_index h key)

    let add_seq tbl i =
      Seq.iter (fun (k,v) -> add tbl k v) i

    let replace_seq tbl i =
      Seq.iter (fun (k,v) -> replace tbl k v) i

    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl

    let iter = iter
    let filter_map_inplace = filter_map_inplace
    let fold = fold
    let length = length
    let stats = stats
    let to_seq = to_seq
    let to_seq_keys = to_seq_keys
    let to_seq_values = to_seq_values
  end

module MakePortable(H: sig @@ portable include HashedType end)
  : sig @@ portable include S with type key = H.t end =
  struct
    include MakeSeededPortable(struct
        type t = H.t
        let equal = H.equal
        let seeded_hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end

(* Polymorphic hash function-based tables *)
(* Code included below the functorial interface to guard against accidental
   use - see #2202 *)

external seeded_hash_param :
  int -> int -> int -> 'a -> int @@ portable = "caml_hash_exn"

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

let key_index h key =
  if Obj.size (Obj.repr h) >= 4
  then (seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)
  else invalid_arg "Hashtbl: unsupported hash table format"

let add h key data =
  let i = key_index h key in
  let bucket = Cons{key; data; next=h.data.(i)} in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h

let rec remove_bucket h i key prec = function
  | Empty ->
      ()
  | (Cons {key=k; next}) as c ->
      if compare k key = 0
      then begin
        h.size <- h.size - 1;
        match prec with
        | Empty -> h.data.(i) <- next
        | Cons c -> c.next <- next
      end
      else remove_bucket h i key c next

let remove h key =
  let i = key_index h key in
  remove_bucket h i key Empty h.data.(i)

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons{key=k; data; next} ->
      if compare key k = 0 then data else find_rec key next

let find h key =
  match h.data.(key_index h key) with
  | Empty -> raise Not_found
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then d1 else
      match next1 with
      | Empty -> raise Not_found
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then d2 else
          match next2 with
          | Empty -> raise Not_found
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then d3 else find_rec key next3

let rec find_rec_opt key = function
  | Empty ->
      None
  | Cons{key=k; data; next} ->
      if compare key k = 0 then Some data else find_rec_opt key next

let find_opt h key =
  match h.data.(key_index h key) with
  | Empty -> None
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then Some d1 else
      match next1 with
      | Empty -> None
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then Some d2 else
          match next2 with
          | Empty -> None
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then Some d3 else find_rec_opt key next3

let find_all h key =
  let[@tail_mod_cons] rec find_in_bucket = function
  | Empty ->
      []
  | Cons{key=k; data; next} ->
      if compare k key = 0
      then data :: find_in_bucket next
      else find_in_bucket next in
  find_in_bucket h.data.(key_index h key)

let rec replace_bucket key data = function
  | Empty ->
      true
  | Cons ({key=k; next} as slot) ->
      if compare k key = 0
      then (slot.key <- key; slot.data <- data; false)
      else replace_bucket key data next

let replace h key data =
  let i = key_index h key in
  let l = h.data.(i) in
  if replace_bucket key data l then begin
    h.data.(i) <- Cons{key; data; next=l};
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h
  end

let rec mem_in_bucket key = function
  | Empty ->
      false
  | Cons{key=k; next} ->
      compare k key = 0 || mem_in_bucket key next

let mem h key =
  mem_in_bucket key h.data.(key_index h key)

let add_seq tbl i =
  Seq.iter (fun (k,v) -> add tbl k v) i

let replace_seq tbl i =
  Seq.iter (fun (k,v) -> replace tbl k v) i

let of_seq i =
  let tbl = create 16 in
  replace_seq tbl i;
  tbl

let rebuild ?(random = is_randomized ()) h =
  let s = power_2_above 16 (Array.length h.data) in
  let seed =
    if random
    then DLS.access (fun access -> Random.State.bits (DLS.get access prng_key))
    else if Obj.size (Obj.repr h) >= 4 then h.seed
    else 0 in
  let h' = {
    size = h.size;
    data = Array.make s Empty;
    seed = seed;
    initial_size = if Obj.size (Obj.repr h) >= 4 then h.initial_size else s
  } in
  insert_all_buckets (key_index h') false h.data h'.data;
  h'
