(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

(* CR mshinwell: Add a [compare] value to the functor argument and use it
   to sort the results from [elements], [bindings] etc? *)

(* The following is a "little endian" implementation. *)

(* CR mshinwell: Can we fix the traversal order by swapping endianness?
   What other (dis)advantages might that have? *)

let zero_bit i bit =
  i land bit = 0

let lowest_bit x =
  x land (-x)

let branching_bit prefix0 prefix1 =
  lowest_bit (prefix0 lxor prefix1)

let mask i bit =
  i land (bit - 1)

let match_prefix i prefix bit =
  mask i bit = prefix

let equal_prefix prefix0 bit0 prefix1 bit1 =
  bit0 = bit1 && prefix0 = prefix1

let shorter bit0 bit1 =
  match bit0 < 0, bit1 < 0 with
  | false, false -> bit0 < bit1
  | true, false
  | false, true -> bit0 > bit1
  | true, true -> assert false

let includes_prefix prefix0 bit0 prefix1 bit1 =
  shorter bit0 bit1
  && match_prefix prefix1 prefix0 bit0

(*
let includes_prefix prefix0 bit0 prefix1 bit1 =
  (bit0 - 1) < (bit1 - 1)
  && match_prefix prefix1 prefix0 bit0
*)

let compare_prefix prefix0 bit0 prefix1 bit1 =
  let c = compare bit0 bit1 in
  if c = 0 then compare prefix0 prefix1
  else c

module Make_set (Elt : sig
  val print : Format.formatter -> int -> unit
end) = struct
  type elt = int

  type t =
    | Empty
    | Leaf of int
    | Branch of int * int * t * t

  let empty = Empty

  let is_empty t =
    match t with
    | Empty -> true
    | Leaf _ -> false
    | Branch _ -> false

  let singleton i = Leaf i

  let rec mem i = function
    | Empty -> false
    | Leaf j -> j = i
    | Branch(prefix, bit, t0, t1) ->
        if not (match_prefix i prefix bit) then false
        else if zero_bit i bit then mem i t0
        else mem i t1

  let branch prefix bit t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | t0, t1 -> Branch(prefix, bit, t0, t1)

  let join prefix0 t0 prefix1 t1 =
    let bit = branching_bit prefix0 prefix1 in
    if zero_bit prefix0 bit then
      Branch(mask prefix0 bit, bit, t0, t1)
    else
      Branch(mask prefix0 bit, bit, t1, t0)

  (* CR mshinwell: This is now [add_or_replace] *)
  let rec add i = function
    | Empty -> Leaf i
    | Leaf j as t ->
      if i = j then Leaf i
      else join i (Leaf i) j t
    | Branch(prefix, bit, t0, t1) as t ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          Branch(prefix, bit, add i t0, t1)
        else
          Branch(prefix, bit, t0, add i t1)
      else
        join i (Leaf i) prefix t

  let rec remove i = function
    | Empty -> Empty
    | Leaf j as t ->
      if i = j then Empty
      else t
    | Branch (prefix, bit, t0, t1) as t ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (remove i t0) t1
        else
          branch prefix bit t0 (remove i t1)
      else
        t

  let rec union t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Leaf i, Leaf j when i = j -> Leaf i
    | Leaf i, Leaf j -> join i (Leaf i) j t1
    | Leaf i, Branch (prefix, bit, t10, t11) ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (union t0 t10) t11
        else
          branch prefix bit t10 (union t0 t11)
      else
        join i (Leaf i) prefix t1
    | Branch (prefix, bit, t00, t01), Leaf i ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (union t1 t00) t01
        else
          branch prefix bit t00 (union t1 t01)
      else
        join i (Leaf i) prefix t0
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        branch prefix0 bit0 (union t00 t10) (union t01 t11)
      else if includes_prefix prefix0 bit0 prefix1 bit1 then
        if zero_bit prefix1 bit0 then
          branch prefix0 bit0 (union t00 t1) t01
        else
          branch prefix0 bit0 t00 (union t01 t1)
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          branch prefix1 bit1 (union t0 t10) t11
        else
          branch prefix1 bit1 t10 (union t0 t11)
      else
        join prefix0 t0 prefix1 t1

  let rec subset t0 t1 =
    match t0, t1 with
    | Empty, _ -> true
    | _, Empty -> false
    | Branch _, Leaf _ -> false
    | Leaf i, _ -> mem i t1
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          subset t00 t10 && subset t01 t11
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            subset t0 t10
          else
            subset t0 t11
        else
          false

  let rec intersection_is_empty t0 t1 =
    match t0, t1 with
    | Empty, _ -> true
    | _, Empty -> true
    | Leaf i, _ -> not (mem i t1)
    | _, Leaf i -> not (mem i t0)
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          intersection_is_empty t00 t10 && intersection_is_empty t01 t11
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            intersection_is_empty t00 t1
          else
            intersection_is_empty t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            intersection_is_empty t0 t10
          else
            intersection_is_empty t0 t11
        else
          true

  let rec inter t0 t1 =
    match t0, t1 with
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Leaf i, _ -> if mem i t1 then t0 else Empty
    | _, Leaf i -> if mem i t0 then t1 else Empty
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          branch prefix0 bit0 (inter t00 t10) (inter t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            inter t00 t1
          else
            inter t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            inter t0 t10
          else
            inter t0 t11
        else
          Empty

  let rec diff t0 t1 =
    match t0, t1 with
    | Empty, _ -> Empty
    | _, Empty -> t0
    | Leaf i, _ -> if mem i t1 then Empty else t0
    | _, Leaf i -> remove i t0
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          branch prefix0 bit0 (diff t00 t10) (diff t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            branch prefix0 bit0 (diff t00 t1) t01
          else
            branch prefix0 bit0 t00 (diff t01 t1)
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            diff t0 t10
          else
            diff t0 t11
        else
          t0

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch(_, _, t0, t1) -> cardinal t0 + cardinal t1

  let rec iter f = function
    | Empty -> ()
    | Leaf elt -> f elt
    | Branch(_, _, t0, t1) -> iter f t0; iter f t1

  let rec fold f t acc =
    match t with
    | Empty -> acc
    | Leaf elt -> f elt acc
    | Branch(_, _, t0, t1) -> fold f t0 (fold f t1 acc)

  let rec for_all p = function
    | Empty -> true
    | Leaf elt -> p elt
    | Branch(_, _, t0, t1) -> for_all p t0 && for_all p t1

  let rec exists p = function
    | Empty -> false
    | Leaf elt -> p elt
    | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

  let filter p t =
    let rec loop p acc = function
      | Empty -> acc
      | Leaf i -> if p i then add i acc else acc
      | Branch(_, _, t0, t1) -> loop p (loop p acc t0) t1
    in
    loop p Empty t

  let filter_map f t =
    let rec loop f acc = function
      | Empty -> acc
      | Leaf i ->
        begin match f i with
        | None -> acc
        | Some j -> add j acc
        end
      | Branch(_, _, t0, t1) -> loop f (loop f acc t0) t1
    in
    loop f Empty t

  let partition p t =
    let rec loop ((true_, false_) as acc) = function
      | Empty -> acc
      | Leaf i ->
        if p i then (add i true_, false_)
        else (true_, add i false_)
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop (Empty, Empty) t

  let rec choose = function
    | Empty -> raise Not_found
    | Leaf key -> key
    | Branch(_, _, t0, _) -> choose t0

  let choose_opt t =
    match choose t with
    | exception Not_found -> None
    | choice -> Some choice

  let elements t =
    let rec loop acc = function
      | Empty -> acc
      | Leaf i -> i :: acc
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop [] t

  let min_elt t =
    let rec loop = function
      | Empty -> raise Not_found
      | Leaf i -> i
      | Branch(_, _, t0, t1) ->
        let i0 = loop t0 in
        let i1 = loop t1 in
          if i0 < i1 then i0
          else i1
    in
    loop t

  let min_elt_opt t =
    match min_elt t with
    | exception Not_found -> None
    | min -> Some min

  let max_elt t =
    let rec loop = function
      | Empty -> raise Not_found
      | Leaf i -> i
      | Branch(_, _, t0, t1) ->
        let i0 = loop t0 in
        let i1 = loop t1 in
          if i0 > i1 then i0
          else i1
    in
    loop t

  let max_elt_opt t =
    match max_elt t with
    | exception Not_found -> None
    | max -> Some max

  let rec equal t0 t1 =
    if t0 == t1 then true
    else match t0, t1 with
    | Empty, Empty -> true
    | Leaf i, Leaf j -> i = j
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        equal t00 t10 && equal t01 t11
      else false
    | _, _ -> false

  let rec compare t0 t1 =
    match t0, t1 with
    | Empty, Empty -> 0
    | Leaf i, Leaf j ->
      if i = j then 0
      else if i < j then -1
      else 1
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      let c = compare_prefix prefix0 bit0 prefix1 bit1 in
      if c = 0 then
        let c = compare t00 t10 in
        if c = 0 then compare t01 t11
        else c
      else c
    | Empty, Leaf _ -> 1
    | Empty, Branch _ -> 1
    | Leaf _, Branch _ -> 1
    | Leaf _, Empty -> -1
    | Branch _, Empty -> -1
    | Branch _, Leaf _ -> -1

  let split i t =
    let rec loop ((lt, present, gt) as acc) = function
      | Empty -> acc
      | Leaf j ->
          if i = j then (lt, true, gt)
          else if j < i then (add j lt, present, gt)
          else (lt, present, add j gt)
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop (Empty, false, Empty) t

  let find_opt _ _ = Misc.fatal_error "find_opt not yet implemented"

  let find_first _ _ = Misc.fatal_error "find_first not yet implemented"

  let find_first_opt _ _ = Misc.fatal_error "find_first_opt not yet implemented"

  let find_last _ _ = Misc.fatal_error "find_last not yet implemented"

  let find_last_opt _ _ = Misc.fatal_error "find_last_opt not yet implemented"

  let get_singleton t =
    match t with
    | Empty | Branch _ -> None
    | Leaf elt -> Some elt

  let to_seq t =
    let rec aux acc () =
      match acc with
      | [] -> Seq.Nil
      | t0 :: r ->
        begin match t0 with
        | Empty -> aux r ()
        | Leaf x -> Seq.Cons (x, aux r)
        | Branch (_, _, t1, t2) -> aux (t1 :: t2 :: r) ()
        end
    in
    aux [t]

  let to_rev_seq _ = Misc.fatal_error "to_rev_seq not yet implemented"

  let to_seq_from _ _ = Misc.fatal_error "to_seq_from not yet implemented"

  let add_seq _ _ = Misc.fatal_error "add_seq not yet implemented"

  let of_seq _ = Misc.fatal_error "of_seq not yet implemented"

  let fixpoint _ _ = Misc.fatal_error "fixpoint not yet implemented"

  (* CR mshinwell: copied from [Container_types] *)

  let output _ _ = Misc.fatal_error "output not yet implemented"

  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" Elt.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let find elt t =
    if mem elt t then elt
    else raise Not_found

  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f t = fold (fun elt acc -> add (f elt) acc) t empty

  let rec union_list ts =
    match ts with
    | [] -> empty
    | t::ts -> union t (union_list ts)

  let disjoint _ _ = Misc.fatal_error "disjoint not yet implemented"
end [@@@inline always]

module Make_map (Key : sig
  val print : Format.formatter -> int -> unit
end) (Set : Container_types.Set with module T := Numeric_types.Int) =
struct
  type key = int

  module Set = Set

  module T : sig
    type +'a t = private
      | Empty
      | Leaf of int * 'a
      | Branch of int * int * 'a t * 'a t

    val empty : 'a t
    val leaf : int -> 'a -> 'a t
    val branch : int -> int -> 'a t -> 'a t -> 'a t
    val branch_non_empty : int -> int -> 'a t -> 'a t -> 'a t
  end = struct
    type 'a t =
      | Empty
      | Leaf of int * 'a
      | Branch of int * int * 'a t * 'a t

    let empty = Empty
    let leaf key datum = Leaf (key, datum) [@@inline always]

    let branch prefix bit t0 t1 =
      match t0, t1 with
      | Empty, _ -> t1
      | _, Empty -> t0
      | _, _ -> Branch (prefix, bit, t0, t1)
    [@@inline always]

    let branch_non_empty prefix bit t0 t1 =
      Branch (prefix, bit, t0, t1)
    [@@inline always]
  end

  include T

  let is_empty t =
    match t with
    | Empty -> true
    | Leaf _ -> false
    | Branch _ -> false

  let print_debug print_datum ppf t =
    let rec pp ppf t =
      match t with
      | Empty -> Format.pp_print_string ppf "()"
      | Leaf (k, v) ->
        Format.fprintf ppf "@[<hv 1>(%x@ %a)@]"
          k print_datum v
      | Branch (k1, k2, l, r) ->
        Format.fprintf ppf "@[<hv 1>(branch@ %x@ %x@ %a@ %a)@]"
          k1 k2 pp l pp r
    in
    pp ppf t

  let singleton i d = leaf i d

  let zero_bit i bit =
    i land bit = 0

  let lowest_bit x =
    x land (-x)

  let branching_bit prefix0 prefix1 =
    lowest_bit (prefix0 lxor prefix1)

  let mask i bit =
    i land (bit - 1)

  let match_prefix i prefix bit =
    mask i bit = prefix

  let equal_prefix prefix0 bit0 prefix1 bit1 =
    bit0 = bit1 && prefix0 = prefix1

  let includes_prefix prefix0 bit0 prefix1 bit1 =
    (bit0 - 1) < (bit1 - 1)
    && match_prefix prefix1 prefix0 bit0

  let compare_prefix prefix0 bit0 prefix1 bit1 =
    let c = compare bit0 bit1 in
    if c = 0 then compare prefix0 prefix1
    else c

  let rec mem i = function
    | Empty -> false
    | Leaf(j, _) -> j = i
    | Branch(prefix, bit, t0, t1) ->
        if not (match_prefix i prefix bit) then false
        else if zero_bit i bit then mem i t0
        else mem i t1

  let join prefix0 t0 prefix1 t1 =
    let bit = branching_bit prefix0 prefix1 in
    if zero_bit prefix0 bit then
      branch (mask prefix0 bit) bit t0 t1
    else
      branch (mask prefix0 bit) bit t1 t0

  (* CR mshinwell: This is now [add_or_replace], like [Map] *)
  let rec add i d = function
    | Empty -> leaf i d
    | Leaf(j, _) as t ->
      if i = j then leaf i d
      else join i (leaf i d) j t
    | Branch(prefix, bit, t0, t1) as t ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch_non_empty prefix bit (add i d t0) t1
        else
          branch_non_empty prefix bit t0 (add i d t1)
      else
        join i (leaf i d) prefix t

  let rec replace key f = function
    | Empty -> empty
    | Leaf (key', datum) as t ->
      if key = key' then
        let datum = f datum in
        leaf key datum
      else
        t
    | Branch (prefix, bit, t0, t1) as t ->
      if match_prefix key prefix bit then
        if zero_bit key bit then
          branch_non_empty prefix bit (replace key f t0) t1
        else
          branch_non_empty prefix bit t0 (replace key f t1)
      else
        t

  let rec update key f = function
    | Empty ->
      begin match f None with
      | None -> empty
      | Some datum -> leaf key datum
      end
    | Leaf (key', datum) as t ->
      if key = key' then
        begin match f (Some datum) with
        | None -> empty
        | Some datum -> leaf key datum
        end
      else
        begin match f None with
        | None -> t
        | Some datum -> join key (leaf key datum) key' t
        end
    | Branch (prefix, bit, t0, t1) as t ->
      if match_prefix key prefix bit then
        if zero_bit key bit then
          branch prefix bit (update key f t0) t1
        else
          branch prefix bit t0 (update key f t1)
      else
        match f None with
        | None -> t
        | Some datum -> join key (leaf key datum) prefix t

  let rec remove i = function
    | Empty -> empty
    | Leaf(j, _) as t ->
      if i = j then empty
      else t
    | Branch (prefix, bit, t0, t1) as t ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (remove i t0) t1
        else
          branch prefix bit t0 (remove i t1)
      else
        t

  (* CR mshinwell: Provide a [union] where [f] doesn't return an [option]. *)
  let rec union f t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Leaf (i, d0), Leaf (j, d1) when i = j ->
      (* CR mshinwell: [join] in [Typing_env_level] is relying on the fact
         that the arguments to [f] are always in the correct order, i.e. that
         the first datum comes from [t0] and the second from [t1].
         Document. *)
      begin match f i d0 d1 with
      | None -> empty
      | Some datum -> leaf i datum
      end
    | Leaf (i, d0), Leaf (j, _) -> join i (leaf i d0) j t1
    | Leaf (i, d), Branch (prefix, bit, t10, t11) ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (union f t0 t10) t11
        else
          branch prefix bit t10 (union f t0 t11)
      else
        join i (leaf i d) prefix t1
    | Branch (prefix, bit, t00, t01), Leaf (i, d) ->
      if match_prefix i prefix bit then
        let f i d0 d1 = f i d1 d0 in  (* CR mshinwell: add flag to disable? *)
        if zero_bit i bit then
          branch prefix bit (union f t1 t00) t01
        else
          branch prefix bit t00 (union f t1 t01)
      else
        join i (leaf i d) prefix t0
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        branch prefix0 bit0 (union f t00 t10) (union f t01 t11)
      else if includes_prefix prefix0 bit0 prefix1 bit1 then
        if zero_bit prefix1 bit0 then
          branch prefix0 bit0 (union f t00 t1) t01
        else
          branch prefix0 bit0 t00 (union f t01 t1)
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          branch prefix1 bit1 (union f t0 t10) t11
        else
          branch prefix1 bit1 t10 (union f t0 t11)
      else
        join prefix0 t0 prefix1 t1

  (* CR mshinwell: rename to subset_domain and inter_domain? *)

  let rec subset t0 t1 =
    match t0, t1 with
    | Empty, _ -> true
    | _, Empty -> false
    | Branch _, Leaf _ -> false
    | Leaf(i, _), _ -> mem i t1
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          subset t00 t10 && subset t01 t11
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            subset t0 t10
          else
            subset t0 t11
        else
          false

  let rec inter_domains t0 t1 =
    match t0, t1 with
    | Empty, _ -> empty
    | _, Empty -> empty
    | Leaf(i, _), _ -> if mem i t1 then t0 else empty
    | _, Leaf(i, _) -> if mem i t0 then t1 else empty
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          branch prefix0 bit0 (inter_domains t00 t10) (inter_domains t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            inter_domains t00 t1
          else
            inter_domains t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            inter_domains t0 t10
          else
            inter_domains t0 t11
        else
          empty

  let rec find i = function
    | Empty -> raise Not_found
    | Leaf(j, d) -> if j = i then d else raise Not_found
    | Branch(prefix, bit, t0, t1) ->
        if not (match_prefix i prefix bit) then raise Not_found
        else if zero_bit i bit then find i t0
        else find i t1

  let rec inter f t0 t1 =
    match t0, t1 with
    | Empty, _ -> empty
    | _, Empty -> empty
    | Leaf (i, d0), _ ->
      begin match find i t1 with
      | exception Not_found -> empty
      | d1 -> leaf i (f i d0 d1)
      end
    | _, Leaf (i, d1) ->
      begin match find i t0 with
      | exception Not_found -> empty
      | d0 -> leaf i (f i d0 d1)
      end
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          branch prefix0 bit0 (inter f t00 t10) (inter f t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            inter f t00 t1
          else
            inter f t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            inter f t0 t10
          else
            inter f t0 t11
        else
          empty

  let rec inter_domain_is_non_empty t0 t1 =
    match t0, t1 with
    | Empty, _
    | _, Empty -> false
    | Leaf (i, _), _ -> mem i t1
    | _, Leaf (i, _) -> mem i t0
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          (inter_domain_is_non_empty t00 t10)
            || (inter_domain_is_non_empty t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            inter_domain_is_non_empty t00 t1
          else
            inter_domain_is_non_empty t01 t1
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            inter_domain_is_non_empty t0 t10
          else
            inter_domain_is_non_empty t0 t11
        else
          false

  let rec diff t0 t1 =
    match t0, t1 with
    | Empty, _ -> empty
    | _, Empty -> t0
    | Leaf(i, _), _ -> if mem i t1 then empty else t0
    | _, Leaf(i, _) -> remove i t0
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
        if equal_prefix prefix0 bit0 prefix1 bit1 then
          branch prefix0 bit0 (diff t00 t10) (diff t01 t11)
        else if includes_prefix prefix0 bit0 prefix1 bit1 then
          if zero_bit prefix1 bit0 then
            branch prefix0 bit0 (diff t00 t1) t01
          else
            branch prefix0 bit0 t00 (diff t01 t1)
        else if includes_prefix prefix1 bit1 prefix0 bit0 then
          if zero_bit prefix0 bit1 then
            diff t0 t10
          else
            diff t0 t11
        else
          t0

  let rec cardinal = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch(_, _, t0, t1) -> cardinal t0 + cardinal t1

  let rec iter f = function
    | Empty -> ()
    | Leaf(key, d) -> f key d
    | Branch(_, _, t0, t1) -> iter f t0; iter f t1

  let rec fold f t acc =
    match t with
    | Empty -> acc
    | Leaf(key, d) -> f key d acc
    | Branch(_, _, t0, t1) -> fold f t0 (fold f t1 acc)

  let rec for_all p = function
    | Empty -> true
    | Leaf(key, d) -> p key d
    | Branch(_, _, t0, t1) -> for_all p t0 && for_all p t1

  let rec exists p = function
    | Empty -> false
    | Leaf(key, d) -> p key d
    | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

  let filter p t =
    let rec loop acc = function
      | Empty -> acc
      | Leaf(i, d) -> if p i d then add i d acc else acc
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop empty t

  let partition p t =
    let rec loop ((true_, false_) as acc) = function
      | Empty -> acc
      | Leaf(i, d) ->
        if p i d then (add i d true_, false_)
        else (true_, add i d false_)
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop (empty, empty) t

  let rec choose = function
    | Empty -> raise Not_found
    | Leaf(key, d) -> key, d
    | Branch(_, _, t0, _) -> choose t0

  let choose_opt t =
    match choose t with
    | exception Not_found -> None
    | choice -> Some choice

  let elements t =
    let rec loop acc = function
      | Empty -> acc
      | Leaf(_, d) -> d :: acc
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop [] t

  let min_binding t =
    let rec loop = function
      | Empty -> raise Not_found
      | Leaf(i, d) -> i, d
      | Branch(_, _, t0, t1) ->
        let i0, d0 = loop t0 in
        let i1, d1 = loop t1 in
          if i0 < i1 then i0, d0
          else i1, d1
    in
    loop t

  let min_binding_opt t =
    match min_binding t with
    | exception Not_found -> None
    | min -> Some min

  let max_binding t =
    let rec loop = function
      | Empty -> raise Not_found
      | Leaf(i, d) -> i, d
      | Branch(_, _, t0, t1) ->
        let i0, d0 = loop t0 in
        let i1, d1 = loop t1 in
          if i0 > i1 then i0, d0
          else i1, d1
    in
    loop t

  let max_binding_opt t =
    match max_binding t with
    | exception Not_found -> None
    | max -> Some max

  let rec equal f t0 t1 =
    if t0 == t1 then true
    else match t0, t1 with
    | Empty, Empty -> true
    | Leaf(i, d0), Leaf(j, d1) -> i = j && f d0 d1
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        equal f t00 t10 && equal f t01 t11
      else false
    | _, _ -> false

  let rec compare f t0 t1 =
    match t0, t1 with
    | Empty, Empty -> 0
    | Leaf(i, d0), Leaf(j, d1) ->
      let c =
        if i = j then 0
        else if i < j then -1
        else 1
      in
      if c <> 0 then c
      else f d0 d1
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      let c = compare_prefix prefix0 bit0 prefix1 bit1 in
      if c = 0 then
        let c = compare f t00 t10 in
        if c = 0 then compare f t01 t11
        else c
      else c
    | Empty, Leaf _ -> 1
    | Empty, Branch _ -> 1
    | Leaf _, Branch _ -> 1
    | Leaf _, Empty -> -1
    | Branch _, Empty -> -1
    | Branch _, Leaf _ -> -1

  let split i t =
    let rec loop ((lt, mem, gt) as acc) = function
      | Empty -> acc
      | Leaf(j, d) ->
          if i = j then (lt, Some d, gt)
          else if j < i then (add j d lt, mem, gt)
          else (lt, mem, add j d gt)
      | Branch(_, _, t0, t1) -> loop (loop acc t0) t1
    in
    loop (empty, None, empty) t

  let rec bindings_aux acc t =
    match t with
    | Empty -> acc
    | Leaf (key, d) -> (key, d) :: acc
    | Branch(_, _, t0, t1) -> bindings_aux (bindings_aux acc t0) t1

  let bindings s =
    List.sort (fun (id1, _) (id2, _) -> Int.compare id1 id2)
      (bindings_aux [] s)

  let rec merge' : type a b c.
    (key -> a option -> b option -> c option)
    -> a t -> b t -> c t
    = fun f t0 t1 ->
    match t0, t1 with
    (* Empty cases, just recurse and be sure to call f on all
       leaf cases recursively *)
    | Empty, Empty -> empty
    | Empty, Leaf (i, d) ->
    begin match f i None (Some d) with
      | None -> empty
      | Some d' -> leaf i d'
      end
    | Leaf (i, d), Empty ->
      begin match f i (Some d) None with
      | None -> empty
      | Some d' -> leaf i d'
      end
    | Empty, Branch (prefix, bit, t10, t11) ->
      branch prefix bit (merge' f t0 t10) (merge' f t0 t11)
    | Branch (prefix, bit, t00, t01), Empty ->
      branch prefix bit (merge' f t00 t1) (merge' f t01 t1)

    (* Leaf cases *)
    | Leaf (i, d0), Leaf (j, d1) when i = j ->
      begin match f i (Some d0) (Some d1) with
      | None -> empty
      | Some datum -> leaf i datum
      end
    | Leaf (i, d0), Leaf (j, d1) ->
      begin match f i (Some d0) None, f j None (Some d1) with
      | None, None -> empty
      | Some d0, None -> leaf i d0
      | None, Some d1 -> leaf j d1
      | Some d0, Some d1 -> join i (leaf i d0) j (leaf j d1)
      end

    (* leaf <-> Branch cases *)
    | Leaf (i, d), Branch (prefix, bit, t10, t11) ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (merge' f t0 t10) (merge' f empty t11)
        else
          branch prefix bit (merge' f empty t10) (merge' f t0 t11)
      else
        begin match f i (Some d) None with
        | None -> merge' f empty t1
        | Some d -> join i (leaf i d) prefix (merge' f empty t1)
        end
    | Branch (prefix, bit, t00, t01), Leaf (i, d) ->
      if match_prefix i prefix bit then
        if zero_bit i bit then
          branch prefix bit (merge' f t00 t1) (merge' f t01 empty)
        else
          branch prefix bit (merge' f t00 empty) (merge' f t01 t1)
      else
        begin match f i None (Some d) with
        | None -> merge' f t0 empty
        | Some d -> join i (leaf i d) prefix (merge' f t0 empty)
        end
    | Branch(prefix0, bit0, t00, t01), Branch(prefix1, bit1, t10, t11) ->
      if equal_prefix prefix0 bit0 prefix1 bit1 then
        branch prefix0 bit0 (merge' f t00 t10) (merge' f t01 t11)
      else if includes_prefix prefix0 bit0 prefix1 bit1 then
        if zero_bit prefix1 bit0 then
          branch prefix0 bit0 (merge' f t00 t1) (merge' f t01 empty)
        else
          branch prefix0 bit0 (merge' f t00 empty) (merge' f t01 t1)
      else if includes_prefix prefix1 bit1 prefix0 bit0 then
        if zero_bit prefix0 bit1 then
          branch prefix1 bit1 (merge' f t0 t10) (merge' f empty t11)
        else
          branch prefix1 bit1 (merge' f empty t10) (merge' f t0 t11)
      else
        join prefix0 (merge' f t0 empty) prefix1 (merge' f empty t1)

  let find_opt t key =
    match find t key with
    | exception Not_found -> None
    | datum -> Some datum

  let find_first _ _ = Misc.fatal_error "find_first not yet implemented"

  let find_first_opt _ _ = Misc.fatal_error "find_first_opt not yet implemented"

  let find_last _ _ = Misc.fatal_error "find_last not yet implemented"

  let find_last_opt _ _ = Misc.fatal_error "find_last_opt not yet implemented"

  let get_singleton t =
    match t with
    | Empty | Branch _ -> None
    | Leaf (key, datum) -> Some (key, datum)

  let get_singleton_exn _ =
    Misc.fatal_error "get_singleton_exn not yet implemented"

  (* CR mshinwell: provide efficient implementations: *)

  let map f t =
    fold (fun key datum result ->
        add key (f datum) result)
      t
      empty

  let mapi f t =
    fold (fun key datum result ->
        add key (f key datum) result)
      t
      empty

  let map_sharing = map

  let to_seq t =
    let rec aux acc () =
      match acc with
      | [] -> Seq.Nil
      | t0 :: r ->
        begin match t0 with
        | Empty -> aux r ()
        | Leaf (key, value) -> Seq.Cons((key, value), aux r)
        | Branch (_, _, t1, t2) -> aux (t1 :: t2 :: r) ()
        end
    in
    aux [t]

  let to_rev_seq _ = Misc.fatal_error "to_rev_seq not yet implemented"

  let to_seq_from _ _ = Misc.fatal_error "to_seq_from not yet implemented"

  let add_seq _ _ = Misc.fatal_error "add_seq not yet implemented"

  let of_seq _ = Misc.fatal_error "of_seq not yet implemented"

  (* CR mshinwell: copied from [Container_types] *)

  let filter_map f t =
    fold (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map)
      t empty

  let of_list l =
    List.fold_left (fun map (id, v) -> add id v map) empty l

  let merge f t0 t1 = merge' f t0 t1

  (* CR mshinwell: fix this *)
  let disjoint_union ?eq ?print t1 t2 =
    if t1 == t2 then t1
    else begin
      ignore print;
      let fail key =
        Misc.fatal_errorf
          "Patricia_tree.disjoint_union: key %a is in intersection"
          Key.print key
      in
      union (fun key datum1 datum2 ->
          match eq with
          | None -> fail key
          | Some eq -> if eq datum1 datum2 then Some datum1 else fail key)
        t1 t2
    end

  let union_left _ _ = Misc.fatal_error "union_left not yet implemented"

  let union_right _ _ = Misc.fatal_error "union_right not yet implemented"

  let union_merge _ _ _ = Misc.fatal_error "union_merge not yet implemented"

  let rename _ _ = Misc.fatal_error "rename not yet implemented"

  let map_keys _ _ = Misc.fatal_error "map_keys not yet implemented"

  let print print_datum ppf t =
    if is_empty t then
      Format.fprintf ppf "{}"
    else
      Format.fprintf ppf "@[<hov 1>{%a}@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          (fun ppf (key, datum) ->
            Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
              Key.print key print_datum datum))
        (bindings t)

  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data _ =
    Misc.fatal_error "transpose_keys_and_data not yet implemented"

  let transpose_keys_and_data_set _ =
    Misc.fatal_error "transpose_keys_and_data_set not yet implemented"

  let diff_domains = diff

  let fold2_stop_on_key_mismatch _ _ _ _ =
    Misc.fatal_error "fold2_stop_on_key_mismatch not yet implemented"

  let () =
    ignore subset;
    ignore inter_domains;
    ignore diff;
    ignore elements
end [@@@inline always]
