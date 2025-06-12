(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Key = struct
  type t = int

  let print ppf i =
    let chunk n = (i lsr (n * 16)) land 0xffff in
    Format.fprintf ppf "%04x_%04x_%04x_%04x" (chunk 3) (chunk 2) (chunk 1)
      (chunk 0)
end

module Value = struct
  type t =
    | A
    | B

  let equal t1 t2 = match t1, t2 with A, A | B, B -> true | _ -> false

  let compare t1 t2 =
    match t1, t2 with A, A | B, B -> 0 | A, B -> -1 | B, A -> 1

  let print ppf t =
    Format.pp_print_string ppf (match t with A -> "A" | B -> "B")
end

module Tree = Flambda2_algorithms.Patricia_tree.Make (Key)
module Set = Tree.Set
module Map = Tree.Map

let ( <=> ) = Bool.equal

let ( ==> ) a f = (not a) || f ()

let ( = ) = Int.equal

let ( != ) i j = not (i = j)

let raises_not_found f x =
  match f x with _ -> false | exception Not_found -> true

let option_of_not_found f x =
  match f x with y -> Some y | exception Not_found -> None

module type Value = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

let reflexive compare a = compare a a = 0

let antisymmetric compare a b = compare a b = ~-(compare b a)

let transitive compare a b c =
  let cmp_a_b = compare a b in
  compare b c = cmp_a_b ==> fun () -> compare a c = cmp_a_b

let equal_list_up_to_order compare l1 l2 =
  List.equal
    (fun a1 a2 -> compare a1 a2 = 0)
    (List.sort compare l1) (List.sort compare l2)

module Set_and_element = struct
  (* A set, bundled with an element of the set. Also known as a pointed set. *)
  type t =
    { set : Set.t;
      element : Key.t
    }
end

module Set_specs = struct
  open Set_and_element

  (* These are all meant to be read as universally-quantified propositions. *)

  let valid s = Set.valid s

  let mem_empty k = not (Set.mem k Set.empty)

  let mem_known_mem { set = s; element = e } = Set.mem e s

  let is_empty_vs_equal s = Set.is_empty s <=> Set.equal s Set.empty

  let add_valid e s = Set.valid (Set.add e s)

  let add_mem e s = Set.mem e (Set.add e s)

  let add_subset e s = Set.subset s (Set.add e s)

  let add_cardinal e s =
    (not (Set.mem e s)) ==> fun () ->
    Set.cardinal (Set.add e s) = Set.cardinal s + 1

  let add_elt { set = s; element = e } = Set.equal (Set.add e s) s

  let singleton_valid e = Set.valid (Set.singleton e)

  let singleton_vs_elements e =
    List.equal Int.equal (Set.singleton e |> Set.elements) [e]

  let remove_valid e s = Set.valid (Set.remove e s)

  let remove_elt_not_mem { set = s; element = e } =
    not (Set.mem e (Set.remove e s))

  let remove_elt_subset { set = s; element = e } = Set.subset (Set.remove e s) s

  let remove_elt_cardinal { set = s; element = e } =
    Set.cardinal (Set.remove e s) = Set.cardinal s - 1

  let remove_non_elt e s =
    (not (Set.mem e s)) ==> fun () -> Set.equal (Set.remove e s) s

  let union_valid s1 s2 = Set.valid (Set.union s1 s2)

  let union_subset s1 s2 =
    let su = Set.union s1 s2 in
    Set.subset s1 su && Set.subset s2 su

  let union_mem s1 s2 =
    let su = Set.union s1 s2 in
    Set.for_all (fun e -> Set.mem e s1 || Set.mem e s2) su

  let union_with_self s = Set.equal (Set.union s s) s

  let union_sharing_vs_union s1 s2 =
    Set.equal (Set.union_sharing s1 s2) (Set.union s1 s2)

  let union_sharing_with_subset s1 s2 =
    Set.union_sharing s1 (Set.inter s1 s2) == s1

  let union_sharing_with_self s = Set.union_sharing s s == s

  let union_shared_vs_union_sharing s1 s2 =
    Set.equal (Set.union_shared s1 s2) (Set.union s1 s2)

  let union_shared_with_subset s1 s2 =
    Set.union_sharing s1 (Set.inter s1 s2) == s1

  let union_shared_with_self s = Set.union_shared s s == s

  let inter_valid s1 s2 = Set.valid (Set.inter s1 s2)

  let inter s1 s2 =
    let si = Set.inter s1 s2 in
    Set.equal si (Set.filter (fun e -> Set.mem e s2) s1)
    && Set.equal si (Set.filter (fun e -> Set.mem e s1) s2)

  let inter_with_self s = Set.equal (Set.inter s s) s

  let disjoint s1 s2 = Set.disjoint s1 s2 <=> Set.is_empty (Set.inter s1 s2)

  let diff_valid s1 s2 = Set.valid (Set.diff s1 s2)

  let diff s1 s2 =
    Set.equal (Set.diff s1 s2) (Set.filter (fun e -> not (Set.mem e s2)) s1)

  let diff_with_self s = Set.is_empty (Set.diff s s)

  let diff_sharing_vs_diff s1 s2 =
    Set.equal (Set.diff_sharing s1 s2) (Set.diff s1 s2)

  let diff_sharing_of_disjoint s1 s2 =
    Set.diff_sharing s1 (Set.diff s2 s1) == s1

  let diff_shared_vs_diff s1 s2 =
    Set.equal (Set.diff_shared s1 s2) (Set.diff s1 s2)

  let diff_shared_of_disjoint s1 s2 = Set.diff_shared s1 (Set.diff s2 s1) == s1

  let compare_vs_equal s1 s2 = Set.compare s1 s2 = 0 <=> Set.equal s1 s2

  let compare_refl = reflexive Set.compare

  let compare_antisym = antisymmetric Set.compare

  let compare_trans = transitive Set.compare

  let subset_refl s = Set.subset s s

  (* Not great coverage in the equal case but this is covered by subset_refl *)
  let subset_antisym s1 s2 =
    (Set.subset s1 s2 && Set.subset s2 s1) <=> Set.equal s1 s2

  (* CR lmaurer: Probably terrible case coverage; should use small sets with
     small keys *)
  let subset_trans s1 s2 s3 =
    (Set.subset s1 s2 && Set.subset s2 s3) ==> fun () -> Set.subset s1 s3

  let subset_vs_mem s1 s2 =
    Set.subset s1 s2 <=> Set.for_all (fun e -> Set.mem e s2) s1

  let iter_vs_fold s =
    (* Might like to use a random function to transform a state, but the
       traversal order isn't specified (and it's different between [iter] and
       [fold]), so we can just add instead of trying to generate a random
       commutative function *)
    let result_by_iter =
      let state = ref 0 in
      Set.iter (fun i -> state := i + !state) s;
      !state
    in
    let result_by_fold = Set.fold ( + ) s 0 in
    result_by_iter = result_by_fold

  let equal_vs_list s1 s2 =
    Set.equal s1 s2
    <=> List.equal Int.equal (s1 |> Set.elements) (s2 |> Set.elements)

  let map_valid f s = Set.valid (Set.map f s)

  let map_vs_list f s =
    Set.equal (Set.map f s) (List.map f (s |> Set.elements) |> Set.of_list)

  let for_all f s = Set.for_all f s <=> List.for_all f (s |> Set.elements)

  let exists f s = Set.exists f s <=> List.exists f (s |> Set.elements)

  let filter_valid f s = Set.valid (Set.filter f s)

  let filter_vs_list f s =
    List.equal Int.equal
      (Set.filter f s |> Set.elements)
      (List.filter f (s |> Set.elements))

  let filter_map_vs_list f s =
    Set.equal (Set.filter_map f s)
      (List.filter_map f (s |> Set.elements) |> Set.of_list)

  let partition_valid f s =
    let s_t, s_f = Set.partition f s in
    Set.valid s_t && Set.valid s_f

  let partition f s =
    let s_t, s_f = Set.partition f s in
    Set.equal s_t (Set.filter f s)
    && Set.equal s_f (Set.filter (fun e -> not (f e)) s)

  let cardinal_vs_list_length s =
    Set.cardinal s = List.length (s |> Set.elements)

  let elements s =
    let l = s |> Set.elements in
    Set.for_all (fun e -> List.mem e l) s
    && List.for_all (fun e -> Set.mem e s) l

  let min_elt_vs_opt s =
    Option.equal Int.equal (Set.min_elt_opt s)
      (option_of_not_found Set.min_elt s)

  let max_elt_vs_opt s =
    Option.equal Int.equal (Set.max_elt_opt s)
      (option_of_not_found Set.max_elt s)

  let min_elt s =
    match Set.min_elt s with
    | exception Not_found -> Set.is_empty s
    | e -> Set.mem e s && Set.for_all (fun e' -> e <= e') s

  let max_elt s =
    match Set.max_elt s with
    | exception Not_found -> Set.is_empty s
    | e -> Set.mem e s && Set.for_all (fun e' -> e >= e') s

  let choose_vs_opt s =
    Option.equal Int.equal (Set.choose_opt s) (option_of_not_found Set.choose s)

  let choose s =
    match Set.choose s with
    | exception Not_found -> Set.is_empty s
    | e -> Set.mem e s

  let split_valid e s =
    let s_lo, _, s_hi = Set.split e s in
    Set.valid s_lo && Set.valid s_hi

  let split_vs_filter e s =
    let s_lo, _, s_hi = Set.split e s in
    Set.equal s_lo (Set.filter (fun e' -> e' < e) s)
    && Set.equal s_hi (Set.filter (fun e' -> e' > e) s)

  let split_vs_mem e s =
    let _, is_mem, _ = Set.split e s in
    is_mem <=> Set.mem e s

  let find_elt s = Set.for_all (fun e -> Set.find e s = e) s

  let find_non_elt e s =
    (not (Set.mem e s)) ==> fun () -> raises_not_found (Set.find e) s

  let of_list_valid l = Set.valid (Set.of_list l)

  let of_list_then_elements l =
    List.equal Int.equal
      (l |> Set.of_list |> Set.elements)
      (l |> List.sort_uniq Int.compare)

  let elements_then_of_list s = Set.equal s (s |> Set.elements |> Set.of_list)

  let to_seq s =
    List.equal Int.equal (s |> Set.to_seq |> List.of_seq) (s |> Set.elements)

  let union_list l =
    Set.equal (Set.union_list l) (List.fold_left Set.union Set.empty l)

  let get_singleton s =
    Option.equal Int.equal (Set.get_singleton s)
      (match Set.cardinal s with 1 -> Set.choose_opt s | _ -> None)
end

module Map_and_binding = struct
  (* A map, together with a key in the map and its value. *)
  type 'a t =
    { map : 'a Map.t;
      key : Key.t;
      value : 'a
    }
end

module Map_specs (V : Value) = struct
  open Map_and_binding

  let ( =? ) = Option.equal V.equal

  let valid m = Map.valid m

  let equal_bindings (k1, v1) (k2, v2) = Int.equal k1 k2 && V.equal v1 v2

  let find_opt_vs_find k m =
    Map.find_opt k m =? option_of_not_found (Map.find k) m

  let find_opt_empty k = Map.find_opt k Map.empty =? None

  let find_opt_known_mem { map = m; key = k; value = v } =
    Map.find_opt k m =? Some v

  let mem k m = Map.find_opt k m =? None <=> not (Map.mem k m)

  let is_empty_vs_equal m = Map.is_empty m <=> Map.equal V.equal m Map.empty

  let add_valid k v m = Map.valid (Map.add k v m)

  let add_same k v m = Map.find_opt k (Map.add k v m) =? Some v

  let add_other k1 k2 v m =
    k1 != k2 ==> fun () -> Map.find_opt k1 (Map.add k2 v m) =? Map.find_opt k1 m

  let update_valid k f m = Map.valid (Map.update k f m)

  let update_same k f m =
    Map.find_opt k (Map.update k f m) =? f (Map.find_opt k m)

  let update_other k1 k2 f m =
    k1 != k2 ==> fun () ->
    Map.find_opt k1 (Map.update k2 f m) =? Map.find_opt k1 m

  let singleton_valid k v = Map.valid (Map.singleton k v)

  let singleton_same k v = Map.find_opt k (Map.singleton k v) =? Some v

  let singleton_other k1 k2 v =
    k1 != k2 ==> fun () ->
    Map.find_opt k1 (Map.singleton k2 v) =? if k1 = k2 then Some v else None

  let remove_valid k m = Map.valid (Map.remove k m)

  let remove_same k m = Map.find_opt k (Map.remove k m) =? None

  let remove_other k1 k2 m =
    k1 != k2 ==> fun () ->
    Map.find_opt k1 (Map.remove k2 m) =? Map.find_opt k1 m

  let merge_valid f m1 m2 = Map.valid (Map.merge f m1 m2)

  let merge f { map = m1; key = k1; _ } { map = m2; key = k2; _ } =
    let mm = Map.merge f m1 m2 in
    let correct_for_key k =
      Map.find_opt k mm =? f k (Map.find_opt k m1) (Map.find_opt k m2)
    in
    correct_for_key k1 && correct_for_key k2

  let union_valid f m1 m2 = Map.valid (Map.union f m1 m2)

  let union f { map = m1; key = k1; value = v1 }
      { map = m2; key = k2; value = v2 } =
    let mu = Map.union f m1 m2 in
    let correct_for_k1 =
      match Map.find_opt k1 m2 with
      | None -> Map.find_opt k1 mu =? Some v1
      | Some v2 -> Map.find_opt k1 mu =? f k1 v1 v2
    in
    let correct_for_k2 =
      match Map.find_opt k2 m1 with
      | None -> Map.find_opt k2 mu =? Some v2
      | Some v1 -> Map.find_opt k2 mu =? f k2 v1 v2
    in
    correct_for_k1 && correct_for_k2

  let union_mem f m1 m2 =
    let mu = Map.union f m1 m2 in
    mu |> Map.for_all (fun k _ -> Map.mem k m1 || Map.mem k m2)

  let compare_vs_equal m1 m2 =
    Map.compare V.compare m1 m2 = 0 <=> Map.equal V.equal m1 m2

  let compare_refl = reflexive (Map.compare V.compare)

  let compare_antisym = antisymmetric (Map.compare V.compare)

  let compare_trans = transitive (Map.compare V.compare)

  let equal_vs_list m1 m2 =
    Map.equal V.equal m1 m2
    <=> List.equal equal_bindings (m1 |> Map.bindings) (m2 |> Map.bindings)

  let for_all_vs_list f m =
    Map.for_all f m <=> List.for_all (fun (k, v) -> f k v) (m |> Map.bindings)

  let exists_vs_list f m =
    Map.exists f m <=> List.exists (fun (k, v) -> f k v) (m |> Map.bindings)

  let iter_vs_fold m =
    (* See comment on [Set_specs.iter_vs_fold] *)
    let int_of_value (v : Value.t) = match v with A -> 1 | B -> -1 in
    let result_by_iter =
      let state = ref 0 in
      Map.iter (fun k v -> state := k + (v |> int_of_value) + !state) m;
      !state
    in
    let result_by_fold =
      Map.fold (fun k v state -> k + (v |> int_of_value) + state) m 0
    in
    result_by_iter = result_by_fold

  let filter_valid f m = Map.valid (Map.filter f m)

  let filter_subset f m =
    let mf = Map.filter f m in
    Set.subset (Map.keys mf) (Map.keys m)

  let filter_find_opt f { map = m; key = k; value = v } =
    let mf = Map.filter f m in
    Map.find_opt k mf =? if f k v then Some v else None

  let filter_map_valid f m = Map.valid (Map.filter_map f m)

  let filter_map_subset f m =
    let mf = Map.filter_map f m in
    Set.subset (Map.keys mf) (Map.keys m)

  let filter_map f { map = m; key = k; value = v } =
    let mf = Map.filter_map f m in
    Map.find_opt k mf =? f k v

  let partition_valid f m =
    let m_t, m_f = Map.partition f m in
    Map.valid m_t && Map.valid m_f

  let partition f m =
    let m_t, m_f = Map.partition f m in
    Map.equal V.equal m_t (Map.filter f m)
    && Map.equal V.equal m_f (Map.filter (fun k e -> not (f k e)) m)

  let cardinal_vs_list_length m =
    Map.cardinal m = List.length (m |> Map.bindings)

  let find_opt_elt_vs_list_assoc_opt { map = m; key = k; value = v } =
    List.assoc_opt k (m |> Map.bindings) =? Some v

  let find_opt_other_vs_list_assoc_opt k m =
    (not (Map.mem k m)) ==> fun () ->
    List.assoc_opt k (m |> Map.bindings) =? None

  let min_binding_vs_opt m =
    Option.equal equal_bindings (Map.min_binding_opt m)
      (option_of_not_found Map.min_binding m)

  let max_binding_vs_opt m =
    Option.equal equal_bindings (Map.max_binding_opt m)
      (option_of_not_found Map.max_binding m)

  let min_binding_opt m =
    match Map.min_binding_opt m with
    | None -> Map.is_empty m
    | Some (k, v) ->
      Map.find_opt k m =? Some v && Map.for_all (fun k' _ -> k' >= k) m

  let max_binding_opt m =
    match Map.max_binding_opt m with
    | None -> Map.is_empty m
    | Some (k, v) ->
      Map.find_opt k m =? Some v && Map.for_all (fun k' _ -> k' <= k) m

  let choose_vs_opt m =
    Option.equal equal_bindings (Map.choose_opt m)
      (option_of_not_found Map.choose m)

  let choose_opt m =
    match Map.choose_opt m with
    | None -> Map.is_empty m
    | Some (k, v) -> Map.find_opt k m =? Some v

  let split_valid k m =
    let m_lo, _, m_hi = Map.split k m in
    Map.valid m_lo && Map.valid m_hi

  let split_elt { map = m; key = k; value = v } =
    let _, binding, _ = Map.split k m in
    binding =? Some v

  let split_other k m =
    (not (Map.mem k m)) ==> fun () ->
    let _, binding, _ = Map.split k m in
    binding =? None

  let split_vs_filter k m =
    let m_lo, _, m_hi = Map.split k m in
    Map.equal V.equal m_lo (Map.filter (fun k' _ -> k' < k) m)
    && Map.equal V.equal m_hi (Map.filter (fun k' _ -> k' > k) m)

  let map_valid f m = Map.valid (Map.map f m)

  let map f m =
    List.equal equal_bindings
      (Map.map f m |> Map.bindings)
      (List.map (fun (k, v) -> k, f v) (m |> Map.bindings))

  let mapi_valid f m = Map.valid (Map.mapi f m)

  let mapi f m =
    List.equal equal_bindings
      (Map.mapi f m |> Map.bindings)
      (List.map (fun (k, v) -> k, f k v) (m |> Map.bindings))

  let to_seq_vs_bindings m =
    List.equal equal_bindings
      (m |> Map.to_seq |> List.of_seq)
      (m |> Map.bindings)

  let of_list_valid l = Map.valid (Map.of_list l)

  module Equality_on_bindings = struct
    let sort_by_key l = List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2) l

    let group_by_key l =
      let rec groups l =
        match l with
        | [] -> []
        | (k, v) :: l ->
          let rec group l =
            match l with
            | [] -> [], []
            | (k', v') :: l when k = k' ->
              let g, l = group l in
              v' :: g, l
            | l -> [], l
          in
          let g, l = group l in
          (k, v :: g) :: groups l
      in
      groups l

    let same_bindings_up_to_duplicate_keys l1 l2 =
      let l1 = l1 |> group_by_key in
      let l2 = l2 |> group_by_key in
      let rec check l1 l2 =
        match l1, l2 with
        | [], [] -> true
        | [], _ | _, [] -> false
        | (k1, vs1) :: l1, (k2, vs2) :: l2 ->
          Int.equal k1 k2
          && List.exists (fun v1 -> List.exists (V.equal v1) vs2) vs1
          && check l1 l2
      in
      check l1 l2
  end

  open Equality_on_bindings

  let of_list_then_bindings l =
    same_bindings_up_to_duplicate_keys
      (l |> Map.of_list |> Map.bindings)
      (sort_by_key l)

  let bindings_then_of_list m =
    Map.equal V.equal (m |> Map.bindings |> Map.of_list) m

  (* CR lmaurer: Fix [Tree.Map.disjoint_union] so we can test it *)

  let map_keys_valid f m = Map.valid (Map.map_keys f m)

  let map_keys f m =
    same_bindings_up_to_duplicate_keys
      (Map.map_keys f m |> Map.bindings)
      (sort_by_key (List.map (fun (k, v) -> f k, v) (m |> Map.bindings)))

  let keys_vs_of_list m =
    Set.equal (Map.keys m) (m |> Map.bindings |> List.map fst |> Set.of_list)

  let data_vs_bindings m =
    equal_list_up_to_order V.compare (Map.data m)
      (Map.bindings m |> List.map snd)

  let of_set_vs_of_list f s =
    Map.equal V.equal (Map.of_set f s)
      (Map.of_list (s |> Set.elements |> List.map (fun k -> k, f k)))

  let diff_domains_valid m1 m2 = Map.valid (Map.diff_domains m1 m2)

  let diff_domains { map = m1; key = k1; value = v1 } m2 =
    let md = Map.diff_domains m1 m2 in
    Map.find_opt k1 md =? if Map.mem k1 m2 then None else Some v1

  let diff_domains_self m = Map.is_empty (Map.diff_domains m m)

  let diff_valid f m1 m2 = Map.valid (Map.diff f m1 m2)

  let diff_vs_diff_domains m1 m2 =
    Map.equal V.equal (Map.diff_domains m1 m2)
      (Map.diff (fun _ _ _ -> None) m1 m2)

  let diff_sharing_vs_diff f m1 m2 =
    Map.equal V.equal (Map.diff f m1 m2) (Map.diff_sharing f m1 m2)

  let diff_sharing_fst m1 m2 =
    Map.diff_sharing (fun _key v1 _v2 -> Some v1) m1 m2 == m1

  let diff_shared_vs_diff f m1 m2 =
    let f k v1 v2 = if v1 == v2 then None else f k v1 v2 in
    Map.equal V.equal (Map.diff f m1 m2) (Map.diff_shared f m1 m2)

  let diff_shared_phys_eq f m = Map.is_empty (Map.diff_shared f m m)

  let diff_shared_disjoint f m1 m2 =
    let m2 = Map.diff_domains m2 m1 in
    Map.diff_shared f m1 m2 == m1

  let inter_valid f m1 m2 = Map.valid (Map.inter f m1 m2)

  let inter_then_find_opt f m1 m2 =
    let mi = Map.inter f m1 m2 in
    let in_both k v =
      match Map.find_opt k m1, Map.find_opt k m2 with
      | Some v1, Some v2 -> V.equal v (f k v1 v2)
      | _, _ -> false
    in
    Map.for_all in_both mi

  let inter_vs_filter f m1 m2 =
    let mi = Map.inter f m1 m2 in
    let mf = Map.filter (fun k _ -> Map.mem k m2) m1 in
    (* Just check the keys; values are checked in [inter_then_find_opt] *)
    Set.equal (Map.keys mi) (Map.keys mf)

  (* See note on [Types.generate_key]; this should actually have good
     coverage *)
  let inter_domain_is_non_empty m1 m2 =
    Map.inter_domain_is_non_empty m1 m2
    <=> not (Map.is_empty (Map.inter (fun _k v _ -> v) m1 m2))

  let get_singleton m =
    match Map.cardinal m with
    | 1 -> Option.equal equal_bindings (Map.get_singleton m) (Map.choose_opt m)
    | _ -> Option.is_none (Map.get_singleton m)

  let replace_valid k f m = Map.valid (Map.replace k f m)

  let replace_same k f m =
    Map.find_opt k (Map.replace k f m) =? Option.map f (Map.find_opt k m)

  let replace_elt f { map = m; key = k; value = v } =
    Map.find_opt k (Map.replace k f m) =? Some (f v)

  let replace_other k1 k2 f m =
    k1 != k2 ==> fun () ->
    Map.find_opt k1 (Map.replace k2 f m) =? Map.find_opt k1 m

  let map_sharing_valid f m = Map.valid (Map.map_sharing f m)

  let map_sharing_vs_map f m =
    Map.equal V.equal (Map.map_sharing f m) (Map.map f m)

  let map_sharing_id m = Map.map_sharing (fun v -> v) m == m

  let filter_map_sharing_valid f m = Map.valid (Map.filter_map_sharing f m)

  let filter_map_sharing_vs_filter_map f m =
    Map.equal V.equal (Map.filter_map_sharing f m) (Map.filter_map f m)

  let filter_map_sharing_id m =
    Map.filter_map_sharing (fun _k v -> Some v) m == m
end

(* CR-someday lmaurer: Move the [Abitrary.t] for perms into a separate module
   and put the permutation tests in their own file. *)

(* A permutation and a value that is not a fixed point under the permutation. In
   other words, the value is an element of the underlying maps. *)
module Perm_and_non_fixed_point (T : Flambda2_algorithms.Container_types.S) =
struct
  type t =
    { perm : Flambda2_nominal.Permutation.Make(T).t;
      value : T.t
    }
end

module Perm_specs (T : Flambda2_algorithms.Container_types.S) = struct
  module Perm = Flambda2_nominal.Permutation.Make (T)

  open Perm_and_non_fixed_point (T)

  let apply_empty v = T.equal (Perm.apply Perm.empty v) v

  (* This mostly tests [Types.container_and_element] *)
  let apply_non_fixed_point { perm = p; value = v } =
    not (T.equal v (Perm.apply p v))

  let test_compose_one ~compose_one p v v' v_test =
    let p' = compose_one p v v' in
    let expected =
      let v1 = Perm.apply p v_test in
      if T.equal v1 v then v' else if T.equal v1 v' then v else v1
    in
    T.equal (Perm.apply p' v_test) expected
    && T.equal (Perm.apply (Perm.inverse p') expected) v_test

  let apply_compose_one_same p v v' =
    let compose_one p v v' = Perm.compose_one ~first:p v v' in
    test_compose_one ~compose_one p v v' v
    && test_compose_one ~compose_one p v v' v'

  let apply_compose_one_other p v v' v_test =
    let compose_one p v v' = Perm.compose_one ~first:p v v' in
    test_compose_one ~compose_one p v v' v_test

  let apply_compose_one_fresh_same p v v' =
    T.equal v' (Perm.apply p v') ==> fun () ->
    let compose_one p v v' = Perm.compose_one_fresh p v ~fresh:v' in
    test_compose_one ~compose_one p v v' v
    && test_compose_one ~compose_one p v v' v'

  let apply_compose_one_fresh_other p v v' v_test =
    T.equal v' (Perm.apply p v') ==> fun () ->
    let compose_one p v v' = Perm.compose_one_fresh p v ~fresh:v' in
    test_compose_one ~compose_one p v v' v_test

  let apply_compose p1 p2 v_test =
    let p12 = Perm.compose ~first:p1 ~second:p2 in
    let p1_inv = Perm.inverse p1 in
    let p2_inv = Perm.inverse p2 in
    T.equal (Perm.apply p12 v_test) (Perm.apply p2 (Perm.apply p1 v_test))
    && T.equal
         (Perm.apply (Perm.inverse p12) v_test)
         (Perm.apply (Perm.compose ~first:p2_inv ~second:p1_inv) v_test)

  let apply_compose_mem_left { perm = p1; value = v } p2 = apply_compose p1 p2 v

  let apply_compose_mem_right p1 { perm = p2; value = v } =
    apply_compose p1 p2 v

  let apply_compose_mem_both { perm = p1; value = v } p2 v' =
    let p2 = Perm.compose_one ~first:p2 v v' in
    apply_compose p1 p2 v

  let compose_inverse_self p =
    let p_inv = Perm.inverse p in
    Perm.is_empty (Perm.compose ~first:p ~second:p_inv)
    && Perm.is_empty (Perm.compose ~first:p_inv ~second:p)
end

module Types = struct
  open Minicheck

  type key = int

  (* This is designed to generate collisions reasonably often: there's a ~1/63
     chance that two calls generate the same result, and I calculate that (a la
     Birthday Paradox) there's a roughly even chance that 10 calls will generate
     some key at least twice.

     (Showing my work: For two keys to be the same, they must[1] both use the
     first generator (1/3 * 1/3 = 1/9 probability) and they must get the same of
     the 7 possible constants (1/7 probability) for an overall probability of
     1/63. Among 10 keys, there are 10*9/2 = 45 pairs, and the chance that none
     of them are equal is (62/63)^45 ~= 48.7%.)

     [1] They could, of course, be the same even if one is generated by
     [Generator.log_int], but this probability is negligible even given that
     [log_int] skews toward small numbers. *)
  let generate_key =
    Generator.choose
      [ 1, Generator.one_of [0; 1; 2; 3; -1; Int.min_int; Int.max_int];
        1, Generator.log_int;
        1, Generator.map Generator.log_int ~f:( ~- ) ]

  let drop_leading_digits key : key Seq.t =
    let rec next mask : key Seq.node =
      if mask = 0
      then Nil
      else
        let key' = key land mask in
        let mask' = mask lsr 1 in
        if key = key' then next mask' else Cons (key', fun () -> next mask')
    in
    fun () -> next (-1 lsr 1)

  let shrink_key key =
    if key = 0 then Seq.empty else Seq.cons 0 (drop_leading_digits key)

  let key =
    Arbitrary.define_simple ~generator:generate_key ~shrinker:shrink_key
      ~printer:Key.print ()

  let generate_unique_list_of_length arb ~compare ~max_length =
    Generator.list (Arbitrary.generate_repr arb) ~length:max_length
    |> Generator.map ~f:(List.sort_uniq compare)

  let generate_unique_list arb ~compare r =
    let max_length = Generator.small_nat ~less_than:20 r in
    generate_unique_list_of_length arb ~compare ~max_length r

  let shrink_unique_list arb l =
    Shrinker.list (Arbitrary.shrink arb) l |> Seq.map (List.sort_uniq compare)

  let [@ocamlformat "disable"] print_list_as_set print_elt ppf l =
    let pp_sep ppf () = Format.fprintf ppf "@,; " in
    Format.fprintf ppf "@[<hov>{ %a }@]"
      (Format.pp_print_list ~pp_sep print_elt) l

  let get_unique_list_as_set arb l =
    l |> List.map (Arbitrary.value arb) |> Set.of_list

  let set =
    let generator = generate_unique_list key ~compare:Int.compare in
    let shrinker = shrink_unique_list key in
    let printer = print_list_as_set Key.print in
    let get_value = get_unique_list_as_set key in
    Arbitrary.define ~generator ~shrinker ~printer ~get_value ()

  let collection_and_element ~(collection_arb : ('c, 'c_repr) Arbitrary.t)
      ~(generate_element : 'c -> repr:'c_repr -> 'e Generator.t)
      ~(print_element : 'e Printer.t) ~(is_empty : 'c -> bool)
      ~(mem : 'e -> 'c -> bool) : ('c * 'e, _) Arbitrary.t =
    let generator r =
      let generate_with_repr r =
        let repr = Arbitrary.generate_repr collection_arb r in
        let coll = Arbitrary.value collection_arb repr in
        coll, repr
      in
      let coll, repr =
        Generator.filter generate_with_repr
          ~f:(fun (coll, _) -> not (is_empty coll))
          r
      in
      let e = generate_element coll ~repr r in
      coll, repr, e
    in
    let shrinker (_, repr, e) =
      Seq.filter_map
        (fun repr ->
          let coll = Arbitrary.value collection_arb repr in
          if mem e coll then Some (coll, repr, e) else None)
        (Arbitrary.shrink collection_arb repr)
    in
    let printer ppf (_coll, repr, e) =
      Printer.pair (Arbitrary.print collection_arb) print_element ppf (repr, e)
    in
    let get_value (coll, _repr, e) = coll, e in
    Arbitrary.define ~generator ~shrinker ~printer ~get_value ()

  let set_and_element =
    collection_and_element ~collection_arb:set
      ~generate_element:(fun _ ~repr r -> Generator.one_of repr r)
      ~print_element:Key.print ~is_empty:Set.is_empty ~mem:Set.mem
    |> Arbitrary.map ~f:(fun (s, e) -> Set_and_element.{ set = s; element = e })

  let generate_assoc_list_of_length key_arb val_arb ~max_length =
    generate_unique_list_of_length (Arbitrary.pair key_arb val_arb) ~max_length
      ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2)

  let print_binding print_key print_val ppf (k, v) =
    Format.fprintf ppf "@[<hv>%a@ -> %a@]" print_key k print_val v

  let print_assoc_list_as_set print_key print_val ppf list =
    print_list_as_set (print_binding print_key print_val) ppf list

  let generate_map_repr_of_size val_arb ~max_size =
    generate_assoc_list_of_length key val_arb ~max_length:max_size

  let generate_map_repr val_arb r =
    let max_size = Generator.small_nat ~less_than:20 r in
    generate_map_repr_of_size val_arb ~max_size r

  let shrink_map_repr val_arb = shrink_unique_list (Arbitrary.pair key val_arb)

  let print_map_repr val_arb =
    print_assoc_list_as_set Key.print (Arbitrary.print val_arb)

  let assoc_list val_arb =
    let generator = generate_map_repr val_arb in
    let shrinker = shrink_map_repr val_arb in
    let printer =
      Printer.list (Printer.pair Key.print (Arbitrary.print val_arb))
    in
    let get_value l =
      List.map (fun (key, repr) -> key, Arbitrary.value val_arb repr) l
    in
    Arbitrary.define ~generator ~shrinker ~printer ~get_value ()

  let map val_arb =
    Arbitrary.map (assoc_list val_arb) ~f:Map.of_list
    |> Arbitrary.with_repr_printer ~printer:(print_map_repr val_arb)

  let map_and_binding val_arb =
    collection_and_element ~collection_arb:(map val_arb)
      ~generate_element:(fun _ ~repr r -> Generator.one_of repr r)
      ~print_element:(print_binding Key.print (Arbitrary.print val_arb))
      ~is_empty:Map.is_empty
      ~mem:(fun (k, _) m -> Map.mem k m)
    |> Arbitrary.map ~f:(fun (m, (k, v)) ->
           let v = Arbitrary.value val_arb v in
           Map_and_binding.{ map = m; key = k; value = v })

  module Key_container_types :
    Flambda2_algorithms.Container_types.S with type t = int = struct
    module T = struct
      include Int

      let hash = Hashtbl.hash

      let print = Key.print
    end

    include T
    module Set = Set
    module Map = Map
  end

  module Perm = Flambda2_nominal.Permutation.Make (Key_container_types)
  module Perm_and_non_fixed_point =
    Perm_and_non_fixed_point (Key_container_types)

  let print_two_way_binding print_key print_val ppf (k, v) =
    Format.fprintf ppf "@[<hv>%a@ <-> %a@]" print_key k print_val v

  let get_perm_value l =
    List.fold_left
      (fun p (v, v') -> Perm.compose_one ~first:p v v')
      Perm.empty l

  let perm =
    Arbitrary.map (assoc_list key) ~f:get_perm_value
    |> Arbitrary.with_repr_printer
         ~printer:
           (print_list_as_set (print_two_way_binding Key.print Key.print))

  let perm_and_non_fixed_point =
    let generate_element p ~repr r =
      (* Make sure the generated value isn't accidentally a fixed point of the
         permutation. This could happen if both [(v, v')] and [(v', v)] are in
         the generated list. *)
      Generator.filter (Generator.one_of repr)
        ~f:(fun (v, _) -> v != Perm.apply p v)
        r
    in
    collection_and_element ~collection_arb:perm ~generate_element
      ~print_element:(print_two_way_binding Key.print Key.print)
      ~is_empty:Perm.is_empty ~mem:(fun (v, _) p -> v != Perm.apply p v)
    |> Arbitrary.map ~f:(fun (p, (v, _)) ->
           Perm_and_non_fixed_point.{ perm = p; value = v })
end

let () =
  let open Minicheck in
  let runner = create_runner () in
  let () =
    let open Types in
    let open Set_specs in
    let elt = key in
    let elt_to_elt = Arbitrary.fn elt ~hash_arg:Hashtbl.hash in
    let elt_to_bool = Arbitrary.fn Arbitrary.bool ~hash_arg:Hashtbl.hash in
    let elt_to_elt_option =
      Arbitrary.fn (Arbitrary.option elt) ~hash_arg:Hashtbl.hash
    in
    let elt_list =
      Arbitrary.bind_generator (Generator.small_nat ~less_than:20)
        ~f:(fun length -> Arbitrary.list elt ~length)
    in
    let set_list =
      Arbitrary.bind_generator (Generator.small_nat ~less_than:6)
        ~f:(fun length -> Arbitrary.list set ~length)
    in
    let c ?n ?seed name f arbitrary_impls =
      Runner.check runner ~name:("Set: " ^ name) ?n ?seed ~arbitrary_impls ~f
    in
    c "sets are valid" valid [set];
    c "mem vs. empty" mem_empty [elt];
    c "mem on known member" mem_known_mem [set_and_element];
    c "is_empty vs. equal" is_empty_vs_equal [set];
    c "add is valid" add_valid [elt; set];
    c "add then mem" add_mem [elt; set];
    c "add then subset" add_subset [elt; set];
    c "add then cardinal" add_cardinal [elt; set];
    c "add existing element" add_elt [set_and_element];
    c "singleton is valid" singleton_valid [elt];
    c "singleton vs. elements" singleton_vs_elements [elt];
    c "remove is valid" remove_valid [elt; set];
    c "remove element then mem" remove_elt_not_mem [set_and_element];
    c "remove element then subset" remove_elt_subset [set_and_element];
    c "remove element then cardinal" remove_elt_cardinal [set_and_element];
    c "remove non-element" remove_non_elt [elt; set];
    c "union is valid" union_valid [set; set];
    c "union is superset of arguments" union_subset [set; set];
    c "union has no extra elements" union_mem [set; set];
    c "union with self" union_with_self [set];
    c "union_sharing vs. union" union_sharing_vs_union [set; set];
    c "union_sharing with subset" union_sharing_with_subset [set; set];
    c "union_sharing with self" union_sharing_with_self [set];
    c "union_shared vs. union" union_shared_vs_union_sharing [set; set];
    c "union_shared with subset" union_shared_with_subset [set; set];
    c "union_shared with self" union_shared_with_self [set];
    c "inter is valid" inter_valid [set; set];
    c "inter" inter [set; set];
    c "inter with self" inter_with_self [set];
    c "disjoint" disjoint [set; set];
    c "diff is valid" diff_valid [set; set];
    c "diff" diff [set; set];
    c "diff with self" diff_with_self [set];
    c "diff_sharing vs. diff" diff_sharing_vs_diff [set; set];
    c "diff_sharing of disjoint" diff_sharing_of_disjoint [set; set];
    c "diff_shared vs. diff" diff_shared_vs_diff [set; set];
    c "diff_shared of disjoint" diff_shared_of_disjoint [set; set];
    c "compare vs. equal" compare_vs_equal [set; set];
    c "compare is reflexive" compare_refl [set];
    c "compare is antisymmetric" compare_antisym [set; set];
    c "compare is transitive" compare_trans [set; set; set];
    c "equal vs. List.equal" equal_vs_list [set; set];
    c "subset is reflexive" subset_refl [set];
    c "subset is antisymmetric" subset_antisym [set; set];
    c "subset is transitive" subset_trans [set; set; set];
    c "subset vs. mem" subset_vs_mem [set; set];
    c "iter vs. fold" iter_vs_fold [set];
    c "map is valid" map_valid [elt_to_elt; set];
    c "map vs. List.map" map_vs_list [elt_to_elt; set];
    c "for_all" for_all [elt_to_bool; set];
    c "exists" exists [elt_to_bool; set];
    c "filter is valid" filter_valid [elt_to_bool; set];
    c "filter vs. List.filter" filter_vs_list [elt_to_bool; set];
    c "filter_map vs. List.filter_map" filter_map_vs_list
      [elt_to_elt_option; set];
    c "partition is valid" partition_valid [elt_to_bool; set];
    c "partition" partition [elt_to_bool; set];
    c "cardinal vs. List.length" cardinal_vs_list_length [set];
    c "elements" elements [set];
    c "min_elt vs. min_elt_opt" min_elt_vs_opt [set];
    c "max_elt vs. max_elt_opt" max_elt_vs_opt [set];
    c "min_elt" min_elt [set];
    c "max_elt" max_elt [set];
    c "choose vs. choose_opt" choose_vs_opt [set];
    c "choose" choose [set];
    c "split is valid" split_valid [elt; set];
    c "split vs. filter" split_vs_filter [elt; set];
    c "split vs. mem" split_vs_mem [elt; set];
    c "find on element" find_elt [set];
    c "find on non-element" find_non_elt [elt; set];
    c "of_list is valid" of_list_valid [elt_list];
    c "of_list then elements" of_list_then_elements [elt_list];
    c "elements_then_of_list" elements_then_of_list [set];
    c "to_seq" to_seq [set];
    c "union_list" union_list [set_list];
    c "get_singleton" get_singleton [set]
  in
  let () =
    let module Map_specs = Map_specs (Value) in
    let open Types in
    let open Map_specs in
    let key : Key.t Arbitrary.simple = key in
    let value : (Value.t, _) Arbitrary.t =
      Arbitrary.map Arbitrary.bool ~f:(fun b -> if b then Value.A else Value.B)
      |> Arbitrary.with_value_printer ~printer:Value.print
    in
    let bindings = assoc_list value in
    let map = Types.map value in
    let map_and_binding = Types.map_and_binding value in
    let key_to_key = Arbitrary.fn key in
    let key_to_value = Arbitrary.fn value in
    let value_to_value = Arbitrary.fn value in
    let value_option_to_value_option = Arbitrary.fn (Arbitrary.option value) in
    let key_and_value_to_bool = Arbitrary.fn2 Arbitrary.bool in
    let key_and_value_to_value = Arbitrary.fn2 value in
    let key_and_value_to_value_option =
      Arbitrary.fn2 (Arbitrary.option value)
    in
    let merging_function = Arbitrary.fn3 (Arbitrary.option value) in
    let union_function = Arbitrary.fn3 (Arbitrary.option value) in
    let diff_function = Arbitrary.fn3 (Arbitrary.option value) in
    let inter_function = Arbitrary.fn3 value in
    let c ?n ?seed name f arbitrary_impls =
      Runner.check runner ~name:("Map: " ^ name) ?n ?seed ~arbitrary_impls ~f
    in
    c "maps are valid" valid [map];
    c "find_opt vs. find" find_opt_vs_find [key; map];
    c "find_opt of known member" find_opt_known_mem [map_and_binding];
    c "find_opt of empty" find_opt_empty [key];
    c "mem" mem [key; map];
    c "is_empty vs. equal" is_empty_vs_equal [map];
    c "add is valid" add_valid [key; value; map];
    c "add then find_opt" add_same [key; value; map];
    c "add then find_opt other" add_other [key; key; value; map];
    c "update is valid" update_valid [key; value_option_to_value_option; map];
    c "update then find_opt" update_same [key; value_option_to_value_option; map];
    c "update then find_opt other" update_other
      [key; key; value_option_to_value_option; map];
    c "singleton is valid" singleton_valid [key; value];
    c "singleton then find_opt" singleton_same [key; value];
    c "singleton then find_opt of other" singleton_other [key; key; value];
    c "remove is valid" remove_valid [key; map];
    c "remove then find_opt" remove_same [key; map];
    c "remove then find_opt of other" remove_other [key; key; map];
    c "merge is valid" merge_valid [merging_function; map; map];
    c "merge" merge [merging_function; map_and_binding; map_and_binding];
    c "union is valid" union_valid [union_function; map; map];
    c "union" union [union_function; map_and_binding; map_and_binding];
    c "union then mem" union_mem [union_function; map; map];
    c "compare vs. equal" compare_vs_equal [map; map];
    c "compare is reflexive" compare_refl [map];
    c "compare is antisymmetric" compare_antisym [map; map];
    c "compare is transitive" compare_trans [map; map; map];
    c "equal vs. List.equal" equal_vs_list [map; map];
    c "iter vs. fold" iter_vs_fold [map];
    c "for_all vs. List.for_all" for_all_vs_list [key_and_value_to_bool; map];
    c "exists vs. List.exists" exists_vs_list [key_and_value_to_bool; map];
    c "filter is valid" filter_valid [key_and_value_to_bool; map];
    c "filter returns subset" filter_subset [key_and_value_to_bool; map];
    c "filter then find_opt" filter_find_opt
      [key_and_value_to_bool; map_and_binding];
    c "filter_map is valid" filter_map_valid [key_and_value_to_value_option; map];
    c "filter_map_subset" filter_map_subset [key_and_value_to_value_option; map];
    c "filter_map" filter_map [key_and_value_to_value_option; map_and_binding];
    c "partition is valid" partition_valid [key_and_value_to_bool; map];
    c "partition" partition [key_and_value_to_bool; map];
    c "cardinal vs. List.length" cardinal_vs_list_length [map];
    c "find_opt of element vs. List.assoc_opt" find_opt_elt_vs_list_assoc_opt
      [map_and_binding];
    c "find_opt of non-element vs. List.assoc_opt"
      find_opt_other_vs_list_assoc_opt [key; map];
    c "min_binding vs. min_binding_opt" min_binding_vs_opt [map];
    c "max_binding vs. max_binding_opt" max_binding_vs_opt [map];
    c "min_binding_opt" min_binding_opt [map];
    c "max_binding_opt" max_binding_opt [map];
    c "choose vs. choose_opt" choose_vs_opt [map];
    c "choose_opt" choose_opt [map];
    c "split is valid" split_valid [key; map];
    c "split on element" split_elt [map_and_binding];
    c "split on non-element" split_other [key; map];
    c "split vs. filter" split_vs_filter [key; map];
    c "map is valid" map_valid [value_to_value; map];
    c "map" Map_specs.map [value_to_value; map];
    c "mapi is valid" mapi_valid [key_and_value_to_value; map];
    c "mapi" mapi [key_and_value_to_value; map];
    c "to_seq vs. bindings" to_seq_vs_bindings [map];
    c "of_list is valid" of_list_valid [bindings];
    c "of_list then bindings" of_list_then_bindings [bindings];
    c "bindings_then_of_list" bindings_then_of_list [map];
    c "map_keys is valid" map_keys_valid [key_to_key; map];
    c "map_keys" map_keys [key_to_key; map];
    c "keys vs. Set.of_list" keys_vs_of_list [map];
    c "data vs. bindings" data_vs_bindings [map];
    c "of_set vs. of_list" of_set_vs_of_list [key_to_value; set];
    c "diff_domains valid" diff_domains_valid [map; map];
    c "diff_domains" diff_domains [map_and_binding; map];
    c "diff_domains of self" diff_domains_self [map];
    c "diff valid" diff_valid [diff_function; map; map];
    c "diff vs. diff_domains" diff_vs_diff_domains [map; map];
    c "diff_sharing vs. diff" diff_sharing_vs_diff [diff_function; map; map];
    c "diff_sharing of fst" diff_sharing_fst [map; map];
    c "diff_shared vs. diff" diff_shared_vs_diff [diff_function; map; map];
    c "diff_shared of phys. eq" diff_shared_phys_eq [diff_function; map];
    c "diff_shared of disjoint" diff_shared_disjoint [diff_function; map; map];
    c "inter is valid" inter_valid [inter_function; map; map];
    c "inter then find_opt" inter_then_find_opt [inter_function; map; map];
    c "inter vs. filter" inter_vs_filter [inter_function; map; map];
    c "inter_domain_is_non_empty" inter_domain_is_non_empty [map; map];
    c "get_singleton" get_singleton [map];
    c "replace is valid" replace_valid [key; value_to_value; map];
    c "replace of elt" replace_elt [value_to_value; map_and_binding];
    c "replace then find_opt" replace_same [key; value_to_value; map];
    c "replace then find_opt other" replace_other [key; key; value_to_value; map];
    c "map_sharing is valid" map_sharing_valid [value_to_value; map];
    c "map_sharing vs. map" map_sharing_vs_map [value_to_value; map];
    c "map_sharing of id" map_sharing_id [map];
    c "filter_map_sharing is valid" filter_map_sharing_valid
      [key_and_value_to_value_option; map];
    c "filter_map_sharing vs. filter_map" filter_map_sharing_vs_filter_map
      [key_and_value_to_value_option; map];
    c "filter_map_sharing of id" filter_map_sharing_id [map];
    ()
  in
  let () =
    let module Perm_specs = Perm_specs (Types.Key_container_types) in
    let open Types in
    let open Perm_specs in
    let value = key in
    let c ?n ?seed name f arbitrary_impls =
      Runner.check runner ~name:("Perm: " ^ name) ?n ?seed ~arbitrary_impls ~f
    in
    c "apply of empty" apply_empty [value];
    c "apply_non_fixed_point" apply_non_fixed_point [perm_and_non_fixed_point];
    c "compose_one then apply same" apply_compose_one_same [perm; value; value];
    c "compose_one then apply other" apply_compose_one_other
      [perm; value; value; value];
    c "compose_one_fresh then apply same" apply_compose_one_fresh_same
      [perm; value; value];
    c "compose_one_fresh then apply other" apply_compose_one_fresh_other
      [perm; value; value; value];
    c "compose then apply" apply_compose [perm; perm; value];
    c "compose then apply with value from left" apply_compose_mem_left
      [perm_and_non_fixed_point; perm];
    c "compose then apply with value from right" apply_compose_mem_right
      [perm; perm_and_non_fixed_point];
    c "compose then apply with value from both" apply_compose_mem_both
      [perm_and_non_fixed_point; perm; value];
    c "compose with own inverse" compose_inverse_self [perm];
    ()
  in
  let failure_count = Runner.failure_count runner in
  if failure_count > 0
  then
    let () =
      Format.printf "%d failures\n" failure_count;
      exit 1
    in
    ()
