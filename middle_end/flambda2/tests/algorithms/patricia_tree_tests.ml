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

let ( = ) = Int.equal

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
  compare b c != cmp_a_b || compare a c = cmp_a_b

let equal_list_up_to_order compare l1 l2 =
  List.equal
    (fun a1 a2 -> compare a1 a2 = 0)
    (List.sort compare l1) (List.sort compare l2)

module Set_specs = struct
  (* These are all meant to be read as universally-quantified propositions. *)

  let mem_vs_empty k = not (Set.mem k Set.empty)

  let is_empty_vs_equal s = Set.is_empty s <=> Set.equal s Set.empty

  let add_mem e s = Set.mem e (Set.add e s)

  let add_subset e s = Set.subset s (Set.add e s)

  let add_cardinal e s =
    Set.mem e s || Set.cardinal (Set.add e s) = Set.cardinal s + 1

  let add_elt s = Set.for_all (fun e -> Set.equal (Set.add e s) s) s

  let singleton_vs_elements e =
    List.equal Int.equal (Set.singleton e |> Set.elements) [e]

  let remove_elt_not_mem s =
    Set.for_all (fun e -> not (Set.mem e (Set.remove e s))) s

  let remove_elt_subset s =
    Set.for_all (fun e -> Set.subset (Set.remove e s) s) s

  let remove_elt_cardinal s =
    Set.for_all (fun e -> Set.cardinal (Set.remove e s) = Set.cardinal s - 1) s

  let remove_non_elt e s = Set.mem e s || Set.equal (Set.remove e s) s

  let union s1 s2 =
    let su = Set.union s1 s2 in
    Set.subset s1 su && Set.subset s2 su
    && Set.for_all (fun e -> Set.mem e s1 || Set.mem e s2) su

  let union_with_self s = Set.equal (Set.union s s) s

  let inter s1 s2 =
    Set.equal (Set.inter s1 s2) (Set.filter (fun e -> Set.mem e s2) s1)

  let inter_with_self s = Set.equal (Set.inter s s) s

  let disjoint s1 s2 = Set.disjoint s1 s2 <=> Set.is_empty (Set.inter s1 s2)

  let diff s1 s2 =
    Set.equal (Set.diff s1 s2) (Set.filter (fun e -> not (Set.mem e s2)) s1)

  let diff_with_self s = Set.is_empty (Set.diff s s)

  let compare_vs_equal s1 s2 = Set.compare s1 s2 = 0 <=> Set.equal s1 s2

  let compare_refl = reflexive Set.compare

  let compare_antisym = antisymmetric Set.compare

  let compare_trans = transitive Set.compare

  let equal_vs_elements s1 s2 =
    Set.equal s1 s2
    <=> List.equal Int.equal (s1 |> Set.elements) (s2 |> Set.elements)

  let map_vs_elements f s =
    Set.equal (Set.map f s) (List.map f (s |> Set.elements) |> Set.of_list)

  let for_all f s = Set.for_all f s <=> List.for_all f (s |> Set.elements)

  let exists f s = Set.exists f s <=> List.exists f (s |> Set.elements)

  let filter_vs_elements f s =
    List.equal Int.equal
      (Set.filter f s |> Set.elements)
      (List.filter f (s |> Set.elements))

  let filter_map_vs_elements f s =
    Set.equal (Set.filter_map f s)
      (List.filter_map f (s |> Set.elements) |> Set.of_list)

  let partition f s =
    let s_t, s_f = Set.partition f s in
    Set.equal s_t (Set.filter f s)
    && Set.equal s_f (Set.filter (fun e -> not (f e)) s)

  let cardinal_vs_elements s = Set.cardinal s = List.length (s |> Set.elements)

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

  let split e s =
    let s_lo, is_mem, s_hi = Set.split e s in
    Set.equal s_lo (Set.filter (fun e' -> e' < e) s)
    && Set.equal s_hi (Set.filter (fun e' -> e' > e) s)
    && is_mem <=> Set.mem e s

  let find_elt s = Set.for_all (fun e -> Set.find e s = e) s

  let find_non_elt e s = Set.mem e s || raises_not_found (Set.find e) s

  let to_seq s =
    List.equal Int.equal
      (s |> Set.to_seq |> List.of_seq |> List.sort Int.compare)
      (s |> Set.elements |> List.sort Int.compare)

  let union_list l =
    Set.equal (Set.union_list l) (List.fold_left Set.union Set.empty l)

  let get_singleton s =
    Option.equal Int.equal (Set.get_singleton s)
      (match Set.cardinal s with 1 -> Set.choose_opt s | _ -> None)
end

module Map_specs (V : Value) = struct
  let ( =? ) = Option.equal V.equal

  let equal_bindings (k1, v1) (k2, v2) = Int.equal k1 k2 && V.equal v1 v2

  let compare_bindings (k1, v1) (k2, v2) =
    match Int.compare k1 k2 with 0 -> V.compare v1 v2 | c -> c

  let find_opt_vs_find k m =
    Map.find_opt k m =? option_of_not_found (Map.find k) m

  let find_opt_empty k = Map.find_opt k Map.empty =? None

  let mem k m = Map.find_opt k m =? None <=> not (Map.mem k m)

  let is_empty_vs_equal m = Map.is_empty m <=> Map.equal V.equal m Map.empty

  let add_same k v m = Map.find_opt k (Map.add k v m) =? Some v

  let add_other k1 k2 v m =
    k1 = k2 || Map.find_opt k1 (Map.add k2 v m) =? Map.find_opt k1 m

  let update_same k f m =
    Map.find_opt k (Map.update k f m) =? f (Map.find_opt k m)

  let update_other k1 k2 f m =
    k1 = k2 || Map.find_opt k1 (Map.update k2 f m) =? Map.find_opt k1 m

  let singleton k1 k2 v =
    Map.find_opt k1 (Map.singleton k2 v) =? if k1 = k2 then Some v else None

  let remove_same k m = Map.find_opt k (Map.remove k m) =? None

  let remove_other k1 k2 m =
    k1 = k2 || Map.find_opt k1 (Map.remove k2 m) =? Map.find_opt k1 m

  let merge f m1 m2 =
    let mm = Map.merge f m1 m2 in
    let correct_for_key k =
      Map.find_opt k mm =? f k (Map.find_opt k m1) (Map.find_opt k m2)
    in
    Map.for_all (fun k _ -> correct_for_key k) m1
    && Map.for_all (fun k _ -> correct_for_key k) m2

  let union f m1 m2 =
    let mu = Map.union f m1 m2 in
    m1
    |> Map.for_all (fun k v1 ->
           match Map.find_opt k m2 with
           | None -> Map.find_opt k mu =? Some v1
           | Some v2 -> Map.find_opt k mu =? f k v1 v2)
    && m2
       |> Map.for_all (fun k v2 ->
              match Map.find_opt k m1 with
              | None -> Map.find_opt k mu =? Some v2
              | Some _ -> (* already checked *) true)
    && mu |> Map.for_all (fun k _ -> Map.mem k m1 || Map.mem k m2)

  let compare_vs_equal m1 m2 =
    Map.compare V.compare m1 m2 = 0 <=> Map.equal V.equal m1 m2

  let compare_refl = reflexive (Map.compare V.compare)

  let compare_antisym = antisymmetric (Map.compare V.compare)

  let compare_trans = transitive (Map.compare V.compare)

  let equal_vs_bindings m1 m2 =
    Map.equal V.equal m1 m2
    <=> List.equal equal_bindings (m1 |> Map.bindings) (m2 |> Map.bindings)

  let for_all_vs_bindings f m =
    Map.for_all f m <=> List.for_all (fun (k, v) -> f k v) (m |> Map.bindings)

  let exists_vs_bindings f m =
    Map.exists f m <=> List.exists (fun (k, v) -> f k v) (m |> Map.bindings)

  let filter f m =
    let mf = Map.filter f m in
    Set.subset (Map.keys mf) (Map.keys m)
    && m
       |> Map.for_all (fun k v ->
              Map.find_opt k mf =? if f k v then Some v else None)

  let filter_map f m =
    let mf = Map.filter_map f m in
    Set.subset (Map.keys mf) (Map.keys m)
    && m |> Map.for_all (fun k v -> Map.find_opt k mf =? f k v)

  let partition f m =
    let m_t, m_f = Map.partition f m in
    Map.equal V.equal m_t (Map.filter f m)
    && Map.equal V.equal m_f (Map.filter (fun k e -> not (f k e)) m)

  let cardinal_vs_bindings m = Map.cardinal m = List.length (m |> Map.bindings)

  let find_opt_elt_vs_bindings m =
    let bindings = m |> Map.bindings in
    m |> Map.for_all (fun k v -> List.assoc_opt k bindings =? Some v)

  let find_opt_other_vs_bindings k m =
    Map.mem k m || List.assoc_opt k (m |> Map.bindings) =? None

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

  let split k m =
    let m_lo, binding, m_hi = Map.split k m in
    binding =? Map.find_opt k m
    && Map.equal V.equal m_lo (Map.filter (fun k' _ -> k' < k) m)
    && Map.equal V.equal m_hi (Map.filter (fun k' _ -> k' > k) m)

  let map f m =
    List.equal equal_bindings
      (Map.map f m |> Map.bindings)
      (List.map (fun (k, v) -> k, f v) (m |> Map.bindings))

  let mapi f m =
    List.equal equal_bindings
      (Map.mapi f m |> Map.bindings)
      (List.map (fun (k, v) -> k, f k v) (m |> Map.bindings))

  let to_seq_vs_bindings m =
    List.equal equal_bindings
      (m |> Map.to_seq |> List.of_seq |> List.sort compare_bindings)
      (m |> Map.bindings)

  module Equality_on_bindings = struct
    let sort_by_key l = List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2) l

    let sort_and_group_by_key l =
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
      groups (sort_by_key l)

    let same_bindings_up_to_duplicate_keys l1 l2 =
      let l1 = l1 |> sort_and_group_by_key in
      let l2 = l2 |> sort_and_group_by_key in
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
    same_bindings_up_to_duplicate_keys (l |> Map.of_list |> Map.bindings) l

  let bindings_then_of_list m =
    Map.equal V.equal (m |> Map.bindings |> Map.of_list) m

  (* CR lmaurer: Fix [Tree.Map.disjoint_union] so we can test it *)

  let map_keys f m =
    same_bindings_up_to_duplicate_keys
      (Map.map_keys f m |> Map.bindings)
      (List.map (fun (k, v) -> f k, v) (m |> Map.bindings))

  let keys_vs_of_list m =
    Set.equal (Map.keys m) (m |> Map.bindings |> List.map fst |> Set.of_list)

  let data_vs_bindings m =
    equal_list_up_to_order V.compare (Map.data m)
      (Map.bindings m |> List.map snd)

  let of_set_vs_of_list f s =
    Map.equal V.equal (Map.of_set f s)
      (Map.of_list (s |> Set.elements |> List.map (fun k -> k, f k)))

  let diff_domains m1 m2 =
    let md = Map.diff_domains m1 m2 in
    m1
    |> Map.for_all (fun k v1 ->
           Map.find_opt k md =? if Map.mem k m2 then None else Some v1)

  let inter f m1 m2 =
    let mi = Map.inter f m1 m2 in
    let in_both k v =
      match Map.find_opt k m1, Map.find_opt k m2 with
      | Some v1, Some v2 -> V.equal v (f k v1 v2)
      | _, _ -> false
    in
    Map.for_all in_both mi

  let inter_domain_is_non_empty m1 m2 =
    Map.inter_domain_is_non_empty m1 m2
    <=> not (Map.is_empty (Map.inter (fun _k v _ -> v) m1 m2))

  let get_singleton m =
    match Map.cardinal m with
    | 1 -> Option.equal equal_bindings (Map.get_singleton m) (Map.choose_opt m)
    | _ -> Option.is_none (Map.get_singleton m)

  let replace_same k f m =
    Map.find_opt k (Map.replace k f m) =? Option.map f (Map.find_opt k m)

  let replace_other k1 k2 f m =
    k1 = k2 || Map.find_opt k1 (Map.replace k2 f m) =? Map.find_opt k1 m

  let map_sharing_vs_map f m =
    Map.equal V.equal (Map.map_sharing f m) (Map.map f m)

  let map_sharing_id m = Map.map_sharing (fun v -> v) m == m
end

module Types = struct
  open Minicheck

  let key =
    Type.choose
      [ 1, Type.one_of [0; 1; 2; 3; -1; Int.min_int; Int.max_int];
        5, Type.log_int ]
    |> Type.with_print ~print:Key.print

  let unique_list ty =
    Type.choose [9, Type.list ty ~expected_length:20; 1, Type.const []]
    |> Type.map_generate ~f:(List.sort_uniq Int.compare)

  let [@ocamlformat "disable"] print_list_as_set print_elt ppf l =
    let pp_sep ppf () = Format.fprintf ppf "@,; " in
    Format.fprintf ppf "@[<hov>{ %a }@]"
      (Format.pp_print_list ~pp_sep print_elt) l

  let set =
    Type.map ~f:Set.of_list (unique_list key)
    |> Type.with_print ~print:(fun ppf s ->
           print_list_as_set Key.print ppf (s |> Set.elements))

  let assoc_list key_ty val_ty =
    Type.list (Type.pair key_ty val_ty) ~expected_length:20
    |> Type.map_generate ~f:(fun pairs ->
           List.sort_uniq (fun (k1, _) (k2, _) -> Int.compare k1 k2) pairs)

  let map val_ty =
    let print ppf m =
      let print_binding ppf (k, v) =
        Format.fprintf ppf "@[<hv>%a@ -> %a@]" Key.print k (Type.print val_ty) v
      in
      print_list_as_set print_binding ppf (m |> Map.bindings)
    in
    Type.map ~f:Map.of_list (assoc_list key val_ty) |> Type.with_print ~print
end

let () =
  let open Minicheck in
  let () =
    let open Types in
    let open Set_specs in
    let elt = key in
    let elt_to_elt = Type.fn elt ~hash_arg:Hashtbl.hash in
    let elt_to_bool = Type.fn Type.bool ~hash_arg:Hashtbl.hash in
    let elt_to_elt_option = Type.fn (Type.option elt) ~hash_arg:Hashtbl.hash in

    let c ?n ?verbose name f types =
      check ~name:("Set: " ^ name) ?n ?verbose ~types ~f ()
    in

    c "mem vs. empty" mem_vs_empty [elt];

    c "is_empty vs. equal" is_empty_vs_equal [set];

    c "add then mem" add_mem [elt; set];

    c "add then subset" add_subset [elt; set];

    c "add then cardinal" add_cardinal [elt; set];

    c "add existing element" add_elt [set];

    c "singleton vs. elements" singleton_vs_elements [elt];

    c "remove element then mem" remove_elt_not_mem [set];

    c "remove element then subset" remove_elt_subset [set];

    c "remove element then cardinal" remove_elt_cardinal [set];

    c "remove non-element" remove_non_elt [elt; set];

    c "union" union [set; set];

    c "union with self" union_with_self [set];

    c "inter" inter [set; set];

    c "inter with self" inter_with_self [set];

    c "disjoint" disjoint [set; set];

    c "diff" diff [set; set];

    c "diff with self" diff_with_self [set];

    c "compare vs. equal" compare_vs_equal [set; set];

    c "compare is reflexive" compare_refl [set];

    c "compare is antisymmetric" compare_antisym [set; set];

    c "compare is transitive" compare_trans [set; set; set];

    c "equal vs. elements" equal_vs_elements [set; set];

    c "map vs. elements" map_vs_elements [elt_to_elt; set];

    c "for_all" for_all [elt_to_bool; set];

    c "exists" exists [elt_to_bool; set];

    c "filter vs. elements" filter_vs_elements [elt_to_bool; set];

    c "filter_map vs. elements" filter_map_vs_elements [elt_to_elt_option; set];

    c "partition" partition [elt_to_bool; set];

    c "cardinal vs. elements" cardinal_vs_elements [set];

    c "elements" elements [set];

    c "min_elt vs. min_elt_opt" min_elt_vs_opt [set];

    c "max_elt vs. max_elt_opt" max_elt_vs_opt [set];

    c "min_elt" min_elt [set];

    c "max_elt" max_elt [set];

    c "choose vs. choose_opt" choose_vs_opt [set];

    c "choose" choose [set];

    c "split" split [elt; set];

    c "find_elt" find_elt [set];

    c "find_non_elt" find_non_elt [elt; set];

    c "to_seq" to_seq [set];

    c "union_list" union_list [Type.list set ~expected_length:5];

    c "get_singleton" get_singleton [set];
    ()
  in
  let () =
    let module Map_specs = Map_specs (Value) in
    let open Types in
    let open Map_specs in
    let key : Key.t Type.t = key in
    let value : Value.t Type.t =
      Type.map Type.bool ~f:(fun b -> if b then Value.A else Value.B)
      |> Type.with_print ~print:Value.print
    in
    let map = Types.map value in
    let key_to_key = Type.fn key in
    let key_to_value = Type.fn value in
    let value_to_value = Type.fn value in
    let value_option_to_value_option = Type.fn (Type.option value) in
    let key_and_value_to_bool = Type.fn2 Type.bool in
    let key_and_value_to_value = Type.fn2 value in
    let key_and_value_to_value_option = Type.fn2 (Type.option value) in
    let merging_function = Type.fn3 (Type.option value) in
    let union_function = Type.fn3 (Type.option value) in
    let inter_function = Type.fn3 value in

    let c ?n ?verbose name f types =
      check ~name:("Map: " ^ name) ?n ?verbose ~types ~f ()
    in

    c "find_opt vs. find" find_opt_vs_find [key; map];

    c "find_opt of empty" find_opt_empty [key];

    c "mem" mem [key; map];

    c "is_empty vs. equal" is_empty_vs_equal [map];

    c "add then find_opt" add_same [key; value; map];

    c "add then find_opt other" add_other [key; key; value; map];

    c "update then find_opt" update_same [key; value_option_to_value_option; map];

    c "update then find_opt other" update_other
      [key; key; value_option_to_value_option; map];

    c "singleton" singleton [key; key; value];

    c "remove then find_opt" remove_same [key; map];

    c "remove then find_opt other" remove_other [key; key; map];

    c "merge" merge [merging_function; map; map];

    c "union" union [union_function; map; map];

    c "compare vs. equal" compare_vs_equal [map; map];

    c "compare is reflexive" compare_refl [map];

    c "compare is antisymmetric" compare_antisym [map; map];

    c "compare is transitive" compare_trans [map; map; map];

    c "equal vs. bindings" equal_vs_bindings [map; map];

    c "for_all vs. bindings" for_all_vs_bindings [key_and_value_to_bool; map];

    c "exists vs. bindings" exists_vs_bindings [key_and_value_to_bool; map];

    c "filter" filter [key_and_value_to_bool; map];

    c "filter_map" filter_map [key_and_value_to_value_option; map];

    c "partition" partition [key_and_value_to_bool; map];

    c "cardinal vs. bindings" cardinal_vs_bindings [map];

    c "find_opt of element vs. bindings" find_opt_elt_vs_bindings [map];

    c "find_opt of non-element vs. bindings" find_opt_other_vs_bindings
      [key; map];

    c "min_binding vs. min_binding_opt" min_binding_vs_opt [map];

    c "max_binding vs. max_binding_opt" max_binding_vs_opt [map];

    c "min_binding_opt" min_binding_opt [map];

    c "max_binding_opt" max_binding_opt [map];

    c "choose_ vs. choose_opt" choose_vs_opt [map];

    c "choose_opt" choose_opt [map];

    c "split" split [key; map];

    c "map" Map_specs.map [value_to_value; map];

    c "mapi" mapi [key_and_value_to_value; map];

    c "to_seq vs. bindings" to_seq_vs_bindings [map];

    c "of_list then bindings" of_list_then_bindings [assoc_list key value];

    c "bindings_then_of_list" bindings_then_of_list [map];

    c "map_keys" map_keys [key_to_key; map];

    c "keys vs. Set.of_list" keys_vs_of_list [map];

    c "data vs. bindings" data_vs_bindings [map];

    c "of_set vs. of_list" of_set_vs_of_list [key_to_value; set];

    c "diff_domains" diff_domains [map; map];

    c "inter" inter [inter_function; map; map];

    c "inter_domain_is_non_empty" inter_domain_is_non_empty [map; map];

    c "get_singleton" get_singleton [map];

    c "replace then find_opt" replace_same [key; value_to_value; map];

    c "replace then find_opt other" replace_other [key; key; value_to_value; map];

    c "map_sharing vs. map" map_sharing_vs_map [value_to_value; map];

    c "map_sharing of id" map_sharing_id [map];
    ()
  in
  if Minicheck.something_has_failed () then exit 1
