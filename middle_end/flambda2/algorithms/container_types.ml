(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


include Container_types_intf

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t

  let compare (a1, b1) (a2, b2) =
    let c = A.compare a1 a2 in
    if c <> 0 then c else B.compare b1 b2

  let hash (a, b) = Hashtbl.hash (A.hash a, B.hash b)

  let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2

  let [@ocamlformat "disable"] print ppf (a, b) = Format.fprintf ppf " (%a, @ %a)" A.print a B.print b
end

module Make_map (T : Thing) (Set : Set_plus_stdlib with type elt = T.t) = struct
  include Map.Make [@inlined hint] (T)
  module Set = Set

  let of_list l = List.fold_left (fun map (id, v) -> add id v map) empty l

  let disjoint_union ?eq ?print m1 m2 =
    ignore print;
    union
      (fun _id v1 v2 ->
        let ok = match eq with None -> false | Some eq -> eq v1 v2 in
        if not ok
        then
          (* let _err = match print with | None -> Format.asprintf
             "Map.disjoint_union %a" T.print id | Some print -> Format.asprintf
             "Map.disjoint_union %a => %a <> %a" T.print id print v1 print v2
             in *)
          invalid_arg "disjoint_union"
        else Some v1)
      m1 m2

  let map_keys f m = of_list (List.map (fun (k, v) -> f k, v) (bindings m))

  let [@ocamlformat "disable"] print print_datum ppf t =
    let module Lmap = Lmap.Make (T) in
    Lmap.print print_datum ppf (Lmap.of_list (bindings t))

  let print_debug = print

  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty

  let diff_domains t1 t2 =
    if is_empty t2
    then t1
    else
      merge
        (fun _key datum1 datum2 ->
          match datum1, datum2 with
          | None, None -> None
          | Some datum1, None -> Some datum1
          | None, Some _datum2 -> None
          | Some _datum1, Some _datum2 -> None)
        t1 t2

  let inter f t1 t2 =
    merge
      (fun key datum1_opt datum2_opt ->
        match datum1_opt, datum2_opt with
        | None, None | None, Some _ | Some _, None -> None
        | Some datum1, Some datum2 -> Some (f key datum1 datum2))
      t1 t2

  exception Found_common_element

  let inter_domain_is_non_empty t1 t2 =
    match
      merge
        (fun _ datum1_opt datum2_opt ->
          match datum1_opt, datum2_opt with
          | None, None | None, Some _ | Some _, None -> None
          | Some _, Some _ -> raise Found_common_element)
        t1 t2
    with
    | (_ : _ t) -> false
    | exception Found_common_element -> true

  exception More_than_one_binding

  let get_singleton_exn t =
    (* Not as fast as being in the stdlib, but doesn't allocate. *)
    if is_empty t
    then raise Not_found
    else
      try
        let (_ : int) =
          fold
            (fun _key _elt iter_count ->
              if iter_count > 0 then raise More_than_one_binding else 1)
            t 0
        in
        choose t
      with More_than_one_binding -> raise Not_found

  let get_singleton t =
    match get_singleton_exn t with
    | exception Not_found -> None
    | binding -> Some binding

  let replace _ _ _ : _ t = Misc.fatal_error "Not yet implemented"

  let map_sharing f t =
    let changed = ref false in
    let t' =
      map
        (fun v ->
          let v' = f v in
          if not (v == v') then changed := true;
          v')
        t
    in
    if not !changed then t else t'
end
[@@inline always]

module Make_set (T : Thing_no_hash) : Set_plus_stdlib with type elt = T.t =
struct
  include Set.Make [@inlined hint] (T)

  let [@ocamlformat "disable"] print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let of_list l =
    match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))

  let rec union_list ts =
    match ts with [] -> empty | t :: ts -> union t (union_list ts)

  exception More_than_one_element

  let get_singleton t =
    (* Not as fast as being in the stdlib, but doesn't allocate. *)
    if is_empty t
    then None
    else
      try
        let (_ : int) =
          fold
            (fun _elt iter_count ->
              if iter_count > 0 then raise More_than_one_element else 1)
            t 0
        in
        choose_opt t
      with More_than_one_element -> None
end
[@@inline always]

module Make (T : Thing) = struct
  module T = T
  include T
  module Set = Make_set (T)
  module Map = Make_map (T) (Set)
end
[@@inline always]

module Make_pair (T1 : S) (T2 : S) = struct
  module Pair = Pair (T1.T) (T2.T)
  include Make (Pair)

  let create_from_cross_product t1_set t2_set =
    T1.Set.fold
      (fun t1 result ->
        T2.Set.fold (fun t2 result -> Set.add (t1, t2) result) t2_set result)
      t1_set Set.empty
end
