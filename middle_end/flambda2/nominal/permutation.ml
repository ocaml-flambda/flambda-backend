(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The invariant checks are extremely slow and unlikely to be generally
   useful. *)
let check_invariants = false

module Make (N : Container_types.S) = struct
  type t =
    { forwards : N.t N.Map.t;
      backwards : N.t N.Map.t
    }

  let empty = { forwards = N.Map.empty; backwards = N.Map.empty }

  let inverse { forwards; backwards } =
    { forwards = backwards; backwards = forwards }

  let [@ocamlformat "disable"] print ppf { forwards; backwards; } =
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      (N.Map.print N.print) backwards

  let is_empty t = N.Map.is_empty t.forwards

  let[@inline always] invariant { forwards; backwards } =
    if check_invariants
    then (
      let is_bijection map =
        let domain = N.Map.keys map in
        let range_list = N.Map.data map in
        let range = N.Set.of_list range_list in
        N.Set.equal domain range
      in
      assert (is_bijection forwards);
      assert (N.Map.cardinal forwards = N.Map.cardinal backwards);
      assert (
        N.Map.for_all
          (fun n1 n2 ->
            assert (not (N.equal n1 n2));
            match N.Map.find n2 backwards with
            | exception Not_found -> false
            | n1' -> N.equal n1 n1')
          forwards))

  let apply t n =
    match N.Map.find n t.forwards with exception Not_found -> n | n -> n

  let apply_backwards t n =
    match N.Map.find n t.backwards with exception Not_found -> n | n -> n

  let add_to_map n1 n2 map =
    if N.equal n1 n2 then N.Map.remove n1 map else N.Map.add n1 n2 map

  let[@inline always] post_swap t n1 n2 =
    let n1' = apply_backwards t n1 in
    let n2' = apply_backwards t n2 in
    let forwards = add_to_map n1' n2 (add_to_map n2' n1 t.forwards) in
    let backwards = add_to_map n2 n1' (add_to_map n1 n2' t.backwards) in
    let t = { forwards; backwards } in
    invariant t;
    t

  (* CR-someday lmaurer: Define [N.Map.left_union] so we don't have this Some
     silliness. *)
  let left_union map1 map2 = N.Map.union (fun _k l _r -> Some l) map1 map2

  let compose ~second ~first =
    (* Find the triples [n1, n2, n3] where [first n1 = n2] and [second n2 =
       n3]. *)
    let chained =
      N.Map.inter (fun _n2 n1 n3 -> n1, n3) first.backwards second.forwards
    in
    (* Take the union of the forward directions, taking the first in case of a
       collision (since the first is the one that will actually act on the
       key) *)
    let forwards = left_union first.forwards second.forwards in
    (* Add a correction for each chained triple [n1, n2, n3]. The above left
       union maps [n1] to [n2], and we need it to map to [n3] instead. One might
       worry that the left union also includes an erroneous binding from [n2] to
       [n3], but this is not so: We know that [first n2 <> n2] because [first]
       is a permutation and [first n1 = n2]. Therefore [first.forwards] must
       have [n2] as a key, so our left union already clobbered the binding from
       [n2] to [n3] that appeared in [second.forwards].

       Unfortunately, these keys are in no particular structure, so we just have
       to [fold] and add them one at a time. *)
    let forwards =
      N.Map.fold (fun _ (n1, n3) -> add_to_map n1 n3) chained forwards
    in
    (* Similarly, take the union of the backward directions, only now the second
       permutation wins because it acts first *)
    let backwards = left_union second.backwards first.backwards in
    (* Again, correct for each chained triple *)
    let backwards =
      N.Map.fold (fun _ (n1, n3) -> add_to_map n3 n1) chained backwards
    in
    { forwards; backwards }

  let compose_one ~first n1 n2 = post_swap first n1 n2

  let compose_one_fresh t n1 ~fresh:n2 =
    let n1' = apply_backwards t n1 in
    let forwards = add_to_map n1' n2 (add_to_map n2 n1 t.forwards) in
    let backwards = add_to_map n2 n1' (add_to_map n1 n2 t.backwards) in
    let t = { forwards; backwards } in
    invariant t;
    t
end
