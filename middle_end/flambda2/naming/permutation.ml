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

[@@@ocaml.warning "+a-4-30-40-41-42"]

let check_invariants = false

module Make (N : Container_types.S) = struct
  type t = {
    forwards : N.t N.Map.t;
    backwards : N.t N.Map.t;
  }

  let empty =
    { forwards = N.Map.empty;
      backwards = N.Map.empty;
    }

  let [@ocamlformat "disable"] print ppf { forwards; backwards; } =
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      (N.Map.print N.print) backwards

  let is_empty t =
    N.Map.is_empty t.forwards

  let [@inline always] invariant { forwards; backwards; } =
    if check_invariants then begin
      let is_bijection map =
        let domain = N.Map.keys map in
        let range_list = N.Map.data map in
        let range = N.Set.of_list range_list in
        N.Set.equal domain range
      in
      assert (is_bijection forwards);
      assert (N.Map.cardinal forwards = N.Map.cardinal backwards);
      assert (N.Map.for_all (fun n1 n2 ->
          assert (not (N.equal n1 n2));
          match N.Map.find n2 backwards with
          | exception Not_found -> false
          | n1' -> N.equal n1 n1')
        forwards)
    end

  let apply t n =
    match N.Map.find n t.forwards with
    | exception Not_found -> n
    | n -> n

  let apply_backwards t n =
    match N.Map.find n t.backwards with
    | exception Not_found -> n
    | n -> n

  let add_to_map n1 n2 map =
    if N.equal n1 n2 then N.Map.remove n1 map
    else N.Map.add n1 n2 map

  let [@inline always] flip t =
    { forwards = t.backwards;
      backwards = t.forwards;
    }

  let [@inline always] post_swap t n1 n2 =
    let n1' = apply_backwards t n1 in
    let n2' = apply_backwards t n2 in
    let forwards = add_to_map n1' n2 (add_to_map n2' n1 t.forwards) in
    let backwards = add_to_map n2 n1' (add_to_map n1 n2' t.backwards) in
    let t = { forwards; backwards; } in
    invariant t;
    t

  let pre_swap t n1 n2 =
    flip (post_swap (flip t) n1 n2)

  let rec compose ~second ~first =
    match N.Map.choose second.forwards with
    | exception Not_found ->
      invariant first;
      first
    | n1, n2 ->
      let first = post_swap first n1 n2 in
      let second = pre_swap second n1 n2 in
      compose ~second ~first

  let compose_one ~first n1 n2 =
    post_swap first n1 n2

  let compose_one_fresh t n1 ~fresh:n2 =
    let n1' = apply_backwards t n1 in
    let forwards = add_to_map n1' n2 (add_to_map n2 n1 t.forwards) in
    let backwards = add_to_map n2 n1' (add_to_map n1 n2 t.backwards) in
    let t = { forwards; backwards; } in
    invariant t;
    t
end
