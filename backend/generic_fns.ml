(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm
open Cmm_helpers
module CU = Compilation_unit

let considered_as_small_threshold = 20

module Partition = struct
  type t =
    | Small
    | Curry of int
    | Apply of int
    | Send of int

  let rank = function Small -> 0 | Curry _ -> 1 | Apply _ -> 2 | Send _ -> 3

  let curry x = Curry x

  let send x = Send x

  let apply x = Apply x

  let create cat arity =
    let arity = List.length arity in
    if arity <= considered_as_small_threshold then Small else cat arity

  let compare a b =
    let c = compare (rank a) (rank b) in
    if c <> 0
    then c
    else
      match a, b with
      | Small, Small -> 0
      | Curry a, Curry b -> compare a b
      | Apply a, Apply b -> compare a b
      | Send a, Send b -> compare a b
      | _ -> assert false

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = struct
    include Map.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let of_seq_multi seq =
      Seq.fold_left
        (fun tbl (key, elt) ->
          update key
            (function None -> Some [elt] | Some s -> Some (elt :: s))
            tbl)
        empty seq
  end

  module Hashtbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal a b = compare a b = 0

    let hash x = Hashtbl.hash x
  end)

  let to_string = function
    | Curry arity -> "curry_" ^ string_of_int arity
    | Apply arity -> "apply_" ^ string_of_int arity
    | Send arity -> "send_" ^ string_of_int arity
    | Small -> "small"

  let name' x = "_cached_generic_functions_" ^ to_string x

  let name x = "caml" ^ name' x

  let to_cu x = CU.create CU.Prefix.empty (CU.Name.of_string (name' x))
end

module Tbl0 = struct
  type t =
    { curry : (Lambda.function_kind * machtype list * machtype, unit) Hashtbl.t;
      apply : (machtype list * machtype * Cmx_format.alloc_mode, unit) Hashtbl.t;
      send  : (machtype list * machtype * Cmx_format.alloc_mode, unit) Hashtbl.t
    }

  let make () =
    { curry = Hashtbl.create 10;
      apply = Hashtbl.create 10;
      send = Hashtbl.create 10
    }

  let add_uncached t Cmx_format.{ curry_fun; apply_fun; send_fun } =
    List.iter (fun f -> Hashtbl.replace t.curry f ()) curry_fun;
    List.iter (fun f -> Hashtbl.replace t.apply f ()) apply_fun;
    List.iter (fun f -> Hashtbl.replace t.send f ()) send_fun

  let of_fns fns =
    let t = make () in
    add_uncached t fns;
    t

  let entries t : Cmx_format.generic_fns =
    let sorted_keys tbl =
      let keys = Hashtbl.fold (fun k () acc -> k :: acc) tbl [] in
      List.sort compare keys
    in
    { curry_fun = sorted_keys t.curry;
      apply_fun = sorted_keys t.apply;
      send_fun = sorted_keys t.send
    }
end

module Cache = struct
  type send =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Cmx_format.alloc_mode

  type apply =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Cmx_format.alloc_mode

  type curry =
    Lambda.function_kind
    * Cmm.machtype_component array list
    * Cmm.machtype_component array

  let has_singleton_layout_value = function [| Val |] -> true | _ -> false

  let only_concerns_values ~arity ~result =
    has_singleton_layout_value result
    && List.for_all has_singleton_layout_value arity

  let len_arity arity =
    List.fold_left
      (fun acc a -> match a with [| Val |] -> acc + 1 | _ -> acc)
      0 arity

  let max_send = 20

  let max_tuplify = 100

  let mem_curry (kind, arity, result) =
    (* For now we don't cache generic functions involving unboxed types *)
    if not (only_concerns_values ~arity ~result)
    then false
    else
      match kind with
      | Lambda.Tupled ->
        let l = len_arity arity in
        2 <= l && l <= considered_as_small_threshold
      | Lambda.Curried { nlocal } ->
        let l = len_arity arity in
        let in_bounds = 2 <= l && l <= Lambda.max_arity () in
        if not in_bounds
        then false
        else if nlocal = 0
        then true
        else if nlocal = 1
        then true
        else if nlocal = l
        then true
        else false

  let mem_send (arity, result, alloc) =
    (* For now we don't cache generic functions involving unboxed types *)
    if not (only_concerns_values ~arity ~result)
    then false
    else
      match alloc with
      | Cmx_format.Alloc_local -> len_arity arity = 0
      | Cmx_format.Alloc_heap -> len_arity arity <= max_send

  let mem_apply (arity, result, alloc) =
    (* For now we don't cache generic functions involving unboxed types *)
    if not (only_concerns_values ~arity ~result)
    then false
    else
      match alloc with
      | Cmx_format.Alloc_local ->
        let l = len_arity arity in
        2 <= l && l <= considered_as_small_threshold
      | Cmx_format.Alloc_heap ->
        let l = len_arity arity in
        2 <= l && l <= Lambda.max_arity ()

  let partition_curry (_, arity, _) = Partition.(create curry arity)

  let partition_send (arity, _, _) = Partition.(create send arity)

  let partition_apply (arity, _, _) = Partition.(create apply arity)

  let arity n = List.init n (fun _ -> [| Val |])

  let result = [| Val |]

  let all_curry () =
    let tuplify =
      Seq.init (max_tuplify + 1) (fun n -> Lambda.Tupled, arity n, result)
    in
    let curry =
      Seq.init
        (Lambda.max_arity () + 1)
        (fun n ->
          Seq.init
            (Lambda.max_arity () + 1)
            (fun nlocal -> Lambda.Curried { nlocal }, arity n, result))
      |> Seq.concat
    in
    Seq.append tuplify curry |> Seq.filter mem_curry
    |> Seq.map (fun f -> partition_curry f, f)
    |> Partition.Map.of_seq_multi
    |> Partition.Map.map (fun curry_fun ->
           { Cmx_format.curry_fun; send_fun = []; apply_fun = [] })

  let all_send () =
    let send =
      Seq.init (max_send + 1) (fun n ->
          Seq.cons
            (arity n, result, Cmx_format.Alloc_local)
            (Seq.return (arity n, result, Cmx_format.Alloc_heap)))
      |> Seq.concat
    in
    Seq.filter mem_send send
    |> Seq.map (fun f -> partition_send f, f)
    |> Partition.Map.of_seq_multi
    |> Partition.Map.map (fun send_fun ->
           { Cmx_format.send_fun; curry_fun = []; apply_fun = [] })

  let all_apply () =
    let apply =
      Seq.init
        (Lambda.max_arity () + 1)
        (fun n ->
          Seq.cons
            (arity n, result, Cmx_format.Alloc_local)
            (Seq.return (arity n, result, Cmx_format.Alloc_heap)))
      |> Seq.concat
    in
    Seq.filter mem_apply apply
    |> Seq.map (fun f -> partition_apply f, f)
    |> Partition.Map.of_seq_multi
    |> Partition.Map.map (fun apply_fun ->
           { Cmx_format.apply_fun; send_fun = []; curry_fun = [] })

  let all () =
    (* [is_curry], [is_send] and [is_apply] are also used to determine if a
       generate function was cached. When we generate the cached generated
       functions, we explore the space of all potential candidates and rely on
       these functions to filter out the one that we'll actually generate. It's
       okay to have a search space bigger than needed, however it's not okay to
       have a search space that does not englobe all candidates as it will
       result in weird errors at link-time. We maybe could use Z3 to
       automatically derive a good search space in the future as the filters
       might become more complexed with unboxed types. *)
    assert (considered_as_small_threshold <= max_tuplify);
    assert (considered_as_small_threshold <= max_send);
    assert (considered_as_small_threshold <= Lambda.max_arity ());
    let curry_fns = all_curry () in
    let send_fns = all_send () in
    let apply_fns = all_apply () in
    let out = Hashtbl.create 100 in
    let add f =
      Partition.Map.iter
        (fun key x ->
          let t =
            match Hashtbl.find_opt out key with
            | None ->
              let t = Tbl0.make () in
              Hashtbl.add out key t;
              t
            | Some t -> t
          in
          Tbl0.add_uncached t x)
        f
    in
    List.iter add [curry_fns; send_fns; apply_fns];
    out
end

module Tbl = struct
  include Tbl0

  let add ~imports (t : t) (Cmx_format.{ curry_fun; apply_fun; send_fun } as f)
      =
    if !Flambda_backend_flags.use_cached_generic_functions
    then
      let imports =
        List.fold_left
          (fun acc f ->
            if not (Cache.mem_curry f)
            then (
              Hashtbl.replace t.curry f ();
              acc)
            else Partition.Set.add (Cache.partition_curry f) acc)
          imports curry_fun
      in
      let imports =
        List.fold_left
          (fun acc f ->
            if not (Cache.mem_apply f)
            then (
              Hashtbl.replace t.apply f ();
              acc)
            else Partition.Set.add (Cache.partition_apply f) acc)
          imports apply_fun
      in
      List.fold_left
        (fun acc f ->
          if not (Cache.mem_send f)
          then (
            Hashtbl.replace t.send f ();
            acc)
          else Partition.Set.add (Cache.partition_send f) acc)
        imports send_fun
    else (
      add_uncached t f;
      imports)
end

let default_generic_fns : Cmx_format.generic_fns =
  { curry_fun = [];
    apply_fun =
      [ [typ_val; typ_val], typ_val, Cmx_format.Alloc_heap;
        [typ_val; typ_val; typ_val], typ_val, Cmx_format.Alloc_heap ];
    send_fun = []
  }

(* These apply funs are always present in the main program because the run-time
   system needs them (cf. runtime/<arch>.S) . *)
let compile ~shared tbl =
  if not shared
  then ignore (Tbl.add ~imports:Partition.Set.empty tbl default_generic_fns);
  let ({ curry_fun; apply_fun; send_fun } : Cmx_format.generic_fns) =
    Tbl.entries tbl
  in
  List.concat_map curry_function curry_fun
  @ List.map send_function send_fun
  @ List.map apply_function apply_fun

let imported_units p =
  Partition.Set.to_seq p |> Seq.map Partition.to_cu |> List.of_seq
