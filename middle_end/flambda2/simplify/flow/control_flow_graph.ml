(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module T = Flow_types
module G = Strongly_connected_components.Make (Continuation)

type t =
  { dummy_toplevel_cont : Continuation.t;
    callers : G.directed_graph;
    parents : Continuation.t Continuation.Map.t;
    children : Continuation.Set.t Continuation.Map.t
  }

let create ~dummy_toplevel_cont { T.Acc.map; _ } =
  let parents =
    Continuation.Map.filter_map
      (fun _ (elt : T.Continuation_info.t) -> elt.parent_continuation)
      map
  in
  let children =
    Continuation.Map.fold
      (fun k parent acc ->
        Continuation.Map.update parent
          (function
            | None -> Some (Continuation.Set.singleton k)
            | Some set -> Some (Continuation.Set.add k set))
          acc)
      parents Continuation.Map.empty
  in
  let callers =
    Continuation.Map.fold
      (fun caller (elt : T.Continuation_info.t) acc ->
        let acc =
          Continuation.Map.merge
            (fun _callee acc args ->
              match acc, args with
              | None, None -> assert false
              | Some set, None -> Some set
              | None, Some _ -> Some (Continuation.Set.singleton caller)
              | Some set, Some _ -> Some (Continuation.Set.add caller set))
            acc elt.apply_cont_args
        in
        acc)
      map
      (Continuation.Map.singleton dummy_toplevel_cont Continuation.Set.empty)
  in
  { dummy_toplevel_cont; callers; parents; children }

(* This does not need to be tail-rec as other parts of flambda2 are already not
   tail-rec in the number of nested continuations. *)
let map_fold_on_children { children; dummy_toplevel_cont; _ } f acc =
  let rec aux k acc =
    let acc, to_add = f k acc in
    let map = Continuation.Map.singleton k to_add in
    match Continuation.Map.find k children with
    | exception Not_found -> map
    | s ->
      Continuation.Set.fold
        (fun child map -> Continuation.Map.disjoint_union map (aux child acc))
        s map
  in
  aux dummy_toplevel_cont acc

let compute_available_variables ~(source_info : T.Acc.t) t =
  map_fold_on_children t
    (fun k acc ->
      let elt = Continuation.Map.find k source_info.map in
      let extra_vars =
        match Continuation.Map.find k source_info.extra with
        | exception Not_found -> Variable.Set.empty
        | epa ->
          let extra_params =
            Continuation_extra_params_and_args.extra_params epa
          in
          Bound_parameters.var_set extra_params
      in
      let acc =
        Variable.Set.union
          (Variable.Set.union acc (Bound_parameters.var_set elt.params))
          extra_vars
      in
      acc, acc)
    Variable.Set.empty

let compute_added_extra_args added_extra_args t =
  map_fold_on_children t
    (fun k available ->
      ( Variable.Set.union (Continuation.Map.find k added_extra_args) available,
        available ))
    Variable.Set.empty

let fixpoint t ~init ~f =
  let components = G.connected_components_sorted_from_roots_to_leaf t.callers in
  let res =
    Array.fold_left
      (fun res component ->
        match component with
        | G.No_loop callee -> (
          match Continuation.Map.find callee res with
          | exception Not_found -> res
          | callee_set ->
            Continuation.Set.fold
              (fun caller res ->
                let caller_set = Continuation.Map.find caller res in
                let caller_new_set =
                  f ~caller ~caller_set ~callee ~callee_set
                in
                Continuation.Map.add caller caller_new_set res)
              (Continuation.Map.find callee t.callers)
              res)
        | G.Has_loop conts ->
          let q = Queue.create () in
          List.iter (fun k -> Queue.add k q) conts;
          (* We enforce the invariant that [q] contains no duplicates, and that
             [!q_s] precisely contains the continuations in [conts] that are not
             in [q]. Besides ensuring we don't add the same element twice to
             [q], this enforces as well that only continuations in the current
             strongly-connected component, that is, the continuations in
             [conts], can ever be in [q]. *)
          let q_s = ref Continuation.Set.empty in
          let cur = ref res in
          while not (Queue.is_empty q) do
            let callee = Queue.pop q in
            q_s := Continuation.Set.add callee !q_s;
            let callee_set = Continuation.Map.find callee !cur in
            let callers =
              match Continuation.Map.find callee t.callers with
              | exception Not_found ->
                Misc.fatal_errorf "Callers not found for: %a" Continuation.print
                  callee
              | callers -> callers
            in
            Continuation.Set.iter
              (fun caller ->
                let caller_set = Continuation.Map.find caller !cur in
                let caller_new_set =
                  f ~caller ~caller_set ~callee ~callee_set
                in
                if not (Variable.Set.equal caller_set caller_new_set)
                then (
                  cur := Continuation.Map.add caller caller_new_set !cur;
                  if Continuation.Set.mem caller !q_s
                  then (
                    Queue.add caller q;
                    q_s := Continuation.Set.remove caller !q_s)))
              callers
          done;
          !cur)
      init components
  in
  res

let extra_args_for_aliases_overapproximation ~required_names
    ~(source_info : T.Acc.t) ~unboxed_blocks doms t =
  let available_variables = compute_available_variables ~source_info t in
  let remove_vars_in_scope_of k var_set =
    let elt : T.Continuation_info.t = Continuation.Map.find k source_info.map in
    let res =
      Variable.Set.diff var_set (Continuation.Map.find k available_variables)
    in
    Variable.Set.diff res elt.defined
  in
  (* We remove aliases to unboxed blocks, so that they won't try to be passed as
     extra args. These would normally be deleted, except in recursive
     continuations, where they would still be added and cause the code to
     fail. *)
  let init =
    Continuation.Map.mapi
      (fun k elt ->
        let s =
          List.fold_left
            (fun acc param ->
              match Variable.Map.find param doms with
              | exception Not_found ->
                if Name.Set.mem (Name.var param) required_names
                then
                  Misc.fatal_errorf "Dom not found for: %a@." Variable.print
                    param
                else acc
              | dom ->
                if Variable.equal param dom
                   || Variable.Set.mem dom unboxed_blocks
                then acc
                else Variable.Set.add dom acc)
            Variable.Set.empty
            (Bound_parameters.vars elt.T.Continuation_info.params)
        in
        let s = remove_vars_in_scope_of k s in
        s)
      source_info.map
  in
  let added_extra_args =
    fixpoint t ~init
      ~f:(fun
           ~caller
           ~caller_set:caller_aliases_needed
           ~callee:_
           ~callee_set:callee_aliases_needed
         ->
        Variable.Set.union caller_aliases_needed
          (remove_vars_in_scope_of caller callee_aliases_needed))
  in
  added_extra_args

let minimize_extra_args_for_one_continuation ~(source_info : T.Acc.t)
    ~unboxed_blocks ~available_added_extra_args doms k aliases_needed =
  let available = Continuation.Map.find k available_added_extra_args in
  let extra_args_for_aliases = Variable.Set.diff aliases_needed available in
  let elt = Continuation.Map.find k source_info.map in
  let exception_handler_first_param : Variable.t option =
    if elt.is_exn_handler
    then
      match Bound_parameters.to_list elt.params with
      | [] ->
        Misc.fatal_errorf
          "exception handler continuation %a must have at least one parameter"
          Continuation.print k
      | first :: _ -> Some (Bound_parameter.var first)
    else None
  in
  (* For exception continuations the first parameter cannot be removed, so
     instead of rewriting the parameter to its dominator, we instead rewrite
     every alias to the exception parameter *)
  let extra_args_for_aliases, exception_handler_first_param_aliased =
    match exception_handler_first_param with
    | None -> extra_args_for_aliases, None
    | Some exception_param -> (
      match Variable.Map.find exception_param doms with
      | exception Not_found -> extra_args_for_aliases, None
      | alias ->
        if Variable.equal exception_param alias
        then extra_args_for_aliases, None
        else
          ( Variable.Set.remove alias extra_args_for_aliases,
            Some (alias, exception_param) ))
  in
  let removed_aliased_params_and_extra_params, lets_to_introduce =
    List.fold_left
      (fun (removed, lets_to_introduce) param ->
        let default alias =
          let removed = Variable.Set.add param removed in
          let lets_to_introduce =
            if Variable.Set.mem alias unboxed_blocks
            then lets_to_introduce
            else Variable.Map.add param alias lets_to_introduce
          in
          removed, lets_to_introduce
        in
        match Variable.Map.find param doms with
        | exception Not_found -> removed, lets_to_introduce
        | alias -> (
          if Variable.equal param alias
          then removed, lets_to_introduce
          else
            match exception_handler_first_param_aliased with
            | None -> default alias
            | Some (aliased_to, exception_param) ->
              let is_first_exception_param =
                Variable.equal exception_param param
              in
              if is_first_exception_param
              then removed, lets_to_introduce
              else if Variable.equal alias aliased_to
              then default exception_param
              else default alias))
      (Variable.Set.empty, Variable.Map.empty)
      (Bound_parameters.vars elt.params)
  in
  let recursive_continuation_wrapper :
      T.Continuation_param_aliases.recursive_continuation_wrapper =
    if elt.recursive && not (Variable.Set.is_empty extra_args_for_aliases)
    then Wrapper_needed
    else No_wrapper
  in
  let res : T.Continuation_param_aliases.t =
    { extra_args_for_aliases;
      removed_aliased_params_and_extra_params;
      lets_to_introduce;
      recursive_continuation_wrapper
    }
  in
  res

let minimize_extra_args_for_aliases ~source_info ~unboxed_blocks doms
    added_extra_args t =
  let available_added_extra_args =
    compute_added_extra_args added_extra_args t
  in
  Continuation.Map.mapi
    (minimize_extra_args_for_one_continuation ~source_info ~unboxed_blocks
       ~available_added_extra_args doms)
    added_extra_args

let compute_continuation_extra_args_for_aliases ~speculative ~required_names
    ~source_info ~unboxed_blocks doms t :
    T.Continuation_param_aliases.t Continuation.Map.t =
  let added_extra_args =
    extra_args_for_aliases_overapproximation ~required_names ~source_info
      ~unboxed_blocks doms t
  in
  let extra_args_for_toplevel_cont =
    Continuation.Map.find t.dummy_toplevel_cont added_extra_args
  in
  (* When doing speculative inlining, the flow analysis only has access to the
     inlined body of the function being (speculatively) inlined. Thus, it is
     possible for a canonical alias to be defined outside the inliend body (a
     typical occurrence would be a String length compute before the call of the
     inlined funciton, but shared with the uses inside the funciton by the cse).

     Note that while this is true for aliases, we do not need a similar
     mechanism for mutable unboxing, since when doing mutable unboxing, we
     require that we have seen the creation of the block. *)
  if (not speculative)
     && not (Variable.Set.is_empty extra_args_for_toplevel_cont)
  then
    Misc.fatal_errorf
      "ERROR:@\n\
       Toplevel continuation cannot have needed extra argument for aliases: \
       %a@."
      Variable.Set.print extra_args_for_toplevel_cont;
  let extra_args_for_aliases =
    minimize_extra_args_for_aliases ~source_info ~unboxed_blocks doms
      added_extra_args t
  in
  extra_args_for_aliases

module Dot = struct
  let node_id ~ctx ppf (cont : Continuation.t) =
    Format.fprintf ppf "node_%d_%d" ctx (cont :> int)

  let node ?(extra_args = Variable.Set.empty) ?(info = "") ~df ~pp_node ~ctx ()
      ppf cont =
    let params, shape =
      match Continuation.Map.find cont df.T.Acc.map with
      | exception Not_found -> "[ none ]", ""
      | elt ->
        let params =
          Format.asprintf "[%a]"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
               Variable.print)
            (Bound_parameters.vars elt.params)
        in
        let shape = if elt.recursive then "shape=record" else "" in
        params, shape
    in
    Format.fprintf ppf "%a [label=\"%a %s %s%s\" %s %s];@\n" (node_id ~ctx) cont
      Continuation.print cont params
      (String.map
         (function '{' -> '[' | '}' -> ']' | c -> c)
         (Format.asprintf "%a" Variable.Set.print extra_args))
      (String.map
         (function '{' -> '[' | '}' -> ']' | c -> c)
         (Format.asprintf "%a" pp_node cont))
      shape info

  let nodes ~df ~ctx ~return_continuation ~exn_continuation
      ~continuation_parameters ~pp_node ppf cont_map =
    Continuation.Set.iter
      (fun cont ->
        let extra_args =
          Option.map
            (fun continuation_parameters ->
              continuation_parameters
                .T.Continuation_param_aliases.extra_args_for_aliases)
            (Continuation.Map.find_opt cont continuation_parameters)
        in
        let info =
          if Continuation.equal return_continuation cont
          then "color=blue"
          else if Continuation.equal exn_continuation cont
          then "color=red"
          else ""
        in
        node ?extra_args ~df ~ctx ~info ~pp_node () ppf cont)
      cont_map

  let edge ~ctx ~color ppf src dst =
    Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) dst
      (node_id ~ctx) src color

  let edges ~ctx ~color ppf edge_map =
    Continuation.Map.iter
      (fun src dst_set ->
        Continuation.Set.iter (fun dst -> edge ~ctx ~color ppf src dst) dst_set)
      edge_map

  let edges' ~ctx ~color ppf edge_map =
    Continuation.Map.iter (fun src dst -> edge ~ctx ~color ppf src dst) edge_map

  let print ~ctx ~df ~print_name ppf ~return_continuation ~exn_continuation
      ?(pp_node = fun _ppf _cont -> ()) ~continuation_parameters (t : t) =
    let dummy_toplevel_cont = t.dummy_toplevel_cont in
    let all_conts =
      Continuation.Map.fold
        (fun cont callers acc ->
          Continuation.Set.add cont (Continuation.Set.union callers acc))
        t.callers
        (Continuation.Set.of_list
           [dummy_toplevel_cont; return_continuation; exn_continuation])
    in
    let all_conts =
      Continuation.Map.fold
        (fun cont _parent acc -> Continuation.Set.add cont acc)
        t.parents all_conts
    in
    Flambda_colours.without_colours ~f:(fun () ->
        Format.fprintf ppf
          "subgraph cluster_%d { label=\"%s\";@\n%a%a@\n%a@\n%a@\n}@." ctx
          print_name
          (node ~df ~ctx ~pp_node ())
          dummy_toplevel_cont
          (nodes ~df ~return_continuation ~exn_continuation ~ctx
             ~continuation_parameters ~pp_node)
          all_conts
          (edges' ~ctx ~color:"green")
          t.parents
          (edges ~ctx ~color:"black")
          t.callers)
end
