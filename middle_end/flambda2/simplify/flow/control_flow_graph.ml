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

type t =
  { dummy_toplevel_cont : Continuation.t;
    callers : Continuation.Set.t Continuation.Map.t;
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

(* This does not need to be tail-rec as other parts of flambda2 are already
   not tail-rec in the number of nested continuations. *)
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

let compute_transitive_parents t =
  map_fold_on_children t
    (fun k acc ->
       let acc = Continuation.Set.add k acc in
       acc, acc)
    Continuation.Set.empty

let compute_added_extra_args added_extra_args t =
  map_fold_on_children t
    (fun k available ->
       ( Variable.Set.union (Continuation.Map.find k added_extra_args) available,
         available ))
    Variable.Set.empty

let compute_continuation_extra_args_for_aliases ~speculative ~required_names
    ~(source_info : T.Acc.t) doms t : T.Continuation_param_aliases.t Continuation.Map.t =
  let available_variables = compute_available_variables ~source_info t in
  let transitive_parents = compute_transitive_parents t in
  let remove_vars_in_scope_of k var_set =
    let elt : T.Continuation_info.t = Continuation.Map.find k source_info.map in
    let res =
      Variable.Set.diff var_set (Continuation.Map.find k available_variables)
    in
    Variable.Set.diff res elt.defined
  in
  let q_is_empty, pop, push =
    let q = Queue.create () in
    let q_s = ref Continuation.Set.empty in
    ( (fun () -> Queue.is_empty q),
      (fun () ->
         let k = Queue.pop q in
         q_s := Continuation.Set.remove k !q_s;
         k),
      fun k ->
        if not (Continuation.Set.mem k !q_s)
        then (
          Queue.add k q;
          q_s := Continuation.Set.add k !q_s) )
  in
  let init =
    Continuation.Map.mapi
      (fun k elt ->
         let s =
           List.fold_left
             (fun acc param ->
                let dom =
                  match Variable.Map.find param doms with
                  | exception Not_found ->
                    if Name.Set.mem (Name.var param) required_names
                    then
                      Misc.fatal_errorf "Dom not found for: %a@." Variable.print
                        param
                    else param
                  | dom -> dom
                in
                if Variable.equal param dom
                then acc
                else Variable.Set.add dom acc)
             Variable.Set.empty
             (Bound_parameters.vars elt.T.Continuation_info.params)
         in
         let s = remove_vars_in_scope_of k s in
         if not (Variable.Set.is_empty s) then push k;
         s)
      source_info.map
  in
  let res = ref init in
  while not (q_is_empty ()) do
    let k = pop () in
    let elt = Continuation.Map.find k source_info.map in
    let aliases_needed = Continuation.Map.find k !res in
    let callers =
      match Continuation.Map.find k t.callers with
      | exception Not_found ->
        Misc.fatal_errorf "Callers not found for: %a" Continuation.print k
      | callers -> callers
    in
    let callers =
      if not elt.recursive
      then callers
      else
        Continuation.Set.filter
          (fun caller ->
             not
               (Continuation.Set.mem k
                  (Continuation.Map.find caller transitive_parents)))
          callers
    in
    Continuation.Set.iter
      (fun caller ->
         let old_aliases_needed = Continuation.Map.find caller !res in
         let new_aliases_needed =
           Variable.Set.union old_aliases_needed
             (remove_vars_in_scope_of caller aliases_needed)
         in
         if not (Variable.Set.equal old_aliases_needed new_aliases_needed)
         then (
           res := Continuation.Map.add caller new_aliases_needed !res;
           push caller))
      callers
  done;
  let extra_args_for_toplevel_cont =
    Continuation.Map.find t.dummy_toplevel_cont !res
  in
  (* When doing speculative inlining, the flow analysis only has access to the
     inlined body of the function being (speculatively) inlined. Thus, it is
     possible for a canonical alias to be defined outside the inliend body (a
     typical occurrence would be a String length compute before the call of the
     inlined funciton, but shared with the uses inside the funciton by the cse).

     Note that while this is true for aliases, we do not need a similar mechanism
     for mutable unboxing, since when doing mutable unboxing, we require that we
     have seen the creation of the block. *)
  if not speculative && not (Variable.Set.is_empty extra_args_for_toplevel_cont)
  then
    Format.eprintf
      "ERROR:@\nToplevel continuation cannot have needed extra argument for aliases: %a@."
      Variable.Set.print extra_args_for_toplevel_cont;
  let added_extra_args = !res in
  let available_added_extra_args =
    compute_added_extra_args added_extra_args t
  in
  Continuation.Map.mapi
    (fun k aliases_needed ->
       let available = Continuation.Map.find k available_added_extra_args in
       let extra_args_for_aliases =
         Variable.Set.diff aliases_needed available
       in
       let elt = Continuation.Map.find k source_info.map in
       let exception_handler_first_param : Variable.t option =
         if elt.is_exn_handler
         then
           match Bound_parameters.to_list elt.params with
           | [] ->
             Misc.fatal_errorf
               "exception handler continuation %a must have at least one \
                parameter"
               Continuation.print k
           | first :: _ -> Some (Bound_parameter.var first)
         else None
       in
       (* For exception continuations the first parameter cannot be removed, so
          instead of rewriting the parameter to its dominator, we instead
          rewrite every alias to the exception parameter *)
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
              match Variable.Map.find param doms with
              | exception Not_found -> removed, lets_to_introduce
              | alias -> (
                  if Variable.equal param alias
                  then removed, lets_to_introduce
                  else
                    match exception_handler_first_param_aliased with
                    | None ->
                      let removed = Variable.Set.add param removed in
                      let lets_to_introduce =
                        Variable.Map.add param alias lets_to_introduce
                      in
                      removed, lets_to_introduce
                    | Some (aliased_to, exception_param) ->
                      let is_first_exception_param =
                        Variable.equal exception_param param
                      in
                      if is_first_exception_param
                      then removed, lets_to_introduce
                      else
                        let alias =
                          if Variable.equal alias aliased_to
                          then exception_param
                          else alias
                        in
                        let removed = Variable.Set.add param removed in
                        let lets_to_introduce =
                          Variable.Map.add param alias lets_to_introduce
                        in
                        removed, lets_to_introduce))
           (Variable.Set.empty, Variable.Map.empty)
           (Bound_parameters.vars elt.params)
       in
       let recursive_continuation_wrapper : T.Continuation_param_aliases.recursive_continuation_wrapper =
         if elt.recursive && not (Variable.Set.is_empty extra_args_for_aliases)
         then Wrapper_needed
         else No_wrapper
       in
       let res : T.Continuation_param_aliases.t = {
         extra_args_for_aliases;
         removed_aliased_params_and_extra_params;
         lets_to_introduce;
         recursive_continuation_wrapper
       } in
       res)
    added_extra_args

module Dot = struct
  let node_id ~ctx ppf (cont : Continuation.t) =
    Format.fprintf ppf "node_%d_%d" ctx (cont :> int)

  let node ?(extra_args = Variable.Set.empty) ?(info = "") ~df ~pp_node ~ctx
      () ppf cont =
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
    Format.fprintf ppf "%a [label=\"%a %s %s%s\" %s %s];@\n" (node_id ~ctx)
      cont Continuation.print cont params
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
                continuation_parameters.T.Continuation_param_aliases.extra_args_for_aliases)
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
         Continuation.Set.iter
           (fun dst -> edge ~ctx ~color ppf src dst)
           dst_set)
      edge_map

  let edges' ~ctx ~color ppf edge_map =
    Continuation.Map.iter
      (fun src dst -> edge ~ctx ~color ppf src dst)
      edge_map

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
