(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Heterogenous_list
open With_name

type outcome =
  | Accept
  | Skip

module Make (Iterator : Leapfrog.Iterator) = struct
  type 's stack =
    | Stack_nil : nil stack
    | Stack_cons :
        'a Iterator.t * 'a option ref * ('a -> 's) continuation * 's stack
        -> ('a -> 's) stack

  and 's continuation = 's stack -> unit

  type ('a, 's) instruction =
    | Advance : ('a, 's) instruction
    | Up : ('x, 's) instruction -> ('x, 'a -> 's) instruction
    | Dispatch : ('a, 'b -> 's) instruction
    | Seek :
        'b option ref * 'b Iterator.t * ('a, 's) instruction * string * string
        -> ('a, 's) instruction
    | Open :
        'b Iterator.t
        * 'b option ref
        * ('a, 'b -> 's) instruction
        * ('a, 'b -> 's) instruction
        * string
        * string
        -> ('a, 's) instruction
    | Action : 'a * ('a, 's) instruction -> ('a, 's) instruction
    | Call :
        ('b Constant.hlist -> unit)
        * 'b Option_ref.hlist
        * ('a, 's) instruction
        * string
        * string list
        -> ('a, 's) instruction

  let pp_instruction pp_act ff instr =
    let pp_initiator ppf depth =
      if depth > 0 then Format.pp_print_space ppf ()
    in
    let pp_terminator ppf = Format.fprintf ppf "@]" in
    let rec pp_terminators ppf depth =
      if depth > 0
      then (
        pp_terminator ppf;
        pp_terminators ppf (depth - 1))
    in
    let rec pp_instruction : type s. _ -> (_, s) instruction * int -> unit =
     fun ff (instr, depth) ->
      match instr with
      | Advance ->
        (* Default terminator *)
        pp_terminators ff depth
      | Up instr ->
        let rec print_breaks : type s. _ -> (_, s) instruction -> unit =
         fun n instr ->
          match instr with
          | Up instr -> print_breaks (n + 1) instr
          | Advance | Open _ | Seek _ | Dispatch | Action _ | Call _ ->
            Format.fprintf ff "%a" pp_initiator depth;
            if n > 1
            then Format.fprintf ff "break %d" n
            else Format.fprintf ff "break";
            Format.fprintf ff "%t%a" pp_terminator pp_instruction
              (instr, depth - n)
        in
        print_breaks 1 instr
      | Open (_iterator, _var, instr1, Dispatch, iterator_name, var_name) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for %s in %s:@]%a" pp_initiator
          depth var_name iterator_name pp_instruction
          (instr1, depth + 1)
      | Seek (_var, _iterator, instr, var_name, iterator_name) ->
        Format.fprintf ff "%a@[<v 2>@[<hov 2>for _ in {%s} ⨝ %s:@]%a"
          pp_initiator depth var_name iterator_name pp_instruction
          (instr, depth + 1)
      | Dispatch ->
        Format.fprintf ff "%adispatch%a" pp_initiator depth pp_terminators depth
      | Open (_iterator, _var, instr1, instr2, iterator_name, var_name) ->
        Format.fprintf ff
          "%a@[<v 2>@[<hov 2>open (%s : %s) [@;<1 0>%a@;<1 -2>]@] {%a"
          pp_initiator depth var_name iterator_name pp_instruction (instr2, 0)
          pp_instruction
          (instr1, depth + 1)
      | Action (a, instr) ->
        Format.fprintf ff "%a@[<v 2>%a@]%a" pp_initiator depth pp_act a
          pp_instruction (instr, depth)
      | Call (_f, _l, instr, name, names) ->
        Format.fprintf ff "%a%s (%a)%a" pp_initiator depth name
          (Format.pp_print_list
             ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
             Format.pp_print_string)
          names pp_instruction (instr, depth)
    in
    pp_instruction ff (instr, 0)

  let[@inline always] dispatch ~advance (stack : (_ -> _) stack) =
    let (Stack_cons (iterator, cell, level, next_stack)) = stack in
    match Iterator.current iterator with
    | Some current_key ->
      Iterator.accept iterator;
      cell.contents <- Some current_key;
      level stack
    | None -> advance next_stack

  let[@loop] rec advance : type s. s continuation =
   fun stack ->
    match stack with
    | Stack_nil -> ()
    | Stack_cons (iterator, _, _, _) as stack ->
      Iterator.advance iterator;
      dispatch ~advance stack

  type t = nil continuation

  let[@inline] execute (type a) ~(evaluate : a -> outcome) instruction =
    let rec execute : type s. (a, s) instruction -> s continuation =
     fun instruction stack ->
      match instruction with
      | Advance -> advance stack
      | Up k ->
        let (Stack_cons (_, _, _, stack)) = stack in
        execute k stack
      | Open (iterator, cell, for_each, k, _iterator_name, _cell_name) ->
        Iterator.init iterator;
        execute k (Stack_cons (iterator, cell, execute for_each, stack))
      | Seek (key_ref, iterator, k, _key_ref_name, _iterator_name) -> (
        let key = Option.get !key_ref in
        Iterator.init iterator;
        Iterator.seek iterator key;
        match Iterator.current iterator with
        | Some current_key when Iterator.equal_key iterator current_key key ->
          Iterator.accept iterator;
          execute k stack
        | None | Some _ -> advance stack)
      | Dispatch -> dispatch ~advance stack
      | Action (op, k) -> (
        match (evaluate [@inlined hint]) op with
        | Accept -> execute k stack
        | Skip -> advance stack)
      | Call (f, rs, k, _name, _names) ->
        f (Option_ref.get rs);
        execute k stack
    in
    execute instruction

  let create ~evaluate (instruction : (_, _) instruction) =
    execute ~evaluate instruction

  let run continuation = continuation Stack_nil

  let advance = Advance

  let up i = Up i

  let dispatch = Dispatch

  let seek r it k = Seek (r.value, it.value, k, r.name, it.name)

  let open_ i cell a dispatch =
    Open (i.value, cell.value, a, dispatch, i.name, cell.name)

  let action a k = Action (a, k)

  let call f ~name y k = Call (f, y.values, k, name, y.names)

  let rec refs : type s. s Iterator.hlist -> s Option_ref.hlist = function
    | [] -> []
    | _ :: iterators -> ref None :: refs iterators

  type erev = Erev : 's Iterator.hlist * 's Option_ref.hlist -> erev

  let iterate :
      type a.
      (a Constant.hlist -> unit) ->
      a Iterator.hlist with_names ->
      (_, nil) instruction =
   fun f iterators ->
    let iterator_names = iterators.names in
    let iterators = iterators.values in
    let rec rev0 :
        type s. s Iterator.hlist -> s Option_ref.hlist -> erev -> erev =
     fun iterators refs acc ->
      match iterators, refs with
      | [], [] -> acc
      | iterator :: iterators, r :: refs ->
        let (Erev (rev_iterators, rev_refs)) = acc in
        rev0 iterators refs (Erev (iterator :: rev_iterators, r :: rev_refs))
    in
    let rs = refs iterators in
    let (Erev (rev_iterators, rev_refs)) = rev0 iterators rs (Erev ([], [])) in
    let rec loop :
        type s a.
        (a -> s) Iterator.hlist ->
        (a -> s) Option_ref.hlist ->
        (_, a -> s) instruction ->
        string list ->
        (_, nil) instruction =
     fun iterators refs instruction iterator_names ->
      match iterators, refs, iterator_names with
      | [iterator], [r], name :: _ ->
        open_ { value = iterator; name } { value = r; name = "_" } instruction
          dispatch
      | iterator :: (_ :: _ as iterators), r :: refs, name :: iterator_names ->
        loop iterators refs
          (open_ { value = iterator; name } { value = r; name = "_" }
             instruction dispatch)
          iterator_names
      | _, _, [] ->
        Misc.fatal_errorf "Incorrect number of names for iterators in [iterate]"
    in
    match rev_iterators with
    | [] -> Advance
    | _ :: _ ->
      loop rev_iterators rev_refs
        (call f ~name:"yield"
           { values = rs; names = List.map (fun _ -> "_") iterator_names }
           advance)
        (List.rev iterator_names)

  type 'a iterator =
    | Iterator of ('a Constant.hlist -> unit) ref * nil continuation

  type void = |

  let iterator iterators =
    let evaluate : void -> outcome = function _ -> . in
    let f_ref = ref ignore in
    let f args = !f_ref args in
    Iterator (f_ref, execute ~evaluate (iterate f iterators))

  let[@inline] iter f (Iterator (f_ref, continuation)) =
    f_ref := f;
    continuation Stack_nil;
    f_ref := ignore
end
