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

type outcome =
  | Accept
  | Skip

module Make (Iterator : Leapfrog.Iterator) = struct
  type ('y, 's) stack =
    | Stack_nil : ('y, nil) stack
    | Stack_cons :
        'a Iterator.t
        * 'a option ref
        * ('y, 'a -> 's) continuation
        * ('y, 's) stack
        -> ('y, 'a -> 's) stack

  and 'y suspension =
    | Suspension :
        { stack : ('y, 's) stack;
          continuation : ('y, 's) continuation
        }
        -> 'y suspension

  and ('y, 's) continuation = ('y, 's) stack -> 'y suspension * 'y option

  type ('a, 'y, 's) instruction =
    | Advance : ('a, 'y, 's) instruction
    | Up : ('x, 'y, 's) instruction -> ('x, 'y, 'a -> 's) instruction
    | Dispatch : ('a, 'y, 'b -> 's) instruction
    | Open :
        'b Iterator.t
        * 'b option ref
        * ('a, 'y, 'b -> 's) instruction
        * ('a, 'y, 'b -> 's) instruction
        -> ('a, 'y, 's) instruction
    | Action : 'a * ('a, 'y, 's) instruction -> ('a, 'y, 's) instruction
    | Call :
        ('b Constant.hlist -> unit)
        * 'b Option_ref.hlist
        * ('a, 'y, 's) instruction
        -> ('a, 'y, 's) instruction
    | Yield :
        'y Option_ref.hlist * ('a, 'y Constant.hlist, 's) instruction
        -> ('a, 'y Constant.hlist, 's) instruction

  let rec exhausted : type y. (y, nil) continuation =
   fun Stack_nil ->
    Suspension { stack = Stack_nil; continuation = exhausted }, None

  let[@inline always] dispatch ~advance (stack : (_, _ -> _) stack) =
    let (Stack_cons (iterator, cell, level, stack)) = stack in
    match Iterator.current iterator with
    | Some current_key ->
      Iterator.accept iterator;
      cell := Some current_key;
      level (Stack_cons (iterator, cell, level, stack))
    | None -> advance stack

  let[@loop] rec advance : type y s. (y, s) continuation =
   fun stack ->
    match stack with
    | Stack_nil -> exhausted stack
    | Stack_cons (iterator, _, _, _) as stack ->
      Iterator.advance iterator;
      dispatch ~advance stack

  let step : type y. y suspension ref -> y option =
   fun state ->
    let (Suspension { stack; continuation }) = !state in
    let suspension, outcome = continuation stack in
    state := suspension;
    outcome

  type 'y t = State : 'y suspension ref -> 'y t [@@unboxed]

  let[@inline] execute (type a) ~(evaluate : a -> outcome) instruction =
    let rec execute : type s y. (a, y, s) instruction -> (y, s) continuation =
     fun instruction stack ->
      match instruction with
      | Advance -> advance stack
      | Up k ->
        let (Stack_cons (_, _, _, stack)) = stack in
        execute k stack
      | Open (iterator, cell, for_each, k) ->
        Iterator.init iterator;
        execute k (Stack_cons (iterator, cell, execute for_each, stack))
      | Dispatch -> dispatch ~advance stack
      | Action (op, k) -> (
        match (evaluate [@inlined hint]) op with
        | Accept -> execute k stack
        | Skip -> advance stack)
      | Call (f, rs, k) ->
        f (Option_ref.get rs);
        execute k stack
      | Yield (rs, k) ->
        Suspension { stack; continuation = execute k }, Some (Option_ref.get rs)
    in
    execute instruction

  let create ~evaluate (instruction : (_, _, _) instruction) =
    let continuation = execute ~evaluate instruction in
    State (ref (Suspension { stack = Stack_nil; continuation }))

  let advance = Advance

  let up i = Up i

  let dispatch = Dispatch

  let open_ i cell a dispatch = Open (i, cell, a, dispatch)

  let action a k = Action (a, k)

  let yield y k = Yield (y, k)

  let call f y k = Call (f, y, k)

  let rec refs : type s. s Iterator.hlist -> s Option_ref.hlist = function
    | [] -> []
    | _ :: iterators -> ref None :: refs iterators

  type erev = Erev : 's Iterator.hlist * 's Option_ref.hlist -> erev

  let iterate :
      type a. a Iterator.hlist -> (_, a Constant.hlist, nil) instruction =
   fun iterators ->
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
        type y s a.
        (a -> s) Iterator.hlist ->
        (a -> s) Option_ref.hlist ->
        (_, y Constant.hlist, a -> s) instruction ->
        (_, y Constant.hlist, nil) instruction =
     fun iterators refs instruction ->
      match iterators, refs with
      | [iterator], [r] -> open_ iterator r instruction dispatch
      | iterator :: (_ :: _ as iterators), r :: refs ->
        loop iterators refs (open_ iterator r instruction dispatch)
    in
    match rev_iterators with
    | [] -> Advance
    | _ :: _ -> loop rev_iterators rev_refs (yield rs advance)

  type void = |

  let iterator iterators =
    let evaluate : void -> outcome = function _ -> . in
    create ~evaluate (iterate iterators)

  let[@inline] fold f (State state) init =
    let rec loop state acc =
      match step state with
      | Some output -> loop state (f output acc)
      | None -> acc
    in
    loop state init

  let[@inline] iter f state = fold (fun keys () -> f keys) state ()
end
