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

type ('y, 'r) state =
  | Yielded of 'y
  | Complete of 'r

module Make (Iterator : Leapfrog.Iterator) = struct
  type ('i, 'x, 'y, 's) stack =
    | Stack_nil : ('i, 'x, 'y, nil) stack
    | Stack_cons :
        'a
        * 'a Iterator.t
        * ('i, 'x, 'y, 'a -> 's) compiled
        * ('i, 'x, 'y, 's) stack
        -> ('i, 'x, 'y, 'a -> 's) stack

  and ('i, 'x, 'y) suspension =
    | Suspension :
        { stack : ('i, 'x, 'y, 's) stack;
          state : 'i;
          instruction : ('i, 'x, 'y, 's) compiled
        }
        -> ('i, 'x, 'y) suspension

  and ('i, 'x, 'y, 's) compiled =
    'i -> ('i, 'x, 'y, 's) stack -> ('i, 'x, 'y) suspension * ('y, 'i) state

  let rec exhausted :
      type x y i.
      i -> (i, x, y, nil) stack -> (i, x, y) suspension * (y, i) state =
   fun state Stack_nil ->
    ( Suspension { state; stack = Stack_nil; instruction = exhausted },
      Complete state )

  let rec advance :
      type i x y s.
      i -> (i, x, y, s) stack -> (i, x, y) suspension * (y, i) state =
   fun input stack ->
    match stack with
    | Stack_nil -> exhausted input stack
    | Stack_cons (_, iterator, level, stack) -> (
      Iterator.advance iterator;
      match Iterator.current iterator with
      | Some current_key ->
        Iterator.accept iterator;
        level input (Stack_cons (current_key, iterator, level, stack))
      | None -> advance input stack)

  let pop : type i a x y s. (i, x, y, s) compiled -> (i, x, y, a -> s) compiled
      =
   fun k input (Stack_cons (_, _, _, stack)) -> k input stack

  let open_ :
      type a x y s.
      a Iterator.t -> (_, x, y, a -> s) compiled -> (_, x, y, s) compiled =
   fun iterator k input stack ->
    Iterator.init iterator;
    match Iterator.current iterator with
    | Some current_key ->
      Iterator.accept iterator;
      k input (Stack_cons (current_key, iterator, k, stack))
    | None -> advance input stack

  let yield rs k : (_, _, _, _) compiled =
   fun state stack ->
    Suspension { state; stack; instruction = k }, Yielded (Option_ref.get rs)

  let set_output r k input (Stack_cons (v, _, _, _) as stack) =
    r := Some v;
    k input stack

  let step : type y i. (i, _, y) suspension ref -> (y, i) state =
   fun state ->
    let (Suspension { stack; state = input; instruction }) = !state in
    let suspension, outcome = instruction input stack in
    state := suspension;
    outcome

  type 'y t = State : ('i, 'a, 'y) suspension ref -> 'y t [@@unboxed]

  let create input instruction =
    State (ref (Suspension { state = input; stack = Stack_nil; instruction }))

  type ('a, 'y, 's) instruction =
    | Advance : ('a, 'y, 's) instruction
    | Pop : ('x, 'y, 's) instruction -> ('x, 'y, 'a -> 's) instruction
    | Open :
        'b Iterator.t * ('a, 'y, 'b -> 's) instruction
        -> ('a, 'y, 's) instruction
    | Action : 'a * ('a, 'y, 's) instruction -> ('a, 'y, 's) instruction
    | Yield :
        'y Option_ref.hlist * ('a, 'y Constant.hlist, 's) instruction
        -> ('a, 'y Constant.hlist, 's) instruction
    | Set_output :
        'a option ref * ('x, 'y, 'a -> 's) instruction
        -> ('x, 'y, 'a -> 's) instruction

  let compile (type a i) ~(evaluate : a -> i -> outcome) instruction :
      (_, _, _, _) compiled =
    let action op k input stack =
      match (evaluate [@inlined hint]) op input with
      | Accept -> k input stack
      | Skip -> advance input stack
    in
    let rec compile : type y s. (a, y, s) instruction -> (i, a, y, s) compiled =
      function
      | Advance -> advance
      | Pop k -> pop (compile k)
      | Open (iterator, k) -> open_ iterator (compile k)
      | Action (a, k) -> action a (compile k)
      | Yield (rs, k) -> yield rs (compile k)
      | Set_output (r, k) -> set_output r (compile k)
    in
    compile instruction

  let advance = Advance

  let pop i = Pop i

  let open_ i a = Open (i, a)

  let action a i = Action (a, i)

  let yield y i = Yield (y, i)

  let set_output r i = Set_output (r, i)

  let rec refs : type s. s Iterator.hlist -> s Option_ref.hlist = function
    | [] -> []
    | _ :: iterators -> ref None :: refs iterators

  type erev = Erev : 's Iterator.hlist * 's Option_ref.hlist -> erev

  let iterate :
      type x s. s Iterator.hlist -> (x, s Constant.hlist, nil) instruction =
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
        type y s.
        s Iterator.hlist ->
        s Option_ref.hlist ->
        (x, y Constant.hlist, s) instruction ->
        (x, y Constant.hlist, nil) instruction =
     fun iterators refs instruction ->
      match iterators, refs with
      | [], [] -> instruction
      | iterator :: iterators, r :: refs ->
        loop iterators refs (open_ iterator (set_output r instruction))
    in
    loop rev_iterators rev_refs (yield rs advance)

  type void = |

  let iterator iterators =
    let evaluate : void -> unit -> outcome = function _ -> . in
    create () (compile ~evaluate (iterate iterators))

  let[@inline] fold f (State state) init =
    let rec loop state acc =
      match step state with
      | Yielded output -> loop state (f output acc)
      | Complete _ -> acc
    in
    loop state init

  let[@inline] iter f state = fold (fun keys () -> f keys) state ()
end
