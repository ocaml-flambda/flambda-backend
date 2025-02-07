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

type action =
  | Bind_iterator : 'a option Named_ref.t * 'a Trie.Iterator.t -> action
  | Unless :
      ('t, 'k, 'v) Trie.is_trie * 't Named_ref.t * 'k Option_ref.hlist
      -> action

let bind_iterator var iterator = Bind_iterator (var, iterator)

let unless id cell args = Unless (Table.Id.is_trie id, cell, args)

type binder = Bind_table : ('t, 'k, 'v) Table.Id.t * 't Named_ref.t -> binder

type actions = { mutable rev_actions : action list }

let create_actions () = { rev_actions = [] }

let add_action actions action =
  actions.rev_actions <- action :: actions.rev_actions

let pp_action ff = function
  | Bind_iterator (x, _it) -> Format.fprintf ff "%a := <it>" Named_ref.pp_name x
  | Unless (_, t, l) ->
    Format.fprintf ff "if (%a(%a))@ continue" Named_ref.pp_name t
      Option_ref.pp_name_hlist l

module Order : sig
  type t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val parameters : t

  val succ : t -> t
end = struct
  type t = int

  let print = Format.pp_print_int

  let compare = Int.compare

  let parameters = -1

  let succ o = o + 1
end

module Level = struct
  type 'a t =
    { name : string;
      order : Order.t;
      actions : actions;
      mutable iterators : 'a Trie.Iterator.t list;
      mutable output : 'a option Named_ref.t option
    }

  let print ppf { name; order; _ } =
    Format.fprintf ppf "%s (with order %a)" name Order.print order

  let create ~order name =
    { name; order; output = None; iterators = []; actions = create_actions () }

  let use_output level =
    match level.output with
    | None ->
      let output : _ Named_ref.t =
        { contents = None; printed_name = level.name }
      in
      level.output <- Some output;
      output
    | Some output -> output

  let actions { actions; _ } = actions

  let add_iterator level iterator =
    level.iterators <- iterator :: level.iterators

  let order { order; _ } = order

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)
end

type level_list = Level_list : 'a Level.hlist -> level_list [@@unboxed]

type levels =
  { mutable rev_levels : level_list;
    mutable last_order : Order.t
  }

let create_levels () =
  { rev_levels = Level_list []; last_order = Order.parameters }

let add_new_level levels name =
  let order = Order.succ levels.last_order in
  let level = Level.create ~order name in
  let (Level_list rev_levels) = levels.rev_levels in
  levels.rev_levels <- Level_list (level :: rev_levels);
  levels.last_order <- order;
  level

module Join_iterator = Leapfrog.Join (Trie.Iterator)
module VM = Virtual_machine.Make (Join_iterator)

type binders = { mutable rev_binders : binder list }

let create_binders () = { rev_binders = [] }

let add_binder binders binder =
  binders.rev_binders <- binder :: binders.rev_binders

type context =
  { levels : levels;
    actions : actions;
    binders : binders;
    naive_binders : binders
  }

let create_context () =
  { levels = create_levels ();
    actions = create_actions ();
    binders = create_binders ();
    naive_binders = create_binders ()
  }

let add_new_level context name = add_new_level context.levels name

let add_iterator context id =
  let handler, iterators, _ = Table.Id.create_iterator id in
  add_binder context.binders (Bind_table (id, handler));
  iterators

let add_naive_binder context id =
  let handler : _ Named_ref.t =
    { contents = Trie.empty (Table.Id.is_trie id);
      printed_name = Table.Id.name id
    }
  in
  add_binder context.naive_binders (Bind_table (id, handler));
  handler

let initial_actions { actions; _ } = actions

type 'v t =
  { cursor_binders : binder list;
    cursor_naive_binders : binder list;
    instruction : (action, nil) VM.instruction;
    callback : ('v Constant.hlist -> unit) ref
  }

type 'a cursor = 'a t

let print ppf { cursor_binders; instruction; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]@ %a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun ppf (Bind_table (table_id, _)) -> Table.Id.print ppf table_id))
    cursor_binders
    (VM.pp_instruction pp_action)
    instruction

let apply_actions actions instruction =
  (* Note: we must preserve the order of [Bind_iterator] actions in order to
     initialize iterators in the correct order. Otherwise, we would miscompile
     [P (x, x, x)] (we would initialize the 3rd argument before the 2nd). *)
  List.fold_left
    (fun instruction action -> VM.action action instruction)
    instruction actions.rev_actions

(* NB: the variables must be passed in reverse order, i.e. deepest variable
   first. *)
let rec open_rev_vars :
    type a s.
    (a -> s) Level.hlist ->
    (action, a -> s) VM.instruction ->
    (action, nil) VM.instruction =
 fun vars instruction ->
  match vars with
  | var :: vars -> (
    match var.iterators with
    | [] ->
      Misc.fatal_errorf
        "@[<v>@[Variable '%a' is never used in a binding position.@]@ @[Hint: \
         A position is binding if it respects the provided variable \
         ordering.@]@]"
        Level.print var
    | _ -> (
      let instruction = apply_actions var.actions instruction in
      let cell : _ Named_ref.t =
        (* If we do not need the output (we usually do), write it to a dummy
           [ref] for simplicity. *)
        match var.output with
        | Some output -> output
        | None -> { contents = None; printed_name = "_" }
      in
      match vars with
      | [] ->
        VM.open_
          (Join_iterator.create var.iterators)
          cell instruction VM.dispatch
      | _ :: _ as vars ->
        open_rev_vars vars
          (VM.open_
             (Join_iterator.create var.iterators)
             cell instruction VM.dispatch)))

(* Optimisation: if we do not use the output from the last variable, we only
   need the first matching value of that variable.

   NB: the variables must be passed in reverse order, i.e. deepest variable
   first. *)
let rec pop_rev_vars : type s. s Level.hlist -> (action, s) VM.instruction =
  function
  | [] -> VM.advance
  | var :: vars -> (
    match var.output with None -> VM.up (pop_rev_vars vars) | _ -> VM.advance)

type call =
  | Call :
      { func : 'a Constant.hlist -> unit;
        args : 'a Option_ref.hlist
      }
      -> call

let create_call func args = Call { func; args }

let create ?(calls = []) ?output context =
  let { levels; actions; binders; naive_binders } = context in
  let (Level_list rev_levels) = levels.rev_levels in
  let callback = ref ignore in
  let make k =
    let k =
      match output with
      | None -> k
      | Some output -> VM.call (fun args -> !callback args) output k
    in
    (* Make sure to compute calls in the provided order. *)
    List.fold_right (fun (Call { func; args }) k -> VM.call func args k) calls k
  in
  let instruction : (_, nil) VM.instruction =
    match rev_levels with
    | [] -> make @@ pop_rev_vars rev_levels
    | _ :: _ -> open_rev_vars rev_levels @@ make @@ pop_rev_vars rev_levels
  in
  let instruction = apply_actions actions instruction in
  { cursor_binders = binders.rev_binders;
    cursor_naive_binders = naive_binders.rev_binders;
    instruction;
    callback
  }

let bind_table (Bind_table (id, handler)) database =
  let table = Table.Map.get id database in
  if Trie.is_empty (Table.Id.is_trie id) table
  then false
  else (
    handler.contents <- Table.Map.get id database;
    true)

let bind_table_list binders database =
  List.iter (fun binder -> ignore @@ bind_table binder database) binders

let evaluate op =
  match op with
  | Bind_iterator (value, it) -> (
    let value = Option.get value.contents in
    Trie.Iterator.init it;
    Trie.Iterator.seek it value;
    match Trie.Iterator.current it with
    | Some found when Trie.Iterator.equal_key it found value ->
      Trie.Iterator.accept it;
      Virtual_machine.Accept
    | None | Some _ -> Virtual_machine.Skip)
  | Unless (is_trie, cell, args) ->
    if Option.is_some
         (Trie.find_opt is_trie (Option_ref.get args) cell.contents)
    then Virtual_machine.Skip
    else Virtual_machine.Accept

let naive_iter cursor db f =
  bind_table_list cursor.cursor_binders db;
  bind_table_list cursor.cursor_naive_binders db;
  cursor.callback := f;
  VM.run (VM.create ~evaluate cursor.instruction);
  cursor.callback := ignore

let naive_fold cursor db f acc =
  let acc = ref acc in
  naive_iter cursor db (fun args -> acc := f args !acc);
  !acc

(* Seminaive evaluation iterates over all the {b new} tuples in the [diff]
   database that are not in the [previous] database.

   [current] must be equal to [concat ~earlier:previous ~later:diff]. *)
let[@inline] seminaive_run cursor ~previous ~diff ~current =
  bind_table_list cursor.cursor_binders current;
  bind_table_list cursor.cursor_naive_binders current;
  let rec loop binders =
    match binders with
    | [] -> ()
    | binder :: binders ->
      if bind_table binder diff
      then VM.run (VM.create ~evaluate cursor.instruction);
      if bind_table binder previous then loop binders
  in
  loop cursor.cursor_binders

module With_parameters = struct
  type nonrec ('p, 'v) t =
    { parameters : 'p Option_ref.hlist;
      cursor : 'v t
    }

  let without_parameters { parameters = []; cursor } = cursor

  let create ~parameters ?calls ?output context =
    { cursor = create ?calls ?output context; parameters }

  let naive_fold { parameters; cursor } ps db f acc =
    Option_ref.set parameters ps;
    naive_fold cursor db f acc

  let naive_iter { parameters; cursor } ps db f =
    Option_ref.set parameters ps;
    naive_iter cursor db f

  let seminaive_run { parameters; cursor } ps ~previous ~diff ~current =
    Option_ref.set parameters ps;
    seminaive_run ~previous ~diff ~current cursor
end
