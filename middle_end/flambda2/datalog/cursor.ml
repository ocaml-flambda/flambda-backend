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

open Datalog_imports

type vm_action =
  | Unless :
      ('t, 'k, 'v) Trie.is_trie
      * 't ref
      * 'k Option_ref.hlist
      * string
      * string list
      -> vm_action

type action =
  | Bind_iterator :
      'a option ref with_name * 'a Trie.Iterator.t with_name
      -> action
  | VM_action : vm_action -> action

let bind_iterator var iterator = Bind_iterator (var, iterator)

let unless id cell args =
  VM_action
    (Unless
       (Table.Id.is_trie id, cell, args.values, Table.Id.name id, args.names))

type binder = Bind_table : ('t, 'k, 'v) Table.Id.t * 't ref -> binder

type actions = { mutable rev_actions : action list }

let create_actions () = { rev_actions = [] }

let add_action actions action =
  actions.rev_actions <- action :: actions.rev_actions

let pp_cursor_action ff = function
  | Unless (_, _t, _l, t_name, l_names) ->
    Format.fprintf ff "if %s(%a):@ continue" t_name
      (Format.pp_print_list
         ~pp_sep:(fun ff () -> Format.fprintf ff ", ")
         Format.pp_print_string)
      l_names

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
      mutable iterators : 'a Trie.Iterator.t with_name list;
      mutable output : 'a option ref with_name option
    }

  let print ppf { name; order; _ } =
    Format.fprintf ppf "%s (with order %a)" name Order.print order

  let create ~order name =
    { name; order; output = None; iterators = []; actions = create_actions () }

  let use_output level =
    match level.output with
    | None ->
      let output = { value = ref None; name = level.name } in
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
  let handler = ref (Trie.empty (Table.Id.is_trie id)) in
  add_binder context.naive_binders (Bind_table (id, handler));
  handler

let initial_actions { actions; _ } = actions

type 'v t =
  { cursor_binders : binder list;
    cursor_naive_binders : binder list;
    instruction : (vm_action, nil) VM.instruction;
    callback : ('v Constant.hlist -> unit) ref
  }

type 'a cursor = 'a t

let print ppf { cursor_binders; instruction; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]@ %a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun ppf (Bind_table (table_id, _)) -> Table.Id.print ppf table_id))
    cursor_binders
    (VM.pp_instruction pp_cursor_action)
    instruction

let apply_actions actions instruction =
  (* Note: we must preserve the order of [Bind_iterator] actions in order to
     initialize iterators in the correct order. Otherwise, we would miscompile
     [P (x, x, x)] (we would initialize the 3rd argument before the 2nd). *)
  List.fold_left
    (fun instruction action ->
      match action with
      | Bind_iterator (var, iterator) ->
        let iterator =
          { value = Join_iterator.create [iterator.value];
            name = iterator.name
          }
        in
        VM.seek var iterator instruction
      | VM_action action -> VM.action action instruction)
    instruction actions.rev_actions

(* NB: the variables must be passed in reverse order, i.e. deepest variable
   first. *)
let rec open_rev_vars :
    type a s.
    (a -> s) Level.hlist ->
    (vm_action, a -> s) VM.instruction ->
    (vm_action, nil) VM.instruction =
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
      let cell =
        (* If we do not need the output (we usually do), write it to a dummy
           [ref] for simplicity. *)
        match var.output with
        | Some output -> output
        | None -> { value = ref None; name = "_" }
      in
      let iterators = List.map (fun it -> it.value) var.iterators in
      let iterator_names = List.map (fun it -> it.name) var.iterators in
      let iterator =
        { value = Join_iterator.create iterators;
          name = String.concat " ⨝ " iterator_names
        }
      in
      match vars with
      | [] -> VM.open_ iterator cell instruction VM.dispatch
      | _ :: _ as vars ->
        open_rev_vars vars (VM.open_ iterator cell instruction VM.dispatch)))

(* Optimisation: if we do not use the output from the last variable, we only
   need the first matching value of that variable.

   NB: the variables must be passed in reverse order, i.e. deepest variable
   first. *)
let rec pop_rev_vars : type s. s Level.hlist -> (vm_action, s) VM.instruction =
  function
  | [] -> VM.advance
  | var :: vars -> (
    match var.output with None -> VM.up (pop_rev_vars vars) | _ -> VM.advance)

type call =
  | Call :
      { func : 'a Constant.hlist -> unit;
        name : string;
        args : 'a Option_ref.hlist with_names
      }
      -> call

let create_call func ~name args = Call { func; name; args }

let create ?(calls = []) ?output context =
  let { levels; actions; binders; naive_binders } = context in
  let (Level_list rev_levels) = levels.rev_levels in
  let callback = ref ignore in
  let make k =
    let k =
      match output with
      | None -> k
      | Some output ->
        VM.call (fun args -> !callback args) ~name:"yield" output k
    in
    (* Make sure to compute calls in the provided order. *)
    List.fold_right
      (fun (Call { func; name; args }) k -> VM.call func ~name args k)
      calls k
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

let evaluate = function
  | Unless (is_trie, cell, args, _cell_name, _args_names) ->
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

  let print ppf { cursor; _ } = print ppf cursor

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
