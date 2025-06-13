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

module Parameter = struct
  type 'a t =
    { name : string;
      sender : 'a option Channel.sender;
      receiver : 'a option Channel.receiver
    }

  let create name =
    let sender, receiver = Channel.create None in
    { name; sender; receiver }

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let rec to_senders : type a. a hlist -> a Option_sender.hlist = function
    | [] -> []
    | p :: ps -> p.sender :: to_senders ps
end

module Term = struct
  type 'a t =
    | Variable of 'a Cursor.Level.t
    | Parameter of 'a Parameter.t
    | Constant of 'a

  let constant c = Constant c

  include Heterogenous_list.Make (struct
    type nonrec 'a t = 'a t
  end)

  let rec parameters : type a. a Parameter.hlist -> a hlist = function
    | [] -> []
    | var :: vars -> Parameter var :: parameters vars
end

type atom = Atom : ('t, 'k, unit) Table.Id.t * 'k Term.hlist -> atom

module String = struct
  include Heterogenous_list.Make (struct
    type 'a t = string
  end)
end

let rec add_variables : type a. _ -> a String.hlist -> a Term.hlist =
 fun info names ->
  match names with
  | [] -> []
  | name :: names ->
    let var = Cursor.add_new_level info name in
    Variable var :: add_variables info names

type 'p context =
  { parameters : 'p Parameter.hlist;
    context : Cursor.context
  }

type ('p, 'a) program = 'p context -> 'a

let map_program x f info = f (x info)

let rec add_parameters : type a. a String.hlist -> a Parameter.hlist = function
  | [] -> []
  | name :: names -> Parameter.create name :: add_parameters names

let with_parameters params f info =
  let [] = info.parameters in
  let parameters = add_parameters params in
  f (Term.parameters parameters) { info with parameters }

let foreach vars f info =
  let variables = add_variables info.context vars in
  f variables info

let compile xs f =
  foreach xs f { context = Cursor.create_context (); parameters = [] }

let bind_iterator actions var iterator =
  Cursor.add_action actions (Cursor.bind_iterator var iterator)

let rec bind_atom :
    type a.
    order:_ -> _ -> a Term.hlist -> a Trie.Iterator.hlist -> string list -> unit
    =
 fun ~order post_level args iterators iterator_names ->
  match args, iterators, iterator_names with
  | [], [], [] -> ()
  | [], [], _ :: _ -> Misc.fatal_error "Too many names in [bind_atom]"
  | _, _, [] -> Misc.fatal_error "Missing names in [bind_atom]"
  | ( this_arg :: other_args,
      this_iterator :: other_iterators,
      this_iterator_name :: other_iterators_names ) -> (
    let this_iterator = { value = this_iterator; name = this_iterator_name } in
    match this_arg with
    | Constant cte ->
      let _send, recv = Channel.create (Some cte) in
      bind_iterator post_level
        { value = recv; name = "<constant>" }
        this_iterator;
      bind_atom ~order post_level other_args other_iterators
        other_iterators_names
    | Parameter param ->
      bind_iterator post_level
        { value = param.receiver; name = param.name }
        this_iterator;
      bind_atom ~order post_level other_args other_iterators
        other_iterators_names
    | Variable var ->
      let var_order = Cursor.Level.order var in
      if Cursor.Order.compare var_order order > 0
      then (
        Cursor.Level.add_iterator var this_iterator;
        bind_atom ~order:var_order (Cursor.Level.actions var) other_args
          other_iterators other_iterators_names)
      else (
        bind_iterator post_level (Cursor.Level.use_output var) this_iterator;
        bind_atom ~order post_level other_args other_iterators
          other_iterators_names))

let bind_atom post_level args iterator =
  bind_atom ~order:Cursor.Order.parameters post_level args iterator.values
    iterator.names

let where_atom id args k info =
  let iterators = Cursor.add_iterator info.context id in
  bind_atom (Cursor.initial_actions info.context) args iterators;
  k info

let rec find_last_binding0 : type a. order:_ -> _ -> a Term.hlist -> _ =
 fun ~order post_level args ->
  match args with
  | [] -> post_level
  | arg :: args -> (
    match arg with
    | Constant _ | Parameter _ -> find_last_binding0 ~order post_level args
    | Variable var ->
      let var_order = Cursor.Level.order var in
      if Cursor.Order.compare var_order order > 0
      then find_last_binding0 ~order:var_order (Cursor.Level.actions var) args
      else find_last_binding0 ~order post_level args)

let find_last_binding post_level args =
  find_last_binding0 ~order:Cursor.Order.parameters post_level args

let compile_term : 'a Term.t -> 'a option Channel.receiver with_name = function
  | Constant cte ->
    let _send, recv = Channel.create (Some cte) in
    { value = recv; name = "<constant>" }
  | Parameter param -> { value = param.receiver; name = param.name }
  | Variable var -> Cursor.Level.use_output var

let rec compile_terms :
    type a. a Term.hlist -> a Option_receiver.hlist with_names =
 fun vars ->
  match vars with
  | [] -> { values = []; names = [] }
  | term :: terms ->
    let { value; name } = compile_term term in
    let { values; names } = compile_terms terms in
    { values = value :: values; names = name :: names }

let unless_atom id args k info =
  let refs = compile_terms args in
  let post_level =
    find_last_binding (Cursor.initial_actions info.context) args
  in
  let r = Cursor.add_naive_binder info.context id in
  Cursor.add_action post_level (Cursor.unless id r refs);
  k info

let unless_eq repr arg1 arg2 k info =
  let ref1 = compile_term arg1 in
  let ref2 = compile_term arg2 in
  let post_level =
    find_last_binding (Cursor.initial_actions info.context) [arg1; arg2]
  in
  Cursor.add_action post_level (Cursor.unless_eq repr ref1 ref2);
  k info

let filter f args k info =
  let refs = compile_terms args in
  let post_level =
    find_last_binding (Cursor.initial_actions info.context) args
  in
  Cursor.add_action post_level (Cursor.filter f refs);
  k info

type callback =
  | Callback :
      { func : 'a Constant.hlist -> unit;
        name : string;
        args : 'a Term.hlist
      }
      -> callback

let create_callback func ~name args = Callback { func; name; args }

let yield output (info : _ context) =
  let output = compile_terms output in
  Cursor.With_parameters.create
    ~parameters:(Parameter.to_senders info.parameters)
    info.context ~output

let execute callbacks (info : _ context) =
  let calls =
    List.map
      (fun (Callback { func; name; args }) ->
        Cursor.create_call func ~name (compile_terms args))
      callbacks
  in
  Cursor.With_parameters.create ~calls
    ~parameters:(Parameter.to_senders info.parameters)
    info.context
