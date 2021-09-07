(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Simple = Reg_width_things.Simple

type continuation_kind = Normal | Exn_handler

(* (* For checking that push- and pop-trap operations match up correctly. *)

   module Continuation_stack : sig type t

   val var : unit -> t val root : unit -> t val push : Trap_id.t ->
   Continuation.t -> t -> t

   val unify : Continuation.t -> t -> t -> unit end = struct type t0 = | Root |
   Var (* Debug *) | Link of t | Push of Trap_id.t * Continuation.t * t

   and t = t0 ref

   let var () = ref Var

   let root () = ref Root

   let push id cont s = ref (Push (id, cont, s))

   let rec repr t = match !t with | Link s -> let u = repr s in t := u; u | v ->
   v

   let rec occurs_check cont t checked = if t == checked then begin
   Misc.fatal_errorf "Malformed exception continuation \ (recursive stack) for
   %a" Continuation.print cont end; match !checked with | Var | Root -> () |
   Link s | Push (_, _, s) -> occurs_check cont t s

   let rec unify cont t1 t2 = if t1 == t2 then () else match repr t1, repr t2
   with | Link _, _ | _, Link _ -> assert false | Var, _ -> occurs_check cont t1
   t2; t1 := Link t2 | _, Var -> occurs_check cont t2 t1; t2 := Link t1 | Root,
   Root -> () | Push (id1, c1, s1), Push (id2, c2, s2) -> if not (Trap_id.equal
   id1 id2) then begin Misc.fatal_errorf "Malformed exception continuation \
   (mismatched trap ID) for %a" Continuation.print cont end; if not
   (Continuation.equal c1 c2) then begin Misc.fatal_errorf "Malformed exception
   continuation \ (mismatched continuations, %a vs. %a) for %a"
   Continuation.print c1 Continuation.print c2 Continuation.print cont end;
   unify cont s1 s2 | Root, Push _ | Push _, Root -> Misc.fatal_errorf
   "Malformed exception continuation \ (root stack is not empty) for %a"
   Continuation.print cont end *)

type t =
  { all_names_seen : Name.Set.t ref;
    all_continuations_seen : Continuation.Set.t ref;
    all_closure_ids_seen : Closure_id.Set.t ref;
    uses_of_closure_ids_seen : Closure_id.Set.t ref;
    all_var_within_closures_seen : Var_within_closure.Set.t ref;
    uses_of_var_within_closures_seen : Var_within_closure.Set.t ref;
    names : Flambda_kind.t Name.Map.t;
    continuations :
      (Flambda_arity.With_subkinds.t * continuation_kind)
      (* * Continuation_stack.t *)
      Continuation.Map.t
        (* continuation_stack : Continuation_stack.t; *)
  }

let create () =
  { all_names_seen = ref Name.Set.empty;
    all_continuations_seen = ref Continuation.Set.empty;
    all_closure_ids_seen = ref Closure_id.Set.empty;
    uses_of_closure_ids_seen = ref Closure_id.Set.empty;
    all_var_within_closures_seen = ref Var_within_closure.Set.empty;
    uses_of_var_within_closures_seen = ref Var_within_closure.Set.empty;
    names = Name.Map.empty;
    continuations =
      Continuation.Map.empty
      (* continuation_stack = Continuation_stack.var (); *)
  }

let add_name t name kind =
  (*Format.eprintf "add_variable %a :: %a\n%s\n%!" Variable.print var
    Flambda_kind.print kind (Printexc.raw_backtrace_to_string
    (Printexc.get_callstack 50));*)
  if Name.Map.mem name t.names
  then
    Misc.fatal_errorf
      "Duplicate binding of name %a which is already bound in the current scope"
      Name.print name;
  if Name.Set.mem name !(t.all_names_seen)
  then
    Misc.fatal_errorf
      "Duplicate binding of name %a which is bound in some other scope"
      Name.print name;
  (* let compilation_unit = Compilation_unit.get_current_exn () in if not
     (Name.in_compilation_unit name compilation_unit) then begin
     Misc.fatal_errorf "Binding occurrence of name %a cannot occur in \ this
     compilation unit since the name is from another compilation \ unit"
     Name.print name end; *)
  t.all_names_seen := Name.Set.add name !(t.all_names_seen);
  { t with names = Name.Map.add name kind t.names }

let add_variable t var kind = add_name t (Name.var var) kind

let add_variables t vars_and_kinds =
  List.fold_left (fun t (var, kind) -> add_variable t var kind) t vars_and_kinds

let add_kinded_parameters t kinded_params =
  List.fold_left
    (fun t kinded_param ->
      add_name t
        (Kinded_parameter.name kinded_param)
        (Flambda_kind.With_subkind.kind (Kinded_parameter.kind kinded_param)))
    t kinded_params

let add_symbol t sym kind =
  let name = Name.symbol sym in
  if Name.Map.mem name t.names
  then
    Misc.fatal_errorf
      "Duplicate binding of symbol %a which is already bound in the current \
       scope"
      Symbol.print sym;
  if Name.Set.mem name !(t.all_names_seen)
  then
    Misc.fatal_errorf
      "Duplicate binding of symbol %a which is bound in some other scope"
      Symbol.print sym;
  let compilation_unit = Compilation_unit.get_current_exn () in
  if not (Symbol.in_compilation_unit sym compilation_unit)
  then
    Misc.fatal_errorf
      "Binding occurrence of symbol %a cannot occur in this compilation unit \
       since the symbol is from another compilation unit"
      Symbol.print sym;
  t.all_names_seen := Name.Set.add name !(t.all_names_seen);
  { t with names = Name.Map.add name kind t.names }

let add_continuation t cont arity kind (* stack *) =
  if Continuation.Map.mem cont t.continuations
  then
    Misc.fatal_errorf
      "Duplicate binding of continuation %a which is already bound in the \
       current scope"
      Continuation.print cont;
  if Continuation.Set.mem cont !(t.all_continuations_seen)
  then
    Misc.fatal_errorf
      "Duplicate binding of continuation %a which is bound in some other scope"
      Continuation.print cont;
  t.all_continuations_seen
    := Continuation.Set.add cont !(t.all_continuations_seen);
  { t with
    continuations =
      Continuation.Map.add cont (arity, kind (*, stack *)) t.continuations
  }

let name_is_bound t name = Name.Map.mem name t.names

let variable_is_bound t var = Name.Map.mem (Name.var var) t.names

let check_name_is_bound t name =
  if not (name_is_bound t name)
  then Misc.fatal_errorf "Unbound name %a" Name.print name

let check_simple_is_bound t (simple : Simple.t) =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> check_name_is_bound t name)
    ~const:(fun _ -> ())

let check_simples_are_bound t simples =
  List.iter (fun simple -> check_simple_is_bound t simple) simples

let check_variable_is_bound t var = check_name_is_bound t (Name.var var)

let check_variables_are_bound t vars =
  List.iter (fun var -> check_variable_is_bound t var) vars

let check_name_is_bound_and_of_kind t name desired_kind =
  match Name.Map.find name t.names with
  | exception Not_found -> Misc.fatal_errorf "Unbound name %a" Name.print name
  | kind ->
    if not (Flambda_kind.equal kind desired_kind)
    then
      Misc.fatal_errorf "Name %a of kind %a cannot be used at kind %a"
        Name.print name Flambda_kind.print kind Flambda_kind.print desired_kind

let check_simple_is_bound_and_of_kind t (simple : Simple.t) desired_kind =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
      check_name_is_bound_and_of_kind t name desired_kind)
    ~const:(fun const ->
      let actual_kind = Reg_width_const.kind const in
      if not (Flambda_kind.equal actual_kind desired_kind)
      then
        Misc.fatal_errorf "Simple term %a of kind %a cannot be used at kind %a"
          Simple.print simple Flambda_kind.print actual_kind Flambda_kind.print
          desired_kind)

let check_simples_are_bound_and_of_kind t simples desired_kind =
  List.iter
    (fun simple -> check_simple_is_bound_and_of_kind t simple desired_kind)
    simples

let check_variable_is_bound_and_of_kind t var desired_kind =
  check_name_is_bound_and_of_kind t (Name.var var) desired_kind

let check_variables_are_bound_and_of_kind t vars desired_kind =
  List.iter
    (fun var -> check_variable_is_bound_and_of_kind t var desired_kind)
    vars

let check_symbol_is_bound t sym = check_name_is_bound t (Name.symbol sym)

let find_continuation_opt t cont =
  match Continuation.Map.find cont t.continuations with
  | exception Not_found -> None
  | result -> Some result

let continuation_arity t cont =
  match find_continuation_opt t cont with
  | Some (arity, _ (*, _ *)) -> arity
  | None -> Misc.fatal_errorf "Unbound continuation %a" Continuation.print cont

let kind_of_name t name =
  match Name.Map.find name t.names with
  | exception Not_found ->
    Misc.fatal_errorf "Unbound name %a" Name.print_sexp name
  | kind -> kind

let kind_of_simple t (simple : Simple.t) =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> kind_of_name t name)
    ~const:(fun const -> Reg_width_const.kind const)

let kind_of_variable t var = kind_of_name t (Name.var var)

(* let current_continuation_stack t = t.continuation_stack

   let set_current_continuation_stack t continuation_stack = { t with
   continuation_stack; } *)

let add_closure_id t id =
  (* The same closure ID may be bound multiple times in the same program, so
     there is no membership check here. *)
  let compilation_unit = Compilation_unit.get_current_exn () in
  if not (Closure_id.in_compilation_unit id compilation_unit)
  then
    Misc.fatal_errorf
      "Binding occurrence of closure ID %a cannot occur in this compilation \
       unit since the closure ID is from another compilation unit"
      Closure_id.print id;
  t.all_closure_ids_seen := Closure_id.Set.add id !(t.all_closure_ids_seen)

let add_use_of_closure_id t id =
  (* The closure binding may be out of scope, so there is no check that [id] is
     in scope. *)
  t.uses_of_closure_ids_seen
    := Closure_id.Set.add id !(t.uses_of_closure_ids_seen)

let add_var_within_closure t id =
  (* The same var-within-closure may be bound multiple times in the same
     program, so there is no membership check here. *)
  let compilation_unit = Compilation_unit.get_current_exn () in
  if not (Var_within_closure.in_compilation_unit id compilation_unit)
  then
    Misc.fatal_errorf
      "Binding occurrence of var-within-closure %a cannot occur in this \
       compilation unit since the closure ID is from another compilation unit"
      Var_within_closure.print id;
  t.all_var_within_closures_seen
    := Var_within_closure.Set.add id !(t.all_var_within_closures_seen)

let add_use_of_var_within_closure t id =
  t.uses_of_var_within_closures_seen
    := Var_within_closure.Set.add id !(t.uses_of_var_within_closures_seen)

let closure_ids_not_declared t =
  Closure_id.Set.diff !(t.uses_of_closure_ids_seen) !(t.all_closure_ids_seen)

let var_within_closures_not_declared t =
  Var_within_closure.Set.diff
    !(t.uses_of_var_within_closures_seen)
    !(t.all_var_within_closures_seen)

let prepare_for_function_body t ~parameters_with_kinds ~my_closure ~return_cont
    ~return_cont_arity ~exception_cont =
  (* CR mshinwell for pchambart: Is one continuation stack correct now that we
     have exception continuations? *)
  (* let continuation_stack = Continuation_stack.var () in *)
  let continuations =
    Continuation.Map.singleton return_cont
      (return_cont_arity, Normal (*, continuation_stack *))
  in
  let continuations =
    Continuation.Map.add exception_cont
      ( [Flambda_kind.With_subkind.any_value],
        Exn_handler (*, continuation_stack *) )
      continuations
  in
  let names = Name.symbols_only_map t.names in
  let t = { t with names; continuations (* continuation_stack; *) } in
  add_variables
    (add_variable t my_closure Flambda_kind.value)
    parameters_with_kinds
