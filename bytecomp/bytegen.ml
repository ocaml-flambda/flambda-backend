(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Blambda
open Instruct
open Debuginfo.Scoped_location

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter;
  !label_counter

(**** Operations on compilation environments. ****)

let empty_env = { ce_stack = Ident.empty; ce_closure = Not_in_closure }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack; ce_closure = env.ce_closure }

let rec add_vars idlist pos env =
  match idlist with
  | [] -> env
  | id :: rem -> add_vars rem (pos + 1) (add_var id pos env)

(* Compute the closure environment *)

let rec add_positions entries pos_to_entry ~pos ~delta = function
  | [] -> entries, pos
  | id :: rem ->
    let entries = Ident.add id (pos_to_entry pos) entries in
    add_positions entries pos_to_entry ~pos:(pos + delta) ~delta rem

type function_definition =
  | Single_non_recursive
  | Multiple_recursive of Ident.t list

let closure_entries fun_defs fvs =
  let funct_entries, pos_end_functs =
    match fun_defs with
    | Single_non_recursive ->
      (* No need to store the function in the environment, but we still need to
         reserve a slot in the closure block *)
      Ident.empty, 3
    | Multiple_recursive functs ->
      add_positions Ident.empty (fun pos -> Function pos) ~pos:0 ~delta:3 functs
  in
  (* Note: [pos_end_functs] is the position where we would store the next
     function if there was one, and points after an eventual infix tag.
     Since that was the last function, we don't need the last infix tag
     and start storing free variables at [pos_end_functs - 1]. *)
  let all_entries, _end_pos =
    add_positions funct_entries
      (fun pos -> Free_variable pos)
      ~pos:(pos_end_functs - 1) ~delta:1 fvs
  in
  all_entries

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
  | Kbranch lbl :: _ as cont -> lbl, cont
  | Klabel lbl :: _ as cont -> lbl, cont
  | cont ->
    let lbl = new_label () in
    lbl, Klabel lbl :: cont

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let rec make_branch_2 lbl n cont = function
  | Kreturn m :: _ -> Kreturn (n + m), cont
  | Klabel _ :: c -> make_branch_2 lbl n cont c
  | Kpop m :: c -> make_branch_2 lbl (n + m) cont c
  | _ -> (
    match lbl with
    | Some lbl -> Kbranch lbl, cont
    | None ->
      let lbl = new_label () in
      Kbranch lbl, Klabel lbl :: cont)

let make_branch cont =
  match cont with
  | (Kbranch _ as branch) :: _ -> branch, cont
  | (Kreturn _ as return) :: _ -> return, cont
  | Kraise k :: _ -> Kraise k, cont
  | Klabel lbl :: _ -> make_branch_2 (Some lbl) 0 cont cont
  | _ -> make_branch_2 None 0 cont cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label cont =
  match cont with
  | Klabel label0 :: _ when label = label0 -> cont
  | _ -> Kbranch label :: cont

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
  | [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0
  then cont
  else
    match cont with
    | Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn (n + m) :: cont
    | Kraise _ :: _ -> cont
    | _ -> Kpop n :: cont

(** Add a constant in front of a continuation *)
let add_const const = function
  | (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const :: cont

let add_const_unit = add_const Lambda.const_unit

let rec push_dummies n k =
  match n with
  | 0 -> k
  | _ -> Kconst Lambda.const_unit :: Kpush :: push_dummies (n - 1) k

(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev with
    ev_pos = 0;
    (* patched in emitcode *)
    ev_kind = kind;
    ev_info = info;
    ev_repr = repr
  }

let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
  | Event_other, info -> info
  | info, Event_other -> info
  | _ -> fatal_error "Bytegen.merge_infos"

let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
  | Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | Event_child r, Event_parent r' when r == r' -> Event_parent r
  | _, _ -> fatal_error "Bytegen.merge_repr"

let merge_events ev ev' =
  let maj, min =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)
    | Event_pseudo, _ -> ev', ev
    | _, Event_pseudo -> ev, ev'
    (* Keep following event, supposedly more informative *)
    | Event_before, (Event_after _ | Event_before) -> ev', ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)

let weaken_event ev cont =
  match ev.ev_kind with
  | Event_after _ -> (
    match cont with
    | Kpush :: Kevent ({ ev_repr = Event_none } as ev') :: c -> (
      match ev.ev_info with
      | Event_return _ ->
        (* Weaken event *)
        let repr = ref 1 in
        let ev = copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
        and ev' = copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr) in
        Kevent ev :: Kpush :: Kevent ev' :: c
      | _ ->
        (* Only keep following event, equivalent *)
        cont)
    | _ -> Kevent ev :: cont)
  | _ -> Kevent ev :: cont

let add_event ev = function
  | Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont -> weaken_event ev cont

(**** Compilation of a lambda expression ****)

type stack_info =
  { try_blocks : int list;
    (* list of stack size for each nested try block *)
    sz_static_raises : (int * (int * int * int list)) list;
    (* association staticraise numbers -> (lbl,size of stack, try_blocks *)
    max_stack_used : int ref
        (* Maximal stack size reached during the current function body *)
  }

let create_stack_info () =
  { try_blocks = []; sz_static_raises = []; max_stack_used = ref 0 }

(* association staticraise numbers -> (lbl,size of stack, try_blocks *)

let push_static_raise stack_info i lbl_handler sz =
  { stack_info with
    sz_static_raises =
      (i, (lbl_handler, sz, stack_info.try_blocks))
      :: stack_info.sz_static_raises
  }

let find_raise_label stack_info i =
  try List.assoc i stack_info.sz_static_raises
  with Not_found ->
    Misc.fatal_error ("exit(" ^ Int.to_string i ^ ") outside appropriated catch")

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump stack_info l sz =
  match l with
  | Staticraise (i, []) ->
    let label, size, tb = find_raise_label stack_info i in
    if sz = size && tb == stack_info.try_blocks then Some label else None
  | _ -> None

(* Function bodies that remain to be compiled *)

type function_to_compile =
  { params : Ident.t list; (* function parameters *)
    body : blambda; (* the function body *)
    label : label; (* the label of the function entry *)
    entries : closure_entry Ident.tbl;
        (* the offsets for the free variables
           and mutually recursive functions *)
    rec_pos : int
  }
(* rank in recursive definition *)

let functions_to_compile = (Stack.create () : function_to_compile Stack.t)

(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref Compilation_unit.dummy

let check_stack stack_info sz =
  let curr = stack_info.max_stack_used in
  if sz > !curr then curr := sz

(** Translate a primitive to a bytecode instruction *)
let comp_primitive stack_info p sz args =
  check_stack stack_info sz;
  match (p : Blambda.primitive) with
  | Getglobal cu -> Kgetglobal cu
  | Setglobal cu -> Ksetglobal cu
  | Getpredef id -> Kgetpredef id
  | Intcomp cmp -> Kintcomp cmp
  | Getfield n -> Kgetfield n
  | Getvectitem -> Kgetvectitem
  | Setfield n -> Ksetfield n
  | Setvectitem -> Ksetvectitem
  | Getfloatfield n -> Kgetfloatfield n
  | Setfloatfield n -> Ksetfloatfield n
  | Ccall name -> Kccall (name, List.length args)
  | Negint -> Knegint
  | Addint -> Kaddint
  | Subint -> Ksubint
  | Mulint -> Kmulint
  | Divint -> Kdivint
  | Modint -> Kmodint
  | Andint -> Kandint
  | Orint -> Korint
  | Xorint -> Kxorint
  | Lslint -> Klslint
  | Lsrint -> Klsrint
  | Asrint -> Kasrint
  | Offsetint n -> Koffsetint n
  | Offsetref n -> Koffsetref n
  | Getbyteschar -> Kgetbyteschar
  | Setbyteschar -> Ksetbyteschar
  | Vectlength -> Kvectlength
  | Isint -> Kisint
  | Boolnot -> Kboolnot
  | Makefloatblock -> Kmakefloatblock (List.length args)
  | Makeblock { tag } -> Kmakeblock (List.length args, tag)
  | Check_signals -> Kcheck_signals
  | Raise kind -> Kraise kind
  | Make_faux_mixedblock { total_len; tag } ->
    Kmake_faux_mixedblock (total_len, tag)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rec is_tailcall = function
  | Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

(* CR dkalinichenko: this error happens because we run tests
   under [oxcaml/tests] with the boot compiler instead of the
   final compiler. Run them using the final compiler.*)
(* We cannot use the [float32] or [or_null] types in the compiler. *)
external is_boot_compiler : unit -> bool = "caml_is_boot_compiler"

external float32_of_string : string -> Obj.t = "caml_float32_of_string"

let rec contains_float32s_or_nulls = function
  | Const_base (Const_float32 _ | Const_unboxed_float32 _) | Const_null -> true
  | Const_block (_, fields) -> List.exists contains_float32s_or_nulls fields
  | Const_mixed_block _ ->
    Misc.fatal_error "[Const_mixed_block] not supported in bytecode."
  | _ -> false

let rec translate_float32s_or_nulls stack_info env cst sz cont =
  match cst with
  | Const_base (Const_float32 f | Const_unboxed_float32 f) ->
    let i = float32_of_string f in
    Kconst (Const_base (Const_int32 (Obj.obj i)))
    :: Kccall ("caml_float32_of_bits_bytecode", 1)
    :: cont
  | Const_null ->
    Kconst (Const_base (Const_int 0))
    :: Kccall ("caml_int_as_pointer", 1)
    :: cont
  | Const_block (tag, fields) as cst when contains_float32s_or_nulls cst ->
    let fields = List.map (fun field -> Const field) fields in
    let cont = Kmakeblock (List.length fields, tag) :: cont in
    comp_args stack_info env fields sz cont
  | Const_mixed_block _ ->
    Misc.fatal_error "[Const_mixed_block] not supported in bytecode."
  | _ as cst -> Kconst cst :: cont

and comp_expr stack_info env exp sz cont =
  check_stack stack_info sz;
  match (exp : blambda) with
  | Var id -> (
    try
      let pos = Ident.find_same id env.ce_stack in
      Kacc (sz - pos) :: cont
    with Not_found -> (
      let not_found () =
        fatal_error ("Bytegen.comp_expr: var " ^ Ident.unique_name id)
      in
      match env.ce_closure with
      | Not_in_closure -> not_found ()
      | In_closure { entries; env_pos } -> (
        match Ident.find_same id entries with
        | Free_variable pos -> Kenvacc (pos - env_pos) :: cont
        | Function pos -> Koffsetclosure (pos - env_pos) :: cont
        | exception Not_found -> not_found ())))
  | Const cst ->
    if is_boot_compiler ()
    then translate_float32s_or_nulls stack_info env cst sz cont
    else add_const cst cont
  | Apply { func; args; nontail } ->
    let nargs = List.length args in
    if (not nontail) && is_tailcall cont
    then
      comp_args stack_info env args sz
        (Kpush
        :: comp_expr stack_info env func (sz + nargs)
             (Kappterm (nargs, sz + nargs) :: discard_dead_code cont))
    else if nargs < 4
    then
      comp_args stack_info env args sz
        (Kpush
        :: comp_expr stack_info env func (sz + nargs) (Kapply nargs :: cont))
    else
      let lbl, cont1 = label_code cont in
      Kpush_retaddr lbl
      :: comp_args stack_info env args (sz + 3)
           (Kpush
           :: comp_expr stack_info env func
                (sz + 3 + nargs)
                (Kapply nargs :: cont1))
  | Send { method_kind; met; obj; args; nontail } ->
    let obj_and_args = obj :: args in
    let nargs = List.length obj_and_args in
    let getmethod, args' =
      match method_kind with
      | Self -> Kgetmethod, met :: obj_and_args
      | Public -> (
        match met with
        | Const (Const_base (Const_int n)) -> Kgetpubmet n, obj_and_args
        | _ -> Kgetdynmet, met :: obj_and_args)
    in
    if is_tailcall cont && not nontail
    then
      comp_args stack_info env args' sz
        (getmethod :: Kappterm (nargs, sz + nargs) :: discard_dead_code cont)
    else if nargs < 4
    then comp_args stack_info env args' sz (getmethod :: Kapply nargs :: cont)
    else
      let lbl, cont1 = label_code cont in
      Kpush_retaddr lbl
      :: comp_args stack_info env args' (sz + 3)
           (getmethod :: Kapply nargs :: cont1)
  | Function { params; body; free_variables } ->
    (* assume kind = Curried *)
    let lbl = new_label () in
    let fv = Ident.Set.elements free_variables in
    let entries = closure_entries Single_non_recursive fv in
    let to_compile = { params; body; label = lbl; entries; rec_pos = 0 } in
    Stack.push to_compile functions_to_compile;
    comp_args stack_info env
      (List.map (fun n -> Var n) fv)
      sz
      (Kclosure (lbl, List.length fv) :: cont)
  | Let { id; arg; body } ->
    comp_expr stack_info env arg sz
      (Kpush
      :: comp_expr stack_info
           (add_var id (sz + 1) env)
           body (sz + 1) (add_pop 1 cont))
  | Letrec { decls = decl; body; free_variables_of_decls } ->
    let ndecl = List.length decl in
    let fv = Ident.Set.elements free_variables_of_decls in
    let rec_idents = List.map (fun { id } -> id) decl in
    let entries = closure_entries (Multiple_recursive rec_idents) fv in
    let rec comp_fun pos = function
      | [] -> []
      | { def = { params; body } } :: rem ->
        let lbl = new_label () in
        let to_compile =
          { params; body; label = lbl; entries; rec_pos = pos }
        in
        Stack.push to_compile functions_to_compile;
        lbl :: comp_fun (pos + 1) rem
    in
    let lbls = comp_fun 0 decl in
    comp_args stack_info env
      (List.map (fun n -> Var n) fv)
      sz
      (Kclosurerec (lbls, List.length fv)
      :: comp_expr stack_info
           (add_vars rec_idents (sz + 1) env)
           body (sz + ndecl) (add_pop ndecl cont))
  | Prim (Boolnot, [arg]) ->
    let newcont =
      match cont with
      | Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
      | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
      | _ -> Kboolnot :: cont
    in
    comp_expr stack_info env arg sz newcont
  | Sequand (exp1, exp2) -> (
    match cont with
    | Kbranchifnot lbl :: _ ->
      comp_expr stack_info env exp1 sz
        (Kbranchifnot lbl :: comp_expr stack_info env exp2 sz cont)
    | Kbranchif lbl :: cont1 ->
      let lbl2, cont2 = label_code cont1 in
      comp_expr stack_info env exp1 sz
        (Kbranchifnot lbl2
        :: comp_expr stack_info env exp2 sz (Kbranchif lbl :: cont2))
    | _ ->
      let lbl, cont1 = label_code cont in
      comp_expr stack_info env exp1 sz
        (Kstrictbranchifnot lbl :: comp_expr stack_info env exp2 sz cont1))
  | Sequor (exp1, exp2) -> (
    match cont with
    | Kbranchif lbl :: _ ->
      comp_expr stack_info env exp1 sz
        (Kbranchif lbl :: comp_expr stack_info env exp2 sz cont)
    | Kbranchifnot lbl :: cont1 ->
      let lbl2, cont2 = label_code cont1 in
      comp_expr stack_info env exp1 sz
        (Kbranchif lbl2
        :: comp_expr stack_info env exp2 sz (Kbranchifnot lbl :: cont2))
    | _ ->
      let lbl, cont1 = label_code cont in
      comp_expr stack_info env exp1 sz
        (Kstrictbranchif lbl :: comp_expr stack_info env exp2 sz cont1))
  | Prim (Raise k, args) ->
    comp_args stack_info env args sz (Kraise k :: discard_dead_code cont)
  | Prim (Makefloatblock, args) ->
    comp_args stack_info env args sz (Kmakefloatblock (List.length args) :: cont)
  | Prim (Make_faux_mixedblock { total_len; tag }, args) ->
    (* There is no notion of a mixed block at runtime in bytecode. Further,
       source-level unboxed types are represented as boxed in bytecode, so
       no ceremony is needed to box values before inserting them into
       the (normal, unmixed) block.
    *)
    comp_args stack_info env args sz
      (Kmake_faux_mixedblock (total_len, tag) :: cont)
  | Context_switch (Resume, args) ->
    let nargs = List.length args - 1 in
    assert (nargs = 3);
    if is_tailcall cont
    then (
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      check_stack stack_info 3;
      comp_args stack_info env args sz
        (Kresumeterm (sz + nargs) :: discard_dead_code cont))
    else (
      (* Resume itself only pushes 2 words, but perform adds another *)
      check_stack stack_info (sz + nargs + 3);
      comp_args stack_info env args sz (Kresume :: cont))
  | Context_switch (Runstack, args) ->
    let nargs = List.length args in
    assert (nargs = 3);
    if is_tailcall cont
    then (
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      check_stack stack_info 3;
      Kconst Lambda.const_unit :: Kpush
      :: comp_args stack_info env args (sz + 1)
           (Kresumeterm (sz + nargs) :: discard_dead_code cont))
    else (
      (* Resume itself only pushes 2 words, but perform adds another *)
      check_stack stack_info (sz + nargs + 3);
      Kconst Lambda.const_unit :: Kpush
      :: comp_args stack_info env args (sz + 1) (Kresume :: cont))
  | Context_switch (Reperform, args) ->
    let nargs = List.length args - 1 in
    assert (nargs = 2);
    check_stack stack_info (sz + 3);
    if is_tailcall cont
    then
      comp_args stack_info env args sz
        (Kreperformterm (sz + nargs) :: discard_dead_code cont)
    else fatal_error "Reperform used in non-tail position"
  | Context_switch (Perform, args) ->
    let nargs = List.length args - 1 in
    comp_args stack_info env args sz
      (check_stack stack_info (sz + nargs - 1 + 4);
       Kperform :: cont)
  | Prim (p, args) ->
    let nargs = List.length args - 1 in
    comp_args stack_info env args sz
      (comp_primitive stack_info p (sz + nargs - 1) args :: cont)
  | Staticcatch { body; id = i; args = vars; handler } ->
    let nvars = List.length vars in
    let branch1, cont1 = make_branch cont in
    let r =
      if nvars <> 1
      then
        (* general case *)
        let lbl_handler, cont2 =
          label_code
            (comp_expr stack_info
               (add_vars vars (sz + 1) env)
               handler (sz + nvars) (add_pop nvars cont1))
        in
        let stack_info =
          push_static_raise stack_info i lbl_handler (sz + nvars)
        in
        push_dummies nvars
          (comp_expr stack_info env body (sz + nvars)
             (add_pop nvars (branch1 :: cont2)))
      else
        (* small optimization for nvars = 1 *)
        let var = match vars with [var] -> var | _ -> assert false in
        let lbl_handler, cont2 =
          label_code
            (Kpush
            :: comp_expr stack_info
                 (add_var var (sz + 1) env)
                 handler (sz + 1) (add_pop 1 cont1))
        in
        let stack_info = push_static_raise stack_info i lbl_handler sz in
        comp_expr stack_info env body sz (branch1 :: cont2)
    in
    r
  | Staticraise (i, args) -> (
    let cont = discard_dead_code cont in
    let label, size, tb = find_raise_label stack_info i in
    let cont = branch_to label cont in
    let rec loop sz tbb =
      if tb == tbb
      then add_pop (sz - size) cont
      else
        match tbb with
        | [] -> assert false
        | try_sz :: tbb ->
          add_pop (sz - try_sz - 4) (Kpoptrap :: loop try_sz tbb)
    in
    let cont = loop sz stack_info.try_blocks in
    match args with
    | [arg] ->
      (* optim, argument passed in accumulator *)
      comp_expr stack_info env arg sz cont
    | _ -> comp_exit_args stack_info env args sz size cont)
  | Trywith { body; param = id; handler } ->
    let branch1, cont1 = make_branch cont in
    let lbl_handler = new_label () in
    let body_cont =
      Kpoptrap :: branch1 :: Klabel lbl_handler :: Kpush
      :: comp_expr stack_info
           (add_var id (sz + 1) env)
           handler (sz + 1) (add_pop 1 cont1)
    in
    let stack_info =
      { stack_info with try_blocks = sz :: stack_info.try_blocks }
    in
    let l = comp_expr stack_info env body (sz + 4) body_cont in
    Kpushtrap lbl_handler :: l
  | Ifthenelse { cond; ifso; ifnot } ->
    comp_binary_test stack_info env cond ifso ifnot sz cont
  | Sequence (exp1, exp2) ->
    comp_expr stack_info env exp1 sz (comp_expr stack_info env exp2 sz cont)
  | While { cond = wh_cond; body = wh_body } ->
    let lbl_loop = new_label () in
    let lbl_test = new_label () in
    Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals
    :: comp_expr stack_info env wh_body sz
         (Klabel lbl_test
         :: comp_expr stack_info env wh_cond sz
              (Kbranchif lbl_loop :: add_const_unit cont))
  | For
      { id = for_id;
        from = for_from;
        to_ = for_to;
        dir = for_dir;
        body = for_body
      } ->
    let lbl_loop = new_label () in
    let lbl_exit = new_label () in
    let offset = match for_dir with Upto -> 1 | Downto -> -1 in
    let comp = match for_dir with Upto -> Gtint | Downto -> Ltint in
    comp_expr stack_info env for_from sz
      (Kpush
      :: comp_expr stack_info env for_to (sz + 1)
           (Kpush :: Kpush :: Kacc 2 :: Kintcomp comp :: Kbranchif lbl_exit
          :: Klabel lbl_loop :: Kcheck_signals
           :: comp_expr stack_info
                (add_var for_id (sz + 1) env)
                for_body (sz + 2)
                (Kacc 1 :: Kpush :: Koffsetint offset :: Kassign 2 :: Kacc 1
               :: Kintcomp Neq :: Kbranchif lbl_loop :: Klabel lbl_exit
                :: add_const_unit (add_pop 2 cont))))
  | Switch { arg; const_cases; block_cases; cases = acts } ->
    let branch, cont1 = make_branch cont in
    let c = ref (discard_dead_code cont1) in
    (* label actions *)
    let lbls = Array.make (Array.length acts) 0 in
    for i = Array.length acts - 1 downto 0 do
      let lbl, c1 =
        label_code (comp_expr stack_info env acts.(i) sz (branch :: !c))
      in
      lbls.(i) <- lbl;
      c := discard_dead_code c1
    done;
    (* Build label vectors *)
    let lbl_blocks = Array.map (Array.get lbls) block_cases in
    let lbl_consts = Array.map (Array.get lbls) const_cases in
    comp_expr stack_info env arg sz (Kswitch (lbl_consts, lbl_blocks) :: !c)
  | Assign (id, expr) -> (
    try
      let pos = Ident.find_same id env.ce_stack in
      comp_expr stack_info env expr sz (Kassign (sz - pos) :: cont)
    with Not_found -> fatal_error "Bytegen.comp_expr: assign")
  | Event (lam, lev) -> (
    let ev_defname =
      string_of_scoped_location ~include_zero_alloc:false lev.lev_loc
    in
    let event kind info =
      { ev_pos = 0;
        (* patched in emitcode *)
        ev_module = Compilation_unit.full_path_as_string !compunit_name;
        ev_loc = to_location lev.lev_loc;
        ev_kind = kind;
        ev_defname;
        ev_info = info;
        ev_typenv = Env.summary lev.lev_env;
        ev_typsubst = Subst.identity;
        ev_compenv = env;
        ev_stacksize = sz;
        ev_repr =
          (match lev.lev_repr with
          | None -> Event_none
          | Some ({ contents = 1 } as repr)
            when lev.lev_kind = Lambda.Lev_function ->
            Event_child repr
          | Some ({ contents = 1 } as repr) -> Event_parent repr
          | Some repr when lev.lev_kind = Lambda.Lev_function ->
            Event_parent repr
          | Some repr -> Event_child repr)
      }
    in
    match lev.lev_kind with
    | Lev_before ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_before Event_other in
      add_event ev c
    | Lev_function ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_pseudo Event_function in
      add_event ev c
    | Lev_pseudo ->
      let c = comp_expr stack_info env lam sz cont in
      let ev = event Event_pseudo Event_other in
      add_event ev c
    | Lev_after ty ->
      let preserve_tailcall =
        match lam with
        | Apply { nontail; _ } | Send { nontail; _ } -> not nontail
        | Prim _ -> false
        | _ -> true
      in
      if preserve_tailcall && is_tailcall cont
      then (* don't destroy tail call opt *)
        comp_expr stack_info env lam sz cont
      else
        let info =
          match lam with
          | Apply { args; _ } -> Event_return (List.length args)
          | Send { obj; args; _ } -> Event_return (List.length (obj :: args))
          | Prim (_, args) -> Event_return (List.length args)
          | _ -> Event_other
        in
        let ev = event (Event_after ty) info in
        let cont1 = add_event ev cont in
        comp_expr stack_info env lam sz cont1)
  | Pseudo_event (expr, loc) ->
    let cont =
      if !Clflags.debug
      then
        let ev_defname =
          string_of_scoped_location ~include_zero_alloc:false loc
        in
        let ev =
          { ev_pos = 0;
            (* patched in emitcode *)
            ev_module = Compilation_unit.full_path_as_string !compunit_name;
            ev_loc = to_location loc;
            ev_defname;
            ev_kind = Event_pseudo;
            ev_info = Event_other;
            (* Dummy *)
            ev_typenv = Env.Env_empty;
            (* Dummy *)
            ev_typsubst = Subst.identity;
            (* Dummy *)
            ev_compenv = empty_env;
            (* Dummy *)
            ev_stacksize = 0;
            (* Dummy *)
            ev_repr = Event_none
          }
          (* Dummy *)
        in
        add_event ev cont
      else cont
    in
    comp_expr stack_info env expr sz cont

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args stack_info env argl sz cont =
  comp_expr_list stack_info env (List.rev argl) sz cont

and comp_expr_list stack_info env exprl sz cont =
  match exprl with
  | [] -> cont
  | [exp] -> comp_expr stack_info env exp sz cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kpush :: comp_expr_list stack_info env rem (sz + 1) cont)

and comp_exit_args stack_info env argl sz pos cont =
  comp_expr_list_assign stack_info env (List.rev argl) sz pos cont

and comp_expr_list_assign stack_info env exprl sz pos cont =
  match exprl with
  | [] -> cont
  | exp :: rem ->
    comp_expr stack_info env exp sz
      (Kassign (sz - pos)
      :: comp_expr_list_assign stack_info env rem sz (pos - 1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test stack_info env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Const Lambda.const_unit
    then
      let lbl_end, cont1 = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr stack_info env ifso sz cont1
    else
      match code_as_jump stack_info ifso sz with
      | Some label ->
        let cont = comp_expr stack_info env ifnot sz cont in
        Kbranchif label :: cont
      | None -> (
        match code_as_jump stack_info ifnot sz with
        | Some label ->
          let cont = comp_expr stack_info env ifso sz cont in
          Kbranchifnot label :: cont
        | None ->
          let branch_end, cont1 = make_branch cont in
          let lbl_not, cont2 =
            label_code (comp_expr stack_info env ifnot sz cont1)
          in
          Kbranchifnot lbl_not
          :: comp_expr stack_info env ifso sz (branch_end :: cont2))
  in
  comp_expr stack_info env cond sz cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  let stack_info = create_stack_info () in
  let code = comp_expr stack_info env exp sz cont in
  let used_safe = !(stack_info.max_stack_used) + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold
  then
    Kconst (Const_base (Const_int used_safe))
    :: Kccall ("caml_ensure_stack_capacity", 1)
    :: code
  else code

(**** Compilation of functions ****)

let comp_function tc cont =
  let arity = List.length tc.params in
  let ce_stack, _last_pos =
    add_positions Ident.empty Fun.id ~pos:arity ~delta:(-1) tc.params
  in
  let env =
    { ce_stack;
      ce_closure = In_closure { entries = tc.entries; env_pos = 3 * tc.rec_pos }
    }
  in
  let cont = comp_block env tc.body arity (Kreturn arity :: cont) in
  if arity > 1
  then Krestart :: Klabel tc.label :: Kgrab (arity - 1) :: cont
  else Klabel tc.label :: cont

let comp_remainder cont =
  let c = ref cont in
  (try
     while true do
       c := comp_function (Stack.pop functions_to_compile) !c
     done
   with Stack.Empty -> ());
  !c

(**** Compilation of a lambda phrase ****)

let reset () =
  label_counter := 0;
  compunit_name := Compilation_unit.dummy;
  Stack.clear functions_to_compile

let compile_gen ?modulename ~init_stack expr =
  reset ();
  (match modulename with Some name -> compunit_name := name | None -> ());
  Fun.protect ~finally:reset (fun () ->
      let init_code = comp_block empty_env expr init_stack [] in
      if Stack.length functions_to_compile > 0
      then
        let lbl_init = new_label () in
        Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code), false
      else init_code, true)

let compile_implementation modulename expr =
  fst (compile_gen ~modulename ~init_stack:0 expr)

let compile_phrase expr = compile_gen ~init_stack:1 expr
