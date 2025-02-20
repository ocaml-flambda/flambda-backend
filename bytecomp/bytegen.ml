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

(*  bytegen.ml : translation of blambda terms to lists of instructions. *)

open Misc
open Lambda
open Blambda
open Instruct
open Debuginfo.Scoped_location

(**** Label generation ****)

type function_to_compile =
  { params : Ident.t list;  (** function parameters *)
    body : blambda;  (** the function body *)
    label : label;  (** the label of the function entry *)
    entries : closure_entry Ident.tbl;
        (** the offsets for the free variables
           and mutually recursive functions *)
    rec_pos : int  (** rank in recursive definition *)
  }

type t =
  { compunit_name : Compilation_unit.t;
        (** Name of current compilation unit (for debugging events) *)
    next_label : label;
    functions_to_compile : function_to_compile list;
        (** Function bodies that remain to be compiled *)
    max_stack_used : int;
    cont : instruction list
  }

let create ?(compunit_name = Compilation_unit.dummy) () =
  { compunit_name;
    next_label = 0;
    functions_to_compile = [];
    max_stack_used = 0;
    cont = []
  }

let new_label t =
  let label = t.next_label in
  label, { t with next_label = label + 1 }

(**** Operations on compilation environments. ****)

type stack_frame =
  { try_blocks : int list;  (** list of stack size for each nested try block *)
    size : int  (** stack size during evaluation of the current expression *)
  }

type static_handler =
  { handler : label;
    frame : stack_frame
  }

type stack_info =
  { frame : stack_frame;
    static_handlers : (static_label * static_handler) list;
        (** association staticraise numbers -> static_handler *)
    env : compilation_env
  }

let empty_env = { ce_stack = Ident.empty; ce_closure = Not_in_closure }

(* Add a stack-allocated variable *)

let add_var id stack =
  let size = stack.frame.size + 1 in
  { stack with
    env = { stack.env with ce_stack = Ident.add id size stack.env.ce_stack };
    frame = { stack.frame with size }
  }

let rec add_vars idlist stack =
  match idlist with [] -> stack | id :: rem -> add_vars rem (add_var id stack)

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

(** Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)
let label_code t =
  match t.cont with
  | (Kbranch lbl | Klabel lbl) :: _ -> lbl, t
  | _ ->
    let label = t.next_label in
    label, { t with next_label = label + 1; cont = Klabel label :: t.cont }

(** Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)
let make_branch t =
  match t.cont with
  | ((Kbranch _ | Kraise _) as next) :: _ -> next, t
  | cont ->
    let rec look_for_return t n = function
      | Klabel _ :: cont -> look_for_return t n cont
      | Kpop m :: cont -> look_for_return t (n + m) cont
      | Kreturn m :: _ -> Kreturn n, t
      | _ ->
        let lbl, t = label_code t in
        Kbranch lbl, t
    in
    look_for_return t 0 cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label t =
  match t.cont with
  | Klabel label0 :: _ when label = label0 -> t
  | cont -> { t with cont = Kbranch label :: cont }

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code t =
  match t.cont with
  | (Klabel _ | Krestart | Ksetglobal _) :: _ -> t
  | ([] as cont) | _ :: cont -> discard_dead_code { t with cont }

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n t =
  if n = 0
  then t
  else
    match t.cont with
    | Kpop m :: cont -> add_pop (n + m) { t with cont }
    | Kreturn m :: cont -> { t with cont = Kreturn (n + m) :: cont }
    | Kraise _ :: _ -> t
    | cont -> { t with cont = Kpop n :: cont }

(* Add the constant "unit" in front of a continuation *)

let kconst0 = Kconst (Const_base (Const_int 0))

let add_const_unit t =
  match t.cont with
  | (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> kconst0 :: cont

let push_dummies n t =
  match n with
  | 0 -> t
  | n ->
    assert (n >= 0);
    let rec push n k = if n = 0 then k else push (n - 1) (Kpush :: k) in
    { t with cont = kconst0 :: push n t.cont }

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

let add_event ev t =
  match t.cont with
  | Kevent ev' :: cont ->
    { t with cont = weaken_event (merge_events ev ev') cont }
  | cont -> { t with cont = weaken_event ev cont }

(* Pseudo events are ignored by the debugger. They are only used for
   generating backtraces.

   We prefer adding this event here rather than in lambda generation
   1) there are many different situations where a Pmakeblock can
      be generated
   2) we prefer inserting a pseudo event rather than an event after
      to prevent the debugger to stop at every single allocation. *)
let add_pseudo_event loc t =
  if not !Clflags.debug
  then t
  else
    let ev_defname = string_of_scoped_location ~include_zero_alloc:false loc in
    let ev =
      { ev_pos = 0;
        (* patched in emitcode *)
        ev_module = Compilation_unit.full_path_as_string t.compunit_name;
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
    add_event ev t

(**** Compilation of a blambda expression ****)

let push_static_catch si ~static_label ~handler =
  { si with
    static_handlers =
      (static_label, { handler; frame = si.frame }) :: si.static_handlers
  }

let find_raise_label stack_info ~static_label =
  try List.assoc static_label stack_info.static_handlers
  with Not_found ->
    Misc.fatal_errorf "exit(%d) outside appropriated catch" static_label

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump stack_info = function
  | Staticraise (static_label, []) -> (
    match find_raise_label stack_info ~static_label with
    | { handler; frame } ->
      if stack_info.frame = frame then Some handler else None)
  | _ -> None

let check_stack t stack_info =
  let sz = stack_info.frame.size in
  let curr = t.max_stack_used in
  if sz <= t.max_stack_used then t else { t with max_stack_used = sz }

let push stack n =
  { stack with frame = { stack.frame with size = stack.frame.size + n } }

let[@inline always] ( @@ ) instr t = { t with cont = instr :: t.cont }

let comp_primitive stack_info p nargs t =
  let t = check_stack t stack_info in
  match (p : Blambda.primitive) with
  | Getglobal cu -> Kgetglobal cu @@ t
  | Getpredef id -> Kgetpredef id @@ t
  | Boolnot -> (
    match t.cont with
    | Kbranchif lbl :: cont -> { t with cont = Kbranchifnot lbl :: cont }
    | Kbranchifnot lbl :: cont -> { t with cont = Kbranchif lbl :: cont }
    | _ -> Kboolnot @@ t)
  | Isint -> Kisint @@ t
  | Vectlength -> Kvectlength @@ t
  | Setglobal cu -> Ksetglobal cu @@ t
  | Getfield i -> Kgetfield i @@ t
  | Getfloatfield i -> Kgetfloatfield i @@ t
  | Raise k -> Kraise k @@ discard_dead_code t
  | Offsetint i -> Koffsetint i @@ t
  | Offsetref i -> Koffsetref i @@ t
  | Negint -> Knegint @@ t
  | Addint -> Kaddint @@ t
  | Subint -> Ksubint @@ t
  | Mulint -> Kmulint @@ t
  | Divint -> Kdivint @@ t
  | Modint -> Kmodint @@ t
  | Andint -> Kandint @@ t
  | Orint -> Korint @@ t
  | Xorint -> Kxorint @@ t
  | Lslint -> Klslint @@ t
  | Lsrint -> Klsrint @@ t
  | Asrint -> Kasrint @@ t
  | Intcomp cmp -> Kintcomp cmp @@ t
  | Isout -> Kisout @@ t
  | Getstringchar -> Kgetstringchar @@ t
  | Getbyteschar -> Kgetbyteschar @@ t
  | Getvectitem -> Kgetvectitem @@ t
  | Setfield i -> Ksetfield i @@ t
  | Setfloatfield i -> Ksetfloatfield i @@ t
  | Setvectitem -> Ksetvectitem @@ t
  | Setbyteschar -> Ksetbyteschar @@ t
  | Ccall name -> Kccall (name, nargs) @@ t
  | Makeblock { tag } -> Kmakeblock (tag, nargs) @@ t
  | Makefloatblock -> Kmakefloatblock nargs @@ t
  | Make_faux_mixedblock { total_len; tag } ->
    (* There is no notion of a mixed block at runtime in bytecode. Further,
       source-level unboxed types are represented as boxed in bytecode, so
       no ceremony is needed to box values before inserting them into
       the (normal, unmixed) block.
    *)
    Kmake_faux_mixedblock (total_len, tag) @@ t

let is_immed n = immed_min <= n && n <= immed_max

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

(* CR dkalinichenko: this error happens because we run tests
   under [flambda_backend/tests] with the boot compiler instead of the
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

let rec translate_float32s_or_nulls stack cst t =
  match cst with
  | Const_base (Const_float32 f | Const_unboxed_float32 f) ->
    let i = float32_of_string f in
    Kconst (Const_base (Const_int32 (Obj.obj i)))
    @@ Kccall ("caml_float32_of_bits_bytecode", 1)
    @@ t
  | Const_null ->
    Kconst (Const_base (Const_int 0)) @@ Kccall ("caml_int_as_pointer", 1) @@ t
  | Const_block (tag, fields) as cst when contains_float32s_or_nulls cst ->
    let fields = List.map (fun field -> Const field) fields in
    let t = Kmakeblock (List.length fields, tag) @@ t in
    comp_args stack fields t
  | Const_mixed_block _ ->
    Misc.fatal_error "[Const_mixed_block] not supported in bytecode."
  | _ as cst -> Kconst cst @@ t

and comp_expr stack exp t =
  let t = check_stack t stack in
  let sz = stack.frame.size in
  match (exp : Blambda.blambda) with
  | Var id -> (
    match Ident.find_same id stack.env.ce_stack with
    | pos -> Kacc (sz - pos) @@ t
    | exception Not_found -> (
      let not_found () =
        Misc.fatal_errorf "Bytegen.comp_expr: var %s" (Ident.unique_name id)
      in
      match stack.env.ce_closure with
      | Not_in_closure -> not_found ()
      | In_closure { entries; env_pos } -> (
        match Ident.find_same id entries with
        | Free_variable pos -> Kenvacc (pos - env_pos) @@ t
        | Function pos -> Koffsetclosure (pos - env_pos) @@ t
        | exception Not_found -> not_found ())))
  | Const cst ->
    if is_boot_compiler ()
    then translate_float32s_or_nulls stack cst t
    else Kconst cst @@ t
  | Apply { func; args; tailcall } -> (
    let nargs = List.length args in
    match tailcall with
    | Tailcall ->
      comp_args stack args
        (Kpush
        @@ comp_expr (push stack nargs) func
             (Kappterm (nargs, sz + nargs) @@ discard_dead_code t))
    | Nontail ->
      if nargs < 4
      then
        comp_args stack args
          (Kpush @@ comp_expr (push stack nargs) func (Kapply nargs @@ t))
      else
        let lbl, t = label_code t in
        let stack = push stack 3 in
        Kpush_retaddr lbl
        @@ comp_args stack args
             (Kpush @@ comp_expr (push stack nargs) func (Kapply nargs @@ t)))
  | Send { self; met; obj_and_args; tailcall } -> (
    let nargs = List.length obj_and_args in
    let getmethod, args =
      if self
      then Kgetmethod, met :: obj_and_args
      else
        match met with
        | Const (Const_base (Const_int n)) -> Kgetpubmet n, obj_and_args
        | _ -> Kgetdynmet, met :: obj_and_args
    in
    match tailcall with
    | Tailcall ->
      comp_args stack args
        (getmethod @@ Kappterm (nargs, sz + nargs) @@ discard_dead_code t)
    | Nontail ->
      if nargs < 4
      then comp_args stack args (getmethod @@ Kapply nargs @@ t)
      else
        let lbl, t = label_code t in
        Kpush_retaddr lbl
        @@ comp_args (push stack 3) args (getmethod @@ Kapply nargs @@ t))
  | Function { params; body; loc; free_variables } ->
    let fv = Ident.Set.elements free_variables in
    (* assume kind = Curried *)
    let label, t = new_label (add_pseudo_event loc t) in
    let entries = closure_entries Single_non_recursive fv in
    let t =
      let to_compile = { params; body; label; entries; rec_pos = 0 } in
      { t with functions_to_compile = to_compile :: t.functions_to_compile }
    in
    comp_args stack
      (List.map (fun n -> Var n) fv)
      (Kclosure (label, List.length fv) @@ t)
  | Let { id; arg; body } ->
    comp_expr stack arg
      (Kpush @@ comp_expr (add_var id stack) body (add_pop 1 t))
  | Letrec { decls; body; free_variables } ->
    let ndecl = List.length decls in
    let fv = Ident.Set.elements free_variables in
    let rec_idents = List.map (fun decl -> decl.id) decls in
    let entries = closure_entries (Multiple_recursive rec_idents) fv in
    let rec comp_fun t rec_pos = function
      | [] -> t, []
      | { def = { params; body } } :: rem ->
        let label, t = new_label t in
        let to_compile = { params; body; label; entries; rec_pos } in
        let t =
          { t with functions_to_compile = to_compile :: t.functions_to_compile }
        in
        let t, labels = comp_fun t (rec_pos + 1) rem in
        t, label :: labels
    in
    let t, lbls = comp_fun t 0 decls in
    comp_args stack
      (List.map (fun n -> Var n) fv)
      (Kclosurerec (lbls, List.length fv)
      @@ comp_expr (add_vars rec_idents stack) body (add_pop ndecl t))
  | Sequand (exp1, exp2) -> (
    match t.cont with
    | Kbranchifnot lbl :: _ ->
      comp_expr stack exp1 (Kbranchifnot lbl @@ comp_expr stack exp2 t)
    | Kbranchif lbl :: cont ->
      let t = { t with cont } in
      let lbl2, t = label_code t in
      comp_expr stack exp1
        (Kbranchifnot lbl2 @@ comp_expr stack exp2 (Kbranchif lbl @@ t))
    | _ ->
      let lbl, t = label_code t in
      comp_expr stack exp1 (Kstrictbranchifnot lbl @@ comp_expr stack exp2 t))
  | Sequor (exp1, exp2) -> (
    match t.cont with
    | Kbranchif lbl :: _ ->
      comp_expr stack exp1 (Kbranchif lbl @@ comp_expr stack exp2 t)
    | Kbranchifnot lbl :: cont ->
      let t = { t with cont } in
      let lbl2, t = label_code t in
      comp_expr stack exp1
        (Kbranchif lbl2 @@ comp_expr stack exp2 (Kbranchifnot lbl @@ t))
    | _ ->
      let lbl, t = label_code t in
      comp_expr stack exp1 (Kstrictbranchif lbl @@ comp_expr stack exp2 t))
  | Context_switch (Resume, tailcall, args) -> (
    let nargs = List.length args - 1 in
    assert (nargs = 3);
    match tailcall with
    | Tailcall ->
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      let t = check_stack t (push stack 3) in
      comp_args stack args (Kresumeterm (sz + nargs) @@ discard_dead_code t)
    | Nontail ->
      (* Resume itself only pushes 2 words, but perform adds another *)
      let t = check_stack t (push stack (nargs + 3)) in
      comp_args stack args (Kresume @@ t))
  | Context_switch (Runstack, tailcall, args) -> (
    kconst0 @@ Kpush
    @@
    let nargs = List.length args in
    assert (nargs = 3);
    match tailcall with
    | Tailcall ->
      (* Resumeterm itself only pushes 2 words, but perform adds another *)
      let t = check_stack t (push stack 3) in
      comp_args (push stack 1) args
        (Kresumeterm (sz + nargs) @@ discard_dead_code t)
    | Nontail ->
      (* Resume itself only pushes 2 words, but perform adds another *)
      let t = check_stack t (push stack (nargs + 3)) in
      comp_args (push stack 1) args (Kresume @@ t))
  | Context_switch (Reperform, tailcall, args) -> (
    let nargs = List.length args - 1 in
    assert (nargs = 2);
    let t = check_stack t (push stack 3) in
    match tailcall with
    | Nontail -> Misc.fatal_error "Reperform used in non-tail position"
    | Tailcall ->
      comp_args stack args (Kreperformterm (sz + nargs) @@ discard_dead_code t))
  (* Integer first for enabling further optimization (cf. emitcode.ml)  *)
  | Prim (p, args) ->
    let nargs = List.length args in
    comp_args stack args (comp_primitive (push stack (nargs - 1)) p nargs t)
  | Staticcatch { body; args = i, vars; handler } -> (
    let nvars = List.length vars in
    let branch1, cont1 = make_branch cont in
    match vars with
    | [] | _ :: _ :: _ ->
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
           (add_pop nvars (branch1 @@ cont2)))
    | [var] ->
      (* small optimization for nvars = 1 *)
      let lbl_handler, cont2 =
        label_code
          (Kpush
          @@ comp_expr stack_info
               (add_var var (sz + 1) env)
               handler (sz + 1) (add_pop 1 cont1))
      in
      let stack_info = push_static_raise stack_info i lbl_handler sz in
      comp_expr stack body (branch1 @@ cont2))
  | Staticraise (i, args) -> (
    let t = discard_dead_code t in
    let { handler = label; frame = { size; try_blocks = tb } } =
      find_raise_label stack ~static_label:i
    in
    let cont = branch_to label cont in
    let rec loop sz tbb =
      if tb == tbb
      then add_pop (sz - size) cont
      else
        match tbb with
        | [] -> assert false
        | try_sz :: tbb ->
          add_pop (sz - try_sz - 4) (Kpoptrap @@ loop try_sz tbb)
    in
    let cont = loop sz stack_info.try_blocks in
    match args with
    | [arg] ->
      (* optim, argument passed in accumulator *)
      comp_expr stack arg cont
    | _ -> comp_exit_args stack args size cont)
  | Trywith { body; id; handler } ->
    let branch1, cont1 = make_branch cont in
    let lbl_handler = new_label () in
    let body_cont =
      Kpoptrap @@ branch1 @@ Klabel lbl_handler @@ Kpush
      @@ comp_expr stack_info
           (add_var id (sz + 1) env)
           handler (sz + 1) (add_pop 1 cont1)
    in
    let stack_info =
      { stack_info with try_blocks = sz @@ stack_info.try_blocks }
    in
    let l = comp_expr stack_info env body (sz + 4) body_cont in
    Kpushtrap lbl_handler @@ l
  | Ifthenelse { cond; ifso; ifnot } -> comp_binary_test stack cond ifso ifnot t
  | Sequence (exp1, exp2) ->
    comp_expr stack exp1 (comp_expr stack_info env exp2 sz cont)
  | While { wh_cond; wh_body } ->
    let lbl_loop = new_label () in
    let lbl_test = new_label () in
    Kbranch lbl_test @@ Klabel lbl_loop @@ Kcheck_signals
    @@ comp_expr stack wh_body
         (Klabel lbl_test
         @@ comp_expr stack wh_cond (Kbranchif lbl_loop @@ add_const_unit cont)
         )
  | For { for_id; for_from; for_to; for_dir; for_body } ->
    let lbl_loop = new_label () in
    let lbl_exit = new_label () in
    let offset = match for_dir with Upto -> 1 | Downto -> -1 in
    let comp = match for_dir with Upto -> Cgt | Downto -> Clt in
    comp_expr stack for_from
      (Kpush
      @@ comp_expr stack_info env for_to (sz + 1)
           (Kpush @@ Kpush @@ Kacc 2 @@ Kintcomp comp @@ Kbranchif lbl_exit
          @@ Klabel lbl_loop @@ Kcheck_signals
           @@ comp_expr stack_info
                (add_var for_id (sz + 1) env)
                for_body (sz + 2)
                (Kacc 1 @@ Kpush @@ Koffsetint offset @@ Kassign 2 @@ Kacc 1
               @@ Kintcomp Cne @@ Kbranchif lbl_loop @@ Klabel lbl_exit
                @@ add_const_unit (add_pop 2 cont))))
  | Switch { arg; int_cases; tag_cases; cases } ->
    let branch, t = make_branch t in
    let t = ref (discard_dead_code t) in
    let lbls = Array.make (Array.length cases) 0 in
    for i = Array.length acts - 1 downto 0 do
      let lbl, t' =
        label_code (comp_expr stack_info env acts.(i) sz (branch @@ !t))
      in
      t := discard_dead_code t';
      lbls.(i) <- lbl
    done;
    (* Build label vectors *)
    let lbl_blocks = Array.map (Array.get lbl) tag_cases in
    let lbl_consts = Array.map (Array.get lbl) int_cases in
    comp_expr stack arg (Kswitch (lbl_consts, lbl_blocks) @@ !t)
  | Assign (id, expr) -> (
    try
      let pos = Ident.find_same id stack.env.ce_stack in
      comp_expr stack expr (Kassign (sz - pos) @@ cont)
    with Not_found -> Misc.fatal_error "Bytegen.comp_expr: assign")
  | Event (lam, lev) -> (
    let ev_defname =
      string_of_scoped_location ~include_zero_alloc:false lev.lev_loc
    in
    let event kind info =
      { ev_pos = 0;
        (* patched in emitcode *)
        ev_module = Compilation_unit.full_path_as_string t.compunit_name;
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
          | Some ({ contents = 1 } as repr) when lev.lev_kind = Lev_function ->
            Event_child repr
          | Some ({ contents = 1 } as repr) -> Event_parent repr
          | Some repr when lev.lev_kind = Lev_function -> Event_parent repr
          | Some repr -> Event_child repr)
      }
    in
    match lev.lev_kind with
    | Lev_before ->
      let c = comp_expr stack lam cont in
      let ev = event Event_before Event_other in
      add_event ev c
    | Lev_function ->
      let c = comp_expr stack lam cont in
      let ev = event Event_pseudo Event_function in
      add_event ev c
    | Lev_pseudo ->
      let c = comp_expr stack lam cont in
      let ev = event Event_pseudo Event_other in
      add_event ev c
    | Lev_after ty ->
      let preserve_tailcall =
        match lam with
        | Prim (prim, _, _) -> preserve_tailcall_for_prim prim
        | Lapply { ap_region_close = rc; _ } | Lsend (_, _, _, _, rc, _, _, _)
          ->
          not (is_nontail rc)
        | _ -> true
      in
      if preserve_tailcall && is_tailcall cont
      then (* don't destroy tail call opt *)
        comp_expr stack lam cont
      else
        let info =
          match lam with
          | Lapply { ap_args = args } -> Event_return (List.length args)
          | Lsend (_, _, _, args, _, _, _, _) ->
            Event_return (List.length args + 1)
          | Prim (_, args, _) -> Event_return (List.length args)
          | _ -> Event_other
        in
        let ev = event (Event_after ty) info in
        let cont1 = add_event ev cont in
        comp_expr stack lam cont1)

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args t stack argl = comp_expr_list t stack (List.rev argl)

and comp_expr_list t stack exprl =
  match exprl with
  | [] -> t
  | [exp] -> comp_expr t stack exp
  | exp :: rem ->
    comp_expr stack exp
      (Kpush @@ comp_expr_list stack_info env rem (sz + 1) cont)

and comp_exit_args stack argl pos cont =
  comp_expr_list_assign stack_info env (List.rev argl) sz pos cont

and comp_expr_list_assign stack exprl pos cont =
  match exprl with
  | [] -> cont
  | exp :: rem ->
    comp_expr stack exp
      (Kassign (sz - pos) @@ comp_expr_list_assign stack rem (pos - 1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test stack_info env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Const const_unit
    then
      let lbl_end, cont1 = label_code cont in
      Kstrictbranchifnot lbl_end @@ comp_expr stack ifso cont1
    else
      match code_as_jump stack_info ifso sz with
      | Some label ->
        let cont = comp_expr stack ifnot cont in
        Kbranchif label @@ cont
      | None -> (
        match code_as_jump stack_info ifnot sz with
        | Some label ->
          let cont = comp_expr stack ifso cont in
          Kbranchifnot label @@ cont
        | None ->
          let branch_end, cont1 = make_branch cont in
          let lbl_not, cont2 = label_code (comp_expr stack ifnot cont1) in
          Kbranchifnot lbl_not @@ comp_expr stack ifso (branch_end @@ cont2))
  in
  comp_expr stack cond cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  let stack_info = create_stack_info () in
  let code = comp_expr stack exp cont in
  let used_safe = !(stack_info.max_stack_used) + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold
  then
    Kconst (Const_base (Const_int used_safe))
    @@ Kccall ("caml_ensure_stack_capacity", 1)
    @@ code
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
  let cont = comp_block env tc.body arity (Kreturn arity @@ cont) in
  if arity > 1
  then Krestart @@ Klabel tc.label @@ Kgrab (arity - 1) @@ cont
  else Klabel tc.label @@ cont

let comp_remainder cont =
  let c = ref cont in
  (try
     while true do
       c := comp_function (Stack.pop functions_to_compile) !c
     done
   with Stack.Empty -> ());
  !c

(**** Compilation of a lambda phrase ****)

let compile_gen ?modulename ~init_stack expr =
  reset ();
  (match modulename with Some name -> compunit_name := name | None -> ());
  Fun.protect ~finally:reset (fun () ->
      let init_code = comp_block empty_env expr init_stack [] in
      if Stack.length functions_to_compile > 0
      then
        let lbl_init = new_label () in
        Kbranch lbl_init @@ comp_remainder (Klabel lbl_init @@ init_code), false
      else init_code, true)

let compile_implementation modulename expr =
  fst (compile_gen ~modulename ~init_stack:0 expr)

let compile_phrase expr = compile_gen ~init_stack:1 expr
