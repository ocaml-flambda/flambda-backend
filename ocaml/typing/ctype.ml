(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Operations on core types *)

open Misc
open Asttypes
open Types
open Btype
open Errortrace
open Mode
open Local_store
module Int = Misc.Stdlib.Int

(*
   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctly
   manipulated by [apply], [expand_head] and [moregeneral].
*)

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (+++ TO DO...), and abbreviations defined by a
     class do not depend on sharing thanks to constrained
     abbreviations. (Of course, even if some sharing is lost, typing
     will still be correct.)
   - All nodes of a type have a level : that way, one knows whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing (generic level being considered
     as greatest).
   - The level of a type constructor is superior to the binding
     time of its path.
   - Recursive types without limitation should be handled (even if
     there is still an occur check). This avoid treating specially the
     case for objects, for instance. Furthermore, the occur check
     policy can then be easily changed.
*)

(**** Errors ****)

(* There are two classes of errortrace-related exceptions: *traces* and
   *errors*.  The former, whose names end with [_trace], contain
   [Errortrace.trace]s, representing traces that are currently being built; they
   are local to this file.  All the internal functions that implement
   unification, type equality, and moregen raise trace exceptions.  Once we are
   done, in the top level functions such as [unify], [equal], and [moregen], we
   catch the trace exceptions and transform them into the analogous error
   exception.  This indicates that we are done building the trace, and expect
   the error to flow out of unification, type equality, or moregen into
   surrounding code (with some few exceptions when these top-level functions are
   used as building blocks elsewhere.)  Only the error exceptions are exposed in
   [ctype.mli]; the trace exceptions are an implementation detail.  Any trace
   exception that escapes from a function in this file is a bug. *)

exception Unify_trace    of unification trace
exception Equality_trace of comparison  trace
exception Moregen_trace  of comparison  trace

exception Unify    of unification_error
exception Equality of equality_error
exception Moregen  of moregen_error
exception Subtype  of Subtype.error

exception Escape of type_expr escape

(* For local use: throw the appropriate exception.  Can be passed into local
   functions as a parameter *)
type _ trace_exn =
| Unify    : unification trace_exn
| Moregen  : comparison  trace_exn
| Equality : comparison  trace_exn

let raise_trace_for
      (type variant)
      (tr_exn : variant trace_exn)
      (tr     : variant trace) : 'a =
  match tr_exn with
  | Unify    -> raise (Unify_trace    tr)
  | Equality -> raise (Equality_trace tr)
  | Moregen  -> raise (Moregen_trace  tr)

(* Uses of this function are a bit suspicious, as we usually want to maintain
   trace information; sometimes it makes sense, however, since we're maintaining
   the trace at an outer exception handler. *)
let raise_unexplained_for tr_exn =
  raise_trace_for tr_exn []

let raise_for tr_exn e =
  raise_trace_for tr_exn [e]

(* Thrown from [moregen_kind] *)
exception Public_method_to_private_method

let escape kind = {kind; context = None}
let escape_exn kind = Escape (escape kind)
let scope_escape_exn ty = escape_exn (Equation ty)
let raise_escape_exn kind = raise (escape_exn kind)
let raise_scope_escape_exn ty = raise (scope_escape_exn ty)

exception Tags of label * label

let () =
  Location.register_error_of_exn
    (function
      | Tags (l, l') ->
          Some
            Location.
              (errorf ~loc:(in_file !input_name)
                 "In this program,@ variant constructors@ `%s and `%s@ \
                  have the same hash value.@ Change one of them." l l'
              )
      | _ -> None
    )

exception Cannot_expand

exception Cannot_apply

exception Cannot_subst

exception Cannot_unify_universal_variables

exception Incompatible

(**** Type level management ****)

let current_level = s_ref 0
let nongen_level = s_ref 0
let global_level = s_ref 0
let saved_level = s_ref []

let get_current_level () = !current_level
let init_def level = current_level := level; nongen_level := level
let begin_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level; nongen_level := !current_level
let begin_class_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level
let raise_nongen_level () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  nongen_level := !current_level
let end_def () =
  let (cl, nl) = List.hd !saved_level in
  saved_level := List.tl !saved_level;
  current_level := cl; nongen_level := nl
let create_scope () =
  init_def (!current_level + 1);
  !current_level

let wrap_end_def f = Misc.try_finally f ~always:end_def

let with_local_level ?post f =
  begin_def ();
  let result = wrap_end_def f in
  Option.iter (fun g -> g result) post;
  result
let with_local_level_if cond f ~post =
  if cond then with_local_level f ~post else f ()
let with_local_level_iter f ~post =
  begin_def ();
  let result, l = wrap_end_def f in
  List.iter post l;
  result
let with_local_level_iter_if cond f ~post =
  if cond then with_local_level_iter f ~post else fst (f ())
let with_local_level_if_principal f ~post =
  with_local_level_if !Clflags.principal f ~post
let with_local_level_iter_if_principal f ~post =
  with_local_level_iter_if !Clflags.principal f ~post
let with_level ~level f =
  begin_def (); init_def level;
  let result = wrap_end_def f in
  result
let with_level_if cond ~level f =
  if cond then with_level ~level f else f ()

let with_local_level_for_class ?post f =
  begin_class_def ();
  let result = wrap_end_def f in
  Option.iter (fun g -> g result) post;
  result

let with_raised_nongen_level f =
  raise_nongen_level ();
  wrap_end_def f


let reset_global_level () =
  global_level := !current_level
let increase_global_level () =
  let gl = !global_level in
  global_level := !current_level;
  gl
let restore_global_level gl =
  global_level := gl

(**** Control tracing of GADT instances *)

let trace_gadt_instances = ref false
let check_trace_gadt_instances env =
  not !trace_gadt_instances && Env.has_local_constraints env &&
  (trace_gadt_instances := true; cleanup_abbrev (); true)

let reset_trace_gadt_instances b =
  if b then trace_gadt_instances := false

let wrap_trace_gadt_instances env f x =
  let b = check_trace_gadt_instances env in
  let y = f x in
  reset_trace_gadt_instances b;
  y

(**** Abbreviations without parameters ****)
(* Shall reset after generalizing *)

let simple_abbrevs = ref Mnil

let proper_abbrevs tl abbrev =
  if tl <> [] || !trace_gadt_instances || !Clflags.principal
  then abbrev
  else simple_abbrevs

(**** Some type creators ****)

(* Re-export generic type creators *)

let newty desc              = newty2 ~level:!current_level desc
let new_scoped_ty scope desc = newty3 ~level:!current_level ~scope desc

let newvar ?name jkind =
  newty2 ~level:!current_level (Tvar { name; jkind })
let new_rep_var ?name ~why () =
  let jkind, sort = Jkind.of_new_sort_var ~why in
  newvar ?name jkind, sort
let newvar2 ?name level jkind = newty2 ~level (Tvar { name; jkind })
let new_global_var ?name jkind =
  newty2 ~level:!global_level (Tvar { name; jkind })
let newstub ~scope jkind =
  newty3 ~level:!current_level ~scope (Tvar { name = None; jkind })

let newobj fields      = newty (Tobject (fields, ref None))

let newconstr path tyl = newty (Tconstr (path, tyl, ref Mnil))

let newmono ty = newty (Tpoly(ty, []))

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(**** unification mode ****)

type equations_generation =
  | Forbidden
  | Allowed of { equated_types : TypePairs.t }

type unification_mode =
  | Expression (* unification in expression *)
  | Pattern of
      { equations_generation : equations_generation;
        assume_injective : bool;
        allow_recursive_equations : bool; }
    (* unification in pattern which may add local constraints *)
  | Subst
    (* unification during type constructor expansion; more
       relaxed than [Expression] in some cases. *)

let umode = ref Expression

let in_pattern_mode () =
  match !umode with
  | Expression | Subst -> false
  | Pattern _ -> true

let in_subst_mode () =
  match !umode with
  | Expression | Pattern _ -> false
  | Subst -> true

let can_generate_equations () =
  match !umode with
  | Expression | Subst | Pattern { equations_generation = Forbidden } -> false
  | Pattern { equations_generation = Allowed _ } -> true

(* Can only be called when generate_equations is true.  Tracks equations only to
   improve error messages. *)
let record_equation t1 t2 =
  match !umode with
  | Expression | Subst | Pattern { equations_generation = Forbidden } ->
      assert false
  | Pattern { equations_generation = Allowed { equated_types } } ->
      TypePairs.add equated_types (t1, t2)

let can_assume_injective () =
  match !umode with
  | Expression | Subst -> false
  | Pattern { assume_injective } -> assume_injective

let in_counterexample () =
  match !umode with
  | Expression | Subst -> false
  | Pattern { allow_recursive_equations } -> allow_recursive_equations

let allow_recursive_equations () =
  !Clflags.recursive_types
  || match !umode with
     | Expression | Subst -> false
     | Pattern { allow_recursive_equations } -> allow_recursive_equations

let set_mode_pattern ~allow_recursive_equations ~equated_types f =
  let equations_generation = Allowed { equated_types } in
  let assume_injective = true in
  let new_umode =
    Pattern
      { equations_generation;
        assume_injective;
        allow_recursive_equations }
  in
  Misc.protect_refs [ Misc.R (umode, new_umode) ] f

let without_assume_injective f =
  match !umode with
  | Expression | Subst -> f ()
  | Pattern r ->
      let new_umode = Pattern { r with assume_injective = false } in
      Misc.protect_refs [ Misc.R (umode, new_umode) ] f

let without_generating_equations f =
  match !umode with
  | Expression | Subst -> f ()
  | Pattern r ->
    let new_umode = Pattern { r with equations_generation = Forbidden } in
    Misc.protect_refs [ Misc.R (umode, new_umode) ] f

(* Unification generally must check that the jkinds of the two types being
   unified agree.  However, sometimes we need to delay or skip these jkind
   checks, and this is tracked by the [jkind_unification_mode] in [lmode].

   - [Perform_checks] is the usual case where we just do the checks.

   - [Delay_checks] is for when we want to delay checks to be performed later.
     Unification sometimes occurs before the checking for circular /
     ill-founded types.  Jkind checking does things that blow up on those
     types.  So we save the checks that would be done and do them after the
     circularity checking.

   - [Skip_checks] is for when we want to skip the checks entirely.  Sometimes
     unification is reached through apply/subst in contexts that make jkind
     checking unnecessary.  For now, we're only using this in the more specific
     case that apply is reached through [Subst.ctype_apply_env_empty], because
     the empty environment also makes it impossible for us to perform the
     relevant checks.
*)
type jkind_unification_mode =
  | Perform_checks
  | Delay_checks of (type_expr * Jkind.t) list ref
  | Skip_checks

let lmode = ref Perform_checks

let delay_jkind_checks_in f =
  let r = ref [] in
  Misc.protect_refs [Misc.R (lmode, Delay_checks r)] f;
  !r

let skip_jkind_checks_in f =
  Misc.protect_refs [Misc.R (lmode, Skip_checks)] f

(*** Checks for type definitions ***)

let rec in_current_module = function
  | Path.Pident _ -> true
  | Path.Pdot _ | Path.Papply _ -> false
  | Path.Pextra_ty (p, _) -> in_current_module p

let in_pervasives p =
  in_current_module p &&
  try ignore (Env.find_type p (Lazy.force Env.initial)); true
  with Not_found -> false

let is_datatype decl=
  match decl.type_kind with
    Type_record _ | Type_variant _ | Type_open -> true
  | Type_abstract _ -> false


                  (**********************************************)
                  (*  Miscellaneous operations on object types  *)
                  (**********************************************)

(* Note:
   We need to maintain some invariants:
   * cty_self must be a Tobject
   * ...
*)

(**** Object field manipulation. ****)

let object_fields ty =
  match get_desc ty with
    Tobject (fields, _) -> fields
  | _                   -> assert false

let flatten_fields ty =
  let rec flatten l ty =
    match get_desc ty with
      Tfield(s, k, ty1, ty2) ->
        flatten ((s, k, ty1)::l) ty2
    | _ ->
        (l, ty)
  in
    let (l, r) = flatten [] ty in
    (List.sort (fun (n, _, _) (n', _, _) -> compare n n') l, r)

let build_fields level =
  List.fold_right
    (fun (s, k, ty1) ty2 -> newty2 ~level (Tfield(s, k, ty1, ty2)))

let associate_fields fields1 fields2 =
  let rec associate p s s' =
    function
      (l, []) ->
        (List.rev p, (List.rev s) @ l, List.rev s')
    | ([], l') ->
        (List.rev p, List.rev s, (List.rev s') @ l')
    | ((n, k, t)::r, (n', k', t')::r') when n = n' ->
        associate ((n, k, t, k', t')::p) s s' (r, r')
    | ((n, k, t)::r, ((n', _k', _t')::_ as l')) when n < n' ->
        associate p ((n, k, t)::s) s' (r, l')
    | (((_n, _k, _t)::_ as l), (n', k', t')::r') (* when n > n' *) ->
        associate p s ((n', k', t')::s') (l, r')
  in
  associate [] [] [] (fields1, fields2)

(**** Check whether an object is open ****)

(* +++ The abbreviation should eventually be expanded *)
let rec object_row ty =
  match get_desc ty with
    Tobject (t, _)     -> object_row t
  | Tfield(_, _, _, t) -> object_row t
  | _ -> ty

let opened_object ty =
  match get_desc (object_row ty) with
  | Tvar _  | Tunivar _ | Tconstr _ -> true
  | _                               -> false

let concrete_object ty =
  match get_desc (object_row ty) with
  | Tvar _             -> false
  | _                  -> true

(**** Row variable of an object type ****)

let rec fields_row_variable ty =
  match get_desc ty with
  | Tfield (_, _, _, ty) -> fields_row_variable ty
  | Tvar _               -> ty
  | _                    -> assert false

(**** Object name manipulation ****)
(* +++ Bientot obsolete *)

let set_object_name id params ty =
  match get_desc ty with
  | Tobject (fi, nm) ->
      let rv = fields_row_variable fi in
      set_name nm (Some (Path.Pident id, rv::params))
  | Tconstr (_, _, _) -> ()
  | _ -> fatal_error "Ctype.set_object_name"

let remove_object_name ty =
  match get_desc ty with
    Tobject (_, nm)   -> set_name nm None
  | Tconstr (_, _, _) -> ()
  | _                 -> fatal_error "Ctype.remove_object_name"

                  (*******************************************)
                  (*  Miscellaneous operations on row types  *)
                  (*******************************************)

let sort_row_fields = List.sort (fun (p,_) (q,_) -> compare p q)

let rec merge_rf r1 r2 pairs fi1 fi2 =
  match fi1, fi2 with
    (l1,f1 as p1)::fi1', (l2,f2 as p2)::fi2' ->
      if l1 = l2 then merge_rf r1 r2 ((l1,f1,f2)::pairs) fi1' fi2' else
      if l1 < l2 then merge_rf (p1::r1) r2 pairs fi1' fi2 else
      merge_rf r1 (p2::r2) pairs fi1 fi2'
  | [], _ -> (List.rev r1, List.rev_append r2 fi2, pairs)
  | _, [] -> (List.rev_append r1 fi1, List.rev r2, pairs)

let merge_row_fields fi1 fi2 =
  match fi1, fi2 with
    [], _ | _, [] -> (fi1, fi2, [])
  | [p1], _ when not (List.mem_assoc (fst p1) fi2) -> (fi1, fi2, [])
  | _, [p2] when not (List.mem_assoc (fst p2) fi1) -> (fi1, fi2, [])
  | _ -> merge_rf [] [] [] (sort_row_fields fi1) (sort_row_fields fi2)

let rec filter_row_fields erase = function
    [] -> []
  | (_l,f as p)::fi ->
      let fi = filter_row_fields erase fi in
      match row_field_repr f with
        Rabsent -> fi
      | Reither(_,_,false) when erase ->
          link_row_field_ext ~inside:f rf_absent; fi
      | _ -> p :: fi


(* Ensure all mode variables are fully determined *)
let remove_mode_and_jkind_variables ty =
  let visited = ref TypeSet.empty in
  let rec go ty =
    if TypeSet.mem ty !visited then () else begin
      visited := TypeSet.add ty !visited;
      match get_desc ty with
      | Tvar { jkind } -> Jkind.default_to_value jkind
      | Tunivar { jkind } -> Jkind.default_to_value jkind
      | Tarrow ((_,marg,mret),targ,tret,_) ->
         let _ = Alloc.zap_to_legacy marg in
         let _ = Alloc.zap_to_legacy mret in
         go targ; go tret
      | _ -> iter_type_expr go ty
    end
  in go ty

                    (**************************************)
                    (*  Check genericity of type schemes  *)
                    (**************************************)

type variable_kind = Row_variable | Type_variable
exception Non_closed of type_expr * variable_kind

(* [free_vars] collects the variables of the input type expression. It
   is used for several different things in the type-checker, with the
   following bells and whistles:
   - If [env] is Some typing environment, types in the environment
     are expanded to check whether the apparently-free variable would vanish
     during expansion.
   - We collect both type variables and row variables, paired with
     a [variable_kind] to distinguish them.
   - We do not count "virtual" free variables -- free variables stored in
     the abbreviation of an object type that has been expanded (we store
     the abbreviations for use when displaying the type).

   [free_vars] returns a [(variable * bool) list], while
   [free_variables] below drops the type/row information
   and only returns a [variable list].
 *)
let free_vars ?env tys =
  let rec fv ~kind acc ty =
    if not (try_mark_node ty) then acc
    else match get_desc ty, env with
      | Tvar _, _ ->
          (ty, kind) :: acc
      | Tconstr (path, tl, _), Some env ->
          let acc =
            match Env.find_type_expansion path env with
            | exception Not_found -> acc
            | (_, body, _) ->
                if get_level body = generic_level then acc
                else (ty, kind) :: acc
          in
          List.fold_left (fv ~kind:Type_variable) acc tl
      | Tobject (ty, _), _ ->
          (* ignoring the second parameter of [Tobject] amounts to not
             counting "virtual free variables". *)
          fv ~kind:Row_variable acc ty
      | Tfield (_, _, ty1, ty2), _ ->
          let acc = fv ~kind:Type_variable acc ty1 in
          fv ~kind:Row_variable acc ty2
      | Tvariant row, _ ->
          let acc = fold_row (fv ~kind:Type_variable) acc row in
          if static_row row then acc
          else fv ~kind:Row_variable acc (row_more row)
      | _    ->
          fold_type_expr (fv ~kind) acc ty
  in
  List.fold_left (fv ~kind:Type_variable) [] tys

let free_variables ?env ty =
  let tl = List.map fst (free_vars ?env [ty]) in
  unmark_type ty;
  tl

let free_non_row_variables_of_list tyl =
  let tl =
    List.filter_map (function (v, Type_variable) -> Some v
                            | (_, Row_variable) -> None)
      (free_vars tyl)
  in
  List.iter unmark_type tyl;
  tl

let closed_type ty =
  remove_mode_and_jkind_variables ty;
  match free_vars [ty] with
      []           -> ()
  | (v, real) :: _ -> raise (Non_closed (v, real))

let closed_parameterized_type params ty =
  List.iter mark_type params;
  let ok =
    try closed_type ty; true with Non_closed _ -> false in
  List.iter unmark_type params;
  unmark_type ty;
  ok

let closed_type_decl decl =
  try
    List.iter mark_type decl.type_params;
    List.iter remove_mode_and_jkind_variables decl.type_params;
    begin match decl.type_kind with
      Type_abstract _ ->
        ()
    | Type_variant (v, _rep) ->
        List.iter
          (fun {cd_args; cd_res; _} ->
            match cd_res with
            | Some res_ty ->
                (* gadts cannot have free type variables, but they might
                   have undefaulted sort variables; these lines default
                   them. Test case: typing-layouts-gadt-sort-var/test.ml *)
                begin match cd_args with
                | Cstr_tuple l -> List.iter (fun (ty, _) ->
                    remove_mode_and_jkind_variables ty) l
                | Cstr_record l -> List.iter (fun l ->
                    remove_mode_and_jkind_variables l.ld_type) l
                end;
                remove_mode_and_jkind_variables res_ty
            | None -> List.iter closed_type (tys_of_constr_args cd_args)
          )
          v
    | Type_record(r, _rep) ->
        List.iter (fun l -> closed_type l.ld_type) r
    | Type_open -> ()
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> closed_type ty
    end;
    unmark_type_decl decl;
    None
  with Non_closed (ty, _) ->
    unmark_type_decl decl;
    Some ty

let closed_extension_constructor ext =
  try
    List.iter mark_type ext.ext_type_params;
    begin match ext.ext_ret_type with
    | Some res_ty ->
        (* gadts cannot have free type variables, but they might
           have undefaulted sort variables; these lines default
           them. Test case: typing-layouts-gadt-sort-var/test_extensible.ml *)
        iter_type_expr_cstr_args remove_mode_and_jkind_variables ext.ext_args;
        remove_mode_and_jkind_variables res_ty
    | None ->
        iter_type_expr_cstr_args closed_type ext.ext_args
    end;
    unmark_extension_constructor ext;
    None
  with Non_closed (ty, _) ->
    unmark_extension_constructor ext;
    Some ty

type closed_class_failure = {
  free_variable: type_expr * variable_kind;
  meth: string;
  meth_ty: type_expr;
}
exception CCFailure of closed_class_failure

let closed_class params sign =
  List.iter mark_type params;
  ignore (try_mark_node sign.csig_self_row);
  try
    Meths.iter
      (fun lab (priv, _, ty) ->
        if priv = Mpublic then begin
          try closed_type ty with Non_closed (ty0, variable_kind) ->
            raise (CCFailure {
              free_variable = (ty0, variable_kind);
              meth = lab;
              meth_ty = ty;
            })
        end)
      sign.csig_meths;
    List.iter unmark_type params;
    unmark_class_signature sign;
    None
  with CCFailure reason ->
    List.iter unmark_type params;
    unmark_class_signature sign;
    Some reason


                            (**********************)
                            (*  Type duplication  *)
                            (**********************)


(* Duplicate a type, preserving only type variables *)
let duplicate_type ty =
  Subst.type_expr Subst.identity ty

(* Same, for class types *)
let duplicate_class_type ty =
  Subst.class_type Subst.identity ty


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec generalize ty =
  let level = get_level ty in
  if (level > !current_level) && (level <> generic_level) then begin
    set_level ty generic_level;
    (* recur into abbrev for the speed *)
    begin match get_desc ty with
      Tconstr (_, _, abbrev) ->
        iter_abbrev generalize !abbrev
    | _ -> ()
    end;
    iter_type_expr generalize ty
  end

let generalize ty =
  simple_abbrevs := Mnil;
  generalize ty

(* Generalize the structure and lower the variables *)

let rec generalize_structure ty =
  let level = get_level ty in
  if level <> generic_level then begin
    if is_Tvar ty && level > !current_level then
      set_level ty !current_level
    else if level > !current_level then begin
      begin match get_desc ty with
        Tconstr (_, _, abbrev) ->
          abbrev := Mnil
      | _ -> ()
      end;
      set_level ty generic_level;
      iter_type_expr generalize_structure ty
    end
  end

let generalize_structure ty =
  simple_abbrevs := Mnil;
  generalize_structure ty

(* Generalize the spine of a function, if the level >= !current_level *)

let rec generalize_spine ty =
  let level = get_level ty in
  if level < !current_level || level = generic_level then () else
  match get_desc ty with
    Tarrow (_, ty1, ty2, _) ->
      set_level ty generic_level;
      generalize_spine ty1;
      generalize_spine ty2;
  | Tpoly (ty', _) ->
      set_level ty generic_level;
      generalize_spine ty'
  | Ttuple tyl ->
      set_level ty generic_level;
      List.iter (fun (_,t) -> generalize_spine t) tyl
  | Tpackage (_, fl) ->
      set_level ty generic_level;
      List.iter (fun (_n, ty) -> generalize_spine ty) fl
  | Tconstr (_, tyl, memo) ->
      set_level ty generic_level;
      memo := Mnil;
      List.iter generalize_spine tyl
  | _ -> ()

let forward_try_expand_safe = (* Forward declaration *)
  ref (fun _env _ty -> assert false)

(*
   Lower the levels of a type (assume [level] is not
   [generic_level]).
*)

let rec normalize_package_path env p =
  let t =
    try (Env.find_modtype_lazy p env).mtd_type
    with Not_found -> None
  in
  match t with
  | Some (Mty_ident p) -> normalize_package_path env p
  | Some (Mty_signature _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _) | None ->
      match p with
        Path.Pdot (p1, s) ->
          (* For module aliases *)
          let p1' = Env.normalize_module_path None env p1 in
          if Path.same p1 p1' then p else
          normalize_package_path env (Path.Pdot (p1', s))
      | _ -> p

let rec check_scope_escape env level ty =
  let orig_level = get_level ty in
  if try_logged_mark_node ty then begin
    if level < get_scope ty then
      raise_scope_escape_exn ty;
    begin match get_desc ty with
    | Tconstr (p, _, _) when level < Path.scope p ->
        begin match !forward_try_expand_safe env ty with
        | ty' ->
            check_scope_escape env level ty'
        | exception Cannot_expand ->
            raise_escape_exn (Constructor p)
        end
    | Tpackage (p, fl) when level < Path.scope p ->
        let p' = normalize_package_path env p in
        if Path.same p p' then raise_escape_exn (Module_type p);
        check_scope_escape env level
          (newty2 ~level:orig_level (Tpackage (p', fl)))
    | _ ->
        iter_type_expr (check_scope_escape env level) ty
    end;
  end

let check_scope_escape env level ty =
  let snap = snapshot () in
  try check_scope_escape env level ty; backtrack snap
  with Escape e ->
    backtrack snap;
    raise (Escape { e with context = Some ty })

let rec update_scope scope ty =
  if get_scope ty < scope then begin
    if get_level ty < scope then raise_scope_escape_exn ty;
    set_scope ty scope;
    (* Only recurse in principal mode as this is not necessary for soundness *)
    if !Clflags.principal then iter_type_expr (update_scope scope) ty
  end

let update_scope_for tr_exn scope ty =
  try
    update_scope scope ty
  with Escape e -> raise_for tr_exn (Escape e)

(* Note: the level of a type constructor must be greater than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
    (without this constraint, the type system would actually be unsound.)
*)

let rec update_level env level expand ty =
  if get_level ty > level then begin
    if level < get_scope ty then raise_scope_escape_exn ty;
    match get_desc ty with
      Tconstr(p, _tl, _abbrev) when level < Path.scope p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          let ty' = !forward_try_expand_safe env ty in
          link_type ty ty';
          update_level env level expand ty'
        with Cannot_expand ->
          raise_escape_exn (Constructor p)
        end
    | Tconstr(p, (_ :: _ as tl), _) ->
        let variance =
          try (Env.find_type p env).type_variance
          with Not_found -> List.map (fun _ -> Variance.unknown) tl in
        let needs_expand =
          expand ||
          List.exists2
            (fun var ty -> var = Variance.null && get_level ty > level)
            variance tl
        in
        begin try
          if not needs_expand then raise Cannot_expand;
          let ty' = !forward_try_expand_safe env ty in
          link_type ty ty';
          update_level env level expand ty'
        with Cannot_expand ->
          set_level ty level;
          iter_type_expr (update_level env level expand) ty
        end
    | Tpackage (p, fl) when level < Path.scope p ->
        let p' = normalize_package_path env p in
        if Path.same p p' then raise_escape_exn (Module_type p);
        set_type_desc ty (Tpackage (p', fl));
        update_level env level expand ty
    | Tobject (_, ({contents=Some(p, _tl)} as nm))
      when level < Path.scope p ->
        set_name nm None;
        update_level env level expand ty
    | Tvariant row ->
        begin match row_name row with
        | Some (p, _tl) when level < Path.scope p ->
            set_type_desc ty (Tvariant (set_row_name row None))
        | _ -> ()
        end;
        set_level ty level;
        iter_type_expr (update_level env level expand) ty
    | Tfield(lab, _, ty1, _)
      when lab = dummy_method && level < get_scope ty1 ->
        raise_escape_exn Self
    | _ ->
        set_level ty level;
        (* XXX what about abbreviations in Tconstr ? *)
        iter_type_expr (update_level env level expand) ty
  end

(* First try without expanding, then expand everything,
   to avoid combinatorial blow-up *)
let update_level env level ty =
  if get_level ty > level then begin
    let snap = snapshot () in
    try
      update_level env level false ty
    with Escape _ ->
      backtrack snap;
      update_level env level true ty
  end

let update_level_for tr_exn env level ty =
  try
    update_level env level ty
  with Escape e -> raise_for tr_exn (Escape e)

(* Lower level of type variables inside contravariant branches *)

let rec lower_contravariant env var_level visited contra ty =
  let must_visit =
    get_level ty > var_level &&
    match Hashtbl.find visited (get_id ty) with
    | done_contra -> contra && not done_contra
    | exception Not_found -> true
  in
  if must_visit then begin
    Hashtbl.add visited (get_id ty) contra;
    let lower_rec = lower_contravariant env var_level visited in
    match get_desc ty with
      Tvar _ -> if contra then set_level ty var_level
    | Tconstr (_, [], _) -> ()
    | Tconstr (path, tyl, _abbrev) ->
       let variance, maybe_expand =
         try
           let typ = Env.find_type path env in
           typ.type_variance,
           type_kind_is_abstract typ
          with Not_found ->
            (* See testsuite/tests/typing-missing-cmi-2 for an example *)
            List.map (fun _ -> Variance.unknown) tyl,
            false
        in
        if List.for_all ((=) Variance.null) variance then () else
          let not_expanded () =
            List.iter2
              (fun v t ->
                if v = Variance.null then () else
                  if Variance.(mem May_weak v)
                  then lower_rec true t
                  else lower_rec contra t)
              variance tyl in
          if maybe_expand then (* we expand cautiously to avoid missing cmis *)
            match !forward_try_expand_safe env ty with
            | ty -> lower_rec contra ty
            | exception Cannot_expand -> not_expanded ()
          else not_expanded ()
    | Tpackage (_, fl) ->
        List.iter (fun (_n, ty) -> lower_rec true ty) fl
    | Tarrow (_, t1, t2, _) ->
        lower_rec true t1;
        lower_rec contra t2
    | _ ->
        iter_type_expr (lower_rec contra) ty
  end

let lower_variables_only env level ty =
  simple_abbrevs := Mnil;
  lower_contravariant env level (Hashtbl.create 7) true ty

let lower_contravariant env ty =
  simple_abbrevs := Mnil;
  lower_contravariant env !nongen_level (Hashtbl.create 7) false ty

let rec generalize_class_type' gen =
  function
    Cty_constr (_, params, cty) ->
      List.iter gen params;
      generalize_class_type' gen cty
  | Cty_signature csig ->
      gen csig.csig_self;
      gen csig.csig_self_row;
      Vars.iter (fun _ (_, _, ty) -> gen ty) csig.csig_vars;
      Meths.iter (fun _ (_, _, ty) -> gen ty) csig.csig_meths
  | Cty_arrow (_, ty, cty) ->
      gen ty;
      generalize_class_type' gen cty

let generalize_class_type cty =
  generalize_class_type' generalize cty

let generalize_class_type_structure cty =
  generalize_class_type' generalize_structure cty

(* Correct the levels of type [ty]. *)
let correct_levels ty =
  duplicate_type ty

(* Only generalize the type ty0 in ty *)
let limited_generalize ty0 ty =
  let graph = Hashtbl.create 17 in
  let idx = ref lowest_level in
  let roots = ref [] in

  let rec inverse pty ty =
    let level = get_level ty in
    if (level > !current_level) || (level = generic_level) then begin
      decr idx;
      Hashtbl.add graph !idx (ty, ref pty);
      if (level = generic_level) || eq_type ty ty0 then
        roots := ty :: !roots;
      set_level ty !idx;
      iter_type_expr (inverse [ty]) ty
    end else if level < lowest_level then begin
      let (_, parents) = Hashtbl.find graph level in
      parents := pty @ !parents
    end

  and generalize_parents ty =
    let idx = get_level ty in
    if idx <> generic_level then begin
      set_level ty generic_level;
      List.iter generalize_parents !(snd (Hashtbl.find graph idx));
      (* Special case for rows: must generalize the row variable *)
      match get_desc ty with
        Tvariant row ->
          let more = row_more row in
          let lv = get_level more in
          if (lv < lowest_level || lv > !current_level)
          && lv <> generic_level then set_level more generic_level
      | _ -> ()
    end
  in

  inverse [] ty;
  if get_level ty0 < lowest_level then
    iter_type_expr (inverse []) ty0;
  List.iter generalize_parents !roots;
  Hashtbl.iter
    (fun _ (ty, _) ->
       if get_level ty <> generic_level then set_level ty !current_level)
    graph

let limited_generalize_class_type rv cty =
  generalize_class_type' (limited_generalize rv) cty

(* Compute statically the free univars of all nodes in a type *)
(* This avoids doing it repeatedly during instantiation *)

type inv_type_expr =
    { inv_type : type_expr;
      mutable inv_parents : inv_type_expr list }

let rec inv_type hash pty ty =
  try
    let inv = TypeHash.find hash ty in
    inv.inv_parents <- pty @ inv.inv_parents
  with Not_found ->
    let inv = { inv_type = ty; inv_parents = pty } in
    TypeHash.add hash ty inv;
    iter_type_expr (inv_type hash [inv]) ty

let compute_univars ty =
  let inverted = TypeHash.create 17 in
  inv_type inverted [] ty;
  let node_univars = TypeHash.create 17 in
  let rec add_univar univ inv =
    match get_desc inv.inv_type with
      Tpoly (_ty, tl) when List.memq (get_id univ) (List.map get_id tl) -> ()
    | _ ->
        try
          let univs = TypeHash.find node_univars inv.inv_type in
          if not (TypeSet.mem univ !univs) then begin
            univs := TypeSet.add univ !univs;
            List.iter (add_univar univ) inv.inv_parents
          end
        with Not_found ->
          TypeHash.add node_univars inv.inv_type (ref(TypeSet.singleton univ));
          List.iter (add_univar univ) inv.inv_parents
  in
  TypeHash.iter (fun ty inv -> if is_Tunivar ty then add_univar ty inv)
    inverted;
  fun ty ->
    try !(TypeHash.find node_univars ty) with Not_found -> TypeSet.empty


let fully_generic ty =
  let rec aux ty =
    if not_marked_node ty then
      if get_level ty = generic_level then
        (flip_mark_node ty; iter_type_expr aux ty)
      else raise Exit
  in
  let res = try aux ty; true with Exit -> false in
  unmark_type ty;
  res


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


let rec find_repr p1 =
  function
    Mnil ->
      None
  | Mcons (Public, p2, ty, _, _) when Path.same p1 p2 ->
      Some ty
  | Mcons (_, _, _, _, rem) ->
      find_repr p1 rem
  | Mlink {contents = rem} ->
      find_repr p1 rem

(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is.

   During instantiation, the result of copying a generic node is
   "cached" in-place by temporarily mutating the node description by
   a stub [Tsubst (newvar ())] using [For_copy.redirect_desc]. The
   scope of this mutation is determined by the [copy_scope] parameter,
   and the [For_copy.with_scope] helper is in charge of creating a new
   scope and performing the necessary book-keeping -- in particular
   reverting the in-place updates after the instantiation is done. *)

let abbreviations = ref (ref Mnil)
  (* Abbreviation memorized. *)

(* partial: we may not wish to copy the non generic types
   before we call type_pat *)
let rec copy ?partial ?keep_names copy_scope ty =
  let copy = copy ?partial ?keep_names copy_scope in
  match get_desc ty with
    Tsubst (ty, _) -> ty
  | desc ->
    let level = get_level ty in
    if level <> generic_level && partial = None then ty else
    (* We only forget types that are non generic and do not contain
       free univars *)
    let forget =
      if level = generic_level then generic_level else
      match partial with
        None -> assert false
      | Some (free_univars, keep) ->
          if not (is_Tpoly ty) && TypeSet.is_empty (free_univars ty) then
            if keep then level else !current_level
          else generic_level
    in
    if forget <> generic_level then
      (* Using jkind "any" is ok here: We're forgetting the type because it
         will be unified with the original later. *)
      newty2 ~level:forget
        (Tvar { name = None; jkind = Jkind.any ~why:Dummy_jkind })
    else
    let t = newstub ~scope:(get_scope ty) (Jkind.any ~why:Dummy_jkind) in
    For_copy.redirect_desc copy_scope ty (Tsubst (t, None));
    let desc' =
      match desc with
      | Tconstr (p, tl, _) ->
          let abbrevs = proper_abbrevs tl !abbreviations in
          begin match find_repr p !abbrevs with
            Some ty when not (eq_type ty t) ->
              Tlink ty
          | _ ->
          (*
             One must allocate a new reference, so that abbrevia-
             tions belonging to different branches of a type are
             independent.
             Moreover, a reference containing a [Mcons] must be
             shared, so that the memorized expansion of an abbrevi-
             ation can be released by changing the content of just
             one reference.
          *)
              Tconstr (p, List.map copy tl,
                       ref (match !(!abbreviations) with
                              Mcons _ -> Mlink !abbreviations
                            | abbrev  -> abbrev))
          end
      | Tvariant row ->
          let more = row_more row in
          let mored = get_desc more in
          (* We must substitute in a subtle way *)
          (* Tsubst takes a tuple containing the row var and the variant *)
          begin match mored with
            Tsubst (_, Some ty2) ->
              (* This variant type has been already copied *)
              (* Change the stub to avoid Tlink in the new type *)
              For_copy.redirect_desc copy_scope ty (Tsubst (ty2, None));
              Tlink ty2
          | _ ->
              (* If the row variable is not generic, we must keep it *)
              let keep = get_level more <> generic_level && partial = None in
              let more' =
                match mored with
                  Tsubst (ty, None) -> ty
                  (* TODO: is this case possible?
                     possibly an interaction with (copy more) below? *)
                | Tconstr _ | Tnil ->
                    copy more
                | Tvar _ | Tunivar _ ->
                    if keep then more else newty mored
                |  _ -> assert false
              in
              let row =
                match get_desc more' with (* PR#6163 *)
                  Tconstr (x,_,_) when not (is_fixed row) ->
                    let Row {fields; more; closed; name} = row_repr row in
                    create_row ~fields ~more ~closed ~name
                      ~fixed:(Some (Reified x))
                | _ -> row
              in
              (* Open row if partial for pattern and contains Reither *)
              let more', row =
                match partial with
                  Some (free_univars, false) ->
                    let not_reither (_, f) =
                      match row_field_repr f with
                        Reither _ -> false
                      | _ -> true
                    in
                    let fields = row_fields row in
                    if row_closed row && not (is_fixed row)
                    && TypeSet.is_empty (free_univars ty)
                    && not (List.for_all not_reither fields) then
                      let more' = newvar (Jkind.value ~why:Row_variable) in
                      (more',
                       create_row ~fields:(List.filter not_reither fields)
                         ~more:more' ~closed:false ~fixed:None ~name:None)
                    else (more', row)
                | _ -> (more', row)
              in
              (* Register new type first for recursion *)
              For_copy.redirect_desc copy_scope more
                (Tsubst(more', Some t));
              (* Return a new copy *)
              Tvariant (copy_row copy true row keep more')
          end
      | Tobject (ty1, _) when partial <> None ->
          Tobject (copy ty1, ref None)
      | _ -> copy_type_desc ?keep_names copy desc
    in
    Transient_expr.set_stub_desc t desc';
    t

(**** Variants of instantiations ****)

let instance ?partial sch =
  let partial =
    match partial with
      None -> None
    | Some keep -> Some (compute_univars sch, keep)
  in
  For_copy.with_scope (fun copy_scope ->
    copy ?partial copy_scope sch)

let generic_instance sch =
  let old = !current_level in
  current_level := generic_level;
  let ty = instance sch in
  current_level := old;
  ty

let instance_list schl =
  For_copy.with_scope (fun copy_scope ->
    List.map (fun t -> copy copy_scope t) schl)

(* Create unique names to new type constructors.
   Used for existential types and local constraints. *)
let get_new_abstract_name env s =
  (* unique names are needed only for error messages *)
  if in_counterexample () then s else
  let name index =
    if index = 0 && s <> "" && s.[String.length s - 1] <> '$' then s else
    Printf.sprintf "%s%d" s index
  in
  let check index =
    match Env.find_type_by_name (Longident.Lident (name index)) env with
    | _ -> false
    | exception Not_found -> true
  in
  let index = Misc.find_first_mono check in
  name index

let new_local_type ?(loc = Location.none) ?manifest_and_scope jkind ~jkind_annot =
  let manifest, expansion_scope =
    match manifest_and_scope with
      None -> None, Btype.lowest_level
    | Some (ty, scope) -> Some ty, scope
  in
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract Abstract_def;
    type_jkind = jkind;
    type_jkind_annotation = jkind_annot;
    type_private = Public;
    type_manifest = manifest;
    type_variance = [];
    type_separability = [];
    type_is_newtype = true;
    type_expansion_scope = expansion_scope;
    type_loc = loc;
    type_attributes = [];
    type_unboxed_default = false;
    type_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
  }

let existential_name cstr ty =
  match get_desc ty with
  | Tvar { name = Some name } -> "$" ^ cstr.cstr_name ^ "_'" ^ name
  | _ -> "$" ^ cstr.cstr_name

type existential_treatment =
  | Keep_existentials_flexible
  | Make_existentials_abstract of { env: Env.t ref; scope: int }

let instance_constructor existential_treatment cstr =
  For_copy.with_scope (fun copy_scope ->
    let copy_existential =
      match existential_treatment with
      | Keep_existentials_flexible -> copy copy_scope
      | Make_existentials_abstract {env; scope = fresh_constr_scope} ->
          fun existential ->
            (* CR layouts v1.5: Add test case that hits this once we have syntax
               for it *)
            let jkind =
              match get_desc existential with
              | Tvar { jkind } -> jkind
              | Tvariant _ -> Jkind.value ~why:Row_variable
                  (* Existential row variable *)
              | _ -> assert false
            in
            let decl = new_local_type jkind ~jkind_annot:None in
            let name = existential_name cstr existential in
            let (id, new_env) =
              Env.enter_type (get_new_abstract_name !env name) decl !env
                ~scope:fresh_constr_scope in
            env := new_env;
            let to_unify = newty (Tconstr (Path.Pident id,[],ref Mnil)) in
            let tv = copy copy_scope existential in
            assert (is_Tvar tv);
            link_type tv to_unify;
            tv
    in
    let ty_ex = List.map copy_existential cstr.cstr_existentials in
    let ty_res = copy copy_scope cstr.cstr_res in
    let ty_args =
      List.map (fun (ty, gf) -> copy copy_scope ty, gf) cstr.cstr_args
    in
    (ty_args, ty_res, ty_ex)
  )

let instance_parameterized_type ?keep_names sch_args sch =
  For_copy.with_scope (fun copy_scope ->
    let ty_args = List.map (fun t -> copy ?keep_names copy_scope t) sch_args in
    let ty = copy copy_scope sch in
    (ty_args, ty)
  )

(* [map_kind f kind] maps [f] over all the types in [kind]. [f] must preserve jkinds *)
let map_kind f = function
  | (Type_abstract _ | Type_open) as k -> k
  | Type_variant (cl, rep) ->
      Type_variant (
        List.map
          (fun c ->
             {c with
              cd_args = map_type_expr_cstr_args f c.cd_args;
              cd_res = Option.map f c.cd_res
             })
          cl, rep)
  | Type_record (fl, rr) ->
      Type_record (
        List.map
          (fun l ->
             {l with ld_type = f l.ld_type}
          ) fl, rr)


let instance_declaration decl =
  For_copy.with_scope (fun copy_scope ->
    {decl with type_params = List.map (copy copy_scope) decl.type_params;
     type_manifest = Option.map (copy copy_scope) decl.type_manifest;
     type_kind = map_kind (copy copy_scope) decl.type_kind;
    }
  )

let generic_instance_declaration decl =
  let old = !current_level in
  current_level := generic_level;
  let decl = instance_declaration decl in
  current_level := old;
  decl

let instance_class params cty =
  let rec copy_class_type copy_scope = function
    | Cty_constr (path, tyl, cty) ->
        let tyl' = List.map (copy copy_scope) tyl in
        let cty' = copy_class_type copy_scope cty in
        Cty_constr (path, tyl', cty')
    | Cty_signature sign ->
        Cty_signature
          {csig_self = copy copy_scope sign.csig_self;
           csig_self_row = copy copy_scope sign.csig_self_row;
           csig_vars =
             Vars.map
               (function (m, v, ty) -> (m, v, copy copy_scope ty))
               sign.csig_vars;
           csig_meths =
             Meths.map
               (function (p, v, ty) -> (p, v, copy copy_scope ty))
               sign.csig_meths}
    | Cty_arrow (l, ty, cty) ->
        Cty_arrow (l, copy copy_scope ty, copy_class_type copy_scope cty)
  in
  For_copy.with_scope (fun copy_scope ->
    let params' = List.map (copy copy_scope) params in
    let cty' = copy_class_type copy_scope cty in
    (params', cty')
  )

(**** Instantiation for types with free universal variables ****)

(* [copy_sep] is used to instantiate first-class polymorphic types.
   * It first makes a separate copy of the type as a graph, omitting nodes
     that have no free univars.
   * In this first pass, [visited] is used as a mapping for previously visited
     nodes, and must already contain all the free univars in [ty].
   * The remaining (univar-closed) parts of the type are then instantiated
     with [copy] using a common [copy_scope].
   The reason to work in two passes lies in recursive types such as:
     [let h (x : < m : 'a. < n : 'a; p : 'b > > as 'b) = x#m]
   The type of [x#m] should be:
     [ < n : 'c; p : < m : 'a. < n : 'a; p : 'b > > as 'b > ]
   I.e., the universal type variable ['a] is both instantiated as a fresh
   type variable ['c] when outside of its binder, and kept as universal
   when under its binder.
   Assumption: in the first call to [copy_sep], all the free univars should
   be bound by the same [Tpoly] node. This guarantees that they are only
   bound when under this [Tpoly] node, which has no free univars, and as
   such is not part of the separate copy. In turn, this allows the separate
   copy to keep the sharing of the original type without breaking its
   binding structure.
 *)
let copy_sep ~copy_scope ~fixed ~(visited : type_expr TypeHash.t) sch =
  let free = compute_univars sch in
  let delayed_copies = ref [] in
  let add_delayed_copy t ty =
    delayed_copies :=
      lazy (Transient_expr.set_stub_desc t (Tlink (copy copy_scope ty))) ::
      !delayed_copies
  in
  let rec copy_rec ~may_share (ty : type_expr) =
    let univars = free ty in
    if is_Tvar ty || may_share && TypeSet.is_empty univars then
      if get_level ty <> generic_level then ty else
      (* jkind not consulted during copy_sep, so Any is safe *)
      let t = newstub ~scope:(get_scope ty) (Jkind.any ~why:Dummy_jkind) in
      add_delayed_copy t ty;
      t
    else try
      TypeHash.find visited ty
    with Not_found -> begin
      let t = newstub ~scope:(get_scope ty) (Jkind.any ~why:Dummy_jkind) in
      TypeHash.add visited ty t;
      let desc' =
        match get_desc ty with
        | Tvariant row ->
            let more = row_more row in
            (* We shall really check the level on the row variable *)
            let keep = is_Tvar more && get_level more <> generic_level in
            (* In that case we should keep the original, but we still
               call copy to correct the levels *)
            if keep then
              (add_delayed_copy t ty;
               Tvar { name = None;
                      jkind = Jkind.value ~why:Polymorphic_variant })
            else
            let more' = copy_rec ~may_share:false more in
            let fixed' = fixed && (is_Tvar more || is_Tunivar more) in
            let row =
              copy_row (copy_rec ~may_share:true) fixed' row keep more' in
            Tvariant row
        | Tfield (p, k, ty1, ty2) ->
            (* the kind is kept shared, see Btype.copy_type_desc *)
            Tfield (p, field_kind_internal_repr k,
                    copy_rec ~may_share:true ty1,
                    copy_rec ~may_share:false ty2)
        | desc -> copy_type_desc (copy_rec ~may_share:true) desc
      in
      Transient_expr.set_stub_desc t desc';
      t
    end
  in
  let ty = copy_rec ~may_share:true sch in
  List.iter Lazy.force !delayed_copies;
  ty

let instance_poly' copy_scope ~keep_names fixed univars sch =
  (* In order to compute univars below, [sch] should not contain [Tsubst] *)
  let copy_var ty =
    match get_desc ty with
      Tunivar { name; jkind } ->
        if keep_names then newty (Tvar { name; jkind }) else newvar jkind
    | _ -> assert false
  in
  let vars = List.map copy_var univars in
  let visited = TypeHash.create 17 in
  List.iter2 (TypeHash.add visited) univars vars;
  let ty = copy_sep ~copy_scope ~fixed ~visited sch in
  vars, ty

let instance_poly ?(keep_names=false) fixed univars sch =
  For_copy.with_scope (fun copy_scope ->
    instance_poly' copy_scope ~keep_names fixed univars sch
  )

let instance_label fixed lbl =
  For_copy.with_scope (fun copy_scope ->
    let vars, ty_arg =
      match get_desc lbl.lbl_arg with
        Tpoly (ty, tl) ->
          instance_poly' copy_scope ~keep_names:false fixed tl ty
      | _ ->
          [], copy copy_scope lbl.lbl_arg
    in
    (* call [copy] after [instance_poly] to avoid introducing [Tsubst] *)
    let ty_res = copy copy_scope lbl.lbl_res in
    (vars, ty_arg, ty_res)
  )

let prim_mode mvar = function
  | Primitive.Prim_global, _ -> Locality.allow_right Locality.global
  | Primitive.Prim_local, _ -> Locality.allow_right Locality.local
  | Primitive.Prim_poly, _ ->
    match mvar with
    | Some mvar -> mvar
    | None -> assert false

(** Returns a new mode variable whose locality is the given locality, while
    all other axes are from the given [m]. This function is too specific to be
    put in [mode.ml] *)
let with_locality locality m =
  let m' = Alloc.newvar () in
  Locality.equate_exn (Alloc.locality m') locality;
  Alloc.submode_exn m' (Alloc.set_locality_max m);
  Alloc.submode_exn (Alloc.set_locality_min m) m';
  m'

let rec instance_prim_locals locals mvar macc finalret ty =
  match locals, get_desc ty with
  | l :: locals, Tarrow ((lbl,marg,mret),arg,ret,commu) ->
     let marg = with_locality  (prim_mode (Some mvar) l) marg in
     let macc =
       Alloc.join [
        Alloc.disallow_right mret;
        Alloc.close_over marg;
        Alloc.partial_apply macc
       ]
     in
     let mret =
       match locals with
       | [] -> with_locality finalret mret
       | _ :: _ ->
          let mret', _ = Alloc.newvar_above macc in (* curried arrow *)
          mret'
     in
     let ret = instance_prim_locals locals mvar macc finalret ret in
     newty2 ~level:(get_level ty) (Tarrow ((lbl,marg,mret),arg,ret, commu))
  | _ :: _, _ -> assert false
  | [], _ ->
     ty

(* This function makes a copy of [ty] if [desc] is marked [prim_is_layout_poly]
   AND at least one generic type variable with jkind [any] is present. The
   function returns [ty] unchanged otherwise.

   When making the copy, all generic type variables with jkind [any] will be
   modified to have a sort var jkind. The same sort var will be used for all
   such rewrites.

   The copy should also have the same level information as [ty].  This is done
   in three steps:
   1. Change [ty] directly within a copy scope to have the sort var in place of
      jkind [any].
   2. Call [generic_instance] on this modified [ty] to make the actual copy we
      return (non-generic & generic levels should be preserved).
   3. Exit the copy scope thus restoring [ty] to its original state.

   No non-generic type variables should be present in [ty] due to it being the
   type of an external declaration. However, the code is written without
   relaying this assumption. *)
let instance_prim_layout (desc : Primitive.description) ty =
  if not desc.prim_is_layout_poly
  then ty, None
  else
  let new_sort_and_jkind = ref None in
  let get_jkind () =
    match !new_sort_and_jkind with
    | Some (_, jkind) ->
      jkind
    | None ->
      let jkind, sort =
        Jkind.of_new_sort_var ~why:Layout_poly_in_external
      in
      new_sort_and_jkind := Some (sort, jkind);
      jkind
  in
  For_copy.with_scope (fun copy_scope ->
    let rec inner ty =
      let level = get_level ty in
      (* only change type vars on generic_level to avoid modifying ones captured
         from an outer scope *)
      if level = generic_level && try_mark_node ty then begin
        begin match get_desc ty with
        | Tvar ({ jkind; _ } as r) when Jkind.is_any jkind ->
          For_copy.redirect_desc copy_scope ty
            (Tvar {r with jkind = get_jkind ()})
        | Tunivar ({ jkind; _ } as r) when Jkind.is_any jkind ->
          For_copy.redirect_desc copy_scope ty
            (Tunivar {r with jkind = get_jkind ()})
        | _ -> ()
        end;
        iter_type_expr inner ty
      end
    in
    inner ty;
    unmark_type ty;
    match !new_sort_and_jkind with
    | Some (sort, _) ->
      (* We don't want to lower the type vars from generic_level due to usages
         in [includecore.ml]. This means an extra [instance] call is needed in
         [type_ident], but we only hit it if it's layout polymorphic. *)
      generic_instance ty, Some sort
    | None -> ty, None)


let instance_prim_mode (desc : Primitive.description) ty =
  let is_poly = function Primitive.Prim_poly, _ -> true | _ -> false in
  if is_poly desc.prim_native_repr_res ||
       List.exists is_poly desc.prim_native_repr_args then
    let mode = Locality.newvar () in
    let finalret = prim_mode (Some mode) desc.prim_native_repr_res in
    instance_prim_locals desc.prim_native_repr_args
      mode (Alloc.disallow_right Alloc.legacy) finalret ty,
    Some mode
  else
    ty, None

let instance_prim (desc : Primitive.description) ty =
  let ty, sort = instance_prim_layout desc ty in
  let ty, mode = instance_prim_mode desc ty in
  ty, mode, sort

(**** Instantiation with parameter substitution ****)

(* NB: since this is [unify_var], it raises [Unify], not [Unify_trace] *)
let unify_var' = (* Forward declaration *)
  ref (fun _env _ty1 _ty2 -> assert false)

let subst env level priv abbrev oty params args body =
  if List.length params <> List.length args then raise Cannot_subst;
  let old_level = !current_level in
  current_level := level;
  let body0 = newvar (Jkind.any ~why:Dummy_jkind) in          (* Stub *)
  let undo_abbrev =
    match oty with
    | None -> fun () -> () (* No abbreviation added *)
    | Some ty ->
        match get_desc ty with
          Tconstr (path, tl, _) ->
            let abbrev = proper_abbrevs tl abbrev in
            memorize_abbrev abbrev priv path ty body0;
            fun () -> forget_abbrev abbrev path
        | _ -> assert false
  in
  abbreviations := abbrev;
  let (params', body') = instance_parameterized_type params body in
  abbreviations := ref Mnil;
  let old_umode = !umode in
  umode := Subst;
  try
    !unify_var' env body0 body';
    List.iter2 (!unify_var' env) params' args;
    current_level := old_level;
    umode := old_umode;
    body'
  with Unify _ ->
    current_level := old_level;
    umode := old_umode;
    undo_abbrev ();
    raise Cannot_subst

(* CR layouts: Can we actually just always ignore jkinds in apply/subst?

   It seems like almost, but there may be cases where it would forget
   information.  We thought the below would be one such case, but it actually
   works even if we skip the check.  Work out why.

   (* Trying to hit a bad subst case *)
   type 'a t = string * 'a
   type 'b s = 'b * int constraint 'b = 'a t

   type ('a : immediate) u = Imm of 'a

   (* Trying to get unification to drop jkind information and get this thing to
      accept values *)
   let foo (x : 'a) =
     ignore (Imm x);
     ignore (fun (y : (string * 'a) s) : (_ * int) -> y);
     ()

   let _ = foo "hi"
*)
(*
   Default to generic level. Usually, only the shape of the type matters, not
   whether it is generic or not. [generic_level] might be somewhat slower, but
   it ensures invariants on types are enforced (decreasing levels), and we don't
   care about efficiency here.
*)
let apply ?(use_current_level = false) env params body args =
  let level = if use_current_level then !current_level else generic_level in
  try
    subst env level Public (ref Mnil) None params args body
  with
    Cannot_subst -> raise Cannot_apply

let () =
  Subst.ctype_apply_env_empty :=
    fun params body args ->
      skip_jkind_checks_in (fun () -> apply Env.empty params body args)

                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)

(*
   If the environment has changed, memorized expansions might not
   be correct anymore, and so we flush the cache. This is safe but
   quite pessimistic: it would be enough to flush the cache when a
   type or module definition is overridden in the environment.
*)
let previous_env = ref Env.empty
(*let string_of_kind = function Public -> "public" | Private -> "private"*)
let check_abbrev_env env =
  if env != !previous_env then begin
    (* prerr_endline "cleanup expansion cache"; *)
    cleanup_abbrev ();
    previous_env := env
  end


(* Expand an abbreviation. The expansion is memorized. *)
(*
   Assume the level is greater than the path binding time of the
   expanded abbreviation.
*)
(*
   An abbreviation expansion will fail in either of these cases:
   1. The type constructor does not correspond to a manifest type.
   2. The type constructor is defined in an external file, and this
      file is not in the path (missing -I options).
   3. The type constructor is not in the "local" environment. This can
      happens when a non-generic type variable has been instantiated
      afterwards to the not yet defined type constructor. (Actually,
      this cannot happen at the moment due to the strong constraints
      between type levels and constructor binding time.)
   4. The expansion requires the expansion of another abbreviation,
      and this other expansion fails.
*)
let expand_abbrev_gen kind find_type_expansion env ty =
  check_abbrev_env env;
  match get_desc ty with
    Tconstr (path, args, abbrev) ->
      let level = get_level ty in
      let scope = get_scope ty in
      let lookup_abbrev = proper_abbrevs args abbrev in
      begin match find_expans kind path !lookup_abbrev with
        Some ty' ->
          (* prerr_endline
            ("found a "^string_of_kind kind^" expansion for "^Path.name path);*)
          if level <> generic_level then
            begin try
              update_level env level ty'
            with Escape _ ->
              (* XXX This should not happen.
                 However, levels are not correctly restored after a
                 typing error *)
              ()
            end;
          begin try
            update_scope scope ty';
          with Escape _ ->
            (* XXX This should not happen.
               However, levels are not correctly restored after a
               typing error *)
            ()
          end;
          ty'
      | None ->
          match find_type_expansion path env with
          | exception Not_found ->
            (* another way to expand is to normalize the path itself *)
            let path' = Env.normalize_type_path None env path in
            if Path.same path path' then raise Cannot_expand
            else newty2 ~level (Tconstr (path', args, abbrev))
          | (params, body, lv) ->
            (* prerr_endline
              ("add a "^string_of_kind kind^" expansion for "^Path.name path);*)
            let ty' =
              try
                subst env level kind abbrev (Some ty) params args body
              with Cannot_subst -> raise_escape_exn Constraint
            in
            (* For gadts, remember type as non exportable *)
            (* The ambiguous level registered for ty' should be the highest *)
            (* if !trace_gadt_instances then begin *)
            let scope = Misc.Stdlib.Int.max lv (get_scope ty) in
            update_scope scope ty;
            update_scope scope ty';
            ty'
      end
  | _ ->
      assert false

(* Expand respecting privacy *)
let expand_abbrev env ty =
  expand_abbrev_gen Public Env.find_type_expansion env ty

(* Expand once the head of a type *)
let expand_head_once env ty =
  try
    expand_abbrev env ty
  with Cannot_expand | Escape _ -> assert false

(* Check whether a type can be expanded *)
let safe_abbrev env ty =
  let snap = Btype.snapshot () in
  try ignore (expand_abbrev env ty); true with
    Cannot_expand ->
      Btype.backtrack snap;
      false
  | Escape _ ->
      Btype.backtrack snap;
      cleanup_abbrev ();
      false

(* Expand the head of a type once.
   Raise Cannot_expand if the type cannot be expanded.
   May raise Escape, if a recursion was hidden in the type. *)
let try_expand_once env ty =
  match get_desc ty with
    Tconstr _ -> expand_abbrev env ty
  | _ -> raise Cannot_expand

(* This one only raises Cannot_expand *)
let try_expand_safe env ty =
  let snap = Btype.snapshot () in
  try try_expand_once env ty
  with Escape _ ->
    Btype.backtrack snap; cleanup_abbrev (); raise Cannot_expand

(* Fully expand the head of a type. *)
let rec try_expand_head
    (try_once : Env.t -> type_expr -> type_expr) env ty =
  let ty' = try_once env ty in
  try try_expand_head try_once env ty'
  with Cannot_expand -> ty'

(* Unsafe full expansion, may raise [Unify [Escape _]]. *)
let expand_head_unif env ty =
  try
    try_expand_head try_expand_once env ty
  with
  | Cannot_expand -> ty
  | Escape e -> raise_for Unify (Escape e)

(* Safe version of expand_head, never fails *)
let expand_head env ty =
  try try_expand_head try_expand_safe env ty
  with Cannot_expand -> ty

let _ = forward_try_expand_safe := try_expand_safe


(* Expand until we find a non-abstract type declaration,
   use try_expand_safe to avoid raising "Unify _" when
   called on recursive types
 *)

type typedecl_extraction_result =
  | Typedecl of Path.t * Path.t * type_declaration
  | Has_no_typedecl
  | May_have_typedecl

let rec extract_concrete_typedecl env ty =
  match get_desc ty with
    Tconstr (p, _, _) ->
      begin match Env.find_type p env with
      | exception Not_found -> May_have_typedecl
      | decl ->
          if not (type_kind_is_abstract decl) then Typedecl(p, p, decl)
          else begin
            match try_expand_safe env ty with
            | exception Cannot_expand -> May_have_typedecl
            | ty ->
                match extract_concrete_typedecl env ty with
                | Typedecl(_, p', decl) -> Typedecl(p, p', decl)
                | Has_no_typedecl -> Has_no_typedecl
                | May_have_typedecl -> May_have_typedecl
          end
      end
  | Tpoly(ty, _) -> extract_concrete_typedecl env ty
  | Tarrow _ | Ttuple _ | Tobject _ | Tfield _ | Tnil
  | Tvariant _ | Tpackage _ -> Has_no_typedecl
  | Tvar _ | Tunivar _ -> May_have_typedecl
  | Tlink _ | Tsubst _ -> assert false

(* Implementing function [expand_head_opt], the compiler's own version of
   [expand_head] used for type-based optimisations.
   [expand_head_opt] uses [Env.find_type_expansion_opt] to access the
   manifest type information of private abstract data types which is
   normally hidden to the type-checker out of the implementation module of
   the private abbreviation. *)

let expand_abbrev_opt env ty =
  expand_abbrev_gen Private Env.find_type_expansion_opt env ty

let safe_abbrev_opt env ty =
  let snap = Btype.snapshot () in
  try ignore (expand_abbrev_opt env ty); true
  with Cannot_expand | Escape _ ->
    Btype.backtrack snap;
    false

let try_expand_once_opt env ty =
  match get_desc ty with
    Tconstr _ -> expand_abbrev_opt env ty
  | _ -> raise Cannot_expand

let try_expand_safe_opt env ty =
  let snap = Btype.snapshot () in
  try try_expand_once_opt env ty
  with Escape _ ->
    Btype.backtrack snap; raise Cannot_expand

let expand_head_opt env ty =
  try try_expand_head try_expand_safe_opt env ty with Cannot_expand -> ty


type unbox_result =
  (* unboxing process made a step: either an unboxing or removal of a [Tpoly] *)
  | Stepped of type_expr
  (* no step to make; we're all done here *)
  | Final_result of type_expr
  (* definition not in environment: missing cmi *)
  | Missing of Path.t

(* We use expand_head_opt version of expand_head to get access
   to the manifest type of private abbreviations. *)
let unbox_once env ty =
  let ty = expand_head_opt env ty in
  match get_desc ty with
  | Tconstr (p, args, _) ->
    begin match Env.find_type p env with
    | exception Not_found -> Missing p
    | decl ->
      begin match find_unboxed_type decl with
      | None -> Final_result ty
      | Some ty2 ->
        let ty2 = match get_desc ty2 with Tpoly (t, _) -> t | _ -> ty2 in
        Stepped (apply env decl.type_params ty2 args)
      end
    end
  | Tpoly (ty, _) -> Stepped ty
  | _ -> Final_result ty

(* We use ty_prev to track the last type for which we found a definition,
   allowing us to return a type for which a definition was found even if
   we eventually bottom out at a missing cmi file, or otherwise. *)
let rec get_unboxed_type_representation env ty_prev ty fuel =
  if fuel < 0 then Error ty else
    match unbox_once env ty with
    | Stepped ty2 ->
      get_unboxed_type_representation env ty ty2 (fuel - 1)
    | Final_result ty2 -> Ok ty2
    | Missing _ -> Ok ty_prev

let get_unboxed_type_representation env ty =
  (* Do not give too much fuel: PR#7424 *)
  get_unboxed_type_representation env ty ty 100

let get_unboxed_type_approximation env ty =
  match get_unboxed_type_representation env ty with
  | Ok ty | Error ty -> ty

(* When computing a jkind, we distinguish two cases because some callers might
   want to update the jkind.
   - Jkind: We compute the jkind, and the type wasn't a variable.
   - Var: The type was a var, we return the jkind and the type_expr it was in,
     in case the caller wants to update it. *)
type jkind_result =
  | Jkind of Jkind.t
  | TyVar of Jkind.t * type_expr

let jkind_of_result = function
  | Jkind l -> l
  | TyVar (l,_) -> l

let tvariant_not_immediate row =
  (* if all labels are devoid of arguments, not a pointer *)
  (* CR layouts v5: Polymorphic variants with all void args can probably
     be immediate, but we don't allow them to have void args right now. *)
  not (row_closed row)
  || List.exists
    (fun (_,field) -> match row_field_repr field with
      | Rpresent (Some _) | Reither (false, _, _) -> true
      | _ -> false)
    (row_fields row)

(* We assume here that [get_unboxed_type_representation] has already been
   called, if the type is a Tconstr.  This allows for some optimization by
   callers (e.g., skip expanding if the kind tells them enough).

   Note that this really returns an upper bound, and in particular returns [Any]
   in some edge cases (when [get_unboxed_type_representation] ran out of fuel,
   or when the type is a Tconstr that is missing from the Env due to a missing
   cmi). *)
let rec estimate_type_jkind env ty =
  let open Jkind in
  match get_desc ty with
  | Tconstr(p, _, _) -> begin
    try
      Jkind (Env.find_type p env).type_jkind
    with
      Not_found -> Jkind (any ~why:(Missing_cmi p))
  end
  | Tvariant row ->
      if tvariant_not_immediate row
      then Jkind (value ~why:Polymorphic_variant)
      else Jkind (immediate ~why:Immediate_polymorphic_variant)
  | Tvar { jkind } when get_level ty = generic_level ->
    (* Once a Tvar gets generalized with a jkind, it should be considered
       as fixed (similar to the Tunivar case below).

       This notably prevents [constrain_type_jkind] from changing layout
       [any] to a sort or changing the externality once the Tvar gets
       generalized.

       This, however, still allows sort variables to get instantiated. *)
    Jkind jkind
  | Tvar { jkind } -> TyVar (jkind, ty)
  | Tarrow _ -> Jkind (value ~why:Arrow)
  | Ttuple _ -> Jkind (value ~why:Tuple)
  | Tobject _ -> Jkind (value ~why:Object)
  | Tfield _ -> Jkind (value ~why:Tfield)
  | Tnil -> Jkind (value ~why:Tnil)
  | (Tlink _ | Tsubst _) -> assert false
  | Tunivar { jkind } -> Jkind jkind
  | Tpoly (ty, _) -> estimate_type_jkind env ty
  | Tpackage _ -> Jkind (value ~why:First_class_module)

(**** checking jkind relationships ****)

type type_jkind_sub_result =
  | Success
    (* The [Type_var] case might still be "success"; caller should check.
       We don't just report success here because if the caller unifies the
       tyvar, error messages improve. *)
  | Type_var of Jkind.t * type_expr
  | Missing_cmi of Jkind.t * Path.t
  | Failure of Jkind.t

let type_jkind_sub env ty ~check_sub =
  let shallow_check ty =
    match estimate_type_jkind env ty with
    | Jkind ty_jkind -> if check_sub ty_jkind then Success else Failure ty_jkind
    | TyVar (ty_jkind, ty) -> Type_var (ty_jkind, ty)
  in
  (* The "fuel" argument here is used because we're duplicating the loop of
     `get_unboxed_type_representation`, but performing jkind checking at each
     step.  This allows to check examples like:

       type 'a t = 'a list
       type s = { lbl : s t } [@@unboxed]

     Here, we want to see [s t] has jkind value, and this only requires
     expanding once to see [t] is list and [s] is irrelevant.  But calling
     [get_unboxed_type_representation] itself would otherwise get into a nasty
     loop trying to also expand [s], and then performing jkind checking to
     ensure it's a valid argument to [t].  (We believe there are still loops
     like this that can occur, though, and may need a more principled solution
     later).  *)
  let rec loop ty fuel =
    (* This is an optimization to avoid unboxing if we can tell the constraint
       is satisfied from the type_kind *)
    match get_desc ty with
    | Tconstr(p, _args, _abbrev) ->
        let jkind_bound =
          try (Env.find_type p env).type_jkind
          with Not_found -> Jkind.any ~why:(Missing_cmi p)
        in
        if check_sub jkind_bound
        then Success
        else if fuel < 0 then Failure jkind_bound
        else begin match unbox_once env ty with
          | Final_result ty -> shallow_check ty
          | Stepped ty -> loop ty (fuel - 1)
          | Missing missing_cmi_for ->
            Missing_cmi (jkind_bound, missing_cmi_for)
        end
    | Tpoly (ty, _) -> loop ty fuel
    | _ -> shallow_check ty
  in
  loop ty 100

(* The ~fixed argument controls what effects this may have on `ty`.  If false,
   then we will update the jkind of type variables to make the check true, if
   possible.  If true, we won't (but will still instantiate sort variables).

   Precondition: [jkind] is not [any]. This common case is short-circuited
   before calling this function. (Though the current implementation is still
   correct on [any].)
*)
let constrain_type_jkind ~fixed env ty jkind =
  match type_jkind_sub env ty
          ~check_sub:(fun ty_jkind -> Jkind.sub ty_jkind jkind) with
  | Success -> Ok ()
  | Type_var (ty_jkind, ty) ->
    if fixed then Jkind.sub_or_error ty_jkind jkind else
    let jkind_inter =
      Jkind.intersection ~reason:Tyvar_refinement_intersection
        ty_jkind jkind
    in
    Result.map (set_var_jkind ty) jkind_inter
  | Missing_cmi (ty_jkind, missing_cmi) ->
    Error Jkind.(Violation.of_ ~missing_cmi
      (Not_a_subjkind
         (update_reason ty_jkind (Missing_cmi missing_cmi), jkind)))
  | Failure ty_jkind ->
    Error (Jkind.Violation.of_ (Not_a_subjkind (ty_jkind, jkind)))

let constrain_type_jkind ~fixed env ty jkind =
  (* An optimization to avoid doing any work if we're checking against
     any. *)
  if Jkind.is_any jkind then Ok ()
  else constrain_type_jkind ~fixed env ty jkind

let check_type_jkind env ty jkind =
  constrain_type_jkind ~fixed:true env ty jkind

let constrain_type_jkind env ty jkind =
  constrain_type_jkind ~fixed:false env ty jkind

let () =
  Env.constrain_type_jkind := constrain_type_jkind

let check_type_externality env ty ext =
  let check_sub ty_jkind =
    Jkind.(Externality.le (get_externality_upper_bound ty_jkind) ext)
  in
  match type_jkind_sub env ty ~check_sub with
  | Success -> true
  | Type_var (ty_jkind, _) -> check_sub ty_jkind
  | Missing_cmi _ -> false (* safe answer *)
  | Failure _ -> false

let check_decl_jkind env decl jkind =
  match Jkind.sub_or_error decl.type_jkind jkind with
  | Ok () as ok -> ok
  | Error _ as err ->
      match decl.type_manifest with
      | None -> err
      | Some ty -> check_type_jkind env ty jkind

let constrain_decl_jkind env decl jkind =
  match Jkind.sub_or_error decl.type_jkind jkind with
  | Ok () as ok -> ok
  | Error _ as err ->
      match decl.type_manifest with
      | None -> err
      | Some ty -> constrain_type_jkind env ty jkind

let check_type_jkind_exn env texn ty jkind =
  match check_type_jkind env ty jkind with
  | Ok _ -> ()
  | Error err -> raise_for texn (Bad_jkind (ty,err))

let constrain_type_jkind_exn env texn ty jkind =
  match constrain_type_jkind env ty jkind with
  | Ok _ -> ()
  | Error err -> raise_for texn (Bad_jkind (ty,err))

let estimate_type_jkind env typ =
  jkind_of_result (estimate_type_jkind env typ)

let type_jkind env ty =
  estimate_type_jkind env (get_unboxed_type_approximation env ty)

let type_jkind_purely env ty =
  if !Clflags.principal || Env.has_local_constraints env then
    (* We snapshot to keep this pure; see the test in [typing-local/crossing.ml]
       that mentions snapshotting for an example. *)
    let snap = Btype.snapshot () in
    let jkind = type_jkind env ty in
    Btype.backtrack snap;
    jkind
  else
    type_jkind env ty

let type_sort ~why env ty =
  let jkind, sort = Jkind.of_new_sort_var ~why in
  match constrain_type_jkind env ty jkind with
  | Ok _ -> Ok sort
  | Error _ as e -> e

(* Note: Because [estimate_type_jkind] actually returns an upper bound, this
   function computes an inaccurate intersection in some cases.

   This is OK because of where it is used, which is related to gadt equations.
   The question we're trying to answer there is not really "is this
   intersection inhabited" but rather "do we know for sure that this
   intersection is uninhabited".  It's fine to call the intersection non-empty
   in some cases where its not (this will happen when pattern matching on a
   "false" GADT pattern), but not to say the intersection is empty if it isn't.
*)
let rec intersect_type_jkind ~reason env ty1 jkind2 =
  match get_desc ty1 with
  | Tpoly (ty, _) -> intersect_type_jkind ~reason env ty jkind2
  | _ ->
    (* [intersect_type_jkind] is called rarely, so we don't bother with trying
       to avoid this call as in [constrain_type_jkind] *)
    let ty1 = get_unboxed_type_approximation env ty1 in
    Jkind.intersection ~reason (estimate_type_jkind env ty1) jkind2

(* See comment on [jkind_unification_mode] *)
let unification_jkind_check env ty jkind =
  match !lmode with
  | Perform_checks -> constrain_type_jkind_exn env Unify ty jkind
  | Delay_checks r -> r := (ty,jkind) :: !r
  | Skip_checks -> ()

exception Incompatible_with_erasability_requirements of
  Ident.t option * Location.t

let () =
  Location.register_error_of_exn (function
  | Incompatible_with_erasability_requirements (id, loc) ->
    let format_id ppf = function
      | Some id -> Format.fprintf ppf " in %s" (Ident.name id)
      | None -> ()
    in
    Some (Location.errorf ~loc
      "@[Usage of layout immediate/immediate64%a can't be erased.@;\
      This error is produced due to the use of -only-erasable-extensions.@]"
      format_id id)
  | _ -> None)

let check_and_update_generalized_ty_jkind ?name ~loc ty =
  let immediacy_check jkind =
    let is_immediate jkind =
      (* Just check externality and layout, because that's what actually matters
         for upstream code. We check both for a known value and something that
         might turn out later to be value. This is the conservative choice. *)
      Jkind.(Externality.le (get_externality_upper_bound jkind) External64 &&
             match get_layout jkind with
               | Some (Sort Value) | None -> true
               | _ -> false)
    in
    if Language_extension.erasable_extensions_only ()
      && is_immediate jkind
    then
      raise (Incompatible_with_erasability_requirements (name, loc))
    else ()
  in
  let rec inner ty =
    let level = get_level ty in
    if level = generic_level && try_mark_node ty then begin
      begin match get_desc ty with
      | Tvar ({ jkind; _ } as r) ->
        immediacy_check jkind;
        let new_jkind =
          Jkind.(update_reason jkind (Generalized (name, loc)))
        in
        set_type_desc ty (Tvar {r with jkind = new_jkind})
      | Tunivar ({ jkind; _ } as r) ->
        immediacy_check jkind;
        let new_jkind =
          Jkind.(update_reason jkind (Generalized (name, loc)))
        in
        set_type_desc ty (Tunivar {r with jkind = new_jkind})
      | _ -> ()
      end;
      iter_type_expr inner ty
    end
  in
  inner ty;
  unmark_type ty

let is_principal ty =
  not !Clflags.principal || get_level ty = generic_level

(* Recursively expand the head of a type.
   Also expand #-types.

   Error printing relies on [full_expand] returning exactly its input (i.e., a
   physically equal type) when nothing changes. *)
let full_expand ~may_forget_scope env ty =
  let ty =
    if may_forget_scope then
      try expand_head_unif env ty with Unify_trace _ ->
        (* #10277: forget scopes when printing trace *)
        with_level ~level:(get_level ty) begin fun () ->
          (* The same as [expand_head], except in the failing case we return the
           *original* type, not [correct_levels ty].*)
          try try_expand_head try_expand_safe env (correct_levels ty) with
          | Cannot_expand -> ty
        end
    else expand_head env ty
  in
  match get_desc ty with
    Tobject (fi, {contents = Some (_, v::_)}) when is_Tvar v ->
      newty2 ~level:(get_level ty) (Tobject (fi, ref None))
  | _ ->
      ty

(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let (_, body, _) = Env.find_type_expansion path env in
    get_level body = generic_level
  with
    Not_found ->
      false

let generic_private_abbrev env path =
  try
    match Env.find_type path env with
      {type_kind = Type_abstract _;
       type_private = Private;
       type_manifest = Some body} ->
         get_level body = generic_level
    | _ -> false
  with Not_found -> false

let is_contractive env p =
  try
    let decl = Env.find_type p env in
    in_pervasives p && decl.type_manifest = None || is_datatype decl
  with Not_found -> false


                              (*****************)
                              (*  Occur check  *)
                              (*****************)


exception Occur

let rec occur_rec env allow_recursive visited ty0 ty =
  if eq_type ty ty0 then raise Occur;
  match get_desc ty with
    Tconstr(p, _tl, _abbrev) ->
      if allow_recursive && is_contractive env p then () else
      begin try
        if TypeSet.mem ty visited then raise Occur;
        let visited = TypeSet.add ty visited in
        iter_type_expr (occur_rec env allow_recursive visited ty0) ty
      with Occur -> try
        let ty' = try_expand_head try_expand_once env ty in
        (* This call used to be inlined, but there seems no reason for it.
           Message was referring to change in rev. 1.58 of the CVS repo. *)
        occur_rec env allow_recursive visited ty0 ty'
      with Cannot_expand ->
        raise Occur
      end
  | Tobject _ | Tvariant _ ->
      ()
  | _ ->
      if allow_recursive ||  TypeSet.mem ty visited then () else begin
        let visited = TypeSet.add ty visited in
        iter_type_expr (occur_rec env allow_recursive visited ty0) ty
      end

let type_changed = ref false (* trace possible changes to the studied type *)

let merge r b = if b then r := true

let occur env ty0 ty =
  let allow_recursive = allow_recursive_equations () in
  let old = !type_changed in
  try
    while
      type_changed := false;
      if not (eq_type ty0 ty) then
        occur_rec env allow_recursive TypeSet.empty ty0 ty;
      !type_changed
    do () (* prerr_endline "changed" *) done;
    merge type_changed old
  with exn ->
    merge type_changed old;
    raise exn

let occur_for tr_exn env t1 t2 =
  try
    occur env t1 t2
  with Occur -> raise_for tr_exn (Rec_occur(t1, t2))

let occur_in env ty0 t =
  try occur env ty0 t; false with Occur -> true

(* Check that a local constraint is well-founded *)
(* PR#6405: not needed since we allow recursion and work on normalized types *)
(* PR#6992: we actually need it for contractiveness *)
(* This is a simplified version of occur, only for the rectypes case *)

let rec local_non_recursive_abbrev ~allow_rec strict visited env p ty =
  (*Format.eprintf "@[Check %s =@ %a@]@." (Path.name p) !Btype.print_raw ty;*)
  if not (List.memq (get_id ty) visited) then begin
    match get_desc ty with
      Tconstr(p', args, _abbrev) ->
        if Path.same p p' then raise Occur;
        if allow_rec && not strict && is_contractive env p' then () else
        let visited = get_id ty :: visited in
        begin try
          (* try expanding, since [p] could be hidden *)
          local_non_recursive_abbrev ~allow_rec strict visited env p
            (try_expand_head try_expand_safe_opt env ty)
        with Cannot_expand ->
          let params =
            try (Env.find_type p' env).type_params
            with Not_found -> args
          in
          List.iter2
            (fun tv ty ->
              let strict = strict || not (is_Tvar tv) in
              local_non_recursive_abbrev ~allow_rec strict visited env p ty)
            params args
        end
    | Tobject _ | Tvariant _ when not strict ->
        ()
    | _ ->
        if strict || not allow_rec then (* PR#7374 *)
          let visited = get_id ty :: visited in
          iter_type_expr
            (local_non_recursive_abbrev ~allow_rec true visited env p) ty
  end

let local_non_recursive_abbrev env p ty =
  let allow_rec = allow_recursive_equations () in
  try (* PR#7397: need to check trace_gadt_instances *)
    wrap_trace_gadt_instances env
      (local_non_recursive_abbrev ~allow_rec false [] env p) ty;
    true
  with Occur -> false


                   (*****************************)
                   (*  Polymorphic Unification  *)
                   (*****************************)

(* Since we cannot duplicate universal variables, unification must
   be done at meta-level, using bindings in univar_pairs *)
(* TODO: use find_opt *)
(* Invariant: [jkind1] and [jkind2] (newly added) have to be the
   jkinds of Tunivar [t1] and [t2] respectively.

   They carry redundant information but are added to save two calls to
   [get_desc] which are usually performed already at the call site. *)
let unify_univar t1 t2 jkind1 jkind2 pairs =
  if not (Jkind.equal jkind1 jkind2) then raise Cannot_unify_universal_variables;
  let rec inner t1 t2 = function
    (cl1, cl2) :: rem ->
      let find_univ t cl =
        try
          let (_, r) = List.find (fun (t',_) -> eq_type t t') cl in
          Some r
        with Not_found -> None
      in
      begin match find_univ t1 cl1, find_univ t2 cl2 with
        Some {contents=Some t'2}, Some _ when eq_type t2 t'2 ->
          ()
      | Some({contents=None} as r1), Some({contents=None} as r2) ->
          set_univar r1 t2; set_univar r2 t1
      | None, None ->
        inner t1 t2 rem
      | _ ->
          raise Cannot_unify_universal_variables
      end
  | [] -> raise Cannot_unify_universal_variables
  in
  inner t1 t2 pairs

(* The same as [unify_univar], but raises the appropriate exception instead of
   [Cannot_unify_universal_variables] *)
let unify_univar_for tr_exn t1 t2 jkind1 jkind2 univar_pairs =
  try unify_univar t1 t2 jkind1 jkind2 univar_pairs
  with Cannot_unify_universal_variables -> raise_unexplained_for tr_exn

(* Test the occurrence of free univars in a type *)
(* That's way too expensive. Must do some kind of caching *)
(* If [inj_only=true], only check injective positions *)
let occur_univar ?(inj_only=false) env ty =
  let visited = ref TypeMap.empty in
  let rec occur_rec bound ty =
    if not_marked_node ty then
      if TypeSet.is_empty bound then
        (flip_mark_node ty; occur_desc bound ty)
      else try
        let bound' = TypeMap.find ty !visited in
        if not (TypeSet.subset bound' bound) then begin
          visited := TypeMap.add ty (TypeSet.inter bound bound') !visited;
          occur_desc bound ty
        end
      with Not_found ->
        visited := TypeMap.add ty bound !visited;
        occur_desc bound ty
  and occur_desc bound ty =
      match get_desc ty with
        Tunivar _ ->
          if not (TypeSet.mem ty bound) then
            raise_escape_exn (Univ ty)
      | Tpoly (ty, tyl) ->
          let bound = List.fold_right TypeSet.add tyl bound in
          occur_rec bound  ty
      | Tconstr (_, [], _) -> ()
      | Tconstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (fun t v ->
                (* The null variance only occurs in type abbreviations and
                   corresponds to type variables that do not occur in the
                   definition (expansion would erase them completely).
                   The type-checker consistently ignores type expressions
                   in this position. Physical expansion, as done in `occur`,
                   would be costly here, since we need to check inside
                   object and variant types too. *)
                if Variance.(if inj_only then mem Inj v else not (eq v null))
                then occur_rec bound t)
              tl td.type_variance
          with Not_found ->
            if not inj_only then List.iter (occur_rec bound) tl
          end
      | _ -> iter_type_expr (occur_rec bound) ty
  in
  Misc.try_finally (fun () ->
      occur_rec TypeSet.empty ty
    )
    ~always:(fun () -> unmark_type ty)

let has_free_univars env ty =
  try occur_univar ~inj_only:false env ty; false with Escape _ -> true
let has_injective_univars env ty =
  try occur_univar ~inj_only:true env ty; false with Escape _ -> true

let occur_univar_for tr_exn env ty =
  try
    occur_univar env ty
  with Escape e -> raise_for tr_exn (Escape e)

(* Grouping univars by families according to their binders *)
let add_univars =
  List.fold_left (fun s (t,_) -> TypeSet.add t s)

let get_univar_family univar_pairs univars =
  if univars = [] then TypeSet.empty else
  let insert s = function
      cl1, (_::_ as cl2) ->
        if List.exists (fun (t1,_) -> TypeSet.mem t1 s) cl1 then
          add_univars s cl2
        else s
    | _ -> s
  in
  let s = List.fold_right TypeSet.add univars TypeSet.empty in
  List.fold_left insert s univar_pairs

(* Whether a family of univars escapes from a type *)
let univars_escape env univar_pairs vl ty =
  let family = get_univar_family univar_pairs vl in
  let visited = ref TypeSet.empty in
  let rec occur t =
    if TypeSet.mem t !visited then () else begin
      visited := TypeSet.add t !visited;
      match get_desc t with
        Tpoly (t, tl) ->
          if List.exists (fun t -> TypeSet.mem t family) tl then ()
          else occur t
      | Tunivar _ -> if TypeSet.mem t family then raise_escape_exn (Univ t)
      | Tconstr (_, [], _) -> ()
      | Tconstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (* see occur_univar *)
              (fun t v -> if not Variance.(eq v null) then occur t)
              tl td.type_variance
          with Not_found ->
            List.iter occur tl
          end
      | _ ->
          iter_type_expr occur t
    end
  in
  occur ty

(* Wrapper checking that no variable escapes and updating univar_pairs *)
let enter_poly env univar_pairs t1 tl1 t2 tl2 f =
  let old_univars = !univar_pairs in
  let known_univars =
    List.fold_left (fun s (cl,_) -> add_univars s cl)
      TypeSet.empty old_univars
  in
  if List.exists (fun t -> TypeSet.mem t known_univars) tl1 then
     univars_escape env old_univars tl1 (newty(Tpoly(t2,tl2)));
  if List.exists (fun t -> TypeSet.mem t known_univars) tl2 then
    univars_escape env old_univars tl2 (newty(Tpoly(t1,tl1)));
  let cl1 = List.map (fun t -> t, ref None) tl1
  and cl2 = List.map (fun t -> t, ref None) tl2 in
  univar_pairs := (cl1,cl2) :: (cl2,cl1) :: old_univars;
  Misc.try_finally (fun () -> f t1 t2)
    ~always:(fun () -> univar_pairs := old_univars)

let enter_poly_for tr_exn env univar_pairs t1 tl1 t2 tl2 f =
  try
    enter_poly env univar_pairs t1 tl1 t2 tl2 f
  with Escape e -> raise_for tr_exn (Escape e)

let univar_pairs = ref []

(**** Instantiate a generic type into a poly type ***)

let polyfy env ty vars =
  let subst_univar copy_scope ty =
    match get_desc ty with
    | Tvar { name; jkind } when get_level ty = generic_level ->
        let t = newty (Tunivar { name; jkind }) in
        For_copy.redirect_desc copy_scope ty (Tsubst (t, None));
        Some t
    | _ -> None
  in
  (* need to expand twice? cf. Ctype.unify2 *)
  let vars = List.map (expand_head env) vars in
  let vars = List.map (expand_head env) vars in
  For_copy.with_scope (fun copy_scope ->
    let vars' = List.filter_map (subst_univar copy_scope) vars in
    let ty = copy copy_scope ty in
    let ty = newty2 ~level:(get_level ty) (Tpoly(ty, vars')) in
    let complete = List.length vars = List.length vars' in
    ty, complete
  )

(* assumption: [ty] is fully generalized. *)
let reify_univars env ty =
  let vars = free_variables ty in
  let ty, _ = polyfy env ty vars in
  ty

                              (*****************)
                              (*  Unification  *)
                              (*****************)



let rec has_cached_expansion p abbrev =
  match abbrev with
    Mnil                    -> false
  | Mcons(_, p', _, _, rem) -> Path.same p p' || has_cached_expansion p rem
  | Mlink rem               -> has_cached_expansion p !rem

(**** Transform error trace ****)
(* +++ Move it to some other place ? *)
(* That's hard to do because it relies on the expansion machinery in Ctype,
   but still might be nice. *)

let expand_type env ty =
  { ty       = ty;
    expanded = full_expand ~may_forget_scope:true env ty }

let expand_any_trace map env trace =
  map (expand_type env) trace

let expand_trace env trace =
  expand_any_trace Errortrace.map env trace

let expand_subtype_trace env trace =
  expand_any_trace Subtype.map env trace

let expand_to_unification_error env trace =
  unification_error ~trace:(expand_trace env trace)

let expand_to_equality_error env trace subst =
  equality_error ~trace:(expand_trace env trace) ~subst

let expand_to_moregen_error env trace =
  moregen_error ~trace:(expand_trace env trace)

(* [expand_trace] and the [expand_to_*_error] functions take care of most of the
   expansion in this file, but we occasionally need to build [Errortrace.error]s
   in other ways/elsewhere, so we expose some machinery for doing so
*)

(* Equivalent to [expand_trace env [Diff {got; expected}]] for a single
   element *)
let expanded_diff env ~got ~expected =
  Diff (map_diff (expand_type env) {got; expected})

(* Diff while transforming a [type_expr] into an [expanded_type] without
   expanding *)
let unexpanded_diff ~got ~expected =
  Diff (map_diff trivial_expansion {got; expected})

(**** Unification ****)

let rec deep_occur_rec t0 ty =
  if get_level ty >= get_level t0 && try_mark_node ty then begin
    if eq_type ty t0 then raise Occur;
    iter_type_expr (deep_occur_rec t0) ty
  end

(* Return whether [t0] occurs in any type in [tyl]. Objects are also traversed. *)
let deep_occur_list t0 tyl =
  try
    List.iter (deep_occur_rec t0) tyl;
    List.iter unmark_type tyl;
    false
  with Occur ->
    List.iter unmark_type tyl;
    true

let deep_occur t0 ty =
  try
    deep_occur_rec t0 ty;
    unmark_type ty;
    false
  with Occur ->
    unmark_type ty;
    true

let gadt_equations_level = ref None

let get_gadt_equations_level () =
  match !gadt_equations_level with
  | None -> assert false
  | Some x -> x

(* a local constraint can be added only if the rhs
   of the constraint does not contain any Tvars.
   They need to be removed using this function *)
let reify env t =
  let fresh_constr_scope = get_gadt_equations_level () in
  let create_fresh_constr lev name jkind =
    let name = match name with Some s -> "$'"^s | _ -> "$" in
    let decl = new_local_type jkind ~jkind_annot:None in
    let (id, new_env) =
      Env.enter_type (get_new_abstract_name !env name) decl !env
        ~scope:fresh_constr_scope in
    let path = Path.Pident id in
    let t = newty2 ~level:lev (Tconstr (path,[],ref Mnil))  in
    env := new_env;
    path, t
  in
  let visited = ref TypeSet.empty in
  let rec iterator ty =
    if TypeSet.mem ty !visited then () else begin
      visited := TypeSet.add ty !visited;
      match get_desc ty with
        Tvar { name; jkind } ->
          let level = get_level ty in
          let path, t = create_fresh_constr level name jkind in
          link_type ty t;
          if level < fresh_constr_scope then
            raise_for Unify (Escape (escape (Constructor path)))
      | Tvariant r ->
          if not (static_row r) then begin
            if is_fixed r then iterator (row_more r) else
            let m = row_more r in
            match get_desc m with
              Tvar { name; jkind }  ->
                let level = get_level m in
                let path, t = create_fresh_constr level name jkind in
                let row =
                  let fixed = Some (Reified path) in
                  create_row ~fields:[] ~more:t ~fixed
                    ~name:(row_name r) ~closed:(row_closed r) in
                link_type m (newty2 ~level (Tvariant row));
                if level < fresh_constr_scope then
                  raise_for Unify (Escape (escape (Constructor path)))
            | _ -> assert false
          end;
          iter_row iterator r
      | _ ->
          iter_type_expr iterator ty
    end
  in
  iterator t

let find_expansion_scope env path =
  match Env.find_type path env with
  | { type_manifest = None ; _ } | exception Not_found -> generic_level
  | decl -> decl.type_expansion_scope

let non_aliasable p decl =
  (* in_pervasives p ||  (subsumed by in_current_module) *)
  in_current_module p && not decl.type_is_newtype

(* This checks whether a gadt equation can be added for a type.

   The `for_jkind_eqn` parameter indicates whether this is an equation that
   just modifies a jkind, or a normal gadt equation (see add_jkind_equation).

   In the jkind case, we don't check that the type is not non_aliasable.  The
   non_aliasable check serves two purposes for gadt equations, neither of which
   is needed in the jkind case:

   1) It restricts gadt equations for abstract types defined in the current
   module.  This is actually unnecessary even for normal gadt equations and may
   change in the future (see upstream PR 8900).

   2) It restricts gadt equations on pervasives, which a hacky implementation of
   the fact that every type in the initial environment is incompatible with
   every other type.  But its fine to refine the jkind of these types (and we
   want to in some cases, like giving int32 the jkind immediate64 on
   appropriate platforms).

   CR layouts: the non_aliasable check could really be combined with the
   type_kind_is_abstract check, and both could be guarded by ~for_jkind_eqn, allowing
   the refinement of jkinds of data types.  Write some tests cases and make
   that change.
*)
let is_instantiable env ~for_jkind_eqn p =
  try
    let decl = Env.find_type p env in
    type_kind_is_abstract decl &&
    decl.type_private = Public &&
    decl.type_arity = 0 &&
    decl.type_manifest = None &&
    (for_jkind_eqn || not (non_aliasable p decl))
  with Not_found -> false


let compatible_paths p1 p2 =
  let open Predef in
  Path.same p1 p2 ||
  Path.same p1 path_bytes && Path.same p2 path_string ||
  Path.same p1 path_string && Path.same p2 path_bytes

(* Check for datatypes carefully; see PR#6348 *)
let rec expands_to_datatype env ty =
  match get_desc ty with
    Tconstr (p, _, _) ->
      begin try
        is_datatype (Env.find_type p env) ||
        expands_to_datatype env (try_expand_safe env ty)
      with Not_found | Cannot_expand -> false
      end
  | _ -> false

(* [mcomp] tests if two types are "compatible" -- i.e., if they could ever
   unify.  (This is distinct from [eqtype], which checks if two types *are*
   exactly the same.)  This is used to decide whether GADT cases are
   unreachable.  It is broadly part of unification. *)

(* mcomp type_pairs subst env t1 t2 does not raise an
   exception if it is possible that t1 and t2 are actually
   equal, assuming the types in type_pairs are equal and
   that the mapping subst holds.
   Assumes that both t1 and t2 do not contain any tvars
   and that both their objects and variants are closed
 *)

let rec mcomp type_pairs env t1 t2 =
  if eq_type t1 t2 then () else
  match (get_desc t1, get_desc t2) with
  | (Tvar _, _)
  | (_, Tvar _)  ->
      ()
    (* CR layouts: This could be made more precise based on jkinds *)
  | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
      ()
  | _ ->
      let t1' = expand_head_opt env t1 in
      let t2' = expand_head_opt env t2 in
      (* Expansion may have changed the representative of the types... *)
      if eq_type t1' t2' then () else
      if not (TypePairs.mem type_pairs (t1', t2')) then begin
        TypePairs.add type_pairs (t1', t2');
        match (get_desc t1', get_desc t2') with
        | (Tvar _, _)
        | (_, Tvar _)  ->
            ()
        | (Tarrow ((l1,_,_), t1, u1, _), Tarrow ((l2,_,_), t2, u2, _))
          when l1 = l2 || not (is_optional l1 || is_optional l2) ->
            mcomp type_pairs env t1 t2;
            mcomp type_pairs env u1 u2;
        | (Ttuple tl1, Ttuple tl2) ->
            mcomp_labeled_list type_pairs env tl1 tl2
        | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) ->
            mcomp_type_decl type_pairs env p1 p2 tl1 tl2
        | (Tconstr (_, [], _), _) when has_injective_univars env t2' ->
            raise_unexplained_for Unify
        | (_, Tconstr (_, [], _)) when has_injective_univars env t1' ->
            raise_unexplained_for Unify
        | (Tconstr (p, _, _), _) | (_, Tconstr (p, _, _)) ->
            begin try
              let decl = Env.find_type p env in
              if non_aliasable p decl || is_datatype decl then
                raise Incompatible
            with Not_found -> ()
            end
        (*
        | (Tpackage (p1, n1, tl1), Tpackage (p2, n2, tl2)) when n1 = n2 ->
            mcomp_list type_pairs env tl1 tl2
        *)
        | (Tpackage _, Tpackage _) -> ()
        | (Tvariant row1, Tvariant row2) ->
            mcomp_row type_pairs env row1 row2
        | (Tobject (fi1, _), Tobject (fi2, _)) ->
            mcomp_fields type_pairs env fi1 fi2
        | (Tfield _, Tfield _) ->       (* Actually unused *)
            mcomp_fields type_pairs env t1' t2'
        | (Tnil, Tnil) ->
            ()
        | (Tpoly (t1, []), Tpoly (t2, [])) ->
            mcomp type_pairs env t1 t2
        | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
            (try
               enter_poly env univar_pairs
                 t1 tl1 t2 tl2 (mcomp type_pairs env)
             with Escape _ -> raise Incompatible)
        | (Tunivar {jkind=jkind1}, Tunivar {jkind=jkind2}) ->
            (try unify_univar t1' t2' jkind1 jkind2 !univar_pairs
             with Cannot_unify_universal_variables -> raise Incompatible)
        | (_, _) ->
            raise Incompatible
      end

and mcomp_list type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise Incompatible;
  List.iter2 (mcomp type_pairs env) tl1 tl2

and mcomp_labeled_list type_pairs env labeled_tl1 labeled_tl2 =
  if not (Int.equal (List.length labeled_tl1) (List.length labeled_tl2)) then
    raise Incompatible;
  List.iter2
    (fun (label1, ty1) (label2, ty2) ->
      if not (Option.equal String.equal label1 label2) then
        raise Incompatible;
      mcomp type_pairs env ty1 ty2)
    labeled_tl1 labeled_tl2

and mcomp_fields type_pairs env ty1 ty2 =
  if not (concrete_object ty1 && concrete_object ty2) then assert false;
  let (fields2, rest2) = flatten_fields ty2 in
  let (fields1, rest1) = flatten_fields ty1 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let has_present =
    List.exists (fun (_, k, _) -> field_kind_repr k = Fpublic) in
  mcomp type_pairs env rest1 rest2;
  if has_present miss1  && get_desc (object_row ty2) = Tnil
  || has_present miss2  && get_desc (object_row ty1) = Tnil
  then raise Incompatible;
  List.iter
    (function (_n, k1, t1, k2, t2) ->
       mcomp_kind k1 k2;
       mcomp type_pairs env t1 t2)
    pairs

and mcomp_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fpublic, Fabsent)
  | (Fabsent, Fpublic) -> raise Incompatible
  | _                  -> ()

and mcomp_row type_pairs env row1 row2 =
  let r1, r2, pairs = merge_row_fields (row_fields row1) (row_fields row2) in
  let cannot_erase (_,f) =
    match row_field_repr f with
      Rpresent _ -> true
    | Rabsent | Reither _ -> false
  in
  if row_closed row1 && List.exists cannot_erase r2
  || row_closed row2 && List.exists cannot_erase r1 then raise Incompatible;
  List.iter
    (fun (_,f1,f2) ->
      match row_field_repr f1, row_field_repr f2 with
      | Rpresent None, (Rpresent (Some _) | Reither (_, _::_, _) | Rabsent)
      | Rpresent (Some _), (Rpresent None | Reither (true, _, _) | Rabsent)
      | (Reither (_, _::_, _) | Rabsent), Rpresent None
      | (Reither (true, _, _) | Rabsent), Rpresent (Some _) ->
          raise Incompatible
      | Rpresent(Some t1), Rpresent(Some t2) ->
          mcomp type_pairs env t1 t2
      | Rpresent(Some t1), Reither(false, tl2, _) ->
          List.iter (mcomp type_pairs env t1) tl2
      | Reither(false, tl1, _), Rpresent(Some t2) ->
          List.iter (mcomp type_pairs env t2) tl1
      | _ -> ())
    pairs

and mcomp_type_decl type_pairs env p1 p2 tl1 tl2 =
  try
    let decl = Env.find_type p1 env in
    let decl' = Env.find_type p2 env in
    if compatible_paths p1 p2 then begin
      let inj =
        try List.map Variance.(mem Inj) (Env.find_type p1 env).type_variance
        with Not_found -> List.map (fun _ -> false) tl1
      in
      List.iter2
        (fun i (t1,t2) -> if i then mcomp type_pairs env t1 t2)
        inj (List.combine tl1 tl2)
    end else if non_aliasable p1 decl && non_aliasable p2 decl' then
      raise Incompatible
    else
      match decl.type_kind, decl'.type_kind with
      | Type_record (lst,r), Type_record (lst',r')
        when equal_record_representation r r' ->
          mcomp_list type_pairs env tl1 tl2;
          mcomp_record_description type_pairs env lst lst'
      | Type_variant (v1,r), Type_variant (v2,r')
        when equal_variant_representation r r' ->
          mcomp_list type_pairs env tl1 tl2;
          mcomp_variant_description type_pairs env v1 v2
      | Type_open, Type_open ->
          mcomp_list type_pairs env tl1 tl2
      | Type_abstract _, Type_abstract _ -> ()
      | Type_abstract _, _ when not (non_aliasable p1 decl)-> ()
      | _, Type_abstract _ when not (non_aliasable p2 decl') -> ()
      | _ -> raise Incompatible
  with Not_found -> ()

and mcomp_type_option type_pairs env t t' =
  match t, t' with
    None, None -> ()
  | Some t, Some t' -> mcomp type_pairs env t t'
  | _ -> raise Incompatible

and mcomp_variant_description type_pairs env xs ys =
  let rec iter = fun x y ->
    match x, y with
    | c1 :: xs, c2 :: ys   ->
      mcomp_type_option type_pairs env c1.cd_res c2.cd_res;
      begin match c1.cd_args, c2.cd_args with
      | Cstr_tuple l1, Cstr_tuple l2 -> mcomp_tuple_description type_pairs env l1 l2
      | Cstr_record l1, Cstr_record l2 ->
          mcomp_record_description type_pairs env l1 l2
      | _ -> raise Incompatible
      end;
     if Ident.name c1.cd_id = Ident.name c2.cd_id
      then iter xs ys
      else raise Incompatible
    | [],[] -> ()
    | _ -> raise Incompatible
  in
  iter xs ys

and mcomp_tuple_description type_pairs env =
  let rec iter x y =
    match x, y with
    | (ty1, gf1) :: xs, (ty2, gf2) :: ys ->
      mcomp type_pairs env ty1 ty2;
      if gf1 = gf2
      then iter xs ys
      else raise Incompatible
    | [], [] -> ()
    | _ -> raise Incompatible
  in
  iter

and mcomp_record_description type_pairs env =
  let rec iter x y =
    match x, y with
    | l1 :: xs, l2 :: ys ->
        mcomp type_pairs env l1.ld_type l2.ld_type;
        if Ident.name l1.ld_id = Ident.name l2.ld_id &&
           l1.ld_mutable = l2.ld_mutable &&
           l1.ld_global = l2.ld_global
        then iter xs ys
        else raise Incompatible
    | [], [] -> ()
    | _ -> raise Incompatible
  in
  iter

let mcomp env t1 t2 =
  mcomp (TypePairs.create 4) env t1 t2

let mcomp_for tr_exn env t1 t2 =
  try
    mcomp env t1 t2
  with Incompatible -> raise_unexplained_for tr_exn

(* Real unification *)

let find_lowest_level ty =
  let lowest = ref generic_level in
  let rec find ty =
    if not_marked_node ty then begin
      let level = get_level ty in
      if level < !lowest then lowest := level;
      flip_mark_node ty;
      iter_type_expr find ty
    end
  in find ty; unmark_type ty; !lowest

let jkind_of_abstract_type_declaration env p =
  try
    (* CR layouts: This lookup duplicates work already done in is_instantiable,
       which guards the case of unify3 that reaches this function.  Would be
       nice to eliminate the duplication, but is seems tricky to do so without
       complicating unify3. *)
    let typ = Env.find_type p env in
    typ.type_jkind, typ.type_jkind_annotation
  with
    Not_found -> assert false

let add_jkind_equation ~reason env destination jkind1 =
  (* Here we check whether the source and destination jkinds intersect.  If
     they don't, we can give a type error.  If they do, and destination is
     abstract, we can improve type checking by assigning destination that
     jkind. *)
  match intersect_type_jkind ~reason !env destination jkind1 with
  | Error err -> raise_for Unify (Bad_jkind (destination,err))
  | Ok jkind -> begin
      match get_desc destination with
      | Tconstr (p, _, _) when is_instantiable ~for_jkind_eqn:true !env p ->
        begin
          try
            let decl = Env.find_type p !env in
            if not (Jkind.equal jkind decl.type_jkind)
            then
              let refined_decl = { decl with type_jkind = jkind } in
              env := Env.add_local_type p refined_decl !env
          with
            Not_found -> ()
        end
      | _ -> ()
    end

let add_gadt_equation env source destination =
  (* Format.eprintf "@[add_gadt_equation %s %a@]@."
    (Path.name source) !Btype.print_raw destination; *)
  if has_free_univars !env destination then
    occur_univar ~inj_only:true !env destination
  else if local_non_recursive_abbrev !env source destination then begin
    let destination = duplicate_type destination in
    let expansion_scope =
      Int.max (Path.scope source) (get_gadt_equations_level ())
    in
    (* Recording the actual jkind here is required, not just for efficiency.
       When we check the jkind later, we may not be able to see the local
       equation because of its scope. *)
    let jkind, jkind_annot =
      jkind_of_abstract_type_declaration !env source
    in
    add_jkind_equation ~reason:(Gadt_equation source) env destination jkind;
    let decl =
      new_local_type ~manifest_and_scope:(destination, expansion_scope) jkind
        ~jkind_annot
    in
    env := Env.add_local_type source decl !env;
    cleanup_abbrev ()
  end

let unify_eq_set = TypePairs.create 11

let order_type_pair t1 t2 =
  if get_id t1 <= get_id t2 then (t1, t2) else (t2, t1)

let add_type_equality t1 t2 =
  TypePairs.add unify_eq_set (order_type_pair t1 t2)

let eq_package_path env p1 p2 =
  Path.same p1 p2 ||
  Path.same (normalize_package_path env p1) (normalize_package_path env p2)

let nondep_type' = ref (fun _ _ _ -> assert false)
let package_subtype = ref (fun _ _ _ _ _ -> assert false)

exception Nondep_cannot_erase of Ident.t

let rec concat_longident lid1 =
  let open Longident in
  function
    Lident s -> Ldot (lid1, s)
  | Ldot (lid2, s) -> Ldot (concat_longident lid1 lid2, s)
  | Lapply (lid2, lid) -> Lapply (concat_longident lid1 lid2, lid)

let nondep_instance env level id ty =
  let ty = !nondep_type' env [id] ty in
  if level = generic_level then duplicate_type ty else
  let old = !current_level in
  current_level := level;
  let ty = instance ty in
  current_level := old;
  ty

(* Find the type paths nl1 in the module type mty2, and add them to the
   list (nl2, tl2). raise Not_found if impossible *)
let complete_type_list ?(allow_absent=false) env fl1 lv2 mty2 fl2 =
  (* This is morally WRONG: we're adding a (dummy) module without a scope in the
     environment. However no operation which cares about levels/scopes is going
     to happen while this module exists.
     The only operations that happen are:
     - Env.find_type_by_name
     - nondep_instance
     None of which check the scope.

     It'd be nice if we avoided creating such temporary dummy modules and broken
     environments though. *)
  let id2 = Ident.create_local "Pkg" in
  let env' = Env.add_module id2 Mp_present mty2 env in
  let rec complete fl1 fl2 =
    match fl1, fl2 with
      [], _ -> fl2
    | (n, _) :: nl, (n2, _ as nt2) :: ntl' when n >= n2 ->
        nt2 :: complete (if n = n2 then nl else fl1) ntl'
    | (n, _) :: nl, _ ->
        let lid = concat_longident (Longident.Lident "Pkg") n in
        match Env.find_type_by_name lid env' with
        | (_, {type_arity = 0; type_kind = Type_abstract _;
               type_private = Public; type_manifest = Some t2}) ->
            begin match nondep_instance env' lv2 id2 t2 with
            | t -> (n, t) :: complete nl fl2
            | exception Nondep_cannot_erase _ ->
                if allow_absent then
                  complete nl fl2
                else
                  raise Exit
            end
        | (_, {type_arity = 0; type_kind = Type_abstract _;
               type_private = Public; type_manifest = None})
          when allow_absent ->
            complete nl fl2
        | _ -> raise Exit
        | exception Not_found when allow_absent->
            complete nl fl2
  in
  match complete fl1 fl2 with
  | res -> res
  | exception Exit -> raise Not_found

(* raise Not_found rather than Unify if the module types are incompatible *)
let unify_package env unify_list lv1 p1 fl1 lv2 p2 fl2 =
  let ntl2 = complete_type_list env fl1 lv2 (Mty_ident p2) fl2
  and ntl1 = complete_type_list env fl2 lv1 (Mty_ident p1) fl1 in
  unify_list (List.map snd ntl1) (List.map snd ntl2);
  if eq_package_path env p1 p2
  || !package_subtype env p1 fl1 p2 fl2
  && !package_subtype env p2 fl2 p1 fl1 then () else raise Not_found

let unify_alloc_mode_for tr_exn a b =
  match Alloc.equate a b with
  | Ok () -> ()
  | Error _ -> raise_unexplained_for tr_exn

(* force unification in Reither when one side has a non-conjunctive type *)
let rigid_variants = ref false

let unify_eq t1 t2 =
  eq_type t1 t2
  || (in_pattern_mode ()
      && TypePairs.mem unify_eq_set (order_type_pair t1 t2))

let unify1_var env t1 t2 =
  let jkind = match get_desc t1 with
    | Tvar { jkind } -> jkind
    | _ -> assert false
  in
  occur_for Unify env t1 t2;
  match occur_univar_for Unify env t2 with
  | () ->
      begin
        try
          update_level env (get_level t1) t2;
          update_scope (get_scope t1) t2;
        with Escape e ->
          raise_for Unify (Escape e)
      end;
      unification_jkind_check env t2 jkind;
      link_type t1 t2;
      true
  | exception Unify_trace _ when in_pattern_mode () ->
      false

(* Called from unify3 *)
let unify3_var env jkind1 t1' t2 t2' =
  occur_for Unify !env t1' t2;
  match occur_univar_for Unify !env t2 with
  | () -> begin
      unification_jkind_check !env t2' jkind1;
      link_type t1' t2
    end
  | exception Unify_trace _ when in_pattern_mode () ->
      reify env t1';
      reify env t2';
      if can_generate_equations () then begin
        occur_univar ~inj_only:true !env t2';
        record_equation t1' t2';
      end

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem too high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)

let rec unify (env:Env.t ref) t1 t2 =
  (* First step: special cases (optimizations) *)
  if unify_eq t1 t2 then () else
  let reset_tracing = check_trace_gadt_instances !env in
  try
    type_changed := true;
    begin match (get_desc t1, get_desc t2) with
      (Tvar _, Tconstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (Tconstr _, Tvar _) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (Tvar _, _) ->
        if unify1_var !env t1 t2 then () else unify2 env t1 t2
    | (_, Tvar _) ->
        if unify1_var !env t2 t1 then () else unify2 env t1 t2
    | (Tunivar { jkind = k1 }, Tunivar { jkind = k2 }) ->
        unify_univar_for Unify t1 t2 k1 k2 !univar_pairs;
        update_level_for Unify !env (get_level t1) t2;
        update_scope_for Unify (get_scope t1) t2;
        link_type t1 t2
    | (Tconstr (p1, [], a1), Tconstr (p2, [], a2))
          when Path.same p1 p2 (* && actual_mode !env = Old *)
            (* This optimization assumes that t1 does not expand to t2
               (and conversely), so we fall back to the general case
               when any of the types has a cached expansion. *)
            && not (has_cached_expansion p1 !a1
                 || has_cached_expansion p2 !a2) ->
        update_level_for Unify !env (get_level t1) t2;
        update_scope_for Unify (get_scope t1) t2;
        link_type t1 t2
    | (Tconstr _, Tconstr _) when Env.has_local_constraints !env ->
        unify2_rec env t1 t1 t2 t2
    | _ ->
        unify2 env t1 t2
    end;
    reset_trace_gadt_instances reset_tracing;
  with Unify_trace trace ->
    reset_trace_gadt_instances reset_tracing;
    raise_trace_for Unify (Diff {got = t1; expected = t2} :: trace)

and unify2 env t1 t2 = unify2_expand env t1 t1 t2 t2

and unify2_rec env t10 t1 t20 t2 =
  if unify_eq t1 t2 then () else
  try match (get_desc t1, get_desc t2) with
  | (Tconstr (p1, tl1, a1), Tconstr (p2, tl2, a2)) ->
      if Path.same p1 p2 && tl1 = [] && tl2 = []
      && not (has_cached_expansion p1 !a1 || has_cached_expansion p2 !a2)
      then begin
        update_level_for Unify !env (get_level t1) t2;
        update_scope_for Unify (get_scope t1) t2;
        link_type t1 t2
      end else
        if find_expansion_scope !env p1 > find_expansion_scope !env p2
        then unify2_rec env t10 t1 t20 (try_expand_safe !env t2)
        else unify2_rec env t10 (try_expand_safe !env t1) t20 t2
  | _ ->
      raise Cannot_expand
  with Cannot_expand ->
    unify2_expand env t10 t1 t20 t2

and unify2_expand env t1 t1' t2 t2' =
  (* Second step: expansion of abbreviations *)
  (* Expansion may change the representative of the types. *)
  ignore (expand_head_unif !env t1');
  ignore (expand_head_unif !env t2');
  let t1' = expand_head_unif !env t1' in
  let t2' = expand_head_unif !env t2' in
  let lv = Int.min (get_level t1') (get_level t2') in
  let scope = Int.max (get_scope t1') (get_scope t2') in
  update_level_for Unify !env lv t2;
  update_level_for Unify !env lv t1;
  update_scope_for Unify scope t2;
  update_scope_for Unify scope t1;
  if unify_eq t1' t2' then () else

  let t1, t2 =
    if !Clflags.principal
    && (find_lowest_level t1' < lv || find_lowest_level t2' < lv) then
      (* Expand abbreviations hiding a lower level *)
      (* Should also do it for parameterized types, after unification... *)
      (match get_desc t1 with Tconstr (_, [], _) -> t1' | _ -> t1),
      (match get_desc t2 with Tconstr (_, [], _) -> t2' | _ -> t2)
    else (t1, t2)
  in
  if unify_eq t1 t1' || not (unify_eq t2 t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify_trace trace ->
      raise_trace_for Unify (swap_trace trace)

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let tt1' = Transient_expr.repr t1' in
  let d1 = tt1'.desc and d2 = get_desc t2' in
  let create_recursion =
    (not (eq_type t2 t2')) && (deep_occur t1'  t2) in

  begin match (d1, d2) with (* handle vars and univars specially *)
    (Tunivar { jkind = k1 }, Tunivar { jkind = k2 }) ->
      unify_univar_for Unify t1' t2' k1 k2 !univar_pairs;
      link_type t1' t2'
  | (Tvar { jkind }, _) ->
      unify3_var env jkind t1' t2 t2'
  | (_, Tvar { jkind }) ->
      unify3_var env jkind t2' t1 t1'
  | (Tfield _, Tfield _) -> (* special case for GADTs *)
      unify_fields env t1' t2'
  | _ ->
    if in_pattern_mode () then
      add_type_equality t1' t2'
    else begin
      occur_for Unify !env t1' t2;
      link_type t1' t2
    end;
    try
      begin match (d1, d2) with
        (Tarrow ((l1,a1,r1), t1, u1, c1),
         Tarrow ((l2,a2,r2), t2, u2, c2))
           when
             (l1 = l2 ||
              (!Clflags.classic || in_pattern_mode ()) &&
               not (is_optional l1 || is_optional l2)) ->
          unify_alloc_mode_for Unify a1 a2;
          unify_alloc_mode_for Unify r1 r2;
          unify  env t1 t2; unify env  u1 u2;
          begin match is_commu_ok c1, is_commu_ok c2 with
          | false, true -> set_commu_ok c1
          | true, false -> set_commu_ok c2
          | false, false -> link_commu ~inside:c1 c2
          | true, true -> ()
          end
      | (Ttuple labeled_tl1, Ttuple labeled_tl2) ->
          unify_labeled_list env labeled_tl1 labeled_tl2
      | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
          if not (can_generate_equations ()) then
            unify_list env tl1 tl2
          else if can_assume_injective () then
            without_assume_injective (fun () -> unify_list env tl1 tl2)
          else if in_current_module p1 (* || in_pervasives p1 *)
               || List.exists (expands_to_datatype !env) [t1'; t1; t2]
          then
            unify_list env tl1 tl2
          else
            let inj =
              try List.map Variance.(mem Inj)
                    (Env.find_type p1 !env).type_variance
              with Not_found -> List.map (fun _ -> false) tl1
            in
            List.iter2
              (fun i (t1, t2) ->
                if i then unify env t1 t2 else
                without_generating_equations
                  begin fun () ->
                    let snap = snapshot () in
                    try unify env t1 t2 with Unify_trace _ ->
                      backtrack snap;
                      reify env t1;
                      reify env t2
                  end)
              inj (List.combine tl1 tl2)
      | (Tconstr (path,[],_),
         Tconstr (path',[],_))
        when is_instantiable !env ~for_jkind_eqn:false path
          && is_instantiable !env ~for_jkind_eqn:false path'
          && can_generate_equations () ->
          let source, destination =
            if Path.scope path > Path.scope path'
            then  path , t2'
            else  path', t1'
          in
          record_equation t1' t2';
          add_gadt_equation env source destination
      | (Tconstr (path,[],_), _)
        when is_instantiable !env ~for_jkind_eqn:false path
          && can_generate_equations () ->
          reify env t2';
          record_equation t1' t2';
          add_gadt_equation env path t2'
      | (_, Tconstr (path,[],_))
        when is_instantiable !env ~for_jkind_eqn:false path
          && can_generate_equations () ->
          reify env t1';
          record_equation t1' t2';
          add_gadt_equation env path t1'
      | (Tconstr (_,_,_), _) | (_, Tconstr (_,_,_)) when in_pattern_mode () ->
          reify env t1';
          reify env t2';
          if can_generate_equations () then (
            mcomp_for Unify !env t1' t2';
            record_equation t1' t2'
          )
      | (Tobject (fi1, nm1), Tobject (fi2, _)) ->
          unify_fields env fi1 fi2;
          (* Type [t2'] may have been instantiated by [unify_fields] *)
          (* XXX One should do some kind of unification... *)
          begin match get_desc t2' with
            Tobject (_, {contents = Some (_, va::_)}) when
              (match get_desc va with
                Tvar _|Tunivar _|Tnil -> true | _ -> false) -> ()
          | Tobject (_, nm2) -> set_name nm2 !nm1
          | _ -> ()
          end
      | (Tvariant row1, Tvariant row2) ->
          if not (in_pattern_mode ()) then
            unify_row env row1 row2
          else begin
            let snap = snapshot () in
            try unify_row env row1 row2
            with Unify_trace _ ->
              backtrack snap;
              reify env t1';
              reify env t2';
              if can_generate_equations () then (
                mcomp_for Unify !env t1' t2';
                record_equation t1' t2'
              )
          end
      | (Tfield(f,kind,_,rem), Tnil) | (Tnil, Tfield(f,kind,_,rem)) ->
          begin match field_kind_repr kind with
            Fprivate when f <> dummy_method ->
              link_kind ~inside:kind field_absent;
              if d2 = Tnil then unify env rem t2'
              else unify env (newgenty Tnil) rem
          | _      ->
              if f = dummy_method then
                raise_for Unify (Obj Self_cannot_be_closed)
              else if d1 = Tnil then
                raise_for Unify (Obj (Missing_field(First, f)))
              else
                raise_for Unify (Obj (Missing_field(Second, f)))
          end
      | (Tnil, Tnil) ->
          ()
      | (Tpoly (t1, []), Tpoly (t2, [])) ->
          unify env t1 t2
      | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
          enter_poly_for Unify !env univar_pairs t1 tl1 t2 tl2 (unify env)
      | (Tpackage (p1, fl1), Tpackage (p2, fl2)) ->
          begin try
            unify_package !env (unify_list env)
              (get_level t1) p1 fl1 (get_level t2) p2 fl2
          with Not_found ->
            if not (in_pattern_mode ()) then raise_unexplained_for Unify;
            List.iter (fun (_n, ty) -> reify env ty) (fl1 @ fl2);
            (* if !generate_equations then List.iter2 (mcomp !env) tl1 tl2 *)
          end
      | (Tnil,  Tconstr _ ) ->
          raise_for Unify (Obj (Abstract_row Second))
      | (Tconstr _,  Tnil ) ->
          raise_for Unify (Obj (Abstract_row First))
      | (_, _) -> raise_unexplained_for Unify
      end;
      (* XXX Commentaires + changer "create_recursion"
         ||| Comments + change "create_recursion" *)
      if create_recursion then
        match get_desc t2 with
          Tconstr (p, tl, abbrev) ->
            forget_abbrev abbrev p;
            let t2'' = expand_head_unif !env t2 in
            if not (closed_parameterized_type tl t2'') then
              link_type t2 t2'
        | _ ->
            () (* t2 has already been expanded by update_level *)
    with Unify_trace trace ->
      Transient_expr.set_desc tt1' d1;
      raise_trace_for Unify trace
  end

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise_unexplained_for Unify;
  List.iter2 (unify env) tl1 tl2

and unify_labeled_list env labeled_tl1 labeled_tl2 =
  if not (Int.equal (List.length labeled_tl1) (List.length labeled_tl2)) then
    raise_unexplained_for Unify;
  List.iter2
    (fun (label1, ty1) (label2, ty2) ->
      if not (Option.equal String.equal label1 label2) then
        raise_unexplained_for Unify;
      unify env ty1 ty2)
    labeled_tl1 labeled_tl2

(* Build a fresh row variable for unification *)
and make_rowvar level use1 rest1 use2 rest2  =
  let set_name ty name =
    match get_desc ty with
      Tvar { name = None; jkind } -> set_type_desc ty (Tvar { name; jkind })
    | _ -> ()
  in
  let name =
    match get_desc rest1, get_desc rest2 with
      Tvar { name = Some _ as name1 }, Tvar { name = Some _ as name2 } ->
        if get_level rest1 <= get_level rest2 then name1 else name2
    | Tvar { name = Some _ as name }, _ ->
        if use2 then set_name rest2 name; name
    | _, Tvar { name = Some _ as name } ->
        if use1 then set_name rest2 name; name
    | _ -> None
  in
  if use1 then rest1 else
  if use2 then rest2
  else newty2 ~level (Tvar { name; jkind = Jkind.value ~why:Row_variable })

and unify_fields env ty1 ty2 =          (* Optimization *)
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let l1 = get_level ty1 and l2 = get_level ty2 in
  let va = make_rowvar (Int.min l1 l2) (miss2=[]) rest1 (miss1=[]) rest2 in
  let tr1 = Transient_expr.repr rest1 and tr2 = Transient_expr.repr rest2 in
  let d1 = tr1.desc and d2 = tr2.desc in
  try
    unify env (build_fields l1 miss1 va) rest2;
    unify env rest1 (build_fields l2 miss2 va);
    List.iter
      (fun (name, k1, t1, k2, t2) ->
        unify_kind k1 k2;
        try
          if !trace_gadt_instances && not (in_subst_mode ()) then begin
            (* in_subst_mode: see PR#11771 *)
            update_level_for Unify !env (get_level va) t1;
            update_scope_for Unify (get_scope va) t1
          end;
          unify env t1 t2
        with Unify_trace trace ->
          raise_trace_for Unify
            (incompatible_fields ~name ~got:t1 ~expected:t2 :: trace)
      )
      pairs
  with exn ->
    Transient_expr.set_desc tr1 d1;
    Transient_expr.set_desc tr2 d2;
    raise exn

and unify_kind k1 k2 =
  match field_kind_repr k1, field_kind_repr k2 with
    (Fprivate, (Fprivate | Fpublic)) -> link_kind ~inside:k1 k2
  | (Fpublic, Fprivate)              -> link_kind ~inside:k2 k1
  | (Fpublic, Fpublic)               -> ()
  | _                                -> assert false

and unify_row env row1 row2 =
  let Row {fields = row1_fields; more = rm1;
           closed = row1_closed; name = row1_name} = row_repr row1 in
  let Row {fields = row2_fields; more = rm2;
           closed = row2_closed; name = row2_name} = row_repr row2 in
  if unify_eq rm1 rm2 then () else
  let r1, r2, pairs = merge_row_fields row1_fields row2_fields in
  if r1 <> [] && r2 <> [] then begin
    let ht = Hashtbl.create (List.length r1) in
    List.iter (fun (l,_) -> Hashtbl.add ht (hash_variant l) l) r1;
    List.iter
      (fun (l,_) ->
        try raise (Tags(l, Hashtbl.find ht (hash_variant l)))
        with Not_found -> ())
      r2
  end;
  let fixed1 = fixed_explanation row1 and fixed2 = fixed_explanation row2 in
  let more = match fixed1, fixed2 with
    | Some _, Some _ -> if get_level rm2 < get_level rm1 then rm2 else rm1
    | Some _, None -> rm1
    | None, Some _ -> rm2
    | None, None ->
        newty2 ~level:(Int.min (get_level rm1) (get_level rm2))
          (Tvar { name = None; jkind = Jkind.value ~why:Row_variable })
  in
  let fixed = merge_fixed_explanation fixed1 fixed2
  and closed = row1_closed || row2_closed in
  let keep switch =
    List.for_all
      (fun (_,f1,f2) ->
        let f1, f2 = switch f1 f2 in
        row_field_repr f1 = Rabsent || row_field_repr f2 <> Rabsent)
      pairs
  in
  let empty fields =
    List.for_all (fun (_,f) -> row_field_repr f = Rabsent) fields in
  (* Check whether we are going to build an empty type *)
  if closed && (empty r1 || row2_closed) && (empty r2 || row1_closed)
  && List.for_all
      (fun (_,f1,f2) ->
        row_field_repr f1 = Rabsent || row_field_repr f2 = Rabsent)
      pairs
  then raise_for Unify (Variant No_intersection);
  let name =
    if row1_name <> None && (row1_closed || empty r2) &&
      (not row2_closed || keep (fun f1 f2 -> f1, f2) && empty r1)
    then row1_name
    else if row2_name <> None && (row2_closed || empty r1) &&
      (not row1_closed || keep (fun f1 f2 -> f2, f1) && empty r2)
    then row2_name
    else None
  in
  let set_more pos row rest =
    let rest =
      if closed then
        filter_row_fields (row_closed row) rest
      else rest in
    begin match fixed_explanation row with
      | None ->
          if rest <> [] && row_closed row then
            raise_for Unify (Variant (No_tags(pos,rest)))
      | Some fixed ->
          if closed && not (row_closed row) then
            raise_for Unify (Variant (Fixed_row(pos,Cannot_be_closed,fixed)))
          else if rest <> [] then
            let case = Cannot_add_tags (List.map fst rest) in
            raise_for Unify (Variant (Fixed_row(pos,case,fixed)))
    end;
    (* The following test is not principal... should rather use Tnil *)
    let rm = row_more row in
    (*if !trace_gadt_instances && rm.desc = Tnil then () else*)
    if !trace_gadt_instances && not (in_subst_mode ()) then
      (* in_subst_mode: see PR#11771 *)
      update_level_for Unify !env (get_level rm) (newgenty (Tvariant row));
    if has_fixed_explanation row then
      if eq_type more rm then () else
      if is_Tvar rm then link_type rm more else unify env rm more
    else
      let ty =
        newgenty (Tvariant
                    (create_row ~fields:rest ~more ~closed ~fixed ~name))
      in
      update_level_for Unify !env (get_level rm) ty;
      update_scope_for Unify (get_scope rm) ty;
      link_type rm ty
  in
  let tm1 = Transient_expr.repr rm1 and tm2 = Transient_expr.repr rm2 in
  let md1 = tm1.desc and md2 = tm2.desc in
  begin try
    set_more Second row2 r1;
    set_more First row1 r2;
    List.iter
      (fun (l,f1,f2) ->
        try unify_row_field env fixed1 fixed2 rm1 rm2 l f1 f2
        with Unify_trace trace ->
          raise_trace_for Unify (Variant (Incompatible_types_for l) :: trace)
      )
      pairs;
    if static_row row1 then begin
      let rm = row_more row1 in
      if is_Tvar rm then link_type rm (newty2 ~level:(get_level rm) Tnil)
    end
  with exn ->
    Transient_expr.set_desc tm1 md1;
    Transient_expr.set_desc tm2 md2;
    raise exn
  end

and unify_row_field env fixed1 fixed2 rm1 rm2 l f1 f2 =
  let if_not_fixed (pos,fixed) f =
    match fixed with
    | None -> f ()
    | Some fix ->
        let tr = [Variant(Fixed_row(pos,Cannot_add_tags [l],fix))] in
        raise_trace_for Unify tr in
  let first = First, fixed1 and second = Second, fixed2 in
  let either_fixed = match fixed1, fixed2 with
    | None, None -> false
    | _ -> true in
  if f1 == f2 then () else
  match row_field_repr f1, row_field_repr f2 with
    Rpresent(Some t1), Rpresent(Some t2) -> unify env t1 t2
  | Rpresent None, Rpresent None -> ()
  | Reither(c1, tl1, m1), Reither(c2, tl2, m2) ->
      if eq_row_field_ext f1 f2 then () else
      let no_arg = c1 || c2 and matched = m1 || m2 in
      if either_fixed && not no_arg
      && List.length tl1 = List.length tl2 then begin
        (* PR#7496 *)
        let f = rf_either [] ~no_arg ~matched in
        link_row_field_ext ~inside:f1 f; link_row_field_ext ~inside:f2 f;
        List.iter2 (unify env) tl1 tl2
      end
      else let redo =
        (m1 || m2 || either_fixed ||
         !rigid_variants && (List.length tl1 = 1 || List.length tl2 = 1)) &&
        begin match tl1 @ tl2 with [] -> false
        | t1 :: tl ->
            if no_arg then raise_unexplained_for Unify;
            Types.changed_row_field_exts [f1;f2] (fun () ->
                List.iter (unify env t1) tl
              )
        end in
      if redo then unify_row_field env fixed1 fixed2 rm1 rm2 l f1 f2 else
      let remq tl =
        List.filter (fun ty -> not (List.exists (eq_type ty) tl)) in
      let tl1' = remq tl2 tl1 and tl2' = remq tl1 tl2 in
      (* PR#6744 *)
      let (tlu1,tl1') = List.partition (has_free_univars !env) tl1'
      and (tlu2,tl2') = List.partition (has_free_univars !env) tl2' in
      begin match tlu1, tlu2 with
        [], [] -> ()
      | (tu1::tlu1), _ :: _ ->
          (* Attempt to merge all the types containing univars *)
          List.iter (unify env tu1) (tlu1@tlu2)
      | (tu::_, []) | ([], tu::_) ->
          occur_univar_for Unify !env tu
      end;
      (* Is this handling of levels really principal? *)
      let update_levels rm =
        List.iter
          (fun ty ->
            update_level_for Unify !env (get_level rm) ty;
            update_scope_for Unify (get_scope rm) ty)
      in
      update_levels rm2 tl1';
      update_levels rm1 tl2';
      let f1' = rf_either tl2' ~no_arg ~matched in
      let f2' = rf_either tl1' ~use_ext_of:f1' ~no_arg ~matched in
      link_row_field_ext ~inside:f1 f1'; link_row_field_ext ~inside:f2 f2';
  | Reither(_, _, false), Rabsent ->
      if_not_fixed first (fun () -> link_row_field_ext ~inside:f1 f2)
  | Rabsent, Reither(_, _, false) ->
      if_not_fixed second (fun () -> link_row_field_ext ~inside:f2 f1)
  | Rabsent, Rabsent -> ()
  | Reither(false, tl, _), Rpresent(Some t2) ->
      if_not_fixed first (fun () ->
          let s = snapshot () in
          link_row_field_ext ~inside:f1 f2;
          update_level_for Unify !env (get_level rm1) t2;
          update_scope_for Unify (get_scope rm1) t2;
          (try List.iter (fun t1 -> unify env t1 t2) tl
           with exn -> undo_first_change_after s; raise exn)
        )
  | Rpresent(Some t1), Reither(false, tl, _) ->
      if_not_fixed second (fun () ->
          let s = snapshot () in
          link_row_field_ext ~inside:f2 f1;
          update_level_for Unify !env (get_level rm2) t1;
          update_scope_for Unify (get_scope rm2) t1;
          (try List.iter (unify env t1) tl
           with exn -> undo_first_change_after s; raise exn)
        )
  | Reither(true, [], _), Rpresent None ->
      if_not_fixed first (fun () -> link_row_field_ext ~inside:f1 f2)
  | Rpresent None, Reither(true, [], _) ->
      if_not_fixed second (fun () -> link_row_field_ext ~inside:f2 f1)
  | _ -> raise_unexplained_for Unify

let unify env ty1 ty2 =
  let snap = Btype.snapshot () in
  try
    unify env ty1 ty2
  with
    Unify_trace trace ->
      undo_compress snap;
      raise (Unify (expand_to_unification_error !env trace))

let unify_gadt ~equations_level:lev ~allow_recursive_equations
      (env:Env.t ref) ty1 ty2 =
  try
    univar_pairs := [];
    gadt_equations_level := Some lev;
    let equated_types = TypePairs.create 0 in
    set_mode_pattern ~allow_recursive_equations ~equated_types
      (fun () -> unify env ty1 ty2);
    gadt_equations_level := None;
    TypePairs.clear unify_eq_set;
    equated_types
  with e ->
    gadt_equations_level := None;
    TypePairs.clear unify_eq_set;
    raise e

let unify_var ~from_subst env t1 t2 =
  if eq_type t1 t2 then () else
  match get_desc t1, get_desc t2 with
    Tvar _, Tconstr _ when deep_occur t1 t2 ->
      unify (ref env) t1 t2
    | Tvar { jkind }, _ ->
      let reset_tracing = check_trace_gadt_instances env in
      begin try
        occur_for Unify env t1 t2;
        update_level_for Unify env (get_level t1) t2;
        update_scope_for Unify (get_scope t1) t2;
        if not from_subst then begin
          unification_jkind_check env t2 jkind
        end;
        link_type t1 t2;
        reset_trace_gadt_instances reset_tracing;
      with Unify_trace trace ->
        reset_trace_gadt_instances reset_tracing;
        raise (Unify (expand_to_unification_error
                        env
                        (Diff { got = t1; expected = t2 } :: trace)))
      end
  | _ ->
      unify (ref env) t1 t2

(* CR layouts: comment here explaining why it's safe to skip jkind checks in
   this case when called from subst. *)
let _ = unify_var' := unify_var ~from_subst:true
let unify_var = unify_var ~from_subst:false

let unify_pairs env ty1 ty2 pairs =
  univar_pairs := pairs;
  unify env ty1 ty2

let unify env ty1 ty2 =
  unify_pairs (ref env) ty1 ty2 []

let unify_delaying_jkind_checks env ty1 ty2 =
  delay_jkind_checks_in (fun () ->
    unify_pairs (ref env) ty1 ty2 [])

(* Lower the level of a type to the current level *)
let enforce_current_level env ty =
  unify_var env (newvar (Jkind.any ~why:Dummy_jkind)) ty


(**** Special cases of unification ****)

let expand_head_trace env t =
  let reset_tracing = check_trace_gadt_instances env in
  let t = expand_head_unif env t in
  reset_trace_gadt_instances reset_tracing;
  t

(*
   Unify [t] and [l:'a -> 'b]. Return ['a] and ['b].
   In [-nolabels] mode, label mismatch is accepted when
   (1) the requested label is ""
   (2) the original label is not optional
*)

type filter_arrow_failure =
  | Unification_error of unification_error
  | Label_mismatch of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      }
  | Not_a_function
  | Jkind_error of type_expr * Jkind.Violation.t

exception Filter_arrow_failed of filter_arrow_failure

type filtered_arrow =
  { ty_arg : type_expr;
    arg_mode : Mode.Alloc.lr;
    ty_ret : type_expr;
    ret_mode : Mode.Alloc.lr
  }

let filter_arrow env t l ~force_tpoly =
  let function_type level =
    let k_arg = Jkind.any ~why:Inside_of_Tarrow in
    let k_res = Jkind.any ~why:Inside_of_Tarrow in
    let ty_arg =
      if not force_tpoly then begin
        assert (not (is_optional l));
        newvar2 level k_arg
      end else begin
        let t1 =
          if is_optional l then
            newty2 ~level
              (* CR layouts v5: Change the Jkind.value when option can
                 hold non-values. *)
              (Tconstr(Predef.path_option,
                       [newvar2 level Predef.option_argument_jkind],
                       ref Mnil))
          else
            newvar2 level k_arg
        in
        newty2 ~level (Tpoly(t1, []))
      end
    in
    let ty_ret = newvar2 level k_res in
    let arg_mode = Alloc.newvar () in
    let ret_mode = Alloc.newvar () in
    let t' =
      newty2 ~level (Tarrow ((l, arg_mode, ret_mode), ty_arg, ty_ret, commu_ok))
    in
    t', { ty_arg; arg_mode; ty_ret; ret_mode }
  in
  let t =
    try expand_head_trace env t
    with Unify_trace trace ->
      let t', _ = function_type (get_level t) in
      raise (Filter_arrow_failed
               (Unification_error
                  (expand_to_unification_error
                     env
                     (Diff { got = t'; expected = t } :: trace))))
  in
  match get_desc t with
    Tvar { jkind } ->
      let t', arrow_desc = function_type (get_level t) in
      begin match constrain_type_jkind env t' jkind with
      | Ok _ -> ()
      | Error err ->
        raise (Filter_arrow_failed
                 (Unification_error
                    (expand_to_unification_error
                       env
                       [Bad_jkind (t',err)])))
      end;
      link_type t t';
      arrow_desc
  | Tarrow((l', arg_mode, ret_mode), ty_arg, ty_ret, _) ->
      if l = l' || !Clflags.classic && l = Nolabel && not (is_optional l')
      then
        { ty_arg; arg_mode; ty_ret; ret_mode }
      else raise (Filter_arrow_failed
                    (Label_mismatch
                       { got = l; expected = l'; expected_type = t }))
  | _ ->
      raise (Filter_arrow_failed Not_a_function)

exception Filter_mono_failed

let filter_mono ty =
  match get_desc ty with
  | Tpoly(ty, []) -> ty
  | Tpoly _ -> raise Filter_mono_failed
  | _ -> assert false

exception Filter_arrow_mono_failed

let filter_arrow_mono env t l =
  match filter_arrow env t l ~force_tpoly:true with
  | exception Filter_arrow_failed _ -> raise Filter_arrow_mono_failed
  | {ty_arg; _} as farr ->
      match filter_mono ty_arg with
      | exception Filter_mono_failed -> raise Filter_arrow_mono_failed
      | ty_arg -> { farr with ty_arg }

type filter_method_failure =
  | Unification_error of unification_error
  | Not_a_method
  | Not_an_object of type_expr
  | Not_a_value of Jkind.Violation.t

exception Filter_method_failed of filter_method_failure

(* Used by [filter_method]. *)
let rec filter_method_field env name ty =
  let method_type ~level =
      let ty1 = newvar2 level (Jkind.value ~why:Object_field) in
      let ty2 = newvar2 level (Jkind.value ~why:Row_variable) in
      let ty' = newty2 ~level (Tfield (name, field_public, ty1, ty2)) in
      ty', ty1
  in
  let ty =
    try expand_head_trace env ty
    with Unify_trace trace ->
      let level = get_level ty in
      let ty', _ = method_type ~level in
      raise (Filter_method_failed
               (Unification_error
                  (expand_to_unification_error
                     env
                     (Diff { got = ty; expected = ty' } :: trace))))
  in
  match get_desc ty with
  | Tvar _ ->
      let level = get_level ty in
      let ty', ty1 = method_type ~level in
      link_type ty ty';
      ty1
  | Tfield(n, kind, ty1, ty2) ->
      if n = name then begin
        unify_kind kind field_public;
        ty1
      end else
        filter_method_field env name ty2
  | _ ->
      raise (Filter_method_failed Not_a_method)

(* Unify [ty] and [< name : 'a; .. >]. Return ['a]. *)
let filter_method env name ty =
  let object_type ~level ~scope =
      let ty1 = newvar2 level (Jkind.value ~why:Row_variable) in
      let ty' = newty3 ~level ~scope (Tobject (ty1, ref None)) in
      let ty_meth = filter_method_field env name ty1 in
      (ty', ty_meth)
  in
  let ty =
    try expand_head_trace env ty
    with Unify_trace trace ->
      let level = get_level ty in
      let scope = get_scope ty in
      let ty', _ = object_type ~level ~scope in
      raise (Filter_method_failed
               (Unification_error
                  (expand_to_unification_error
                     env
                     (Diff { got = ty; expected = ty' } :: trace))))
  in
  match get_desc ty with
  | Tvar _ ->
      let level = get_level ty in
      let scope = get_scope ty in
      let ty', ty_meth = object_type ~level ~scope in
      begin match
        constrain_type_jkind env ty (Jkind.value ~why:Object)
      with
      | Ok _ -> ()
      | Error err -> raise (Filter_method_failed (Not_a_value err))
      end;
      link_type ty ty';
      ty_meth
  | Tobject(f, _) ->
      filter_method_field env name f
  | _ ->
      raise (Filter_method_failed (Not_an_object ty))

exception Filter_method_row_failed

let rec filter_method_row env name priv ty =
  let ty = expand_head env ty in
  match get_desc ty with
  | Tvar _ ->
      let level = get_level ty in
      let field = newvar2 level (Jkind.value ~why:Object_field) in
      let row = newvar2 level (Jkind.value ~why:Row_variable) in
      let kind, priv =
        match priv with
        | Private ->
            let kind = field_private () in
            kind, Mprivate kind
        | Public ->
            field_public, Mpublic
      in
      let ty' = newty2 ~level (Tfield (name, kind, field, row)) in
      link_type ty ty';
      priv, field, row
  | Tfield(n, kind, ty1, ty2) ->
      if n = name then begin
        let priv =
          match priv with
          | Public ->
              unify_kind kind field_public;
              Mpublic
          | Private -> Mprivate kind
        in
        priv, ty1, ty2
      end else begin
        let level = get_level ty in
        let priv, field, row = filter_method_row env name priv ty2 in
        let row = newty2 ~level (Tfield (n, kind, ty1, row)) in
        priv, field, row
      end
  | Tnil ->
      if name = Btype.dummy_method then raise Filter_method_row_failed
      else begin
        match priv with
        | Public -> raise Filter_method_row_failed
        | Private ->
          let level = get_level ty in
          let kind = field_absent in
          Mprivate kind, newvar2 level (Jkind.value ~why:Object_field), ty
      end
  | _ ->
      raise Filter_method_row_failed

(* Operations on class signatures *)

let new_class_signature () =
  let row = newvar (Jkind.value ~why:Row_variable) in
  let self = newobj row in
  { csig_self = self;
    csig_self_row = row;
    csig_vars = Vars.empty;
    csig_meths = Meths.empty; }

let add_dummy_method env ~scope sign =
  let _, ty, row =
    filter_method_row env dummy_method Private sign.csig_self_row
  in
  unify env ty (new_scoped_ty scope (Ttuple []));
  sign.csig_self_row <- row

type add_method_failure =
  | Unexpected_method
  | Type_mismatch of Errortrace.unification_error

exception Add_method_failed of add_method_failure

let add_method env label priv virt ty sign =
  let meths = sign.csig_meths in
  let priv, virt =
    match Meths.find label meths with
    | (priv', virt', ty') -> begin
        let priv =
          match priv' with
          | Mpublic -> Mpublic
          | Mprivate k ->
            match priv with
            | Public ->
                begin match field_kind_repr k with
                | Fpublic -> ()
                | Fprivate -> link_kind ~inside:k field_public
                | Fabsent -> assert false
                end;
                Mpublic
            | Private -> priv'
        in
        let virt =
          match virt' with
          | Concrete -> Concrete
          | Virtual -> virt
        in
        match unify env ty ty' with
        | () -> priv, virt
        | exception Unify trace ->
            raise (Add_method_failed (Type_mismatch trace))
      end
    | exception Not_found -> begin
        let priv, ty', row =
          match filter_method_row env label priv sign.csig_self_row with
          | priv, ty', row ->
              priv, ty', row
          | exception Filter_method_row_failed ->
              raise (Add_method_failed Unexpected_method)
        in
        match unify env ty ty' with
        | () ->
            sign.csig_self_row <- row;
            priv, virt
        | exception Unify trace ->
            raise (Add_method_failed (Type_mismatch trace))
      end
  in
  let meths = Meths.add label (priv, virt, ty) meths in
  sign.csig_meths <- meths

type add_instance_variable_failure =
  | Mutability_mismatch of mutable_flag
  | Type_mismatch of Errortrace.unification_error

exception Add_instance_variable_failed of add_instance_variable_failure

let check_mutability mut mut' =
  match mut, mut' with
  | Mutable, Mutable -> ()
  | Immutable, Immutable -> ()
  | Mutable, Immutable | Immutable, Mutable ->
      raise (Add_instance_variable_failed (Mutability_mismatch mut))

let add_instance_variable ~strict env label mut virt ty sign =
  let vars = sign.csig_vars in
  let virt =
    match Vars.find label vars with
    | (mut', virt', ty') ->
        let virt =
          match virt' with
          | Concrete -> Concrete
          | Virtual -> virt
        in
        if strict then begin
          check_mutability mut mut';
          match unify env ty ty' with
          | () -> ()
          | exception Unify trace ->
              raise (Add_instance_variable_failed (Type_mismatch trace))
        end;
        virt
    | exception Not_found -> virt
  in
  let vars = Vars.add label (mut, virt, ty) vars in
  sign.csig_vars <- vars

type inherit_class_signature_failure =
  | Self_type_mismatch of Errortrace.unification_error
  | Method of label * add_method_failure
  | Instance_variable of label * add_instance_variable_failure

exception Inherit_class_signature_failed of inherit_class_signature_failure

let unify_self_types env sign1 sign2 =
  let self_type1 = sign1.csig_self in
  let self_type2 = sign2.csig_self in
  match unify env self_type1 self_type2 with
  | () -> ()
  | exception Unify err -> begin
      match err.trace with
      | Errortrace.Diff _ :: Errortrace.Incompatible_fields {name; _} :: rem ->
          let err = Errortrace.unification_error ~trace:rem in
          let failure = Method (name, Type_mismatch err) in
          raise (Inherit_class_signature_failed failure)
      | _ ->
          raise (Inherit_class_signature_failed (Self_type_mismatch err))
    end

(* Unify components of sign2 into sign1 *)
let inherit_class_signature ~strict env sign1 sign2 =
  unify_self_types env sign1 sign2;
  Meths.iter
    (fun label (priv, virt, ty) ->
       let priv =
         match priv with
         | Mpublic -> Public
         | Mprivate kind ->
             assert (field_kind_repr kind = Fabsent);
             Private
       in
       match add_method env label priv virt ty sign1 with
       | () -> ()
       | exception Add_method_failed failure ->
           let failure = Method(label, failure) in
           raise (Inherit_class_signature_failed failure))
    sign2.csig_meths;
  Vars.iter
    (fun label (mut, virt, ty) ->
       match add_instance_variable ~strict env label mut virt ty sign1 with
       | () -> ()
       | exception Add_instance_variable_failed failure ->
           let failure = Instance_variable(label, failure) in
           raise (Inherit_class_signature_failed failure))
    sign2.csig_vars

let update_class_signature env sign =
  let self = expand_head env sign.Types.csig_self in
  let fields, row = flatten_fields (object_fields self) in
  let meths, implicitly_public, implicitly_declared =
    List.fold_left
      (fun (meths, implicitly_public, implicitly_declared) (lab, k, ty) ->
         if lab = dummy_method then
           meths, implicitly_public, implicitly_declared
         else begin
           match Meths.find lab meths with
           | priv, virt, ty' ->
               let meths, implicitly_public =
                 match priv, field_kind_repr k with
                 | Mpublic, _ -> meths, implicitly_public
                 | Mprivate _, Fpublic ->
                     let meths = Meths.add lab (Mpublic, virt, ty') meths in
                     let implicitly_public = lab :: implicitly_public in
                     meths, implicitly_public
                 | Mprivate _, _ -> meths, implicitly_public
               in
               meths, implicitly_public, implicitly_declared
           | exception Not_found ->
               let meths, implicitly_declared =
                 match field_kind_repr k with
                 | Fpublic ->
                     let meths = Meths.add lab (Mpublic, Virtual, ty) meths in
                     let implicitly_declared = lab :: implicitly_declared in
                     meths, implicitly_declared
                 | Fprivate ->
                     let meths =
                       Meths.add lab (Mprivate k, Virtual, ty) meths
                     in
                     let implicitly_declared = lab :: implicitly_declared in
                     meths, implicitly_declared
                 | Fabsent -> meths, implicitly_declared
               in
               meths, implicitly_public, implicitly_declared
         end)
      (sign.csig_meths, [], []) fields
  in
  sign.csig_meths <- meths;
  sign.csig_self_row <- row;
  implicitly_public, implicitly_declared

let hide_private_methods env sign =
  let self = expand_head env sign.Types.csig_self in
  let fields, _ = flatten_fields (object_fields self) in
  List.iter
    (fun (_, k, _) ->
       match field_kind_repr k with
       | Fprivate -> link_kind ~inside:k field_absent
       | _    -> ())
    fields

let close_class_signature env sign =
  let rec close env ty =
    let ty = expand_head env ty in
    match get_desc ty with
    | Tvar _ ->
        let level = get_level ty in
        link_type ty (newty2 ~level Tnil); true
    | Tfield(lab, _, _, _) when lab = dummy_method ->
        false
    | Tfield(_, _, _, ty') -> close env ty'
    | Tnil -> true
    | _ -> assert false
  in
  let self = expand_head env sign.csig_self in
  close env (object_fields self)

let generalize_class_signature_spine env sign =
  (* Generalize the spine of methods *)
  let meths = sign.csig_meths in
  Meths.iter (fun _ (_, _, ty) -> generalize_spine ty) meths;
  let new_meths =
    Meths.map
      (fun (priv, virt, ty) -> (priv, virt, generic_instance ty))
      meths
  in
  (* But keep levels correct on the type of self *)
  Meths.iter
    (fun _ (_, _, ty) ->
       unify_var env (newvar (Jkind.value ~why:Object)) ty)
    meths;
  sign.csig_meths <- new_meths

                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(*
   Update the level of [ty]. First check that the levels of generic
   variables from the subject are not lowered.
*)
let moregen_occur env level ty =
  let rec occur ty =
    let lv = get_level ty in
    if lv <= level then () else
    if is_Tvar ty && lv >= generic_level - 1 then raise Occur else
    if try_mark_node ty then iter_type_expr occur ty
  in
  begin try
    occur ty; unmark_type ty
  with Occur ->
    unmark_type ty; raise_unexplained_for Moregen
  end;
  (* also check for free univars *)
  occur_univar_for Moregen env ty;
  update_level_for Moregen env level ty

type moregen_pairs =
  { invariant_pairs : TypePairs.t;
    covariant_pairs : TypePairs.t;
    contravariant_pairs : TypePairs.t;
    bivariant_pairs : TypePairs.t; }

let fresh_moregen_pairs () =
  { invariant_pairs = TypePairs.create 13;
    covariant_pairs = TypePairs.create 13;
    contravariant_pairs = TypePairs.create 13;
    bivariant_pairs = TypePairs.create 13; }

type moregen_variance =
  | Invariant
  | Covariant
  | Contravariant
  | Bivariant

let neg_variance = function
  | Invariant -> Invariant
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Bivariant -> Bivariant

let compose_variance variance v =
  match variance with
  | Invariant -> Invariant
  | Bivariant -> Bivariant
  | Covariant | Contravariant ->
    match Variance.get_upper v with
    | true, true -> Invariant
    | false, false -> Bivariant
    | false, true -> neg_variance variance
    | true, false -> variance

let relevant_pairs pairs v =
  match v with
  | Invariant -> pairs.invariant_pairs
  | Covariant -> pairs.covariant_pairs
  | Contravariant -> pairs.contravariant_pairs
  | Bivariant -> pairs.bivariant_pairs

let moregen_alloc_mode v a1 a2 =
  match
    match v with
    | Invariant -> Result.map_error ignore (Alloc.equate a1 a2)
    | Covariant -> Result.map_error ignore (Alloc.submode a1 a2)
    | Contravariant -> Result.map_error ignore (Alloc.submode a2 a1)
    | Bivariant -> Ok ()
  with
  | Ok () -> ()
  | Error _  -> raise_unexplained_for Moregen

let may_instantiate inst_nongen t1 =
  let level = get_level t1 in
  if inst_nongen then level <> generic_level - 1
                 else level =  generic_level

let rec moregen inst_nongen variance type_pairs env t1 t2 =
  if eq_type t1 t2 then () else

  try
    match (get_desc t1, get_desc t2) with
      (Tvar { jkind }, _) when may_instantiate inst_nongen t1 ->
        moregen_occur env (get_level t1) t2;
        update_scope_for Moregen (get_scope t1) t2;
        occur_for Moregen env t1 t2;
        (* use [check], not [constrain], here because [constrain] would be like
        instantiating [t2], which we do not wish to do *)
        check_type_jkind_exn env Moregen t2 jkind;
        link_type t1 t2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        if eq_type t1' t2' then () else
        let pairs = relevant_pairs type_pairs variance in
        if not (TypePairs.mem pairs (t1', t2')) then begin
          TypePairs.add pairs (t1', t2');
          match (get_desc t1', get_desc t2') with
            (Tvar { jkind }, _) when may_instantiate inst_nongen t1' ->
              moregen_occur env (get_level t1') t2;
              update_scope_for Moregen (get_scope t1') t2;
              (* use [check], not [constrain], here because [constrain] would be like
              instantiating [t2], which we do not wish to do *)
              check_type_jkind_exn env Moregen t2 jkind;
              link_type t1' t2
          | (Tarrow ((l1,a1,r1), t1, u1, _),
             Tarrow ((l2,a2,r2), t2, u2, _)) when
               (l1 = l2
                || !Clflags.classic && not (is_optional l1 || is_optional l2)) ->
              moregen inst_nongen (neg_variance variance) type_pairs env t1 t2;
              moregen inst_nongen variance type_pairs env u1 u2;
              moregen_alloc_mode (neg_variance variance) a1 a2;
              moregen_alloc_mode variance r1 r2
          | (Ttuple labeled_tl1, Ttuple labeled_tl2) ->
              moregen_labeled_list inst_nongen variance type_pairs env
                labeled_tl1 labeled_tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 -> begin
              match variance with
              | Invariant | Bivariant ->
                  moregen_list inst_nongen variance type_pairs env tl1 tl2
              | _ ->
                match Env.find_type p1 env with
                | decl ->
                    moregen_param_list inst_nongen variance type_pairs env
                      decl.type_variance tl1 tl2
                | exception Not_found ->
                    moregen_list inst_nongen Invariant type_pairs env tl1 tl2
            end
          | (Tpackage (p1, fl1), Tpackage (p2, fl2)) ->
              begin try
                unify_package env (moregen_list inst_nongen variance type_pairs env)
                  (get_level t1') p1 fl1 (get_level t2') p2 fl2
              with Not_found -> raise_unexplained_for Moregen
              end
          | (Tnil,  Tconstr _ ) -> raise_for Moregen (Obj (Abstract_row Second))
          | (Tconstr _,  Tnil ) -> raise_for Moregen (Obj (Abstract_row First))
          | (Tvariant row1, Tvariant row2) ->
              moregen_row inst_nongen variance type_pairs env row1 row2
          | (Tobject (fi1, _nm1), Tobject (fi2, _nm2)) ->
              moregen_fields inst_nongen variance type_pairs env fi1 fi2
          | (Tfield _, Tfield _) ->           (* Actually unused *)
              moregen_fields inst_nongen variance type_pairs env
                t1' t2'
          | (Tnil, Tnil) ->
              ()
          | (Tpoly (t1, []), Tpoly (t2, [])) ->
              moregen inst_nongen variance type_pairs env t1 t2
          | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
              enter_poly_for Moregen env univar_pairs t1 tl1 t2 tl2
                (moregen inst_nongen variance type_pairs env)
          | (Tunivar {jkind=k1}, Tunivar {jkind=k2}) ->
              unify_univar_for Moregen t1' t2' k1 k2 !univar_pairs
          | (_, _) ->
              raise_unexplained_for Moregen
        end
  with Moregen_trace trace ->
    raise_trace_for Moregen (Diff {got = t1; expected = t2} :: trace)


and moregen_list inst_nongen variance type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise_unexplained_for Moregen;
  List.iter2 (moregen inst_nongen variance type_pairs env) tl1 tl2

and moregen_labeled_list inst_nongen variance type_pairs env labeled_tl1
    labeled_tl2 =
  if not (Int.equal (List.length labeled_tl1) (List.length labeled_tl2)) then
    raise_unexplained_for Moregen;
  List.iter2
    (fun (label1, ty1) (label2, ty2) ->
      if not (Option.equal String.equal label1 label2) then
        raise_unexplained_for Moregen;
      moregen inst_nongen variance type_pairs env ty1 ty2)
    labeled_tl1 labeled_tl2

and moregen_param_list inst_nongen variance type_pairs env vl tl1 tl2 =
  match vl, tl1, tl2 with
  | [], [], [] -> ()
  | v :: vl, t1 :: tl1, t2 :: tl2 ->
    let param_variance = compose_variance variance v in
    moregen inst_nongen param_variance type_pairs env t1 t2;
    moregen_param_list inst_nongen variance type_pairs env vl tl1 tl2
  | _, _, _ -> raise_unexplained_for Moregen

and moregen_fields inst_nongen variance type_pairs env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  begin
    match miss1 with
    | (n, _, _) :: _ -> raise_for Moregen (Obj (Missing_field (Second, n)))
    | [] -> ()
  end;
  moregen inst_nongen variance type_pairs env rest1
    (build_fields (get_level ty2) miss2 rest2);
  List.iter
    (fun (name, k1, t1, k2, t2) ->
       (* The below call should never throw [Public_method_to_private_method] *)
       moregen_kind k1 k2;
       try moregen inst_nongen variance type_pairs env t1 t2 with Moregen_trace trace ->
         raise_trace_for Moregen
           (incompatible_fields ~name ~got:t1 ~expected:t2 :: trace)
    )
    pairs

and moregen_kind k1 k2 =
  match field_kind_repr k1, field_kind_repr k2 with
    (Fprivate, (Fprivate | Fpublic)) -> link_kind ~inside:k1 k2
  | (Fpublic, Fpublic)               -> ()
  | (Fpublic, Fprivate)              -> raise Public_method_to_private_method
  | (Fabsent, _) | (_, Fabsent)      -> assert false

and moregen_row inst_nongen variance type_pairs env row1 row2 =
  let Row {fields = row1_fields; more = rm1; closed = row1_closed} =
    row_repr row1 in
  let Row {fields = row2_fields; more = rm2; closed = row2_closed;
           fixed = row2_fixed} = row_repr row2 in
  if eq_type rm1 rm2 then () else
  let may_inst =
    is_Tvar rm1 && may_instantiate inst_nongen rm1 || get_desc rm1 = Tnil in
  let r1, r2, pairs = merge_row_fields row1_fields row2_fields in
  let r1, r2 =
    if row2_closed then
      filter_row_fields may_inst r1, filter_row_fields false r2
    else r1, r2
  in
  begin
    if r1 <> [] then raise_for Moregen (Variant (No_tags (Second, r1)))
  end;
  if row1_closed then begin
    match row2_closed, r2 with
    | false, _ -> raise_for Moregen (Variant (Openness Second))
    | _, _ :: _ -> raise_for Moregen (Variant (No_tags (First, r2)))
    | _, [] -> ()
  end;
  let md1 = get_desc rm1 (* This lets us undo a following [link_type] *) in
  begin match md1, get_desc rm2 with
    Tunivar {jkind=k1}, Tunivar {jkind=k2} ->
      unify_univar_for Moregen rm1 rm2 k1 k2 !univar_pairs
  | Tunivar _, _ | _, Tunivar _ ->
      raise_unexplained_for Moregen
  | _ when static_row row1 -> ()
  | _ when may_inst ->
      let ext =
        newgenty (Tvariant
                    (create_row ~fields:r2 ~more:rm2 ~name:None
                       ~fixed:row2_fixed ~closed:row2_closed))
      in
      moregen_occur env (get_level rm1) ext;
      update_scope_for Moregen (get_scope rm1) ext;
      (* This [link_type] has to be undone if the rest of the function fails *)
      link_type rm1 ext
  | Tconstr _, Tconstr _ ->
      moregen inst_nongen variance type_pairs env rm1 rm2
  | _ -> raise_unexplained_for Moregen
  end;
  try
    List.iter
      (fun (l,f1,f2) ->
         if f1 == f2 then () else
         match row_field_repr f1, row_field_repr f2 with
         (* Both matching [Rpresent]s *)
         | Rpresent(Some t1), Rpresent(Some t2) -> begin
             try
               moregen inst_nongen variance type_pairs env t1 t2
             with Moregen_trace trace ->
               raise_trace_for Moregen
                 (Variant (Incompatible_types_for l) :: trace)
           end
         | Rpresent None, Rpresent None -> ()
         (* Both [Reither] *)
         | Reither(c1, tl1, _), Reither(c2, tl2, m2) -> begin
             try
               if not (eq_row_field_ext f1 f2) then begin
                 if c1 && not c2 then raise_unexplained_for Moregen;
                 let f2' =
                   rf_either [] ~use_ext_of:f2 ~no_arg:c2 ~matched:m2 in
                 link_row_field_ext ~inside:f1 f2';
                 if List.length tl1 = List.length tl2 then
                   List.iter2 (moregen inst_nongen variance type_pairs env) tl1 tl2
                 else match tl2 with
                   | t2 :: _ ->
                     List.iter
                       (fun t1 -> moregen inst_nongen variance type_pairs env t1 t2)
                       tl1
                   | [] -> if tl1 <> [] then raise_unexplained_for Moregen
               end
             with Moregen_trace trace ->
               raise_trace_for Moregen
                 (Variant (Incompatible_types_for l) :: trace)
           end
         (* Generalizing [Reither] *)
         | Reither(false, tl1, _), Rpresent(Some t2) when may_inst -> begin
             try
               link_row_field_ext ~inside:f1 f2;
               List.iter
                 (fun t1 -> moregen inst_nongen variance type_pairs env t1 t2)
                 tl1
             with Moregen_trace trace ->
               raise_trace_for Moregen
                 (Variant (Incompatible_types_for l) :: trace)
           end
         | Reither(true, [], _), Rpresent None when may_inst ->
             link_row_field_ext ~inside:f1 f2
         | Reither(_, _, _), Rabsent when may_inst ->
             link_row_field_ext ~inside:f1 f2
         (* Both [Rabsent]s *)
         | Rabsent, Rabsent -> ()
         (* Mismatched constructor arguments *)
         | Rpresent (Some _), Rpresent None
         | Rpresent None, Rpresent (Some _) ->
             raise_for Moregen (Variant (Incompatible_types_for l))
         (* Mismatched presence *)
         | Reither _, Rpresent _ ->
             raise_for Moregen
               (Variant (Presence_not_guaranteed_for (First, l)))
         | Rpresent _, Reither _ ->
             raise_for Moregen
               (Variant (Presence_not_guaranteed_for (Second, l)))
         (* Missing tags *)
         | Rabsent, (Rpresent _ | Reither _) ->
             raise_for Moregen (Variant (No_tags (First, [l, f2])))
         | (Rpresent _ | Reither _), Rabsent ->
             raise_for Moregen (Variant (No_tags (Second, [l, f1]))))
      pairs
  with exn ->
    (* Undo [link_type] if we failed *)
    set_type_desc rm1 md1; raise exn

(*
   Non-generic variable can be instantiated only if [inst_nongen] is
   true. So, [inst_nongen] should be set to false if the subject might
   contain non-generic variables (and we do not want them to be
   instantiated).
   Usually, the subject is given by the user, and the pattern
   is unimportant.  So, no need to propagate abbreviations.
*)
let moregeneral env inst_nongen pat_sch subj_sch =
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let subj_inst = instance subj_sch in
  let subj = duplicate_type subj_inst in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let patt = instance pat_sch in
  Misc.try_finally
    (fun () ->
       try
         univar_pairs := [];
         let type_pairs = fresh_moregen_pairs () in
         moregen inst_nongen Covariant type_pairs env patt subj
       with Moregen_trace trace ->
         (* Moregen splits the generic level into two finer levels:
            [generic_level] and [generic_level - 1].  In order to properly
            detect and print weak variables when printing this error, we need to
            merge them back together, by regeneralizing the levels of the types
            after they were instantiated at [generic_level - 1] above.  Because
            [moregen] does some unification that we need to preserve for more
            legible error messages, we have to manually perform the
            regeneralization rather than backtracking. *)
         current_level := generic_level - 2;
         generalize subj_inst;
         raise (Moregen (expand_to_moregen_error env trace)))
    ~always:(fun () -> current_level := old_level)

let is_moregeneral env inst_nongen pat_sch subj_sch =
  match moregeneral env inst_nongen pat_sch subj_sch with
  | () -> true
  | exception Moregen _ -> false

let all_distinct_vars env vars =
  let tys = ref TypeSet.empty in
  List.for_all
    (fun ty ->
      let ty = expand_head env ty in
      if TypeSet.mem ty !tys then false else begin
       tys := TypeSet.add ty !tys;
       is_Tvar ty
     end)
    vars

type matches_result =
  | Unification_failure of Errortrace.unification_error
  | Jkind_mismatch of { original_jkind : jkind; inferred_jkind : jkind
                       ; ty : type_expr }
  | All_good

(* Alternative approach: "rigidify" a type scheme,
   and check validity after unification *)
(* Simpler, no? *)

let rec rigidify_rec vars ty =
  if try_mark_node ty then
    begin match get_desc ty with
    | Tvar { jkind } ->
        vars := TypeMap.add ty jkind !vars
    | Tvariant row ->
        let Row {more; name; closed} = row_repr row in
        if is_Tvar more && not (has_fixed_explanation row) then begin
          let more' = newty2 ~level:(get_level more) (get_desc more) in
          let row' =
            create_row ~fixed:(Some Rigid) ~fields:[] ~more:more'
              ~name ~closed
          in link_type more (newty2 ~level:(get_level ty) (Tvariant row'))
        end;
        iter_row (rigidify_rec vars) row;
        (* only consider the row variable if the variant is not static *)
        if not (static_row row) then
          rigidify_rec vars (row_more row)
    | _ ->
        iter_type_expr (rigidify_rec vars) ty
    end

(* remember free variables in a type so we can make sure they aren't unified;
   should be paired with a call to [all_distinct_vars_with_original__jkinds]
   later. *)
let rigidify ty =
  let vars = ref TypeMap.empty in
  rigidify_rec vars ty;
  unmark_type ty;
  List.map (fun (trans_expr, lay) -> Transient_expr.type_expr trans_expr, lay)
    (TypeMap.bindings !vars)

(* this version doesn't carry the unification error, which is computed after
   the error is detected *)
module No_trace = struct
  type matches_result_ =
    | Unification_failure
    | Jkind_mismatch of { original_jkind : jkind; inferred_jkind : jkind
                       ; ty : type_expr }
    | All_good
end

let all_distinct_vars_with_original_jkinds env vars_jkinds =
  let open No_trace in
  let tys = ref TypeSet.empty in
  let folder acc (ty, original_jkind) =
    match acc with
    | Unification_failure | Jkind_mismatch _ -> acc
    | All_good ->
       let open No_trace in
       let ty = expand_head env ty in
       if TypeSet.mem ty !tys then Unification_failure else begin
         tys := TypeSet.add ty !tys;
         match get_desc ty with
         | Tvar { jkind = inferred_jkind } ->
           if Jkind.equate inferred_jkind original_jkind
           then All_good
           else Jkind_mismatch { original_jkind; inferred_jkind; ty }
         | _ -> Unification_failure
       end
  in
  List.fold_left folder All_good vars_jkinds

let matches ~expand_error_trace env ty ty' =
  let snap = snapshot () in
  let vars_jkinds = rigidify ty in
  cleanup_abbrev ();
  match unify env ty ty' with
  | () ->
    let result =
      match all_distinct_vars_with_original_jkinds env vars_jkinds with
      | Unification_failure ->
        let diff =
            if expand_error_trace
            then expanded_diff env ~got:ty ~expected:ty'
            else unexpanded_diff ~got:ty ~expected:ty'
        in
        Unification_failure (unification_error ~trace:[diff])
      | Jkind_mismatch { original_jkind; inferred_jkind; ty } ->
        Jkind_mismatch { original_jkind; inferred_jkind; ty }
      | All_good -> All_good
    in
    backtrack snap;
    result
  | exception Unify err ->
      backtrack snap;
      Unification_failure err

                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)

let expand_head_rigid env ty =
  let old = !rigid_variants in
  rigid_variants := true;
  let ty' = expand_head env ty in
  rigid_variants := old; ty'

let eqtype_subst type_pairs subst t1 l1 t2 l2 =
  if List.exists
      (fun (t,t') ->
        let found1 = eq_type t1 t in
        let found2 = eq_type t2 t' in
        if found1 && found2 then true else
        if found1 || found2 then raise_unexplained_for Equality else false)
      !subst
  then ()
  else begin
    if not (Jkind.equal l1 l2)
      then raise_for Equality (Unequal_var_jkinds (t1, l1, t2, l2));
    subst := (t1, t2) :: !subst;
    TypePairs.add type_pairs (t1, t2)
  end

let rec eqtype rename type_pairs subst env t1 t2 =
  if eq_type t1 t2 then () else

  try
    match (get_desc t1, get_desc t2) with
      (Tvar { jkind = k1 }, Tvar { jkind = k2 }) when rename ->
        eqtype_subst type_pairs subst t1 k1 t2 k2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head_rigid env t1 in
        let t2' = expand_head_rigid env t2 in
        (* Expansion may have changed the representative of the types... *)
        if eq_type t1' t2' then () else
        if not (TypePairs.mem type_pairs (t1', t2')) then begin
          TypePairs.add type_pairs (t1', t2');
          match (get_desc t1', get_desc t2') with
            (Tvar { jkind = k1 }, Tvar { jkind = k2 }) when rename ->
              eqtype_subst type_pairs subst t1' k1 t2' k2
          | (Tarrow ((l1,a1,r1), t1, u1, _),
             Tarrow ((l2,a2,r2), t2, u2, _)) when
               (l1 = l2
                || !Clflags.classic && not (is_optional l1 || is_optional l2)) ->
              eqtype rename type_pairs subst env t1 t2;
              eqtype rename type_pairs subst env u1 u2;
              eqtype_alloc_mode a1 a2;
              eqtype_alloc_mode r1 r2
          | (Ttuple labeled_tl1, Ttuple labeled_tl2) ->
              eqtype_labeled_list rename type_pairs subst env labeled_tl1
                labeled_tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (Tpackage (p1, fl1), Tpackage (p2, fl2)) ->
              begin try
                unify_package env (eqtype_list rename type_pairs subst env)
                  (get_level t1') p1 fl1 (get_level t2') p2 fl2
              with Not_found -> raise_unexplained_for Equality
              end
          | (Tnil,  Tconstr _ ) ->
              raise_for Equality (Obj (Abstract_row Second))
          | (Tconstr _,  Tnil ) ->
              raise_for Equality (Obj (Abstract_row First))
          | (Tvariant row1, Tvariant row2) ->
              eqtype_row rename type_pairs subst env row1 row2
          | (Tobject (fi1, _nm1), Tobject (fi2, _nm2)) ->
              eqtype_fields rename type_pairs subst env fi1 fi2
          | (Tfield _, Tfield _) ->       (* Actually unused *)
              eqtype_fields rename type_pairs subst env
                t1' t2'
          | (Tnil, Tnil) ->
              ()
          | (Tpoly (t1, []), Tpoly (t2, [])) ->
              eqtype rename type_pairs subst env t1 t2
          | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
              enter_poly_for Equality env univar_pairs t1 tl1 t2 tl2
                (eqtype rename type_pairs subst env)
          | (Tunivar {jkind=k1}, Tunivar {jkind=k2}) ->
              unify_univar_for Equality t1' t2' k1 k2 !univar_pairs
          | (_, _) ->
              raise_unexplained_for Equality
        end
  with Equality_trace trace ->
    raise_trace_for Equality (Diff {got = t1; expected = t2} :: trace)

and eqtype_list rename type_pairs subst env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise_unexplained_for Equality;
  List.iter2 (eqtype rename type_pairs subst env) tl1 tl2

and eqtype_labeled_list rename type_pairs subst env labeled_tl1 labeled_tl2 =
  if not (Int.equal (List.length labeled_tl1) (List.length labeled_tl2)) then
    raise_unexplained_for Equality;
  List.iter2
    (fun (label1, ty1) (label2, ty2) ->
      if not (Option.equal String.equal label1 label2) then
        raise_unexplained_for Equality;
      eqtype rename type_pairs subst env ty1 ty2)
    labeled_tl1 labeled_tl2

and eqtype_fields rename type_pairs subst env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  (* First check if same row => already equal *)
  let same_row =
    eq_type rest1 rest2 || TypePairs.mem type_pairs (rest1,rest2)
  in
  if same_row then () else
  (* Try expansion, needed when called from Includecore.type_manifest *)
  match get_desc (expand_head_rigid env rest2) with
    Tobject(ty2,_) -> eqtype_fields rename type_pairs subst env ty1 ty2
  | _ ->
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  eqtype rename type_pairs subst env rest1 rest2;
  match miss1, miss2 with
  | ((n, _, _)::_, _) -> raise_for Equality (Obj (Missing_field (Second, n)))
  | (_, (n, _, _)::_) -> raise_for Equality (Obj (Missing_field (First, n)))
  | [], [] ->
      List.iter
        (function (name, k1, t1, k2, t2) ->
           eqtype_kind k1 k2;
           try
             eqtype rename type_pairs subst env t1 t2;
           with Equality_trace trace ->
             raise_trace_for Equality
               (incompatible_fields ~name ~got:t1 ~expected:t2 :: trace))
        pairs

and eqtype_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
  | (Fprivate, Fprivate)
  | (Fpublic, Fpublic)   -> ()
  | _                    -> raise_unexplained_for Unify
                            (* It's probably not possible to hit this case with
                               real OCaml code *)

and eqtype_row rename type_pairs subst env row1 row2 =
  (* Try expansion, needed when called from Includecore.type_manifest *)
  match get_desc (expand_head_rigid env (row_more row2)) with
    Tvariant row2 -> eqtype_row rename type_pairs subst env row1 row2
  | _ ->
  let r1, r2, pairs = merge_row_fields (row_fields row1) (row_fields row2) in
  if row_closed row1 <> row_closed row2 then begin
    raise_for Equality
      (Variant (Openness (if row_closed row2 then First else Second)))
  end;
  if not (row_closed row1) then begin
    match r1, r2 with
    | _::_, _ -> raise_for Equality (Variant (No_tags (Second, r1)))
    | _, _::_ -> raise_for Equality (Variant (No_tags (First,  r2)))
    | _, _ -> ()
  end;
  begin
    match filter_row_fields false r1 with
    | [] -> ();
    | _ :: _ as r1 -> raise_for Equality (Variant (No_tags (Second, r1)))
  end;
  begin
    match filter_row_fields false r2 with
    | [] -> ()
    | _ :: _ as r2 -> raise_for Equality (Variant (No_tags (First, r2)))
  end;
  if not (static_row row1) then
    eqtype rename type_pairs subst env (row_more row1) (row_more row2);
  List.iter
    (fun (l,f1,f2) ->
       if f1 == f2 then () else
       match row_field_repr f1, row_field_repr f2 with
       (* Both matching [Rpresent]s *)
       | Rpresent(Some t1), Rpresent(Some t2) -> begin
           try
             eqtype rename type_pairs subst env t1 t2
           with Equality_trace trace ->
             raise_trace_for Equality
               (Variant (Incompatible_types_for l) :: trace)
         end
       | Rpresent None, Rpresent None -> ()
       (* Both matching [Reither]s *)
       | Reither(c1, [], _), Reither(c2, [], _) when c1 = c2 -> ()
       | Reither(c1, t1::tl1, _), Reither(c2, t2::tl2, _)
         when c1 = c2 -> begin
           try
             eqtype rename type_pairs subst env t1 t2;
             if List.length tl1 = List.length tl2 then
               (* if same length allow different types (meaning?) *)
               List.iter2 (eqtype rename type_pairs subst env) tl1 tl2
             else begin
               (* otherwise everything must be equal *)
               List.iter (eqtype rename type_pairs subst env t1) tl2;
               List.iter
                 (fun t1 -> eqtype rename type_pairs subst env t1 t2) tl1
             end
           with Equality_trace trace ->
             raise_trace_for Equality
               (Variant (Incompatible_types_for l) :: trace)
         end
       (* Both [Rabsent]s *)
       | Rabsent, Rabsent -> ()
       (* Mismatched constructor arguments *)
       | Rpresent (Some _), Rpresent None
       | Rpresent None, Rpresent (Some _)
       | Reither _, Reither _ ->
           raise_for Equality (Variant (Incompatible_types_for l))
       (* Mismatched presence *)
       | Reither _, Rpresent _ ->
           raise_for Equality
             (Variant (Presence_not_guaranteed_for (First, l)))
       | Rpresent _, Reither _ ->
           raise_for Equality
             (Variant (Presence_not_guaranteed_for (Second, l)))
       (* Missing tags *)
       | Rabsent, (Rpresent _ | Reither _) ->
           raise_for Equality (Variant (No_tags (First, [l, f2])))
       | (Rpresent _ | Reither _), Rabsent ->
           raise_for Equality (Variant (No_tags (Second, [l, f1]))))
    pairs

and eqtype_alloc_mode m1 m2 =
  (* FIXME implement properly *)
  unify_alloc_mode_for Equality m1 m2

(* Must empty univar_pairs first *)
let eqtype_list rename type_pairs subst env tl1 tl2 =
  univar_pairs := [];
  let snap = Btype.snapshot () in
  Misc.try_finally
    ~always:(fun () -> backtrack snap)
    (fun () -> eqtype_list rename type_pairs subst env tl1 tl2)

let eqtype rename type_pairs subst env t1 t2 =
  eqtype_list rename type_pairs subst env [t1] [t2]

(* Two modes: with or without renaming of variables *)
let equal env rename tyl1 tyl2 =
  let subst = ref [] in
  try eqtype_list rename (TypePairs.create 11) subst env tyl1 tyl2
  with Equality_trace trace ->
    raise (Equality (expand_to_equality_error env trace !subst))

let is_equal env rename tyl1 tyl2 =
  match equal env rename tyl1 tyl2 with
  | () -> true
  | exception Equality _ -> false

let rec equal_private env params1 ty1 params2 ty2 =
  match equal env true (params1 @ [ty1]) (params2 @ [ty2]) with
  | () -> ()
  | exception (Equality _ as err) ->
      match try_expand_safe_opt env (expand_head env ty1) with
      | ty1' -> equal_private env params1 ty1' params2 ty2
      | exception Cannot_expand -> raise err

                          (*************************)
                          (*  Class type matching  *)
                          (*************************)

type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of Env.t * equality_error
  | CM_Class_type_mismatch of Env.t * class_type * class_type
  | CM_Parameter_mismatch of Env.t * moregen_error
  | CM_Val_type_mismatch of string * Env.t * comparison_error
  | CM_Meth_type_mismatch of string * Env.t * comparison_error
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string

exception Failure of class_match_failure list

let match_class_sig_shape ~strict sign1 sign2 =
  let errors =
    Meths.fold
      (fun lab (priv, vr, _) err ->
         match Meths.find lab sign1.csig_meths with
         | exception Not_found -> CM_Missing_method lab::err
         | (priv', vr', _) ->
             match priv', priv with
             | Mpublic, Mprivate _ -> CM_Public_method lab::err
             | Mprivate _, Mpublic when strict -> CM_Private_method lab::err
             | _, _ ->
               match vr', vr with
               | Virtual, Concrete -> CM_Virtual_method lab::err
               | _, _ -> err)
      sign2.csig_meths []
  in
  let errors =
    Meths.fold
      (fun lab (priv, vr, _) err ->
         if Meths.mem lab sign2.csig_meths then err
         else begin
           let err =
             match priv with
             | Mpublic -> CM_Hide_public lab :: err
             | Mprivate _ -> err
           in
           match vr with
           | Virtual -> CM_Hide_virtual ("method", lab) :: err
           | Concrete -> err
         end)
      sign1.csig_meths errors
  in
  let errors =
    Vars.fold
      (fun lab (mut, vr, _) err ->
         match Vars.find lab sign1.csig_vars with
         | exception Not_found -> CM_Missing_value lab::err
         | (mut', vr', _) ->
             match mut', mut with
             | Immutable, Mutable -> CM_Non_mutable_value lab::err
             | _, _ ->
               match vr', vr with
               | Virtual, Concrete -> CM_Non_concrete_value lab::err
               | _, _ -> err)
      sign2.csig_vars errors
  in
  Vars.fold
    (fun lab (_,vr,_) err ->
      if vr = Virtual && not (Vars.mem lab sign2.csig_vars) then
        CM_Hide_virtual ("instance variable", lab) :: err
      else err)
    sign1.csig_vars errors

let rec moregen_clty trace type_pairs env cty1 cty2 =
  try
    match cty1, cty2 with
    | Cty_constr (_, _, cty1), _ ->
        moregen_clty true type_pairs env cty1 cty2
    | _, Cty_constr (_, _, cty2) ->
        moregen_clty true type_pairs env cty1 cty2
    | Cty_arrow (l1, ty1, cty1'), Cty_arrow (l2, ty2, cty2') when l1 = l2 ->
        begin
          try moregen true Covariant type_pairs env ty1 ty2 with Moregen_trace trace ->
            raise (Failure [
              CM_Parameter_mismatch (env, expand_to_moregen_error env trace)])
        end;
        moregen_clty false type_pairs env cty1' cty2'
    | Cty_signature sign1, Cty_signature sign2 ->
        Meths.iter
          (fun lab (_, _, ty) ->
             match Meths.find lab sign1.csig_meths with
             | exception Not_found ->
               (* This function is only called after checking that
                  all methods in sign2 are present in sign1. *)
               assert false
             | (_, _, ty') ->
                 match moregen true Covariant type_pairs env ty' ty with
                 | () -> ()
                 | exception Moregen_trace trace ->
                     raise (Failure [
                       CM_Meth_type_mismatch
                         (lab,
                          env,
                          Moregen_error
                            (expand_to_moregen_error env trace))]))
          sign2.csig_meths;
        Vars.iter
          (fun lab (_, _, ty) ->
             match Vars.find lab sign1.csig_vars with
             | exception Not_found ->
               (* This function is only called after checking that
                  all instance variables in sign2 are present in sign1. *)
               assert false
             | (_, _, ty') ->
                 match moregen true Covariant type_pairs env ty' ty with
                 | () -> ()
                 | exception Moregen_trace trace ->
                     raise (Failure [
                       CM_Val_type_mismatch
                         (lab,
                          env,
                          Moregen_error
                            (expand_to_moregen_error env trace))]))
          sign2.csig_vars
    | _ ->
        raise (Failure [])
  with
    Failure error when trace || error = [] ->
      raise (Failure (CM_Class_type_mismatch (env, cty1, cty2)::error))

let match_class_types ?(trace=true) env pat_sch subj_sch =
  let sign1 = signature_of_class_type pat_sch in
  let sign2 = signature_of_class_type subj_sch in
  let errors = match_class_sig_shape ~strict:false sign1 sign2 in
  match errors with
  | [] ->
      let old_level = !current_level in
      current_level := generic_level - 1;
      (*
         Generic variables are first duplicated with [instance].  So,
         their levels are lowered to [generic_level - 1].  The subject is
         then copied with [duplicate_type].  That way, its levels won't be
         changed.
      *)
      let (_, subj_inst) = instance_class [] subj_sch in
      let subj = duplicate_class_type subj_inst in
      current_level := generic_level;
      (* Duplicate generic variables *)
      let (_, patt) = instance_class [] pat_sch in
      let type_pairs = fresh_moregen_pairs () in
      let sign1 = signature_of_class_type patt in
      let sign2 = signature_of_class_type subj in
      let self1 = sign1.csig_self in
      let self2 = sign2.csig_self in
      let row1 = sign1.csig_self_row in
      let row2 = sign2.csig_self_row in
      TypePairs.add type_pairs.invariant_pairs (self1, self2);
      (* Always succeeds *)
      moregen true Covariant type_pairs env row1 row2;
      let res =
        match moregen_clty trace type_pairs env patt subj with
        | () -> []
        | exception Failure res ->
          (* We've found an error.  Moregen splits the generic level into two
             finer levels: [generic_level] and [generic_level - 1].  In order
             to properly detect and print weak variables when printing this
             error, we need to merge them back together, by regeneralizing the
             levels of the types after they were instantiated at
             [generic_level - 1] above.  Because [moregen] does some
             unification that we need to preserve for more legible error
             messages, we have to manually perform the regeneralization rather
             than backtracking. *)
          current_level := generic_level - 2;
          generalize_class_type subj_inst;
          res
      in
      current_level := old_level;
      res
  | errors ->
      CM_Class_type_mismatch (env, pat_sch, subj_sch) :: errors

let equal_clsig trace type_pairs subst env sign1 sign2 =
  try
    Meths.iter
      (fun lab (_, _, ty) ->
         match Meths.find lab sign1.csig_meths with
         | exception Not_found ->
             (* This function is only called after checking that
                all methods in sign2 are present in sign1. *)
             assert false
         | (_, _, ty') ->
             match eqtype true type_pairs subst env ty' ty with
             | () -> ()
             | exception Equality_trace trace ->
                 raise (Failure [
                   CM_Meth_type_mismatch
                     (lab,
                      env,
                      Equality_error
                        (expand_to_equality_error env trace !subst))]))
      sign2.csig_meths;
    Vars.iter
      (fun lab (_, _, ty) ->
         match Vars.find lab sign1.csig_vars with
         | exception Not_found ->
             (* This function is only called after checking that
                all instance variables in sign2 are present in sign1. *)
             assert false
         | (_, _, ty') ->
             match eqtype true type_pairs subst env ty' ty with
             | () -> ()
             | exception Equality_trace trace ->
                 raise (Failure [
                   CM_Val_type_mismatch
                     (lab,
                      env,
                      Equality_error
                        (expand_to_equality_error env trace !subst))]))
      sign2.csig_vars
  with
    Failure error when trace ->
      raise (Failure (CM_Class_type_mismatch
                        (env, Cty_signature sign1, Cty_signature sign2)::error))

let match_class_declarations env patt_params patt_type subj_params subj_type =
  let sign1 = signature_of_class_type patt_type in
  let sign2 = signature_of_class_type subj_type in
  let errors = match_class_sig_shape ~strict:true sign1 sign2 in
  match errors with
  | [] -> begin
      try
        let subst = ref [] in
        let type_pairs = TypePairs.create 53 in
        let self1 = sign1.csig_self in
        let self2 = sign2.csig_self in
        let row1 = sign1.csig_self_row in
        let row2 = sign2.csig_self_row in
        TypePairs.add type_pairs (self1, self2);
        (* Always succeeds *)
        eqtype true type_pairs subst env row1 row2;
        let lp = List.length patt_params in
        let ls = List.length subj_params in
        if lp  <> ls then
          raise (Failure [CM_Parameter_arity_mismatch (lp, ls)]);
        List.iter2 (fun p s ->
          try eqtype true type_pairs subst env p s with Equality_trace trace ->
            raise (Failure
                     [CM_Type_parameter_mismatch
                        (env, expand_to_equality_error env trace !subst)]))
          patt_params subj_params;
     (* old code: equal_clty false type_pairs subst env patt_type subj_type; *)
        equal_clsig false type_pairs subst env sign1 sign2;
        (* Use moregeneral for class parameters, need to recheck everything to
           keeps relationships (PR#4824) *)
        let clty_params =
          List.fold_right (fun ty cty -> Cty_arrow (Labelled "*",ty,cty)) in
        match_class_types ~trace:false env
          (clty_params patt_params patt_type)
          (clty_params subj_params subj_type)
      with Failure r -> r
    end
  | error ->
      error


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(**** Build a subtype of a given type. ****)

(* build_subtype:
   [visited] traces traversed object and variant types
   [loops] is a mapping from variables to variables, to reproduce
     positive loops in a class type
   [posi] true if the current variance is positive
   [level] number of expansions/enlargement allowed on this branch *)

let warn = ref false  (* whether double coercion might do better *)
let pred_expand n = if n mod 2 = 0 && n > 0 then pred n else n
let pred_enlarge n = if n mod 2 = 1 then pred n else n

type change = Unchanged | Equiv | Changed
let max_change c1 c2 =
  match c1, c2 with
  | _, Changed | Changed, _ -> Changed
  | Equiv, _ | _, Equiv -> Equiv
  | _ -> Unchanged

let collect l = List.fold_left (fun c1 (_, c2) -> max_change c1 c2) Unchanged l

let rec filter_visited = function
    [] -> []
  | {desc=Tobject _|Tvariant _} :: _ as l -> l
  | _ :: l -> filter_visited l

let memq_warn t visited =
  if List.memq t visited then (warn := true; true) else false

let find_cltype_for_path env p =
  let cl_abbr = Env.find_hash_type p env in
  match cl_abbr.type_manifest with
    Some ty ->
      begin match get_desc ty with
        Tobject(_,{contents=Some(p',_)}) when Path.same p p' -> cl_abbr, ty
      | _ -> raise Not_found
      end
  | None -> assert false

let has_constr_row' env t =
  has_constr_row (expand_abbrev env t)

let build_submode_pos m =
  let m', changed = Alloc.newvar_below m in
  let c = if changed then Changed else Unchanged in
  m', c

let build_submode_neg m =
  let m', changed = Alloc.newvar_above m in
  let c = if changed then Changed else Unchanged in
  m', c

let build_submode posi m =
  if posi then build_submode_pos (Alloc.allow_left m)
  else build_submode_neg (Alloc.allow_right m)

(* CR layouts v2.8: use the meet-with-constant morphism when available *)
(* CR layouts v2.8: merge with Typecore.mode_cross_left when [Value] and
   [Alloc] get unified *)
(* The approach here works only for 2-element modal axes. *)
let mode_cross_left env ty mode =
  (* CR layouts v2.8: The old check didn't check for principality, and so
     this one doesn't either. I think it should. But actually test results
     are bad when checking for principality. Really, I'm surprised that
     the types here aren't principal. In any case, leaving the check out
     now; will return and figure this out later. *)
  let jkind = type_jkind_purely env ty in
  let upper_bounds = Jkind.get_modal_upper_bounds jkind in
  let mode = Alloc.disallow_right mode in
  let mode =
    if Locality.Const.le upper_bounds.locality Locality.Const.min
    then Alloc.set_locality_min mode
    else mode
  in
  let mode =
    if Linearity.Const.le upper_bounds.linearity Linearity.Const.min
    then Alloc.set_linearity_min mode
    else mode
  in
  let mode =
    if Uniqueness.Const.le upper_bounds.uniqueness Uniqueness.Const.min
    then Alloc.set_uniqueness_min mode
    else mode
  in
  mode

(* CR layouts v2.8: merge with Typecore.expect_mode_cross when [Value]
   and [Alloc] get unified *)
(* The approach here works only for 2-element modal axes. *)
let mode_cross_right env ty mode =
  (* CR layouts v2.8: This should probably check for principality. See
     similar comment in [mode_cross_left]. *)
  let jkind = type_jkind_purely env ty in
  let upper_bounds = Jkind.get_modal_upper_bounds jkind in
  let mode = Alloc.disallow_left mode in
  let mode =
    if Locality.Const.le upper_bounds.locality Locality.Const.min
    then Alloc.set_locality_max mode
    else mode
  in
  let mode =
    if Linearity.Const.le upper_bounds.linearity Linearity.Const.min
    then Alloc.set_linearity_max mode
    else mode
  in
  let mode =
    if Uniqueness.Const.le upper_bounds.uniqueness Uniqueness.Const.min
    then Alloc.set_uniqueness_max mode
    else mode
  in
  mode

let rec build_subtype env (visited : transient_expr list)
    (loops : (int * type_expr) list) posi level t =
  match get_desc t with
    Tvar _ ->
      if posi then
        try
          let t' = List.assq (get_id t) loops in
          warn := true;
          (t', Equiv)
        with Not_found ->
          (t, Unchanged)
      else
        (t, Unchanged)
  | Tarrow((l,a,r), t1, t2, _) ->
      let tt = Transient_expr.repr t in
      if memq_warn tt visited then (t, Unchanged) else
      let visited = tt :: visited in
      let (t1', c1) = build_subtype env visited loops (not posi) level t1 in
      let (t2', c2) = build_subtype env visited loops posi level t2 in
      let (a', c3) =
        if level > 2 then begin
          (* If posi, then t1' >= t1, and we pick t1; otherwise we pick t1'. In
            either case we pick the smaller type which is the "real" type of
            runtime values, and easier to cross modes (and thus making the
            mode-crossing more complete). *)
          let t1 = if posi then t1 else t1' in
          let posi_arg = not posi in
          if posi_arg then begin
            let a = mode_cross_right env t1 a in
            build_submode_pos a
          end else begin
            let a = mode_cross_left env t1 a in
            build_submode_neg a
          end
        end else a, Unchanged
      in
      let (r', c4) =
        if level > 2 then build_submode posi r else r, Unchanged
      in
      let c = max_change c1 (max_change c2 (max_change c3 c4)) in
      if c > Unchanged
      then (newty (Tarrow((l,a',r'), t1', t2', commu_ok)), c)
      else (t, Unchanged)
  | Ttuple labeled_tlist ->
      let tt = Transient_expr.repr t in
      if memq_warn tt visited then (t, Unchanged) else
      let visited = tt :: visited in
      let labels, tlist = List.split labeled_tlist in
      let tlist' =
        List.map (build_subtype env visited loops posi level) tlist
      in
      let c = collect tlist' in
      if c > Unchanged then
        (newty (Ttuple (List.combine labels (List.map fst tlist'))), c)
      else (t, Unchanged)
  | Tconstr(p, tl, abbrev)
    when level > 0 && generic_abbrev env p && safe_abbrev env t
    && not (has_constr_row' env t) ->
      let t' = expand_abbrev env t in
      let level' = pred_expand level in
      begin try match get_desc t' with
        Tobject _ when posi && not (opened_object t') ->
          let cl_abbr, body = find_cltype_for_path env p in
          let ty =
            try
              subst env !current_level Public abbrev None
                cl_abbr.type_params tl body
            with Cannot_subst -> assert false in
          let ty1, tl1 =
            match get_desc ty with
              Tobject(ty1,{contents=Some(p',tl1)}) when Path.same p p' ->
                ty1, tl1
            | _ -> raise Not_found
          in
          (* Fix PR#4505: do not set ty to Tvar when it appears in tl1,
             as this occurrence might break the occur check.
             XXX not clear whether this correct anyway... *)
          if deep_occur_list ty tl1 then raise Not_found;
          set_type_desc ty
            (Tvar { name = None;
                    jkind = Jkind.value
                               ~why:(Unknown "build subtype 1")});
          let t'' = newvar (Jkind.value ~why:(Unknown "build subtype 2"))
          in
          let loops = (get_id ty, t'') :: loops in
          (* May discard [visited] as level is going down *)
          let (ty1', c) =
            build_subtype env [Transient_expr.repr t']
              loops posi (pred_enlarge level') ty1 in
          assert (is_Tvar t'');
          let nm =
            if c > Equiv || deep_occur ty ty1' then None else Some(p,tl1) in
          set_type_desc t'' (Tobject (ty1', ref nm));
          (try unify_var env ty t with Unify _ -> assert false);
          ( t'', Changed)
      | _ -> raise Not_found
      with Not_found ->
        let (t'',c) =
          build_subtype env visited loops posi level' t' in
        if c > Unchanged then (t'',c)
        else (t, Unchanged)
      end
  | Tconstr(p, tl, _abbrev) ->
      (* Must check recursion on constructors, since we do not always
         expand them *)
      let tt = Transient_expr.repr t in
      if memq_warn tt visited then (t, Unchanged) else
      let visited = tt :: visited in
      begin try
        let decl = Env.find_type p env in
        if level = 0 && generic_abbrev env p && safe_abbrev env t
        && not (has_constr_row' env t)
        then warn := true;
        let tl' =
          List.map2
            (fun v t ->
              let (co,cn) = Variance.get_upper v in
              if cn then
                if co then (t, Unchanged)
                else build_subtype env visited loops (not posi) level t
              else
                if co then build_subtype env visited loops posi level t
                else (newvar (Jkind.value
                                ~why:(Unknown "build_subtype 3")),
                      Changed))
            decl.type_variance tl
        in
        let c = collect tl' in
        if c > Unchanged then (newconstr p (List.map fst tl'), c)
        else (t, Unchanged)
      with Not_found ->
        (t, Unchanged)
      end
  | Tvariant row ->
      let tt = Transient_expr.repr t in
      if memq_warn tt visited || not (static_row row) then (t, Unchanged) else
      let level' = pred_enlarge level in
      let visited =
        tt :: if level' < level then [] else filter_visited visited in
      let fields = filter_row_fields false (row_fields row) in
      let fields =
        List.map
          (fun (l,f as orig) -> match row_field_repr f with
            Rpresent None ->
              if posi then
                (l, rf_either_of None), Unchanged
              else
                orig, Unchanged
          | Rpresent(Some t) ->
              let (t', c) = build_subtype env visited loops posi level' t in
              let f =
                if posi && level > 0
                then rf_either_of (Some t')
                else rf_present (Some t')
              in (l, f), c
          | _ -> assert false)
          fields
      in
      let c = collect fields in
      let row =
        create_row ~fields:(List.map fst fields)
          ~more:(newvar (Jkind.value ~why:Row_variable))
          ~closed:posi ~fixed:None
          ~name:(if c > Unchanged then None else row_name row)
      in
      (newty (Tvariant row), Changed)
  | Tobject (t1, _) ->
      let tt = Transient_expr.repr t in
      if memq_warn tt visited || opened_object t1 then (t, Unchanged) else
      let level' = pred_enlarge level in
      let visited =
        tt :: if level' < level then [] else filter_visited visited in
      let (t1', c) = build_subtype env visited loops posi level' t1 in
      if c > Unchanged then (newty (Tobject (t1', ref None)), c)
      else (t, Unchanged)
  | Tfield(s, _, t1, t2) (* Always present *) ->
      let (t1', c1) = build_subtype env visited loops posi level t1 in
      let (t2', c2) = build_subtype env visited loops posi level t2 in
      let c = max_change c1 c2 in
      if c > Unchanged then (newty (Tfield(s, field_public, t1', t2')), c)
      else (t, Unchanged)
  | Tnil ->
      if posi then
        let v = newvar (Jkind.value ~why:Tnil) in
        (v, Changed)
      else begin
        warn := true;
        (t, Unchanged)
      end
  | Tsubst _ | Tlink _ ->
      assert false
  | Tpoly(t1, tl) ->
      let (t1', c) = build_subtype env visited loops posi level t1 in
      if c > Unchanged then (newty (Tpoly(t1', tl)), c)
      else (t, Unchanged)
  | Tunivar _ | Tpackage _ ->
      (t, Unchanged)

let enlarge_type env ty =
  warn := false;
  (* [level = 4] allows 2 expansions involving objects/variants *)
  let (ty', _) = build_subtype env [] [] true 4 ty in
  (ty', !warn)

(**** Check whether a type is a subtype of another type. ****)

(*
    During the traversal, a trace of visited types is maintained. It
    is printed in case of error.
    Constraints (pairs of types that must be equals) are accumulated
    rather than being enforced straight. Indeed, the result would
    otherwise depend on the order in which these constraints are
    enforced.
    A function enforcing these constraints is returned. That way, type
    variables can be bound to their actual values before this function
    is called (see Typecore).
    Only well-defined abbreviations are expanded (hence the tests
    [generic_abbrev ...]).
*)

let subtypes = TypePairs.create 17

let subtype_error ~env ~trace ~unification_trace =
  raise (Subtype (Subtype.error
                    ~trace:(expand_subtype_trace env (List.rev trace))
                    ~unification_trace))

let subtype_alloc_mode env trace a1 a2 =
  match Alloc.submode a1 a2 with
  | Ok () -> ()
  | Error _ -> subtype_error ~env ~trace ~unification_trace:[]

let rec subtype_rec env trace t1 t2 cstrs =
  if eq_type t1 t2 then cstrs else

  if TypePairs.mem subtypes (t1, t2) then
    cstrs
  else begin
    TypePairs.add subtypes (t1, t2);
    match (get_desc t1, get_desc t2) with
      (Tvar _, _) | (_, Tvar _) ->
        (trace, t1, t2, !univar_pairs)::cstrs
    | (Tarrow((l1,a1,r1), t1, u1, _),
       Tarrow((l2,a2,r2), t2, u2, _)) when l1 = l2
      || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
        let cstrs =
          subtype_rec
            env
            (Subtype.Diff {got = t2; expected = t1} :: trace)
            t2 t1
            cstrs
        in
        let a2 = mode_cross_left env t2 a2 in
         subtype_alloc_mode env trace a2 a1;
        (* RHS mode of arrow types indicates allocation in the parent region
           and is not subject to mode crossing *)
        subtype_alloc_mode env trace r1 r2;
        subtype_rec
          env
          (Subtype.Diff {got = u1; expected = u2} :: trace)
          u1 u2
          cstrs
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_labeled_list env trace tl1 tl2 cstrs
    | (Tconstr(p1, [], _), Tconstr(p2, [], _)) when Path.same p1 p2 ->
        cstrs
    | (Tconstr(p1, _tl1, _abbrev1), _)
      when generic_abbrev env p1 && safe_abbrev env t1 ->
        subtype_rec env trace (expand_abbrev env t1) t2 cstrs
    | (_, Tconstr(p2, _tl2, _abbrev2))
      when generic_abbrev env p2 && safe_abbrev env t2 ->
        subtype_rec env trace t1 (expand_abbrev env t2) cstrs
    | (Tconstr(p1, tl1, _), Tconstr(p2, tl2, _)) when Path.same p1 p2 ->
        begin try
          let decl = Env.find_type p1 env in
          List.fold_left2
            (fun cstrs v (t1, t2) ->
              let (co, cn) = Variance.get_upper v in
              if co then
                if cn then
                  (trace, newty2 ~level:(get_level t1) (Ttuple[None, t1]),
                   newty2 ~level:(get_level t2) (Ttuple[None, t2]), !univar_pairs)
                  :: cstrs
                else
                  subtype_rec
                    env
                    (Subtype.Diff {got = t1; expected = t2} :: trace)
                    t1 t2
                    cstrs
              else
                if cn
                then
                  subtype_rec
                    env
                    (Subtype.Diff {got = t2; expected = t1} :: trace)
                    t2 t1
                    cstrs
                else cstrs)
            cstrs decl.type_variance (List.combine tl1 tl2)
        with Not_found ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (Tconstr(p1, _, _), _)
      when generic_private_abbrev env p1 && safe_abbrev_opt env t1 ->
        subtype_rec env trace (expand_abbrev_opt env t1) t2 cstrs
(*  | (_, Tconstr(p2, _, _)) when generic_private_abbrev false env p2 ->
        subtype_rec env trace t1 (expand_abbrev_opt env t2) cstrs *)
    | (Tobject (f1, _), Tobject (f2, _))
      when is_Tvar (object_row f1) && is_Tvar (object_row f2) ->
        (* Same row variable implies same object. *)
        (trace, t1, t2, !univar_pairs)::cstrs
    | (Tobject (f1, _), Tobject (f2, _)) ->
        subtype_fields env trace f1 f2 cstrs
    | (Tvariant row1, Tvariant row2) ->
        begin try
          subtype_row env trace row1 row2 cstrs
        with Exit ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (Tpoly (u1, []), Tpoly (u2, [])) ->
        subtype_rec env trace u1 u2 cstrs
    | (Tpoly (u1, tl1), Tpoly (u2, [])) ->
        let _, u1' = instance_poly false tl1 u1 in
        subtype_rec env trace u1' u2 cstrs
    | (Tpoly (u1, tl1), Tpoly (u2,tl2)) ->
        begin try
          enter_poly env univar_pairs u1 tl1 u2 tl2
            (fun t1 t2 -> subtype_rec env trace t1 t2 cstrs)
        with Escape _ ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (Tpackage (p1, fl1), Tpackage (p2, fl2)) ->
        begin try
          let ntl1 =
            complete_type_list env fl2 (get_level t1) (Mty_ident p1) fl1
          and ntl2 =
            complete_type_list env fl1 (get_level t2) (Mty_ident p2) fl2
              ~allow_absent:true in
          let cstrs' =
            List.map
              (fun (n2,t2) -> (trace, List.assoc n2 ntl1, t2, !univar_pairs))
              ntl2
          in
          if eq_package_path env p1 p2 then cstrs' @ cstrs
          else begin
            (* need to check module subtyping *)
            let snap = Btype.snapshot () in
            match List.iter (fun (_, t1, t2, _) -> unify env t1 t2) cstrs' with
            | () when !package_subtype env p1 fl1 p2 fl2 ->
              Btype.backtrack snap; cstrs' @ cstrs
            | () | exception Unify _ ->
              Btype.backtrack snap; raise Not_found
          end
        with Not_found ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (_, _) ->
        (trace, t1, t2, !univar_pairs)::cstrs
  end

and subtype_labeled_list env trace labeled_tl1 labeled_tl2 cstrs =
  if not (Int.equal (List.length labeled_tl1) (List.length labeled_tl2)) then
    subtype_error ~env ~trace ~unification_trace:[];
  List.fold_left2
    (fun cstrs (label1, ty1) (label2, ty2) ->
      if not (Option.equal String.equal label1 label2) then
        subtype_error ~env ~trace ~unification_trace:[];
      subtype_rec
        env
        (Subtype.Diff { got = ty1; expected = ty2 } :: trace)
        ty1 ty2
        cstrs)
    cstrs labeled_tl1 labeled_tl2

and subtype_fields env trace ty1 ty2 cstrs =
  (* Assume that either rest1 or rest2 is not Tvar *)
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let cstrs =
    if get_desc rest2 = Tnil then cstrs else
    if miss1 = [] then
      subtype_rec
        env
        (Subtype.Diff {got = rest1; expected = rest2} :: trace)
        rest1 rest2
        cstrs
    else
      (trace, build_fields (get_level ty1) miss1 rest1, rest2,
       !univar_pairs) :: cstrs
  in
  let cstrs =
    if miss2 = [] then cstrs else
    (trace, rest1, build_fields (get_level ty2) miss2
                     (newvar (Jkind.value ~why:Object_field)),
     !univar_pairs) :: cstrs
  in
  List.fold_left
    (fun cstrs (_, _k1, t1, _k2, t2) ->
       (* These fields are always present *)
       subtype_rec
         env
         (Subtype.Diff {got = t1; expected = t2} :: trace)
         t1 t2
         cstrs)
    cstrs pairs

and subtype_row env trace row1 row2 cstrs =
  let Row {fields = row1_fields; more = more1; closed = row1_closed} =
    row_repr row1 in
  let Row {fields = row2_fields; more = more2; closed = row2_closed} =
    row_repr row2 in
  let r1, r2, pairs =
    merge_row_fields row1_fields row2_fields in
  let r1 = if row2_closed then filter_row_fields false r1 else r1 in
  let r2 = if row1_closed then filter_row_fields false r2 else r2 in
  match get_desc more1, get_desc more2 with
    Tconstr(p1,_,_), Tconstr(p2,_,_) when Path.same p1 p2 ->
      subtype_rec
        env
        (Subtype.Diff {got = more1; expected = more2} :: trace)
        more1 more2
        cstrs
  | (Tvar _|Tconstr _|Tnil), (Tvar _|Tconstr _|Tnil)
    when row1_closed && r1 = [] ->
      List.fold_left
        (fun cstrs (_,f1,f2) ->
          match row_field_repr f1, row_field_repr f2 with
            (Rpresent None|Reither(true,_,_)), Rpresent None ->
              cstrs
          | Rpresent(Some t1), Rpresent(Some t2) ->
              subtype_rec
                env
                (Subtype.Diff {got = t1; expected = t2} :: trace)
                t1 t2
                cstrs
          | Reither(false, t1::_, _), Rpresent(Some t2) ->
              subtype_rec
                env
                (Subtype.Diff {got = t1; expected = t2} :: trace)
                t1 t2
                cstrs
          | Rabsent, _ -> cstrs
          | _ -> raise Exit)
        cstrs pairs
  | Tunivar _, Tunivar _
    when row1_closed = row2_closed && r1 = [] && r2 = [] ->
      let cstrs =
        subtype_rec
          env
          (Subtype.Diff {got = more1; expected = more2} :: trace)
          more1 more2
          cstrs
      in
      List.fold_left
        (fun cstrs (_,f1,f2) ->
          match row_field_repr f1, row_field_repr f2 with
            Rpresent None, Rpresent None
          | Reither(true,[],_), Reither(true,[],_)
          | Rabsent, Rabsent ->
              cstrs
          | Rpresent(Some t1), Rpresent(Some t2)
          | Reither(false,[t1],_), Reither(false,[t2],_) ->
              subtype_rec
                env
                (Subtype.Diff {got = t1; expected = t2} :: trace)
                t1 t2
                cstrs
          | _ -> raise Exit)
        cstrs pairs
  | _ ->
      raise Exit

let subtype env ty1 ty2 =
  TypePairs.clear subtypes;
  univar_pairs := [];
  (* Build constraint set. *)
  let cstrs =
    subtype_rec env [Subtype.Diff {got = ty1; expected = ty2}] ty1 ty2 []
  in
  TypePairs.clear subtypes;
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2, pairs) ->
         try unify_pairs (ref env) t1 t2 pairs with Unify {trace} ->
           subtype_error ~env ~trace:trace0 ~unification_trace:(List.tl trace))
      (List.rev cstrs)

                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)

(* Utility for printing. The resulting type is not used in computation. *)
let rec unalias_object ty =
  let level = get_level ty in
  match get_desc ty with
    Tfield (s, k, t1, t2) ->
      newty2 ~level (Tfield (s, k, t1, unalias_object t2))
  | Tvar _ | Tnil as desc ->
      newty2 ~level desc
  | Tunivar _ ->
      ty
  | Tconstr _ ->
      newvar2 level (Jkind.any ~why:Dummy_jkind)
  | _ ->
      assert false

let unalias ty =
  let level = get_level ty in
  match get_desc ty with
    Tvar _ | Tunivar _ ->
      ty
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      newty2 ~level
        (Tvariant
           (create_row ~fields ~name ~fixed ~closed ~more:
              (newty2 ~level:(get_level more) (get_desc more))))
  | Tobject (ty, nm) ->
      newty2 ~level (Tobject (unalias_object ty, nm))
  | desc ->
      newty2 ~level desc

(* Return the arity (as for curried functions) of the given type. *)
let rec arity ty =
  match get_desc ty with
    Tarrow(_, _t1, t2, _) -> 1 + arity t2
  | _ -> 0

(* Check for non-generalizable type variables *)
let add_nongen_vars_in_schema =
  let rec loop env ((visited, weak_set) as acc) ty =
    if TypeSet.mem ty visited
    then acc
    else begin
      let visited = TypeSet.add ty visited in
      match get_desc ty with
      | Tvar _ when get_level ty <> generic_level ->
          visited, TypeSet.add ty weak_set
      | Tconstr _ ->
          let (_, unexpanded_candidate) as unexpanded_candidate' =
            fold_type_expr
              (loop env)
              (visited, weak_set)
              ty
          in
          (* Using `==` is okay because `loop` will return the original set
             when it does not change it. Similarly, `TypeSet.add` will return
             the original set if the element is already present. *)
          if unexpanded_candidate == weak_set
          then (visited, weak_set)
          else begin
            match
              loop env (visited, weak_set)
                (try_expand_head try_expand_safe env ty)
            with
            | exception Cannot_expand -> unexpanded_candidate'
            | expanded_result -> expanded_result
          end
      | Tfield(_, kind, t1, t2) ->
          let visited, weak_set =
            match field_kind_repr kind with
            | Fpublic -> loop env (visited, weak_set) t1
            | _ -> visited, weak_set
          in
          loop env (visited, weak_set) t2
      | Tvariant row ->
          let visited, weak_set =
            fold_row (loop env) (visited, weak_set) row
          in
          if not (static_row row)
          then loop env (visited, weak_set) (row_more row)
          else (visited, weak_set)
      | _ ->
          fold_type_expr (loop env) (visited, weak_set) ty
    end
  in
  fun env acc ty ->
    remove_mode_and_jkind_variables ty;
    let _, result = loop env (TypeSet.empty, acc) ty in
    result

(* Return all non-generic variables of [ty]. *)
let nongen_vars_in_schema env ty =
  let result = add_nongen_vars_in_schema env TypeSet.empty ty in
  if TypeSet.is_empty result
  then None
  else Some result

(* Check that all type variables are generalizable *)
(* Use Env.empty to prevent expansion of recursively defined object types;
   cf. typing-poly/poly.ml *)
let nongen_class_type =
  let add_nongen_vars_in_schema' ty weak_set =
    add_nongen_vars_in_schema Env.empty weak_set ty
  in
  let add_nongen_vars_in_schema_fold fold m weak_set =
    let f _key (_,_,ty) weak_set =
      add_nongen_vars_in_schema Env.empty weak_set ty
    in
    fold f m weak_set
  in
  let rec nongen_class_type cty weak_set =
    match cty with
    | Cty_constr (_, params, _) ->
        List.fold_left
          (add_nongen_vars_in_schema Env.empty)
          weak_set
          params
    | Cty_signature sign ->
        weak_set
        |> add_nongen_vars_in_schema' sign.csig_self
        |> add_nongen_vars_in_schema' sign.csig_self_row
        |> add_nongen_vars_in_schema_fold Meths.fold sign.csig_meths
        |> add_nongen_vars_in_schema_fold Vars.fold sign.csig_vars
    | Cty_arrow (_, ty, cty) ->
        add_nongen_vars_in_schema' ty weak_set
        |> nongen_class_type cty
  in
  nongen_class_type

let nongen_class_declaration cty =
  List.fold_left
    (add_nongen_vars_in_schema Env.empty)
    TypeSet.empty
    cty.cty_params
  |> nongen_class_type cty.cty_type

let nongen_vars_in_class_declaration cty =
  let result = nongen_class_declaration cty in
  if TypeSet.is_empty result
  then None
  else Some result

(* Normalize a type before printing, saving... *)
(* Cannot use mark_type because deep_occur uses it too *)
let rec normalize_type_rec visited ty =
  if not (TypeSet.mem ty !visited) then begin
    visited := TypeSet.add ty !visited;
    let tm = row_of_type ty in
    begin if not (is_Tconstr ty) && is_constr_row ~allow_ident:false tm then
      match get_desc tm with (* PR#7348 *)
        Tconstr (Path.Pdot(m,i), tl, _abbrev) ->
          let i' = String.sub i 0 (String.length i - 4) in
          set_type_desc ty (Tconstr(Path.Pdot(m,i'), tl, ref Mnil))
      | _ -> assert false
    else match get_desc ty with
    | Tvariant row ->
      let Row {fields = orig_fields; more; name; fixed; closed} =
        row_repr row in
      let fields = List.map
          (fun (l,f) ->
            l,
            match row_field_repr f with Reither(b, ty::(_::_ as tyl), m) ->
              let tyl' =
                List.fold_left
                  (fun tyl ty ->
                     if List.exists
                          (fun ty' -> is_equal Env.empty false [ty] [ty'])
                          tyl
                     then tyl
                     else ty::tyl)
                  [ty] tyl
              in
              if List.length tyl' <= List.length tyl then
                rf_either (List.rev tyl') ~use_ext_of:f ~no_arg:b ~matched:m
              else f
            | _ -> f)
          orig_fields in
      let fields =
        List.sort (fun (p,_) (q,_) -> compare p q)
          (List.filter (fun (_,fi) -> row_field_repr fi <> Rabsent) fields) in
      set_type_desc ty (Tvariant
                          (create_row ~fields ~more ~name ~fixed ~closed))
    | Tobject (fi, nm) ->
        begin match !nm with
        | None -> ()
        | Some (n, v :: l) ->
            if deep_occur_list ty l then
              (* The abbreviation may be hiding something, so remove it *)
              set_name nm None
            else
            begin match get_desc v with
            | Tvar _ | Tunivar _ -> ()
            | Tnil -> set_type_desc ty (Tconstr (n, l, ref Mnil))
            | _    -> set_name nm None
            end
        | _ ->
            fatal_error "Ctype.normalize_type_rec"
        end;
        let level = get_level fi in
        if level < lowest_level then () else
        let fields, row = flatten_fields fi in
        let fi' = build_fields level fields row in
        set_type_desc fi (get_desc fi')
    | _ -> ()
    end;
    iter_type_expr (normalize_type_rec visited) ty;
  end

let normalize_type ty =
  normalize_type_rec (ref TypeSet.empty) ty


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


(*
   Variables are left unchanged. Other type nodes are duplicated, with
   levels set to generic level.
   We cannot use Tsubst here, because unification may be called by
   expand_abbrev.
*)

let nondep_hash     = TypeHash.create 47
let nondep_variants = TypeHash.create 17
let clear_hash ()   =
  TypeHash.clear nondep_hash; TypeHash.clear nondep_variants

let rec nondep_type_rec ?(expand_private=false) env ids ty =
  let try_expand env t =
    if expand_private then try_expand_safe_opt env t
    else try_expand_safe env t
  in
  match get_desc ty with
    Tvar _ | Tunivar _ -> ty
  | _ -> try TypeHash.find nondep_hash ty
  with Not_found ->
    let ty' = newgenstub ~scope:(get_scope ty)
                (Jkind.any ~why:Dummy_jkind) in
    TypeHash.add nondep_hash ty ty';
    match
      match get_desc ty with
      | Tconstr(p, tl, _abbrev) as desc ->
          begin try
            (* First, try keeping the same type constructor p *)
            match Path.find_free_opt ids p with
            | Some id ->
               raise (Nondep_cannot_erase id)
            | None ->
               Tconstr(p, List.map (nondep_type_rec env ids) tl, ref Mnil)
          with (Nondep_cannot_erase _) as exn ->
            (* If that doesn't work, try expanding abbrevs *)
            try Tlink (nondep_type_rec ~expand_private env ids
                         (try_expand env (newty2 ~level:(get_level ty) desc)))
              (*
                 The [Tlink] is important. The expanded type may be a
                 variable, or may not be completely copied yet
                 (recursive type), so one cannot just take its
                 description.
               *)
            with Cannot_expand -> raise exn
          end
      | Tpackage(p, fl) when Path.exists_free ids p ->
          let p' = normalize_package_path env p in
          begin match Path.find_free_opt ids p' with
          | Some id -> raise (Nondep_cannot_erase id)
          | None ->
            let nondep_field_rec (n, ty) = (n, nondep_type_rec env ids ty) in
            Tpackage (p', List.map nondep_field_rec fl)
          end
      | Tobject (t1, name) ->
          Tobject (nondep_type_rec env ids t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          if Path.exists_free ids p then None
                          else Some (p, List.map (nondep_type_rec env ids) tl)))
      | Tvariant row ->
          let more = row_more row in
          (* We must keep sharing according to the row variable *)
          begin try
            let ty2 = TypeHash.find nondep_variants more in
            (* This variant type has been already copied *)
            TypeHash.add nondep_hash ty ty2;
            Tlink ty2
          with Not_found ->
            (* Register new type first for recursion *)
            TypeHash.add nondep_variants more ty';
            let static = static_row row in
            let more' =
              if static then newgenty Tnil else nondep_type_rec env ids more
            in
            (* Return a new copy *)
            let row =
              copy_row (nondep_type_rec env ids) true row true more' in
            match row_name row with
              Some (p, _tl) when Path.exists_free ids p ->
                Tvariant (set_row_name row None)
            | _ -> Tvariant row
          end
      | desc -> copy_type_desc (nondep_type_rec env ids) desc
    with
    | desc ->
      Transient_expr.set_stub_desc ty' desc;
      ty'
    | exception e ->
      TypeHash.remove nondep_hash ty;
      raise e

let nondep_type env id ty =
  try
    let ty' = nondep_type_rec env id ty in
    clear_hash ();
    ty'
  with Nondep_cannot_erase _ as exn ->
    clear_hash ();
    raise exn

let () = nondep_type' := nondep_type

(* Preserve sharing inside type declarations. *)
let nondep_type_decl env mid is_covariant decl =
  try
    let params = List.map (nondep_type_rec env mid) decl.type_params in
    let tk =
      try map_kind (nondep_type_rec env mid) decl.type_kind
      with Nondep_cannot_erase _ when is_covariant -> Type_abstract Abstract_def
    and tm, priv =
      match decl.type_manifest with
      | None -> None, decl.type_private
      | Some ty ->
          try Some (nondep_type_rec env mid ty), decl.type_private
          with Nondep_cannot_erase _ when is_covariant ->
            clear_hash ();
            try Some (nondep_type_rec ~expand_private:true env mid ty),
                Private
            with Nondep_cannot_erase _ ->
              None, decl.type_private
    in
    clear_hash ();
    let priv =
      match tm with
      | Some ty when Btype.has_constr_row ty -> Private
      | _ -> priv
    in
    { type_params = params;
      type_arity = decl.type_arity;
      type_kind = tk;
      type_jkind = decl.type_jkind;
      type_jkind_annotation = decl.type_jkind_annotation;
      type_manifest = tm;
      type_private = priv;
      type_variance = decl.type_variance;
      type_separability = decl.type_separability;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = decl.type_loc;
      type_attributes = decl.type_attributes;
      type_unboxed_default = decl.type_unboxed_default;
      type_uid = decl.type_uid;
    }
  with Nondep_cannot_erase _ as exn ->
    clear_hash ();
    raise exn

(* Preserve sharing inside extension constructors. *)
let nondep_extension_constructor env ids ext =
  try
    let type_path, type_params =
      match Path.find_free_opt ids ext.ext_type_path with
      | Some id ->
        begin
          let ty =
            newgenty (Tconstr(ext.ext_type_path, ext.ext_type_params, ref Mnil))
          in
          let ty' = nondep_type_rec env ids ty in
            match get_desc ty' with
                Tconstr(p, tl, _) -> p, tl
              | _ -> raise (Nondep_cannot_erase id)
        end
      | None ->
        let type_params =
          List.map (nondep_type_rec env ids) ext.ext_type_params
        in
          ext.ext_type_path, type_params
    in
    let args = map_type_expr_cstr_args (nondep_type_rec env ids) ext.ext_args in
    let ret_type = Option.map (nondep_type_rec env ids) ext.ext_ret_type in
      clear_hash ();
      { ext_type_path = type_path;
        ext_type_params = type_params;
        ext_args = args;
        ext_arg_jkinds = ext.ext_arg_jkinds;
        ext_constant = ext.ext_constant;
        ext_ret_type = ret_type;
        ext_private = ext.ext_private;
        ext_attributes = ext.ext_attributes;
        ext_loc = ext.ext_loc;
        ext_uid = ext.ext_uid;
      }
  with Nondep_cannot_erase _ as exn ->
    clear_hash ();
    raise exn


(* Preserve sharing inside class types. *)
let nondep_class_signature env id sign =
  { csig_self = nondep_type_rec env id sign.csig_self;
    csig_self_row = nondep_type_rec env id sign.csig_self_row;
    csig_vars =
      Vars.map (function (m, v, t) -> (m, v, nondep_type_rec env id t))
        sign.csig_vars;
    csig_meths =
      Meths.map (function (p, v, t) -> (p, v, nondep_type_rec env id t))
        sign.csig_meths }

let rec nondep_class_type env ids =
  function
    Cty_constr (p, _, cty) when Path.exists_free ids p ->
      nondep_class_type env ids cty
  | Cty_constr (p, tyl, cty) ->
      Cty_constr (p, List.map (nondep_type_rec env ids) tyl,
                   nondep_class_type env ids cty)
  | Cty_signature sign ->
      Cty_signature (nondep_class_signature env ids sign)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, nondep_type_rec env ids ty, nondep_class_type env ids cty)

let nondep_class_declaration env ids decl =
  assert (not (Path.exists_free ids decl.cty_path));
  let decl =
    { cty_params = List.map (nondep_type_rec env ids) decl.cty_params;
      cty_variance = decl.cty_variance;
      cty_type = nondep_class_type env ids decl.cty_type;
      cty_path = decl.cty_path;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (nondep_type_rec env ids ty)
        end;
      cty_loc = decl.cty_loc;
      cty_attributes = decl.cty_attributes;
      cty_uid = decl.cty_uid;
    }
  in
  clear_hash ();
  decl

let nondep_cltype_declaration env ids decl =
  assert (not (Path.exists_free ids decl.clty_path));
  let decl =
    { clty_params = List.map (nondep_type_rec env ids) decl.clty_params;
      clty_variance = decl.clty_variance;
      clty_type = nondep_class_type env ids decl.clty_type;
      clty_path = decl.clty_path;
      clty_hash_type = nondep_type_decl env ids false decl.clty_hash_type ;
      clty_loc = decl.clty_loc;
      clty_attributes = decl.clty_attributes;
      clty_uid = decl.clty_uid;
    }
  in
  clear_hash ();
  decl

(* collapse conjunctive types in class parameters *)
let rec collapse_conj env visited ty =
  let id = get_id ty in
  if List.memq id visited then () else
  let visited = id :: visited in
  match get_desc ty with
    Tvariant row ->
      List.iter
        (fun (_l,fi) ->
          match row_field_repr fi with
            Reither (_c, t1::(_::_ as tl), _m) ->
              List.iter (unify env t1) tl
          | _ ->
              ())
        (row_fields row);
      iter_row (collapse_conj env visited) row
  | _ ->
      iter_type_expr (collapse_conj env visited) ty

let collapse_conj_params env params =
  List.iter (collapse_conj env []) params

let same_constr env t1 t2 =
  let t1 = expand_head env t1 in
  let t2 = expand_head env t2 in
  match get_desc t1, get_desc t2 with
  | Tconstr (p1, _, _), Tconstr (p2, _, _) -> Path.same p1 p2
  | _ -> false

let () =
  Env.same_constr := same_constr

(* For use with ocamldebug *)
type global_state =
  { current_level : int ref;
    nongen_level : int ref;
    global_level : int ref;
  }

let global_state : global_state =
  { current_level;
    nongen_level;
    global_level;
  }

let print_global_state fmt global_state =
  let print_field fmt s r = Format.fprintf fmt "%s = %d;@;" s !r in
  let print_fields fmt { current_level; nongen_level; global_level; } =
    print_field fmt "current_level" current_level;
    print_field fmt "nongen_level" nongen_level;
    print_field fmt "global_level" global_level;
  in
  Format.fprintf fmt "@[<1>{@;%a}@]" print_fields global_state
