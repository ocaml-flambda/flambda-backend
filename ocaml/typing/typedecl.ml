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

(**** Typing of type definitions ****)

open Misc
open Asttypes
open Parsetree
open Primitive
open Types
open Typetexp

module String = Misc.Stdlib.String

type native_repr_kind = Unboxed | Untagged

type jkind_sort_loc = Cstr_tuple | Record | Unboxed_record | External

(* Our static analyses explore the set of type expressions "reachable"
   from a type declaration, by expansion of definitions or by the
   subterm relation (a type expression is syntactically contained
   in another). *)
type reaching_type_path = reaching_type_step list
and reaching_type_step =
  | Expands_to of type_expr * type_expr
  | Contains of type_expr * type_expr

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string * Env.t * reaching_type_path
  | Cycle_in_def of string * Env.t * reaching_type_path
  | Definition_mismatch of type_expr * Env.t * Includecore.type_mismatch option
  | Constraint_failed of Env.t * Errortrace.unification_error
  | Inconsistent_constraint of Env.t * Errortrace.unification_error
  | Type_clash of Env.t * Errortrace.unification_error
  | Non_regular of {
      definition: Path.t;
      used_as: type_expr;
      defined_as: type_expr;
      reaching_path: reaching_type_path;
    }
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Env.t * Includecore.type_mismatch
  | Rebind_wrong_type of
      Longident.t * Env.t * Errortrace.unification_error
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Variance of Typedecl_variance.error
  | Unavailable_type_constructor of Path.t
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Jkind_mismatch_of_type of type_expr * Jkind.Violation.t
  | Jkind_mismatch_of_path of Path.t * Jkind.Violation.t
  | Jkind_mismatch_in_check_constraints of type_expr * Jkind.Violation.t
  | Jkind_sort of
      { kloc : jkind_sort_loc
      ; typ : type_expr
      ; err : Jkind.Violation.t
      }
  | Jkind_empty_record
<<<<<<< HEAD
  | Non_value_in_sig of Jkind.Violation.t * string
  | Invalid_jkind_in_block of type_expr * Jkind.Sort.const * jkind_sort_loc
||||||| parent of 114ab8b0 (Enable layout histories (#1823))
  | Non_value_in_sig of Jkind.Violation.t * string
  | Float64_in_block of type_expr * jkind_sort_loc
=======
  | Non_value_in_sig of Jkind.Violation.t * string * type_expr
  | Float64_in_block of type_expr * jkind_sort_loc
>>>>>>> 114ab8b0 (Enable layout histories (#1823))
  | Mixed_block
  | Separability of Typedecl_separability.error
  | Bad_unboxed_attribute of string
  | Boxed_and_unboxed
  | Nonrec_gadt
  | Invalid_private_row_declaration of type_expr
  | Local_not_enabled

open Typedtree

exception Error of Location.t * error

let get_unboxed_from_attributes sdecl =
  let unboxed = Builtin_attributes.has_unboxed sdecl.ptype_attributes in
  let boxed = Builtin_attributes.has_boxed sdecl.ptype_attributes in
  match boxed, unboxed with
  | true, true -> raise (Error(sdecl.ptype_loc, Boxed_and_unboxed))
  | true, false -> Some false
  | false, true -> Some true
  | false, false -> None

(* [make_params] creates sort variables - these can be defaulted away (as in
   transl_type_decl) or unified with existing sort-variable-free types (as in
   transl_with_constraint). *)
let make_params env path params =
  TyVarEnv.reset (); (* [transl_type_param] binds type variables *)
  let make_param (sty, v) =
    try
      (transl_type_param env path sty, v)
    with Already_bound ->
      raise(Error(sty.ptyp_loc, Repeated_parameter))
  in
    List.map make_param params

(* Enter all declared types in the environment as abstract types *)

let add_type ~check id decl env =
  Builtin_attributes.warning_scope ~ppwarning:false decl.type_attributes
    (fun () -> Env.add_type ~check id decl env)

(* Add a dummy type declaration to the environment, with the given arity.
   The [type_kind] is [Type_abstract], but there is a generic [type_manifest]
   for abbreviations, to allow polymorphic expansion, except if
   [abstract_abbrevs] is given along with a reason for not allowing expansion.
   This function is only used in [transl_type_decl]. *)
let enter_type ?abstract_abbrevs rec_flag env sdecl (id, uid) =
  let needed =
    match rec_flag with
    | Asttypes.Nonrecursive ->
        begin match sdecl.ptype_kind with
        | Ptype_variant scds ->
            List.iter (fun cd ->
              if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
              scds
        | _ -> ()
        end;
        Btype.is_row_name (Ident.name id)
    | Asttypes.Recursive -> true
  in
  if not needed then env else
  let arity = List.length sdecl.ptype_params in
  let path = Path.Pident id in

  (* There is some trickiness going on here with the jkind.  It expands on an
     old trick used in the manifest of [decl] below.

     Consider a declaration like:

        type t = foo list_of_values
        and foo = Bar

     When [enter_type] is called, we haven't yet analyzed anything about the
     manifests and kinds of the declarations, so it's natural to give [t] and
     [foo] jkind [Any].  But, while translating [t]'s manifest, we'll need to
     know [foo] has jkind [value], because it is used as the argument to
     [list_of_values]. And this check will occur before we've looked at [foo] at
     all.

     One can imagine solutions, like estimating the jkind based on the kind
     (tricky for unboxed) or parameterizing the type_expr translation with an
     option to not do full jkind checking in some cases and fix it up later
     (ugly).

     Instead, we build on an old trick that is used to handle constraints.
     Consider declarations like:

       type 'a t = 'a constraint 'a = ('b * 'c)

       type s = r t
       and r = int * string

     Here we face a similar problem in the context of constraints.  While
     translating [s]'s manifest (which is [r t]), we'll need to know that [t]'s
     constraint is satisfied (i.e., that [r] is a tuple).  But we don't know
     anything about [r] yet!

     The solution, in three parts:
     1) [enter_type], here, is used to construct [temp_env], an environment
        where we set the manifest of recursively defined things like [s]
        and [t] to just be a fresh type variable.
     2) [transl_declaration] checks constraints in [temp_env].  This succeeds,
        because [r]'s manifest is a variable and therefore unifies with
        ['b * 'c].
     3) After we've built the real environment with the actual manifests
        ([new_env] in [transl_type_decl]), the function [update_type] checks
        that the manifests from the old environment (here containing the
        information that [r] must be some pair to satisfy the constraint) are
        unified with the manifests from the new environment, ensuring the actual
        definitions satisfy those constraints.

     If [r] were, e.g., defined to be [int list], step 3 would fail.

     To handle the original jkind example, we piggyback off that approach - the
     jkind of the variable put in manifests here is updated when constraints
     are checked and then unified with the real manifest and checked against the
     kind. *)
  let type_jkind, type_jkind_annotation, sdecl_attributes =
    Jkind.of_type_decl_default
      ~context:(Type_declaration path)
      ~default:(Jkind.any ~why:Initial_typedecl_env)
      sdecl
  in
  let abstract_reason, type_manifest =
    match sdecl.ptype_manifest, abstract_abbrevs with
    | (None, _ | Some _, None) -> Abstract_def, Some (Ctype.newvar type_jkind)
    | Some _, Some reason -> reason, None
  in
  let type_params =
    List.map (fun (param, _) ->
        let name = get_type_param_name param in
        let jkind = get_type_param_jkind path param in
        Btype.newgenvar ?name jkind)
      sdecl.ptype_params
  in
  let decl =
    { type_params;
      type_arity = arity;
      type_kind = Type_abstract abstract_reason;
      type_jkind;
      type_jkind_annotation;
      type_private = sdecl.ptype_private;
      type_manifest;
      type_variance = Variance.unknown_signature ~injective:false ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl_attributes;
      type_unboxed_default = false;
      type_uid = uid;
    }
  in
  add_type ~check:true id decl env

(* [update_type] performs step 3 of the process described in the comment in
   [enter_type]: We unify the manifest of each type with the definition of that
   variable in [temp_env], which contains any requirements on the type implied
   by its use in other mutually defined types.

   In particular, we want to ensure that the manifest of this type has a jkind
   compatible with its uses in mutually defined types.  One subtlety is that we
   don't actually perform those jkind checks here - we use
   [Ctype.unify_delaying_jkind_checks] to record any needed jkind checks, but
   don't perform them until slightly later in [transl_type_decl].

   The reason for this delay is ill-formed, circular types.  These haven't been
   ruled out yet, and as a result jkind checking can fall into an infinite loop
   where jkind checking expands types, and these type expansions in subst
   trigger jkind checks that trigger type expansions that trigger jkind checks
   that...  These circular types are ruled out just after [update_type] in
   [transl_type_decl], and then we perform the delayed checks.
*)
let update_type temp_env env id loc =
  let path = Path.Pident id in
  let decl = Env.find_type path temp_env in
  match decl.type_manifest with None -> assert false
  | Some ty ->
      try
        Ctype.(unify_delaying_jkind_checks env
                 (newconstr path decl.type_params) ty)
      with Ctype.Unify err ->
        raise (Error(loc, Type_clash (env, err)))

(* Determine if a type's values are represented by floats at run-time. *)
(* CR layouts v2.5: Should we check for unboxed float here? Is a record with all
   unboxed floats the same as a float record?

   reisenberg: Yes. And actually a record mixing floats and unboxed floats is
   also a float-record, and should be made to work. We'll have to make sure to
   add the boxing operations in the right spot at projections, but that should
   be possible.
*)
let is_float env ty =
  match get_desc (Ctype.get_unboxed_type_approximation env ty) with
    Tconstr(p, _, _) -> Path.same p Predef.path_float
  | _ -> false

(* Determine if a type definition defines a fixed type. (PW) *)
let is_fixed_type sd =
  let rec has_row_var sty =
    match sty.ptyp_desc with
      (* CR layouts upstreaming: The Ptyp_alias case also covers the case for a
         jkind annotation, conveniently. When upstreaming jkinds, this
         function will need a case for jkind-annotation aliases. *)
      Ptyp_alias (sty, _) -> has_row_var sty
    | Ptyp_class _
    | Ptyp_object (_, Open)
    | Ptyp_variant (_, Open, _)
    | Ptyp_variant (_, Closed, Some _) -> true
    | _ -> false
  in
  match sd.ptype_manifest with
    None -> false
  | Some sty ->
      sd.ptype_kind = Ptype_abstract &&
      sd.ptype_private = Private &&
      has_row_var sty

(* Set the row variable to a fixed type in a private row type declaration.
   (e.g. [ type t = private [< `A | `B ] ] or [type u = private < .. > ])
   Require [is_fixed_type decl] as a precondition
*)
let set_private_row env loc p decl =
  let tm =
    match decl.type_manifest with
      None -> assert false
    | Some t -> Ctype.expand_head env t
  in
  let rv =
    match get_desc tm with
      Tvariant row ->
        let Row {fields; more; closed; name} = row_repr row in
        set_type_desc tm
          (Tvariant (create_row ~fields ~more ~closed ~name
                       ~fixed:(Some Fixed_private)));
        if Btype.static_row row then
          (* the syntax hinted at the existence of a row variable,
             but there is in fact no row variable to make private, e.g.
             [ type t = private [< `A > `A] ] *)
          raise (Error(loc, Invalid_private_row_declaration tm))
        else more
    | Tobject (ty, _) ->
        let r = snd (Ctype.flatten_fields ty) in
        if not (Btype.is_Tvar r) then
          (* a syntactically open object was closed by a constraint *)
          raise (Error(loc, Invalid_private_row_declaration tm));
        r
    | _ -> assert false
  in
  set_type_desc rv (Tconstr (p, decl.type_params, ref Mnil))

(* Translate one type declaration *)

let transl_global_flags loc attrs =
  let transl_global_flag loc (r : (bool,unit) result) =
    match r with
    | Ok b -> b
    | Error () -> raise(Error(loc, Local_not_enabled))
  in
  let global = transl_global_flag loc (Builtin_attributes.has_global attrs) in
  match global with
  | true -> Types.Global
  | false -> Types.Unrestricted

let transl_labels ~new_var_jkind env univars closed lbls =
  assert (lbls <> []);
  let all_labels = ref String.Set.empty in
  List.iter
    (fun {pld_name = {txt=name; loc}} ->
       if String.Set.mem name !all_labels then
         raise(Error(loc, Duplicate_label name));
       all_labels := String.Set.add name !all_labels)
    lbls;
  let mk {pld_name=name;pld_mutable=mut;pld_type=arg;pld_loc=loc;
          pld_attributes=attrs} =
    Builtin_attributes.warning_scope attrs
      (fun () ->
         let arg = Ast_helper.Typ.force_poly arg in
         let cty = transl_simple_type ~new_var_jkind env ?univars ~closed Mode.Alloc.Const.legacy arg in
         let gbl =
           match mut with
           | Mutable -> Types.Global
           | Immutable -> transl_global_flags loc attrs
         in
         {ld_id = Ident.create_local name.txt;
          ld_name = name; ld_mutable = mut; ld_global = gbl;
          ld_type = cty; ld_loc = loc; ld_attributes = attrs}
      )
  in
  let lbls = List.map mk lbls in
  let lbls' =
    List.map
      (fun ld ->
         let ty = ld.ld_type.ctyp_type in
         let ty = match get_desc ty with Tpoly(t,[]) -> t | _ -> ty in
         {Types.ld_id = ld.ld_id;
          ld_mutable = ld.ld_mutable;
          ld_global = ld.ld_global;
          ld_jkind = Jkind.any ~why:Dummy_jkind;
            (* Updated by [update_label_jkinds] *)
          ld_type = ty;
          ld_loc = ld.ld_loc;
          ld_attributes = ld.ld_attributes;
          ld_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
         }
      )
      lbls in
  lbls, lbls'

let transl_types_gf ~new_var_jkind env univars closed tyl =
  let mk arg =
    let cty = transl_simple_type ~new_var_jkind env ?univars ~closed Mode.Alloc.Const.legacy arg in
    let gf = transl_global_flags arg.ptyp_loc arg.ptyp_attributes in
    (cty, gf)
  in
  let tyl_gfl = List.map mk tyl in
  let tyl_gfl' = List.map (fun (cty, gf) -> cty.ctyp_type, gf) tyl_gfl in
  tyl_gfl, tyl_gfl'

let transl_constructor_arguments ~new_var_jkind env univars closed = function
  | Pcstr_tuple l ->
      let flds, flds' = transl_types_gf ~new_var_jkind env univars closed l in
      Types.Cstr_tuple flds',
      Cstr_tuple flds
  | Pcstr_record l ->
      let lbls, lbls' = transl_labels ~new_var_jkind env univars closed l in
      Types.Cstr_record lbls',
      Cstr_record lbls

(* Note that [make_constructor] does not fill in the [ld_jkind] field of any
   computed record types, because it's called too early in the translation of a
   type declaration to compute accurate jkinds in the presence of recursively
   defined types. It is updated later by [update_constructor_arguments_jkinds]
*)
let make_constructor
      env loc ~cstr_path ~type_path type_params (svars : _ Either.t)
      sargs sret_type =
  let tvars = match svars with
    | Left vars_only -> List.map (fun v -> v.txt, None) vars_only
    | Right vars_jkinds ->
        List.map
          (fun (v, l) ->
            v.txt,
            Option.map
              (fun annot ->
                 let const =
                    Jkind.const_of_user_written_annotation
                      ~context:(Constructor_type_parameter (cstr_path, v.txt))
                      annot
                 in
                 const, annot)
              l)
          vars_jkinds
  in
  match sret_type with
  | None ->
      let args, targs =
        transl_constructor_arguments ~new_var_jkind:Any env None true sargs
      in
        tvars, targs, None, args, None
  | Some sret_type ->
      (* if it's a generalized constructor we must first narrow and
         then widen so as to not introduce any new constraints *)
      (* narrow and widen are now invoked through wrap_type_variable_scope *)
      TyVarEnv.with_local_scope begin fun () ->
      let closed =
        match svars with
        | Left [] | Right [] -> false
        | _ -> true
      in
      let targs, tret_type, args, ret_type, _univars =
        Ctype.with_local_level_if closed begin fun () ->
          TyVarEnv.reset ();
          let univar_list =
            match svars with
            | Left vars_only -> TyVarEnv.make_poly_univars vars_only
            | Right vars_jkinds ->
              TyVarEnv.make_poly_univars_jkinds
                ~context:(fun v -> Constructor_type_parameter (cstr_path, v))
                vars_jkinds
          in
          let univars = if closed then Some univar_list else None in
          let args, targs =
            transl_constructor_arguments ~new_var_jkind:Sort env univars closed sargs
          in
          let tret_type =
            transl_simple_type ~new_var_jkind:Sort env ?univars ~closed Mode.Alloc.Const.legacy
              sret_type
          in
          let ret_type = tret_type.ctyp_type in
          (* TODO add back type_path as a parameter ? *)
          begin match get_desc ret_type with
          | Tconstr (p', _, _) when Path.same type_path p' -> ()
          | _ ->
              let trace =
                (* Expansion is not helpful here -- the restriction on GADT
                   return types is purely syntactic.  (In the worst case,
                   expansion produces gibberish.) *)
                [Ctype.unexpanded_diff
                   ~got:ret_type
                   ~expected:(Ctype.newconstr type_path type_params)]
              in
              raise (Error(sret_type.ptyp_loc,
                           Constraint_failed(
                           env, Errortrace.unification_error ~trace)))
          end;
          (targs, tret_type, args, ret_type, univar_list)
        end
        ~post: begin fun (_, _, args, ret_type, univars) ->
          Btype.iter_type_expr_cstr_args Ctype.generalize args;
          Ctype.generalize ret_type;
          let _vars = TyVarEnv.instance_poly_univars env loc univars in
          let set_level t = Ctype.enforce_current_level env t in
          Btype.iter_type_expr_cstr_args set_level args;
          set_level ret_type;
        end
      in
      tvars, targs, Some tret_type, args, Some ret_type
      end

let verify_unboxed_attr unboxed_attr sdecl =
  begin match unboxed_attr with
  | (None | Some false) -> ()
  | Some true ->
    let bad msg = raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute msg)) in
    match sdecl.ptype_kind with
    | Ptype_abstract    -> bad "it is abstract"
    | Ptype_open        -> bad "extensible variant types cannot be unboxed"
    | Ptype_record fields -> begin match fields with
        | [] -> bad "it has no fields"
        | _::_::_ -> bad "it has more than one field"
        | [{pld_mutable = Mutable}] -> bad "it is mutable"
        | [{pld_mutable = Immutable}] -> ()
      end
    | Ptype_variant constructors -> begin match constructors with
        | [] -> bad "it has no constructor"
        | (_::_::_) -> bad "it has more than one constructor"
        | [c] -> begin match c.pcd_args with
            | Pcstr_tuple [] ->
                bad "its constructor has no argument"
            | Pcstr_tuple (_::_::_) ->
                bad "its constructor has more than one argument"
            | Pcstr_tuple [_]  ->
                ()
            | Pcstr_record [] ->
                bad "its constructor has no fields"
            | Pcstr_record (_::_::_) ->
                bad "its constructor has more than one field"
            | Pcstr_record [{pld_mutable = Mutable}] ->
                bad "it is mutable"
            | Pcstr_record [{pld_mutable = Immutable}] ->
                ()
          end
      end
  end

(* Note [Default jkinds in transl_declaration]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   For every type declaration we create in transl_declaration, we must
   choose the jkind to use in the [type_jkind] field. Note that choices
   2 and 3 below consult the jkinds of other types. In the case that these
   types are declared in the same mutually recursive group, those jkinds
   will be approximations; see the comments on [enter_type].

   1. If there is a jkind annotation, use that. We might later compute a more
      precise jkind for the type (e.g. [type t : value = int] or [type t :
      value = A | B | C]); this will be updated in [update_decl_jkind] (updates
      from the kind) or [check_coherence] (updates from the manifest), which
      also ensures that the updated jkind is a subjkind of the annotated
      jkind.

   2. If there is no annotation but there is a manifest, use the jkind
      of the manifest. This gets improved in [check_coherence], after
      the manifest jkind might be more accurate.

   3. If there is no annotation and no manifest, the default jkind
      depends on the kind:

      - Abstract types: In this case, we have a fully abstract type declaration,
        like [type t]. We wish to default these to have jkind [value] for
        backward compatibility.

      - [@@unboxed] records and variants: We use [any] as the default.
        This default gets updated in [update_decl_jkind], when we can
        safely look up the jkind of the field. Recursive uses
        of the unboxed type are OK, because [update_decl_jkind] uses
        [Ctype.type_jkind], which looks through unboxed types (and thus
        the choice of [any] is not observed on recursive occurrences).

      - Other records and variants: The jkind of these depends on the jkinds
        of their fields: an enumeration variant is an [immediate], and someday
        (* CR layouts v5: today is the someday! *) we will allow records
        comprising only [void]s, which will also be [immediate].

        So we choose a default of [value], which gets updated in
        [update_decl_jkind]. This default choice does get used when updating
        the jkinds of other types that (recursively) mention the current type,
        but that's OK: the update in [update_decl_jkind] can only change a
        [value] to become [immediate], and yet that change can never affect
        the decision of whether an outer record/variant is a [value] or
        [immediate] (only choices of [void] can do that).

        (Again, any unboxed records/variants are looked through by
        [type_jkind], so a void one of those is OK.)

        It is tempting to use [any] as the default here, but that causes
        trouble around recursive occurrences in [update_decl_jkind].

      - Extensible variants: These really are [value]s, so we just use
        that as the default.

   The jkinds in type declarations are always just upper bounds, as
   we see in this example:

   {[
     type t7 = A | B | C | D of t7_void
     and t7_2 = { x : t7 } [@@unboxed]
     and t7_void [@@void]

     type t7_3 = t7_2 [@@immediate]
   ]}

   The proper jkind of [t7] is [immediate], but that's hard to know. Because
   [t7] has no jkind annotation and no manifest, it gets a default jkind
   of [value]. [t7_2] gets a default of [any]. We update [t7]'s jkind to be
   [immediate] in [update_decl_jkind]. But when updating [t7_2]'s jkind, we
   use the *original, default* jkind for [t7]: [value]. This means that the
   jkind recorded for [t7_2] is actually [value]. The program above is still
   accepted, because the jkind check in [check_coherence] uses [type_jkind],
   which looks through unboxed types. So it's all OK for users, but it's
   unfortunate that the stored jkind on [t7_2] is imprecise.

   (* CR layouts: see if we can do better here. *)
*)

let transl_declaration env sdecl (id, uid) =
  (* Bind type parameters *)
  Ctype.with_local_level begin fun () ->
  TyVarEnv.reset();
  let path = Path.Pident id in
  let tparams = make_params env path sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let cstrs = List.map
    (fun (sty, sty', loc) ->
      transl_simple_type ~new_var_jkind:Any env ~closed:false Mode.Alloc.Const.legacy sty,
      transl_simple_type ~new_var_jkind:Sort env ~closed:false Mode.Alloc.Const.legacy sty', loc)
    sdecl.ptype_cstrs
  in
  let unboxed_attr = get_unboxed_from_attributes sdecl in
  let unbox, unboxed_default =
    match sdecl.ptype_kind with
    | Ptype_variant [{pcd_args = Pcstr_tuple [_]; _}]
    | Ptype_variant [{pcd_args = Pcstr_record [{pld_mutable=Immutable; _}]; _}]
    | Ptype_record [{pld_mutable=Immutable; _}] ->
      Option.value unboxed_attr ~default:!Clflags.unboxed_types,
      Option.is_none unboxed_attr
    | _ -> false, false (* Not unboxable, mark as boxed *)
  in
  verify_unboxed_attr unboxed_attr sdecl;
  let jkind_from_annotation, jkind_annotation, sdecl_attributes =
    match Jkind.of_type_decl ~context:(Type_declaration path) sdecl with
    | Some (jkind, jkind_annotation, sdecl_attributes) ->
        Some jkind, Some jkind_annotation, sdecl_attributes
    | None -> None, None, sdecl.ptype_attributes
  in
  let (tman, man) = match sdecl.ptype_manifest with
      None -> None, None
    | Some sty ->
      let no_row = not (is_fixed_type sdecl) in
      let cty = transl_simple_type ~new_var_jkind:Any env ~closed:no_row Mode.Alloc.Const.legacy sty in
      Some cty, Some cty.ctyp_type
  in
  let any = Jkind.any ~why:Initial_typedecl_env in
  (* jkind_default is the jkind to use for now as the type_jkind when there
     is no annotation and no manifest.
     See Note [Default jkinds in transl_declaration].
  *)
  let (tkind, kind, jkind_default) =
    match sdecl.ptype_kind with
      | Ptype_abstract ->
        Ttype_abstract, Type_abstract Abstract_def, Jkind.value ~why:Default_type_jkind
      | Ptype_variant scstrs ->
        if List.exists (fun cstr -> cstr.pcd_res <> None) scstrs then begin
          match cstrs with
            [] -> ()
          | (_,_,loc)::_ ->
              Location.prerr_warning loc Warnings.Constraint_on_gadt
        end;
        let all_constrs = ref String.Set.empty in
        List.iter
          (fun {pcd_name = {txt = name}} ->
            if String.Set.mem name !all_constrs then
              raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
            all_constrs := String.Set.add name !all_constrs)
          scstrs;
        if List.length
            (List.filter (fun cd -> cd.pcd_args <> Pcstr_tuple []) scstrs)
           > (Config.max_tag + 1) then
          raise(Error(sdecl.ptype_loc, Too_many_constructors));
        let make_cstr scstr =
          let name = Ident.create_local scstr.pcd_name.txt in
          let svars, attributes =
            match Jane_syntax.Layouts.of_constructor_declaration scstr with
            | None ->
              Either.Left scstr.pcd_vars,
              scstr.pcd_attributes
            | Some (vars_jkinds, attributes) ->
              Either.Right vars_jkinds,
              attributes
          in
          let tvars, targs, tret_type, args, ret_type =
            make_constructor env scstr.pcd_loc
              ~cstr_path:(Path.Pident name) ~type_path:path params
              svars scstr.pcd_args scstr.pcd_res
          in
          let tcstr =
            { cd_id = name;
              cd_name = scstr.pcd_name;
              cd_vars = tvars;
              cd_args = targs;
              cd_res = tret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = attributes }
          in
          let cstr =
            { Types.cd_id = name;
              cd_args = args;
              cd_res = ret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = attributes;
              cd_uid = Uid.mk ~current_unit:(Env.get_unit_name ()) }
          in
            tcstr, cstr
        in
        let make_cstr scstr =
          Builtin_attributes.warning_scope scstr.pcd_attributes
            (fun () -> make_cstr scstr)
        in
        let tcstrs, cstrs = List.split (List.map make_cstr scstrs) in
        let rep, jkind =
          if unbox then
            Variant_unboxed, any
          else
            (* We mark all arg jkinds "any" here.  They are updated later,
               after the circular type checks make it safe to check jkinds. *)
            Variant_boxed (
              Array.map
                (fun cstr ->
                   match Types.(cstr.cd_args) with
                   | Cstr_tuple args ->
                     Array.make (List.length args) any
                   | Cstr_record _ -> [| any |])
                (Array.of_list cstrs)
            ),
            Jkind.value ~why:Boxed_variant
        in
          Ttype_variant tcstrs, Type_variant (cstrs, rep), jkind
      | Ptype_record lbls ->
          let lbls, lbls' = transl_labels ~new_var_jkind:Any env None true lbls in
          let rep, jkind =
            if unbox then
              Record_unboxed, any
            else (if List.for_all (fun l -> is_float env l.Types.ld_type) lbls'
            then Record_float
            else Record_boxed (Array.make (List.length lbls) any)),
                 Jkind.value ~why:Boxed_record
          in
          Ttype_record lbls, Type_record(lbls', rep), jkind
      | Ptype_open ->
        Ttype_open, Type_open, Jkind.value ~why:Extensible_variant
      in
    let jkind =
    (* - If there's an annotation, we use that. It's checked against
         a kind in [update_decl_jkind] and the manifest in [check_coherence].
         Both of those functions update the [type_jkind] field in the
         [type_declaration] as appropriate.
       - If there's no annotation but there is a manifest, just use [any].
         This will get updated to the manifest's jkind in [check_coherence].
       - If there's no annotation and no manifest, we fill in with the
         default calculated above here. It will get updated in
         [update_decl_jkind]. See Note [Default jkinds in transl_declaration].
    *)
      match jkind_from_annotation, man with
      | Some annot, _ -> annot
      | None, Some _ -> Jkind.any ~why:Initial_typedecl_env
      | None, None -> jkind_default
    in
    let arity = List.length params in
    let decl =
      { type_params = params;
        type_arity = arity;
        type_kind = kind;
        type_jkind = jkind;
        type_jkind_annotation = jkind_annotation;
        type_private = sdecl.ptype_private;
        type_manifest = man;
        type_variance = Variance.unknown_signature ~injective:false ~arity;
        type_separability = Types.Separability.default_signature ~arity;
        type_is_newtype = false;
        type_expansion_scope = Btype.lowest_level;
        type_loc = sdecl.ptype_loc;
        type_attributes = sdecl_attributes;
        type_unboxed_default = unboxed_default;
        type_uid = uid;
      } in
  (* Check constraints *)
    List.iter
      (fun (cty, cty', loc) ->
        let ty = cty.ctyp_type in
        let ty' = cty'.ctyp_type in
        try Ctype.unify env ty ty' with Ctype.Unify err ->
          raise(Error(loc, Inconsistent_constraint (env, err))))
      cstrs;
  (* Add abstract row *)
    if is_fixed_type sdecl then begin
      let p, _ =
        try Env.find_type_by_name
              (Longident.Lident(Ident.name id ^ "#row")) env
        with Not_found -> assert false
      in
      set_private_row env sdecl.ptype_loc p decl
    end;
    {
      typ_id = id;
      typ_name = sdecl.ptype_name;
      typ_params = tparams;
      typ_type = decl;
      typ_cstrs = cstrs;
      typ_loc = sdecl.ptype_loc;
      typ_manifest = tman;
      typ_kind = tkind;
      typ_private = sdecl.ptype_private;
      typ_attributes = sdecl_attributes;
      typ_jkind_annotation = Option.map snd jkind_annotation;
    }
  end

(* Generalize a type declaration *)

let generalize_decl decl =
  List.iter Ctype.generalize decl.type_params;
  Btype.iter_type_expr_kind Ctype.generalize decl.type_kind;
  begin match decl.type_manifest with
  | None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(* Check that all constraints are enforced *)

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

let rec check_constraints_rec env loc visited ty =
  if TypeSet.mem ty !visited then () else begin
  visited := TypeSet.add ty !visited;
  match get_desc ty with
  | Tconstr (path, args, _) ->
      let decl =
        try Env.find_type path env
        with Not_found ->
          raise (Error(loc, Unavailable_type_constructor path)) in
      let ty' = Ctype.newconstr path (Ctype.instance_list decl.type_params) in
      begin
        (* We don't expand the error trace because that produces types that
           *already* violate the constraints -- we need to report a problem with
           the unexpanded types, or we get errors that talk about the same type
           twice.  This is generally true for constraint errors. *)
        match Ctype.matches ~expand_error_trace:false env ty ty' with
        | Unification_failure err ->
          raise (Error(loc, Constraint_failed (env, err)))
        | Jkind_mismatch { original_jkind; inferred_jkind; ty } ->
          raise (Error(loc, Jkind_mismatch_in_check_constraints (ty,
                              (Jkind.Violation.of_ (Not_a_subjkind
                                 (original_jkind, inferred_jkind))))))
        | All_good -> ()
      end;
      List.iter (check_constraints_rec env loc visited) args
  | Tpoly (ty, tl) ->
      let _, ty = Ctype.instance_poly false tl ty in
      check_constraints_rec env loc visited ty
  | _ ->
      Btype.iter_type_expr (check_constraints_rec env loc visited) ty
  end

let check_constraints_labels env visited l pl =
  let rec get_loc name = function
      [] -> assert false
    | pld :: tl ->
        if name = pld.pld_name.txt then pld.pld_type.ptyp_loc
        else get_loc name tl
  in
  List.iter
    (fun {Types.ld_id=name; ld_type=ty} ->
       check_constraints_rec env (get_loc (Ident.name name) pl) visited ty)
    l

let check_constraints env sdecl (_, decl) =
  let visited = ref TypeSet.empty in
  List.iter2
    (fun (sty, _) ty -> check_constraints_rec env sty.ptyp_loc visited ty)
    sdecl.ptype_params decl.type_params;
  begin match decl.type_kind with
  | Type_abstract _ -> ()
  | Type_variant (l, _rep) ->
      let find_pl = function
          Ptype_variant pl -> pl
        | Ptype_record _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      let pl_index =
        let foldf acc x =
          String.Map.add x.pcd_name.txt x acc
        in
        List.fold_left foldf String.Map.empty pl
      in
      (* CR layouts v5: when we add the "mixed block restriction", we'll
         probably want to check it here. *)
      List.iter
        (fun {Types.cd_id=name; cd_args; cd_res} ->
          let {pcd_args; pcd_res; _} =
            try String.Map.find (Ident.name name) pl_index
            with Not_found -> assert false in
          begin match cd_args, pcd_args with
          | Cstr_tuple tyl, Pcstr_tuple styl ->
              List.iter2
                (fun sty (ty, _) ->
                   check_constraints_rec env sty.ptyp_loc visited ty)
                styl tyl
          | Cstr_record tyl, Pcstr_record styl ->
              check_constraints_labels env visited tyl styl
          | _ -> assert false
          end;
          match pcd_res, cd_res with
          | Some sr, Some r ->
              check_constraints_rec env sr.ptyp_loc visited r
          | _ ->
              () )
        l
  | Type_record (l, _) ->
      let find_pl = function
          Ptype_record pl -> pl
        | Ptype_variant _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      check_constraints_labels env visited l pl
  | Type_open -> ()
  end;
  begin match decl.type_manifest with
  | None -> ()
  | Some ty ->
      let sty =
        match sdecl.ptype_manifest with Some sty -> sty | _ -> assert false
      in
      check_constraints_rec env sty.ptyp_loc visited ty
  end

(*
   Check that the type expression (if present) is compatible with the kind.
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels.

   If the kind is [Type_abstract], we need to check that [type_jkind] (where
   we've stored the jkind annotation, if any) corresponds to the manifest
   (e.g., in the case where [type_jkind] is immediate, we should check the
   manifest is immediate).  It would also be nice to store the best possible
   jkind for this type in the kind, to avoid expansions later.  So, we do the
   relatively expensive thing of computing the best possible jkind for the
   manifest, checking that it's a subjkind of [type_jkind], and then replacing
   [type_jkind] with what we computed.

   CR layouts: if easy, factor out the shared backtracking logic from here
   and is_immediate.
*)
let check_coherence env loc dpath decl =
  match decl with
    { type_kind = (Type_variant _ | Type_record _| Type_open);
      type_manifest = Some ty } ->
      begin match get_desc ty with
        Tconstr(path, args, _) ->
          begin try
            let decl' = Env.find_type path env in
            let err =
              if List.length args <> List.length decl.type_params
              then Some Includecore.Arity
              else begin
                match Ctype.equal env false args decl.type_params with
                | exception Ctype.Equality err ->
                    Some (Includecore.Constraint err)
                | () ->
                    Includecore.type_declarations ~loc ~equality:true env
                      ~mark:true
                      (Path.last path)
                      decl'
                      dpath
                      (Subst.type_declaration
                         (Subst.add_type_path dpath path Subst.identity) decl)
              end
            in
            if err <> None then
              raise(Error(loc, Definition_mismatch (ty, env, err)))
            else
              decl
          with Not_found ->
            raise(Error(loc, Unavailable_type_constructor path))
          end
      | _ -> raise(Error(loc, Definition_mismatch (ty, env, None)))
      end
  | { type_kind = Type_abstract _;
      type_manifest = Some ty } ->
    let jkind' =
      if !Clflags.principal || Env.has_local_constraints env then
        (* We snapshot to keep this pure; see the mode crossing test that
           mentions snapshotting for an example. *)
        let snap = Btype.snapshot () in
        let jkind' = Ctype.type_jkind env ty in
        Btype.backtrack snap;
        jkind'
      else
        Ctype.type_jkind env ty
    in
    begin match Jkind.sub_with_history jkind' decl.type_jkind with
    | Ok jkind' -> { decl with type_jkind = jkind' }
    | Error v ->
      raise (Error (loc, Jkind_mismatch_of_type (ty,v)))
    end
  | { type_manifest = None } -> decl

let check_abbrev env sdecl (id, decl) =
  (id, check_coherence env sdecl.ptype_loc (Path.Pident id) decl)

(* Makes sure a type is representable.  Will lower "any" to "value". *)
(* CR layouts: In the places where this is used, we first call this to
   ensure a type is representable, and then call [Ctype.type_jkind] to get the
   most precise jkind.  These could be combined into some new function
   [Ctype.type_jkind_representable] that avoids duplicated work *)
(* CR layouts: Many places where [check_representable] is called in this file
   should be replaced with checks at the places where values of those types are
   constructed.  We've been conservative here in the first version. This is the
   same issue as with arrows. *)
let check_representable ~why ~allow_float env loc kloc typ =
  match Ctype.type_sort ~why env typ with
  (* CR layouts v3: This is a convenient place to rule out [float#] in
     structures for now, as it is called on all the types in declared blocks in
     kinds, and only them.  But when we have a real mixed block restriction, it
     can't be done here because we're just looking at one type.  *)
  (* CR layouts v2.5: This rules out float# in [@@unboxed] types.  No real need
     to rule that out - I just haven't had time to write tests for it yet. *)
  | Ok s -> begin
      match Jkind.Sort.get_default_value s with
      (* All calls to this are part of [update_decl_jkind], which happens after
         all the defaulting, so we don't expect this actually defaults the
         sort - we just want the [const]. *)
      | Void | Value -> ()
      | Float64 when allow_float -> ()
      (* CR layouts v2.5: If we want to hold back [float#] records from the
         maturity progression of [float64], we can add a check here. *)
      | (Float64 | Word | Bits32 | Bits64 as const) ->
        (* CR layouts v2.1: Consider changing the allow_float parameter to
           allow unboxed ints. *)
        raise (Error (loc, Invalid_jkind_in_block (typ, const, kloc)))
    end
  | Error err -> raise (Error (loc,Jkind_sort {kloc; typ; err}))

(* The [update_x_jkinds] functions infer more precise jkinds in the type kind,
   including which fields of a record are void.  This would be hard to do during
   [transl_declaration] due to mutually recursive types.
*)
(* [update_label_jkinds] additionally returns whether all the jkinds
   were void *)
let update_label_jkinds env loc lbls named =
  (* [named] is [Some jkinds] for top-level records (we will update the
     jkinds) and [None] for inlined records. *)
  (* CR layouts v5: it wouldn't be too hard to support records that are all
     void.  just needs a bit of refactoring in translcore *)
  let update =
    match named with
    | None -> fun _ _ -> ()
    | Some jkinds -> fun idx jkind -> jkinds.(idx) <- jkind
  in
  let lbls =
    List.mapi (fun idx (Types.{ld_type; ld_id; ld_loc} as lbl) ->
      check_representable ~why:(Label_declaration ld_id)
        ~allow_float:(Option.is_some named) env ld_loc Record ld_type;
      let ld_jkind = Ctype.type_jkind env ld_type in
      update idx ld_jkind;
      {lbl with ld_jkind}
    ) lbls
  in
  if List.for_all (fun l -> Jkind.is_void_defaulting l.ld_jkind) lbls then
    raise (Error (loc, Jkind_empty_record))
  else lbls, false
(* CR layouts v5: return true for a record with all voids *)

(* In addition to updated constructor arguments, returns whether
   all arguments are void, useful for detecting enumerations that
   can be [immediate]. *)
let update_constructor_arguments_jkinds env loc cd_args jkinds =
  match cd_args with
  | Types.Cstr_tuple tys ->
    List.iteri (fun idx (ty,_) ->
      check_representable ~why:(Constructor_declaration idx) ~allow_float:false
        env loc Cstr_tuple ty;
      jkinds.(idx) <- Ctype.type_jkind env ty) tys;
    cd_args, Array.for_all Jkind.is_void_defaulting jkinds
  | Types.Cstr_record lbls ->
    let lbls, all_void = update_label_jkinds env loc lbls None in
    jkinds.(0) <- Jkind.value ~why:Boxed_record;
    Types.Cstr_record lbls, all_void

(* This function updates jkind stored in kinds with more accurate jkinds.
   It is called after the circularity checks and the delayed jkind checks
   have happened, so we can fully compute jkinds of types.

   This function is an important part
   of correctness, as it also checks that the jkind computed from a kind
   is consistent (i.e. a subjkind of) any jkind annotation.
   See Note [Default jkinds in transl_declaration].
*)
let update_decl_jkind env dpath decl =
  let open struct
    (* For tracking what types appear in record blocks. *)
    type has_values = Has_values | No_values
    type has_float64s = Has_float64s | No_float64s
  end in

  (* returns updated labels, updated rep, and updated jkind *)
  let update_record_kind loc lbls rep =
    match lbls, rep with
    | [Types.{ld_type; ld_id; ld_loc} as lbl], Record_unboxed ->
      check_representable ~why:(Label_declaration ld_id) ~allow_float:false
        env ld_loc Unboxed_record ld_type;
      let ld_jkind = Ctype.type_jkind env ld_type in
      [{lbl with ld_jkind}], Record_unboxed, ld_jkind
    | _, Record_boxed jkinds ->
      let lbls, all_void = update_label_jkinds env loc lbls (Some jkinds) in
      let jkind = Jkind.for_boxed_record ~all_void in
      let has_values, has_floats =
        Array.fold_left
          (fun (values, floats) jkind ->
             match Jkind.get_default_value jkind with
             | Value | Immediate64 | Immediate -> (Has_values, floats)
             | Float64 -> (values, Has_float64s)
             | Void -> (values, floats)
             (* CR layouts v2.1: make unboxed ints work with records *)
             | Word | Bits32 | Bits64 ->
              Misc.fatal_error "Typedecl.update_record_kind: no support for unboxed ints"
             | Any -> assert false)
          (No_values, No_float64s) jkinds
      in
      let rep =
        match has_values, has_floats with
        | Has_values, Has_float64s -> raise (Error (loc, Mixed_block))
        | Has_values, No_float64s -> rep
        | No_values, Has_float64s -> Record_ufloat
        | No_values, No_float64s ->
          Misc.fatal_error "Typedecl.update_record_kind: empty record"
      in
      lbls, rep, jkind
    | _, Record_float ->
      (* CR layouts v2.5: When we have an unboxed float jkind, does it make
         sense to use that here?  The use of value feels inaccurate, but I think
         the code that would look at first looks at the rep. *)
      let lbls =
        List.map (fun lbl ->
          { lbl with ld_jkind = Jkind.value ~why:Float_record_field })
          lbls
      in
      lbls, rep, Jkind.value ~why:Boxed_record
    | (([] | (_ :: _)), Record_unboxed
      | _, (Record_inlined _ | Record_ufloat)) -> assert false
  in

  (* returns updated constructors, updated rep, and updated jkind *)
  let update_variant_kind cstrs rep =
    (* CR layouts: factor out duplication *)
    match cstrs, rep with
    | [{Types.cd_args;cd_loc} as cstr], Variant_unboxed -> begin
        match cd_args with
        | Cstr_tuple [ty,_] -> begin
            check_representable ~why:(Constructor_declaration 0)
              ~allow_float:false env cd_loc Cstr_tuple ty;
            let jkind = Ctype.type_jkind env ty in
            cstrs, Variant_unboxed, jkind
          end
        | Cstr_record [{ld_type; ld_id; ld_loc} as lbl] -> begin
            check_representable ~why:(Label_declaration ld_id)
              ~allow_float:false env ld_loc Record ld_type;
            let ld_jkind = Ctype.type_jkind env ld_type in
            [{ cstr with Types.cd_args =
                           Cstr_record [{ lbl with ld_jkind }] }],
            Variant_unboxed, ld_jkind
          end
        | (Cstr_tuple ([] | _ :: _ :: _) | Cstr_record ([] | _ :: _ :: _)) ->
          assert false
      end
    | cstrs, Variant_boxed jkinds ->
      let (_,cstrs,all_voids) =
        List.fold_left (fun (idx,cstrs,all_voids) cstr ->
          let cd_args, all_void =
            update_constructor_arguments_jkinds env cstr.Types.cd_loc
              cstr.Types.cd_args jkinds.(idx)
          in
          let cstr = { cstr with Types.cd_args } in
          (idx+1,cstr::cstrs,all_voids && all_void)
        ) (0,[],true) cstrs
      in
      let jkind = Jkind.for_boxed_variant ~all_voids in
      List.rev cstrs, rep, jkind
    | (([] | (_ :: _)), Variant_unboxed | _, Variant_extensible) ->
      assert false
  in

  let new_decl, new_jkind = match decl.type_kind with
    | Type_abstract _ -> decl, decl.type_jkind
    | Type_open ->
      let type_jkind = Jkind.value ~why:Extensible_variant in
      { decl with type_jkind }, type_jkind
    | Type_record (lbls, rep) ->
      let lbls, rep, type_jkind = update_record_kind decl.type_loc lbls rep in
      { decl with type_kind = Type_record (lbls, rep); type_jkind },
      type_jkind
    | Type_variant (cstrs, rep) ->
      let cstrs, rep, type_jkind = update_variant_kind cstrs rep in
      { decl with type_kind = Type_variant (cstrs, rep); type_jkind },
      type_jkind
  in

  (* check that the jkind computed from the kind matches the jkind
     annotation, which was stored in decl.type_jkind *)
  if new_jkind != decl.type_jkind then
    begin match Jkind.sub new_jkind decl.type_jkind with
    | Ok () -> ()
    | Error err ->
      raise(Error(decl.type_loc, Jkind_mismatch_of_path (dpath,err)))
    end;
  new_decl

let update_decls_jkind_reason decls =
  List.map
    (fun (id, decl) ->
       let reason = Jkind.(Generalized (Some id, decl.type_loc)) in
       let update_generalized ty = Ctype.update_generalized_ty_jkind_reason ty reason in
       List.iter update_generalized decl.type_params;
       Btype.iter_type_expr_kind update_generalized decl.type_kind;
       Option.iter update_generalized decl.type_manifest;
       let new_decl = {decl with type_jkind =
                                   Jkind.(update_reason decl.type_jkind reason)} in
       (id, new_decl)
    )
    decls

let update_decls_jkind env decls =
  List.map
    (fun (id, decl) -> (id, update_decl_jkind env (Pident id) decl))
    decls


(* Note: Well-foundedness for OCaml types

   We want to guarantee that all cycles within OCaml types are
   "guarded".

   More precisly, we consider a reachability relation
     "[t] is reachable [guarded|unguarded] from [u]"
   defined as follows:

   - [t1, t2...] are reachable guarded from object types
       [< m1 : t1; m2 : t2; ... >]
     or polymorphic variants
       [[`A of t1 | `B of t2 | ...]].

   - [t1, t2...] are reachable rectypes-guarded from
     [t1 -> t2], [t1 * t2 * ...], and all other built-in
     contractive type constructors.

     (By rectypes-guarded we mean: guarded if -rectypes is set,
      unguarded if it is not set.)

   - If [(t1, t2...) c] is a datatype (variant or record),
     then [t1, t2...] are reachable rectypes-guarded from it.

   - If [(t1, t2...) c] is an abstract type,
     then [t1, t2...] are reachable unguarded from it.

   - If [(t1, t2...) c] is an (expandable) abbreviation,
     then its expansion is reachable unguarded from it.
     Note that we do not define [t1, t2...] as reachable.

   - The relation is transitive and guardedness of a composition
     is the disjunction of each guardedness:
     if t1 is reachable from t2 and t2 is reachable from t3;
     then t1 is reachable guarded from t3 if t1 is guarded in t2
     or t2 is guarded in t3, and reachable unguarded otherwise.

   A type [t] is not well-founded if and only if [t] is reachable
   unguarded in [t].

   Notice that, in the case of datatypes, the arguments of
   a parametrized datatype are reachable (they must not contain
   recursive occurrences of the type), but the definition of the
   datatype is not defined as reachable.

      (* well-founded *)
      type t = Foo of u
      and u = t

      (* ill-founded *)
      type 'a t = Foo of 'a
      and u = u t
      > Error: The type abbreviation u is cyclic

   Indeed, in the second example [u] is reachable unguarded in [u t]
   -- its own definition.
*)

(* Note: Forms of ill-foundedness

   Several OCaml language constructs could introduce ill-founded
   types, and there are several distinct checks that forbid different
   sources of ill-foundedness.

   1. Type aliases.

      (* well-founded *)
      type t = < x : 'a > as 'a

      (* ill-founded, unless -rectypes is used *)
      type t = (int * 'a) as 'a
      > Error: This alias is bound to type int * 'a
      > but is used as an instance of type 'a
      > The type variable 'a occurs inside int * 'a

      Ill-foundedness coming from type aliases is detected by the "occur check"
      used by our type unification algorithm. See typetexp.ml.

   2. Type abbreviations.

      (* well-founded *)
      type t = < x : t >

      (* ill-founded, unless -rectypes is used *)
      type t = (int * t)
      > Error: The type abbreviation t is cyclic

      Ill-foundedness coming from type abbreviations is detected by
      [check_well_founded] below.

  3. Recursive modules.

     (* well-founded *)
     module rec M : sig type t = < x : M.t > end = M

     (* ill-founded, unless -rectypes is used *)
     module rec M : sig type t = int * M.t end = M
     > Error: The definition of M.t contains a cycle:
     >        int * M.t

     This is also checked by [check_well_founded] below,
     as called from [check_recmod_typedecl].

  4. Functor application

     A special case of (3) is that a type can be abstract
     in a functor definition, and be instantiated with
     an abbreviation in an application of the functor.
     This can introduce ill-foundedness, so functor applications
     must be checked by re-checking the type declarations of their result.

     module type T = sig type t end
     module Fix(F:(T -> T)) = struct
       (* this recursive definition is well-founded
          as F(Fixed).t contains no reachable type expression. *)
       module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
     end

     (* well-founded *)
     Module M = Fix(functor (M:T) -> struct type t = < x : M.t > end)

     (* ill-founded *)
     module M = Fix(functor (M:T) -> struct type t = int * M.t end);;
     > Error: In the signature of this functor application:
     >   The definition of Fixed.t contains a cycle:
     >   F(Fixed).t
*)

(* Check that a type expression is well-founded:
   - if -rectypes is used, we must prevent non-contractive fixpoints
     ('a as 'a)
   - if -rectypes is not used, we only allow cycles in the type graph
     if they go through an object or polymorphic variant type *)

let check_well_founded ~abs_env env loc path to_check visited ty0 =
  let rec check parents trace ty =
    if TypeSet.mem ty parents then begin
      (*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*)
      let err =
        let reaching_path, rec_abbrev =
          (* The reaching trace is accumulated in reverse order, we
             reverse it to get a reaching path. *)
          match trace with
          | [] -> assert false
          | Expands_to (ty1, _) :: trace when (match get_desc ty1 with
              Tconstr (p,_,_) -> Path.same p path | _ -> false) ->
                List.rev trace, true
          | trace -> List.rev trace, false
        in
        if rec_abbrev
        then Recursive_abbrev (Path.name path, abs_env, reaching_path)
        else Cycle_in_def (Path.name path, abs_env, reaching_path)
      in raise (Error (loc, err))
    end;
    let (fini, parents) =
      try
        (* Map each node to the set of its already checked parents *)
        let prev = TypeMap.find ty !visited in
        if TypeSet.subset parents prev then (true, parents) else
        let parents = TypeSet.union parents prev in
        visited := TypeMap.add ty parents !visited;
        (false, parents)
      with Not_found ->
        visited := TypeMap.add ty parents !visited;
        (false, parents)
    in
    if fini then () else
    let rec_ok =
      match get_desc ty with
      | Tconstr(p,_,_) ->
          !Clflags.recursive_types && Ctype.is_contractive env p
      | Tobject _ | Tvariant _ -> true
      | _ -> !Clflags.recursive_types
    in
    if rec_ok then () else
    let parents = TypeSet.add ty parents in
    match get_desc ty with
    | Tconstr(p, tyl, _) ->
        let to_check = to_check p in
        if to_check then List.iter (check_subtype parents trace ty) tyl;
        begin match Ctype.try_expand_once_opt env ty with
        | ty' -> check parents (Expands_to (ty, ty') :: trace) ty'
        | exception Ctype.Cannot_expand ->
            if not to_check then List.iter (check_subtype parents trace ty) tyl
        end
    | _ ->
        Btype.iter_type_expr (check_subtype parents trace ty) ty
  and check_subtype parents trace outer_ty inner_ty =
      check parents (Contains (outer_ty, inner_ty) :: trace) inner_ty
  in
  let snap = Btype.snapshot () in
  try Ctype.wrap_trace_gadt_instances env (check TypeSet.empty []) ty0
  with Ctype.Escape _ ->
    (* Will be detected by check_regularity *)
    Btype.backtrack snap

let check_well_founded_manifest ~abs_env env loc path decl =
  if decl.type_manifest = None then () else
  let args =
    (* The jkinds here shouldn't matter for the purposes of
       [check_well_founded] *)
    List.map (fun _ -> Ctype.newvar (Jkind.any ~why:Dummy_jkind))
      decl.type_params
  in
  let visited = ref TypeMap.empty in
  check_well_founded ~abs_env env loc path (Path.same path) visited
    (Ctype.newconstr path args)

(* Given a new type declaration [type t = ...] (potentially mutually-recursive),
   we check that accepting the declaration does not introduce ill-founded types.

   Note: we check that the types at the toplevel of the declaration
   are not reachable unguarded from themselves, that is, we check that
   there is no cycle going through the "root" of the declaration. But
   we *also* check that all the type sub-expressions reachable from
   the root even those that are guarded, are themselves
   well-founded. (So we check the absence of cycles, even for cycles
   going through inner type subexpressions but not the root.

   We are not actually sure that this "deep check" is necessary
   (we don't have an example at hand where it is necessary), but we
   are doing it anyway out of caution.
*)
let check_well_founded_decl ~abs_env env loc path decl to_check =
  let open Btype in
  (* We iterate on all subexpressions of the declaration to check
     "in depth" that no ill-founded type exists. *)
  let it =
    let checked =
      (* [checked] remembers the types that the iterator already
         checked, to avoid looping on cyclic types. *)
      ref TypeSet.empty in
    let visited =
      (* [visited] remembers the inner visits performed by
         [check_well_founded] on each type expression reachable from
         this declaration. This avoids unnecessary duplication of
         [check_well_founded] work when invoked on two parts of the
         type declaration that have common subexpressions. *)
      ref TypeMap.empty in
    {type_iterators with it_type_expr =
     (fun self ty ->
       if TypeSet.mem ty !checked then () else begin
         check_well_founded ~abs_env env loc path to_check visited ty;
         checked := TypeSet.add ty !checked;
         self.it_do_type_expr self ty
       end)} in
  it.it_type_declaration it (Ctype.generic_instance_declaration decl)

(* Check for non-regular abbreviations; an abbreviation
   [type 'a t = ...] is non-regular if the expansion of [...]
   contains instances [ty t] where [ty] is not equal to ['a].

   Note: in the case of a constrained type definition
   [type 'a t = ... constraint 'a = ...], we require
   that all instances in [...] be equal to the constrainted type.
*)
let check_regularity ~abs_env env loc path decl to_check =
  (* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. *)

  if decl.type_params = [] then () else

  let visited = ref TypeSet.empty in

  let rec check_regular cpath args prev_exp trace ty =
    if not (TypeSet.mem ty !visited) then begin
      visited := TypeSet.add ty !visited;
      match get_desc ty with
      | Tconstr(path', args', _) ->
          if Path.same path path' then begin
            if not (Ctype.is_equal abs_env false args args') then
              raise (Error(loc,
                     Non_regular {
                       definition=path;
                       used_as=ty;
                       defined_as=Ctype.newconstr path args;
                       reaching_path=List.rev trace;
                     }))
          end
          (* Attempt to expand a type abbreviation if:
              1- [to_check path'] holds
                 (otherwise the expansion cannot involve [path]);
              2- we haven't expanded this type constructor before
                 (otherwise we could loop if [path'] is itself
                 a non-regular abbreviation). *)
          else if to_check path' && not (List.mem path' prev_exp) then begin
            try
              (* Attempt expansion *)
              let (params0, body0, _) = Env.find_type_expansion path' env in
              let (params, body) =
                Ctype.instance_parameterized_type params0 body0 in
              begin
                try List.iter2 (Ctype.unify abs_env) params args'
                with Ctype.Unify err ->
                  raise (Error(loc, Constraint_failed (abs_env, err)));
              end;
              check_regular path' args
                (path' :: prev_exp) (Expands_to (ty,body) :: trace)
                body
            with Not_found -> ()
          end;
          List.iter (check_subtype cpath args prev_exp trace ty) args'
      | Tpoly (ty, tl) ->
          let (_, ty) = Ctype.instance_poly ~keep_names:true false tl ty in
          check_regular cpath args prev_exp trace ty
      | _ ->
          Btype.iter_type_expr
            (check_subtype cpath args prev_exp trace ty) ty
    end
    and check_subtype cpath args prev_exp trace outer_ty inner_ty =
      let trace = Contains (outer_ty, inner_ty) :: trace in
      check_regular cpath args prev_exp trace inner_ty
  in

  Option.iter
    (fun body ->
      let (args, body) =
        Ctype.instance_parameterized_type
          ~keep_names:true decl.type_params body in
      List.iter (check_regular path args [] []) args;
      check_regular path args [] [] body)
    decl.type_manifest

let check_abbrev_regularity ~abs_env env id_loc_list to_check tdecl =
  let decl = tdecl.typ_type in
  let id = tdecl.typ_id in
  check_regularity ~abs_env env (List.assoc id id_loc_list) (Path.Pident id)
    decl to_check

let check_duplicates sdecl_list =
  let labels = Hashtbl.create 7 and constrs = Hashtbl.create 7 in
  List.iter
    (fun sdecl -> match sdecl.ptype_kind with
      Ptype_variant cl ->
        List.iter
          (fun pcd ->
            try
              let name' = Hashtbl.find constrs pcd.pcd_name.txt in
              Location.prerr_warning pcd.pcd_loc
                (Warnings.Duplicate_definitions
                   ("constructor", pcd.pcd_name.txt, name',
                    sdecl.ptype_name.txt))
            with Not_found ->
              Hashtbl.add constrs pcd.pcd_name.txt sdecl.ptype_name.txt)
          cl
    | Ptype_record fl ->
        List.iter
          (fun {pld_name=cname;pld_loc=loc} ->
            try
              let name' = Hashtbl.find labels cname.txt in
              Location.prerr_warning loc
                (Warnings.Duplicate_definitions
                   ("label", cname.txt, name', sdecl.ptype_name.txt))
            with Not_found -> Hashtbl.add labels cname.txt sdecl.ptype_name.txt)
          fl
    | Ptype_abstract -> ()
    | Ptype_open -> ())
    sdecl_list

(* Force recursion to go through id for private types*)
let name_recursion sdecl id decl =
  match decl with
  | { type_kind = Type_abstract Abstract_def;
      type_manifest = Some ty;
      type_private = Private; } when is_fixed_type sdecl ->
    let ty' = newty2 ~level:(get_level ty) (get_desc ty) in
    if Ctype.deep_occur ty ty' then
      let td = Tconstr(Path.Pident id, decl.type_params, ref Mnil) in
      link_type ty (newty2 ~level:(get_level ty) td);
      {decl with type_manifest = Some ty'}
    else decl
  | _ -> decl

let name_recursion_decls sdecls decls =
  List.map2 (fun sdecl (id, decl) -> (id, name_recursion sdecl id decl))
    sdecls decls

(* Warn on definitions of type "type foo = ()" which redefine a different unit
   type and are likely a mistake. *)
let check_redefined_unit (td: Parsetree.type_declaration) =
  let open Parsetree in
  let is_unit_constructor cd = cd.pcd_name.txt = "()" in
  match td with
  | { ptype_name = { txt = name };
      ptype_manifest = None;
      ptype_kind = Ptype_variant [ cd ] }
    when is_unit_constructor cd ->
      Location.prerr_warning td.ptype_loc (Warnings.Redefining_unit name)
  | _ ->
      ()

let add_types_to_env decls env =
  List.fold_right
    (fun (id, decl) env -> add_type ~check:true id decl env)
    decls env

(* Translate a set of type declarations, mutually recursive or not *)
let transl_type_decl env rec_flag sdecl_list =
  List.iter check_redefined_unit sdecl_list;
  (* Add dummy types for fixed rows *)
  let fixed_types = List.filter is_fixed_type sdecl_list in
  let sdecl_list =
    List.map
      (fun sdecl ->
         let ptype_name =
           let loc = Location.ghostify sdecl.ptype_name.loc in
           mkloc (sdecl.ptype_name.txt ^"#row") loc
         in
         let ptype_kind = Ptype_abstract in
         let ptype_manifest = None in
         let ptype_loc = Location.ghostify sdecl.ptype_loc in
        {sdecl with
           ptype_name; ptype_kind; ptype_manifest; ptype_loc })
      fixed_types
    @ sdecl_list
  in

  (* Create identifiers. *)
  let scope = Ctype.create_scope () in
  let ids_list =
    List.map (fun sdecl ->
      Ident.create_scoped ~scope sdecl.ptype_name.txt,
      Uid.mk ~current_unit:(Env.get_unit_name ())
    ) sdecl_list
  in
  let tdecls, decls, new_env, delayed_jkind_checks =
    Ctype.with_local_level_iter ~post:generalize_decl begin fun () ->
      (* Enter types. *)
      let temp_env =
        List.fold_left2 (enter_type rec_flag) env sdecl_list ids_list in
      (* Translate each declaration. *)
      let current_slot = ref None in
      let warn_unused =
        Warnings.is_active (Warnings.Unused_type_declaration "") in
      let ids_slots (id, _uid as ids) =
        match rec_flag with
        | Asttypes.Recursive when warn_unused ->
            (* See typecore.ml for a description of the algorithm used to
               detect unused declarations in a set of recursive definitions. *)
            let slot = ref [] in
            let td = Env.find_type (Path.Pident id) temp_env in
            Env.set_type_used_callback
              td
              (fun old_callback ->
                match !current_slot with
                | Some slot -> slot := td.type_uid :: !slot
                | None ->
                    List.iter Env.mark_type_used (get_ref slot);
                    old_callback ()
              );
            ids, Some slot
        | Asttypes.Recursive | Asttypes.Nonrecursive ->
            ids, None
      in
      let transl_declaration name_sdecl (id, slot) =
        current_slot := slot;
        Builtin_attributes.warning_scope
          name_sdecl.ptype_attributes
          (fun () -> transl_declaration temp_env name_sdecl id)
      in
      (* Translate declarations, using a temporary environment where
         abbreviations expand to a generic type variable. After that, we check
         the coherence of the translated declarations in the resulting new
         enviroment. *)
      let tdecls =
        List.map2 transl_declaration sdecl_list (List.map ids_slots ids_list) in
      let decls =
        List.map (fun tdecl -> (tdecl.typ_id, tdecl.typ_type)) tdecls in
      current_slot := None;
      (* Check for duplicates *)
      check_duplicates sdecl_list;
      (* Build the final env. *)
      let new_env = add_types_to_env decls env in
      (* Update stubs *)
      let delayed_jkind_checks =
        match rec_flag with
        | Asttypes.Nonrecursive -> []
        | Asttypes.Recursive ->
          List.map2
            (fun (id, _) sdecl ->
               update_type temp_env new_env id sdecl.ptype_loc,
               sdecl.ptype_loc)
            ids_list sdecl_list
      in
      ((tdecls, decls, new_env, delayed_jkind_checks), List.map snd decls)
    end
  in
  (* Check for ill-formed abbrevs *)
  let id_loc_list =
    List.map2 (fun (id, _) sdecl -> (id, sdecl.ptype_loc))
      ids_list sdecl_list
  in
  (* [check_abbrev_regularity] cannot use the new environment, as this might
     result in non-termination. Instead we use a completely abstract version
     of the temporary environment, giving a reason for why abbreviations
     cannot be expanded (#12334, #12368) *)
  let abs_env =
    List.fold_left2
      (enter_type ~abstract_abbrevs:Abstract_rec_check_regularity rec_flag)
      env sdecl_list ids_list in
  List.iter (fun (id, decl) ->
    check_well_founded_manifest ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id) decl)
    decls;
  let to_check =
    function Path.Pident id -> List.mem_assoc id id_loc_list | _ -> false in
  List.iter (fun (id, decl) ->
    check_well_founded_decl ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id)
      decl to_check)
    decls;
  List.iter
    (check_abbrev_regularity ~abs_env new_env id_loc_list to_check) tdecls;
  (* Now that we've ruled out ill-formed types, we can perform the delayed
     jkind checks *)
  List.iter (fun (checks,loc) ->
    List.iter (fun (ty,jkind) ->
      match Ctype.constrain_type_jkind new_env ty jkind with
      | Ok _ -> ()
      | Error err ->
        let err = Errortrace.unification_error ~trace:[Bad_jkind (ty,err)] in
        raise (Error (loc, Type_clash (new_env, err))))
      checks)
    delayed_jkind_checks;
  (* Check that all type variables are closed; this also defaults any remaining
     sort variables. Defaulting must happen before update_decls_jkind,
     Typedecl_seperability.update_decls, and add_types_to_env, all of which need
     to check whether parts of the type are void (and currently use Jkind.equate
     to do this which would set any remaining sort variables to void). It also
     must happen before check_constraints, so that check_constraints can detect
     when a jkind is inferred incorrectly.  (The unification that
     check_constraints does is undone via backtracking, and thus forgetting to
     do the defaulting first is actually unsound: the unification in
     check_constraints will succeed via mutation, be backtracked, and then
     perhaps a sort variable gets defaulted to value. Bad bad.) *)
  List.iter2
    (fun sdecl tdecl ->
      let decl = tdecl.typ_type in
       match Ctype.closed_type_decl decl with
         Some ty -> raise(Error(sdecl.ptype_loc, Unbound_type_var(ty,decl)))
       | None   -> ())
    sdecl_list tdecls;
  (* Check that constraints are enforced *)
  List.iter2 (check_constraints new_env) sdecl_list decls;
  (* Add type properties to declarations *)
  let decls =
    try
      decls
      |> name_recursion_decls sdecl_list
      |> Typedecl_variance.update_decls env sdecl_list
      |> Typedecl_separability.update_decls env
      |> update_decls_jkind new_env
      |> update_decls_jkind_reason
    with
    | Typedecl_variance.Error (loc, err) ->
        raise (Error (loc, Variance err))
    | Typedecl_separability.Error (loc, err) ->
        raise (Error (loc, Separability err))
  in
  (* Check re-exportation, updating [type_jkind] from the manifest *)
  let decls = List.map2 (check_abbrev new_env) sdecl_list decls in
  (* Compute the final environment with variance and immediacy *)
  let final_env = add_types_to_env decls env in
  (* Keep original declaration *)
  let final_decls =
    List.map2
      (fun tdecl (_id2, decl) ->
        { tdecl with typ_type = decl }
      ) tdecls decls
  in
  (* Done *)
  (final_decls, final_env)

(* Translating type extensions *)
let transl_extension_constructor_decl
      env type_path typext_params loc id svars sargs sret_type =
  let tvars, targs, tret_type, args, ret_type =
    make_constructor env loc
      ~cstr_path:(Pident id) ~type_path typext_params
      svars sargs sret_type
  in
  let num_args =
    match targs with
    | Cstr_tuple args -> List.length args
    | Cstr_record _ -> 1
  in
  let jkinds = Array.make num_args (Jkind.any ~why:Dummy_jkind) in
  let args, constant =
    update_constructor_arguments_jkinds env loc args jkinds
  in
  args, jkinds, constant, ret_type,
  Text_decl(tvars, targs, tret_type)

let transl_extension_constructor_jst env type_path _type_params
      typext_params _priv loc id _attrs :
  Jane_syntax.Extension_constructor.t -> _ = function
  | Jext_layout (Lext_decl(vars_jkinds, args, res)) ->
    transl_extension_constructor_decl
      env type_path typext_params loc id (Right vars_jkinds) args res

let transl_extension_constructor ~scope env type_path type_params
                                 typext_params priv sext =
  let id = Ident.create_scoped ~scope sext.pext_name.txt in
  let loc = sext.pext_loc in
  let args, arg_jkinds, constant, ret_type, kind =
    match Jane_syntax.Extension_constructor.of_ast sext with
    | Some (jext, attrs) ->
      transl_extension_constructor_jst
        env type_path type_params typext_params priv loc id attrs jext
    | None ->
    match sext.pext_kind with
      Pext_decl(svars, sargs, sret_type) ->
      transl_extension_constructor_decl
        env type_path typext_params loc id (Left svars) sargs sret_type
    | Pext_rebind lid ->
        let usage : Env.constructor_usage =
          if priv = Public then Env.Exported else Env.Exported_private
        in
        let cdescr = Env.lookup_constructor ~loc:lid.loc usage lid.txt env in
        let (args, cstr_res, _ex) =
          Ctype.instance_constructor Keep_existentials_flexible cdescr
        in
        let res, ret_type =
          if cdescr.cstr_generalized then
            let params = Ctype.instance_list type_params in
            let res = Ctype.newconstr type_path params in
            let ret_type = Some (Ctype.newconstr type_path params) in
              res, ret_type
          else (Ctype.newconstr type_path typext_params), None
        in
        begin
          try
            Ctype.unify env cstr_res res
          with Ctype.Unify err ->
            raise (Error(lid.loc,
                     Rebind_wrong_type(lid.txt, env, err)))
        end;
        (* Remove "_" names from parameters used in the constructor *)
        if not cdescr.cstr_generalized then begin
          let vars =
            Ctype.free_variables
              (Btype.newgenty (Ttuple (List.map (fun (t,_) -> None, t) args)))
          in
          List.iter
            (fun ty ->
              match get_desc ty with
              | Tvar { name = Some "_"; jkind }
                when List.exists (eq_type ty) vars ->
                set_type_desc ty (Tvar { name = None; jkind })
              | _ -> ())
            typext_params
        end;
        (* Ensure that constructor's type matches the type being extended *)
        let cstr_type_path = Btype.cstr_type_path cdescr in
        let cstr_type_params = (Env.find_type cstr_type_path env).type_params in
        let cstr_types =
          (Btype.newgenty
             (Tconstr(cstr_type_path, cstr_type_params, ref Mnil)))
          :: cstr_type_params
        in
        let ext_types =
          (Btype.newgenty
             (Tconstr(type_path, type_params, ref Mnil)))
          :: type_params
        in
        if not (Ctype.is_equal env true cstr_types ext_types) then
          raise (Error(lid.loc,
                       Rebind_mismatch(lid.txt, cstr_type_path, type_path)));
        (* Disallow rebinding private constructors to non-private *)
        begin
          match cdescr.cstr_private, priv with
            Private, Public ->
              raise (Error(lid.loc, Rebind_private lid.txt))
          | _ -> ()
        end;
        let path =
          match cdescr.cstr_tag with
            Extension (path,_) -> path
          | _ -> assert false
        in
        let args =
          match cdescr.cstr_inlined with
          | None ->
              Types.Cstr_tuple args
          | Some decl ->
              let tl =
                match List.map (fun (ty, _) -> get_desc ty) args with
                | [ Tconstr(_, tl, _) ] -> tl
                | _ -> assert false
              in
              let decl = Ctype.instance_declaration decl in
              assert (List.length decl.type_params = List.length tl);
              List.iter2 (Ctype.unify env) decl.type_params tl;
              let lbls =
                match decl.type_kind with
                | Type_record (lbls, Record_inlined _) -> lbls
                | _ -> assert false
              in
              Types.Cstr_record lbls
        in
        args, cdescr.cstr_arg_jkinds, cdescr.cstr_constant, ret_type,
        Text_rebind(path, lid)
  in
  let ext =
    { ext_type_path = type_path;
      ext_type_params = typext_params;
      ext_args = args;
      ext_arg_jkinds = arg_jkinds;
      ext_constant = constant;
      ext_ret_type = ret_type;
      ext_private = priv;
      Types.ext_loc = sext.pext_loc;
      Types.ext_attributes = sext.pext_attributes;
      ext_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    }
  in
    { ext_id = id;
      ext_name = sext.pext_name;
      ext_type = ext;
      ext_kind = kind;
      Typedtree.ext_loc = sext.pext_loc;
      Typedtree.ext_attributes = sext.pext_attributes; }

let transl_extension_constructor ~scope env type_path type_params
    typext_params priv sext =
  Builtin_attributes.warning_scope sext.pext_attributes
    (fun () -> transl_extension_constructor ~scope env type_path type_params
        typext_params priv sext)

let is_rebind ext =
  match ext.ext_kind with
  | Text_rebind _ -> true
  | Text_decl _ -> false

let transl_type_extension extend env loc styext =
  let type_path, type_decl =
    let lid = styext.ptyext_path in
    Env.lookup_type ~loc:lid.loc lid.txt env
  in
  begin
    match type_decl.type_kind with
    | Type_open -> begin
        match type_decl.type_private with
        | Private when extend -> begin
            match
              List.find
                (function {pext_kind = Pext_decl _} -> true
                        | {pext_kind = Pext_rebind _} -> false)
                styext.ptyext_constructors
            with
            | {pext_loc} ->
                raise (Error(pext_loc, Cannot_extend_private_type type_path))
            | exception Not_found -> ()
          end
        | _ -> ()
      end
    | _ ->
        raise (Error(loc, Not_extensible_type type_path))
  end;
  let type_variance =
    List.map (fun v ->
                let (co, cn) = Variance.get_upper v in
                  (not cn, not co, false))
             type_decl.type_variance
  in
  let err =
    if type_decl.type_arity <> List.length styext.ptyext_params then
      Some Includecore.Arity
    else
      if List.for_all2
           (fun (c1, n1, _) (c2, n2, _) -> (not c2 || c1) && (not n2 || n1))
           type_variance
           (Typedecl_variance.variance_of_params styext.ptyext_params)
      then None else Some Includecore.Variance
  in
  begin match err with
  | None -> ()
  | Some err -> raise (Error(loc, Extension_mismatch (type_path, env, err)))
  end;
  let ttype_params, _type_params, constructors =
    (* Note: it would be incorrect to call [create_scope] *after*
       [TyVarEnv.reset] or after [with_local_level] (see #10010). *)
    let scope = Ctype.create_scope () in
    Ctype.with_local_level begin fun () ->
      TyVarEnv.reset();
      let ttype_params = make_params env type_path styext.ptyext_params in
      let type_params = List.map (fun (cty, _) -> cty.ctyp_type) ttype_params in
      List.iter2 (Ctype.unify_var env)
        (Ctype.instance_list type_decl.type_params)
        type_params;
      let constructors =
        List.map (transl_extension_constructor ~scope env type_path
                    type_decl.type_params type_params styext.ptyext_private)
          styext.ptyext_constructors
      in
      (ttype_params, type_params, constructors)
    end
    ~post: begin fun (_, type_params, constructors) ->
      (* Generalize types *)
      List.iter Ctype.generalize type_params;
      List.iter
        (fun ext ->
          Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
          Option.iter Ctype.generalize ext.ext_type.ext_ret_type)
        constructors;
    end
  in
  (* Check that all type variables are closed *)
  List.iter
    (fun ext ->
       match Ctype.closed_extension_constructor ext.ext_type with
         Some ty ->
           raise(Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
       | None -> ())
    constructors;
  (* Check variances are correct *)
  List.iter
    (fun ext->
       (* Note that [loc] here is distinct from [type_decl.type_loc], which
          makes the [loc] parameter to this function useful. [loc] is the
          location of the extension, while [type_decl] points to the original
          type declaration being extended. *)
       try Typedecl_variance.check_variance_extension
             env type_decl ext (type_variance, loc)
       with Typedecl_variance.Error (loc, err) ->
         raise (Error (loc, Variance err)))
    constructors;
  (* Add extension constructors to the environment *)
  let newenv =
    List.fold_left
      (fun env ext ->
         let rebind = is_rebind ext in
         Env.add_extension ~check:true ~rebind ext.ext_id ext.ext_type env)
      env constructors
  in
  let tyext =
    { tyext_path = type_path;
      tyext_txt = styext.ptyext_path;
      tyext_params = ttype_params;
      tyext_constructors = constructors;
      tyext_private = styext.ptyext_private;
      tyext_loc = styext.ptyext_loc;
      tyext_attributes = styext.ptyext_attributes; }
  in
    (tyext, newenv)

let transl_type_extension extend env loc styext =
  Builtin_attributes.warning_scope styext.ptyext_attributes
    (fun () -> transl_type_extension extend env loc styext)

let transl_exception env sext =
  let ext =
    let scope = Ctype.create_scope () in
    Ctype.with_local_level
      (fun () ->
        TyVarEnv.reset();
        transl_extension_constructor ~scope env
          Predef.path_exn [] [] Asttypes.Public sext)
      ~post: begin fun ext ->
        Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
        Option.iter Ctype.generalize ext.ext_type.ext_ret_type;
      end
  in
  (* Check that all type variables are closed *)
  begin match Ctype.closed_extension_constructor ext.ext_type with
    Some ty ->
      raise (Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
  | None -> ()
  end;
  let rebind = is_rebind ext in
  let newenv =
    Env.add_extension ~check:true ~rebind ext.ext_id ext.ext_type env
  in
  ext, newenv

let transl_type_exception env t =
  let contructor, newenv =
    Builtin_attributes.warning_scope t.ptyexn_attributes
      (fun () ->
         transl_exception env t.ptyexn_constructor
      )
  in
  {tyexn_constructor = contructor;
   tyexn_loc = t.ptyexn_loc;
   tyexn_attributes = t.ptyexn_attributes}, newenv


type native_repr_attribute =
  | Native_repr_attr_absent
  | Native_repr_attr_present of native_repr_kind

let get_native_repr_attribute attrs ~global_repr =
  match
    Attr_helper.get_no_payload_attribute ["unboxed"; "ocaml.unboxed"]  attrs,
    Attr_helper.get_no_payload_attribute ["untagged"; "ocaml.untagged"] attrs,
    global_repr
  with
  | None, None, None -> Native_repr_attr_absent
  | None, None, Some repr -> Native_repr_attr_present repr
  | Some _, None, None -> Native_repr_attr_present Unboxed
  | None, Some _, None -> Native_repr_attr_present Untagged
  | Some { Location.loc }, _, _
  | _, Some { Location.loc }, _ ->
    raise (Error (loc, Multiple_native_repr_attributes))

let native_repr_of_type env kind ty =
  match kind, get_desc (Ctype.expand_head_opt env ty) with
  | Untagged, Tconstr (path, _, _) when Path.same path Predef.path_int ->
    Some Untagged_int
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float ->
    Some Unboxed_float
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32 ->
    Some (Unboxed_integer Pint32)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64 ->
    Some (Unboxed_integer Pint64)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_nativeint ->
    Some (Unboxed_integer Pnativeint)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int8x16 ->
    Some (Unboxed_vector (Pvec128 Int8x16))
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int16x8 ->
    Some (Unboxed_vector (Pvec128 Int16x8))
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32x4 ->
    Some (Unboxed_vector (Pvec128 Int32x4))
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64x2 ->
    Some (Unboxed_vector (Pvec128 Int64x2))
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float32x4 ->
    Some (Unboxed_vector (Pvec128 Float32x4))
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float64x2 ->
    Some (Unboxed_vector (Pvec128 Float64x2))
  | _ ->
    None

(* Raises an error when [core_type] contains an [@unboxed] or [@untagged]
   attribute in a strict sub-term. *)
let error_if_has_deep_native_repr_attributes core_type =
  let open Ast_iterator in
  let this_iterator =
    { default_iterator with typ = fun iterator core_type ->
      begin
        match
          get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
        with
        | Native_repr_attr_present kind ->
           raise (Error (core_type.ptyp_loc,
                         Deep_unbox_or_untag_attribute kind))
        | Native_repr_attr_absent -> ()
      end;
      default_iterator.typ iterator core_type }
  in
  default_iterator.typ this_iterator core_type

let make_native_repr env core_type sort ty ~global_repr =
  error_if_has_deep_native_repr_attributes core_type;
  match get_native_repr_attribute core_type.ptyp_attributes ~global_repr with
  | Native_repr_attr_absent ->
    Same_as_ocaml_repr sort
  | Native_repr_attr_present kind ->
    begin match native_repr_of_type env kind ty with
    | None ->
      raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
    | Some repr -> repr
    end

let prim_const_mode m =
  match Mode.Locality.check_const m with
  | Some Global -> Prim_global
  | Some Local -> Prim_local
  | None -> assert false

(* Note that [ty] is guaranteed not to contain sort variables because it was
   produced by [type_scheme], which defaults them.  Further, if ty is an arrow
   we know its bits are representable, so [type_sort_external] can only fail
   on externals with non-arrow types. *)
(* CR layouts v3: When we allow non-representable function args/returns, the
   representability argument above isn't quite right. Decide whether we want to
   allow non-representable types in external args/returns then. *)
let type_sort_external ~why env loc typ =
  match Ctype.type_sort ~why env typ with
  | Ok s -> Jkind.Sort.get_default_value s
  | Error err ->
    raise (Error (loc,Jkind_sort {kloc = External; typ; err}))

let rec parse_native_repr_attributes env core_type ty rmode ~global_repr =
  match core_type.ptyp_desc, get_desc ty,
    get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
  with
  | Ptyp_arrow _, Tarrow _, Native_repr_attr_present kind  ->
    raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
  | Ptyp_arrow (_, ct1, ct2), Tarrow ((_,marg,mret), t1, t2, _), _
    when not (Builtin_attributes.has_curry core_type.ptyp_attributes) ->
    let t1, _ = Btype.tpoly_get_poly t1 in
    let sort_arg =
      type_sort_external ~why:External_argument env ct1.ptyp_loc t1
    in
    let repr_arg = make_native_repr env ct1 sort_arg t1 ~global_repr in
    let mode =
      if Builtin_attributes.has_local_opt ct1.ptyp_attributes
      then Prim_poly
      else prim_const_mode (Mode.Alloc.locality marg)
    in
    let repr_args, repr_res =
      parse_native_repr_attributes env ct2 t2
        (prim_const_mode (Mode.Alloc.locality mret)) ~global_repr
    in
    ((mode, repr_arg) :: repr_args, repr_res)
  | (Ptyp_poly (_, t) | Ptyp_alias (t, _)), _, _ ->
     parse_native_repr_attributes env t ty rmode ~global_repr
  | _ ->
     let rmode =
       if Builtin_attributes.has_local_opt core_type.ptyp_attributes
       then Prim_poly
       else rmode
     in
     let sort_res =
       type_sort_external ~why:External_result env core_type.ptyp_loc ty
     in
     ([], (rmode, make_native_repr env core_type sort_res ty ~global_repr))

let check_unboxable env loc ty =
  let rec check_type acc ty : Path.Set.t =
    let ty = Ctype.expand_head_opt env ty in
    try match get_desc ty with
      | Tconstr (p, _, _) ->
        let tydecl = Env.find_type p env in
        if tydecl.type_unboxed_default then
          Path.Set.add p acc
        else acc
      | Tpoly (ty, []) -> check_type acc ty
      | _ -> acc
    with Not_found -> acc
  in
  let all_unboxable_types = Btype.fold_type_expr check_type Path.Set.empty ty in
  Path.Set.fold
    (fun p () ->
       Location.prerr_warning loc
         (Warnings.Unboxable_type_in_prim_decl (Path.name p))
    )
    all_unboxable_types
    ()

(* Translate a value declaration *)
let transl_value_decl env loc valdecl =
  let cty = Typetexp.transl_type_scheme env valdecl.pval_type in
  (* CR layouts v5: relax this to check for representability. *)
  begin match Ctype.constrain_type_jkind env cty.ctyp_type
                (Jkind.value ~why:Structure_element) with
  | Ok () -> ()
  | Error err ->
    raise(Error(cty.ctyp_loc, Non_value_in_sig(err,valdecl.pval_name.txt,cty.ctyp_type)))
  end;
  let ty = cty.ctyp_type in
  let v =
  match valdecl.pval_prim with
    [] when Env.is_in_signature env ->
      { val_type = ty; val_kind = Val_reg; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes;
        val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
      }
  | [] ->
      raise (Error(valdecl.pval_loc, Val_in_structure))
  | _ ->
      let global_repr =
        match
          get_native_repr_attribute valdecl.pval_attributes ~global_repr:None
        with
        | Native_repr_attr_present repr -> Some repr
        | Native_repr_attr_absent -> None
      in
      let native_repr_args, native_repr_res =
        parse_native_repr_attributes env valdecl.pval_type ty Prim_global ~global_repr
      in
      let prim =
        Primitive.parse_declaration valdecl
          ~native_repr_args
          ~native_repr_res
      in
      if prim.prim_arity = 0 &&
         (prim.prim_name = "" || prim.prim_name.[0] <> '%') then
        raise(Error(valdecl.pval_type.ptyp_loc, Null_arity_external));
      if !Clflags.native_code
      && prim.prim_arity > 5
      && prim.prim_native_name = ""
      then raise(Error(valdecl.pval_type.ptyp_loc, Missing_native_external));
      check_unboxable env loc ty;
      { val_type = ty; val_kind = Val_prim prim; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes;
        val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
      }
  in
  let (id, newenv) =
    Env.enter_value valdecl.pval_name.txt v env
      ~check:(fun s -> Warnings.Unused_value_declaration s)
  in
  let reason = Jkind.Generalized (Some id, loc) in
  Ctype.update_generalized_ty_jkind_reason ty reason;
  let desc =
    {
     val_id = id;
     val_name = valdecl.pval_name;
     val_desc = cty; val_val = v;
     val_prim = valdecl.pval_prim;
     val_loc = valdecl.pval_loc;
     val_attributes = valdecl.pval_attributes;
    }
  in
  desc, newenv

let transl_value_decl env loc valdecl =
  Builtin_attributes.warning_scope valdecl.pval_attributes
    (fun () -> transl_value_decl env loc valdecl)

(* Translate a "with" constraint -- much simplified version of
   transl_type_decl. For a constraint [Sig with t = sdecl],
   there are two declarations of interest in two environments:
   - [sig_decl] is the declaration of [t] in [Sig],
     in the environment [sig_env] (containing the declarations
     of [Sig] before [t])
   - [sdecl] is the new syntactic declaration, to be type-checked
     in the current, outer environment [with_env].

   In particular, note that [sig_env] is an extension of
   [outer_env].
*)
let transl_with_constraint id ?fixed_row_path ~sig_env ~sig_decl ~outer_env
    sdecl =
  Env.mark_type_used sig_decl.type_uid;
  Ctype.with_local_level begin fun () ->
  TyVarEnv.reset();
  (* In the first part of this function, we typecheck the syntactic
     declaration [sdecl] in the outer environment [outer_env]. *)
  let env = outer_env in
  let loc = sdecl.ptype_loc in
  let tparams = make_params env (Pident id) sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let arity = List.length params in
  let constraints =
    List.map (fun (ty, ty', loc) ->
      let cty =
        transl_simple_type ~new_var_jkind:Any env ~closed:false Mode.Alloc.Const.legacy ty
      in
      let cty' =
        transl_simple_type ~new_var_jkind:Sort env ~closed:false Mode.Alloc.Const.legacy ty'
      in
      (* Note: We delay the unification of those constraints
         after the unification of parameters, so that clashing
         constraints report an error on the constraint location
         rather than the parameter location. *)
      (cty, cty', loc)
    ) sdecl.ptype_cstrs
  in
  let no_row = not (is_fixed_type sdecl) in
  let (tman, man) =  match sdecl.ptype_manifest with
      None -> None, None
    | Some sty ->
      let cty =
        transl_simple_type ~new_var_jkind:Any env ~closed:no_row Mode.Alloc.Const.legacy sty
      in
      Some cty, Some cty.ctyp_type
  in
  (* In the second part, we check the consistency between the two
     declarations and compute a "merged" declaration; we now need to
     work in the larger signature environment [sig_env], because
     [sig_decl.type_params] and [sig_decl.type_kind] are only valid
     there. *)
  let env = sig_env in
  let sig_decl = Ctype.instance_declaration sig_decl in
  let arity_ok = arity = sig_decl.type_arity in
  if arity_ok then
    List.iter2 (fun (cty, _) tparam ->
      try Ctype.unify_var env cty.ctyp_type tparam
      with Ctype.Unify err ->
        raise(Error(cty.ctyp_loc, Inconsistent_constraint (env, err)))
    ) tparams sig_decl.type_params;
  List.iter (fun (cty, cty', loc) ->
    (* Note: constraints must also be enforced in [sig_env] because
       they may contain parameter variables from [tparams]
       that have now be unified in [sig_env]. *)
    try Ctype.unify env cty.ctyp_type cty'.ctyp_type
    with Ctype.Unify err ->
      raise(Error(loc, Inconsistent_constraint (env, err)))
    ) constraints;
  let sig_decl_abstract = Btype.type_kind_is_abstract sig_decl in
  let priv =
    if sdecl.ptype_private = Private then Private else
    if arity_ok && not sig_decl_abstract
    then sig_decl.type_private else sdecl.ptype_private
  in
  if arity_ok && not sig_decl_abstract
  && sdecl.ptype_private = Private then
    Location.deprecated loc "spurious use of private";
  let type_kind, type_unboxed_default, type_jkind, type_jkind_annotation =
    (* Here, `man = None` indicates we have a "fake" with constraint built by
       [Typetexp.create_package_mty] for a package type. *)
    if arity_ok && man <> None then
      sig_decl.type_kind,
      sig_decl.type_unboxed_default,
      sig_decl.type_jkind,
      sig_decl.type_jkind_annotation
    else
      (* CR layouts: this is a gross hack.  See the comments in the
         [Ptyp_package] case of [Typetexp.transl_type_aux]. *)
      let jkind = Jkind.value ~why:Package_hack in
        (* Jkind.(of_attributes ~default:value sdecl.ptype_attributes) *)
      Type_abstract Abstract_def, false, jkind, None
  in
  let new_sig_decl =
    { type_params = params;
      type_arity = arity;
      type_kind;
      type_jkind;
      type_jkind_annotation;
      type_private = priv;
      type_manifest = man;
      type_variance = [];
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = loc;
      type_attributes = sdecl.ptype_attributes;
      type_unboxed_default;
      type_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
    }
  in
  Option.iter (fun p -> set_private_row env sdecl.ptype_loc p new_sig_decl)
    fixed_row_path;
  begin match Ctype.closed_type_decl new_sig_decl with None -> ()
  | Some ty -> raise(Error(loc, Unbound_type_var(ty, new_sig_decl)))
  end;
  let new_sig_decl = name_recursion sdecl id new_sig_decl in
  let new_type_variance =
    let required = Typedecl_variance.variance_of_sdecl sdecl in
    try
      Typedecl_variance.compute_decl env ~check:(Some id) new_sig_decl required
    with Typedecl_variance.Error (loc, err) ->
      raise (Error (loc, Variance err)) in
  let new_type_separability =
    try Typedecl_separability.compute_decl env new_sig_decl
    with Typedecl_separability.Error (loc, err) ->
      raise (Error (loc, Separability err)) in
  let new_sig_decl =
    (* we intentionally write this without a fragile { decl with ... }
       to ensure that people adding new fields to type declarations
       consider whether they need to recompute it here; for an example
       of bug caused by the previous approach, see #9607 *)
    {
      type_params = new_sig_decl.type_params;
      type_arity = new_sig_decl.type_arity;
      type_kind = new_sig_decl.type_kind;
      type_jkind = new_sig_decl.type_jkind;
      type_jkind_annotation = new_sig_decl.type_jkind_annotation;
      type_private = new_sig_decl.type_private;
      type_manifest = new_sig_decl.type_manifest;
      type_unboxed_default = new_sig_decl.type_unboxed_default;
      type_is_newtype = new_sig_decl.type_is_newtype;
      type_expansion_scope = new_sig_decl.type_expansion_scope;
      type_loc = new_sig_decl.type_loc;
      type_attributes = new_sig_decl.type_attributes;
      type_uid = new_sig_decl.type_uid;

      type_variance = new_type_variance;
      type_separability = new_type_separability;
    } in
  {
    typ_id = id;
    typ_name = sdecl.ptype_name;
    typ_params = tparams;
    typ_type = new_sig_decl;
    typ_cstrs = constraints;
    typ_loc = loc;
    typ_manifest = tman;
    typ_kind = Ttype_abstract;
    typ_private = sdecl.ptype_private;
    typ_attributes = sdecl.ptype_attributes;
    typ_jkind_annotation = Option.map snd type_jkind_annotation;
  }
  end
  ~post:(fun ttyp -> generalize_decl ttyp.typ_type)

(* Approximate a type declaration: just make all types abstract *)

let abstract_type_decl ~injective ~jkind ~jkind_annotation ~params =
  let arity = List.length params in
  Ctype.with_local_level ~post:generalize_decl begin fun () ->
    let params = List.map Ctype.newvar params in
    { type_params = params;
      type_arity = arity;
      type_kind = Type_abstract Abstract_def;
      type_jkind = jkind;
      type_jkind_annotation = jkind_annotation;
      type_private = Public;
      type_manifest = None;
      type_variance = Variance.unknown_signature ~injective ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = Location.none;
      type_attributes = [];
      type_unboxed_default = false;
      type_uid = Uid.internal_not_actually_unique;
    }
  end

let approx_type_decl sdecl_list =
  let scope = Ctype.create_scope () in
  List.map
    (fun sdecl ->
       let id = Ident.create_scoped ~scope sdecl.ptype_name.txt in
       let path = Path.Pident id in
       let injective = sdecl.ptype_kind <> Ptype_abstract in
       let jkind, jkind_annotation, _sdecl_attributes =
         Jkind.of_type_decl_default
           ~context:(Type_declaration path)
           ~default:(Jkind.value ~why:Default_type_jkind)
           sdecl
       in
       let params =
         List.map (fun (param, _) -> get_type_param_jkind path param)
           sdecl.ptype_params
       in
       (id, abstract_type_decl ~injective ~jkind ~jkind_annotation ~params))
    sdecl_list

(* Check the well-formedness conditions on type abbreviations defined
   within recursive modules. *)

let check_recmod_typedecl env loc recmod_ids path decl =
  (* recmod_ids is the list of recursively-defined module idents.
     (path, decl) is the type declaration to be checked. *)
  let to_check path = Path.exists_free recmod_ids path in
  check_well_founded_decl ~abs_env:env env loc path decl to_check;
  check_regularity ~abs_env:env env loc path decl to_check;
  (* additionally check coherece, as one might build an incoherent signature,
     and use it to build an incoherent module, cf. #7851 *)
  ignore (check_coherence env loc path decl)


(**** Error report ****)

open Format

let explain_unbound_gen ppf tv tl typ kwd pr =
  try
    let ti = List.find (fun ti -> Ctype.deep_occur tv (typ ti)) tl in
    let ty0 = (* Hack to force aliasing when needed *)
      Btype.newgenty (Tobject(tv, ref None)) in
    Printtyp.prepare_for_printing [typ ti; ty0];
    fprintf ppf
      ".@ @[<hov2>In %s@ %a@;<1 -2>the variable %a is unbound@]"
      kwd pr ti Printtyp.prepared_type_expr tv
  with Not_found -> ()

let explain_unbound ppf tv tl typ kwd lab =
  explain_unbound_gen ppf tv tl typ kwd
    (fun ppf ti ->
       fprintf ppf "%s%a" (lab ti) Printtyp.prepared_type_expr (typ ti)
    )

let explain_unbound_single ppf tv ty =
  let trivial ty =
    explain_unbound ppf tv [ty] (fun t -> t) "type" (fun _ -> "") in
  match get_desc ty with
    Tobject(fi,_) ->
      let (tl, rv) = Ctype.flatten_fields fi in
      if eq_type rv tv then trivial ty else
      explain_unbound ppf tv tl (fun (_,_,t) -> t)
        "method" (fun (lab,_,_) -> lab ^ ": ")
  | Tvariant row ->
      if eq_type (row_more row) tv then trivial ty else
      explain_unbound ppf tv (row_fields row)
        (fun (_l,f) -> match row_field_repr f with
          Rpresent (Some t) -> t
        | Reither (_,[t],_) -> t
        | Reither (_,tl,_) -> Btype.newgenty (Ttuple (List.map (fun e -> None, e) tl))
        | _ -> Btype.newgenty (Ttuple[]))
        "case" (fun (lab,_) -> "`" ^ lab ^ " of ")
  | _ -> trivial ty

module Reaching_path = struct
  type t = reaching_type_path

  (* Simplify a reaching path before showing it in error messages. *)
  let simplify path =
    let rec simplify : t -> t = function
      | Contains (ty1, _ty2) :: Contains (_ty2', ty3) :: rest ->
          (* If t1 contains t2 and t2 contains t3, then t1 contains t3
             and we don't need to show t2. *)
          simplify (Contains (ty1, ty3) :: rest)
      | hd :: rest -> hd :: simplify rest
      | [] -> []
    in simplify path

  (* See Printtyp.add_type_to_preparation.

     Note: it is better to call this after [simplify], otherwise some
     type variable names may be used for types that are removed
     by simplification and never actually shown to the user.
  *)
  let add_to_preparation path =
    List.iter (function
      | Contains (ty1, ty2) | Expands_to (ty1, ty2) ->
          List.iter Printtyp.add_type_to_preparation [ty1; ty2]
    ) path

  let pp ppf reaching_path =
    let pp_step ppf = function
      | Expands_to (ty, body) ->
          Format.fprintf ppf "%a = %a"
            Printtyp.prepared_type_expr ty
            Printtyp.prepared_type_expr body
      | Contains (outer, inner) ->
          Format.fprintf ppf "%a contains %a"
            Printtyp.prepared_type_expr outer
            Printtyp.prepared_type_expr inner
    in
    let comma ppf () = Format.fprintf ppf ",@ " in
    Format.(pp_print_list ~pp_sep:comma pp_step) ppf reaching_path

  let pp_colon ppf path =
  Format.fprintf ppf ":@;<1 2>@[<v>%a@]"
    pp path
end

let report_jkind_mismatch_in_check_constraints ppf ty violation =
  fprintf ppf
    "@[<v>Layout mismatch in final type declaration consistency check.@ \
     This is most often caused by the fact that type inference is not@ \
     clever enough to propagate layouts through variables in different@ \
     declarations. It is also not clever enough to produce a good error@ \
     message, so we'll say this instead:@;<1 2>@[%a@]@ \
     The fix will likely be to add a layout annotation on a parameter to@ \
     the declaration where this error is reported.@]"
    (Jkind.Violation.report_with_offender
       ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) violation

let report_error ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Duplicate_constructor s ->
      fprintf ppf "Two constructors are named %s" s
  | Too_many_constructors ->
      fprintf ppf
        "@[Too many non-constant constructors@ -- maximum is %i %s@]"
        (Config.max_tag + 1) "non-constant constructors"
  | Duplicate_label s ->
      fprintf ppf "Two labels are named %s" s
  | Recursive_abbrev (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Printtyp.reset ();
      Reaching_path.add_to_preparation reaching_path;
      fprintf ppf "@[<v>The type abbreviation %s is cyclic%a@]"
        s
        Reaching_path.pp_colon reaching_path
  | Cycle_in_def (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Printtyp.reset ();
      Reaching_path.add_to_preparation reaching_path;
      fprintf ppf "@[<v>The definition of %s contains a cycle%a@]"
        s
        Reaching_path.pp_colon reaching_path
  | Definition_mismatch (ty, _env, None) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]@]"
        "This variant or record definition" "does not match that of type"
        Printtyp.type_expr ty
  | Definition_mismatch (ty, env, Some err) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]%a@]"
        "This variant or record definition" "does not match that of type"
        Printtyp.type_expr ty
        (Includecore.report_type_mismatch
           "the original" "this" "definition" env)
        err
  | Constraint_failed (env, err) ->
      let get_jkind_error : _ Errortrace.elt -> _ = function
      | Bad_jkind (ty, violation) | Bad_jkind_sort (ty, violation) ->
        Some (ty, violation)
      | Unequal_var_jkinds _ | Diff _ | Variant _ | Obj _
      | Escape _ | Incompatible_fields _ | Rec_occur _ -> None
      in
      begin match List.find_map get_jkind_error err.trace with
      | Some (ty, violation) ->
        report_jkind_mismatch_in_check_constraints ppf ty violation
      | None ->
      fprintf ppf "@[<v>Constraints are not satisfied in this type.@ ";
      Printtyp.report_unification_error ppf env err
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "should be an instance of");
      fprintf ppf "@]"
      end
  | Jkind_mismatch_in_check_constraints (ty, violation) ->
      report_jkind_mismatch_in_check_constraints ppf ty violation
  | Non_regular { definition; used_as; defined_as; reaching_path } ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.prepare_for_printing [used_as; defined_as];
      Reaching_path.add_to_preparation reaching_path;
      Printtyp.Naming_context.reset ();
      fprintf ppf
        "@[<hv>This recursive type is not regular.@ \
         The type constructor %s is defined as@;<1 2>type %a@ \
         but it is used as@;<1 2>%a%t\
         All uses need to match the definition for the recursive type \
         to be regular.@]"
        (Path.name definition)
        !Oprint.out_type (Printtyp.tree_of_typexp Type defined_as)
        !Oprint.out_type (Printtyp.tree_of_typexp Type used_as)
        (fun pp ->
           let is_expansion = function Expands_to _ -> true | _ -> false in
           if List.exists is_expansion reaching_path then
             fprintf pp "@ after the following expansion(s)%a@ "
             Reaching_path.pp_colon reaching_path
           else fprintf pp ".@ ")
  | Inconsistent_constraint (env, err) ->
      fprintf ppf "@[<v>The type constraints are not consistent.@ ";
      Printtyp.report_unification_error ppf env err
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type");
      fprintf ppf "@]"
  | Type_clash (env, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "This type constructor expands to type")
        (function ppf ->
           fprintf ppf "but is used here with type")
  | Null_arity_external ->
      fprintf ppf "External identifiers must be functions"
  | Missing_native_external ->
      fprintf ppf "@[<hv>An external function with more than 5 arguments \
                   requires a second stub function@ \
                   for native-code compilation@]"
  | Unbound_type_var (ty, decl) ->
      fprintf ppf "@[A type variable is unbound in this type declaration";
      begin match decl.type_kind, decl.type_manifest with
      | Type_variant (tl, _rep), _ ->
          explain_unbound_gen ppf ty tl (fun c ->
              let tl = tys_of_constr_args c.Types.cd_args in
              Btype.newgenty (Ttuple (List.map (fun t -> None, t) tl))
            )
            "case" (fun ppf c ->
              fprintf ppf
                "%a of %a" Printtyp.ident c.Types.cd_id
                Printtyp.constructor_arguments c.Types.cd_args)
      | Type_record (tl, _), _ ->
          explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
            "field" (fun l -> Ident.name l.Types.ld_id ^ ": ")
      | Type_abstract _, Some ty' ->
          explain_unbound_single ppf ty ty'
      | _ -> ()
      end;
      fprintf ppf "@]"
  | Unbound_type_var_ext (ty, ext) ->
      fprintf ppf "@[A type variable is unbound in this extension constructor";
      let args = tys_of_constr_args ext.ext_args in
      explain_unbound ppf ty args (fun c -> c) "type" (fun _ -> "");
      fprintf ppf "@]"
  | Cannot_extend_private_type path ->
      fprintf ppf "@[%s@ %a@]"
        "Cannot extend private type definition"
        Printtyp.path path
  | Not_extensible_type path ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "Type definition"
        Printtyp.path path
        "is not extensible"
  | Extension_mismatch (path, env, err) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%s@]%a@]"
        "This extension" "does not match the definition of type"
        (Path.name path)
        (Includecore.report_type_mismatch
           "the type" "this extension" "definition" env)
        err
  | Rebind_wrong_type (lid, env, err) ->
      Printtyp.report_unification_error ppf env err
        (function ppf ->
           fprintf ppf "The constructor %a@ has type"
             Printtyp.longident lid)
        (function ppf ->
           fprintf ppf "but was expected to be of type")
  | Rebind_mismatch (lid, p, p') ->
      fprintf ppf
        "@[%s@ %a@ %s@ %s@ %s@ %s@ %s@]"
        "The constructor" Printtyp.longident lid
        "extends type" (Path.name p)
        "whose declaration does not match"
        "the declaration of type" (Path.name p')
  | Rebind_private lid ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "The constructor"
        Printtyp.longident lid
        "is private"
  | Variance (Typedecl_variance.Bad_variance (n, v1, v2)) ->
      let variance (p,n,i) =
        let inj = if i then "injective " else "" in
        match p, n with
          true,  true  -> inj ^ "invariant"
        | true,  false -> inj ^ "covariant"
        | false, true  -> inj ^ "contravariant"
        | false, false -> if inj = "" then "unrestricted" else inj
      in
      (match n with
       | Variance_variable_error { error; variable; context } ->
           Printtyp.prepare_for_printing [ variable ];
           Printtyp.Naming_context.reset ();
           begin match context with
           | Type_declaration (id, decl) ->
               Printtyp.add_type_declaration_to_preparation id decl;
               fprintf ppf "@[<v>%s@;<1 2>%a@;"
                 "In the definition"
                 (Printtyp.prepared_type_declaration id)
                 decl
           | Gadt_constructor c ->
               Printtyp.add_constructor_to_preparation c;
               fprintf ppf "@[<v>%s@;<1 2>%a@;"
                 "In the GADT constructor"
                 Printtyp.prepared_constructor
                 c
           | Extension_constructor (id, e) ->
               Printtyp.add_extension_constructor_to_preparation e;
               fprintf ppf "@[<v>%s@;<1 2>%a@;"
                 "In the extension constructor"
                 (Printtyp.prepared_extension_constructor id)
                 e
           end;
           begin match error with
           | Variance_not_reflected ->
               fprintf ppf "@[%s@ %a@ %s@ %s@ It"
                 "the type variable"
                 Printtyp.prepared_type_expr variable
                 "has a variance that"
                 "is not reflected by its occurrence in type parameters."
           | No_variable ->
               fprintf ppf "@[%s@ %a@ %s@ %s@]@]"
                 "the type variable"
                 Printtyp.prepared_type_expr variable
                 "cannot be deduced"
                 "from the type parameters."
           | Variance_not_deducible ->
               fprintf ppf "@[%s@ %a@ %s@ %s@ It"
                 "the type variable"
                 Printtyp.prepared_type_expr variable
                 "has a variance that"
                 "cannot be deduced from the type parameters."
           end
       | Variance_not_satisfied n ->
           fprintf ppf "@[@[%s@ %s@ The %d%s type parameter"
             "In this definition, expected parameter"
             "variances are not satisfied."
             n (Misc.ordinal_suffix n));
      (match n with
       | Variance_variable_error { error = No_variable; _ } -> ()
       | _ ->
           fprintf ppf " was expected to be %s,@ but it is %s.@]@]"
             (variance v2) (variance v1))
  | Unavailable_type_constructor p ->
      fprintf ppf "The definition of type %a@ is unavailable" Printtyp.path p
  | Variance Typedecl_variance.Varying_anonymous ->
      fprintf ppf "@[%s@ %s@ %s@]"
        "In this GADT definition," "the variance of some parameter"
        "cannot be checked"
  | Val_in_structure ->
      fprintf ppf "Value declarations are only allowed in signatures"
  | Multiple_native_repr_attributes ->
      fprintf ppf "Too many [@@unboxed]/[@@untagged] attributes"
  | Cannot_unbox_or_untag_type Unboxed ->
      fprintf ppf "@[Don't know how to unbox this type.@ \
                   Only float, int32, int64, nativeint, and vector primitives can be unboxed.@]"
  | Cannot_unbox_or_untag_type Untagged ->
      fprintf ppf "@[Don't know how to untag this type.@ \
                   Only int can be untagged.@]"
  | Deep_unbox_or_untag_attribute kind ->
      fprintf ppf
        "@[The attribute '%s' should be attached to@ \
         a direct argument or result of the primitive,@ \
         it should not occur deeply into its type.@]"
        (match kind with Unboxed -> "@unboxed" | Untagged -> "@untagged")
  | Jkind_mismatch_of_path (dpath,v) ->
    (* the type is always printed just above, so print out just the head of the
       path instead of something like [t/3] *)
    let offender ppf = fprintf ppf "type %s" (Ident.name (Path.head dpath)) in
    Jkind.Violation.report_with_offender ~offender ppf v
  | Jkind_mismatch_of_type (ty,v) ->
    let offender ppf = fprintf ppf "type %a" Printtyp.type_expr ty in
    Jkind.Violation.report_with_offender ~offender ppf v
  | Jkind_sort {kloc; typ; err} ->
    let s =
      match kloc with
      | Cstr_tuple -> "Constructor argument"
      | Record -> "Record element"
      | Unboxed_record -> "Unboxed record element"
      | External -> "External"
    in
    fprintf ppf "@[%s types must have a representable layout.@ %a@]" s
      (Jkind.Violation.report_with_offender
         ~offender:(fun ppf -> Printtyp.type_expr ppf typ)) err
  | Jkind_empty_record ->
    fprintf ppf "@[Records must contain at least one runtime value.@]"
  | Non_value_in_sig (err, val_name, ty) ->
    let offender ppf = fprintf ppf "type %a" Printtyp.type_expr ty in
    fprintf ppf "@[This type signature for %s is not a value type.@ %a@]"
<<<<<<< HEAD
      val_name (Jkind.Violation.report_with_name ~name:val_name) err
  | Invalid_jkind_in_block (typ, sort_const, lloc) ->
||||||| parent of 114ab8b0 (Enable layout histories (#1823))
      val_name (Jkind.Violation.report_with_name ~name:val_name) err
  | Float64_in_block (typ, lloc) ->
=======
      val_name (Jkind.Violation.report_with_offender ~offender) err
  | Float64_in_block (typ, lloc) ->
>>>>>>> 114ab8b0 (Enable layout histories (#1823))
    let struct_desc =
      match lloc with
      | Cstr_tuple -> "Variants"
      | Record -> "Records"
      | Unboxed_record -> "Unboxed records"
      | External -> assert false
    in
    fprintf ppf
      "@[Type %a has layout %a.@ %s may not yet contain types of this layout.@]"
      Printtyp.type_expr typ Jkind.Sort.format_const sort_const struct_desc
  | Mixed_block  ->
    fprintf ppf
      "@[Records may not contain both unboxed floats and normal values.@]"
  | Bad_unboxed_attribute msg ->
      fprintf ppf "@[This type cannot be unboxed because@ %s.@]" msg
  | Separability (Typedecl_separability.Non_separable_evar evar) ->
      let pp_evar ppf = function
        | None ->
            fprintf ppf "an unnamed existential variable"
        | Some str ->
            fprintf ppf "the existential variable %a"
              Pprintast.tyvar str in
      fprintf ppf "@[This type cannot be unboxed because@ \
                   it might contain both float and non-float values,@ \
                   depending on the instantiation of %a.@ \
                   You should annotate it with [%@%@ocaml.boxed].@]"
        pp_evar evar
  | Boxed_and_unboxed ->
      fprintf ppf "@[A type cannot be boxed and unboxed at the same time.@]"
  | Nonrec_gadt ->
      fprintf ppf
        "@[GADT case syntax cannot be used in a 'nonrec' block.@]"
  | Invalid_private_row_declaration ty ->
      Format.fprintf ppf
        "@[<hv>This private row type declaration is invalid.@ \
         The type expression on the right-hand side reduces to@;<1 2>%a@ \
         which does not have a free row type variable.@]@,\
         @[<hv>@[@{<hint>Hint@}: If you intended to define a private \
         type abbreviation,@ \
         write explicitly@]@;<1 2>private %a@]"
        Printtyp.type_expr ty Printtyp.type_expr ty
  | Local_not_enabled ->
      fprintf ppf "@[The local extension is disabled@ \
                   To enable it, pass the '-extension local' flag@]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
