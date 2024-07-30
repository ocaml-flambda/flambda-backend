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

(* Operations on module types *)

open Asttypes
open Path
open Types

let freshen ~scope mty =
  Subst.modtype (Rescope scope) Subst.identity mty

(* Strengthen a type as far as possible without unfolding abbreviations,
  pushing strengthening into signatures and functors. Return None if we
  couldn't push it inwards (or eliminate it) and an Mty_strengthen node
  needs to be constructed instead.

  Generally, strengthening with a module M is *aliasable* M is guaranteed
  to be a "real" module (and thus can appear in Mty_alias) and is not
  aliasable if M can be a functor argument. Aliasable strengthening is
  shallow in the sense that we simply replace the types of any submodule
  N by `Mty_alias M.N`. Non-aliasable strengthening is deep since we can't
  form aliases to M or its submodules. Instead, we have to unfold the entire
  module type and strengthen all type definition which is expensive.

  The decision whether a particular strengthening operation is aliasable is
  made when we first introduce it (in Typemod). Strengthening with a functor
  argument remains non-aliasable even if we later substitute a "real" module
  for it. This ensures that strengthening commutes with substitution and
  that it is irrelevant when exactly a strengthening node is expanded.  *)
let rec reduce_strengthen_lazy ~aliasable mty p =
  let open Subst.Lazy in
  match mty with
    Mty_signature sg ->
      Some (Mty_signature(strengthen_lazy_sig ~aliasable sg p))

  | Mty_functor(Named (Some param, arg), res)
    when !Clflags.applicative_functors ->
      Some (Mty_functor(Named (Some param, arg),
        strengthen_lazy ~aliasable:false res (Papply(p, Pident param))))
  | Mty_functor(Named (None, arg), res)
    when !Clflags.applicative_functors ->
      let param = Ident.create_scoped ~scope:(Path.scope p) "Arg" in
      Some (Mty_functor(Named (Some param, arg),
        strengthen_lazy ~aliasable:false res (Papply(p, Pident param))))

  | Mty_strengthen (mty,q,Not_aliasable) when aliasable ->
      (* Normally, we have S/M/N = S/M. However, if the inner strengthening is
        not aliasable and the outer is, we have to strengthen types in S with M
        and modules with N as per the semantics of strengthening.  *)
      begin match reduce_strengthen_lazy ~aliasable:false mty q with
      | Some mty -> reduce_strengthen_lazy ~aliasable:true mty p
      | None -> None
      end
  | Mty_alias _ | Mty_functor _ | Mty_strengthen _ ->
      (* Strengthening aliases, generative functors and already strengthened
        types is a no-op. *)
      Some mty
  | Mty_ident _ -> None

(* Strengthen a type by pushing strengthening inward and/or constructing
    appropriate Mty_strengthen nodes. *)
and strengthen_lazy ~aliasable mty p =
  match reduce_strengthen_lazy ~aliasable mty p with
  | Some mty -> mty
  | None ->
      Subst.Lazy.Mty_strengthen (mty, p, Aliasability.aliasable aliasable)

and strengthen_lazy_sig' ~aliasable sg p =
  let open Subst.Lazy in
  match sg with
    [] -> []
  | (Sig_value(_, _, _) as sigelt) :: rem ->
      sigelt :: strengthen_lazy_sig' ~aliasable rem p
  | Sig_type(id, {type_kind=Type_abstract _}, _, _) :: rem
    when Btype.is_row_name (Ident.name id) ->
      strengthen_lazy_sig' ~aliasable rem p
  | Sig_type(id, decl, rs, vis) :: rem ->
      let newdecl =
        match decl.type_manifest, decl.type_private, decl.type_kind with
          Some _, Public, _ -> decl
        | Some _, Private, (Type_record _ | Type_variant _) -> decl
        | _ ->
            let manif =
              Some(Btype.newgenty(Tconstr(Pdot(p, Ident.name id),
                                          decl.type_params, ref Mnil))) in
            if Btype.type_kind_is_abstract decl then
              { decl with type_private = Public; type_manifest = manif }
            else
              { decl with type_manifest = manif }
      in
      Sig_type(id, newdecl, rs, vis) ::
        strengthen_lazy_sig' ~aliasable rem p
  | (Sig_typext _ as sigelt) :: rem ->
      sigelt :: strengthen_lazy_sig' ~aliasable rem p
  | Sig_module(id, pres, md, rs, vis) :: rem ->
      let str =
        strengthen_lazy_decl ~aliasable md (Pdot(p, Ident.name id))
      in
      Sig_module(id, pres, str, rs, vis)
      :: strengthen_lazy_sig' ~aliasable rem p
  | Sig_modtype(id, decl, vis) :: rem ->
      let newdecl =
        match decl.mtd_type with
        | Some _ when not aliasable ->
            (* [not alisable] condition needed because of recursive modules.
               See [Typemod.check_recmodule_inclusion]. *)
            decl
        | _ ->
            {decl with mtd_type = Some(Mty_ident(Pdot(p,Ident.name id)))}
      in
      Sig_modtype(id, newdecl, vis) ::
      strengthen_lazy_sig' ~aliasable rem p
  | (Sig_class _ as sigelt) :: rem ->
      sigelt :: strengthen_lazy_sig' ~aliasable rem p
  | (Sig_class_type _ as sigelt) :: rem ->
      sigelt :: strengthen_lazy_sig' ~aliasable rem p

and strengthen_lazy_sig ~aliasable sg p =
  let sg = Subst.Lazy.force_signature_once sg in
  let sg = strengthen_lazy_sig' ~aliasable sg p in
  Subst.Lazy.of_value sg

and strengthen_lazy_decl ~aliasable md p =
  let open Subst.Lazy in
  match md.md_type with
  | Mty_alias _ -> md
  | _ when aliasable -> {md with md_type = Mty_alias p}
  | mty -> {md with md_type = strengthen_lazy ~aliasable mty p}

let strengthen ~aliasable mty p =
  let mty = strengthen_lazy ~aliasable (Subst.Lazy.of_modtype mty) p in
  Subst.Lazy.force_modtype mty

let strengthen_decl ~aliasable md p =
  let md = strengthen_lazy_decl ~aliasable (Subst.Lazy.of_module_decl md) p in
  Subst.Lazy.force_module_decl md

(* Perform one reduction on a module type, returning None is it couldn't be
  reduced. Possible reductions are unfolding type abbreviations, pushing
  strengthening inwards and, if aliases is true, resolving module aliases. *)
let rec reduce_lazy ~aliases env mty =
  let open Subst.Lazy in
  match mty with
    Mty_ident p ->
      begin try
        Some (Env.find_modtype_expansion_lazy p env)
      with Not_found ->
        None
      end
  | Mty_alias path when aliases ->
        begin try
          let mty = (Env.find_module_lazy path env).md_type in
          let mty = strengthen_lazy ~aliasable:true mty path in
          Some mty
        with Not_found ->
          (*Location.prerr_warning Location.none
            (Warnings.No_cmi_file (Path.name path));*)
          None
        end
  | Mty_strengthen (mty,p,a) ->
      let aliasable = Aliasability.is_aliasable a in
      begin match reduce_strengthen_lazy ~aliasable mty p with
      | Some mty -> Some mty
      | None ->
        begin match reduce_lazy ~aliases env mty with
        | Some mty -> Some (strengthen_lazy ~aliasable mty p)
        | None -> None
        end
      end
  | Mty_signature _ | Mty_functor _ | Mty_alias _ -> None

let rec scrape_lazy ~aliases env mty =
  match reduce_lazy ~aliases env mty with
  | Some mty -> scrape_lazy ~aliases env mty
  | None -> mty

let reduce_lazy env mty = reduce_lazy ~aliases:false env mty

let reduce env mty =
  Subst.Lazy.of_modtype mty
  |> reduce_lazy env
  |> Option.map Subst.Lazy.force_modtype

(* Expand delayed strengthening *)

let rec expand_lazy env mty =
  let open Subst.Lazy in
  match mty with
  | Mty_strengthen _ ->
    begin match reduce_lazy env mty with
    | Some mty -> expand_lazy env mty
    | None -> mty
    end
  | _ -> mty

let expand env mty =
  Subst.Lazy.of_modtype mty
  |> expand_lazy env
  |> Subst.Lazy.force_modtype

(* Expand delayed strengthening up to the given paths. Note that if M.N is in
    paths then M must be in paths, too. This is guaranteed by expand_to below. *)

let rec expand_paths_lazy paths env =
  let open Subst.Lazy in
  function
    Mty_signature sg ->
      Mty_signature (expand_paths_lazy_sig paths env sg)
  | Mty_functor (param,res) ->
      let param, env = match param with
        Unit -> Unit, env
      | Named (name,mty) ->
          let mty = expand_paths_lazy paths env mty in
          let env = match name with
            | Some param when !Clflags.applicative_functors ->
                Env.add_module_lazy ~update_summary:false param Mp_present mty env
            | Some _ | None -> env
          in
          Named (name, mty), env
      in
      let res = expand_paths_lazy paths env res in
      Mty_functor (param,res)
  | Mty_strengthen (_,p,_) as mty when Path.Set.mem p paths ->
      (* If the path we're strengthening with is in paths then we need to
          unfold the node. *)
      begin match reduce_lazy env mty with
      | Some mty -> expand_paths_lazy paths env mty
      | None -> assert false
          (* This shouldn't be able to happen because for
            `with module M.N := X`, `paths` will only contain M but not M.N
            and M's type can't be abstract. *)
      end
  | Mty_strengthen (mty,p,a) ->
      (* If the path we're strengthening with isn't in paths then we can
          can just unfold the strengthened type but keep the Mty_strengthen
          node. *)
      let mty = expand_paths_lazy paths env mty in
      Mty_strengthen (mty,p,a)
  | Mty_ident _ | Mty_alias _ as mty ->
      mty

and expand_paths_lazy_sig paths env sg =
  let open Subst.Lazy in
  force_signature_once sg
  |> expand_paths_lazy_sig_items paths env
  |> of_value

and expand_paths_lazy_sig_items paths env sg =
  let open Subst.Lazy in
  let expand_item env = function
    | Sig_module (id,pres,md,rs,vis) ->
        let md = { md with md_type = expand_paths_lazy paths env md.md_type }
        in
        let env =
          Env.add_module_declaration_lazy ~update_summary:false id pres md env
        in
        env, Sig_module (id,pres,md,rs,vis)
      | Sig_modtype (id,mtd,vis) ->
          let mt = Option.map (expand_paths_lazy paths env) mtd.mtd_type in
          let mtd = { mtd with mtd_type = mt } in
          let env = Env.add_modtype_lazy ~update_summary:false id mtd env in
          env, Sig_modtype (id,mtd,vis)
      | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_class _
      | Sig_class_type _ as item ->
          env, item
  in
  List.fold_left_map expand_item env sg |> snd

let expand_to env sg paths =
  let rec add_paths paths = function
    | Pdot (p,_) -> add_paths (Path.Set.add p paths) p
    | _ -> paths
  in
  let paths = List.fold_left add_paths Path.Set.empty paths in
  Subst.Lazy.of_signature sg
  |> expand_paths_lazy_sig paths env
  |> Subst.Lazy.force_signature

let rec sig_make_manifest sg =
  match sg with
    [] -> []
  | (Sig_value _ | Sig_class _ | Sig_class_type _) as t :: rem ->
    t :: sig_make_manifest rem
  | Sig_type (id,decl,rs,vis) :: rem ->
    let newdecl =
      match decl.type_manifest, decl.type_private, decl.type_kind with
        Some _, Public, _ -> decl
      | Some _, Private, (Type_record _ | Type_variant _) -> decl
      | _ ->
        let manif =
          Some (Btype.newgenty(Tconstr(Pident id, decl.type_params, ref Mnil)))
        in
        match decl.type_kind with
        | Type_abstract _ ->
          { decl with type_private = Public; type_manifest = manif }
        | (Type_record _ | Type_variant _ | Type_open) ->
          { decl with type_manifest = manif }
    in
    Sig_type(Ident.rename id, newdecl, rs, vis) :: sig_make_manifest rem
  | Sig_typext _ as sigelt :: rem ->
    sigelt :: sig_make_manifest rem
  | Sig_module(id, pres, md, rs, vis) :: rem ->
    let md =
      match md.md_type with
      | Mty_alias _ -> md
      | _ -> {md with md_type = Mty_alias (Pident id)}
    in
    Sig_module(Ident.rename id, pres, md, rs, vis) :: sig_make_manifest rem
  | Sig_modtype(id, decl, vis) :: rem ->
    let newdecl =
      {decl with mtd_type =
                   match decl.mtd_type with
                   | None -> Some (Mty_ident (Pident id))
                   | Some _ -> decl.mtd_type }
    in
    Sig_modtype(Ident.rename id, newdecl, vis) :: sig_make_manifest rem

let rec make_aliases_absent ~aliased pres mty =
  (* aliased=true means that mty is subject to aliasable strengthening
    and thus any module we encounter in it will be an alias (we don't need to
    know of what) and thus absent. This is purely an optimisation over
    expanding Mty_strengthen nodes. *)
  match mty with
  | Mty_alias _ -> Mp_absent, mty
  | Mty_signature sg ->
      let make_item = function
        | Sig_module(id, pres, md, rs, priv) ->
          let pres, md = if aliased
            then Mp_absent, md
            else
              let pres, md_type =
                make_aliases_absent ~aliased:false pres md.md_type
              in
              pres, { md with md_type }
          in
          Sig_module(id, pres, md, rs, priv)
        | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_modtype _
        | Sig_class _ | Sig_class_type _ as item ->
          item
      in
      pres, Mty_signature(List.map make_item sg)
  | Mty_functor(arg, res) ->
      let _, res = make_aliases_absent ~aliased:false Mp_present res in
      pres, Mty_functor(arg, res)
  | Mty_ident _ ->
      pres, mty
  | Mty_strengthen (mty,p,a) ->
      let aliased = aliased || Aliasability.is_aliasable a in
      let pres, res = make_aliases_absent ~aliased pres mty in
      pres, Mty_strengthen (res,p,a)

let scrape_for_type_of env pres mty =
  let rec loop env outer = function
    | Mty_alias path -> begin
        try
          let md = Env.find_module path env in
          let mty = strengthen ~aliasable:false md.md_type path in
          loop env mty mty
        with Not_found -> outer
      end
    | Mty_strengthen (inner,_,_) -> loop env outer inner
    | Mty_ident _ | Mty_signature _ | Mty_functor _ -> outer
  in
  make_aliases_absent ~aliased:false pres (loop env mty mty)

(* Expand manifest module type names at the top of the given module type *)

let scrape_alias_lazy env mty = scrape_lazy ~aliases:true env mty

(* Non-lazy version of scrape_alias *)
let scrape_alias env mty =
  Subst.Lazy.of_modtype mty
  |> scrape_alias_lazy env
  |> Subst.Lazy.force_modtype

let scrape_lazy env mty = scrape_lazy ~aliases:false env mty

let scrape env mty =
  match mty with
    Mty_ident _ | Mty_strengthen _ ->
      Subst.Lazy.force_modtype (scrape_lazy env (Subst.Lazy.of_modtype mty))
  | _ -> mty

let () =
  Printtyp.expand_module_type := expand ;
  Env.scrape_alias := scrape_alias_lazy

let find_type_of_module ~strengthen ~aliasable env path =
  if strengthen then
    let md = Env.find_module_lazy path env in
    let mty = strengthen_lazy ~aliasable md.md_type path in
    Subst.Lazy.force_modtype mty
  else
    (Env.find_module path env).md_type

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let rec nondep_mty_with_presence env va ids pres mty =
  match expand env mty with
    Mty_ident p ->
      begin match Path.find_free_opt ids p with
      | Some id ->
          let expansion =
            try Env.find_modtype_expansion p env
            with Not_found ->
              raise (Ctype.Nondep_cannot_erase id)
          in
          nondep_mty_with_presence env va ids pres expansion
      | None -> pres, mty
      end
  | Mty_alias p ->
      begin match Path.find_free_opt ids p with
      | Some id ->
          let expansion =
            try Env.find_module p env
            with Not_found ->
              raise (Ctype.Nondep_cannot_erase id)
          in
          nondep_mty_with_presence env va ids Mp_present expansion.md_type
      | None -> pres, mty
      end
  | Mty_signature sg ->
      let mty = Mty_signature(nondep_sig env va ids sg) in
      pres, mty
  | Mty_functor(Unit, res) ->
      pres, Mty_functor(Unit, nondep_mty env va ids res)
  | Mty_functor(Named (param, arg), res) ->
      let var_inv =
        match va with Co -> Contra | Contra -> Co | Strict -> Strict in
      let res_env =
        match param with
        | None -> env
        | Some param -> Env.add_module ~arg:true param Mp_present arg env
      in
      let mty =
        Mty_functor(Named (param, nondep_mty env var_inv ids arg),
                    nondep_mty res_env va ids res)
      in
      pres, mty
  | Mty_strengthen (mty,p,a) ->
      (* If we end up strengthening an abstract type with a dependent module,
        just drop the strengthening. *)
      let pres,mty = nondep_mty_with_presence env va ids pres mty
      in
      let mty =
        if Path.exists_free ids p
          then mty
          else strengthen ~aliasable:(Aliasability.is_aliasable a) mty p
      in
      pres,mty

and nondep_mty env va ids mty =
  snd (nondep_mty_with_presence env va ids Mp_present mty)

and nondep_sig_item env va ids = function
  | Sig_value(id, d, vis) ->
      Sig_value(id,
                {d with val_type = Ctype.nondep_type env ids d.val_type},
                vis)
  | Sig_type(id, d, rs, vis) ->
      Sig_type(id, Ctype.nondep_type_decl env ids (va = Co) d, rs, vis)
  | Sig_typext(id, ext, es, vis) ->
      Sig_typext(id, Ctype.nondep_extension_constructor env ids ext, es, vis)
  | Sig_module(id, pres, md, rs, vis) ->
      let pres, mty = nondep_mty_with_presence env va ids pres md.md_type in
      Sig_module(id, pres, {md with md_type = mty}, rs, vis)
  | Sig_modtype(id, d, vis) ->
      begin try
        Sig_modtype(id, nondep_modtype_decl env ids d, vis)
      with Ctype.Nondep_cannot_erase _ as exn ->
        match va with
          Co -> Sig_modtype(id, {mtd_type=None; mtd_loc=Location.none;
                                 mtd_attributes=[]; mtd_uid = d.mtd_uid}, vis)
        | _  -> raise exn
      end
  | Sig_class(id, d, rs, vis) ->
      Sig_class(id, Ctype.nondep_class_declaration env ids d, rs, vis)
  | Sig_class_type(id, d, rs, vis) ->
      Sig_class_type(id, Ctype.nondep_cltype_declaration env ids d, rs, vis)

and nondep_sig env va ids sg =
  let scope = Ctype.create_scope () in
  let sg, env = Env.enter_signature ~scope sg env in
  List.map (nondep_sig_item env va ids) sg

and nondep_modtype_decl env ids mtd =
  {mtd with mtd_type = Option.map (nondep_mty env Strict ids) mtd.mtd_type}

let nondep_supertype env ids = nondep_mty env Co ids
let nondep_sig env ids = nondep_sig env Co ids
let nondep_sig_item env ids = nondep_sig_item env Co ids

let enrich_typedecl env p id decl =
  match decl.type_manifest with
    Some _ -> decl
  | None ->
    match Env.find_type p env with
    | exception Not_found -> decl
        (* Type which was not present in the signature, so we don't have
           anything to do. *)
    | orig_decl ->
        if decl.type_arity <> orig_decl.type_arity then
          decl
        else begin
          let orig_ty =
            Ctype.reify_univars env
              (Btype.newgenty(Tconstr(p, orig_decl.type_params, ref Mnil)))
          in
          let new_ty =
            Ctype.reify_univars env
              (Btype.newgenty(Tconstr(Pident id, decl.type_params, ref Mnil)))
          in
          let env = Env.add_type ~check:false id decl env in
          match Ctype.mcomp env orig_ty new_ty with
          | exception Ctype.Incompatible -> decl
              (* The current declaration is not compatible with the one we got
                 from the signature. We should just fail now, but then, we could
                 also have failed if the arities of the two decls were
                 different, which we didn't. *)
          | () ->
              let orig_ty =
                Btype.newgenty(Tconstr(p, decl.type_params, ref Mnil))
              in
              {decl with type_manifest = Some orig_ty}
        end

let rec enrich_modtype env p mty =
  match expand env mty with
    Mty_signature sg ->
      Mty_signature(List.map (enrich_item env p) sg)
  | _ ->
      mty

and enrich_item env p = function
    Sig_type(id, decl, rs, priv) ->
      Sig_type(id,
                enrich_typedecl env (Pdot(p, Ident.name id)) id decl, rs, priv)
  | Sig_module(id, pres, md, rs, priv) ->
      Sig_module(id, pres,
                  {md with
                   md_type = enrich_modtype env
                       (Pdot(p, Ident.name id)) md.md_type},
                 rs,
                 priv)
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
    Mty_ident _ -> []
  | Mty_alias _ -> []
  | Mty_signature sg -> type_paths_sig env p sg
  | Mty_functor _ -> []
  | Mty_strengthen _ -> []

and type_paths_sig env p sg =
  match sg with
    [] -> []
  | Sig_type(id, _decl, _, _) :: rem ->
      Pdot(p, Ident.name id) :: type_paths_sig env p rem
  | Sig_module(id, pres, md, _, _) :: rem ->
      type_paths env (Pdot(p, Ident.name id)) md.md_type @
      type_paths_sig (Env.add_module_declaration ~check:false id pres md env)
        p rem
  | Sig_modtype(id, decl, _) :: rem ->
      type_paths_sig (Env.add_modtype id decl env) p rem
  | (Sig_value _ | Sig_typext _ | Sig_class _ | Sig_class_type _) :: rem ->
      type_paths_sig env p rem


let rec no_code_needed_mod env pres mty =
  match pres with
  | Mp_absent -> true
  | Mp_present -> begin
      match scrape env mty with
        Mty_ident _ -> false
      | Mty_signature sg -> no_code_needed_sig env sg
      | Mty_functor _ -> false
      | Mty_alias _ -> false
      | Mty_strengthen _ -> false
    end

and no_code_needed_sig env sg =
  match sg with
    [] -> true
  | Sig_value(_id, decl, _) :: rem ->
      begin match decl.val_kind with
      | Val_prim _ -> no_code_needed_sig env rem
      | _ -> false
      end
  | Sig_module(id, pres, md, _, _) :: rem ->
      no_code_needed_mod env pres md.md_type &&
      no_code_needed_sig
        (Env.add_module_declaration ~check:false id pres md env) rem
  | (Sig_type _ | Sig_modtype _ | Sig_class_type _) :: rem ->
      no_code_needed_sig env rem
  | (Sig_typext _ | Sig_class _) :: _ ->
      false

let no_code_needed env mty = no_code_needed_mod env Mp_present mty

(* Check whether a module type may return types *)

let rec contains_type env mty =
  match scrape env mty with
    Mty_ident _ -> raise Exit (* PR#6427 *)
  | Mty_signature sg ->
      contains_type_sig env sg
  | Mty_functor (_, body) ->
      contains_type env body
  | Mty_alias _ ->
      ()
  | Mty_strengthen _ -> raise Exit

and contains_type_sig env = List.iter (contains_type_item env)

and contains_type_item env = function
    Sig_type (_,({type_manifest = None} |
                 {type_kind = Type_abstract _; type_private = Private}),_, _)
  | Sig_modtype _
  | Sig_typext (_, {ext_args = Cstr_record _}, _, _) ->
      (* We consider that extension constructors with an inlined
         record create a type (the inlined record), even though
         it would be technically safe to ignore that considering
         the current constraints which guarantee that this type
         is kept local to expressions.  *)
      raise Exit
  | Sig_module (_, _, {md_type = mty}, _, _) ->
      contains_type env mty
  | Sig_value _
  | Sig_type _
  | Sig_typext _
  | Sig_class _
  | Sig_class_type _ ->
      ()

let contains_type env mty =
  try contains_type env mty; false with Exit -> true


(* Remove module aliases from a signature *)

let rec get_prefixes = function
  | Pident _ -> Path.Set.empty
  | Pdot (p, _) | Papply (p, _) | Pextra_ty (p, _)
    -> Path.Set.add p (get_prefixes p)

let rec get_arg_paths = function
  | Pident _ -> Path.Set.empty
  | Pdot (p, _) | Pextra_ty (p, _) -> get_arg_paths p
  | Papply (p1, p2) ->
      Path.Set.add p2
        (Path.Set.union (get_prefixes p2)
           (Path.Set.union (get_arg_paths p1) (get_arg_paths p2)))

let rec rollback_path subst p =
  try Pident (Path.Map.find p subst)
  with Not_found ->
    match p with
      Pident _ | Papply _ -> p
    | Pdot (p1, s) ->
        let p1' = rollback_path subst p1 in
        if Path.same p1 p1' then p else rollback_path subst (Pdot (p1', s))
    | Pextra_ty (p1, extra) ->
        let p1' = rollback_path subst p1 in
        if Path.same p1 p1' then p
        else rollback_path subst (Pextra_ty (p1', extra))

let rec collect_ids subst bindings p =
    begin match rollback_path subst p with
      Pident id ->
        let ids =
          try collect_ids subst bindings (Ident.find_same id bindings)
          with Not_found -> Ident.Set.empty
        in
        Ident.Set.add id ids
    | _ -> Ident.Set.empty
    end

let collect_arg_paths mty =
  let open Btype in
  let paths = ref Path.Set.empty
  and subst = ref Path.Map.empty
  and bindings = ref Ident.empty in
  (* let rt = Ident.create "Root" in
     and prefix = ref (Path.Pident rt) in *)
  let it_path _type p = paths := Path.Set.union (get_arg_paths p) !paths
  and it_signature_item it si =
    type_iterators.it_signature_item it si;
    match si with
    | Sig_module (id, _, {md_type=Mty_alias p}, _, _) ->
        bindings := Ident.add id p !bindings
    | Sig_module (id, _, {md_type=Mty_signature sg}, _, _) ->
        List.iter
          (function Sig_module (id', _, _, _, _) ->
              subst :=
                Path.Map.add (Pdot (Pident id, Ident.name id')) id' !subst
            | _ -> ())
          sg
    | _ -> ()
  in
  let it = {type_iterators with it_path; it_signature_item} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty;
  Path.Set.fold (fun p -> Ident.Set.union (collect_ids !subst !bindings p))
    !paths Ident.Set.empty

type remove_alias_from =
  | Alias of Ident.t
  | Strengthening
type remove_alias_args =
    { mutable modified: bool;
      exclude: remove_alias_from -> Path.t -> bool;
      scrape: Env.t -> module_type -> module_type }

let rec remove_aliases_mty env args pres mty =
  let args' = {args with modified = false} in
  let res =
    match args.scrape env mty with
      Mty_signature sg ->
        Mp_present, Mty_signature (remove_aliases_sig env args' sg)
    | Mty_alias _ ->
        let mty' = scrape_alias env mty in
        if mty' = mty then begin
          pres, mty
        end else begin
          args'.modified <- true;
          remove_aliases_mty env args' Mp_present mty'
        end
    | Mty_strengthen (mty,p,Aliasable) when not (args.exclude Strengthening p) ->
        let mty = strengthen ~aliasable:false mty p in
        args'.modified <- true;
        Mp_present, mty
    | mty ->
        Mp_present, mty
  in
  if args'.modified then begin
    args.modified <- true;
    res
  end else begin
    pres, mty
  end

and remove_aliases_sig env args sg =
  match sg with
    [] -> []
  | Sig_module(id, pres, md, rs, priv) :: rem  ->
      let pres, mty =
        match md.md_type with
          Mty_alias p when args.exclude (Alias id) p ->
            pres, md.md_type
        | mty ->
            remove_aliases_mty env args pres mty
      in
      Sig_module(id, pres, {md with md_type = mty} , rs, priv) ::
      remove_aliases_sig (Env.add_module id pres mty env) args rem
  | Sig_modtype(id, mtd, priv) :: rem ->
      Sig_modtype(id, mtd, priv) ::
      remove_aliases_sig (Env.add_modtype id mtd env) args rem
  | it :: rem ->
      it :: remove_aliases_sig env args rem

let scrape_for_functor_arg env mty =
  let exclude _id p =
    try ignore (Env.find_module p env); true with Not_found -> false
  in
  let _, mty =
    remove_aliases_mty env {modified=false; exclude; scrape} Mp_present mty
  in
  mty

let scrape_for_type_of ~remove_aliases env mty =
  if remove_aliases then begin
    let excl = collect_arg_paths mty in
    let exclude id _p = match id with
      | Alias id -> Ident.Set.mem id excl
      | Strengthening -> false
    in
    let scrape _ mty = mty in
    let _, mty =
      remove_aliases_mty env {modified=false; exclude; scrape} Mp_present mty
    in
    mty
  end else begin
    let _, mty = scrape_for_type_of env Mp_present mty in
    mty
  end

(* Lower non-generalizable type variables *)

let lower_nongen nglev mty =
  let open Btype in
  let it_type_expr it ty =
    match get_desc ty with
      Tvar _ ->
        let level = get_level ty in
        if level < generic_level && level > nglev then set_level ty nglev
    | _ ->
        type_iterators.it_type_expr it ty
  in
  let it = {type_iterators with it_type_expr} in
  it.it_module_type it mty;
  it.it_module_type unmark_iterators mty
