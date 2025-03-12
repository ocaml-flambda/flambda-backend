open Env

(* The functions in this file provide a mechanism for reconstructing (as much as possible)
   of an environment from an environment summary.  The code in this file is copied from
   the [envaux.ml] file from the compiler distribution.  It differs from [envaux.ml] in
   the treatment of what happens when a module cannot be found.  *)

let env_cache = (Hashtbl.create 59 : (Env.summary * Subst.t, Env.t) Hashtbl.t)

let rec env_from_summary sum subst =
  try Hashtbl.find env_cache (sum, subst) with
  | Not_found ->
    let env =
      match sum with
      | Env_empty -> Env.empty
      | Env_value (s, id, desc, mode) ->
        let desc =
          Subst.Lazy.of_value_description desc |> Subst.Lazy.value_description subst
        in
        Env.add_value_lazy ~mode id desc (env_from_summary s subst)
      | Env_type (s, id, desc) ->
        Env.add_type
          ~check:false
          id
          (Subst.type_declaration subst desc)
          (env_from_summary s subst)
      | Env_extension (s, id, desc) ->
        Env.add_extension
          ~check:false
          ~rebind:false
          id
          (Subst.extension_constructor subst desc)
          (env_from_summary s subst)
      | Env_module (s, id, pres, desc) ->
        let desc = Subst.Lazy.module_decl Keep subst (Subst.Lazy.of_module_decl desc) in
        Env.add_module_declaration_lazy
          ~update_summary:true
          id
          pres
          desc
          (env_from_summary s subst)
      | Env_modtype (s, id, desc) ->
        let desc = Subst.Lazy.modtype_decl Keep subst (Subst.Lazy.of_modtype_decl desc) in
        Env.add_modtype_lazy ~update_summary:true id desc (env_from_summary s subst)
      | Env_class (s, id, desc) ->
        Env.add_class id (Subst.class_declaration subst desc) (env_from_summary s subst)
      | Env_cltype (s, id, desc) ->
        Env.add_cltype id (Subst.cltype_declaration subst desc) (env_from_summary s subst)
      | Env_open (s, path) ->
        let env = env_from_summary s subst in
        let path' = Subst.module_path subst path in
        (* TODO: This is the same code as [envaux.ml] in the
          compiler except for the following line. (Instead of failing, we accept
          missing modules and continue constructing an environment.) To delete this
          file, we need to create a PR in the compiler to make this function accept a
          flag on what it should do in this case.*)
        (* silently fail if we cannot open *)
        (try Env.open_signature_by_path path' env with
         | Not_found -> env)
      | Env_functor_arg (Env_module (s, id, pres, desc), id') when Ident.same id id' ->
        let desc = Subst.Lazy.module_decl Keep subst (Subst.Lazy.of_module_decl desc) in
        Env.add_module_declaration_lazy
          ~update_summary:true
          id
          pres
          desc
          ~arg:true
          (env_from_summary s subst)
      | Env_functor_arg _ -> assert false
      | Env_constraints (s, map) ->
        Path.Map.fold
          (fun path info ->
            Env.add_local_constraint
              (Subst.type_path subst path)
              (Subst.type_declaration subst info))
          map
          (env_from_summary s subst)
      | Env_copy_types s ->
        let env = env_from_summary s subst in
        Env.make_copy_of_types env env
      | Env_persistent (s, id) ->
        let env = env_from_summary s subst in
        Env.add_persistent_structure id env
      | Env_value_unbound (s, str, reason) ->
        let env = env_from_summary s subst in
        Env.enter_unbound_value str reason env
      | Env_module_unbound (s, str, reason) ->
        let env = env_from_summary s subst in
        Env.enter_unbound_module str reason env
    in
    Hashtbl.add env_cache (sum, subst) env;
    env
;;

let env_of_only_summary env = Env.env_of_only_summary env_from_summary env
