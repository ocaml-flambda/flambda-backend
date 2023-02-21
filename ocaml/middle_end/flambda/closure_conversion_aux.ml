(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

module Env = struct
  type t = {
    variables : (Variable.t * Lambda.layout) Ident.tbl;
    mutable_variables : (Mutable_variable.t * Lambda.layout) Ident.tbl;
    static_exceptions : Static_exception.t Numbers.Int.Map.t;
    globals : Symbol.t Numbers.Int.Map.t;
    at_toplevel : bool;
  }

  let empty = {
    variables = Ident.empty;
    mutable_variables = Ident.empty;
    static_exceptions = Numbers.Int.Map.empty;
    globals = Numbers.Int.Map.empty;
    at_toplevel = true;
  }

  let clear_local_bindings env =
    { empty with globals = env.globals }

  let add_var t id var kind =
    { t with variables = Ident.add id (var, kind) t.variables }
  let add_vars t ids vars =
    List.fold_left2 (fun t id (var, kind) -> add_var t id var kind) t ids vars

  let find_var t id =
    try Ident.find_same id t.variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let find_var_exn t id =
    Ident.find_same id t.variables

  let add_mutable_var t id mutable_var kind =
    let mutable_variables = Ident.add id (mutable_var, kind) t.mutable_variables in
    { t with mutable_variables }

  let find_mutable_var_exn t id =
    Ident.find_same id t.mutable_variables

  let add_static_exception t st_exn fresh_st_exn =
    { t with
      static_exceptions =
        Numbers.Int.Map.add st_exn fresh_st_exn t.static_exceptions }

  let find_static_exception t st_exn =
    try Numbers.Int.Map.find st_exn t.static_exceptions
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_static_exception: exn "
        ^ Int.to_string st_exn)

  let add_global t pos symbol =
    { t with globals = Numbers.Int.Map.add pos symbol t.globals }

  let find_global t pos =
    try Numbers.Int.Map.find pos t.globals
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_global: global "
        ^ Int.to_string pos)

  let at_toplevel t = t.at_toplevel

  let not_at_toplevel t = { t with at_toplevel = false; }
end

module Function_decls = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_bound_var : Variable.t;
      kind : Lambda.function_kind;
      mode : Lambda.alloc_mode;
      region : bool;
      params : (Ident.t * Lambda.layout) list;
      return_layout : Lambda.layout;
      body : Lambda.lambda;
      free_idents_of_body : Ident.Set.t;
      attr : Lambda.function_attribute;
      loc : Lambda.scoped_location
    }

    let create ~let_rec_ident ~closure_bound_var ~kind ~mode ~region
          ~params ~return_layout ~body ~attr ~loc =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create_local "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        closure_bound_var;
        kind;
        mode;
        region;
        params;
        return_layout;
        body;
        free_idents_of_body = Lambda.free_variables body;
        attr;
        loc;
      }

    let let_rec_ident t = t.let_rec_ident
    let closure_bound_var t = t.closure_bound_var
    let kind t = t.kind
    let mode t = t.mode
    let region t = t.region
    let params t = t.params
    let return_layout t = t.return_layout
    let body t = t.body
    let free_idents t = t.free_idents_of_body
    let inline t = t.attr.inline
    let specialise t = t.attr.specialise
    let is_a_functor t = t.attr.is_a_functor
    let stub t = t.attr.stub
    let poll_attribute t = t.attr.poll
    let loc t = t.loc

  end

  type t = {
    function_decls : Function_decl.t list;
    all_free_idents : Ident.Set.t;
  }

  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let free_idents_by_function function_decls =
    List.fold_right (fun decl map ->
        Variable.Map.add (Function_decl.closure_bound_var decl)
          (Function_decl.free_idents decl) map)
      function_decls Variable.Map.empty

  let all_free_idents function_decls =
    Variable.Map.fold (fun _ -> Ident.Set.union)
      (free_idents_by_function function_decls) Ident.Set.empty

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents function_decls =
    List.map Function_decl.let_rec_ident function_decls

  (* All parameters of functions in [ts]. *)
  let all_params function_decls =
    List.concat (List.map Function_decl.params function_decls)

  let set_diff (from : Ident.Set.t) (idents : Ident.t list) =
    List.fold_right Ident.Set.remove idents from

  (* CR-someday lwhite: use a different name from above or explain the
     difference *)
  let all_free_idents function_decls =
    set_diff (set_diff (all_free_idents function_decls)
        (List.map fst (all_params function_decls)))
      (let_rec_idents function_decls)

  let create (function_decls : Function_decl.t list) =
    { function_decls;
      all_free_idents = all_free_idents function_decls;
    }

  let to_list t = t.function_decls

  let all_free_idents t = t.all_free_idents

  let closure_env_without_parameters external_env t =
    let closure_env =
      (* For "let rec"-bound functions. *)
      List.fold_right (fun function_decl env ->
          Env.add_var env (Function_decl.let_rec_ident function_decl)
            (Function_decl.closure_bound_var function_decl) Lambda.layout_function)
        t.function_decls (Env.clear_local_bindings external_env)
    in
    (* For free variables. *)
    Ident.Set.fold (fun id env ->
        let _, kind = Env.find_var external_env id in
        Env.add_var env id (Variable.create_with_same_name_as_ident id) kind
      )
      t.all_free_idents closure_env
end
