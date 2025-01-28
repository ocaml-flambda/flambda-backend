(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type decl = Types.type_declaration

type ('prop, 'req) property = {
  eq : 'prop -> 'prop -> bool;
  merge : prop:'prop -> new_prop:'prop -> 'prop;

  default : decl -> 'prop;
  compute : Env.t -> decl -> 'req -> 'prop;
  update_decl : decl -> 'prop -> decl;

  check : Env.t -> Ident.t -> decl -> 'req * 'req option -> unit;
}

let add_type ~check id decl env =
  let open Types in
  Builtin_attributes.warning_scope ~ppwarning:false decl.type_attributes
    (fun () -> Env.add_type ~check id decl env)

let add_types_to_env decls env =
  List.fold_right
    (fun (id, decl) env -> add_type ~check:true id decl env)
    decls env

let compute_property
: ('prop, 'req) property -> Env.t ->
  (Ident.t * decl) list -> ('req * 'req option) list -> (Ident.t * decl) list
= fun property env decls required ->
  (* [decls] and [required] must be lists of the same size,
     with [required] containing the requirement for the corresponding
     declaration in [decls], and if the declaration has an unboxed version,
     the corresponding requirement for that as well. *)
  let props =
    List.map (fun (_id, (decl : decl)) ->
        property.default decl,
        Option.map (fun d -> property.default d) decl.type_unboxed_version)
      decls
  in
  let rec compute_fixpoint props =
    let new_decls =
      List.map2 (fun (id, (decl : decl)) (prop, prop_u) ->
        let type_unboxed_version = Option.map (fun d ->
            property.update_decl d (Option.get prop_u))
          decl.type_unboxed_version
        in
        (id, { (property.update_decl decl prop) with type_unboxed_version }))
        decls props in
    let new_env = add_types_to_env new_decls env in
    let update_prop decl prop req =
      property.merge ~prop ~new_prop:(property.compute new_env decl req)
    in
    let new_props =
      List.map2
        (fun (_id, (decl : decl)) ((prop, prop_u), (req, req_u)) ->
           update_prop decl prop req,
           Option.map (fun d ->
               update_prop d (Option.get prop_u) (Option.get req_u))
             decl.type_unboxed_version)
        new_decls (List.combine props required) in
    if not (List.for_all2
              (fun (prop, prop_u) (prop', prop_u') ->
                 property.eq prop prop'
                 && Option.equal property.eq prop_u prop_u')
              props new_props)
    then compute_fixpoint new_props
    else begin
      List.iter2
        (fun (id, decl) req -> property.check new_env id decl req)
        new_decls required;
      new_decls
    end
  in
  compute_fixpoint props

let compute_property_noreq property env decls =
  let req = List.map
    (fun (_, (d : decl)) -> (), Option.map (fun _ -> ()) d.type_unboxed_version)
    decls
  in
  compute_property property env decls req
