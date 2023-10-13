(* falt-mod : flattening modules *)

open Utils
open Ident
open Path
open Typedtree
open Tast_mapper
open Compat

(** [remove_module id] is a mapper that replaces every occurence of the module [id] in names,
    such that [id].name becomes "id_"^name*)
let remove_module id =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, t_loc, vd, e_id) -> (
            match (path, t_loc.txt) with
            | Pdot (Pident id1, id2), Ldot (_, lid2) ->
                if Ident.same id id1 then
                  {
                    e with
                    exp_desc =
                      mkTexp_ident ~id:e_id
                        ( Pident
                            (Ident.create_local
                               (String.lowercase_ascii (Ident.name id1)
                               ^ "_" ^ id2)),
                          {
                            t_loc with
                            txt =
                              Lident
                                (String.lowercase_ascii (Ident.name id1)
                                ^ "_" ^ lid2);
                          },
                          vd );
                  }
                else Tast_mapper.default.expr mapper e
            | _ -> Tast_mapper.default.expr mapper e)
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (path, t_loc, tl) -> (
            match (path, t_loc.txt) with
            | Pdot (Pident id1, id2), Ldot (_, lid2) ->
                if Ident.same id id1 then
                  {
                    ct with
                    ctyp_desc =
                      Ttyp_constr
                        ( Pident
                            (Ident.create_local
                               (String.lowercase_ascii (Ident.name id1)
                               ^ "_" ^ id2)),
                          {
                            t_loc with
                            txt =
                              Lident
                                (String.lowercase_ascii (Ident.name id1)
                                ^ "_" ^ lid2);
                          },
                          tl );
                  }
                else Tast_mapper.default.typ mapper ct
            | _ -> Tast_mapper.default.typ mapper ct)
        | _ -> Tast_mapper.default.typ mapper ct);
  }

(** [replace_in_pat mod_name pat] replace every name in [pat] by mod_name^"_"^name*)
let rec replace_in_pat : type k. _ -> k general_pattern -> k general_pattern =
 fun mod_name pat ->
  {
    pat with
    pat_desc =
      (match view_tpat pat.pat_desc with
      | Tpat_var (id, str, p_id) ->
          mkTpat_var ~id:p_id
            ( create_local (mod_name ^ "_" ^ Ident.name id),
              { str with txt = mod_name ^ "_" ^ str.txt } )
      | Tpat_alias (p, id, str, p_id) ->
          mkTpat_alias ~id:p_id
            ( replace_in_pat mod_name p,
              create_local (mod_name ^ "_" ^ Ident.name id),
              { str with txt = mod_name ^ "_" ^ str.txt } )
      | Tpat_tuple (vl, id) ->
          mkTpat_tuple ~id (List.map (replace_in_pat mod_name) vl)
      | Tpat_array (vl, id) ->
          mkTpat_array ~id (List.map (replace_in_pat mod_name) vl)
      | O (Tpat_construct (a1, a2, vl, a3)) ->
          Tpat_construct (a1, a2, List.map (replace_in_pat mod_name) vl, a3)
      | O (Tpat_record (r, a1)) ->
          Tpat_record
            ( List.map
                (fun (e1, e2, pat) -> (e1, e2, replace_in_pat mod_name pat))
                r,
              a1 )
      | O (Tpat_or (p1, p2, a1)) ->
          Tpat_or (replace_in_pat mod_name p1, replace_in_pat mod_name p2, a1)
      | O (Tpat_lazy pat) -> Tpat_lazy (replace_in_pat mod_name pat)
      | O (Tpat_variant (lab, Some p, t)) ->
          Tpat_variant (lab, Some (replace_in_pat mod_name p), t)
      | O (Tpat_value _)
      (* p) -> as_computation_pattern (replace_in_pat mod_name p) *)
      | O (Tpat_any | Tpat_constant _ | Tpat_variant _ | Tpat_exception _) ->
          pat.pat_desc
      | O (Tpat_var _ | Tpat_alias _ | Tpat_array _ | Tpat_tuple _) ->
          assert false);
  }

(** [add_module_name_mapper mod_name l] is a mapper which stores in [l] type and value variables defined
   at top level, while replacing their name by mod_name^"_"^name *)
let add_module_name_mapper mod_name l =
  {
    Tast_mapper.default with
    structure_item =
      (fun _ str_it ->
        match str_it.str_desc with
        | Tstr_value (rf, vb_l) ->
            l :=
              List.fold_left
                (fun l vb -> Removedeadcode.var_from_pat vb.vb_pat.pat_desc l)
                !l vb_l;
            {
              str_it with
              str_desc =
                Tstr_value
                  ( rf,
                    List.map
                      (fun vb ->
                        { vb with vb_pat = replace_in_pat mod_name vb.vb_pat })
                      vb_l );
            }
        | Tstr_type (rf, td_l) ->
            l := List.fold_left (fun l td -> td.typ_id :: l) !l td_l;
            {
              str_it with
              str_desc =
                Tstr_type
                  ( rf,
                    List.map
                      (fun td ->
                        {
                          td with
                          typ_id =
                            create_local (mod_name ^ "_" ^ Ident.name td.typ_id);
                          typ_name =
                            {
                              td.typ_name with
                              txt = mod_name ^ "_" ^ td.typ_name.txt;
                            };
                        })
                      td_l );
            }
        | _ -> str_it);
  }

let rec find_structure mb =
  match mb.mod_desc with
  | Tmod_structure str -> Some str
  | Tmod_constraint (me, _, _, _) -> find_structure me
  | _ -> None

(** [flatten_modules_mapper is_modified bounds id vars] finds a module to suppress in [bounds]
    set [id] to the suppressed module name, [vars] to the set of variables from the suppressed module.
    [!is_modified] is false if such a module does not exist.  *)
let flatten_modules_mapper should_remove id vars =
  {
    Tast_mapper.default with
    structure =
      (fun _ str ->
        {
          str with
          str_items =
            List.rev
              (List.fold_left
                 (fun l str_it ->
                   match str_it.str_desc with
                   | Tstr_module mb -> (
                       let str = find_structure mb.mb_expr in
                       match (str, mb.mb_id) with
                       | Some str, Some mod_name ->
                           if should_remove () then (
                             id := mod_name;
                             let rep_mapper =
                               add_module_name_mapper
                                 (String.lowercase_ascii (Ident.name !id))
                                 vars
                             in
                             List.rev
                               (List.map
                                  (rep_mapper.structure_item rep_mapper)
                                  str.str_items)
                             @ l)
                           else str_it :: l
                       | _ -> str_it :: l)
                   | _ -> str_it :: l)
                 [] str.str_items);
        });
  }

let minimize should_remove map cur_name =
  (* /!\ Not adapted to dichotomy, we only modify one module at a time, FIXME! *)
  let id = ref (Ident.create_local "") in
  let to_change = ref [] in
  let mapper = flatten_modules_mapper should_remove id to_change in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  let mod_name = String.lowercase_ascii (Ident.name !id) in
  (* Format.printf "is_modified : %b with %s @." !is_modified mod_name;  *)
  if !to_change <> [] then
    let rep_mapper = remove_module !id in
    let nstr = rep_mapper.structure rep_mapper nstr in
    (* Correcter : module.name/name becoming module_name*)
    let corrected_str =
      List.fold_left
        (fun nstr id ->
          let rep_mapper =
            replace_id id (Ident.create_local (mod_name ^ "_" ^ Ident.name id))
          in
          let nstr = rep_mapper.structure rep_mapper nstr in
          let to_find =
            Pdot
              ( Pident (create_local (String.capitalize_ascii mod_name)),
                Ident.name id )
          in
          let replacement =
            Pident (create_local (mod_name ^ "_" ^ Ident.name id))
          in
          let rep_mapper = replace_path to_find replacement in
          rep_mapper.structure rep_mapper nstr)
        nstr !to_change
    in
    let nmap = Smap.add cur_name corrected_str map in
    (* Correction in multifiles *)
    let correcter nstr =
      List.fold_left
        (fun nstr id ->
          let origin_file =
            Pident
              (create_local
                 (String.capitalize_ascii
                    (String.sub cur_name 0 (String.length cur_name - 3))))
          in
          let to_find =
            Pdot
              ( Pdot (origin_file, String.capitalize_ascii mod_name),
                Ident.name id )
          in
          let replacement =
            Pdot (origin_file, mod_name ^ "_" ^ Ident.name id)
          in
          let rep_mapper = replace_path to_find replacement in
          rep_mapper.structure rep_mapper nstr)
        nstr !to_change
    in
    let nmap = Smap.map correcter nmap in
    (*The t list we are considering here is different*)
    nmap (*Final result*)
  else
    let nmap = Smap.add cur_name nstr map in
    nmap

let minimizer =
  { minimizer_name = "flatten-modules"; minimizer_func = minimize }
