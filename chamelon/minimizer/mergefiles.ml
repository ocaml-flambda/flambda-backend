(* Minimizer : Merge files together  *)

open Typedtree
open Utils
open Ident
open Longident
open Path
open Flatteningmodules
open Compat

(*We'll have to replace the "open n2" by "open n1"
    and then change the names inside*)

(*When merging n1 in n2, remove "open n1" in n2*)
let remove_open n1 =
  {
    Tast_mapper.default with
    structure =
      (fun mapper str ->
        (* Format.printf "before removing : %n @." (List.length str.str_items); *)
        {
          str with
          str_items =
            (let l =
               List.rev
                 (List.fold_left
                    (fun l str_it ->
                      let str_it = mapper.structure_item mapper str_it in
                      match str_it.str_desc with
                      | Tstr_open decl -> (
                          match decl.open_expr.mod_desc with
                          | Tmod_ident (Pident _, { txt = Lident n_txt; _ }) ->
                              (* Format.printf "INMERGE : %s AND %s" n1 n_txt ; *)
                              if n1 = n_txt then l else str_it :: l
                          | _ -> str_it :: l)
                      | _ -> str_it :: l)
                    [] str.str_items)
             in
             (* Format.printf "after removing : %n @." (List.length l); *) l);
        });
  }

(*When merging n1 in n2, replace "open n1" by "open n2" in other files*)
let replace_open n1 n2 =
  {
    Tast_mapper.default with
    structure_item =
      (fun _ str_it ->
        match str_it.str_desc with
        | Tstr_open decl -> (
            match decl.open_expr.mod_desc with
            | Tmod_ident (Pident _, ({ txt = Lident n_txt; _ } as li)) ->
                if n1 = n_txt then
                  {
                    str_it with
                    str_desc =
                      Tstr_open
                        {
                          decl with
                          open_expr =
                            {
                              decl.open_expr with
                              mod_desc =
                                Tmod_ident
                                  ( Pident (create_local n2),
                                    { li with txt = Lident n2 } );
                            };
                        };
                  }
                else str_it
            | _ -> str_it)
        | _ -> str_it);
  }

(*Replace name._ by _*)
let rec remove_head_path p =
  match p with
  | Pdot (Pident _id, s) ->
      (* Format.printf "Remove head : %s @." (Ident.name id) ;  *)
      Pident (Ident.create_local s)
  | Pdot (p, s) -> Pdot (remove_head_path p, s)
  | _ -> p

let rec remove_head_lid li =
  match li with
  | Ldot (Lident _, s) -> Lident s
  | Ldot (li, s) -> Ldot (remove_head_lid li, s)
  | _ -> li

let rec print_path p =
  match p with
  | Pident id -> Ident.name id
  | Pdot (p, s) -> print_path p ^ "." ^ s
  | Papply (t1, t2) -> "app " ^ print_path t1 ^ " " ^ print_path t2
  | Pextra_ty _ -> Format.asprintf "%a" Path.print p

let rec print_path_lid p =
  match p with
  | Lident id -> id
  | Ldot (p, s) -> print_path_lid p ^ "." ^ s
  | Lapply (t1, t2) -> "app " ^ print_path_lid t1 ^ " " ^ print_path_lid t2

let remove_file_name_mapper n1 =
  let n1 = String.capitalize_ascii (String.sub n1 0 (String.length n1 - 3)) in
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (p, li, vd, id) -> (
            match p with
            | Pdot (p1, _) ->
                if Ident.name (Path.head p1) = n1 then
                  {
                    e with
                    exp_desc =
                      mkTexp_ident ~id
                        (remove_head_path p, { li with txt = li.txt }, vd);
                  }
                else e
            | _ -> e)
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (p, li, c) -> (
            match p with
            | Pdot (p1, _) ->
                if Ident.name (Path.head p1) = n1 then
                  {
                    ct with
                    ctyp_desc =
                      Ttyp_constr
                        (remove_head_path p, { li with txt = li.txt }, c);
                  }
                else ct
            | _ -> ct)
        | _ -> Tast_mapper.default.typ mapper ct);
  }

let merge str1 str2 n1 n2 =
  (* Première gestion des scopes *)
  let l = ref [] in
  let add_mod_mapper = add_module_name_mapper n2 l in
  let rem_n1_mapper = remove_file_name_mapper n1 in
  let str2_fresh = add_mod_mapper.structure add_mod_mapper str2 in
  let str2_fresh = rem_n1_mapper.structure rem_n1_mapper str2_fresh in
  let rem_open_mapper =
    remove_open
      (String.capitalize_ascii (String.sub n1 0 (String.length n1 - 3)))
  in
  let str2_fresh = rem_open_mapper.structure rem_open_mapper str2_fresh in
  ({ str1 with str_items = str1.str_items @ str2_fresh.str_items }, !l)

let merge_first l =
  match l with
  | (n1, s1) :: (n2, s2) :: q ->
      let merged, to_rename = merge s1 s2 n1 n2 in
      ((n1, merged) :: q, n1, n2, to_rename)
  | _ -> failwith "not enough files to merge"

let add_to_command c l =
  List.fold_left (fun c (output, _) -> c ^ " " ^ output) c l

let merge_strategy c (names, strs) =
  (* Format.printf "merge : %n" (List.length names) ;  *)
  let paired = List.map2 (fun a b -> (a, b)) names strs in
  let merged = ref [] in
  let to_merge = ref paired in
  while List.length !to_merge > 1 do
    (* Merge the two first files *)
    let new_merged, new_file, merged_file, to_rename = merge_first !to_merge in
    (* Correct the scopes in other files  *)
    let correcter nstr =
      List.fold_left
        (fun nstr id ->
          let to_find =
            Pdot (Pident (create_local merged_file), Ident.name id)
          in
          let replacement =
            Pident (create_local (merged_file ^ "_" ^ Ident.name id))
          in
          let rep_mapper = replace_path to_find replacement in
          let nstr = rep_mapper.structure rep_mapper nstr in
          let correct_open_mappen = replace_open new_file merged_file in
          correct_open_mappen.structure correct_open_mappen nstr)
        nstr to_rename
    in
    let new_str =
      List.map (fun (name, str) -> (name, correcter str)) new_merged
    in
    List.iter (fun (name, str) -> update_single name str) new_str;
    let c = add_to_command (add_to_command c (List.rev !merged)) !to_merge in
    if raise_error c then (
      to_merge := new_merged;
      ignore (Sys.command ("rm " ^ merged_file)))
    else
      let h, q = match !to_merge with h :: q -> (h, q) | [] -> assert false in
      List.iter (fun (name, str) -> update_single name str) !to_merge;
      to_merge := q;
      merged := h :: !merged
  done;
  let final_merge =
    match !to_merge with
    | [] -> List.rev !merged
    | [ h ] -> List.rev (h :: !merged)
    | _ -> failwith "loop condition failed in mergefiles"
  in
  List.fold_left
    (fun (l1, l2) (a, b) -> (a :: l1, b :: l2))
    ([], []) final_merge
