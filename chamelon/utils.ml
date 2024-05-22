open Path
open Str
open Dummy
open Cmt_format
open Typedtree
open Untypeast
open Compat

type ('a, 'b) minimizer = {
  minimizer_name : string;
  minimizer_func : (unit -> bool) -> 'a -> 'b -> 'a;
}

let error_str = ref "Misc.Fatal_error"

exception Not_implemented

module Smap = Stdlib.Map.Make (String)

let is_attr names (attr : attribute) = List.mem attr.attr_name.txt names

(* ______ id replacement mapper ______ *)

let replace_id_exp_desc id to_replace =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then
              { e with exp_desc = to_replace }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
  }

let rec path_eq p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> Ident.name id1 = Ident.name id2
  | Pdot (t1, s1), Pdot (t2, s2) -> path_eq t1 t2 && s1 = s2
  | Papply (t11, t12), Papply (t21, t22) -> path_eq t11 t21 && path_eq t12 t22
  | _ -> false

(** [replace_path path n_path] is a mapper replacing each occurence of the path [path] by [n_path]*)
let replace_path path n_path =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (p1, id_l, vd, id) ->
            if path_eq path p1 then
              {
                e with
                exp_desc =
                  mkTexp_ident ~id
                    (n_path, { id_l with txt = Lident (Path.name n_path) }, vd);
              }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (p1, id_l, c) ->
            if path_eq path p1 then
              {
                ct with
                ctyp_desc =
                  Ttyp_constr
                    ( n_path,
                      { id_l with txt = Lident (Path.name n_path) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | Ttyp_class (p1, id_l, c) ->
            if path_eq path p1 then
              {
                ct with
                ctyp_desc =
                  Ttyp_class
                    ( n_path,
                      { id_l with txt = Lident (Path.name n_path) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct);
  }

(** [replace_id id n_id] is a mapper replacing each occurence of the ident [id] by [n_id]*)
let replace_id id n_id =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, id_l, vd, e_id) ->
            if Ident.same (Path.head path) id then
              {
                e with
                exp_desc =
                  mkTexp_ident ~id:e_id
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      vd );
              }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
    typ =
      (fun mapper ct ->
        match ct.ctyp_desc with
        | Ttyp_constr (path, id_l, c) ->
            if Ident.same (Path.head path) id then
              {
                ct with
                ctyp_desc =
                  Ttyp_constr
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | Ttyp_class (path, id_l, c) ->
            if Ident.same (Path.head path) id then
              {
                ct with
                ctyp_desc =
                  Ttyp_class
                    ( Pident n_id,
                      { id_l with txt = Lident (Ident.name n_id) },
                      List.map (mapper.typ mapper) c );
              }
            else Tast_mapper.default.typ mapper ct
        | _ -> Tast_mapper.default.typ mapper ct);
  }

(* ______ Compilation utils ______*)

let make_command c output_files =
  List.fold_left (fun c output -> c ^ " " ^ output) c output_files

let compile (filename : string) compile_command =
  Sys.command (!compile_command ^ " " ^ filename)

let raise_error compile_command =
  Sys.command (compile_command ^ " 2>&1 | grep " ^ !error_str ^ " > /dev/null")
  = 0

let generate_cmt typing_command (filenames : string list) =
  let params = List.fold_left (fun s output -> s ^ " " ^ output) "" filenames in
  if
    Sys.command (typing_command ^ " -bin-annot -stop-after typing " ^ params)
    = 0
  then (
    let l =
      List.map
        (fun s -> read_cmt (String.sub s 0 (String.length s - 3) ^ ".cmt"))
        filenames
    in
    List.iter
      (fun s ->
        Stdlib.ignore
          (Sys.command ("rm " ^ String.sub s 0 (String.length s - 3) ^ ".cm*")))
      filenames;
    l)
  else failwith "Fail generating.cmt"

let extract_cmt = function
  | Implementation type_struct -> type_struct
  | Partial_implementation _ | Packed _ | Interface _ | Partial_interface _ ->
      raise Not_implemented

let rep_sth = global_replace (regexp_string "*sth*") "__sth__"
let rep_opt = global_replace (regexp_string "*opt*") "__opt__"
let rep_predef = global_replace (regexp_string "( *predef* ).") ""
let rep_def = global_replace (regexp_string "[@#default ]") ""
let fix s = rep_def (rep_predef (rep_opt (rep_sth s)))

let update_single name str =
  let oc = open_out name in
  let parse_tree = fix (Pprintast.string_of_structure (untype_structure str)) in
  output_string oc parse_tree;
  flush oc;
  close_out oc

(** [add_def str] adds dummy1, dummy2 and ignore definitions, needed by some minmizers, in [str]*)
let add_def str =
  {
    str with
    str_items = dummy1_def :: dummy2_def :: ignore_def :: str.str_items;
  }

(** [update_output map] replaces the content of each file
    by its associated structure in [map] *)
let update_output map = Smap.iter update_single (Smap.map add_def map)

let save_outputs map =
  Smap.iter (fun name str -> update_single (name ^ ".tmp") (add_def str)) map
