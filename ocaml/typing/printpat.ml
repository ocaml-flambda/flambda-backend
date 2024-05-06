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

(* Values as patterns pretty printer *)

open Asttypes
open Typedtree
open Types
open Format

let is_cons = function
| {cstr_name = "::"} -> true
| _ -> false

let pretty_const c = match c with
| Const_null -> "null"
| Const_int i -> Printf.sprintf "%d" i
| Const_char c -> Printf.sprintf "%C" c
| Const_string (s, _, _) -> Printf.sprintf "%S" s
| Const_float f -> Printf.sprintf "%s" f
| Const_float32 f -> Printf.sprintf "%s" f
| Const_unboxed_float f -> Printf.sprintf "%s" (Misc.format_as_unboxed_literal f)
| Const_int32 i -> Printf.sprintf "%ldl" i
| Const_int64 i -> Printf.sprintf "%LdL" i
| Const_nativeint i -> Printf.sprintf "%ndn" i
| Const_unboxed_int32 i ->
  Printf.sprintf "%sl" (Misc.format_as_unboxed_literal (Int32.to_string i))
| Const_unboxed_int64 i ->
  Printf.sprintf "%sL" (Misc.format_as_unboxed_literal (Int64.to_string i))
| Const_unboxed_nativeint i ->
  Printf.sprintf "%sn" (Misc.format_as_unboxed_literal (Nativeint.to_string i))

let pretty_extra ppf (cstr, _loc, _attrs) pretty_rest rest =
  match cstr with
  | Tpat_unpack ->
     fprintf ppf "@[(module %a)@]" pretty_rest rest
  | Tpat_constraint _ ->
     fprintf ppf "@[(%a : _)@]" pretty_rest rest
  | Tpat_type _ ->
     fprintf ppf "@[(# %a)@]" pretty_rest rest
  | Tpat_open _ ->
     fprintf ppf "@[(# %a)@]" pretty_rest rest

let rec pretty_val : type k . _ -> k general_pattern -> _ = fun ppf v ->
  match v.pat_extra with
    | extra :: rem ->
       pretty_extra ppf extra
         pretty_val { v with pat_extra = rem }
    | [] ->
  match v.pat_desc with
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var (x,_,_,_) -> fprintf ppf "%s" (Ident.name x)
  | Tpat_constant c -> fprintf ppf "%s" (pretty_const c)
  | Tpat_tuple vs ->
      fprintf ppf "@[(%a)@]" (pretty_list pretty_labeled_val ",") vs
  | Tpat_construct (_, cstr, [], _) ->
      fprintf ppf "%s" cstr.cstr_name
  | Tpat_construct (_, cstr, [w], None) ->
      fprintf ppf "@[<2>%s@ %a@]" cstr.cstr_name pretty_arg w
  | Tpat_construct (_, cstr, vs, vto) ->
      let name = cstr.cstr_name in
      begin match (name, vs, vto) with
        ("::", [v1;v2], None) ->
          fprintf ppf "@[%a::@,%a@]" pretty_car v1 pretty_cdr v2
      | (_, _, None) ->
          fprintf ppf "@[<2>%s@ @[(%a)@]@]" name (pretty_vals ",") vs
      | (_, _, Some ([], _t)) ->
          fprintf ppf "@[<2>%s@ @[(%a : _)@]@]" name (pretty_vals ",") vs
      | (_, _, Some (vl, _t)) ->
          let vars = List.map (fun x -> Ident.name x.txt) vl in
          fprintf ppf "@[<2>%s@ (type %s)@ @[(%a : _)@]@]"
            name (String.concat " " vars) (pretty_vals ",") vs
      end
  | Tpat_variant (l, None, _) ->
      fprintf ppf "`%s" l
  | Tpat_variant (l, Some w, _) ->
      fprintf ppf "@[<2>`%s@ %a@]" l pretty_arg w
  | Tpat_record (lvs,_) ->
      let filtered_lvs = List.filter
          (function
            | (_,_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
            | _ -> true) lvs in
      begin match filtered_lvs with
      | [] -> fprintf ppf "_"
      | (_, lbl, _) :: q ->
          let elision_mark ppf =
            (* we assume that there is no label repetitions here *)
             if Array.length lbl.lbl_all > 1 + List.length q then
               fprintf ppf ";@ _@ "
             else () in
          fprintf ppf "@[{%a%t}@]"
            pretty_lvals filtered_lvs elision_mark
      end
  | Tpat_array (am, _arg_sort, vs) ->
      let punct = if Types.is_mutable am then '|' else ':' in
      fprintf ppf "@[[%c %a %c]@]" punct (pretty_vals " ;") vs punct
  | Tpat_lazy v ->
      fprintf ppf "@[<2>lazy@ %a@]" pretty_arg v
  | Tpat_alias (v, x, _, _, _) ->
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.print x
  | Tpat_value v ->
      fprintf ppf "%a" pretty_val (v :> pattern)
  | Tpat_exception v ->
      fprintf ppf "@[<2>exception@ %a@]" pretty_arg v
  | Tpat_or _ ->
      fprintf ppf "@[(%a)@]" pretty_or v

and pretty_car ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [_ ; _], None)
    when is_cons cstr ->
      fprintf ppf "(%a)" pretty_val v
| _ -> pretty_val ppf v

and pretty_cdr ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [v1 ; v2], None)
    when is_cons cstr ->
      fprintf ppf "%a::@,%a" pretty_car v1 pretty_cdr v2
| _ -> pretty_val ppf v

and pretty_arg ppf v = match v.pat_desc with
| Tpat_construct (_,_,_::_,None)
| Tpat_variant (_, Some _, _) -> fprintf ppf "(%a)" pretty_val v
|  _ -> pretty_val ppf v

and pretty_or : type k . _ -> k general_pattern -> _ = fun ppf v ->
  match v.pat_desc with
  | Tpat_or (v,w,_) ->
      fprintf ppf "%a|@,%a" pretty_or v pretty_or w
  | _ -> pretty_val ppf v

and pretty_list : type k . (_ -> k -> _) -> _ -> _ -> k list -> _ =
  fun print_val sep ppf ->
    function
    | [] -> ()
    | [v] -> print_val ppf v
    | v::vs ->
        fprintf ppf "%a%s@ %a" print_val v sep (pretty_list print_val sep) vs

and pretty_vals sep = pretty_list pretty_val sep

and pretty_labeled_val ppf (l, p) =
  begin match l with
  | Some s -> fprintf ppf "~%s:" s
  | None -> ()
  end;
  pretty_val ppf p

and pretty_lvals ppf = function
  | [] -> ()
  | [_,lbl,v] ->
      fprintf ppf "%s=%a" lbl.lbl_name pretty_val v
  | (_, lbl,v)::rest ->
      fprintf ppf "%s=%a;@ %a"
        lbl.lbl_name pretty_val v pretty_lvals rest

let top_pretty ppf v =
  fprintf ppf "@[%a@]@?" pretty_val v

let pretty_pat p =
  top_pretty Format.str_formatter p ;
  prerr_string (Format.flush_str_formatter ())

type 'k matrix = 'k general_pattern list list

let pretty_line fmt =
  List.iter (fun p ->
    Format.fprintf fmt " <";
    top_pretty fmt p;
    Format.fprintf fmt ">";
  )

let pretty_matrix fmt (pss : 'k matrix) =
  Format.fprintf fmt "begin matrix\n" ;
  List.iter (fun ps ->
    pretty_line fmt ps ;
    Format.fprintf fmt "\n"
  ) pss;
  Format.fprintf fmt "end matrix\n%!"
