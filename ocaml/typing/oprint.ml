(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree

exception Ellipsis

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> fprintf ppf "..."

let print_lident ppf = function
  | "::" -> pp_print_string ppf "(::)"
  | s -> pp_print_string ppf s

let rec print_ident ppf =
  function
    Oide_ident s -> print_lident ppf s.printed_name
  | Oide_dot (id, s) ->
      print_ident ppf id; pp_print_char ppf '.'; print_lident ppf s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let out_ident = ref print_ident

(* Check a character matches the [identchar_latin1] class from the lexer *)
let is_ident_char c =
  match c with
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214' | '\216'..'\246'
  | '\248'..'\255' | '\'' | '0'..'9' -> true
  | _ -> false

let all_ident_chars s =
  let rec loop s len i =
    if i < len then begin
      if is_ident_char s.[i] then loop s len (i+1)
      else false
    end else begin
      true
    end
  in
  let len = String.length s in
  loop s len 0

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  || not (all_ident_chars name)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    pp_print_string ppf name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'

let escape_string s =
  (* Escape only C0 control characters (bytes <= 0x1F), DEL(0x7F), '\\'
     and '"' *)
   let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := !n +
      (match String.unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | '\x00' .. '\x1F'
       | '\x7F' -> 4
       | _ -> 1)
  done;
  if !n = String.length s then s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to String.length s - 1 do
      begin match String.unsafe_get s i with
      | ('\"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | '\x00' .. '\x1F' | '\x7F' as c ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a mod 10));
      | c -> Bytes.unsafe_set s' !n c
      end;
      incr n
    done;
    Bytes.to_string s'
  end

let rec print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      print_elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_typlist print_elem sep ppf tyl

let print_label_type ppf =
  function
  | Some s ->
    pp_print_string ppf s;
    pp_print_string ppf ":";
  | None -> ()

let print_label ppf =
  function
  | Some s ->
    pp_print_string ppf "~";
    pp_print_string ppf s;
    pp_print_string ppf ":";
  | None -> ()

let rec print_labeled_typlist print_elem sep ppf =
  function
    [] -> ()
  | [label, ty] ->
      pp_open_box ppf 0;
      print_label_type ppf label;
      print_elem ppf ty;
      pp_close_box ppf ()
  | (label, ty) :: tyl ->
      pp_open_box ppf 0;
      print_label_type ppf label;
      print_elem ppf ty;
      pp_close_box ppf ();
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_labeled_typlist print_elem sep ppf tyl

let print_out_string ppf s =
  let not_escaped =
    (* let the user dynamically choose if strings should be escaped: *)
    match Sys.getenv_opt "OCAMLTOP_UTF_8" with
    | None -> true
    | Some x ->
        match bool_of_string_opt x with
        | None -> true
        | Some f -> f in
  if not_escaped then
    fprintf ppf "\"%s\"" (escape_string s)
  else
    fprintf ppf "%S" s

external float32_format : string -> Obj.t -> string = "caml_format_float32"

let float32_to_string f = Stdlib.valid_float_lexem (float32_format "%.9g" f)

let print_out_value ppf tree =
  let rec print_tree_1 ppf =
    function
    | Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f ->
        parenthesize_if_neg ppf "%s" (float_repres f)
                                     (f < 0.0 || 1. /. f = neg_infinity)
    | Oval_float32 f ->
        let s = float32_to_string f in
        parenthesize_if_neg ppf "%ss" s (String.starts_with ~prefix:"-" s)
    | Oval_string (_,_, Ostr_bytes) as tree ->
      pp_print_char ppf '(';
      print_simple_tree ppf tree;
      pp_print_char ppf ')';
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> pp_print_string ppf (float_repres f)
    | Oval_float32 f -> fprintf ppf "%ss" (float32_to_string f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string (s, maxlen, kind) ->
       begin try
         let len = String.length s in
         let maxlen = max maxlen 8 in (* always show a little prefix *)
         let s = if len > maxlen then String.sub s 0 maxlen else s in
         begin match kind with
         | Ostr_bytes -> fprintf ppf "Bytes.of_string %S" s
         | Ostr_string -> print_out_string ppf s
         end;
         (if len > maxlen then
            fprintf ppf
              "... (* string length %d; truncated *)" len
         )
          with
          Invalid_argument _ (* "String.create" *)-> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array (tl, am) ->
        let sigil = match am with
          | Mutable   -> '|'
          | Immutable -> ':'
        in
        fprintf ppf "@[<2>[%c%a%c]@]"
          sigil (print_tree_list print_tree_1 ";") tl sigil
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> pp_print_string ppf s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_labeled_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ";@ ";
        fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  and print_labeled_tree_list print_item sep ppf labeled_tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | (label, tree) :: labeled_tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_label ppf label;
          print_item ppf tree;
          print_list false ppf labeled_tree_list
    in
    cautious (print_list true) ppf labeled_tree_list
  in
  cautious print_tree_1 ppf tree

let out_value = ref print_out_value

(* Types *)

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_var = Pprintast.tyvar
let ty_var ~non_gen ppf s =
  pr_var ppf (if non_gen then "_" ^ s else s)

let print_jkind_with_modes ppf print_jkind base modes =
  fprintf ppf "%a mod @[%a@]" print_jkind base
          (print_list (fun ppf -> fprintf ppf "%s") (fun ppf -> fprintf ppf "@ "))
          modes

let print_out_jkind ppf = function
  | Ojkind_const { base; modal_bounds=[] } ->
    fprintf ppf "%s" base
  | Ojkind_const { base; modal_bounds=_::_ as modal_bounds } ->
    print_jkind_with_modes ppf (fun ppf -> fprintf ppf "%s") base modal_bounds
  | Ojkind_var v -> fprintf ppf "%s" v
  | Ojkind_user jkind ->
    let rec print_out_jkind_user ppf = function
      | Ojkind_user_default -> fprintf ppf "_"
      | Ojkind_user_abbreviation abbrev -> fprintf ppf "%s" abbrev
      | Ojkind_user_mod (base, modes) ->
        print_jkind_with_modes ppf print_out_jkind_user base modes
      | Ojkind_user_with _ | Ojkind_user_kind_of _ ->
        failwith "XXX unimplemented jkind syntax"
    in
    print_out_jkind_user ppf jkind

let print_out_jkind_annot ppf = function
  | None -> ()
  | Some lay -> fprintf ppf "@ : %a" print_out_jkind lay

let pr_var_jkind ppf (v, l) = match l with
    | None -> pr_var ppf v
    | Some lay -> fprintf ppf "(%a : %a)"
                    pr_var v
                    print_out_jkind lay

let pr_var_jkinds =
  print_list pr_var_jkind (fun ppf -> fprintf ppf "@ ")

(* NON-LEGACY MODES
  Here, we are printing mode annotations even if the mode extension is
  disabled. Mode extension being disabled means mode annotations are
  disallowed in parsing of this file, but non-legacy modes might still pop
  up. For example, the current file might cite values from other files that
  mention non-legacy modes *)
let print_out_mode_legacy ppf = function
  | Omd_local -> fprintf ppf "local_"
  | Omd_unique -> fprintf ppf "unique_"
  | Omd_once -> fprintf ppf "once_"

let print_out_mode_new = pp_print_string

let print_out_mode_legacy_space ppf m =
  print_out_mode_legacy ppf m;
  pp_print_space ppf ()

let print_out_modes_legacy ppf l =
  pp_print_list print_out_mode_legacy_space ppf l

let print_out_modes_new ppf l =
  (match l with
  | [] -> ()
  | _ -> pp_print_string ppf " @ ");
  pp_print_list ~pp_sep:pp_print_space print_out_mode_new ppf l

let partition_modes l =
  List.partition_map
    (function
    | Omd_legacy m -> Left m
    | Omd_new m -> Right m
    ) l

(* Labeled tuples with the first element labeled sometimes require parens. *)
let is_initially_labeled_tuple ty =
  match ty with
  | Otyp_tuple ((Some _, _) :: _) -> true
  | _ -> false

let print_out_modality_legacy ppf = function
  | Ogf_global -> Format.fprintf ppf "global_"

let print_out_modality ppf = function
  | Ogf_legacy m -> print_out_modality_legacy ppf m
  | Ogf_new m -> pp_print_string ppf m

let print_out_modalities_new ppf l =
  match l with
  | [] -> ()
  | _ ->
    pp_print_space ppf ();
    pp_print_string ppf "@@";
    pp_print_space ppf ();
    pp_print_list ~pp_sep:pp_print_space pp_print_string ppf l

let print_out_modalities_legacy =
  pp_print_list
  (fun ppf m ->
    print_out_modality_legacy ppf m;
    pp_print_space ppf ())

let partition_modalities l =
  List.partition_map (function
  | Ogf_legacy m -> Left m
  | Ogf_new m -> Right m
  ) l

let rec print_out_type_0 ppf =
  function
  | Otyp_alias {non_gen; aliased; alias } ->
    fprintf ppf "@[%a@ as %a@]"
      print_out_type_0 aliased
      (ty_var ~non_gen) alias
  | Otyp_poly ([], ty) ->
      print_out_type_0 ppf ty  (* no "." if there are no vars *)
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_var_jkinds sl
        print_out_type_0 ty
  | ty ->
      print_out_type_1 ppf ty

(* We must parenthesize a labeled tuple with the first element labeled when:
   - It is an argument to a function ([~arg])
   - Or, there is at least one mode to print.
 *)
and print_out_type_mode ~arg mode ppf ty =
  let m_legacy, m_new = partition_modes mode in
  let has_modes =
    match m_legacy with
    | [] -> false
    | _ -> true
  in
  let parens =
    is_initially_labeled_tuple ty
    && (arg || has_modes)
  in
  print_out_modes_legacy ppf m_legacy;
  if parens then
    pp_print_char ppf '(';
  print_out_type_2 ppf ty;
  if parens then
    pp_print_char ppf ')';
  print_out_modes_new ppf m_new

and print_out_type_1 ppf =
  function
  | Otyp_arrow (lab, am, ty1, rm, ty2) ->
      pp_open_box ppf 0;
      let print_type () = print_out_arg am ppf ty1 in
      (match lab with
      | Nolabel -> print_type ()
      | Labelled l ->
          pp_print_string ppf l; pp_print_char ppf ':'; print_type ()
      | Position l ->
          pp_print_string ppf l;
          pp_print_string ppf ":[%call_pos]"
      | Optional l ->
          pp_print_string ppf ("?" ^ l); pp_print_char ppf ':'; print_type ());
      pp_print_string ppf " ->";
      pp_print_space ppf ();
      print_out_ret rm ppf ty2;
      pp_close_box ppf ()
  | Otyp_functor (id, (p, fl), ty) ->
      pp_open_box ppf 0;
      pp_print_string ppf "{";
      print_ident ppf id;
      pp_print_string ppf " : ";
      print_ident ppf p;
      let first = ref true in
      List.iter
        (fun (s, t) ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        fl;
      pp_print_string ppf "} ->";
      pp_print_space  ppf ();
      print_out_type_1 ppf ty;
      pp_close_box ppf ()
  | ty -> print_out_type_2 ppf ty

and print_out_arg am ppf ty =
  print_out_type_mode ~arg:true am ppf ty

and print_out_ret rm ppf =
  function
  | Otyp_arrow _ as ty ->
    begin match rm with
    | Orm_not_arrow _ -> assert false
    | Orm_no_parens ->
      print_out_type_1 ppf ty
    | Orm_parens rm ->
      let m_legacy, m_new = partition_modes rm in
      print_out_modes_legacy ppf m_legacy;
      pp_print_char ppf '(';
      print_out_type_1 ppf ty;
      pp_print_char ppf ')';
      print_out_modes_new ppf m_new
    end
  | ty ->
    match rm with
    | Orm_not_arrow rm -> print_out_type_mode ~arg:false rm ppf ty
    | _ -> assert false

and print_out_type_2 ppf =
  function
  | Otyp_tuple tyl ->
      fprintf
        ppf "@[<0>%a@]" (print_labeled_typlist print_simple_out_type " *") tyl
  | ty -> print_out_type_3 ppf ty
and print_out_type_3 ppf =
  function
    Otyp_class (id, tyl) ->
      fprintf ppf "@[%a#%a@]" print_typargs tyl print_ident id
  | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      print_typargs ppf tyl;
      print_ident ppf id;
      pp_close_box ppf ()
  | Otyp_object {fields; open_row} ->
      fprintf ppf "@[<2>< %a >@]" (print_fields open_row) fields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (non_gen, s) -> ty_var ~non_gen ppf s
  | Otyp_variant (row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_typ typ ->
           print_simple_out_type ppf typ
      in
      fprintf ppf "@[<hov>[%s@[<hv>@[<hv>%a@]%a@]@ ]@]"
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _
  | Otyp_functor _ | Otyp_tuple _ as ty ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_out_type_0 ppf ty;
      pp_print_char ppf ')';
      pp_close_box ppf ()
  | Otyp_abstract | Otyp_open
  | Otyp_sum _ | Otyp_manifest (_, _) -> ()
  | Otyp_record lbls -> print_record_decl ppf lbls
  | Otyp_module (p, fl) ->
      fprintf ppf "@[<1>(module %a" print_ident p;
      let first = ref true in
      List.iter
        (fun (s, t) ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        fl;
      fprintf ppf ")@]"
  | Otyp_attribute (t, attr) ->
      fprintf ppf "@[<1>(%a [@@%s])@]"
        print_out_type_0 t attr.oattr_name
  | Otyp_jkind_annot (t, lay) ->
    fprintf ppf "@[<1>(%a@ :@ %a)@]"
      print_out_type_0 t
      print_out_jkind lay
and print_out_type ppf typ =
  print_out_type_0 ppf typ
and print_simple_out_type ppf typ =
  print_out_type_3 ppf typ
and print_record_decl ppf lbls =
  fprintf ppf "{%a@;<1 -2>}"
    (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
and print_fields open_row ppf =
  function
    [] ->
      if open_row then fprintf ppf "..";
  | [s, t] ->
      fprintf ppf "%s : %a" s print_out_type t;
      if open_row then fprintf ppf ";@ ";
      print_fields open_row ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields open_row) l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> print_simple_out_type ppf ty1; pp_print_space ppf ()
  | tyl ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_typlist print_out_type "," ppf tyl;
      pp_print_char ppf ')';
      pp_close_box ppf ();
      pp_print_space ppf ()
and print_out_label ppf (name, mut, arg, gbl) =
  (* See the notes [NON-LEGACY MODES] *)
  let mut =
    match mut with
    | Om_immutable -> ""
    | Om_mutable None -> "mutable "
    | Om_mutable (Some s) -> "mutable(" ^ s ^ ") "
  in
  let m_legacy, m_new = partition_modalities gbl in
  fprintf ppf "@[<2>%s%a%s :@ %a%a@];"
    mut
    print_out_modalities_legacy m_legacy
    name
    print_out_type arg
    print_out_modalities_new m_new

let out_label = ref print_out_label

let out_modality = ref print_out_modality

let out_type = ref print_out_type

let out_type_args = ref print_typargs

(* Class types *)

let print_type_parameter ppf s =
  if s = "_" then fprintf ppf "_" else pr_var ppf s

let type_parameter ~in_parens ppf
      { oparam_name = ty; oparam_variance = var;
        oparam_injectivity = inj; oparam_jkind = lay } =
  let open Asttypes in
  let format_string : _ format = "%s%s%a%a" in
  let format_string : _ format = match lay with
    | Some _ when not in_parens -> "(" ^^ format_string ^^ ")"
    | _ -> format_string
  in
  fprintf ppf format_string
    (match var with Covariant -> "+" | Contravariant -> "-" | NoVariance ->  "")
    (match inj with Injective -> "!" | NoInjectivity -> "")
    print_type_parameter ty
    print_out_jkind_annot lay

let print_out_class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list (type_parameter ~in_parens:true)
           (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (print_typlist !out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_arrow (lab, ty, cty) ->
      let print_type = print_out_type_2 in
      let label, print_type = match lab with
        | Nolabel -> "", print_type
        | Labelled l -> l ^ ":", print_type
        | Position l -> l ^ ":", fun ppf _ -> pp_print_string ppf "[%call_pos]"
        | Optional l -> "?" ^ l ^ ":", print_type
      in
      fprintf ppf "@[%s%a ->@ %a@]"
        label
        print_type ty
        print_out_class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" !out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
    Ocsg_constraint (ty1, ty2) ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" !out_type ty1
        !out_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name !out_type ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %s%s%s :@ %a@]"
        (if mut then "mutable " else "")
        (if vr then "virtual " else "")
        name !out_type ty

let out_class_type = ref print_out_class_type

(* Signature *)

let out_module_type = ref (fun _ -> failwith "Oprint.out_module_type")
let out_sig_item = ref (fun _ -> failwith "Oprint.out_sig_item")
let out_signature = ref (fun _ -> failwith "Oprint.out_signature")
let out_type_extension = ref (fun _ -> failwith "Oprint.out_type_extension")
let out_functor_parameters =
  ref (fun _ -> failwith "Oprint.out_functor_parameters")

(* For anonymous functor arguments, the logic to choose between
   the long-form
     functor (_ : S) -> ...
   and the short-form
     S -> ...
   is as follows: if we are already printing long-form functor arguments,
   we use the long form unless all remaining functor arguments can use
   the short form. (Otherwise use the short form.)

   For example,
     functor (X : S1) (_ : S2) (Y : S3) (_ : S4) (_ : S5) -> sig end
   will get printed as
     functor (X : S1) (_ : S2) (Y : S3) -> S4 -> S5 -> sig end

   but
     functor (_ : S1) (_ : S2) (Y : S3) (_ : S4) (_ : S5) -> sig end
   gets printed as
     S1 -> S2 -> functor (Y : S3) -> S4 -> S5 -> sig end
*)

(* take a module type that may be a functor type,
   and return the longest prefix list of arguments
   that should be printed in long form. *)

let rec collect_functor_args acc = function
  | Omty_functor (param, mty_res) ->
      collect_functor_args (param :: acc) mty_res
  | non_functor -> (acc, non_functor)
let collect_functor_args mty =
  let l, rest = collect_functor_args [] mty in
  List.rev l, rest

let constructor_of_extension_constructor
    (ext : out_extension_constructor) : out_constructor
=
  {
    ocstr_name = ext.oext_name;
    ocstr_args = ext.oext_args;
    ocstr_return_type = ext.oext_ret_type;
  }

let split_anon_functor_arguments params =
  let rec uncollect_anonymous_suffix acc rest = match acc with
    | Some (None, mty_arg) :: acc ->
        uncollect_anonymous_suffix acc
          (Some (None, mty_arg) :: rest)
    | _ :: _ | [] ->
        (acc, rest)
  in
  let (acc, rest) = uncollect_anonymous_suffix (List.rev params) [] in
  (List.rev acc, rest)

let rec print_out_module_type ppf mty =
  print_out_functor ppf mty

and print_out_functor_parameters ppf l =
  let print_nonanon_arg ppf = function
    | None ->
        fprintf ppf "()"
    | Some (param, mty) ->
        fprintf ppf "(%s : %a)"
          (Option.value param ~default:"_")
          print_out_module_type mty
  in
  let rec print_args ppf = function
    | [] -> ()
    | Some (None, mty_arg) :: l ->
        fprintf ppf "%a ->@ %a"
          print_simple_out_module_type mty_arg
          print_args l
    | _ :: _ as non_anonymous_functor ->
        let args, anons = split_anon_functor_arguments non_anonymous_functor in
        fprintf ppf "@[<2>functor@ %a@]@ ->@ %a"
          (pp_print_list ~pp_sep:pp_print_space print_nonanon_arg) args
          print_args anons
  in
  print_args ppf l

and print_out_functor ppf t =
  let params, non_functor = collect_functor_args t in
  fprintf ppf "@[<2>%a%a@]"
    print_out_functor_parameters params
    print_simple_out_module_type non_functor
and print_simple_out_module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
     begin match sg with
       | [] -> fprintf ppf "sig end"
       | sg ->
          fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_out_signature sg
     end
  | Omty_alias id -> fprintf ppf "(module %a)" print_ident id
  | Omty_functor _ as non_simple ->
     fprintf ppf "(%a)" print_out_module_type non_simple
  | Omty_strengthen (mty, id, unaliasable) ->
     fprintf ppf "(%a with %a%s)"
       print_simple_out_module_type mty
       print_ident id
       (if unaliasable then " [@unaliasable]" else "")
and print_out_signature ppf =
  function
    [] -> ()
  | [item] -> !out_sig_item ppf item
  | Osig_typext(ext, Oext_first) :: items ->
      (* Gather together the extension constructors *)
      let rec gather_extensions acc items =
        match items with
            Osig_typext(ext, Oext_next) :: items ->
              gather_extensions
                (constructor_of_extension_constructor ext :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [constructor_of_extension_constructor ext]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "%a@ %a" !out_type_extension te print_out_signature items
  | item :: items ->
      fprintf ppf "%a@ %a" !out_sig_item item print_out_signature items
and print_out_sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ :@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name !out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ =@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name !out_class_type clt
  | Osig_typext (ext, Oext_exception) ->
      fprintf ppf "@[<2>exception %a@]"
        print_out_constr (constructor_of_extension_constructor ext)
  | Osig_typext (ext, _es) ->
      print_out_extension_constructor ppf ext
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name !out_module_type mty
  | Osig_module (name, Omty_alias id, _) ->
      fprintf ppf "@[<2>module %s =@ %a@]" name print_ident id
  | Osig_module (name, mty, rs) ->
      fprintf ppf "@[<2>%s %s :@ %a@]"
        (match rs with Orec_not -> "module"
                     | Orec_first -> "module rec"
                     | Orec_next -> "and")
        name !out_module_type mty
  | Osig_type(td, rs) ->
        print_out_type_decl
          (match rs with
           | Orec_not   -> "type nonrec"
           | Orec_first -> "type"
           | Orec_next  -> "and")
          ppf td
  | Osig_value { oval_name; oval_type; oval_modalities;
                 oval_prims; oval_attributes } ->
      let kwd = if oval_prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a%a%a@]" kwd value_ident oval_name
        !out_type oval_type
        print_out_modalities_new oval_modalities
        pr_prims oval_prims
        (fun ppf -> List.iter (fun a -> fprintf ppf "@ [@@@@%s]" a.oattr_name))
        oval_attributes
  | Osig_ellipsis ->
      fprintf ppf "..."

and print_out_type_decl kwd ppf td =
  let print_constraints ppf =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" !out_type ty1
           !out_type ty2)
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
      [] -> pp_print_string ppf td.otype_name
    | [param] -> fprintf ppf "@[%a@ %s@]"
                   (type_parameter ~in_parens:false) param td.otype_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list (type_parameter ~in_parens:true)
             (fun ppf -> fprintf ppf ",@ "))
          td.otype_params
          td.otype_name
  in
  let print_manifest ppf =
    function
      Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" !out_type ty
    | _ -> ()
  in
  let print_name_params ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest td.otype_type
  in
  let ty =
    match td.otype_type with
      Otyp_manifest (_, ty) -> ty
    | _ -> td.otype_type
  in
  let print_private ppf = function
    Asttypes.Private -> fprintf ppf " private"
  | Asttypes.Public -> ()
  in
  let print_unboxed ppf =
    if td.otype_unboxed then fprintf ppf " [%@%@unboxed]" else ()
  in
  let print_out_tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a %a"
        print_private td.otype_private
        print_record_decl lbls
  | Otyp_sum constrs ->
    let variants fmt constrs =
        if constrs = [] then fprintf fmt "|" else
        fprintf fmt "%a" (print_list print_out_constr
          (fun ppf -> fprintf ppf "@ | ")) constrs in
    fprintf ppf " =%a@;<1 2>%a"
      print_private td.otype_private variants constrs
  | Otyp_open ->
      fprintf ppf " =%a .."
        print_private td.otype_private
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private td.otype_private
        !out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a%a@]%t%t@]"
    print_name_params
    print_out_jkind_annot td.otype_jkind
    print_out_tkind ty
    print_constraints
    print_unboxed

and print_simple_out_gf_type ppf (ty, gf) =
  let m_legacy, m_new = partition_modalities gf in
  print_out_modalities_legacy ppf m_legacy;
  print_simple_out_type ppf ty;
  print_out_modalities_new ppf m_new

and print_out_constr_args ppf tyl =
  print_typlist print_simple_out_gf_type " *" ppf tyl

and print_out_constr ppf constr =
  let {
    ocstr_name = name;
    ocstr_args = tyl;
    ocstr_return_type = return_type;
  } = constr in
  let name =
    match name with
    | "::" -> "(::)"   (* #7200 *)
    | s -> s
  in
  match return_type with
  | None ->
      begin match tyl with
      | [] ->
          pp_print_string ppf name
      | _ ->
          fprintf ppf "@[<2>%s of@ %a@]" name
            print_out_constr_args tyl
      end
  | Some (vars_jkinds, ret_type) ->
      fprintf ppf "@[<2>%s :@ " name;
      begin match vars_jkinds with
      | [] -> ()
      | _ -> fprintf ppf "@[<hov>%a.@]@ " pr_var_jkinds vars_jkinds
      end;
      begin match tyl with
      | [] ->
          fprintf ppf "%a@]" print_simple_out_type ret_type
      | _ ->
          fprintf ppf "%a -> %a@]"
            print_out_constr_args tyl print_simple_out_type ret_type
      end

and print_out_extension_constructor ppf ext =
  let print_extended_type ppf =
      match ext.oext_type_params with
        [] -> fprintf ppf "%s" ext.oext_type_name
      | [ty_param] ->
        fprintf ppf "@[%a@ %s@]"
          print_type_parameter
          ty_param
          ext.oext_type_name
      | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          ext.oext_type_params
          ext.oext_type_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if ext.oext_private = Asttypes.Private then " private" else "")
    print_out_constr
    (constructor_of_extension_constructor ext)

and print_out_type_extension ppf te =
  let print_extended_type ppf =
    match te.otyext_params with
      [] -> fprintf ppf "%s" te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %s@]"
        print_type_parameter param
        te.otyext_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          te.otyext_params
          te.otyext_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if te.otyext_private = Asttypes.Private then " private" else "")
    (print_list print_out_constr (fun ppf -> fprintf ppf "@ | "))
     te.otyext_constructors

let out_constr = ref print_out_constr
let out_constr_args = ref print_out_constr_args
let _ = out_module_type := print_out_module_type
let _ = out_signature := print_out_signature
let _ = out_sig_item := print_out_sig_item
let _ = out_type_extension := print_out_type_extension
let _ = out_functor_parameters := print_out_functor_parameters

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> match Printexc.use_printers exn with
        | None -> fprintf ppf "@[Exception:@ %a.@]@." !out_value outv
        | Some s -> fprintf ppf "@[Exception:@ %s@]@." s

let rec print_items ppf =
  function
    [] -> ()
  | (Osig_typext(ext, Oext_first), None) :: items ->
      (* Gather together extension constructors *)
      let rec gather_extensions acc items =
        match items with
            (Osig_typext(ext, Oext_next), None) :: items ->
              gather_extensions
                (constructor_of_extension_constructor ext :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [constructor_of_extension_constructor ext]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "@[%a@]" !out_type_extension te;
        if items <> [] then fprintf ppf "@ %a" print_items items
  | (tree, valopt) :: items ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" !out_sig_item tree
            !out_value v
      | None -> fprintf ppf "@[%a@]" !out_sig_item tree
      end;
      if items <> [] then fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." !out_type ty !out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv

let out_phrase = ref print_out_phrase
