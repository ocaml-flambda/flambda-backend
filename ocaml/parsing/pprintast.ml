(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Thomas Gazagnaire, OCamlPro                       *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*               Hongbo Zhang, University of Pennsylvania                 *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Original Code from Ber-metaocaml, modified for 3.12.0 and fixed *)
(* Printing code expressions *)
(* Authors:  Ed Pizzi, Fabrice Le Fessant *)
(* Extensive Rewrite: Hongbo Zhang: University of Pennsylvania *)
(* TODO more fine-grained precedence pretty-printing *)

open Asttypes
open Format
open Location
open Longident
open Parsetree

let prefix_symbols  = [ '!'; '?'; '~' ]
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
                      '$'; '%'; '#' ]

(* type fixity = Infix| Prefix  *)
let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

let letop s =
  String.length s > 3
  && s.[0] = 'l'
  && s.[1] = 'e'
  && s.[2] = 't'
  && List.mem s.[3] infix_symbols

let andop s =
  String.length s > 3
  && s.[0] = 'a'
  && s.[1] = 'n'
  && s.[2] = 'd'
  && List.mem s.[3] infix_symbols

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string  = function
  | "" -> `Normal
  | s when List.mem s special_infix_strings -> `Infix s
  | s when List.mem s.[0] infix_symbols -> `Infix s
  | s when List.mem s.[0] prefix_symbols -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | s when letop s -> `Letop s
  | s when andop s -> `Andop s
  | _ -> `Normal

let view_fixity_of_exp = function
  | {pexp_desc = Pexp_ident {txt=Lident l;_}; pexp_attributes = []} ->
      fixity_of_string l
  | _ -> `Normal

let is_infix  = function `Infix _ -> true | _  -> false
let is_mixfix = function `Mixfix _ -> true | _ -> false
let is_kwdop = function `Letop _ | `Andop _ -> true | _ -> false

let first_is c str =
  str <> "" && str.[0] = c
let last_is c str =
  str <> "" && str.[String.length str - 1] = c

let first_is_in cs str =
  str <> "" && List.mem str.[0] cs

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  let fix = fixity_of_string txt in
  is_infix fix
  || is_mixfix fix
  || is_kwdop fix
  || first_is_in prefix_symbols txt

(* some infixes need spaces around parens to avoid clashes with comment
   syntax *)
let needs_spaces txt =
  first_is '*' txt || last_is '*' txt

(* Turn an arbitrary variable name into a valid OCaml identifier by adding \#
  in case it is a keyword, or parenthesis when it is an infix or prefix
  operator. *)
let ident_of_name ppf txt =
  let format : (_, _, _) format =
    if Lexer.is_keyword txt then "\\#%s"
    else if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in fprintf ppf format txt

let ident_of_name_loc ppf s = ident_of_name ppf s.txt

let protect_longident ppf print_longident longprefix txt =
    if not (needs_parens txt) then
      fprintf ppf "%a.%a" print_longident longprefix ident_of_name txt
    else if needs_spaces txt then
      fprintf ppf "%a.(@;%s@;)" print_longident longprefix txt
    else
      fprintf ppf "%a.(%s)" print_longident longprefix txt

let is_curry_attr attr =
  attr.attr_name.txt = Jane_syntax.Arrow_curry.curry_attr_name

let filter_curry_attrs attrs =
  List.filter (fun attr -> not (is_curry_attr attr)) attrs

let has_non_curry_attr attrs =
  List.exists (fun attr -> not (is_curry_attr attr)) attrs

type space_formatter = (unit, Format.formatter, unit) format

let override = function
  | Override -> "!"
  | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | NoVariance -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

let type_injectivity = function
  | NoInjectivity -> ""
  | Injective -> "!"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple
  | `btrue
  | `bfalse ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ( {txt= Lident "()"; _},_) -> `tuple
  | Pexp_construct ( {txt= Lident "true"; _},_) -> `btrue
  | Pexp_construct ( {txt= Lident "false"; _},_) -> `bfalse
  | Pexp_construct ( {txt= Lident "[]";_},_) -> `nil
  | Pexp_construct ( {txt= Lident"::";_},Some _) ->
      let rec loop exp acc = match exp with
          | {pexp_desc=Pexp_construct ({txt=Lident "[]";_},_);
             pexp_attributes = []} ->
              (List.rev acc,true)
          | {pexp_desc=
             Pexp_construct ({txt=Lident "::";_},
                             Some ({pexp_desc= Pexp_tuple([e1;e2]);
                                    pexp_attributes = []}));
             pexp_attributes = []}
            ->
              loop e2 (e1::acc)
          | e -> (List.rev (e::acc),false) in
      let (ls,b) = loop x []  in
      if b then
        `list ls
      else `cons ls
  | Pexp_construct (x,None) -> `simple (x.txt)
  | _ -> `normal

let is_simple_construct :construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ | `btrue | `bfalse -> true
  | `cons _ | `normal -> false

let pp = fprintf

type ctxt = {
  pipe : bool;
  semi : bool;
  ifthenelse : bool;
  functionrhs : bool;
}

let reset_ctxt = { pipe=false; semi=false; ifthenelse=false; functionrhs=false }
let under_pipe ctxt = { ctxt with pipe=true }
let under_semi ctxt = { ctxt with semi=true }
let under_ifthenelse ctxt = { ctxt with ifthenelse=true }
let under_functionrhs ctxt = { ctxt with functionrhs = true }
(*
let reset_semi ctxt = { ctxt with semi=false }
let reset_ifthenelse ctxt = { ctxt with ifthenelse=false }
let reset_pipe ctxt = { ctxt with pipe=false }
*)

let list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
  ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
  = fun ?sep ?first ?last fu f xs ->
    let first = match first with Some x -> x |None -> ("": _ format6)
    and last = match last with Some x -> x |None -> ("": _ format6)
    and sep = match sep with Some x -> x |None -> ("@ ": _ format6) in
    let aux f = function
      | [] -> ()
      | [x] -> fu f x
      | xs ->
          let rec loop  f = function
            | [x] -> fu f x
            | x::xs ->  fu f x; pp f sep; loop f xs;
            | _ -> assert false in begin
            pp f first; loop f xs; pp f last;
          end in
    aux f xs

let option : 'a. ?first:space_formatter -> ?last:space_formatter ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
  = fun  ?first  ?last fu f a ->
    let first = match first with Some x -> x | None -> ("": _ format6)
    and last = match last with Some x -> x | None -> ("": _ format6) in
    match a with
    | None -> ()
    | Some x -> pp f first; fu f x; pp f last

let paren: 'a . ?first:space_formatter -> ?last:space_formatter ->
  bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  = fun  ?(first=("": _ format6)) ?(last=("": _ format6)) b fu f x ->
    if b then (pp f "("; pp f first; fu f x; pp f last; pp f ")")
    else fu f x

let rec longident f = function
  | Lident s -> ident_of_name f s
  | Ldot(y,s) -> protect_longident f longident y s
  | Lapply (y,s) ->
      pp f "%a(%a)" longident y longident s

let longident_loc f x = pp f "%a" longident x.txt

let constant f = function
  | Pconst_char i ->
      pp f "%C"  i
  | Pconst_string (i, _, None) ->
      pp f "%S" i
  | Pconst_string (i, _, Some delim) ->
      pp f "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) ->
      paren (first_is '-' i) (fun f -> pp f "%s") f i
  | Pconst_integer (i, Some m) ->
      paren (first_is '-' i) (fun f (i, m) -> pp f "%s%c" i m) f (i,m)
  | Pconst_float (i, None) ->
      paren (first_is '-' i) (fun f -> pp f "%s") f i
  | Pconst_float (i, Some m) ->
      paren (first_is '-' i) (fun f (i,m) -> pp f "%s%c" i m) f (i,m)

(* trailing space*)
let mutable_flag f = function
  | Immutable -> ()
  | Mutable -> pp f "mutable@;"
let virtual_flag f  = function
  | Concrete -> ()
  | Virtual -> pp f "virtual@;"

(* trailing space added *)
let rec_flag f rf =
  match rf with
  | Nonrecursive -> ()
  | Recursive -> pp f "rec "
let nonrec_flag f rf =
  match rf with
  | Nonrecursive -> pp f "nonrec "
  | Recursive -> ()
let direction_flag f = function
  | Upto -> pp f "to@ "
  | Downto -> pp f "downto@ "
let private_flag f = function
  | Public -> ()
  | Private -> pp f "private@ "

let iter_loc f ctxt {txt; loc = _} = f ctxt txt

let constant_string f s = pp f "%S" s

let tyvar_of_name s =
  if String.length s >= 2 && s.[1] = '\'' then
    (* without the space, this would be parsed as
       a character literal *)
    "' " ^ s
  else if Lexer.is_keyword s then
    "'\\#" ^ s
  else if String.equal s "_" then
    s
  else
    "'" ^ s

let tyvar ppf s =
  Format.fprintf ppf "%s" (tyvar_of_name s)

let string_loc ppf x = fprintf ppf "%s" x.txt

let tyvar_loc f str = tyvar f str.txt
let string_quot f x = pp f "`%a" ident_of_name x

(* legacy modes and modalities *)
let legacy_mode f { txt = Mode s; _ } =
  let s =
    match s with
    | "local" -> "local_"
    | "unique" -> "unique_"
    | "once" -> "once_"
    | s -> Misc.fatal_errorf "Unrecognized mode %s - should not parse" s
  in
  pp_print_string f s

let legacy_modes f m =
  pp_print_list ~pp_sep:(fun f () -> pp f " ") legacy_mode f m

let optional_legacy_modes f m =
  match m with
  | [] -> ()
  | m ->
    legacy_modes f m;
    pp_print_space f ()

let legacy_modality f m =
  let {txt; _} = (m : modality Location.loc) in
  let s =
    match txt with
    | Modality "global" -> "global_"
    | Modality s -> Misc.fatal_errorf "Unrecognized modality %s - should not parse" s
  in
  pp_print_string f s

let legacy_modalities f m =
  pp_print_list ~pp_sep:(fun f () -> pp f " ") legacy_modality f m

let optional_legacy_modalities f m =
  match m with
  | [] -> ()
  | m ->
    legacy_modalities f m;
    pp_print_space f ()

(* new mode and modality syntax *)
let mode f { txt = Mode s; _ } =
  pp_print_string f s

let modes f m =
  pp_print_list ~pp_sep:(fun f () -> pp f " ") mode f m

let optional_at_modes f m =
  match m with
  | [] -> ()
  | m -> pp f " %@ %a" modes m

let optional_atat_modes f m =
  match m with
  | [] -> ()
  | m -> pp f " %@%@ %a" modes m

let maybe_type_atat_modes pty ctxt f (c, m) =
  pp f "%a%a" (pty ctxt) c optional_atat_modes m

let modality f m =
  let {txt = Modality txt; _} = m in
  pp_print_string f txt

let modalities f m =
  pp_print_list ~pp_sep:(fun f () -> pp f " ") modality f m

let optional_atat_modalities f m =
  match m with
  | [] -> ()
  | m -> pp f " %@%@ %a" modalities m

(* helpers for printing both legacy/new mode syntax *)
let split_out_legacy_modes =
  List.partition (fun m ->
    let Mode txt = m.txt in
    match txt with
    | "local" | "unique" | "once" -> true
    | _ -> false
  )

let maybe_legacy_modes_type_at_modes pty ctxt f (c, m) =
  let legacy, m = split_out_legacy_modes m in
  pp f "%a%a%a" optional_legacy_modes legacy (pty ctxt) c optional_at_modes m

let split_out_legacy_modalities =
  List.partition (fun m ->
    let Modality txt = m.txt in
    match txt with
    | "global" -> true
    | _ -> false
  )

let modalities_type pty ctxt f pca =
  let legacy, m = split_out_legacy_modalities pca.pca_modalities in
  pp f "%a%a%a"
    optional_legacy_modalities legacy
    (pty ctxt) pca.pca_type
    optional_atat_modalities m

let include_kind f = function
  | Functor -> pp f "@ functor"
  | Structure -> ()

(* c ['a,'b] *)
let rec class_params_def ctxt f =  function
  | [] -> ()
  | l ->
      pp f "[%a] " (* space *)
        (list (type_param ctxt) ~sep:",") l

and type_with_label ctxt f (label, c, mode) =
  match label with
  | Nolabel    ->
    maybe_legacy_modes_type_at_modes core_type1 ctxt f (c, mode)
    (* otherwise parenthesize *)
  | Labelled s ->
    pp f "%a:%a" ident_of_name s
      (maybe_legacy_modes_type_at_modes core_type1 ctxt) (c, mode)
  | Optional s ->
    pp f "?%a:%a" ident_of_name s
      (maybe_legacy_modes_type_at_modes core_type1 ctxt) (c, mode)

and jkind ?(nested = false) ctxt f k = match (k : Jane_syntax.Jkind.t) with
  | Default -> pp f "_"
  | Abbreviation s ->
    pp f "%s" s.txt
  | Mod (t, modes) ->
    begin match modes with
    | [] -> Misc.fatal_error "malformed jkind annotation"
    | _ :: _ ->
      if nested then pp f "(";
      pp f "%a mod %a"
        (jkind ~nested:true ctxt) t
        (pp_print_list ~pp_sep:pp_print_space mode) modes;
      if nested then pp f ")";
    end
  | With (t, ty) ->
    if nested then pp f "(";
    pp f "%a with %a" (jkind ~nested:true ctxt) t (core_type ctxt) ty;
    if nested then pp f ")";
  | Kind_of ty -> pp f "kind_of_ %a" (core_type ctxt) ty
  | Product ts ->
    if nested then pp f "(";
    pp f "%a" (list (jkind ~nested:true ctxt) ~sep:"@;&@;") ts;
    if nested then pp f ")"

and jkind_annotation ctxt f annot = jkind ctxt f annot.txt

and tyvar_jkind_loc ctxt ~print_quote f (str,jkind) =
  let pptv =
    if print_quote
    then tyvar
    else fun ppf s -> pp ppf "%s" s
  in
  match jkind with
  | None -> pptv f str.txt
  | Some lay -> pp f "(%a : %a)" pptv str.txt (jkind_annotation ctxt) lay

and core_type ctxt f x =
  match Jane_syntax.Core_type.of_ast x with
  | Some (jtyp, attrs) -> core_type_jane_syntax ctxt attrs f jtyp
  | None ->
  let filtered_attrs = filter_curry_attrs x.ptyp_attributes in
  if filtered_attrs <> [] then begin
    pp f "((%a)%a)" (core_type ctxt) {x with ptyp_attributes=[]}
      (attributes ctxt) filtered_attrs
  end
  else match x.ptyp_desc with
    | Ptyp_arrow (l, ct1, ct2, m1, m2) ->
        pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
          (type_with_label ctxt) (l,ct1,m1) (return_type ctxt) (ct2,m2)
    | Ptyp_alias (ct, s) ->
        pp f "@[<2>%a@;as@;%a@]" (core_type1 ctxt) ct tyvar s.txt
    | Ptyp_poly ([], ct) ->
        core_type ctxt f ct
    | Ptyp_poly (sl, ct) ->
        pp f "@[<2>%a%a@]"
               (fun f l -> match l with
                  | [] -> ()
                  | _ ->
                      pp f "%a@;.@;"
                        (list tyvar_loc ~sep:"@;")  l)
          sl (core_type ctxt) ct
    | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

and core_type1 ctxt f x =
  match Jane_syntax.Core_type.of_ast x with
  | Some (jtyp, attrs) -> core_type1_jane_syntax ctxt attrs f jtyp
  | None ->
  if has_non_curry_attr x.ptyp_attributes then core_type ctxt f x
  else
    match x.ptyp_desc with
    | Ptyp_any -> pp f "_";
    | Ptyp_var s -> tyvar f  s;
    | Ptyp_tuple l ->  pp f "(%a)" (list (core_type1 ctxt) ~sep:"@;*@;") l
    | Ptyp_unboxed_tuple l ->
      core_type1_labeled_tuple ctxt f ~unboxed:true l
    | Ptyp_constr (li, l) ->
        pp f (* "%a%a@;" *) "%a%a"
          (fun f l -> match l with
             |[] -> ()
             |[x]-> pp f "%a@;" (core_type1 ctxt)  x
             | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
          l longident_loc li
    | Ptyp_variant (l, closed, low) ->
        let first_is_inherit = match l with
          | {Parsetree.prf_desc = Rinherit _}::_ -> true
          | _ -> false in
        let type_variant_helper f x =
          match x.prf_desc with
          | Rtag (l, _, ctl) ->
              pp f "@[<2>%a%a@;%a@]" (iter_loc string_quot) l
                (fun f l -> match l with
                   |[] -> ()
                   | _ -> pp f "@;of@;%a"
                            (list (core_type ctxt) ~sep:"&")  ctl) ctl
                (attributes ctxt) x.prf_attributes
          | Rinherit ct -> core_type ctxt f ct in
        pp f "@[<2>[%a%a]@]"
          (fun f l ->
             match l, closed with
             | [], Closed -> ()
             | [], Open -> pp f ">" (* Cf #7200: print [>] correctly *)
             | _ ->
                 pp f "%s@;%a"
                   (match (closed,low) with
                    | (Closed,None) -> if first_is_inherit then " |" else ""
                    | (Closed,Some _) -> "<" (* FIXME desugar the syntax sugar*)
                    | (Open,_) -> ">")
                   (list type_variant_helper ~sep:"@;<1 -2>| ") l) l
          (fun f low -> match low with
             |Some [] |None -> ()
             |Some xs ->
                 pp f ">@ %a"
                   (list string_quot) xs) low
    | Ptyp_object (l, o) ->
        let core_field_type f x = match x.pof_desc with
          | Otag (l, ct) ->
            (* Cf #7200 *)
            pp f "@[<hov2>%a: %a@ %a@ @]" ident_of_name l.txt
              (core_type ctxt) ct (attributes ctxt) x.pof_attributes
          | Oinherit ct ->
            pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
        in
        let field_var f = function
          | Asttypes.Closed -> ()
          | Asttypes.Open ->
              match l with
              | [] -> pp f ".."
              | _ -> pp f " ;.."
        in
        pp f "@[<hov2><@ %a%a@ > @]"
          (list core_field_type ~sep:";") l
          field_var o (* Cf #7200 *)
    | Ptyp_class (li, l) ->   (*FIXME*)
        pp f "@[<hov2>%a@;#%a@]"
          (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")") l
          longident_loc li
    | Ptyp_package (lid, cstrs) ->
        let aux f (s, ct) =
          pp f "type %a@ =@ %a" longident_loc s (core_type ctxt) ct  in
        (match cstrs with
         |[] -> pp f "@[<hov2>(module@ %a)@]" longident_loc lid
         |_ ->
             pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
               (list aux  ~sep:"@ and@ ")  cstrs)
    | Ptyp_open(li, ct) ->
       pp f "@[<hov2>%a.(%a)@]" longident_loc li (core_type ctxt) ct
    | Ptyp_extension e -> extension ctxt f e
    | (Ptyp_arrow _ | Ptyp_alias _ | Ptyp_poly _) ->
       paren true (core_type ctxt) f x

and core_type_jane_syntax ctxt attrs f (x : Jane_syntax.Core_type.t) =
  let filtered_attrs = filter_curry_attrs attrs in
  if filtered_attrs <> [] then begin
    pp f "((%a)%a)" (core_type_jane_syntax ctxt []) x
      (attributes ctxt) filtered_attrs
  end
  else match x with
  | Jtyp_layout (Ltyp_alias { aliased_type; name; jkind }) ->
    pp f "@[<2>%a@;as@;(%a :@ %a)@]"
      (core_type1 ctxt) aliased_type
      tyvar_option (Option.map Location.get_txt name)
      (jkind_annotation ctxt) jkind
  | Jtyp_layout (Ltyp_poly {bound_vars = []; inner_type}) ->
    core_type ctxt f inner_type
  | Jtyp_layout (Ltyp_poly {bound_vars; inner_type}) ->
    let jkind_poly_var f (name, jkind_opt) =
      match jkind_opt with
      | Some jkind -> pp f "(%a@;:@;%a)" tyvar_loc name (jkind_annotation ctxt) jkind
      | None -> tyvar_loc f name
    in
    pp f "@[<2>%a@;.@;%a@]"
        (list jkind_poly_var ~sep:"@;") bound_vars
        (core_type ctxt) inner_type
  | Jtyp_tuple _ | Jtyp_layout (Ltyp_var _) ->
    pp f "@[<2>%a@]" (core_type1_jane_syntax ctxt attrs) x


and core_type1_jane_syntax ctxt attrs f (x : Jane_syntax.Core_type.t) =
  if has_non_curry_attr attrs then core_type_jane_syntax ctxt attrs f x
  else
    match x with
    | Jtyp_layout (Ltyp_var { name; jkind }) ->
      pp f "(%a@;:@;%a)" tyvar_option name (jkind_annotation ctxt) jkind
    | Jtyp_tuple x -> core_type1_labeled_tuple ctxt f ~unboxed:false x
    | Jtyp_layout (Ltyp_alias _ | Ltyp_poly _) ->
      paren true (core_type_jane_syntax ctxt attrs) f x

and tyvar_option f = function
  | None -> pp f "_"
  | Some name -> tyvar f name

and core_type1_labeled_tuple ctxt f ~unboxed
      : Jane_syntax.Labeled_tuples.core_type -> _ =
  fun tl ->
    pp f "%s(%a)" (if unboxed then "#" else "")
      (list (labeled_core_type1 ctxt) ~sep:"@;*@;") tl

and labeled_core_type1 ctxt f (label, ty) =
  begin match label with
  | None   -> ()
  | Some s -> pp f "%s:" s
  end;
  core_type1 ctxt f ty

and return_type ctxt f (x, m) =
  if x.ptyp_attributes <> [] then maybe_legacy_modes_type_at_modes core_type1 ctxt f (x, m)
  else maybe_legacy_modes_type_at_modes core_type ctxt f (x, m)

(********************pattern********************)
(* be cautious when use [pattern], [pattern1] is preferred *)
and pattern ctxt f x =
  match Jane_syntax.Pattern.of_ast x with
  | Some (jpat, attrs) -> pattern_jane_syntax ctxt attrs f jpat
  | None ->
  if x.ppat_attributes <> [] then begin
    pp f "((%a)%a)" (pattern ctxt) {x with ppat_attributes=[]}
      (attributes ctxt) x.ppat_attributes
  end
  else match x.ppat_desc with
    | Ppat_alias (p, s) ->
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p ident_of_name s.txt
    | _ -> pattern_or ctxt f x

and pattern_or ctxt f x =
  let rec left_associative x acc = match x with
    | {ppat_desc=Ppat_or (p1,p2); ppat_attributes = []} ->
        left_associative p1 (p2 :: acc)
    | x -> x :: acc
  in
  match left_associative x [] with
  | [] -> assert false
  | [x] -> pattern1 ctxt f x
  | orpats ->
      pp f "@[<hov0>%a@]" (list ~sep:"@ | " (pattern1 ctxt)) orpats

and pattern1 ctxt (f:Format.formatter) (x:pattern) : unit =
  let rec pattern_list_helper f p = match p with
    | {ppat_desc =
         Ppat_construct
           ({ txt = Lident("::") ;_},
            Some ([], inner_pat));
       ppat_attributes = []} ->
      begin match Jane_syntax.Pattern.of_ast inner_pat, inner_pat.ppat_desc with
      | None, Ppat_tuple([pat1; pat2]) ->
        pp f "%a::%a" (simple_pattern ctxt) pat1 pattern_list_helper pat2 (*RA*)
      | _ -> pattern1 ctxt f p
      end
    | _ -> pattern1 ctxt f p
  in
  if x.ppat_attributes <> [] then pattern ctxt f x
  else match x.ppat_desc with
    | Ppat_variant (l, Some p) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name l (simple_pattern ctxt) p
    | Ppat_construct (({txt=Lident("()"|"[]"|"true"|"false");_}), _) ->
        simple_pattern ctxt f x
    | Ppat_construct (({txt;_} as li), po) ->
        (* FIXME The third field always false *)
        if txt = Lident "::" then
          pp f "%a" pattern_list_helper x
        else
          (match po with
           | Some ([], x) ->
               pp f "%a@;%a"  longident_loc li (simple_pattern ctxt) x
           | Some (vl, x) ->
               pp f "%a@ (type %a)@;%a" longident_loc li
                 (list ~sep:"@ " ident_of_name_loc) vl
                 (simple_pattern ctxt) x
           | None -> pp f "%a" longident_loc li)
    | _ -> simple_pattern ctxt f x

and labeled_pattern1 ctxt (f:Format.formatter) (label, x) : unit =
  let simple_name = match x with
    | {ppat_desc = Ppat_var { txt=s; _ }; ppat_attributes = []; _} -> Some s
    | _ -> None
  in
  match label, simple_name with
  | None, _ ->
    pattern1 ctxt f x
  | Some lbl, Some simple_name when String.equal simple_name lbl ->
    pp f "~%s" lbl
  | Some lbl, _ ->
    pp f "~%s:" lbl;
    pattern1 ctxt f x

and simple_pattern ctxt (f:Format.formatter) (x:pattern) : unit =
  if x.ppat_attributes <> [] then pattern ctxt f x
  else match Jane_syntax.Pattern.of_ast x with
    | Some (jpat, attrs) -> pattern_jane_syntax ctxt attrs f jpat
    | None ->
    match x.ppat_desc with
    | Ppat_construct (({txt=Lident ("()"|"[]"|"true"|"false" as x);_}), None) ->
        pp f  "%s" x
    | Ppat_any -> pp f "_";
    | Ppat_var ({txt = txt;_}) -> ident_of_name f txt
    | Ppat_array l ->
        pp f "@[<2>[|%a|]@]"  (list (pattern1 ctxt) ~sep:";") l
    | Ppat_unpack { txt = None } ->
        pp f "(module@ _)@ "
    | Ppat_unpack { txt = Some s } ->
        pp f "(module@ %s)@ " s
    | Ppat_type li ->
        pp f "#%a" longident_loc li
    | Ppat_record (l, closed) ->
        let longident_x_pattern f (li, p) =
          match (li,p) with
          | ({txt=Lident s;_ },
             {ppat_desc=Ppat_var {txt;_};
              ppat_attributes=[]; _})
            when s = txt ->
              pp f "@[<2>%a@]"  longident_loc li
          | _ ->
              pp f "@[<2>%a@;=@;%a@]" longident_loc li (pattern1 ctxt) p
        in
        begin match closed with
        | Closed ->
            pp f "@[<2>{@;%a@;}@]" (list longident_x_pattern ~sep:";@;") l
        | _ ->
            pp f "@[<2>{@;%a;_}@]" (list longident_x_pattern ~sep:";@;") l
        end
    | Ppat_tuple l ->
        pp f "@[<1>(%a)@]" (list  ~sep:",@;" (pattern1 ctxt))  l (* level1*)
    | Ppat_unboxed_tuple (l, closed) ->
        labeled_tuple_pattern ctxt f ~unboxed:true l closed
    | Ppat_constant (c) -> pp f "%a" constant c
    | Ppat_interval (c1, c2) -> pp f "%a..%a" constant c1 constant c2
    | Ppat_variant (l,None) ->  pp f "`%a" ident_of_name l
    | Ppat_constraint (p, ct, m) ->
        let legacy, m = split_out_legacy_modes m in
        begin match ct with
        | Some ct ->
            pp f "@[<2>(%a%a@;:@;%a%a)@]"
              optional_legacy_modes legacy
              (pattern1 ctxt) p
              (core_type ctxt) ct
              optional_atat_modes m
        | None ->
            pp f "@[<2>(%a%a%a)@]"
              optional_legacy_modes legacy
              (pattern1 ctxt) p
              optional_at_modes m
        end
    | Ppat_lazy p ->
        pp f "@[<2>(lazy@;%a)@]" (simple_pattern ctxt) p
    | Ppat_exception p ->
        pp f "@[<2>exception@;%a@]" (pattern1 ctxt) p
    | Ppat_extension e -> extension ctxt f e
    | Ppat_open (lid, p) ->
        let with_paren =
        match Jane_syntax.Pattern.of_ast p with
        | Some (jpat, _attrs) -> begin match jpat with
        | Jpat_immutable_array (Iapat_immutable_array _) -> false
        | Jpat_layout (Lpat_constant _) -> false
        | Jpat_tuple (_, _) -> true
        end
        | None -> match p.ppat_desc with
        | Ppat_array _ | Ppat_record _
        | Ppat_construct (({txt=Lident ("()"|"[]"|"true"|"false");_}), None) ->
            false
        | _ -> true in
        pp f "@[<2>%a.%a @]" longident_loc lid
          (paren with_paren @@ pattern1 ctxt) p
    | _ -> paren true (pattern ctxt) f x

and pattern_jane_syntax ctxt attrs f (pat : Jane_syntax.Pattern.t) =
  if attrs <> [] then
    pp f "((%a)%a)" (pattern_jane_syntax ctxt []) pat
      (attributes ctxt) attrs
  else
    match pat with
    | Jpat_immutable_array (Iapat_immutable_array l) ->
        pp f "@[<2>[:%a:]@]"  (list (pattern1 ctxt) ~sep:";") l
    | Jpat_layout (Lpat_constant c) -> unboxed_constant ctxt f c
    | Jpat_tuple (l, closed) ->
      labeled_tuple_pattern ctxt f ~unboxed:false l closed

and labeled_tuple_pattern ctxt f ~unboxed l closed =
  let closed_flag ppf = function
  | Closed -> ()
  | Open -> pp ppf ",@;.."
  in
  pp f "@[<1>%s(%a%a)@]"
    (if unboxed then "#" else "")
    (list ~sep:",@;" (labeled_pattern1 ctxt)) l
    closed_flag closed

and label_exp ctxt f (l,opt,p) =
  match l with
  | Nolabel ->
      (* single case pattern parens needed here *)
      pp f "%a" (simple_pattern ctxt) p
  | Optional rest ->
      begin match p with
      | {ppat_desc = Ppat_var {txt;_}; ppat_attributes = []}
        when txt = rest ->
          (match opt with
           | Some o ->
              pp f "?(%a=@;%a)" ident_of_name rest (expression ctxt) o
           | None -> pp f "?%a" ident_of_name rest)
      | _ ->
          (match opt with
           | Some o ->
               (* Remove the legacy modes from the pattern here *)
               let legacy, p =
                 match p.ppat_desc with
                 | Ppat_constraint (p', cty', m') ->
                   let legacy, m' = split_out_legacy_modes m' in
                   let p =
                     match cty', m' with
                     | None, [] -> p'
                     | _ -> { p with ppat_desc = Ppat_constraint (p', cty', m') }
                   in
                   legacy, p
                 | _ -> [], p
               in
               pp f "?%a:(%a%a=@;%a)"
                 ident_of_name rest
                 optional_legacy_modes legacy
                 (pattern1 ctxt) p
                 (expression ctxt) o
           | None -> pp f "?%a:%a" ident_of_name rest (simple_pattern ctxt) p)
      end
  | Labelled l -> match p with
    | {ppat_desc  = Ppat_var {txt;_}; ppat_attributes = []}
      when txt = l ->
        pp f "~%a" ident_of_name l
    | _ ->  pp f "~%a:%a" ident_of_name l (simple_pattern ctxt) p

and sugar_expr ctxt f e =
  if e.pexp_attributes <> [] then false
  else match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident {txt = id; _};
                  pexp_attributes=[]; _}, args)
    when List.for_all (fun (lab, _) -> lab = Nolabel) args -> begin
      let print_indexop a path_prefix assign left sep right print_index indices
          rem_args =
        let print_path ppf = function
          | None -> ()
          | Some m -> pp ppf ".%a" longident m in
        match assign, rem_args with
            | false, [] ->
              pp f "@[%a%a%s%a%s@]"
                (simple_expr ctxt) a print_path path_prefix
                left (list ~sep print_index) indices right; true
            | true, [v] ->
              pp f "@[%a%a%s%a%s@ <-@;<1 2>%a@]"
                (simple_expr ctxt) a print_path path_prefix
                left (list ~sep print_index) indices right
                (simple_expr ctxt) v; true
            | _ -> false in
      match id, List.map snd args with
      | Lident "!", [e] ->
        pp f "@[<hov>!%a@]" (simple_expr ctxt) e; true
      | Ldot (path, ("get"|"set" as func)), a :: other_args -> begin
          let assign = func = "set" in
          let print = print_indexop a None assign in
          match path, other_args with
          | Lident "Array", i :: rest ->
            print ".(" "" ")" (expression ctxt) [i] rest
          | Lident "String", i :: rest ->
            print ".[" "" "]" (expression ctxt) [i] rest
          | Ldot (Lident "Bigarray", "Array1"), i1 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1] rest
          | Ldot (Lident "Bigarray", "Array2"), i1 :: i2 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2] rest
          | Ldot (Lident "Bigarray", "Array3"), i1 :: i2 :: i3 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2; i3] rest
          | Ldot (Lident "Bigarray", "Genarray"),
            {pexp_desc = Pexp_array indexes; pexp_attributes = []} :: rest ->
              print ".{" "," "}" (simple_expr ctxt) indexes rest
          | _ -> false
        end
      | (Lident s | Ldot(_,s)) , a :: i :: rest
        when first_is '.' s ->
          (* extract operator:
             assignment operators end with [right_bracket ^ "<-"],
             access operators end with [right_bracket] directly
          *)
          let multi_indices = String.contains s ';' in
          let i =
              match i.pexp_desc with
                | Pexp_array l when multi_indices -> l
                | _ -> [ i ] in
          let assign = last_is '-' s in
          let kind =
            (* extract the right end bracket *)
            let n = String.length s in
            if assign then s.[n - 3] else s.[n - 1] in
          let left, right = match kind with
            | ')' -> '(', ")"
            | ']' -> '[', "]"
            | '}' -> '{', "}"
            | _ -> assert false in
          let path_prefix = match id with
            | Ldot(m,_) -> Some m
            | _ -> None in
          let left = String.sub s 0 (1+String.index s left) in
          print_indexop a path_prefix assign left ";" right
            (if multi_indices then expression ctxt else simple_expr ctxt)
            i rest
      | _ -> false
    end
  | _ -> false

(* Postcondition: If [x] has any non-Jane Syntax attributes, the output will
   be self-delimiting. (I.e., it will be wrapped in parens.)

   Passing [jane_syntax_parens=true] will insert parens around Jane Syntax
   expressions that aren't already self-delimiting.
*)
and expression ?(jane_syntax_parens = false) ctxt f x =
  match Jane_syntax.Expression.of_ast x with
  | Some (jexpr, attrs) ->
      jane_syntax_expr ctxt attrs f jexpr ~parens:jane_syntax_parens
  | None ->
  if x.pexp_attributes <> [] then
    pp f "((%a)@,%a)" (expression ctxt) {x with pexp_attributes=[]}
      (attributes ctxt) x.pexp_attributes
  else match x.pexp_desc with
    | Pexp_function _ | Pexp_match _ | Pexp_try _ | Pexp_sequence _
    | Pexp_newtype _
      when ctxt.pipe || ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_ifthenelse _ | Pexp_sequence _ when ctxt.ifthenelse ->
        paren true (expression reset_ctxt) f x
    | Pexp_let _ | Pexp_letmodule _ | Pexp_open _
      | Pexp_letexception _ | Pexp_letop _
        when ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_newtype (lid, e) ->
        pp f "@[<2>fun@;(type@;%a)@;%a@]" ident_of_name lid.txt
          (pp_print_pexp_newtype ctxt "->") e
    | Pexp_function (params, constraint_, body) ->
        begin match params, constraint_ with
          (* Omit [fun] if there are no params. *)
          | [], None ->
              (* If function cases are a direct body of a function,
                 the function node should be wrapped in parens so
                 it doesn't become part of the enclosing function. *)
              let should_paren =
                match body with
                | Pfunction_cases _ -> ctxt.functionrhs
                | Pfunction_body _ -> false
              in
              let ctxt' = if should_paren then reset_ctxt else ctxt in
              pp f "@[<2>%a@]" (paren should_paren (function_body ctxt')) body
          | [], Some constraint_ ->
            pp f "@[<2>(%a@;%a)@]"
              (function_body ctxt) body
              (function_constraint ctxt) constraint_
          | _ :: _, _ ->
            pp f "@[<2>fun@;%t@]"
              (fun f ->
                function_params_then_body
                  ctxt f params constraint_ body ~delimiter:"->")
        end
    | Pexp_match (e, l) ->
        pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]"
          (expression reset_ctxt) e (case_list ctxt) l

    | Pexp_try (e, l) ->
        pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
             (* "try@;@[<2>%a@]@\nwith@\n%a"*)
          (expression reset_ctxt) e  (case_list ctxt) l
    | Pexp_let (rf, l, e) ->
        (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
           (*no indentation here, a new line*) *)
        (*   rec_flag rf *)
        pp f "@[<2>%a in@;<1 -2>%a@]"
          (bindings reset_ctxt) (rf,l)
          (expression ctxt) e
    | Pexp_apply
      ({ pexp_desc = Pexp_extension({txt = "extension.exclave"}, PStr []) },
       [Nolabel, sbody]) ->
        pp f "@[<2>exclave_ %a@]" (expression ctxt) sbody
    | Pexp_apply (e, l) ->
        begin if not (sugar_expr ctxt f x) then
            match view_fixity_of_exp e with
            | `Infix s ->
                begin match l with
                | [ (Nolabel, _) as arg1; (Nolabel, _) as arg2 ] ->
                    (* FIXME associativity label_x_expression_param *)
                    pp f "@[<2>%a@;%s@;%a@]"
                      (label_x_expression_param reset_ctxt) arg1 s
                      (label_x_expression_param ctxt) arg2
                | _ ->
                    pp f "@[<2>%a %a@]"
                      (simple_expr ctxt) e
                      (list (label_x_expression_param ctxt)) l
                end
            | `Prefix s ->
                let s =
                  if List.mem s ["~+";"~-";"~+.";"~-."] &&
                   (match l with
                    (* See #7200: avoid turning (~- 1) into (- 1) which is
                       parsed as an int literal *)
                    |[(_,{pexp_desc=Pexp_constant _})] -> false
                    | _ -> true)
                  then String.sub s 1 (String.length s -1)
                  else s in
                begin match l with
                | [(Nolabel, x)] ->
                  pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
                | _   ->
                  pp f "@[<2>%a %a@]" (simple_expr ctxt) e
                    (list (label_x_expression_param ctxt)) l
                end
            | _ ->
                pp f "@[<hov2>%a@]" begin fun f (e,l) ->
                  pp f "%a@ %a" (expression2 ctxt) e
                    (list (label_x_expression_param reset_ctxt))  l
                    (* reset here only because [function,match,try,sequence]
                       are lower priority *)
                end (e,l)
        end

    | Pexp_stack e ->
        (* Similar to the common case of [Pexp_apply] *)
        pp f "@[<hov2>stack_@ %a@]" (expression2 reset_ctxt)  e
    | Pexp_construct (li, Some eo)
      when not (is_simple_construct (view_expr x))-> (* Not efficient FIXME*)
        (match view_expr x with
         | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
         | `normal ->
             pp f "@[<2>%a@;%a@]" longident_loc li
               (simple_expr ctxt) eo
         | _ -> assert false)
    | Pexp_setfield (e1, li, e2) ->
        pp f "@[<2>%a.%a@ <-@ %a@]"
          (simple_expr ctxt) e1 longident_loc li (simple_expr ctxt) e2
    | Pexp_ifthenelse (e1, e2, eo) ->
        (* @;@[<2>else@ %a@]@] *)
        let fmt:(_,_,_)format ="@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]" in
        let expression_under_ifthenelse = expression (under_ifthenelse ctxt) in
        pp f fmt expression_under_ifthenelse e1 expression_under_ifthenelse e2
          (fun f eo -> match eo with
             | Some x ->
                 pp f "@;@[<2>else@;%a@]" (expression (under_semi ctxt)) x
             | None -> () (* pp f "()" *)) eo
    | Pexp_sequence _ ->
        let rec sequence_helper acc = function
          | {pexp_desc=Pexp_sequence(e1,e2); pexp_attributes = []} ->
              sequence_helper (e1::acc) e2
          | v -> List.rev (v::acc) in
        let lst = sequence_helper [] x in
        pp f "@[<hv>%a@]"
          (list (expression (under_semi ctxt)) ~sep:";@;") lst
    | Pexp_new (li) ->
        pp f "@[<hov2>new@ %a@]" longident_loc li;
    | Pexp_setinstvar (s, e) ->
        pp f "@[<hov2>%a@ <-@ %a@]" ident_of_name s.txt (expression ctxt) e
    | Pexp_override l -> (* FIXME *)
        let string_x_expression f (s, e) =
          pp f "@[<hov2>%a@ =@ %a@]" ident_of_name s.txt (expression ctxt) e in
        pp f "@[<hov2>{<%a>}@]"
          (list string_x_expression  ~sep:";"  )  l;
    | Pexp_letmodule (s, me, e) ->
        pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]"
          (Option.value s.txt ~default:"_")
          (module_expr reset_ctxt) me (expression ctxt) e
    | Pexp_letexception (cd, e) ->
        pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
          (extension_constructor ctxt) cd
          (expression ctxt) e
    | Pexp_assert e ->
        pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
    | Pexp_lazy (e) ->
        pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
    (* Pexp_poly: impossible but we should print it anyway, rather than
       assert false *)
    | Pexp_poly (e, None) ->
        pp f "@[<hov2>!poly!@ %a@]" (simple_expr ctxt) e
    | Pexp_poly (e, Some ct) ->
        pp f "@[<hov2>(!poly!@ %a@ : %a)@]"
          (simple_expr ctxt) e (core_type ctxt) ct
    | Pexp_open (o, e) ->
        pp f "@[<2>let open%s %a in@;%a@]"
          (override o.popen_override) (module_expr ctxt) o.popen_expr
          (expression ctxt) e
    | Pexp_variant (l,Some eo) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name l (simple_expr ctxt) eo
    | Pexp_letop {let_; ands; body} ->
        pp f "@[<2>@[<v>%a@,%a@] in@;<1 -2>%a@]"
          (binding_op ctxt) let_
          (list ~sep:"@," (binding_op ctxt)) ands
          (expression ctxt) body
    | Pexp_extension e -> extension ctxt f e
    | Pexp_unreachable -> pp f "."
    | _ -> expression1 ctxt f x

and expression1 ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x ~jane_syntax_parens:true
  else match x.pexp_desc with
    | Pexp_object cs -> pp f "%a" (class_structure ctxt) cs
    | _ -> expression2 ctxt f x
(* used in [Pexp_apply] *)

and expression2 ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x ~jane_syntax_parens:true
  else match x.pexp_desc with
    | Pexp_field (e, li) ->
        pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_loc li
    | Pexp_send (e, s) ->
        pp f "@[<hov2>%a#%a@]" (simple_expr ctxt) e ident_of_name s.txt

    | _ -> simple_expr ctxt f x

and simple_expr ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x ~jane_syntax_parens:true
  else match x.pexp_desc with
    | Pexp_construct _  when is_simple_construct (view_expr x) ->
        (match view_expr x with
         | `nil -> pp f "[]"
         | `tuple -> pp f "()"
         | `btrue -> pp f "true"
         | `bfalse -> pp f "false"
         | `list xs ->
             pp f "@[<hv0>[%a]@]"
               (list (expression (under_semi ctxt)) ~sep:";@;") xs
         | `simple x -> longident f x
         | _ -> assert false)
    | Pexp_ident li ->
        longident_loc f li
    (* (match view_fixity_of_exp x with *)
    (* |`Normal -> longident_loc f li *)
    (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
    | Pexp_constant c -> constant f c;
    | Pexp_pack me ->
        pp f "(module@;%a)" (module_expr ctxt) me
    | Pexp_tuple l ->
        pp f "@[<hov2>(%a)@]" (list (simple_expr ctxt) ~sep:",@;") l
    | Pexp_unboxed_tuple l ->
        labeled_tuple_expr ctxt f ~unboxed:true l
    | Pexp_constraint (e, ct, m) ->
      begin match ct with
      | None ->
        pp f "(%a %a)" legacy_modes m (expression ctxt) e
      | Some ct ->
        let legacy, m = split_out_legacy_modes m in
        pp f "(%a%a : %a)"
          optional_legacy_modes legacy
          (expression ctxt) e
          (maybe_type_atat_modes core_type ctxt) (ct, m)
      end
    | Pexp_coerce (e, cto1, ct) ->
        pp f "(%a%a :> %a)" (expression ctxt) e
          (option (core_type ctxt) ~first:" : " ~last:" ") cto1 (* no sep hint*)
          (core_type ctxt) ct
    | Pexp_variant (l, None) -> pp f "`%a" ident_of_name l
    | Pexp_record (l, eo) ->
        let longident_x_expression f ( li, e) =
          match e with
          |  {pexp_desc=Pexp_ident {txt;_};
              pexp_attributes=[]; _} when li.txt = txt ->
              pp f "@[<hov2>%a@]" longident_loc li
          | _ ->
              pp f "@[<hov2>%a@;=@;%a@]" longident_loc li (simple_expr ctxt) e
        in
        pp f "@[<hv0>@[<hv2>{@;%a%a@]@;}@]"(* "@[<hov2>{%a%a}@]" *)
          (option ~last:" with@;" (simple_expr ctxt)) eo
          (list longident_x_expression ~sep:";@;") l
    | Pexp_array (l) ->
        pp f "@[<0>@[<2>[|%a|]@]@]"
          (list (simple_expr (under_semi ctxt)) ~sep:";") l
    | Pexp_while (e1, e2) ->
        let fmt : (_,_,_) format = "@[<2>while@;%a@;do@;%a@;done@]" in
        pp f fmt (expression ctxt) e1 (expression ctxt) e2
    | Pexp_for (s, e1, e2, df, e3) ->
        let fmt:(_,_,_)format =
          "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]" in
        let expression = expression ctxt in
        pp f fmt (pattern ctxt) s expression e1 direction_flag
          df expression e2 expression e3
    | _ ->  paren true (expression ctxt) f x

and attributes ctxt f l =
  List.iter (attribute ctxt f) l

and item_attributes ctxt f l =
  List.iter (item_attribute ctxt f) l

and attribute ctxt f a =
  pp f "@[<2>[@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

and item_attribute ctxt f a =
  pp f "@[<2>[@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

and floating_attribute ctxt f a =
  pp f "@[<2>[@@@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

and value_description ctxt f x =
  (* note: value_description has an attribute field,
           but they're already printed by the callers this method *)
  pp f "@[<hov2>%a%a%a@]" (core_type ctxt) x.pval_type
    optional_atat_modalities x.pval_modalities
    (fun f x ->
       if x.pval_prim <> []
       then pp f "@ =@ %a" (list constant_string) x.pval_prim
    ) x

and extension ctxt f (s, e) =
  pp f "@[<2>[%%%s@ %a]@]" s.txt (payload ctxt) e

and item_extension ctxt f (s, e) =
  pp f "@[<2>[%%%%%s@ %a]@]" s.txt (payload ctxt) e

and exception_declaration ctxt f x =
  pp f "@[<hov2>exception@ %a@]%a"
    (extension_constructor ctxt) x.ptyexn_constructor
    (item_attributes ctxt) x.ptyexn_attributes

and class_type_field ctxt f x =
  match x.pctf_desc with
  | Pctf_inherit (ct) ->
      pp f "@[<2>inherit@ %a@]%a" (class_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_val (s, mf, vf, ct) ->
      pp f "@[<2>val @ %a%a%a@ :@ %a@]%a"
        mutable_flag mf virtual_flag vf
        ident_of_name s.txt (core_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_method (s, pf, vf, ct) ->
      pp f "@[<2>method %a %a%a :@;%a@]%a"
        private_flag pf virtual_flag vf
        ident_of_name s.txt (core_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_constraint (ct1, ct2) ->
      pp f "@[<2>constraint@ %a@ =@ %a@]%a"
        (core_type ctxt) ct1 (core_type ctxt) ct2
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_attribute a -> floating_attribute ctxt f a
  | Pctf_extension e ->
      item_extension ctxt f e;
      item_attributes ctxt f x.pctf_attributes

and class_signature ctxt f { pcsig_self = ct; pcsig_fields = l ;_} =
  pp f "@[<hv0>@[<hv2>object@[<1>%a@]@ %a@]@ end@]"
    (fun f -> function
         {ptyp_desc=Ptyp_any; ptyp_attributes=[]; _} -> ()
       | ct -> pp f " (%a)" (core_type ctxt) ct) ct
    (list (class_type_field ctxt) ~sep:"@;") l

(* call [class_signature] called by [class_signature] *)
and class_type ctxt f x =
  match x.pcty_desc with
  | Pcty_signature cs ->
      class_signature ctxt f cs;
      attributes ctxt f x.pcty_attributes
  | Pcty_constr (li, l) ->
      pp f "%a%a%a"
        (fun f l -> match l with
           | [] -> ()
           | _  -> pp f "[%a]@ " (list (core_type ctxt) ~sep:"," ) l) l
        longident_loc li
        (attributes ctxt) x.pcty_attributes
  | Pcty_arrow (l, co, cl) ->
      pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
        (type_with_label ctxt) (l,co,[])
        (class_type ctxt) cl
  | Pcty_extension e ->
      extension ctxt f e;
      attributes ctxt f x.pcty_attributes
  | Pcty_open (o, e) ->
      pp f "@[<2>let open%s %a in@;%a@]"
        (override o.popen_override) longident_loc o.popen_expr
        (class_type ctxt) e

(* [class type a = object end] *)
and class_type_declaration_list ctxt f l =
  let class_type_declaration kwd f x =
    let { pci_params=ls; pci_name={ txt; _ }; _ } = x in
    pp f "@[<2>%s %a%a%a@ =@ %a@]%a" kwd
      virtual_flag x.pci_virt
      (class_params_def ctxt) ls
      ident_of_name txt
      (class_type ctxt) x.pci_expr
      (item_attributes ctxt) x.pci_attributes
  in
  match l with
  | [] -> ()
  | [x] -> class_type_declaration "class type" f x
  | x :: xs ->
      pp f "@[<v>%a@,%a@]"
        (class_type_declaration "class type") x
        (list ~sep:"@," (class_type_declaration "and")) xs

and class_field ctxt f x =
  match x.pcf_desc with
  | Pcf_inherit (ovf, ce, so) ->
      pp f "@[<2>inherit@ %s@ %a%a@]%a" (override ovf)
        (class_expr ctxt) ce
        (fun f so -> match so with
           | None -> ();
           | Some (s) -> pp f "@ as %a" ident_of_name s.txt ) so
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
      pp f "@[<2>val%s %a%a =@;%a@]%a" (override ovf)
        mutable_flag mf
        ident_of_name s.txt
        (expression ctxt) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_method (s, pf, Cfk_virtual ct) ->
      pp f "@[<2>method virtual %a %a :@;%a@]%a"
        private_flag pf
        ident_of_name s.txt
        (core_type ctxt) ct
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_virtual ct) ->
      pp f "@[<2>val virtual %a%a :@ %a@]%a"
        mutable_flag mf
        ident_of_name s.txt
        (core_type ctxt) ct
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
      let bind e =
        binding ctxt f
          {pvb_pat=
             {ppat_desc=Ppat_var s;
              ppat_loc=Location.none;
              ppat_loc_stack=[];
              ppat_attributes=[]};
           pvb_expr=e;
           pvb_constraint=None;
           pvb_attributes=[];
           pvb_modes=[];
           pvb_loc=Location.none;
          }
      in
      pp f "@[<2>method%s %a%a@]%a"
        (override ovf)
        private_flag pf
        (fun f -> function
           | {pexp_desc=Pexp_poly (e, Some ct); pexp_attributes=[]; _} ->
               pp f "%a :@;%a=@;%a"
                 ident_of_name s.txt (core_type ctxt) ct (expression ctxt) e
           | {pexp_desc=Pexp_poly (e, None); pexp_attributes=[]; _} ->
               bind e
           | _ -> bind e) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_constraint (ct1, ct2) ->
      pp f "@[<2>constraint %a =@;%a@]%a"
        (core_type ctxt) ct1
        (core_type ctxt) ct2
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_initializer (e) ->
      pp f "@[<2>initializer@ %a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_attribute a -> floating_attribute ctxt f a
  | Pcf_extension e ->
      item_extension ctxt f e;
      item_attributes ctxt f x.pcf_attributes

and class_structure ctxt f { pcstr_self = p; pcstr_fields =  l } =
  pp f "@[<hv0>@[<hv2>object%a@;%a@]@;end@]"
    (fun f p -> match p.ppat_desc with
       | Ppat_any -> ()
       | Ppat_constraint _ -> pp f " %a" (pattern ctxt) p
       | _ -> pp f " (%a)" (pattern ctxt) p) p
    (list (class_field ctxt)) l

and class_expr ctxt f x =
  if x.pcl_attributes <> [] then begin
    pp f "((%a)%a)" (class_expr ctxt) {x with pcl_attributes=[]}
      (attributes ctxt) x.pcl_attributes
  end else
    match x.pcl_desc with
    | Pcl_structure (cs) -> class_structure ctxt f cs
    | Pcl_fun (l, eo, p, e) ->
        pp f "fun@ %a@ ->@ %a"
          (label_exp ctxt) (l,eo,p)
          (class_expr ctxt) e
    | Pcl_let (rf, l, ce) ->
        pp f "%a@ in@ %a"
          (bindings ctxt) (rf,l)
          (class_expr ctxt) ce
    | Pcl_apply (ce, l) ->
        pp f "((%a)@ %a)" (* Cf: #7200 *)
          (class_expr ctxt) ce
          (list (label_x_expression_param ctxt)) l
    | Pcl_constr (li, l) ->
        pp f "%a%a"
          (fun f l-> if l <>[] then
              pp f "[%a]@ "
                (list (core_type ctxt) ~sep:",") l) l
          longident_loc li
    | Pcl_constraint (ce, ct) ->
        pp f "(%a@ :@ %a)"
          (class_expr ctxt) ce
          (class_type ctxt) ct
    | Pcl_extension e -> extension ctxt f e
    | Pcl_open (o, e) ->
        pp f "@[<2>let open%s %a in@;%a@]"
          (override o.popen_override) longident_loc o.popen_expr
          (class_expr ctxt) e

and include_ : 'a. ctxt -> formatter ->
                   contents:(ctxt -> formatter -> 'a -> unit) ->
                   'a include_infos ->
                   unit =
  fun ctxt f ~contents incl ->
    pp f "@[<hov2>include%a@ %a@]%a"
      include_kind incl.pincl_kind
      (contents ctxt) incl.pincl_mod
      (item_attributes ctxt) incl.pincl_attributes

and sig_include ctxt f incl moda =
  include_ ctxt f ~contents:module_type incl;
  optional_atat_modalities f moda

and kind_abbrev ctxt f name jkind =
  pp f "@[<hov2>kind_abbrev_@ %a@ =@ %a@]"
    string_loc name
    (jkind_annotation ctxt) jkind

and module_type ctxt f x =
  if x.pmty_attributes <> [] then begin
    pp f "((%a)%a)" (module_type ctxt) {x with pmty_attributes=[]}
      (attributes ctxt) x.pmty_attributes
  end else
    match Jane_syntax.Module_type.of_ast x with
    | Some (jmty, attrs) -> module_type_jane_syntax ctxt attrs f jmty
    | None ->
    match x.pmty_desc with
    | Pmty_functor (Unit, mt2) ->
        pp f "@[<hov2>() ->@ %a@]" (module_type ctxt) mt2
    | Pmty_functor (Named (s, mt1), mt2) ->
        begin match s.txt with
        | None ->
            pp f "@[<hov2>%a@ ->@ %a@]"
              (module_type1 ctxt) mt1 (module_type ctxt) mt2
        | Some name ->
            pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" name
              (module_type ctxt) mt1 (module_type ctxt) mt2
        end
    | Pmty_with (mt, []) -> module_type ctxt f mt
    | Pmty_with (mt, l) ->
        pp f "@[<hov2>%a@ with@ %a@]"
          (module_type1 ctxt) mt
          (list (with_constraint ctxt) ~sep:"@ and@ ") l
    | _ -> module_type1 ctxt f x

and module_type_jane_syntax ctxt attrs f (mty : Jane_syntax.Module_type.t) =
  if attrs <> [] then
    pp f "((%a)%a)"
      (module_type_jane_syntax ctxt []) mty
      (attributes ctxt) attrs
  else
    match mty with
    | Jmty_strengthen { mty; mod_id } ->
        pp f "@[<hov2>%a@ with@ %a@]"
          (module_type1 ctxt) mty
          longident_loc mod_id

and with_constraint ctxt f = function
  | Pwith_type (li, ({ptype_params= ls ;_} as td)) ->
      pp f "type@ %a %a =@ %a"
        (type_params ctxt) ls
        longident_loc li (type_declaration ctxt) td
  | Pwith_module (li, li2) ->
      pp f "module %a =@ %a" longident_loc li longident_loc li2;
  | Pwith_modtype (li, mty) ->
      pp f "module type %a =@ %a" longident_loc li (module_type ctxt) mty;
  | Pwith_typesubst (li, ({ptype_params=ls;_} as td)) ->
      pp f "type@ %a %a :=@ %a"
        (type_params ctxt) ls
        longident_loc li
        (type_declaration ctxt) td
  | Pwith_modsubst (li, li2) ->
      pp f "module %a :=@ %a" longident_loc li longident_loc li2
  | Pwith_modtypesubst (li, mty) ->
      pp f "module type %a :=@ %a" longident_loc li (module_type ctxt) mty;


and module_type1 ctxt f x =
  match Jane_syntax.Module_type.of_ast x with
  | Some (jmty, attrs) -> module_type_jane_syntax1 ctxt attrs f jmty
  | None ->
  if x.pmty_attributes <> [] then module_type ctxt f x
  else match x.pmty_desc with
    | Pmty_ident li ->
        pp f "%a" longident_loc li;
    | Pmty_alias li ->
        pp f "(module %a)" longident_loc li;
    | Pmty_signature (s) ->
        pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (* "@[<hov>sig@ %a@ end@]" *)
          (list (signature_item ctxt)) s (* FIXME wrong indentation*)
    | Pmty_typeof me ->
        pp f "@[<hov2>module@ type@ of@ %a@]" (module_expr ctxt) me
    | Pmty_extension e -> extension ctxt f e
    | _ -> paren true (module_type ctxt) f x

and module_type_jane_syntax1 ctxt attrs f : Jane_syntax.Module_type.t -> _ =
  function
  | Jmty_strengthen _ as jmty ->
      paren true (module_type_jane_syntax ctxt attrs) f jmty

and signature ctxt f x =  list ~sep:"@\n" (signature_item ctxt) f x

and sig_layout ctxt f
  : Jane_syntax.Layouts.signature_item -> _ = function
  | Lsig_kind_abbrev (name, jkind) ->
      kind_abbrev ctxt f name jkind

and signature_item_jane_syntax ctxt f : Jane_syntax.Signature_item.t -> _ =
  function
  | Jsig_layout sigi -> sig_layout ctxt f sigi

and signature_item ctxt f x : unit =
  match Jane_syntax.Signature_item.of_ast x with
  | Some jsigi -> signature_item_jane_syntax ctxt f jsigi
  | None ->
  match x.psig_desc with
  | Psig_type (rf, l) ->
      type_def_list ctxt f (rf, true, l)
  | Psig_typesubst l ->
      (* Psig_typesubst is never recursive, but we specify [Recursive] here to
         avoid printing a [nonrec] flag, which would be rejected by the parser.
      *)
      type_def_list ctxt f (Recursive, false, l)
  | Psig_value vd ->
      let intro = if vd.pval_prim = [] then "val" else "external" in
      pp f "@[<2>%s@ %a@ :@ %a@]%a" intro
        ident_of_name vd.pval_name.txt
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Psig_typext te ->
      type_extension ctxt f te
  | Psig_exception ed ->
      exception_declaration ctxt f ed
  | Psig_class l ->
      let class_description kwd f ({pci_params=ls;pci_name={txt;_};_} as x) =
        pp f "@[<2>%s %a%a%a@;:@;%a@]%a" kwd
          virtual_flag x.pci_virt
          (class_params_def ctxt) ls
          ident_of_name txt
          (class_type ctxt) x.pci_expr
          (item_attributes ctxt) x.pci_attributes
      in begin
        match l with
        | [] -> ()
        | [x] -> class_description "class" f x
        | x :: xs ->
            pp f "@[<v>%a@,%a@]"
              (class_description "class") x
              (list ~sep:"@," (class_description "and")) xs
      end
  | Psig_module ({pmd_type={pmty_desc=Pmty_alias alias;
                            pmty_attributes=[]; _};_} as pmd) ->
      pp f "@[<hov>module@ %s@ =@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_")
        longident_loc alias
        (item_attributes ctxt) pmd.pmd_attributes
  | Psig_module pmd ->
      pp f "@[<hov>module@ %s@ :@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_")
        (module_type ctxt) pmd.pmd_type
        (item_attributes ctxt) pmd.pmd_attributes
  | Psig_modsubst pms ->
      pp f "@[<hov>module@ %s@ :=@ %a@]%a" pms.pms_name.txt
        longident_loc pms.pms_manifest
        (item_attributes ctxt) pms.pms_attributes
  | Psig_open od ->
      pp f "@[<hov2>open%s@ %a@]%a"
        (override od.popen_override)
        longident_loc od.popen_expr
        (item_attributes ctxt) od.popen_attributes
  | Psig_include (incl, modalities) ->
      sig_include ctxt f incl modalities
  | Psig_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      pp f "@[<hov2>module@ type@ %s%a@]%a"
        s.txt
        (fun f md -> match md with
           | None -> ()
           | Some mt ->
               pp_print_space f () ;
               pp f "@ =@ %a" (module_type ctxt) mt
        ) md
        (item_attributes ctxt) attrs
  | Psig_modtypesubst {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      let md = match md with
        | None -> assert false (* ast invariant *)
        | Some mt -> mt in
      pp f "@[<hov2>module@ type@ %s@ :=@ %a@]%a"
        s.txt (module_type ctxt) md
        (item_attributes ctxt) attrs
  | Psig_class_type (l) -> class_type_declaration_list ctxt f l
  | Psig_recmodule decls ->
      let rec  string_x_module_type_list f ?(first=true) l =
        match l with
        | [] -> () ;
        | pmd :: tl ->
            if not first then
              pp f "@ @[<hov2>and@ %s:@ %a@]%a"
                (Option.value pmd.pmd_name.txt ~default:"_")
                (module_type1 ctxt) pmd.pmd_type
                (item_attributes ctxt) pmd.pmd_attributes
            else
              pp f "@[<hov2>module@ rec@ %s:@ %a@]%a"
                (Option.value pmd.pmd_name.txt ~default:"_")
                (module_type1 ctxt) pmd.pmd_type
                (item_attributes ctxt) pmd.pmd_attributes;
            string_x_module_type_list f ~first:false tl
      in
      string_x_module_type_list f decls
  | Psig_attribute a -> floating_attribute ctxt f a
  | Psig_extension(e, a) ->
      item_extension ctxt f e;
      item_attributes ctxt f a

and module_expr ctxt f x =
  if x.pmod_attributes <> [] then
    pp f "((%a)%a)" (module_expr ctxt) {x with pmod_attributes=[]}
      (attributes ctxt) x.pmod_attributes
  else match x.pmod_desc with
    | Pmod_structure (s) ->
        pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]"
          (list (structure_item ctxt) ~sep:"@\n") s;
    | Pmod_constraint (me, mt) ->
        pp f "@[<hov2>(%a@ :@ %a)@]"
          (module_expr ctxt) me
          (module_type ctxt) mt
    | Pmod_ident (li) ->
        pp f "%a" longident_loc li;
    | Pmod_functor (Unit, me) ->
        pp f "functor ()@;->@;%a" (module_expr ctxt) me
    | Pmod_functor (Named (s, mt), me) ->
        pp f "functor@ (%s@ :@ %a)@;->@;%a"
          (Option.value s.txt ~default:"_")
          (module_type ctxt) mt (module_expr ctxt) me
    | Pmod_apply (me1, me2) ->
        pp f "(%a)(%a)" (module_expr ctxt) me1 (module_expr ctxt) me2
        (* Cf: #7200 *)
    | Pmod_apply_unit me1 ->
        pp f "(%a)()" (module_expr ctxt) me1
    | Pmod_unpack e ->
        pp f "(val@ %a)" (expression ctxt) e
    | Pmod_extension e -> extension ctxt f e

and structure ctxt f x = list ~sep:"@\n" (structure_item ctxt) f x

and payload ctxt f = function
  | PStr [{pstr_desc = Pstr_eval (e, attrs)}] ->
      pp f "@[<2>%a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) attrs
  | PStr x -> structure ctxt f x
  | PTyp x -> pp f ":@ "; core_type ctxt f x
  | PSig x -> pp f ":@ "; signature ctxt f x
  | PPat (x, None) -> pp f "?@ "; pattern ctxt f x
  | PPat (x, Some e) ->
      pp f "?@ "; pattern ctxt f x;
      pp f " when "; expression ctxt f e

and pp_print_pexp_newtype ctxt sep f x =
  (* We go to some trouble to print nested [Lexp_newtype] as
     newtype parameters of the same "fun" (rather than printing several nested
     "fun (type a) -> ..."). This isn't necessary for round-tripping -- it just
     makes the pretty-printing a bit prettier. *)
  match Jane_syntax.Expression.of_ast x with
  | Some (Jexp_layout (Lexp_newtype (str, lay, e)), []) ->
      pp f "@[(type@ %a :@ %a)@]@ %a"
        ident_of_name str.txt
        (jkind_annotation ctxt) lay
        (pp_print_pexp_newtype ctxt sep) e
  | Some (jst, attrs) ->
      pp f "%s@;%a" sep (jane_syntax_expr ctxt attrs ~parens:false) jst
  | None ->
  if x.pexp_attributes <> [] then pp f "%s@;%a" sep (expression ctxt) x
  else
    match x.pexp_desc with
    | Pexp_newtype (str,e) ->
      pp f "(type@ %a)@ %a" ident_of_name str.txt (pp_print_pexp_newtype ctxt sep) e
    | _ ->
       pp f "%s@;%a" sep (expression ctxt) x

and pp_print_params_then_equals ctxt f x =
  if x.pexp_attributes <> [] then pp f "=@;%a" (expression ctxt) x
  else
  match x.pexp_desc with
  | Pexp_function (params, constraint_, body) ->
      function_params_then_body ctxt f params constraint_ body
        ~delimiter:"="
  | _ -> pp_print_pexp_newtype ctxt "=" f x

(* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
and binding ctxt f {pvb_pat=p; pvb_expr=x; pvb_constraint = ct; pvb_modes = modes; _} =
  (* .pvb_attributes have already been printed by the caller, #bindings *)
  let _, modes = split_out_legacy_modes modes in
  match ct with
  | Some (Pvc_constraint { locally_abstract_univars = []; typ }) ->
      pp f "%a@;:@;%a%a@;=@;%a"
        (simple_pattern ctxt) p
        (core_type ctxt) typ
        optional_atat_modes modes
        (expression ctxt) x
  | Some (Pvc_constraint { locally_abstract_univars = vars; typ }) ->
      pp f "%a@;: type@;%a.@;%a%a@;=@;%a"
        (simple_pattern ctxt) p (list pp_print_string ~sep:"@;")
        (List.map (fun x -> x.txt) vars)
        (core_type ctxt) typ
        optional_atat_modes modes
        (expression ctxt) x
  | Some (Pvc_coercion {ground=None; coercion }) ->
      pp f "%a@;:>@;%a%a@;=@;%a"
        (simple_pattern ctxt) p
        (core_type ctxt) coercion
        optional_at_modes modes
        (expression ctxt) x
  | Some (Pvc_coercion {ground=Some ground; coercion }) ->
      pp f "%a@;:%a@;:>@;%a%a@;=@;%a"
        (simple_pattern ctxt) p
        (core_type ctxt) ground
        (core_type ctxt) coercion
        optional_atat_modes modes
        (expression ctxt) x
  | None ->
      (* CR layouts 1.5: We just need to check for [is_desugared_gadt] because
         the parser hasn't been upgraded to parse [let x : type a. ... = ...] as
         [Pvb_constraint] as it has been upstream. Once we encode that with Jane
         Syntax appropriately, we can delete this code.
      *)
      let tyvars_str tyvars = List.map (fun v -> v.txt) tyvars in
      let is_desugared_gadt p e =
        let gadt_pattern =
          match p with
          | {ppat_desc=Ppat_constraint({ppat_desc=Ppat_var _} as pat,
                                      Some {ptyp_desc=Ptyp_poly (args_tyvars, rt)}, _);
            ppat_attributes=[]}->
              Some (pat, args_tyvars, rt)
          | _ -> None in
        let rec gadt_exp tyvars e =
          match e with
          (* no need to handle jkind annotations here; the extracted variables
            don't get printed -- they're just used to decide how to print *)
          | {pexp_desc=Pexp_newtype (tyvar, e); pexp_attributes=[]} ->
              gadt_exp (tyvar :: tyvars) e
          | {pexp_desc=Pexp_constraint (e, Some ct, _); pexp_attributes=[]} ->
              Some (List.rev tyvars, e, ct)
          | _ -> None in
        let gadt_exp = gadt_exp [] e in
        match gadt_pattern, gadt_exp with
        | Some (p, pt_tyvars, pt_ct), Some (e_tyvars, e, e_ct)
          when tyvars_str pt_tyvars = tyvars_str e_tyvars ->
            let ety = Ast_helper.Typ.varify_constructors e_tyvars e_ct in
            if ety = pt_ct then
              Some (p, pt_tyvars, e_ct, e) else None
        | _ -> None
      in
      begin match is_desugared_gadt p x with
      | Some (p, (_ :: _ as tyvars), ct, e) ->
          pp f "%a@;: type@;%a.@;%a%a@;=@;%a"
            (simple_pattern ctxt) p
            (list pp_print_string ~sep:"@;")
            (tyvars_str tyvars)
            (core_type ctxt) ct
            optional_atat_modes modes
            (expression ctxt) e
      | _ ->
        begin match p with
        | {ppat_desc=Ppat_var _; ppat_attributes=[]} ->
          begin match modes with
          | [] ->
            pp f "%a@ %a"
              (simple_pattern ctxt) p
              (pp_print_params_then_equals ctxt) x
          | _ ->
            pp f "(%a%a)@ %a"
              (simple_pattern ctxt) p
              optional_at_modes modes
              (pp_print_params_then_equals ctxt) x
          end
        | _ ->
          pp f "%a%a@;=@;%a"
            (pattern ctxt) p
            optional_at_modes modes
            (expression ctxt) x
        end
      end

(* [in] is not printed *)
and bindings ctxt f (rf,l) =
  let binding kwd rf f x =
    (* The other modes are printed inside [binding] *)
    let legacy, _ = split_out_legacy_modes x.pvb_modes in
    pp f "@[<2>%s %a%a%a@]%a" kwd rec_flag rf
      optional_legacy_modes legacy
      (binding ctxt) x
      (item_attributes ctxt) x.pvb_attributes
  in
  match l with
  | [] -> ()
  | [x] -> binding "let" rf f x
  | x::xs ->
      pp f "@[<v>%a@,%a@]"
        (binding "let" rf) x
        (list ~sep:"@," (binding "and" Nonrecursive)) xs

and binding_op ctxt f x =
  match x.pbop_pat, x.pbop_exp with
  | {ppat_desc = Ppat_var { txt=pvar; _ }; ppat_attributes = []; _},
    {pexp_desc = Pexp_ident { txt=Lident evar; _}; pexp_attributes = []; _}
       when pvar = evar ->
     pp f "@[<2>%s %s@]" x.pbop_op.txt evar
  | pat, exp ->
     pp f "@[<2>%s %a@;=@;%a@]"
       x.pbop_op.txt (pattern ctxt) pat (expression ctxt) exp

and str_layout ctxt f
  : Jane_syntax.Layouts.structure_item -> _ = function
  | Lstr_kind_abbrev (name, jkind) ->
      kind_abbrev ctxt f name jkind

and structure_item_jane_syntax ctxt f : Jane_syntax.Structure_item.t -> _ =
  function
  | Jstr_layout stri -> str_layout ctxt f stri

and structure_item ctxt f x =
  match Jane_syntax.Structure_item.of_ast x with
  | Some jstri -> structure_item_jane_syntax ctxt f jstri
  | None ->
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
      pp f "@[<hov2>;;%a@]%a"
        (expression ctxt) e
        (item_attributes ctxt) attrs
  | Pstr_type (_, []) -> assert false
  | Pstr_type (rf, l)  -> type_def_list ctxt f (rf, true, l)
  | Pstr_value (rf, l) ->
      (* pp f "@[<hov2>let %a%a@]"  rec_flag rf bindings l *)
      pp f "@[<2>%a@]" (bindings ctxt) (rf,l)
  | Pstr_typext te -> type_extension ctxt f te
  | Pstr_exception ed -> exception_declaration ctxt f ed
  | Pstr_module x ->
      let rec module_helper = function
        | {pmod_desc=Pmod_functor(arg_opt,me'); pmod_attributes = []} ->
            begin match arg_opt with
            | Unit -> pp f "()"
            | Named (s, mt) ->
              pp f "(%s:%a)" (Option.value s.txt ~default:"_")
                (module_type ctxt) mt
            end;
            module_helper me'
        | me -> me
      in
      pp f "@[<hov2>module %s%a@]%a"
        (Option.value x.pmb_name.txt ~default:"_")
        (fun f me ->
           let me = module_helper me in
           match me with
           | {pmod_desc=
                Pmod_constraint
                  (me',
                   ({pmty_desc=(Pmty_ident (_)
                               | Pmty_signature (_));_} as mt));
              pmod_attributes = []} ->
               pp f " :@;%a@;=@;%a@;"
                 (module_type ctxt) mt (module_expr ctxt) me'
           | _ -> pp f " =@ %a" (module_expr ctxt) me
        ) x.pmb_expr
        (item_attributes ctxt) x.pmb_attributes
  | Pstr_open od ->
      pp f "@[<2>open%s@;%a@]%a"
        (override od.popen_override)
        (module_expr ctxt) od.popen_expr
        (item_attributes ctxt) od.popen_attributes
  | Pstr_modtype {pmtd_name=s; pmtd_type=md; pmtd_attributes=attrs} ->
      pp f "@[<hov2>module@ type@ %s%a@]%a"
        s.txt
        (fun f md -> match md with
           | None -> ()
           | Some mt ->
               pp_print_space f () ;
               pp f "@ =@ %a" (module_type ctxt) mt
        ) md
        (item_attributes ctxt) attrs
  | Pstr_class l ->
      let extract_class_args cl =
        let rec loop acc = function
          | {pcl_desc=Pcl_fun (l, eo, p, cl'); pcl_attributes = []} ->
              loop ((l,eo,p) :: acc) cl'
          | cl -> List.rev acc, cl
        in
        let args, cl = loop [] cl in
        let constr, cl =
          match cl with
          | {pcl_desc=Pcl_constraint (cl', ct); pcl_attributes = []} ->
              Some ct, cl'
          | _ -> None, cl
        in
        args, constr, cl
      in
      let class_constraint f ct = pp f ": @[%a@] " (class_type ctxt) ct in
      let class_declaration kwd f
          ({pci_params=ls; pci_name={txt;_}; _} as x) =
        let args, constr, cl = extract_class_args x.pci_expr in
        pp f "@[<2>%s %a%a%a %a%a=@;%a@]%a" kwd
          virtual_flag x.pci_virt
          (class_params_def ctxt) ls
          ident_of_name txt
          (list (label_exp ctxt) ~last:"@ ") args
          (option class_constraint) constr
          (class_expr ctxt) cl
          (item_attributes ctxt) x.pci_attributes
      in begin
        match l with
        | [] -> ()
        | [x] -> class_declaration "class" f x
        | x :: xs ->
            pp f "@[<v>%a@,%a@]"
              (class_declaration "class") x
              (list ~sep:"@," (class_declaration "and")) xs
      end
  | Pstr_class_type l -> class_type_declaration_list ctxt f l
  | Pstr_primitive vd ->
      pp f "@[<hov2>external@ %a@ :@ %a@]%a"
        ident_of_name vd.pval_name.txt
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Pstr_include incl ->
      include_ ctxt f ~contents:module_expr incl
  | Pstr_recmodule decls -> (* 3.07 *)
      let aux f = function
        | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) ->
            pp f "@[<hov2>@ and@ %s:%a@ =@ %a@]%a"
              (Option.value pmb.pmb_name.txt ~default:"_")
              (module_type ctxt) typ
              (module_expr ctxt) expr
              (item_attributes ctxt) pmb.pmb_attributes
        | pmb ->
            pp f "@[<hov2>@ and@ %s@ =@ %a@]%a"
              (Option.value pmb.pmb_name.txt ~default:"_")
              (module_expr ctxt) pmb.pmb_expr
              (item_attributes ctxt) pmb.pmb_attributes
      in
      begin match decls with
      | ({pmb_expr={pmod_desc=Pmod_constraint (expr, typ)}} as pmb) :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]%a@ %a@]"
            (Option.value pmb.pmb_name.txt ~default:"_")
            (module_type ctxt) typ
            (module_expr ctxt) expr
            (item_attributes ctxt) pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2) l2
      | pmb :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s@ =@ %a@]%a@ %a@]"
            (Option.value pmb.pmb_name.txt ~default:"_")
            (module_expr ctxt) pmb.pmb_expr
            (item_attributes ctxt) pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2) l2
      | _ -> assert false
      end
  | Pstr_attribute a -> floating_attribute ctxt f a
  | Pstr_extension(e, a) ->
      item_extension ctxt f e;
      item_attributes ctxt f a

and type_param ctxt f (ct, (a,b)) =
  pp f "%s%s%a" (type_variance a) (type_injectivity b) (core_type ctxt) ct

and type_params ctxt f = function
  | [] -> ()
  | l -> pp f "%a " (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",@;") l

and type_def_list ctxt f (rf, exported, l) =
  let type_decl kwd rf f x =
    let eq =
      if (x.ptype_kind = Ptype_abstract)
         && (x.ptype_manifest = None) then ""
      else if exported then " ="
      else " :="
    in
    let layout_annot, x =
      match Jane_syntax.Layouts.of_type_declaration x with
      | None -> Format.dprintf "", x
      | Some (jkind, remaining_attributes) ->
          Format.dprintf " : %a"
            (jkind_annotation ctxt) jkind,
          { x with ptype_attributes = remaining_attributes }
    in
    pp f "@[<2>%s %a%a%a%t%s%a@]%a" kwd
      nonrec_flag rf
      (type_params ctxt) x.ptype_params
      ident_of_name x.ptype_name.txt
      layout_annot eq
      (type_declaration ctxt) x
      (item_attributes ctxt) x.ptype_attributes
  in
  match l with
  | [] -> assert false
  | [x] -> type_decl "type" rf f x
  | x :: xs -> pp f "@[<v>%a@,%a@]"
                 (type_decl "type" rf) x
                 (list ~sep:"@," (type_decl "and" Recursive)) xs

and record_declaration ctxt f lbls =
  let type_record_field f pld =
    let legacy, m = split_out_legacy_modalities pld.pld_modalities in
    pp f "@[<2>%a%a%a:@;%a%a@;%a@]"
      mutable_flag pld.pld_mutable
      optional_legacy_modalities legacy
      ident_of_name pld.pld_name.txt
      (core_type ctxt) pld.pld_type
      optional_atat_modalities m
      (attributes ctxt) pld.pld_attributes
  in
  pp f "{@\n%a}"
    (list type_record_field ~sep:";@\n" )  lbls

and type_declaration ctxt f x =
  (* type_declaration has an attribute field,
     but it's been printed by the caller of this method *)
  let priv f =
    match x.ptype_private with
    | Public -> ()
    | Private -> pp f "@;private"
  in
  let manifest f =
    match x.ptype_manifest with
    | None -> ()
    | Some y ->
        if x.ptype_kind = Ptype_abstract then
          pp f "%t@;%a" priv (core_type ctxt) y
        else
          pp f "@;%a" (core_type ctxt) y
  in
  let constructor_declaration f pcd =
    pp f "|@;";
    let vars_jkinds, attrs =
      match Jane_syntax.Layouts.of_constructor_declaration pcd with
      | None -> List.map (fun v -> v, None) pcd.pcd_vars, pcd.pcd_attributes
      | Some stuff -> stuff
    in
    constructor_declaration ctxt f
      (pcd.pcd_name.txt, vars_jkinds, pcd.pcd_args, pcd.pcd_res, attrs)
  in
  let repr f =
    let intro f =
      if x.ptype_manifest = None then ()
      else pp f "@;="
    in
    match x.ptype_kind with
    | Ptype_variant xs ->
      let variants fmt xs =
        if xs = [] then pp fmt " |" else
          pp fmt "@\n%a" (list ~sep:"@\n" constructor_declaration) xs
      in pp f "%t%t%a" intro priv variants xs
    | Ptype_abstract -> ()
    | Ptype_record l ->
        pp f "%t%t@;%a" intro priv (record_declaration ctxt) l
    | Ptype_open -> pp f "%t%t@;.." intro priv
  in
  let constraints f =
    List.iter
      (fun (ct1,ct2,_) ->
         pp f "@[<hov2>@ constraint@ %a@ =@ %a@]"
           (core_type ctxt) ct1 (core_type ctxt) ct2)
      x.ptype_cstrs
  in
  pp f "%t%t%t" manifest repr constraints

and type_extension ctxt f x =
  let extension_constructor f x =
    pp f "@\n|@;%a" (extension_constructor ctxt) x
  in
  pp f "@[<2>type %a%a += %a@ %a@]%a"
    (fun f -> function
       | [] -> ()
       | l ->
           pp f "%a@;" (list (type_param ctxt) ~first:"(" ~last:")" ~sep:",") l)
    x.ptyext_params
    longident_loc x.ptyext_path
    private_flag x.ptyext_private (* Cf: #7200 *)
    (list ~sep:"" extension_constructor)
    x.ptyext_constructors
    (item_attributes ctxt) x.ptyext_attributes

and constructor_declaration ctxt f (name, vars_jkinds, args, res, attrs) =
  let name =
    match name with
    | "::" -> "(::)"
    | s -> s in
  let pp_vars f vls =
    match vls with
    | [] -> ()
    | _  -> pp f "%a@;.@;" (list (tyvar_jkind_loc ctxt ~print_quote:true) ~sep:"@;")
                           vls
  in
  match res with
  | None ->
      pp f "%s%a@;%a" name
        (fun f -> function
           | Pcstr_tuple [] -> ()
           | Pcstr_tuple l ->
             pp f "@;of@;%a" (list (modalities_type core_type1 ctxt) ~sep:"@;*@;") l
           | Pcstr_record l -> pp f "@;of@;%a" (record_declaration ctxt) l
        ) args
        (attributes ctxt) attrs
  | Some r ->
      pp f "%s:@;%a%a@;%a" name
        pp_vars vars_jkinds
        (fun f -> function
           | Pcstr_tuple [] -> core_type1 ctxt f r
           | Pcstr_tuple l -> pp f "%a@;->@;%a"
                                (list (modalities_type core_type1 ctxt) ~sep:"@;*@;") l
                                (core_type1 ctxt) r
           | Pcstr_record l ->
               pp f "%a@;->@;%a" (record_declaration ctxt) l (core_type1 ctxt) r
        )
        args
        (attributes ctxt) attrs

and extension_constructor ctxt f x =
  (* Cf: #7200 *)
  match Jane_syntax.Extension_constructor.of_ast x with
  | Some (jext, attrs) ->
    extension_constructor_jst ctxt f x.pext_name attrs jext
  | None ->
  match x.pext_kind with
  | Pext_decl(v, l, r) ->
      constructor_declaration ctxt f
        (x.pext_name.txt, List.map (fun v -> v, None) v, l, r, x.pext_attributes)
  | Pext_rebind li ->
      pp f "%s@;=@;%a%a" x.pext_name.txt
        longident_loc li
        (attributes ctxt) x.pext_attributes

and extension_constructor_jst ctxt f name attrs :
  Jane_syntax.Extension_constructor.t -> _ = function
  | Jext_layout (Lext_decl(vl, l, r)) ->
    constructor_declaration ctxt f (name.txt, vl, l, r, attrs)

and case_list ctxt f l : unit =
  let aux f {pc_lhs; pc_guard; pc_rhs} =
    pp f "@;| @[<2>%a%a@;->@;%a@]"
      (pattern ctxt) pc_lhs (option (expression ctxt) ~first:"@;when@;")
      pc_guard (expression (under_pipe ctxt)) pc_rhs
  in
  list aux f l ~sep:""

and label_x_expression_param ctxt f (l,e) =
  let simple_name = match e with
    | {pexp_desc=Pexp_ident {txt=Lident l;_};
       pexp_attributes=[]} -> Some l
    | _ -> None
  in match l with
  | Nolabel  -> expression2 ctxt f e (* level 2*)
  | Optional str ->
      if Some str = simple_name then
        pp f "?%a" ident_of_name str
      else
        pp f "?%a:%a" ident_of_name str (simple_expr ctxt) e
  | Labelled lbl ->
      if Some lbl = simple_name then
        pp f "~%a" ident_of_name lbl
      else
        pp f "~%a:%a" ident_of_name lbl (simple_expr ctxt) e

and tuple_component ctxt f (l,e) =
  let simple_name = match e with
    | {pexp_desc=Pexp_ident {txt=Lident l;_};
       pexp_attributes=[]} -> Some l
    | _ -> None
  in match (simple_name, l) with
  (* Labeled component can be represented with pun *)
  | Some simple_name, Some lbl when String.equal simple_name lbl -> pp f "~%s" lbl
  (* Labeled component general case *)
  | _, Some lbl -> pp f "~%s:%a" lbl (simple_expr ctxt) e
  (* Unlabeled component *)
  | _, None  -> expression2 ctxt f e (* level 2*)

and directive_argument f x =
  match x.pdira_desc with
  | Pdir_string (s) -> pp f "@ %S" s
  | Pdir_int (n, None) -> pp f "@ %s" n
  | Pdir_int (n, Some m) -> pp f "@ %s%c" n m
  | Pdir_ident (li) -> pp f "@ %a" longident li
  | Pdir_bool (b) -> pp f "@ %s" (string_of_bool b)

(* [parens] is whether parens should be inserted around constructs that aren't
   already self-delimiting. E.g. immutable arrays are self-delimiting because
   they begin and end in a bracket.
*)
and jane_syntax_expr ctxt attrs f (jexp : Jane_syntax.Expression.t) ~parens =
  if attrs <> [] then
    pp f "((%a)@,%a)" (jane_syntax_expr ctxt [] ~parens:false) jexp
      (attributes ctxt) attrs
  else match jexp with
  | Jexp_comprehension x -> comprehension_expr ctxt f x
  | Jexp_immutable_array x -> immutable_array_expr ctxt f x
  | Jexp_layout x -> layout_expr ctxt f x ~parens
  | Jexp_tuple ltexp        -> labeled_tuple_expr ctxt f ~unboxed:false ltexp

and comprehension_expr ctxt f (cexp : Jane_syntax.Comprehensions.expression) =
  let punct, comp = match cexp with
    | Cexp_list_comprehension  comp ->
        "", comp
    | Cexp_array_comprehension (amut, comp) ->
        let punct = match amut with
          | Mutable  -> "|"
          | Immutable -> ":"
        in
        punct, comp
  in
  comprehension ctxt f ~open_:("[" ^ punct) ~close:(punct ^ "]") comp

and comprehension ctxt f ~open_ ~close cexp =
  let Jane_syntax.Comprehensions.{ body; clauses } = cexp in
  pp f "@[<hv0>@[<hv2>%s%a@ @[<hv2>%a@]%s@]@]"
    open_
    (expression ctxt) body
    (list ~sep:"@ " (comprehension_clause ctxt)) clauses
    close

and comprehension_clause ctxt f (x : Jane_syntax.Comprehensions.clause) =
  match x with
  | For bindings ->
      pp f "@[for %a@]" (list ~sep:"@]@ @[and " (comprehension_binding ctxt)) bindings
  | When cond ->
      pp f "@[when %a@]" (expression ctxt) cond

and comprehension_binding ctxt f x =
  let Jane_syntax.Comprehensions.{ pattern = pat; iterator; attributes = attrs } = x in
  pp f "%a%a %a"
    (attributes ctxt) attrs
    (pattern ctxt) pat
    (comprehension_iterator ctxt) iterator

and comprehension_iterator ctxt f (x : Jane_syntax.Comprehensions.iterator) =
  match x with
  | Range { start; stop; direction } ->
      pp f "=@ %a %a%a"
        (expression ctxt) start
        direction_flag direction
        (expression ctxt) stop
  | In seq ->
      pp f "in %a" (expression ctxt) seq

and immutable_array_expr ctxt f (x : Jane_syntax.Immutable_arrays.expression) =
  match x with
  | Iaexp_immutable_array elts ->
      pp f "@[<0>@[<2>[:%a:]@]@]"
         (list (simple_expr (under_semi ctxt)) ~sep:";") elts

(* [parens] is the same as the argument to [jane_syntax_expr]. *)
and layout_expr ctxt f (x : Jane_syntax.Layouts.expression) ~parens =
  match x with
  (* see similar case in [expression] *)
  | Lexp_newtype _ when parens || ctxt.pipe || ctxt.semi ->
    paren true (layout_expr reset_ctxt ~parens:false) f x
  | Lexp_constant x -> unboxed_constant ctxt f x
  | Lexp_newtype (lid, jkind, inner_expr) ->
    pp f "@[<2>fun@;(type@;%a :@;%a)@;%a@]"
      ident_of_name lid.txt
      (jkind_annotation ctxt) jkind
      (pp_print_pexp_newtype ctxt "->") inner_expr

and unboxed_constant _ctxt f (x : Jane_syntax.Layouts.constant)
  =
  match x with
  | Float (x, None) ->
    paren (first_is '-' x) (fun f -> pp f "%s") f (Misc.format_as_unboxed_literal x)
  | Float (x, Some suffix)
  | Integer (x, suffix) ->
    paren (first_is '-' x) (fun f (x, suffix) -> pp f "%s%c" x suffix) f
      (Misc.format_as_unboxed_literal x, suffix)

and function_param ctxt f { pparam_desc; pparam_loc = _ } =
  match pparam_desc with
  | Pparam_val (a, b, c) -> label_exp ctxt f (a, b, c)
  | Pparam_newtype (ty, None) -> pp f "(type %a)" ident_of_name ty.txt
  | Pparam_newtype (ty, Some annot) ->
      pp f "(type %a : %a)" ident_of_name ty.txt (jkind_annotation ctxt) annot

and function_body ctxt f x =
  match x with
  | Pfunction_body body -> expression ctxt f body
  | Pfunction_cases (cases, _, attrs) ->
    pp f "@[<hv>function%a%a@]"
      (item_attributes ctxt) attrs
      (case_list ctxt) cases

and function_constraint ctxt f x =
  (* We don't currently print [x.alloc_mode]; this would need
     to go on the enclosing [let] binding.
  *)
  (* Enable warning 9 to ensure that the record pattern doesn't miss any field.
  *)
  match[@ocaml.warning "+9"] x with
  | { type_constraint = Pconstraint ty; mode_annotations } ->
    let _, modes = split_out_legacy_modes mode_annotations in
    pp f ":@;%a%a" (core_type ctxt) ty optional_atat_modes modes
  | { type_constraint = Pcoerce (ty1, ty2); mode_annotations } ->
    let _, modes = split_out_legacy_modes mode_annotations in
    pp f "%a:>@;%a%a"
      (option ~first:":@;" (core_type ctxt)) ty1
      (core_type ctxt) ty2
      optional_atat_modes modes

and function_params_then_body ctxt f params constraint_ body ~delimiter =
  let pp_params f =
    match params with
    | [] -> ()
    | _ :: _ -> pp f "%a@;" (list (function_param ctxt) ~sep:"@ ") params
  in
  pp f "%t%a%s@;%a"
    pp_params
    (option (function_constraint ctxt) ~first:"@;") constraint_
    delimiter
    (function_body (under_functionrhs ctxt)) body

and labeled_tuple_expr ctxt f ~unboxed
      (x : Jane_syntax.Labeled_tuples.expression) =
  pp f "@[<hov2>%s(%a)@]" (if unboxed then "#" else "")
    (list (tuple_component ctxt) ~sep:",@;") x

(******************************************************************************)
(* All exported functions must be defined or redefined below here and wrapped in
   [export_printer] in order to ensure they are invariant with respecto which
   language extensions are enabled. *)

let Language_extension.For_pprintast.{ print_with_maximal_extensions } =
  Language_extension.For_pprintast.make_printer_exporter ()

let print_reset_with_maximal_extensions f =
  print_with_maximal_extensions (f reset_ctxt)

let toplevel_phrase f x =
  match x with
  | Ptop_def (s) ->pp f "@[<hov0>%a@]"  (list (structure_item reset_ctxt)) s
   (* pp_open_hvbox f 0; *)
   (* pp_print_list structure_item f s ; *)
   (* pp_close_box f (); *)
  | Ptop_dir {pdir_name; pdir_arg = None; _} ->
   pp f "@[<hov2>#%s@]" pdir_name.txt
  | Ptop_dir {pdir_name; pdir_arg = Some pdir_arg; _} ->
   pp f "@[<hov2>#%s@ %a@]" pdir_name.txt directive_argument pdir_arg

let toplevel_phrase = print_with_maximal_extensions toplevel_phrase

let expression f x =
  pp f "@[%a@]" (expression reset_ctxt) x

let expression = print_with_maximal_extensions expression

let string_of_expression x =
  ignore (flush_str_formatter ()) ;
  let f = str_formatter in
  expression f x;
  flush_str_formatter ()

let structure = print_reset_with_maximal_extensions structure

let string_of_structure x =
  ignore (flush_str_formatter ());
  let f = str_formatter in
  structure f x;
  flush_str_formatter ()

let top_phrase f x =
  pp_print_newline f ();
  toplevel_phrase f x;
  pp f ";;";
  pp_print_newline f ()

let longident = print_with_maximal_extensions longident
let core_type = print_reset_with_maximal_extensions core_type
let pattern = print_reset_with_maximal_extensions pattern
let signature = print_reset_with_maximal_extensions signature
let module_expr = print_reset_with_maximal_extensions module_expr
let module_type = print_reset_with_maximal_extensions module_type
let class_field = print_reset_with_maximal_extensions class_field
let class_type_field = print_reset_with_maximal_extensions class_type_field
let class_expr = print_reset_with_maximal_extensions class_expr
let class_type = print_reset_with_maximal_extensions class_type
let class_signature = print_reset_with_maximal_extensions class_signature
let structure_item = print_reset_with_maximal_extensions structure_item
let signature_item = print_reset_with_maximal_extensions signature_item
let binding = print_reset_with_maximal_extensions binding
let payload = print_reset_with_maximal_extensions payload
let type_declaration = print_reset_with_maximal_extensions type_declaration
let jkind = print_reset_with_maximal_extensions jkind
