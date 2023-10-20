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
open Ast_helper

let prefix_symbols  = [ '!'; '?'; '~' ] ;;
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

let string_loc ppf x = fprintf ppf "%s" x.txt

(* add parentheses to binders when they are in fact infix or prefix operators *)
let protect_ident ppf txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in fprintf ppf format txt

let protect_longident ppf print_longident longprefix txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%a.%s"
    else if needs_spaces txt then  "%a.(@;%s@;)"
    else "%a.(%s)" in
  fprintf ppf format print_longident longprefix txt

let is_curry_attr attr =
  match attr.attr_name.txt with
  | "extension.curry" -> true
  | _ -> false

let filter_curry_attrs attrs =
  List.filter (fun attr -> not (is_curry_attr attr)) attrs

let has_non_curry_attr attrs =
  List.exists (fun attr -> not (is_curry_attr attr)) attrs

let check_local_attr attrs =
  match
    List.partition (fun attr ->
        attr.attr_name.txt = "extension.local") attrs
  with
  | [], _ -> attrs, false
  | _::_, rest -> rest, true

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
  | `tuple ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ( {txt= Lident "()"; _},_) -> `tuple
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
  | `nil | `tuple | `list _ | `simple _  -> true
  | `cons _ | `normal -> false

let pp = fprintf

type ctxt = {
  pipe : bool;
  semi : bool;
  ifthenelse : bool;
  functionrhs : bool;
}

let reset_ctxt = { pipe=false; semi=false; ifthenelse=false; functionrhs=false}
let under_pipe ctxt = { ctxt with pipe=true }
let under_semi ctxt = { ctxt with semi=true }
let under_ifthenelse ctxt = { ctxt with ifthenelse=true }
let under_functionrhs ctxt = { ctxt with functionrhs=true }
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
  | Lident s -> protect_ident f s
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

let tyvar = Printast.tyvar
let layout_annotation = Jane_syntax.Layouts.Pprint.layout_annotation

let tyvar_layout_loc ~print_quote f (str,layout) =
  let pptv =
    if print_quote
    then tyvar
    else fun ppf s -> Format.fprintf ppf "%s" s
  in
  match layout with
  | None -> pptv f str.txt
  | Some lay -> Format.fprintf f "(%a : %a)" pptv str.txt layout_annotation lay

let tyvar_loc f str = tyvar f str.txt
let string_quot f x = pp f "`%s" x

let non_jane_syntax_expr_attributes expr =
  match Jane_syntax.Expression.of_ast expr with
  | Some (_, attrs) -> attrs
  | None -> expr.pexp_attributes

let maybe_local_type pty ctxt f c =
  let cattrs, is_local = check_local_attr c.ptyp_attributes in
  let c = { c with ptyp_attributes = cattrs } in
  if is_local then
    pp f "local_ %a" (pty ctxt) c
  else
    pty ctxt f c
(* c ['a,'b] *)
let rec class_params_def ctxt f =  function
  | [] -> ()
  | l ->
      pp f "[%a] " (* space *)
        (list (type_param ctxt) ~sep:",") l

and type_with_label ctxt f (label, c) =
  match label with
  | Nolabel    -> maybe_local_type core_type1 ctxt f c (* otherwise parenthesize *)
  | Labelled s -> pp f "%s:%a" s (maybe_local_type core_type1 ctxt) c
  | Optional s -> pp f "?%s:%a" s (maybe_local_type core_type1 ctxt) c

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
    | Ptyp_arrow (l, ct1, ct2) ->
        pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
          (type_with_label ctxt) (l,ct1) (return_type ctxt) ct2
    | Ptyp_alias (ct, s) ->
      pp f "@[<2>%a@;as@;%a@]" (core_type1 ctxt) ct tyvar s
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
            pp f "@[<hov2>%s: %a@ %a@ @]" l.txt
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
    | Ptyp_extension e -> extension ctxt f e
    | _ -> paren true (core_type ctxt) f x

and core_type_jane_syntax ctxt attrs f (x : Jane_syntax.Core_type.t) =
  let filtered_attrs = filter_curry_attrs attrs in
  if filtered_attrs <> [] then begin
    pp f "((%a)%a)" (core_type_jane_syntax ctxt []) x
      (attributes ctxt) filtered_attrs
  end
  else match x with
  | Jtyp_layout (Ltyp_alias { aliased_type; name; layout }) ->
    pp f "@[<2>%a@;as@;(%a :@ %a)@]"
      (core_type1 ctxt) aliased_type
      tyvar_option name
      layout_annotation layout
  | _ -> pp f "@[<2>%a@]" (core_type1_jane_syntax ctxt attrs) x

and core_type1_jane_syntax ctxt attrs f (x : Jane_syntax.Core_type.t) =
  if has_non_curry_attr attrs then core_type_jane_syntax ctxt attrs f x
  else
    match x with
    | Jtyp_layout (Ltyp_var { name; layout }) ->
      pp f "(%a@;:@;%a)" tyvar_option name layout_annotation layout
    | _ -> paren true (core_type_jane_syntax ctxt attrs) f x

and tyvar_option f = function
  | None -> pp f "_"
  | Some name -> tyvar f name

and return_type ctxt f x =
  if x.ptyp_attributes <> [] then maybe_local_type core_type1 ctxt f x
  else maybe_local_type core_type ctxt f x

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
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p protect_ident s.txt
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
        pp f "@[<2>`%s@;%a@]" l (simple_pattern ctxt) p
    | Ppat_construct (({txt=Lident("()"|"[]");_}), _) ->
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
                 (list ~sep:"@ " string_loc) vl
                 (simple_pattern ctxt) x
           | None -> pp f "%a" longident_loc li)
    | _ -> simple_pattern ctxt f x

and simple_pattern ctxt (f:Format.formatter) (x:pattern) : unit =
  if x.ppat_attributes <> [] then pattern ctxt f x
  else match Jane_syntax.Pattern.of_ast x with
    | Some (jpat, attrs) -> pattern_jane_syntax ctxt attrs f jpat
    | None ->
    match x.ppat_desc with
    | Ppat_construct (({txt=Lident ("()"|"[]" as x);_}), None) ->
        pp f  "%s" x
    | Ppat_any -> pp f "_";
    | Ppat_var ({txt = txt;_}) -> protect_ident f txt
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
    | Ppat_constant (c) -> pp f "%a" constant c
    | Ppat_interval (c1, c2) -> pp f "%a..%a" constant c1 constant c2
    | Ppat_variant (l,None) ->  pp f "`%s" l
    | Ppat_constraint (p, ct) ->
        pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt) ct
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
        end
        | None -> match p.ppat_desc with
        | Ppat_array _ | Ppat_record _
        | Ppat_construct (({txt=Lident ("()"|"[]");_}), None) -> false
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

and maybe_local_pat ctxt is_local f p =
  if is_local then
    pp f "(local_ %a)" (simple_pattern ctxt) p
  else
    pp f "%a" (simple_pattern ctxt) p

and label_exp ctxt f (l,opt,p) =
  let pattrs, is_local = check_local_attr p.ppat_attributes in
  let p = { p with ppat_attributes = pattrs } in
  match l with
  | Nolabel ->
      (* single case pattern parens needed here *)
      pp f "%a" (maybe_local_pat ctxt is_local) p
  | Optional rest ->
      begin match p with
      | {ppat_desc = Ppat_var {txt;_}; ppat_attributes = []}
        when txt = rest && not is_local ->
          (match opt with
           | Some o -> pp f "?(%s=@;%a)" rest  (expression ctxt) o
           | None -> pp f "?%s" rest)
      | _ ->
          (match opt with
           | Some o ->
               pp f "?%s:(%s%a=@;%a)"
                 rest
                 (if is_local then "local_ " else "")
                 (pattern1 ctxt) p (expression ctxt) o
           | None -> pp f "?%s:%a" rest (maybe_local_pat ctxt is_local) p)
      end
  | Labelled l -> match p with
    | {ppat_desc  = Ppat_var {txt;_}; ppat_attributes = []}
      when txt = l ->
        if is_local then
          pp f "~(local_ %s)" l
        else
          pp f "~%s" l
    | _ ->  pp f "~%s:%a" l (maybe_local_pat ctxt is_local) p

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
    | Pexp_function _ | Pexp_fun _ | Pexp_match _ | Pexp_try _ | Pexp_sequence _
    | Pexp_newtype _
      when ctxt.pipe || ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_ifthenelse _ | Pexp_sequence _ when ctxt.ifthenelse ->
        paren true (expression reset_ctxt) f x
    | Pexp_let _ | Pexp_letmodule _ | Pexp_open _
      | Pexp_letexception _ | Pexp_letop _
        when ctxt.semi ->
        paren true (expression reset_ctxt) f x
    | Pexp_fun (l, e0, p, e) ->
        pp f "@[<2>fun@;%a@;%a@]"
          (label_exp ctxt) (l, e0, p)
          (pp_print_pexp_function ctxt "->") e
    | Pexp_newtype (lid, e) ->
        pp f "@[<2>fun@;(type@;%s)@;%a@]" lid.txt
          (pp_print_pexp_function ctxt "->") e
    | Pexp_function l ->
        pp f "@[<hv>function%a@]" (case_list ctxt) l
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
      ({ pexp_desc = Pexp_extension({txt = "extension.local"}, PStr []) },
       [Nolabel, sbody]) ->
        pp f "@[<2>local_ %a@]" (expression ctxt) sbody
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
        pp f "@[<hov2>%s@ <-@ %a@]" s.txt (expression ctxt) e
    | Pexp_override l -> (* FIXME *)
        let string_x_expression f (s, e) =
          pp f "@[<hov2>%s@ =@ %a@]" s.txt (expression ctxt) e in
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
        pp f "@[<2>`%s@;%a@]" l (simple_expr ctxt) eo
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
    | Pexp_send (e, s) -> pp f "@[<hov2>%a#%s@]" (simple_expr ctxt) e s.txt

    | _ -> simple_expr ctxt f x

and simple_expr ctxt f x =
  if x.pexp_attributes <> [] then expression ctxt f x ~jane_syntax_parens:true
  else match x.pexp_desc with
    | Pexp_construct _  when is_simple_construct (view_expr x) ->
        (match view_expr x with
         | `nil -> pp f "[]"
         | `tuple -> pp f "()"
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
    | Pexp_constraint (e, ct) ->
        pp f "(%a : %a)" (expression ctxt) e (core_type ctxt) ct
    | Pexp_coerce (e, cto1, ct) ->
        pp f "(%a%a :> %a)" (expression ctxt) e
          (option (core_type ctxt) ~first:" : " ~last:" ") cto1 (* no sep hint*)
          (core_type ctxt) ct
    | Pexp_variant (l, None) -> pp f "`%s" l
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
  pp f "@[<hov2>%a%a@]" (core_type ctxt) x.pval_type
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
      pp f "@[<2>val @ %a%a%s@ :@ %a@]%a"
        mutable_flag mf virtual_flag vf s.txt (core_type ctxt) ct
        (item_attributes ctxt) x.pctf_attributes
  | Pctf_method (s, pf, vf, ct) ->
      pp f "@[<2>method %a %a%s :@;%a@]%a"
        private_flag pf virtual_flag vf s.txt (core_type ctxt) ct
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
        (type_with_label ctxt) (l,co)
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
    pp f "@[<2>%s %a%a%s@ =@ %a@]%a" kwd
      virtual_flag x.pci_virt
      (class_params_def ctxt) ls txt
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
           | Some (s) -> pp f "@ as %s" s.txt ) so
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
      pp f "@[<2>val%s %a%s =@;%a@]%a" (override ovf)
        mutable_flag mf s.txt
        (expression ctxt) e
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_method (s, pf, Cfk_virtual ct) ->
      pp f "@[<2>method virtual %a %s :@;%a@]%a"
        private_flag pf s.txt
        (core_type ctxt) ct
        (item_attributes ctxt) x.pcf_attributes
  | Pcf_val (s, mf, Cfk_virtual ct) ->
      pp f "@[<2>val virtual %a%s :@ %a@]%a"
        mutable_flag mf s.txt
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
           pvb_attributes=[];
           pvb_loc=Location.none;
          }
      in
      pp f "@[<2>method%s %a%a@]%a"
        (override ovf)
        private_flag pf
        (fun f -> function
           | {pexp_desc=Pexp_poly (e, Some ct); pexp_attributes=[]; _} ->
               pp f "%s :@;%a=@;%a"
                 s.txt (core_type ctxt) ct (expression ctxt) e
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
                   functor_:bool ->
                   contents:(ctxt -> formatter -> 'a -> unit) ->
                   'a include_infos ->
                   unit =
  fun ctxt f ~functor_ ~contents incl ->
    pp f "@[<hov2>include%t@ %a@]%a"
      (if functor_ then fun f -> pp f "@ functor" else fun _ -> ())
      (contents ctxt) incl.pincl_mod
      (item_attributes ctxt) incl.pincl_attributes

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
        pp f "@[<hov2>functor () ->@ %a@]" (module_type ctxt) mt2
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
      let ls = List.map fst ls in
      pp f "type@ %a %a =@ %a"
        (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
        ls longident_loc li (type_declaration ctxt) td
  | Pwith_module (li, li2) ->
      pp f "module %a =@ %a" longident_loc li longident_loc li2;
  | Pwith_modtype (li, mty) ->
      pp f "module type %a =@ %a" longident_loc li (module_type ctxt) mty;
  | Pwith_typesubst (li, ({ptype_params=ls;_} as td)) ->
      let ls = List.map fst ls in
      pp f "type@ %a %a :=@ %a"
        (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
        ls longident_loc li
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

and sig_include_functor ctxt f
  : Jane_syntax.Include_functor.signature_item -> _ = function
  | Ifsig_include_functor incl ->
      include_ ctxt f ~functor_:true ~contents:module_type incl

and signature_item_jane_syntax ctxt f : Jane_syntax.Signature_item.t -> _ =
  function
  | Jsig_include_functor ifincl -> sig_include_functor ctxt f ifincl

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
        protect_ident vd.pval_name.txt
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Psig_typext te ->
      type_extension ctxt f te
  | Psig_exception ed ->
      exception_declaration ctxt f ed
  | Psig_class l ->
      let class_description kwd f ({pci_params=ls;pci_name={txt;_};_} as x) =
        pp f "@[<2>%s %a%a%s@;:@;%a@]%a" kwd
          virtual_flag x.pci_virt
          (class_params_def ctxt) ls txt
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
  | Psig_include incl ->
      include_ ctxt f ~functor_:false ~contents:module_type incl
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
  else match Jane_syntax.Module_expr.of_ast x with
    | Some ext -> extension_module_expr ctxt f ext
    | None -> match x.pmod_desc with
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

and pp_print_pexp_function ctxt sep f x =
  (* do not print [@extension.local] on expressions *)
  let attrs, _ = check_local_attr x.pexp_attributes in
  let x = { x with pexp_attributes = attrs } in
  (* We go to some trouble to print nested [Pexp_newtype]/[Lexp_newtype] as
     newtype parameters of the same "fun" (rather than printing several nested
     "fun (type a) -> ..."). This isn't necessary for round-tripping -- it just
     makes the pretty-printing a bit prettier. *)
  match Jane_syntax.Expression.of_ast x with
  | Some (Jexp_n_ary_function (params, c, body), []) ->
      function_params_then_body ctxt f params c body ~delimiter:sep
  | Some (Jexp_layout (Lexp_newtype (str, lay, e)), []) ->
      pp f "@[(type@ %s :@ %a)@]@ %a"
        str.txt
        layout_annotation lay
        (pp_print_pexp_function ctxt sep) e
  | Some (jst, attrs) ->
      pp f "%s@;%a" sep (jane_syntax_expr ctxt attrs ~parens:false) jst
  | None ->
  if x.pexp_attributes <> [] then pp f "%s@;%a" sep (expression ctxt) x
  else
    match x.pexp_desc with
    | Pexp_newtype (str,e) ->
      pp f "(type@ %s)@ %a" str.txt (pp_print_pexp_function ctxt sep) e
    | Pexp_fun (a, b, c, body) ->
      pp f "%a@;%a"
        (label_exp ctxt) (a, b, c)
        (pp_print_pexp_function ctxt sep) body
    | _ ->
       pp f "%s@;%a" sep (expression ctxt) x

(* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
and binding ctxt f {pvb_pat=p; pvb_expr=x; _} =
  (* .pvb_attributes have already been printed by the caller, #bindings *)
  let tyvars_str tyvars = List.map (fun v -> v.txt) tyvars in
  let is_desugared_gadt p e =
    let gadt_pattern =
      match p with
      | {ppat_desc=Ppat_constraint({ppat_desc=Ppat_var _} as pat,
                                   {ptyp_desc=Ptyp_poly (args_tyvars, rt)});
         ppat_attributes=[]}->
          Some (pat, args_tyvars, rt)
      | _ -> None in
    let rec gadt_exp tyvars e =
      match e with
      (* no need to handle layout annotations here; the extracted variables
         don't get printed -- they're just used to decide how to print *)
      | {pexp_desc=Pexp_newtype (tyvar, e); pexp_attributes=[]} ->
          gadt_exp (tyvar :: tyvars) e
      | {pexp_desc=Pexp_constraint (e, ct); pexp_attributes=[]} ->
          Some (List.rev tyvars, e, ct)
      | _ -> None in
    let gadt_exp = gadt_exp [] e in
    match gadt_pattern, gadt_exp with
    | Some (p, pt_tyvars, pt_ct), Some (e_tyvars, e, e_ct)
      when tyvars_str pt_tyvars = tyvars_str e_tyvars ->
      let ety = Typ.varify_constructors e_tyvars e_ct in
      if ety = pt_ct then
      Some (p, pt_tyvars, e_ct, e) else None
    | _ -> None in
  if non_jane_syntax_expr_attributes x <> []
  then
    match p with
    | {ppat_desc=Ppat_constraint({ppat_desc=Ppat_var _; _} as pat,
                                 ({ptyp_desc=Ptyp_poly _; _} as typ));
       ppat_attributes=[]; _} ->
        pp f "%a@;: %a@;=@;%a"
          (simple_pattern ctxt) pat (core_type ctxt) typ (expression ctxt) x
    | _ ->
        pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
  else
  match is_desugared_gadt p x with
  | Some (p, [], ct, e) ->
      pp f "%a@;: %a@;=@;%a"
        (simple_pattern ctxt) p (core_type ctxt) ct (expression ctxt) e
  | Some (p, tyvars, ct, e) -> begin
    pp f "%a@;: type@;%a.@;%a@;=@;%a"
    (simple_pattern ctxt) p (list pp_print_string ~sep:"@;")
    (tyvars_str tyvars) (core_type ctxt) ct (expression ctxt) e
    end
  | None -> begin
      match p with
      | {ppat_desc=Ppat_constraint(p ,ty);
         ppat_attributes=[]} -> (* special case for the first*)
          begin match ty with
          | {ptyp_desc=Ptyp_poly _; ptyp_attributes=[]} ->
              pp f "%a@;:@;%a@;=@;%a" (simple_pattern ctxt) p
                (core_type ctxt) ty (expression ctxt) x
          | _ ->
              pp f "(%a@;:@;%a)@;=@;%a" (simple_pattern ctxt) p
                (core_type ctxt) ty (expression ctxt) x
          end
      | {ppat_desc=Ppat_var _; ppat_attributes=[]} ->
          pp f "%a@ %a" (simple_pattern ctxt) p
            (pp_print_pexp_function ctxt "=") x
      | _ ->
          pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x
    end

(* [in] is not printed *)
and bindings ctxt f (rf,l) =
  let binding kwd rf f x =
    let x, is_local =
      match x.pvb_expr.pexp_desc with
      | Pexp_apply
        ({ pexp_desc = Pexp_extension({txt = "extension.local"}, PStr []) },
         [Nolabel, sbody]) ->
         let sattrs, _ = check_local_attr sbody.pexp_attributes in
         let sbody = {sbody with pexp_attributes = sattrs} in
         let pattrs, _ = check_local_attr x.pvb_pat.ppat_attributes in
         let pat = {x.pvb_pat with ppat_attributes = pattrs} in
         {x with pvb_pat = pat; pvb_expr = sbody}, "local_ "
      | _ -> x, ""
    in
    pp f "@[<2>%s %a%s%a@]%a" kwd rec_flag rf is_local
      (binding ctxt) x (item_attributes ctxt) x.pvb_attributes
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

and str_include_functor ctxt f
  : Jane_syntax.Include_functor.structure_item -> _ = function
  | Ifstr_include_functor incl ->
      include_ ctxt f ~functor_:true ~contents:module_expr incl

and structure_item_jane_syntax ctxt f : Jane_syntax.Structure_item.t -> _ =
  function
  | Jstr_include_functor ifincl -> str_include_functor ctxt f ifincl

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
        pp f "@[<2>%s %a%a%s %a%a=@;%a@]%a" kwd
          virtual_flag x.pci_virt
          (class_params_def ctxt) ls txt
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
        protect_ident vd.pval_name.txt
        (value_description ctxt) vd
        (item_attributes ctxt) vd.pval_attributes
  | Pstr_include incl ->
      include_ ctxt f ~functor_:false ~contents:module_expr incl
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
    pp f "@[<2>%s %a%a%s%s%a@]%a" kwd
      nonrec_flag rf
      (type_params ctxt) x.ptype_params
      x.ptype_name.txt eq
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
  let has_attr pld name =
    List.exists (fun attr -> attr.attr_name.txt = name) pld.pld_attributes
  in
  let field_flag f pld =
    pp f "%a" mutable_flag pld.pld_mutable;
    if has_attr pld "extension.global" then pp f "global_ "
  in
  let type_record_field f pld =
    let pld_attributes =
      List.filter (fun attr ->
        match attr.attr_name.txt with
        | "extension.global" -> false
        | _ -> true) pld.pld_attributes
    in
    pp f "@[<2>%a%s:@;%a@;%a@]"
      field_flag pld
      pld.pld_name.txt
      (core_type ctxt) pld.pld_type
      (attributes ctxt) pld_attributes
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
    let vars_layouts, attrs =
      match Jane_syntax.Layouts.of_constructor_declaration pcd with
      | None -> List.map (fun v -> v, None) pcd.pcd_vars, pcd.pcd_attributes
      | Some stuff -> stuff
    in
    constructor_declaration ctxt f
      (pcd.pcd_name.txt, vars_layouts, pcd.pcd_args, pcd.pcd_res, attrs)
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

and constructor_declaration ctxt f (name, vars_layouts, args, res, attrs) =
  let name =
    match name with
    | "::" -> "(::)"
    | s -> s in
  let pp_vars f vls =
    match vls with
    | [] -> ()
    | _  -> pp f "%a@;.@;" (list (tyvar_layout_loc ~print_quote:true) ~sep:"@;")
                           vls
  in
  match res with
  | None ->
      pp f "%s%a@;%a" name
        (fun f -> function
           | Pcstr_tuple [] -> ()
           | Pcstr_tuple l ->
             pp f "@;of@;%a" (list (core_type1 ctxt) ~sep:"@;*@;") l
           | Pcstr_record l -> pp f "@;of@;%a" (record_declaration ctxt) l
        ) args
        (attributes ctxt) attrs
  | Some r ->
      pp f "%s:@;%a%a@;%a" name
        pp_vars vars_layouts
        (fun f -> function
           | Pcstr_tuple [] -> core_type1 ctxt f r
           | Pcstr_tuple l -> pp f "%a@;->@;%a"
                                (list (core_type1 ctxt) ~sep:"@;*@;") l
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
        pp f "?%s" str
      else
        pp f "?%s:%a" str (simple_expr ctxt) e
  | Labelled lbl ->
      if Some lbl = simple_name then
        pp f "~%s" lbl
      else
        pp f "~%s:%a" lbl (simple_expr ctxt) e

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
  | Jexp_n_ary_function x   ->
      if parens then pp f "(%a)" (n_ary_function_expr reset_ctxt) x
      else n_ary_function_expr ctxt f x

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
  | Lexp_newtype (lid, layout, inner_expr) ->
    pp f "@[<2>fun@;(type@;%s :@;%a)@;%a@]"
      lid.txt
      layout_annotation layout
      (pp_print_pexp_function ctxt "->") inner_expr

and unboxed_constant _ctxt f (x : Jane_syntax.Layouts.constant)
  =
  match x with
  | Float (x, suffix) -> pp f "#%a" constant (Pconst_float (x, suffix))
  | Integer (x, suffix) -> pp f "#%a" constant (Pconst_integer (x, Some suffix))

and function_param ctxt f
    ({ pparam_desc; pparam_loc = _ } :
       Jane_syntax.N_ary_functions.function_param)
  =
  match pparam_desc with
  | Pparam_val (a, b, c) -> label_exp ctxt f (a, b, c)
  | Pparam_newtype (ty, None) -> pp f "(type %s)" ty.txt
  | Pparam_newtype (ty, Some annot) ->
      pp f "(type %s : %a)" ty.txt layout_annotation annot

and function_body ctxt f (x : Jane_syntax.N_ary_functions.function_body) =
  match x with
  | Pfunction_body body -> expression ctxt f body
  | Pfunction_cases (cases, _, attrs) ->
    pp f "@[<hv>function%a%a@]"
      (item_attributes ctxt) attrs
      (case_list ctxt) cases

and function_constraint
    ctxt f (x : Jane_syntax.N_ary_functions.function_constraint)
  =
  (* We don't currently print [x.alloc_mode]; this would need
     to go on the enclosing [let] binding.
  *)
  (* Enable warning 9 to ensure that the record pattern doesn't miss any field.
  *)
  match[@ocaml.warning "+9"] x with
  | { type_constraint = Pconstraint ty; mode_annotations = _ } ->
    pp f ":@;%a" (core_type ctxt) ty
  | { type_constraint = Pcoerce (ty1, ty2); mode_annotations = _ } ->
    pp f "%a:>@;%a"
      (option ~first:":@;" (core_type ctxt)) ty1
      (core_type ctxt) ty2

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

and n_ary_function_expr
      ctxt
      f
      ((params, constraint_, body) as x : Jane_syntax.N_ary_functions.expression)
  =
  if ctxt.pipe || ctxt.semi then
    paren true (n_ary_function_expr reset_ctxt) f x
  else
    match params, constraint_ with
    (* Omit [fun] if there are no params. *)
    | [], None ->
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

and extension_module_expr ctxt f (x : Jane_syntax.Module_expr.t) =
  match x with
  | Emod_instance i -> instance_module_expr ctxt f i

and instance_module_expr ctxt f (x : Jane_syntax.Instances.module_expr) =
  match x with
  | Imod_instance i -> instance ctxt f i

and instance ctxt f (x : Jane_syntax.Instances.instance) =
  match x with
  | { head; args = [] } -> pp f "%s" head
  | { head; args } ->
    pp f "@[<2>%s %a@]" head (list (instance_arg ctxt)) args

and instance_arg ctxt f (param, value) =
  pp f "@[<1>(%a)@;(%a)@]" (instance ctxt) param (instance ctxt) value

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

