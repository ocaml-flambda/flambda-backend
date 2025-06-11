module H = Ast_helper
module M = Ast_mapper
open Parsetree
let empty_tuple loc = H.Exp.tuple ~loc []
let empty_record loc = H.Exp.record ~loc [] None
let empty_apply loc f =
  H.Exp.apply ~loc f []

let foo_exp = H.Exp.constant (Pconst_char 'a') (* irrelevant what this
                                                   expression actually is *)
let var ~loc name = H.Pat.var { txt = name; loc }

let mutable_let_rec loc =
  H.Exp.let_ ~loc Asttypes.Mutable Asttypes.Recursive
    [H.Vb.mk (var ~loc "x") foo_exp] foo_exp

let multiple_mutable_let loc =
  H.Exp.let_ ~loc Asttypes.Mutable Asttypes.Nonrecursive
    [H.Vb.mk (var ~loc "x") foo_exp; H.Vb.mk (var ~loc "y") foo_exp] foo_exp

let mutable_let_ppat_tuple loc =
  H.Exp.let_ ~loc Asttypes.Mutable Asttypes.Nonrecursive
    [H.Vb.mk
      (H.Pat.tuple [None, var ~loc "x"; None, var ~loc "y"] Closed) foo_exp]
    foo_exp

let mutable_let_ppat_any loc =
  H.Exp.let_ ~loc Asttypes.Mutable Asttypes.Nonrecursive
    [H.Vb.mk (H.Pat.any ()) foo_exp] foo_exp


let missing_rhs loc =
  let name = Location.mkloc "T" loc in
  let mtd = H.Mtd.mk ~loc name in
  H.Sig.modtype_subst ~loc mtd

let empty_let loc = H.Str.value ~loc Asttypes.Nonrecursive []
let empty_type loc = H.Str.type_ ~loc Asttypes.Nonrecursive []
let functor_id loc = Location.mkloc
    (Longident.( Lapply (Lident "F", Lident "X"))) loc
let complex_record loc =
  H.Pat.record ~loc [functor_id loc, H.Pat.any ~loc () ] Asttypes.Closed

let alias_with_too_many_nones loc =
  H.Typ.alias ~loc (H.Typ.constr ~loc { txt = Lident "unit"; loc } []) None None

let nested_pat_constraint loc =
  let mode s = Location.mkloc (Mode s) loc in
  H.Pat.constraint_
    (H.Pat.constraint_
       (H.Pat.mk Ppat_any)
       None
       [mode "bar"]
    )
    (Some (H.Typ.mk (Ptyp_any None)))
    [mode "foo"]

(* Malformed labeled tuples *)

let lt_empty_open_pat loc =
  let pat = H.Pat.mk Ppat_any in
  H.Pat.tuple ~loc [] Open

let lt_short_closed_pat loc =
  let pat = H.Pat.mk Ppat_any in
  H.Pat.tuple ~loc [Some "baz", pat] Closed

let super = M.default_mapper
let expr mapper e =
  match e.pexp_desc with
  | Pexp_extension ({txt="tuple";loc},_) -> empty_tuple loc
  | Pexp_extension({txt="record";loc},_) -> empty_record loc
  | Pexp_extension({txt="no_args";loc},PStr[{pstr_desc= Pstr_eval (e,_);_}])
    -> empty_apply loc e
  | Pexp_extension ({txt="mutable_let_rec";loc},_) -> mutable_let_rec loc
  | Pexp_extension ({txt="multiple_mutable_let";loc},_) -> multiple_mutable_let loc
  | Pexp_extension ({txt="mutable_let_ppat_tuple";loc},_) ->
    mutable_let_ppat_tuple loc
  | Pexp_extension ({txt="mutable_let_ppat_any";loc},_) ->
    mutable_let_ppat_any loc
  | _ -> super.M.expr mapper e

let typ mapper t =
  match t.ptyp_desc with
  | Ptyp_extension ({txt="alias_with_too_many_nones";loc},_) ->
      alias_with_too_many_nones loc
  | _ -> super.M.typ mapper t

let pat mapper p =
  match p.ppat_desc with
  | Ppat_extension ({txt="record_with_functor_fields";loc},_) ->
      complex_record loc
  | Ppat_extension ({txt="lt_empty_open_pat";loc},_) ->
      lt_empty_open_pat loc
  | Ppat_extension ({txt="lt_short_closed_pat";loc},_) ->
      lt_short_closed_pat loc
  | Ppat_extension ({txt="nested_pat_constraint";loc},_) ->
      nested_pat_constraint loc
  | _ -> super.M.pat mapper p

let structure_item mapper stri = match stri.pstr_desc with
  | Pstr_extension (({Location.txt="empty_let";loc},_),_) -> empty_let loc
  | Pstr_extension (({Location.txt="empty_type";loc},_),_) -> empty_type loc
  | _ -> super.structure_item mapper stri

let signature_item mapper stri = match stri.psig_desc with
  | Psig_extension (({Location.txt="missing_rhs";loc},_),_) -> missing_rhs loc
  | _ -> super.signature_item mapper stri


let () = M.register "illegal ppx" (fun _ ->
    { super with typ; expr; pat; structure_item; signature_item }
  )
