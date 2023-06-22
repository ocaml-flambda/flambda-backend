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

(* Representation of types and declarations *)

open Asttypes
open Layouts

(* Type expressions for the core language *)

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_expr = transient_expr

and type_desc =
  | Tvar of { name : string option; layout : layout }
  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  | Ttuple of (string option * type_expr) list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of { name : string option; layout : layout }
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list

and arrow_desc =
  arg_label * alloc_mode * alloc_mode

and alloc_mode_const = Global | Local

and alloc_mode_var = {
  mutable upper: alloc_mode_const;
  mutable lower: alloc_mode_const;
  mutable vlower: alloc_mode_var list;
  mutable mark: bool;
  mvid: int;
}

and alloc_mode =
  | Amode of alloc_mode_const
  | Amodevar of alloc_mode_var

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr | Fixed_private | Reified of Path.t | Rigid
and row_field = [`some] row_field_gen
and _ row_field_gen =
    RFpresent : type_expr option -> [> `some] row_field_gen
  | RFeither :
      { no_arg: bool;
        arg_type: type_expr list;
        matched: bool;
        ext: [`some | `none] row_field_gen ref} -> [> `some] row_field_gen
  | RFabsent : [> `some] row_field_gen
  | RFnone : [> `none] row_field_gen

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and any = [`some | `none | `var]
and field_kind = [`some|`var] field_kind_gen
and _ field_kind_gen =
    FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen
  | FKprivate : [> `none] field_kind_gen  (* private method; only under FKvar *)
  | FKpublic  : [> `some] field_kind_gen  (* public method *)
  | FKabsent  : [> `some] field_kind_gen  (* hidden private method *)

and commutable = [`some|`var] commutable_gen
and _ commutable_gen =
    Cok      : [> `some] commutable_gen
  | Cunknown : [> `none] commutable_gen
  | Cvar : {mutable commu: any commutable_gen} -> [> `var] commutable_gen

module TransientTypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* *)

module Uid = Shape.Uid

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set

module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map


(* Value descriptions *)

type value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of
      class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)

and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref

and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (method_privacy * virtual_flag * type_expr) Meths.t; }

and method_privacy =
  | Mpublic
  | Mprivate of field_kind

(* Variance *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16
    | Neg -> 32
    | Inv -> 64
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x b v =
    if b then v lor single x else  v land (lnot (single x))
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = 127
  let covariant = single May_pos lor single Pos lor single Inj
  let swap f1 f2 v =
    let v' = set f1 (mem f2 v) v in set f2 (mem f1 v) v'
  let conjugate v = swap May_pos May_neg (swap Pos Neg v)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj true unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  type signature = t list
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_layout: layout;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_unboxed_default: bool;
    type_uid: Uid.t;
 }

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
    Type_abstract
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and tag = Ordinary of {src_index: int;     (* Unique name (per type) *)
                       runtime_tag: int}   (* The runtime tag *)
        | Extension of Path.t * layout array

and record_representation =
  | Record_unboxed
  | Record_inlined of tag * variant_representation
  | Record_boxed of layout array
  | Record_float

and variant_representation =
  | Variant_unboxed
  | Variant_boxed of layout array array
  | Variant_extensible

and global_flag =
  | Global
  | Nonlocal
  | Unrestricted

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_global: global_flag;
    ld_type: type_expr;
    ld_layout : layout;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_arguments =
  | Cstr_tuple of (type_expr * global_flag) list
  | Cstr_record of label_declaration list

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_arg_layouts: layout array;
    ext_constant: bool;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
 }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type rec_status =
  Trec_not                   (* first in a nonrecursive group *)
| Trec_first                 (* first in a recursive group *)
| Trec_next                  (* not first in a recursive/nonrecursive group *)

type ext_status =
  Text_first                     (* first constructor of an extension *)
| Text_next                      (* not first constructor of an extension *)
| Text_exception                 (* an exception *)

type module_presence =
  | Mp_present
  | Mp_absent

module type Wrap = sig
  type 'a t
end

module type Wrapped = sig
  type 'a wrapped

  type value_description =
    { val_type: type_expr wrapped;                (* Type of the value *)
      val_kind: value_kind;
      val_loc: Location.t;
      val_attributes: Parsetree.attributes;
      val_uid: Uid.t;
    }

  type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

  and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

  and signature = signature_item list wrapped

  and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

  and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

  and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }
end

module Make_wrapped(Wrap : Wrap) = struct
  (* Avoid repeating everything in Wrapped *)
  module rec M : Wrapped with type 'a wrapped = 'a Wrap.t = M
  include M
end

module Map_wrapped(From : Wrapped)(To : Wrapped) = struct
  open From
  type mapper =
    {
      map_signature: mapper -> signature -> To.signature;
      map_type_expr: mapper -> type_expr wrapped -> type_expr To.wrapped
    }

  let signature m = m.map_signature m

  let rec module_type m = function
    | Mty_ident p -> To.Mty_ident p
    | Mty_alias p -> To.Mty_alias p
    | Mty_functor (parm,mty) ->
        To.Mty_functor (functor_parameter m parm, module_type m mty)
    | Mty_signature sg -> To.Mty_signature (signature m sg)

  and functor_parameter m = function
      | Unit -> To.Unit
      | Named (id,mty) -> To.Named (id, module_type m mty)

  let value_description m {val_type; val_kind; val_attributes; val_loc; val_uid} =
    To.{
      val_type = m.map_type_expr m val_type;
      val_kind;
      val_attributes;
      val_loc;
      val_uid
    }

  let module_declaration m {md_type; md_attributes; md_loc; md_uid} =
    To.{
      md_type = module_type m md_type;
      md_attributes;
      md_loc;
      md_uid;
    }

  let modtype_declaration m {mtd_type; mtd_attributes; mtd_loc; mtd_uid} =
    To.{
      mtd_type = Option.map (module_type m) mtd_type;
      mtd_attributes;
      mtd_loc;
      mtd_uid;
    }

  let signature_item m = function
    | Sig_value (id,vd,vis) ->
        To.Sig_value (id, value_description m vd, vis)
    | Sig_type (id,td,rs,vis) ->
        To.Sig_type (id,td,rs,vis)
    | Sig_module (id,pres,md,rs,vis) ->
        To.Sig_module (id, pres, module_declaration m md, rs, vis)
    | Sig_modtype (id,mtd,vis) ->
        To.Sig_modtype (id, modtype_declaration m mtd, vis)
    | Sig_typext (id,ec,es,vis) ->
        To.Sig_typext (id,ec,es,vis)
    | Sig_class (id,cd,rs,vis) ->
        To.Sig_class (id,cd,rs,vis)
    | Sig_class_type (id,ctd,rs,vis) ->
        To.Sig_class_type (id,ctd,rs,vis)
end

include Make_wrapped(struct type 'a t = 'a end)

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: (type_expr * global_flag) list;          (* Type of the arguments *)
    cstr_arg_layouts: layout array;     (* Layouts of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: tag;                      (* Tag for heap blocks *)
    cstr_repr: variant_representation;  (* Repr of the outer variant *)
    cstr_constant: bool;                (* True if all args are void *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

let equal_tag t1 t2 =
  match (t1, t2) with
  | Ordinary {src_index=i1}, Ordinary {src_index=i2} ->
    i2 = i1 (* If i1 = i2, the runtime_tags will also be equal *)
  | Extension (path1,_), Extension (path2,_) -> Path.same path1 path2
  | (Ordinary _ | Extension _), _ -> false

let equal_variant_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Variant_unboxed, Variant_unboxed ->
      true
  | Variant_boxed lays1, Variant_boxed lays2 ->
      Misc.Stdlib.Array.equal (Misc.Stdlib.Array.equal Layout.equal) lays1 lays2
  | Variant_extensible, Variant_extensible ->
      true
  | (Variant_unboxed | Variant_boxed _ | Variant_extensible), _ ->
      false

let equal_record_representation r1 r2 = match r1, r2 with
  | Record_unboxed, Record_unboxed ->
      true
  | Record_inlined (tag1, vr1), Record_inlined (tag2, vr2) ->
      equal_tag tag1 tag2 && equal_variant_representation vr1 vr2
  | Record_boxed lays1, Record_boxed lays2 ->
      Misc.Stdlib.Array.equal Layout.equal lays1 lays2
  | Record_float, Record_float ->
      true
  | (Record_unboxed | Record_inlined _ | Record_boxed _ | Record_float), _ ->
      false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Extension _, Extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

let decl_is_abstract decl =
  match decl.type_kind with
  | Type_abstract -> true
  | Type_record _ | Type_variant _ | Type_open -> false

let find_unboxed_type decl =
  match decl.type_kind with
    Type_record ([{ld_type = arg; _}], Record_unboxed)
  | Type_record ([{ld_type = arg; _}], Record_inlined (_, Variant_unboxed))
  | Type_variant ([{cd_args = Cstr_tuple [arg,_]; _}], Variant_unboxed)
  | Type_variant ([{cd_args = Cstr_record [{ld_type = arg; _}]; _}],
                  Variant_unboxed) ->
    Some arg
  | Type_record (_, ( Record_inlined _ | Record_unboxed
                    | Record_boxed _ | Record_float ))
  | Type_variant (_, ( Variant_boxed _ | Variant_unboxed
                     | Variant_extensible ))
  | Type_abstract | Type_open ->
    None

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_global: global_flag;        (* Is this a global field? *)
    lbl_layout : layout;                (* Layout of the argument *)
    lbl_pos: int;                       (* Position in block *)
    lbl_num: int;                       (* Position in type *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for outer record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

let lbl_pos_void = -1

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, Mp_present, _, _, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_class(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

let signature_item_id = function
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _)
    -> id

type value_mode =
  { r_as_l : alloc_mode;
    r_as_g : alloc_mode; }

(**** Definitions for backtracking ****)

type change =
    Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cscope of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of [`none|`some] row_field_gen ref
  | Ckind of [`var] field_kind_gen
  | Ccommu of [`var] commutable_gen
  | Cuniv of type_expr option ref * type_expr option
  | Cmode_upper of alloc_mode_var * alloc_mode_const
  | Cmode_lower of alloc_mode_var * alloc_mode_const
  | Cmode_vlower of alloc_mode_var * alloc_mode_var list

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

let log_changes chead ctail =
  if chead = Unchanged then (assert (!ctail = Unchanged))
  else begin
    !trail := chead;
    trail := ctail
  end

let append_change ctail ch =
  assert (!(!ctail) = Unchanged);
  let r' = ref Unchanged in
  (!ctail) := Change (ch, r');
  ctail := r'

(* constructor and accessors for [field_kind] *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent

let rec field_kind_internal_repr : field_kind -> field_kind = function
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as fk} ->
      field_kind_internal_repr fk
  | kind -> kind

let field_kind_repr fk =
  match field_kind_internal_repr fk with
  | FKvar _ -> Fprivate
  | FKpublic -> Fpublic
  | FKabsent -> Fabsent

let field_public = FKpublic
let field_absent = FKabsent
let field_private () = FKvar {field_kind=FKprivate}

(* Constructor and accessors for [commutable] *)

let rec is_commu_ok : type a. a commutable_gen -> bool = function
  | Cvar {commu} -> is_commu_ok commu
  | Cunknown -> false
  | Cok -> true

let commu_ok = Cok
let commu_var () = Cvar {commu=Cunknown}

(**** Representative of a type ****)

let rec repr_link (t : type_expr) d : type_expr -> type_expr =
 function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' ->
     log_change (Ccompress (t, t.desc, d));
     t.desc <- d;
     t'

let repr_link1 t = function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' -> t'

let repr t =
  match t.desc with
   Tlink t' ->
     repr_link1 t t'
 | Tfield (_, k, _, t') when field_kind_internal_repr k = FKabsent ->
     repr_link1 t t'
 | _ -> t

(* getters for type_expr *)

let get_desc t = (repr t).desc
let get_level t = (repr t).level
let get_scope t = (repr t).scope
let get_id t = (repr t).id

(* transient type_expr *)

module Transient_expr = struct
  let create desc ~level ~scope ~id = {desc; level; scope; id}
  let set_desc ty d = ty.desc <- d
  let set_stub_desc ty d =
    (match ty.desc with
    | Tvar {name = None; _} -> ()
    | _ -> assert false);
    ty.desc <- d
  let set_level ty lv = ty.level <- lv
  let set_scope ty sc = ty.scope <- sc
  let set_var_layout ty layout' =
    match ty.desc with
    | Tvar { name; _ } ->
      set_desc ty (Tvar { name; layout = layout' })
    | _ -> assert false
  let coerce ty = ty
  let repr = repr
  let type_expr ty = ty
end

(* Comparison for [type_expr]; cannot be used for functors *)

let eq_type t1 t2 = t1 == t2 || repr t1 == repr t2
let compare_type t1 t2 = compare (get_id t1) (get_id t2)

(* Constructor and accessors for [row_desc] *)

let create_row ~fields ~more ~closed ~fixed ~name =
    { row_fields=fields; row_more=more;
      row_closed=closed; row_fixed=fixed; row_name=name }

(* [row_fields] subsumes the original [row_repr] *)
let rec row_fields row =
  match get_desc row.row_more with
  | Tvariant row' ->
      row.row_fields @ row_fields row'
  | _ ->
      row.row_fields

let rec row_repr_no_fields row =
  match get_desc row.row_more with
  | Tvariant row' -> row_repr_no_fields row'
  | _ -> row

let row_more row = (row_repr_no_fields row).row_more
let row_closed row = (row_repr_no_fields row).row_closed
let row_fixed row = (row_repr_no_fields row).row_fixed
let row_name row = (row_repr_no_fields row).row_name

let rec get_row_field tag row =
  let rec find = function
    | (tag',f) :: fields ->
        if tag = tag' then f else find fields
    | [] ->
        match get_desc row.row_more with
        | Tvariant row' -> get_row_field tag row'
        | _ -> RFabsent
  in find row.row_fields

let set_row_name row row_name =
  let row_fields = row_fields row in
  let row = row_repr_no_fields row in
  {row with row_fields; row_name}

type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:type_expr;
             closed:bool;
             fixed:fixed_explanation option;
             name:(Path.t * type_expr list) option }

let row_repr row =
  let fields = row_fields row in
  let row = row_repr_no_fields row in
  Row { fields;
        more = row.row_more;
        closed = row.row_closed;
        fixed = row.row_fixed;
        name = row.row_name }

type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

let rec row_field_repr_aux tl : row_field -> row_field = function
  | RFeither ({ext = {contents = RFnone}} as r) ->
      RFeither {r with arg_type = tl@r.arg_type}
  | RFeither {arg_type;
              ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_repr_aux (tl@arg_type) rf
  | RFpresent (Some _) when tl <> [] ->
      RFpresent (Some (List.hd tl))
  | RFpresent _ as rf -> rf
  | RFabsent -> RFabsent

let row_field_repr fi =
  match row_field_repr_aux [] fi with
  | RFeither {no_arg; arg_type; matched} -> Reither (no_arg, arg_type, matched)
  | RFpresent t -> Rpresent t
  | RFabsent -> Rabsent

let rec row_field_ext (fi : row_field) =
  match fi with
  | RFeither {ext = {contents = RFnone} as ext} -> ext
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_ext rf
  | _ -> Misc.fatal_error "Types.row_field_ext "

let rf_present oty = RFpresent oty
let rf_absent = RFabsent
let rf_either ?use_ext_of ~no_arg arg_type ~matched =
  let ext =
    match use_ext_of with
      Some rf -> row_field_ext rf
    | None -> ref RFnone
  in
  RFeither {no_arg; arg_type; matched; ext}

let rf_either_of = function
  | None ->
      RFeither {no_arg=true; arg_type=[]; matched=false; ext=ref RFnone}
  | Some ty ->
      RFeither {no_arg=false; arg_type=[ty]; matched=false; ext=ref RFnone}

let eq_row_field_ext rf1 rf2 =
  row_field_ext rf1 == row_field_ext rf2

let changed_row_field_exts l f =
  let exts = List.map row_field_ext l in
  f ();
  List.exists (fun r -> !r <> RFnone) exts

let match_row_field ~present ~absent ~either (f : row_field) =
  match f with
  | RFabsent -> absent ()
  | RFpresent t -> present t
  | RFeither {no_arg; arg_type; matched; ext} ->
      let e : row_field option =
        match !ext with
        | RFnone -> None
        | RFeither _ | RFpresent _ | RFabsent as e -> Some e
      in
      either no_arg arg_type matched e


(**** Some type creators ****)

let new_id = Local_store.s_ref (-1)

let create_expr = Transient_expr.create

let newty3 ~level ~scope desc  =
  incr new_id;
  create_expr desc ~level ~scope ~id:!new_id

let newty2 ~level desc =
  newty3 ~level ~scope:Ident.lowest_scope desc

                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> Transient_expr.set_desc ty desc
  | Ccompress (ty, desc, _) -> Transient_expr.set_desc ty desc
  | Clevel (ty, level) -> Transient_expr.set_level ty level
  | Cscope (ty, scope) -> Transient_expr.set_scope ty scope
  | Cname  (r, v)    -> r := v
  | Crow   r         -> r := RFnone
  | Ckind  (FKvar r) -> r.field_kind <- FKprivate
  | Ccommu (Cvar r)  -> r.commu <- Cunknown
  | Cuniv  (r, v)    -> r := v
  | Cmode_upper (v, u) -> v.upper <- u
  | Cmode_lower (v, l) -> v.lower <- l
  | Cmode_vlower (v, vs) -> v.vlower <- vs

type snapshot = changes ref * int
let last_snapshot = Local_store.s_ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  let ty = repr ty in
  let ty' = repr ty' in
  if ty == ty' then () else begin
  log_type ty;
  let desc = ty.desc in
  Transient_expr.set_desc ty (Tlink ty');
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar { name }, Tvar { name = name'; layout = layout' } ->
      begin match name, name' with
      | Some _, None ->
        log_type ty';
        Transient_expr.set_desc ty' (Tvar { name; layout = layout' })
      | None, Some _ -> ()
      | Some _, Some _ ->
        if ty.level < ty'.level then begin
          log_type ty';
          Transient_expr.set_desc ty' (Tvar { name; layout = layout' })
        end
      | None, None   -> ()
      end
  | _ -> ()
  end
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
(* TODO: consider eliminating set_type_desc, replacing it with link types *)
let set_type_desc ty td =
  let ty = repr ty in
  if td != ty.desc then begin
    log_type ty;
    Transient_expr.set_desc ty td
  end
(* TODO: separate set_level into two specific functions: *)
(*  set_lower_level and set_generic_level *)
let set_level ty level =
  let ty = repr ty in
  if level <> ty.level then begin
    if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
    Transient_expr.set_level ty level
  end
(* TODO: introduce a guard and rename it to set_higher_scope? *)
let set_scope ty scope =
  let ty = repr ty in
  if scope <> ty.scope then begin
    if ty.id <= !last_snapshot then log_change (Cscope (ty, ty.scope));
    Transient_expr.set_scope ty scope
  end
let set_var_layout ty layout =
  let ty = repr ty in
  log_type ty;
  Transient_expr.set_var_layout ty layout
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v

let rec link_row_field_ext ~(inside : row_field) (v : row_field) =
  match inside with
  | RFeither {ext = {contents = RFnone} as e} ->
      let RFeither _ | RFpresent _ | RFabsent as v = v in
      log_change (Crow e); e := v
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      link_row_field_ext ~inside:rf v
  | _ -> invalid_arg "Types.link_row_field_ext"

let rec link_kind ~(inside : field_kind) (k : field_kind) =
  match inside with
  | FKvar ({field_kind = FKprivate} as rk) as inside ->
      (* prevent a loop by normalizing k and comparing it with inside *)
      let FKvar _ | FKpublic | FKabsent as k = field_kind_internal_repr k in
      if k != inside then begin
        log_change (Ckind inside);
        rk.field_kind <- k
      end
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as inside} ->
      link_kind ~inside k
  | _ -> invalid_arg "Types.link_kind"

let rec commu_repr : commutable -> commutable = function
  | Cvar {commu = Cvar _ | Cok as commu} -> commu_repr commu
  | c -> c

let rec link_commu ~(inside : commutable) (c : commutable) =
  match inside with
  | Cvar ({commu = Cunknown} as rc) as inside ->
      (* prevent a loop by normalizing c and comparing it with inside *)
      let Cvar _ | Cok as c = commu_repr c in
      if c != inside then begin
        log_change (Ccommu inside);
        rc.commu <- c
      end
  | Cvar {commu = Cvar _ | Cok as inside} ->
      link_commu ~inside c
  | _ -> invalid_arg "Types.link_commu"

let set_commu_ok c = link_commu ~inside:c Cok

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  (!trail, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack ~cleanup_abbrev (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Types.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      trail := changes

let undo_first_change_after (changes, _) =
  match !changes with
  | Change (ch, _) ->
      undo_change ch
  | _ -> ()

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            Transient_expr.set_desc ty desc; r := !next
        | _ -> ())
        log

module Alloc_mode = struct
  type nonrec const = alloc_mode_const = Global | Local
  type t = alloc_mode =
    | Amode of const
    | Amodevar of alloc_mode_var

  let global = Amode Global
  let local = Amode Local
  let of_const = function
    | Global -> global
    | Local -> local

  let min_mode = global

  let max_mode = local

  let le_const a b =
    match a, b with
    | Global, _ | _, Local -> true
    | Local, Global -> false

  let join_const a b =
    match a, b with
    | Local, _ | _, Local -> Local
    | Global, Global -> Global

  let meet_const a b =
    match a, b with
    | Global, _ | _, Global -> Global
    | Local, Local -> Local

  exception NotSubmode
(*
  let pp_c ppf = function
    | Global -> Printf.fprintf ppf "0"
    | Local -> Printf.fprintf ppf "1"
  let pp_v ppf v =
    let i = v.mvid in
    (if i < 26 then Printf.fprintf ppf "%c" (Char.chr (Char.code 'a' + i))
    else Printf.fprintf ppf "v%d" i);
    Printf.fprintf ppf "[%a%a]" pp_c v.lower pp_c v.upper
*)

  let set_lower ~log v lower =
    append_change log (Cmode_lower (v, v.lower));
    v.lower <- lower

  let set_upper ~log v upper =
    append_change log (Cmode_upper (v, v.upper));
    v.upper <- upper

  let set_vlower ~log v vlower =
    append_change log (Cmode_vlower (v, v.vlower));
    v.vlower <- vlower

  let submode_cv ~log m v =
    (* Printf.printf "  %a <= %a\n" pp_c m pp_v v; *)
    if le_const m v.lower then ()
    else if not (le_const m v.upper) then raise NotSubmode
    else begin
      let m = join_const v.lower m in
      set_lower ~log v m;
      if m = v.upper then set_vlower ~log v []
    end

  let rec submode_vc ~log v m =
    (* Printf.printf "  %a <= %a\n" pp_v v pp_c m; *)
    if le_const v.upper m then ()
    else if not (le_const v.lower m) then raise NotSubmode
    else begin
      let m = meet_const v.upper m in
      set_upper ~log v m;
      v.vlower |> List.iter (fun a ->
        (* a <= v <= m *)
        submode_vc ~log a m;
        set_lower ~log v (join_const v.lower a.lower);
      );
      if v.lower = m then set_vlower ~log v []
    end

  let submode_vv ~log a b =
    (* Printf.printf "  %a <= %a\n" pp_v a pp_v b; *)
    if le_const a.upper b.lower then ()
    else if a == b || List.memq a b.vlower then ()
    else begin
      submode_vc ~log a b.upper;
      set_vlower ~log b (a :: b.vlower);
      submode_cv ~log a.lower b;
    end

  let submode a b =
    let log_head = ref Unchanged in
    let log = ref log_head in
    match
      match a, b with
      | Amode a, Amode b ->
         if not (le_const a b) then raise NotSubmode
      | Amodevar v, Amode c ->
         (* Printf.printf "%a <= %a\n" pp_v v pp_c c; *)
         submode_vc ~log v c
      | Amode c, Amodevar v ->
         (* Printf.printf "%a <= %a\n" pp_c c pp_v v; *)
         submode_cv ~log c v
      | Amodevar a, Amodevar b ->
         (* Printf.printf "%a <= %a\n" pp_v a pp_v b; *)
         submode_vv ~log a b
    with
    | () ->
       log_changes !log_head !log;
       Ok ()
    | exception NotSubmode ->
       let backlog = rev_log [] !log_head in
       List.iter undo_change backlog;
       Error ()

  let submode_exn t1 t2 =
    match submode t1 t2 with
    | Ok () -> ()
    | Error () -> invalid_arg "submode_exn"

  let equate a b =
    match submode a b, submode b a with
    | Ok (), Ok () -> Ok ()
    | Error (), _ | _, Error () -> Error ()

  let make_global_exn t =
    submode_exn t global

  let make_local_exn t =
    submode_exn local t

  let next_id = ref (-1)
  let fresh () =
    incr next_id;
    { upper = Local;
      lower = Global;
      vlower = [];
      mvid = !next_id;
      mark = false }

  let rec all_equal v = function
    | [] -> true
    | v' :: rest ->
        if v == v' then all_equal v rest
        else false

  let joinvars vars =
    match vars with
    | [] -> global
    | v :: rest ->
      let v =
        if all_equal v rest then v
        else begin
          let v = fresh () in
          List.iter (fun v' -> submode_exn (Amodevar v') (Amodevar v)) vars;
          v
        end
      in
      Amodevar v

  let join ms =
    let rec aux vars = function
      | [] -> joinvars vars
      | Amode Global :: ms -> aux vars ms
      | Amode Local :: _ -> local
      | Amodevar v :: ms -> aux (v :: vars) ms
    in aux [] ms

  let constrain_upper = function
    | Amode m -> m
    | Amodevar v ->
       submode_exn (Amode v.upper) (Amodevar v);
       v.upper

  exception Became_constant
  let compress_vlower v =
    let nmarked = ref 0 in
    let mark v' =
      assert (not v'.mark);
      v'.mark <- true;
      incr nmarked
    in
    let unmark v' =
      assert v'.mark;
      v'.mark <- false;
      decr nmarked
    in
    let new_lower = ref v.lower in
    let new_vlower = ref v.vlower in
    (* Ensure that each transitive lower bound of v
       is a direct lower bound of v *)
    let rec trans v' =
      if le_const v'.upper !new_lower then ()
      else if v'.mark then ()
      else begin
        mark v';
        new_vlower := v' :: !new_vlower;
        trans_low v'
      end
    and trans_low v' =
      assert (v != v');
      if not (le_const v'.lower v.upper) then
        Misc.fatal_error "compress_vlower: invalid bounds";
      if not (le_const v'.lower !new_lower) then begin
        new_lower := join_const !new_lower v'.lower;
        if !new_lower = v.upper then
          (* v is now a constant, no need to keep computing bounds *)
          raise Became_constant
      end;
      List.iter trans v'.vlower
    in
    mark v;
    List.iter mark v.vlower;
    let became_constant =
      match List.iter trans_low v.vlower with
      | () -> false
      | exception Became_constant -> true
    in
    List.iter unmark !new_vlower;
    unmark v;
    assert (!nmarked = 0);
    if became_constant then new_vlower := [];
    if !new_lower != v.lower || !new_vlower != v.vlower then begin
      let log_head = ref Unchanged in
      let log = ref log_head in
      set_lower ~log v !new_lower;
      set_vlower ~log v !new_vlower;
      log_changes !log_head !log;
    end

  let constrain_lower = function
    | Amode m -> m
    | Amodevar v ->
        compress_vlower v;
        submode_exn (Amodevar v) (Amode v.lower);
        v.lower

  let newvar () = Amodevar (fresh ())

  let newvar_below = function
    | Amode Global -> Amode Global, false
    | m ->
       let v = newvar () in
       submode_exn v m;
       v, true

  let newvar_above = function
    | Amode Local -> Amode Local, false
    | m ->
       let v = newvar () in
       submode_exn m v;
       v, true

  let check_const = function
    | Amode m -> Some m
    | Amodevar v ->
       compress_vlower v;
       if v.lower = v.upper then Some v.lower else None

  let print_const ppf = function
    | Global -> Format.fprintf ppf "Global"
    | Local -> Format.fprintf ppf "Local"

  let print_var_id ppf v =
    Format.fprintf ppf "?%i" v.mvid

  let print_var ppf v =
    compress_vlower v;
    if v.lower = v.upper then begin
      print_const ppf v.lower
    end else if v.vlower = [] then begin
      print_var_id ppf v
    end else begin
      Format.fprintf ppf "%a[> %a]"
        print_var_id v
        (Format.pp_print_list print_var_id) v.vlower
    end

  let print ppf = function
    | Amode m -> print_const ppf m
    | Amodevar v -> print_var ppf v

end

module Value_mode = struct

  type const =
   | Global
   | Regional
   | Local

  let r_as_l : const -> Alloc_mode.const = function
    | Global -> Global
    | Regional -> Local
    | Local -> Local
  [@@warning "-unused-value-declaration"]

  let r_as_g : const -> Alloc_mode.const = function
    | Global -> Global
    | Regional -> Global
    | Local -> Local
  [@@warning "-unused-value-declaration"]

  let of_alloc_consts
        ~(r_as_l : Alloc_mode.const)
        ~(r_as_g : Alloc_mode.const) =
    match r_as_l, r_as_g with
    | Global, Global -> Global
    | Global, Local -> assert false
    | Local, Global -> Regional
    | Local, Local -> Local

  type t = value_mode =
    { r_as_l : Alloc_mode.t;
      (* [r_as_l] is the image of the mode under the [r_as_l] function *)
      r_as_g : Alloc_mode.t;
      (* [r_as_g] is the image of the mode under the [r_as_g] function.
         Always less than [r_as_l]. *) }

  let global =
    let r_as_l = Alloc_mode.global in
    let r_as_g = Alloc_mode.global in
    { r_as_l; r_as_g }

  let regional =
    let r_as_l = Alloc_mode.local in
    let r_as_g = Alloc_mode.global in
    { r_as_l; r_as_g }

  let local =
    let r_as_l = Alloc_mode.local in
    let r_as_g = Alloc_mode.local in
    { r_as_l; r_as_g }

  let of_const = function
    | Global -> global
    | Regional -> regional
    | Local -> local

  let max_mode =
    let r_as_l = Alloc_mode.max_mode in
    let r_as_g = Alloc_mode.max_mode in
    { r_as_l; r_as_g }

  let min_mode =
    let r_as_l = Alloc_mode.min_mode in
    let r_as_g = Alloc_mode.min_mode in
    { r_as_l; r_as_g }

  let of_alloc mode =
    let r_as_l = mode in
    let r_as_g = mode in
    { r_as_l; r_as_g }

  let local_to_regional t = { t with r_as_g = Alloc_mode.global }

  let regional_to_global t = { t with r_as_l = t.r_as_g }

  let regional_to_local t = { t with r_as_g = t.r_as_l }

  let global_to_regional t = { t with r_as_l = Alloc_mode.local }

  let regional_to_global_alloc t = t.r_as_g

  let regional_to_local_alloc t = t.r_as_l

  type error = [`Regionality | `Locality]

  let submode t1 t2 =
    match Alloc_mode.submode t1.r_as_l t2.r_as_l with
    | Error () -> Error `Regionality
    | Ok () as ok -> begin
        match Alloc_mode.submode t1.r_as_g t2.r_as_g with
        | Ok () -> ok
        | Error () -> Error `Locality
      end

  let submode_exn t1 t2 =
    match submode t1 t2 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let rec submode_meet t = function
    | [] -> Ok ()
    | t' :: rest ->
      match submode t t' with
      | Ok () -> submode_meet t rest
      | Error _ as err -> err

  let join ts =
    let r_as_l = Alloc_mode.join (List.map (fun t -> t.r_as_l) ts) in
    let r_as_g = Alloc_mode.join (List.map (fun t -> t.r_as_g) ts) in
    { r_as_l; r_as_g }

  let constrain_upper t =
    let r_as_l = Alloc_mode.constrain_upper t.r_as_l in
    let r_as_g = Alloc_mode.constrain_upper t.r_as_g in
    of_alloc_consts ~r_as_l ~r_as_g

  let constrain_lower t =
    let r_as_l = Alloc_mode.constrain_lower t.r_as_l in
    let r_as_g = Alloc_mode.constrain_lower t.r_as_g in
    of_alloc_consts ~r_as_l ~r_as_g

  let newvar () =
    let r_as_l = Alloc_mode.newvar () in
    let r_as_g = Alloc_mode.newvar () in
    Alloc_mode.submode_exn r_as_g r_as_l;
    { r_as_l; r_as_g }

  let newvar_below = function
    | { r_as_l = Amode Global;
        r_as_g = Amode Global } ->
       global
    | m ->
       let v = newvar () in
       submode_exn v m;
       v

  let check_const t =
    match Alloc_mode.check_const t.r_as_l with
    | None -> None
    | Some r_as_l ->
      match Alloc_mode.check_const t.r_as_g with
      | None -> None
      | Some r_as_g ->
        Some (of_alloc_consts ~r_as_l ~r_as_g)

  let print_const ppf = function
    | Global -> Format.fprintf ppf "Global"
    | Regional -> Format.fprintf ppf "Regional"
    | Local -> Format.fprintf ppf "Local"

  let print ppf t =
    match check_const t with
    | Some const -> print_const ppf const
    | None ->
        Format.fprintf ppf
          "@[<2>r_as_l: %a@ r_as_g: %a@]"
          Alloc_mode.print t.r_as_l
          Alloc_mode.print t.r_as_g

end
