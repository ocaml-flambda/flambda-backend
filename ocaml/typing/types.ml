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

type mutability =
  | Immutable
  | Mutable of Mode.Alloc.Comonadic.Const.t

let is_mutable = function
  | Immutable -> false
  | Mutable _ -> true

(* Type expressions for the core language *)

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_expr = transient_expr

and type_desc =
  | Tvar of { name : string option; jkind : jkind }
  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  | Ttuple of (string option * type_expr) list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of { name : string option; jkind : jkind }
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list

and arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

and arrow_desc =
  arg_label * Mode.Alloc.lr * Mode.Alloc.lr

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

and jkind = type_expr Jkind_types.t

(* jkind depends on types defined in this file, but Jkind.equal is required
   here. When jkind.ml is loaded, it calls set_jkind_equal to fill a ref to the
   function. *)
(** Corresponds to [Jkind.equal] *)
let jkind_equal = ref (fun _ _ ->
    failwith "jkind_equal should be set by jkind.ml")
let set_jkind_equal f = jkind_equal := f

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
(* Variance forms a product lattice of the following partial orders:
     0 <= may_pos <= pos
     0 <= may_weak <= may_neg <= neg
     0 <= inj
   Additionally, the following implications are valid
     pos => inj
     neg => inj
   Examples:
     type 'a t        : may_pos + may_neg + may_weak
     type 'a t = 'a   : pos
     type 'a t = 'a -> unit : neg
     type 'a t = ('a -> unit) -> unit : pos + may_weak
     type 'a t = A of (('a -> unit) -> unit) : pos
     type +'a p = ..  : may_pos + inj
     type +!'a t      : may_pos + inj
     type -!'a t      : may_neg + inj
     type 'a t = A    : inj
 *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2 + 4
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16 + 8 + 1
    | Neg -> 32 + 8 + 4 + 2
    | Inv -> 63
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x v = union v (single x)
  let set_if b x v = if b then set x v else v
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = single Inv
  let covariant = single Pos
  let swap f1 f2 v v' =
    set_if (mem f2 v) f1 (set_if (mem f1 v) f2 v')
  let conjugate v =
    let v' = inter v (union (single Inj) (single May_weak)) in
    swap Pos Neg v (swap May_pos May_neg v v')
  let compose v1 v2 =
    if mem Inv v1 && mem Inj v2 then full else
    let mp =
      mem May_pos v1 && mem May_pos v2 || mem May_neg v1 && mem May_neg v2
    and mn =
      mem May_pos v1 && mem May_neg v2 || mem May_neg v1 && mem May_pos v2
    and mw = mem May_weak v1 && v2 <> null || v1 <> null && mem May_weak v2
    and inj = mem Inj v1 && mem Inj v2
    and pos = mem Pos v1 && mem Pos v2 || mem Neg v1 && mem Neg v2
    and neg = mem Pos v1 && mem Neg v2 || mem Neg v1 && mem Pos v2 in
    List.fold_left (fun v (b,f) -> set_if b f v) null
      [mp, May_pos; mn, May_neg; mw, May_weak; inj, Inj; pos, Pos; neg, Neg]
  let strengthen v =
    if mem May_neg v then v else v land (full - single May_weak)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj unknown else unknown in
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
  { type_params_: type_param list;
    type_noun: type_noun;
    type_jkind: jkind;
    type_jkind_annotation: type_expr Jkind_types.annotation option;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_unboxed_default: bool;
    type_uid: Uid.t;
    type_has_illegal_crossings: bool;
 }

and type_param =
  { param_expr: type_expr;
    variance: Variance.t;
    separability: Separability.t;
  }

and type_noun =
  | Datatype of { manifest: Path.t option; noun: datatype_noun}
  | Equation of { eq: type_equation }

and datatype_noun =
  | Datatype_record of { priv: private_flag; lbls: label_declaration list; rep: record_representation}
  | Datatype_variant of { priv: private_flag; cstrs: constructor_declaration list; rep: variant_representation }
  | Datatype_open of { priv: private_flag }

and type_equation =
  | Type_abstr of { reason: abstract_reason }
  | Type_abbrev of { priv: private_flag; expansion: type_expr }

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
    Type_abstract of abstract_reason
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and tag = Ordinary of {src_index: int;     (* Unique name (per type) *)
                       runtime_tag: int}   (* The runtime tag *)
        | Extension of Path.t * jkind array

and abstract_reason =
    Abstract_def
  | Abstract_rec_check_regularity

and flat_element =
  | Imm
  | Float_boxed
  | Float64
  | Float32
  | Bits32
  | Bits64
  | Word

and mixed_product_shape =
  { value_prefix_len : int;
    flat_suffix : flat_element array;
  }

and record_representation =
  | Record_unboxed
  | Record_inlined of tag * constructor_representation * variant_representation
  | Record_boxed of jkind array
  | Record_float
  | Record_ufloat
  | Record_mixed of mixed_product_shape

and variant_representation =
  | Variant_unboxed
  | Variant_boxed of (constructor_representation * jkind array) array
  | Variant_extensible

and constructor_representation =
  | Constructor_uniform_value
  | Constructor_mixed of mixed_product_shape

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutability;
    ld_modalities: Mode.Modality.Value.Const.t;
    ld_type: type_expr;
    ld_jkind : jkind;
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

and constructor_argument =
  {
    ca_modalities: Mode.Modality.Value.Const.t;
    ca_type: type_expr;
    ca_loc: Location.t;
  }

and constructor_arguments =
  | Cstr_tuple of constructor_argument list
  | Cstr_record of label_declaration list

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_arg_jkinds: jkind array;
    ext_shape: constructor_representation;
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

let tys_of_constr_args = function
  | Cstr_tuple tl -> List.map (fun ca -> ca.ca_type) tl
  | Cstr_record lbls -> List.map (fun l -> l.ld_type) lbls


let expansion_of_public_abbrev decl =
  match decl.type_noun with
  | Equation { eq = Type_abbrev { priv = Public; expansion }} -> expansion
  | _ -> Misc.fatal_errorf "A public type abbreviation was expected"

let non_trivial_expansion decl =
  match decl.type_noun with
  | Equation { eq = Type_abbrev { priv = _; expansion }} -> Some expansion
  | Equation { eq = Type_abstr { reason = _ }}
  | Datatype { manifest = _;
               noun = Datatype_open _ | Datatype_record _ | Datatype_variant _ }
    -> None

(* Legacy properties *)

let create_type_params type_params type_variance type_separability =
  List.map2
    (fun param_expr (variance, separability) ->
      { param_expr; variance; separability })
    type_params (List.combine type_variance type_separability)
let create_type_params_of_unknowns ~injective type_params =
  let arity = List.length type_params in
  create_type_params
    type_params
    (Variance.unknown_signature ~injective ~arity)
    (Separability.default_signature ~arity)

let get_type_arity decl = List.length decl.type_params_

let get_type_params decl = List.map (fun { param_expr } -> param_expr) decl.type_params_
let set_type_params decl param_exprs =
  { decl with type_params_ =
      List.map2
        (fun param param_expr -> { param with param_expr })
        decl.type_params_ param_exprs }

let get_type_variance decl = List.map (fun { variance } -> variance) decl.type_params_
let set_type_variance decl variances =
  { decl with type_params_ =
      List.map2
        (fun param variance -> { param with variance })
        decl.type_params_ variances }

let get_type_separability decl = List.map (fun { separability } -> separability) decl.type_params_
let set_type_separability decl separabilities =
  { decl with type_params_ =
      List.map2
        (fun param separability -> { param with separability })
        decl.type_params_ separabilities }

let get_type_kind decl =
  match decl.type_noun with
  | Datatype { manifest = _; noun = Datatype_record { priv = _; lbls; rep } } -> Type_record (lbls, rep)
  | Datatype { manifest = _; noun = Datatype_variant { priv = _; cstrs; rep } } -> Type_variant (cstrs, rep)
  | Datatype { manifest = _; noun = Datatype_open _ } -> Type_open
  | Equation { eq = Type_abstr { reason } } -> Type_abstract reason
  | Equation { eq = Type_abbrev _ } -> Type_abstract Abstract_def

let get_type_private decl =
  match decl.type_noun with
  | Datatype { manifest = _; noun = Datatype_record { priv; _ } } -> priv
  | Datatype { manifest = _; noun = Datatype_variant { priv; _ } } -> priv
  | Datatype { manifest = _; noun = Datatype_open { priv; _ } } -> priv
  | Equation { eq = Type_abstr { reason = _ } } -> Public
  | Equation { eq = Type_abbrev { priv; expansion = _ } } -> priv

let hide_manifest decl =
  { decl with type_noun = match decl.type_noun with
    | Datatype { manifest = _; noun } -> Datatype { manifest = None; noun }
    | Equation { eq = Type_abstr { reason } } ->
      Equation { eq = Type_abstr { reason } }
    | Equation { eq = Type_abbrev _ } ->
      Equation { eq = Type_abstr { reason = Abstract_def } }
  }

let get_desc_ref = ref (fun _ -> assert false)
let noun_with_manifest type_noun expansion =
  match type_noun with
  | Datatype { manifest = _; noun } ->
    let manifest =
      match !get_desc_ref expansion with
      | Tconstr (path, _, _ ) -> Some path
      | _ -> assert false
    in
    Datatype { manifest; noun }
  | Equation { eq = Type_abstr { reason = _ } } ->
    Equation { eq = Type_abbrev { priv = Public; expansion } }
  | Equation { eq = Type_abbrev { priv; expansion = _ } } ->
    Equation { eq = Type_abbrev { priv; expansion } }

let with_manifest decl expansion =
  { decl with type_noun = noun_with_manifest decl.type_noun expansion }

let noun_publicise_manifest = function
  | Datatype { manifest; noun } ->
    Datatype { manifest;
      noun = match noun with
      | Datatype_record ({ priv = _; _ } as n) -> Datatype_record { n with priv = Public }
      | Datatype_variant ({ priv = _; _ } as n) -> Datatype_variant { n with priv = Public }
      | Datatype_open { priv = _ } -> Datatype_open { priv = Public }
    }
  | Equation { eq = Type_abstr { reason } } ->
    Equation { eq = Type_abstr { reason } }
  | Equation { eq = Type_abbrev { priv = _; expansion } } ->
    Equation { eq = Type_abbrev { priv = Public; expansion } }

let publicise_manifest decl =
  { decl with type_noun = noun_publicise_manifest decl.type_noun }

let noun_privatise_manifest = function
  | Datatype { manifest; noun } ->
    Datatype { manifest;
      noun = match noun with
      | Datatype_record ({ priv = _; _ } as n) -> Datatype_record { n with priv = Private }
      | Datatype_variant ({ priv = _; _ } as n) -> Datatype_variant { n with priv = Private }
      | Datatype_open { priv = _ } -> Datatype_open { priv = Private }
    }
  | Equation { eq = Type_abstr { reason = _ } } ->
    assert false
    (* Equation { eq = Type_abstr { reason } } *)
  | Equation { eq = Type_abbrev { priv = _; expansion } } ->
    Equation { eq = Type_abbrev { priv = Private; expansion } }

let privatise_manifest decl =
  { decl with type_noun = noun_privatise_manifest decl.type_noun }

let newgenty_ref = ref (fun _ -> assert false)
let get_type_manifest decl =
  match decl.type_noun with
  | Datatype { manifest = None; noun = _ } -> None
  | Datatype { manifest = Some path; noun = _ } ->
    Some (!newgenty_ref (Tconstr (path, get_type_params decl, ref Mnil)))
  | Equation { eq = Type_abstr { reason = _ } } -> None
  | Equation { eq = Type_abbrev { priv = _; expansion } } -> Some expansion

let create_type_equation priv manifest =
  match priv, manifest with
  | priv, Some expansion -> Type_abbrev { priv; expansion }
  (* CR jbachurski: 'Private' abstract types don't really exist,
     but are sometimes created.
     Invariant broken: Private abstract types with no manifest
     are observed by [get_type_private] as public. *)
  | (Public | Private), None -> Type_abstr { reason = Abstract_def }

let create_type_equation_in_noun priv manifest =
  Equation { eq = create_type_equation priv manifest }

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
    clty_hash_type: type_declaration;
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

module Aliasability = struct
  type t = Not_aliasable | Aliasable

  let aliasable b = if b then Aliasable else Not_aliasable

  let is_aliasable = function
    | Aliasable -> true
    | Not_aliasable -> false
end

module type Wrap = sig
  type 'a t
end

module type Wrapped = sig
  type 'a wrapped

  type value_description =
    { val_type: type_expr wrapped;                (* Type of the value *)
      val_modalities : Mode.Modality.Value.t;     (* Modalities on the value *)
      val_kind: value_kind;
      val_loc: Location.t;
      val_zero_alloc: Zero_alloc.t;
      val_attributes: Parsetree.attributes;
      val_uid: Uid.t;
    }

  type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t
  | Mty_strengthen of module_type * Path.t * Aliasability.t
      (* See comments about the aliasability of strengthening in mtype.ml *)

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
    | Mty_strengthen (mty,p,aliasable) ->
        To.Mty_strengthen (module_type m mty, p, aliasable)

  and functor_parameter m = function
      | Unit -> To.Unit
      | Named (id,mty) -> To.Named (id, module_type m mty)

  let value_description m {val_type; val_modalities; val_kind; val_zero_alloc;
                           val_attributes; val_loc; val_uid} =
    To.{
      val_type = m.map_type_expr m val_type;
      val_modalities;
      val_kind;
      val_zero_alloc;
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
    cstr_args: constructor_argument list; (* Type of the arguments *)
    cstr_arg_jkinds: jkind array;       (* Jkinds of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: tag;                      (* Tag for heap blocks *)
    cstr_repr: variant_representation;  (* Repr of the outer variant *)
    cstr_shape: constructor_representation; (* Repr of the constructor itself *)
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

let equal_flat_element e1 e2 =
  match e1, e2 with
  | Imm, Imm | Float64, Float64 | Float32, Float32 | Float_boxed, Float_boxed
  | Word, Word | Bits32, Bits32 | Bits64, Bits64
    -> true
  | (Imm | Float64 | Float32 | Float_boxed | Word | Bits32 | Bits64), _ -> false

let compare_flat_element e1 e2 =
  match e1, e2 with
  | Imm, Imm | Float_boxed, Float_boxed | Float64, Float64 | Float32, Float32
  | Word, Word | Bits32, Bits32 | Bits64, Bits64
    -> 0
  | Imm, _ -> -1
  | _, Imm -> 1
  | Float_boxed, _ -> -1
  | _, Float_boxed -> 1
  | Float64, _ -> -1
  | _, Float64 -> 1
  | Float32, _ -> -1
  | _, Float32 -> 1
  | Word, _ -> -1
  | _, Word -> 1
  | Bits32, _ -> -1
  | _, Bits32 -> 1

let equal_mixed_product_shape r1 r2 = r1 == r2 ||
  (* Warning 9 alerts us if we add another field *)
  let[@warning "+9"] { value_prefix_len = l1; flat_suffix = s1 } = r1
  and                { value_prefix_len = l2; flat_suffix = s2 } = r2
  in
  l1 = l2 && Misc.Stdlib.Array.equal equal_flat_element s1 s2

let equal_constructor_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Constructor_uniform_value, Constructor_uniform_value -> true
  | Constructor_mixed mx1, Constructor_mixed mx2 ->
      equal_mixed_product_shape mx1 mx2
  | (Constructor_mixed _ | Constructor_uniform_value), _ -> false

let equal_variant_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Variant_unboxed, Variant_unboxed ->
      true
  | Variant_boxed cstrs_and_jkinds1, Variant_boxed cstrs_and_jkinds2 ->
      Misc.Stdlib.Array.equal (fun (cstr1, jkinds1) (cstr2, jkinds2) ->
          equal_constructor_representation cstr1 cstr2
          && Misc.Stdlib.Array.equal !jkind_equal jkinds1 jkinds2)
        cstrs_and_jkinds1
        cstrs_and_jkinds2
  | Variant_extensible, Variant_extensible ->
      true
  | (Variant_unboxed | Variant_boxed _ | Variant_extensible), _ ->
      false

let equal_record_representation r1 r2 = match r1, r2 with
  | Record_unboxed, Record_unboxed ->
      true
  | Record_inlined (tag1, cr1, vr1), Record_inlined (tag2, cr2, vr2) ->
      (* Equality of tag and variant representation imply equality of
         constructor representation. *)
      ignore (cr1 : constructor_representation);
      ignore (cr2 : constructor_representation);
      equal_tag tag1 tag2 && equal_variant_representation vr1 vr2
  | Record_boxed lays1, Record_boxed lays2 ->
      Misc.Stdlib.Array.equal !jkind_equal lays1 lays2
  | Record_float, Record_float ->
      true
  | Record_ufloat, Record_ufloat ->
      true
  | Record_mixed mx1, Record_mixed mx2 -> equal_mixed_product_shape mx1 mx2
  | (Record_unboxed | Record_inlined _ | Record_boxed _ | Record_float
    | Record_ufloat | Record_mixed _), _ ->
      false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Extension _, Extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

let find_unboxed_type decl =
  match get_type_kind decl with
    Type_record ([{ld_type = arg; _}], Record_unboxed)
  | Type_record ([{ld_type = arg; _}], Record_inlined (_, _, Variant_unboxed))
  | Type_variant ([{cd_args = Cstr_tuple [{ca_type = arg; _}]; _}], Variant_unboxed)
  | Type_variant ([{cd_args = Cstr_record [{ld_type = arg; _}]; _}],
                  Variant_unboxed) ->
    Some arg
  | Type_record (_, ( Record_inlined _ | Record_unboxed
                    | Record_boxed _ | Record_float | Record_ufloat
                    | Record_mixed _))
  | Type_variant (_, ( Variant_boxed _ | Variant_unboxed
                     | Variant_extensible ))
  | Type_abstract _ | Type_open ->
    None

let item_visibility = function
  | Sig_value (_, _, vis)
  | Sig_type (_, _, _, vis)
  | Sig_typext (_, _, _, vis)
  | Sig_module (_, _, _, _, vis)
  | Sig_modtype (_, _, vis)
  | Sig_class (_, _, _, vis)
  | Sig_class_type (_, _, _, vis) -> vis

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Value.Const.t;(* Modalities on the field *)
    lbl_jkind : jkind;                (* Jkind of the argument *)
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

type mixed_product_element =
  | Value_prefix
  | Flat_suffix of flat_element

let get_mixed_product_element { value_prefix_len; flat_suffix } i =
  if i < 0 then Misc.fatal_errorf "Negative index: %d" i;
  if i < value_prefix_len then Value_prefix
  else Flat_suffix flat_suffix.(i - value_prefix_len)

let flat_element_to_string = function
  | Imm -> "Imm"
  | Float_boxed -> "Float_boxed"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Bits32 -> "Bits32"
  | Bits64 -> "Bits64"
  | Word -> "Word"

let flat_element_to_lowercase_string = function
  | Imm -> "imm"
  | Float_boxed -> "float"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Bits32 -> "bits32"
  | Bits64 -> "bits64"
  | Word -> "word"

(**** Definitions for backtracking ****)

type change =
    Ctype : type_expr * type_desc -> change
  | Ccompress : type_expr * type_desc * type_desc -> change
  | Clevel : type_expr * int -> change
  | Cscope : type_expr * int -> change
  | Cname :
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option -> change
  | Crow : [`none|`some] row_field_gen ref -> change
  | Ckind : [`var] field_kind_gen -> change
  | Ccommu : [`var] commutable_gen -> change
  | Cuniv : type_expr option ref * type_expr option -> change
  | Cmodes : Mode.changes -> change
  | Csort : Jkind_types.Sort.change -> change
  | Czero_alloc : Zero_alloc.change -> change

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

let () =
  Mode.set_append_changes (fun changes -> log_change (Cmodes !changes));
  Jkind_types.Sort.set_change_log (fun change -> log_change (Csort change));
  Zero_alloc.set_change_log (fun change -> log_change (Czero_alloc change))

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

let () = get_desc_ref := get_desc

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
  let set_var_jkind ty jkind' =
    match ty.desc with
    | Tvar { name; _ } ->
      set_desc ty (Tvar { name; jkind = jkind' })
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
  | Cmodes c          -> Mode.undo_changes c
  | Csort change -> Jkind_types.Sort.undo_change change
  | Czero_alloc c -> Zero_alloc.undo_change c

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
    Tvar { name }, Tvar { name = name'; jkind = jkind' } ->
      begin match name, name' with
      | Some _, None ->
        log_type ty';
        Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
      | None, Some _ -> ()
      | Some _, Some _ ->
        if ty.level < ty'.level then begin
          log_type ty';
          Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
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
let set_var_jkind ty jkind =
  let ty = repr ty in
  log_type ty;
  Transient_expr.set_var_jkind ty jkind
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
