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

(* Type expressions for the core language *)

type type_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_desc =
    Tvar of string option
  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list

and arrow_desc =
  arg_label * alloc_mode * alloc_mode

and alloc_mode_const = Global | Local

and alloc_mode_var = {
  mutable upper: alloc_mode_const;
  mutable lower: alloc_mode_const;
  mutable vlower: alloc_mode_var list;
  mvid: int;
}

and alloc_mode =
  | Amode of alloc_mode_const
  | Amodevar of alloc_mode_var

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr | Fixed_private | Reified of Path.t | Rigid
and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

and commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* *)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

(* Maps of methods and instance variables *)

module Meths = Misc.Stdlib.String.Map
module Vars = Meths

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
    val_uid: Uid.t;
  }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * Asttypes.mutable_flag *
                 Asttypes.virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)

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
    type_kind: type_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed: unboxed_status;
    type_uid: Uid.t;
 }

and type_kind =
    Type_abstract
  | Type_record of label_declaration list  * record_representation
  | Type_variant of constructor_declaration list
  | Type_open

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

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
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and unboxed_status =
  {
    unboxed: bool;
    default: bool; (* False if the unboxed field was set from an attribute. *)
  }

let unboxed_false_default_false = {unboxed = false; default = false}
let unboxed_false_default_true = {unboxed = false; default = true}
let unboxed_true_default_false = {unboxed = true; default = false}
let unboxed_true_default_true = {unboxed = true; default = true}

type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
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

module Concr = Misc.Stdlib.String.Set

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_signature =
  { csig_self: type_expr;
    csig_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    csig_concr: Concr.t;
    csig_inher: (Path.t * type_expr list) list }

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

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

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

and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

and ext_status =
    Text_first                     (* first constructor of an extension *)
  | Text_next                      (* not first constructor of an extension *)
  | Text_exception                 (* an exception *)


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_normal: int;                   (* Number of non generalized constrs *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

let equal_tag t1 t2 =
  match (t1, t2) with
  | Cstr_constant i1, Cstr_constant i2 -> i2 = i1
  | Cstr_block i1, Cstr_block i2 -> i2 = i1
  | Cstr_unboxed, Cstr_unboxed -> true
  | Cstr_extension (path1, b1), Cstr_extension (path2, b2) ->
      Path.same path1 path2 && b1 = b2
  | (Cstr_constant _|Cstr_block _|Cstr_unboxed|Cstr_extension _), _ -> false

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Cstr_extension _,Cstr_extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_global: global_flag;        (* Is this a global field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

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
  let submode_cv m v =
    (* Printf.printf "  %a <= %a\n" pp_c m pp_v v; *)
    if le_const m v.lower then ()
    else if not (le_const m v.upper) then raise NotSubmode
    else begin
      let m = join_const v.lower m in
      v.lower <- m;
      if m = v.upper then v.vlower <- []
    end

  let rec submode_vc v m =
    (* Printf.printf "  %a <= %a\n" pp_v v pp_c m; *)
    if le_const v.upper m then ()
    else if not (le_const v.lower m) then raise NotSubmode
    else begin
      let m = meet_const v.upper m in
      v.upper <- m;
      v.vlower |> List.iter (fun a ->
        (* a <= v <= m *)
        submode_vc a m;
        v.lower <- join_const v.lower a.lower;
      );
      if v.lower = m then v.vlower <- []
    end

  let submode_vv a b =
    (* Printf.printf "  %a <= %a\n" pp_v a pp_v b; *)
    if le_const a.upper b.lower then ()
    else if List.memq a b.vlower then ()
    else begin
      submode_vc a b.upper;
      b.vlower <- a :: b.vlower;
      submode_cv a.lower b;
    end

  let submode a b =
    match
      match a, b with
      | Amode a, Amode b ->
         if not (le_const a b) then raise NotSubmode
      | Amodevar v, Amode c ->
         (* Printf.printf "%a <= %a\n" pp_v v pp_c c; *)
         submode_vc v c
      | Amode c, Amodevar v ->
         (* Printf.printf "%a <= %a\n" pp_c c pp_v v; *)
         submode_cv c v
      | Amodevar a, Amodevar b ->
         (* Printf.printf "%a <= %a\n" pp_v a pp_v b; *)
         submode_vv a b
    with
    | () -> Ok ()
    | exception NotSubmode -> Error ()

  let submode_exn t1 t2 =
    match submode t1 t2 with
    | Ok () -> ()
    | Error () -> invalid_arg "submode_exn"

  let equate a b =
    match submode a b, submode b a with
    | Ok (), Ok () -> Ok ()
    | Error (), _ | _, Error () -> Error ()

  let next_id = ref (-1)
  let fresh () =
    incr next_id;
    { upper = Local; lower = Global; vlower = []; mvid = !next_id }

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
          List.iter (fun v' -> submode_vv v' v) vars;
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
       submode_cv v.upper v;
       v.upper

  let compress_vlower v =
    (* Ensure that each transitive lower bound of v
       is a direct lower bound of v *)
    let rec trans v' =
      if le_const v'.upper v.lower then ()
      else if List.memq v' v.vlower then ()
      else begin
        v.vlower <- v' :: v.vlower;
        trans_low v'
      end
    and trans_low v' =
      submode_cv v'.lower v;
      List.iter trans v'.vlower
    in
    List.iter trans_low v.vlower

  let constrain_lower = function
    | Amode m -> m
    | Amodevar v ->
        compress_vlower v;
        submode_vc v v.lower;
        v.lower

  let newvar () = Amodevar (fresh ())

  let check_const = function
    | Amode m -> Some m
    | Amodevar v when v.lower = v.upper ->
       Some v.lower
    | Amodevar _ -> None

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

  type t =
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

  type error =
    | Regionality
    | Locality

  let submode t1 t2 =
    match Alloc_mode.submode t1.r_as_l t2.r_as_l with
    | Error () -> Error Regionality
    | Ok () as ok -> begin
        match Alloc_mode.submode t1.r_as_g t2.r_as_g with
        | Ok () -> ok
        | Error () -> Error Locality
      end

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
