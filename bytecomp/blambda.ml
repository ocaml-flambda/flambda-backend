(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Debuginfo.Scoped_location

type static_label = Lambda.static_label

type nullary_primitive =
  | Getglobal of Compilation_unit.t
  | Getpredef of Ident.t

type unary_primitive =
  | Setglobal of Compilation_unit.t
  | Perform
  | Vectlength
  | Isint
  | Getfloatfield of int

type binary_primitive =
  | Negint
  | Addint
  | Subint
  | Mulint
  | Divint
  | Modint
  | Andint
  | Orint
  | Xorint
  | Lslint
  | Lsrint
  | Asrint
  | Intcomp of Lambda.integer_comparison
  | Isout
  | Getbyteschar
  | Getvectitem
  | Setfloatfield of int

type ternary_primitive =
  | Setvectitem
  | Setbyteschar

and primitive =
  | Nullary of nullary_primitive
  | Unary of unary_primitive * t
  | Binary of binary_primitive * t * t
  | Ternary of ternary_primitive * t * t * t
  | Ccall of string * t list

type rec_binding =
  { id : Ident.t;
    def : bfunction
  }

and bfunction =
  { params : Ident.t list;  (** function parameters *)
    body : t;  (** the function body *)
    label : label;  (** the label of the function entry *)
    entries : Debug_event.closure_entry Ident.tbl;
        (** the offsets for the free variables
        and mutually recursive functions *)
    rec_pos : int  (** rank in recursive definition *)
  }

and t =
  | Var of Ident.t
  | Const of Lambda.structured_constant
  | Apply of
      { func : t;
        args : t list
      }
  | Function of bfunction
  | Let of
      { id : Ident.t;
        arg : t;
        body : t
      }
  | Letrec of
      { decls : rec_binding list;
        body : t
      }
  | Prim of primitive
  | Switch of
      { sw_numconsts : int;
        sw_consts : (int * t) list;
        sw_numblocks : int;
        sw_blocks : (int * t) list;
        sw_failaction : t option
      }
  | Staticraise of static_label * t list
  | Staticcatch of
      { body : t;
        args : static_label * Ident.t list;
        handler : t
      }
  | Trywith of
      { body : t;
        id : Ident.t;
        handler : t
      }
  | Ifthenelse of
      { cond : t;
        ifso : t;
        ifnot : t
      }
  | Sequence of t * t
  | While of
      { wh_cond : t;
        wh_body : t
      }
  | For of
      { for_id : Ident.t;
        for_from : t;
        for_to : t;
        for_dir : direction_flag;
        for_body : t
      }
  | Assign of Ident.t * t
  | Lsend of
      { kind : Lambda.meth_kind;
        met : t;
        obj : t;
        args : t list;
        rc : Lambda.region_close
      }
  | Levent of t * Lambda.lambda_event
