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

type direction_flag = Asttypes.direction_flag =
  | Upto
  | Downto

type raise_kind = Lambda.raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type static_label = Lambda.static_label

type tailcall =
  | Tailcall
  | Nontail

type primitive =
  | Getglobal of Compilation_unit.t
  | Getpredef of Ident.t
  | Perform
  | Boolnot
  | Isint
  | Vectlength
  | Setglobal of Compilation_unit.t
  | Getfield of int
  | Getfloatfield of int
  | Raise of raise_kind
  | Offsetint of int
  | Offsetref of int
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
  | Getstringchar
  | Getbyteschar
  | Getvectitem
  | Setfield of int
  | Setfloatfield of int
  | Sequand
  | Sequor
  | Setvectitem
  | Setbyteschar
  | Reperform
  | Runstack of tailcall
  | Resume of tailcall
  | Ccall of string
  | Makeblock of { tag : int }
  | Makefloatblock
  | Make_faux_mixedblock of
      { total_len : int;
        tag : int
      }

and rec_binding =
  { id : Ident.t;
    def : bfunction
  }

and bfunction =
  { params : Ident.t list;
    body : t;
    loc : Debuginfo.Scoped_location.t
  }

and t =
  | Var of Ident.t
  | Const of Lambda.structured_constant
  | Apply of
      { func : t;
        args : t list;
        tailcall : tailcall
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
  | Prim of primitive * t list * Debuginfo.Scoped_location.t
  | Switch of
      { arg : t;
        sw_numconsts : int;
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
  | Send of
      { kind : Lambda.meth_kind;
        met : t;
        obj : t;
        args : t list;
        tailcall : tailcall
      }
  | Event of t * Lambda.lambda_event
