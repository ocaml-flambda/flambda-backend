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

type event = Lambda.lambda_event

type tailcall =
  | Tailcall
  | Nontail

type context_switch =
  | Perform
  | Reperform
  | Runstack
  | Resume

type primitive =
  | Getglobal of Compilation_unit.t
  | Getpredef of Ident.t
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
  | Setvectitem
  | Setbyteschar
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
    body : blambda;
    loc : Debuginfo.Scoped_location.t;
    free_variables : Ident.Set.t
  }

and blambda =
  | Var of Ident.t
  | Const of Lambda.structured_constant
  | Apply of
      { func : blambda;
        args : blambda list;
        tailcall : tailcall
      }
  | Function of bfunction
  | Let of
      { id : Ident.t;
        arg : blambda;
        body : blambda
      }
  | Letrec of
      { decls : rec_binding list;
        body : blambda;
        free_variables : Ident.Set.t
      }
  | Prim of primitive * blambda list
  | Switch of
      { arg : blambda;
        int_cases : int array;
            (** indices into {!cases}, indexed by the value of the immediate *)
        tag_cases : int array;
            (** indexes into {!cases}, indexed by the the block tag *)
        cases : blambda array
      }
  | Staticraise of static_label * blambda list
  | Staticcatch of
      { body : blambda;
        args : static_label * Ident.t list;
        handler : blambda
      }
  | Trywith of
      { body : blambda;
        id : Ident.t;
        handler : blambda
      }
  | Sequence of blambda * blambda
  | Assign of Ident.t * blambda
  | Send of
      { self : bool;
        met : blambda;
        obj_and_args : blambda list;
        tailcall : tailcall
      }
  | Event of blambda * Lambda.lambda_event
  | Pseudo_event of blambda * Debuginfo.Scoped_location.t
  | Context_switch of context_switch * tailcall * blambda list
  | Ifthenelse of
      { cond : blambda;
        ifso : blambda;
        ifnot : blambda
      }
  | While of
      { wh_cond : blambda;
        wh_body : blambda
      }
  | For of
      { for_id : Ident.t;
        for_from : blambda;
        for_to : blambda;
        for_dir : direction_flag;
        for_body : blambda
      }
  | Sequand of blambda * blambda
  | Sequor of blambda * blambda
