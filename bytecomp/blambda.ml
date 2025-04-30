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

(* [structured_constant] needs to match the cmo file format *)

type constant = Lambda.constant

type structured_constant = Lambda.structured_constant =
  | Const_base of constant
  | Const_block of int * structured_constant list
  | Const_mixed_block of
      int * Lambda.mixed_block_shape * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
  | Const_float_block of string list
  | Const_null

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

type comparison = Instruct.comparison =
  | Eq
  | Neq
  | Ltint
  | Gtint
  | Leint
  | Geint
  | Ultint
  | Ugeint

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
  | Intcomp of comparison
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
  | Check_signals

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
  | Const of structured_constant
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
            (** indexes into {!cases}, indexed by the value of the immediate *)
        tag_cases : int array;
            (** indexes into {!cases}, indexed by the the block tag *)
        cases : blambda array
      }
  | Staticraise of static_label * blambda list
  | Staticcatch of
      { id : static_label;
        body : blambda;
        args : Ident.t list;
        handler : blambda;
        recursive : bool
      }
  | Trywith of
      { body : blambda;
        param : Ident.t;
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
  (* CR jvanburen: we could somewhat easily get rid of for loops and/or while loops *)
  | While of
      { cond : blambda;
        body : blambda
      }
  | For of
      { id : Ident.t;
        from : blambda;
        to_ : blambda;
        dir : direction_flag;
        body : blambda
      }
  | Sequand of blambda * blambda
  | Sequor of blambda * blambda
