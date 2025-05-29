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

(** [blambda] is designed to be a lambda-like expression language where every primitive is
    also a bytecode primitive. This allows us to separate the bytecode backend into two
    stages:

    First, Lambda -> Blambda: Preserves the expression structure, but compiles all complex
    primitives down to ones with corresponding bytecode instructions. This will become
    more important as we continue to add more primitives to Lambda which have no
    corresponding bytecode instruction.

    Second, Blambda -> Instructions: Only has to deal with linearizing the Lambda-like
    control flow. The comparatively fragile stack size maintenance and stack index
    computations can remain in their own module which doesn't need to be modified every
    time we change Lambda.
*)

type constant = Lambda.constant

(** [structured_constant] needs to match the cmo file format *)
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

type method_kind =
  | Self
  | Public

(** primitives that correspond to bytecode instructions that don't affect control flow *)
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
    free_variables : Ident.Set.t
        (** if we ever intended to do optimizations/transformations on blambda, this would
        be better as a function than a field *)
  }

and blambda =
  | Var of Ident.t
  | Const of structured_constant
  | Apply of
      { func : blambda;
        args : blambda list;
        nontail : bool
      }
  | Function of bfunction
  | Let of
      { id : Ident.t;
        arg : blambda;
        body : blambda
      }
  | Letrec of
      { decls : rec_binding list;
        free_variables_of_decls : Ident.Set.t;
            (** if we ever intended to do optimizations/transformations on blambda, this
            would be better as a function than a field *)
        body : blambda
      }
  | Prim of primitive * blambda list
  | Switch of
      { arg : blambda;
        const_cases : int array;
            (** indexes into {!cases}, indexed by the value of the immediate *)
        block_cases : int array;
            (** indexes into {!cases}, indexed by the the block tag *)
        cases : blambda array
      }
  | Staticraise of static_label * blambda list
  | Staticcatch of
      { id : static_label;
        body : blambda;
        args : Ident.t list;
        handler : blambda
      }
  | Trywith of
      { body : blambda;
        param : Ident.t;
        handler : blambda
      }
  | Sequence of blambda * blambda
  | Assign of Ident.t * blambda
  | Send of
      { method_kind : method_kind;
        met : blambda;
        obj : blambda;
        args : blambda list;
        nontail : bool
      }
  | Context_switch of context_switch * blambda list
  | Ifthenelse of
      { cond : blambda;
        ifso : blambda;
        ifnot : blambda
      }
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
  | Event of blambda * Lambda.lambda_event
  | Pseudo_event of blambda * Debuginfo.Scoped_location.t
      (** Pseudo events are ignored by the debugger. They are only used for generating
          backtraces.

          We prefer adding this event here rather than in lambda generation because:
          + There are many different situations where a Pmakeblock can be generated.
          + We prefer inserting a pseudo event rather than an event after to prevent the
          debugger to stop at every single allocation.

          Having [Event] and/or [Pseudo_event] make effective pattern-matching on blambda
          hard. However, blambda is only meant to go immediately before the code
          generator, so it shouldn't really be matched on anyway.

          In the future, we could simplify things a bit and use a new [Lev_pseudo_after]
          event kind in the [Event] constructor instead of Pseudo_event, to generate
          during lambda to blambda conversion if [!Clflags.debug] is [true]. *)
